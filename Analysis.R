### Housing production analysis script
# Henry.McKay@dot.ca.gov

# Load packages
library(tigris)
library(dplyr)
library(tidycensus)
library(mapview)
library(sf)
library(ggplot2)
library(scales)

# View variable names for specific datasets/years
#View(load_variables(year = 1990, dataset = "sf1"))
#View(load_variables(year = 2010, dataset = "sf1"))
#View(load_variables(year = 2020, dataset = "pl"))

# Get housing units for 2000 (doesn't work with the API)
#units_2000 <- get_decennial(geography = "block group",
                            #year = 2000,
                            #state = 06,
                            #county = 037,
                            #variables =  "H001001",
                            #geometry = T) 

# Get housing units for 2010
units_2010 <- get_decennial(geography = "block group",
                            year = 2010,
                            state = 06,
                            #county = 037,
                            variables =  "H001001",
                            geometry = T) 

# Get housing units for 2020
units_2020 <- get_decennial(geography = "block group",
                            year = 2020,
                            state = 06,
                            #county = 037,
                            variables =  "H1_001N",
                            geometry = T) 

# Get 2020 blocks (for spatial interpolation weighting)
sac_blocks <- blocks(year = 2020,
                     state = 06)

# Interpolate 2000 units to 2020 Census geographies
#new_2000_units <- interpolate_pw(from = units_2000,
                                 #to = units_2020,
                                 #to_id = "GEOID",
                                 #weights = sac_blocks,
                                 #weight_column = "POP20",
                                 #crs = 3310,
                                 #extensive = T) %>%
  #select(GEOID, value) %>%
  #rename("units_2000" = "value")

# Interpolate 2010 units to 2020 Census geographies
new_2010_units <- interpolate_pw(from = units_2010,
                                 to = units_2020,
                                 to_id = "GEOID",
                                 weights = sac_blocks,
                                 weight_column = "POP20",
                                 crs = 3310,
                                 extensive = T) %>%
  select(GEOID, value) %>%
  rename("units_2010" = "value")

# Get 2020 units (again)
new_units_2020 <- get_decennial(geography = "block group",
                                year = 2020,
                                state = 06,
                                #county = 037,
                                variables =  "H1_001N",
                                geometry = F) %>%
  select(GEOID, value) %>%
  rename("units_2020" = "value")

# Merge all years into a dataframe
df <- merge(new_2010_units,
            new_units_2020,
            by = "GEOID",
            all = T)

# Calculate change stats for each block group
df <- df %>%
  mutate(change = (units_2020 - units_2010)) %>%
  st_drop_geometry()

# Get access to destinations data from the Caltrans EQI
EQI <- readRDS("/Users/Username/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/EQI_Project_Screening_App/data/EQI.rds") %>%
  st_drop_geometry() %>%
  #filter(COUNTYFP20 == "037") %>%
  mutate(GEOID = substr(GEOID20, 1, 12)) %>%
  select(GEOID, TRANSIT_RATIO_JOBS, TRANSIT_RATIO_POIs, PED_RATIO, BIKE_RATIO, POP20) %>%
  group_by(GEOID) %>%
  summarise(transit_access_ratio_jobs = weighted.mean(TRANSIT_RATIO_JOBS, POP20, na.rm = T),
            transit_access_ratio_pois = weighted.mean(TRANSIT_RATIO_POIs, POP20, na.rm = T),
            bike_ratio_pois = weighted.mean(BIKE_RATIO, POP20, na.rm = T),
            ped_ratio_pois = weighted.mean(PED_RATIO, POP20, na.rm = T)) %>%
  mutate(transit_jobs_access_category = as.character(cut(transit_access_ratio_jobs, c(0, .025, .05, .1, .15, .20, .25, .30, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1),
                                                     c("0 - 2.5%", "2.5 - 5%", "5 - 10%", "10 - 15%", "15 - 20%", "20 - 25%", "25 - 30%", "30 - 35%", "35 - 40%", "40 - 45%", "45 - 50%", "50 - 55%", "55 - 60%", "60 - 65%", "65 - 70%", "70 - 75%", "75 - 80%", "80 - 85%", "85 - 90%", "90 - 95%", "95 - 100%")))) %>%
  mutate(transit_pois_access_category = as.character(cut(transit_access_ratio_pois, c(0, .025, .05, .1, .15, .20, .25, .30, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1),
                                                    c("0 - 2.5%", "2.5 - 5%", "5 - 10%", "10 - 15%", "15 - 20%", "20 - 25%", "25 - 30%", "30 - 35%", "35 - 40%", "40 - 45%", "45 - 50%", "50 - 55%", "55 - 60%", "60 - 65%", "65 - 70%", "70 - 75%", "75 - 80%", "80 - 85%", "85 - 90%", "90 - 95%", "95 - 100%")))) %>%
  mutate(bike_pois_access_category = as.character(cut(bike_ratio_pois, c(0, .05, .1, .15, .20, .25, .30, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1),
                                                  c("0 - 5%", "5 - 10%", "10 - 15%", "15 - 20%", "20 - 25%", "25 - 30%", "30 - 35%", "35 - 40%", "40 - 45%", "45 - 50%", "50 - 55%", "55 - 60%", "60 - 65%", "65 - 70%", "70 - 75%", "75 - 80%", "80 - 85%", "85 - 90%", "90 - 95%", "95 - 100%")))) %>%
  mutate(ped_pois_access_category = as.character(cut(ped_ratio_pois, c(0, .05, .1, .15, .20, .25, .30, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1),
                                                 c("0 - 5%", "5 - 10%", "10 - 15%", "15 - 20%", "20 - 25%", "25 - 30%", "30 - 35%", "35 - 40%", "40 - 45%", "45 - 50%", "50 - 55%", "55 - 60%", "60 - 65%", "65 - 70%", "70 - 75%", "75 - 80%", "80 - 85%", "85 - 90%", "90 - 95%", "95 - 100%"))))

# Merge housing and accessibility data
df2 <- merge(df,
             EQI,
             by = "GEOID",
             all.x = T) %>%
  st_drop_geometry() 

# For na/nan values, define as "uncategorized"
df2$transit_jobs_access_category[is.na(df2$transit_jobs_access_category)] <- "Uncategorized"
df2$transit_jobs_access_category[is.nan(df2$transit_jobs_access_category)] <- "Uncategorized"
df2$transit_pois_access_category[is.na(df2$transit_pois_access_category)] <- "Uncategorized"
df2$transit_pois_access_category[is.nan(df2$transit_pois_access_category)] <- "Uncategorized"
df2$bike_pois_access_category[is.na(df2$bike_pois_access_category)] <- "Uncategorized"
df2$bike_pois_access_category[is.nan(df2$bike_pois_access_category)] <- "Uncategorized"
df2$ped_pois_access_category[is.na(df2$ped_pois_access_category)] <- "Uncategorized"
df2$ped_pois_access_category[is.nan(df2$ped_pois_access_category)] <- "Uncategorized"

# Export CSV files for each metric
df_transit_jobs <- df2 %>%
  group_by(transit_jobs_access_category) %>%
  summarise(new_units = sum(change, na.rm = T))
write.csv(df_transit_jobs, "/Users/Username/Downloads/df_transit_jobs.csv", na = "")

df_transit_pois <- df2 %>%
  group_by(transit_pois_access_category) %>%
  summarise(new_units = sum(change, na.rm = T))
write.csv(df_transit_pois, "/Users/Username/Downloads/df_transit_pois.csv", na = "")

df_bike_pois <- df2 %>%
  group_by(bike_pois_access_category) %>%
  summarise(new_units = sum(change, na.rm = T))
write.csv(df_bike_pois, "/Users/Username/Downloads/df_bike_pois.csv", na = "")

df_ped_pois <- df2 %>%
  group_by(ped_pois_access_category) %>%
  summarise(new_units = sum(change, na.rm = T))
write.csv(df_ped_pois, "/Users/Username/Downloads/df_ped_pois.csv", na = "")

### VMT analysis
# Get housing units for 2010
units_2010 <- get_decennial(geography = "tract",
                            year = 2010,
                            state = 06,
                            #county = 037,
                            variables =  "H001001",
                            geometry = T) 

# Get housing units for 2020
units_2020 <- get_decennial(geography = "tract",
                            year = 2020,
                            state = 06,
                            #county = 037,
                            variables =  "H1_001N",
                            geometry = T) 

# Get 2020 blocks (for spatial interpolation weighting)
sac_blocks <- blocks(year = 2020,
                     state = 06)

# Interpolate 2010 units to 2020 Census geographies
new_2010_units <- interpolate_pw(from = units_2010,
                                 to = units_2020,
                                 to_id = "GEOID",
                                 weights = sac_blocks,
                                 weight_column = "POP20",
                                 crs = 3310,
                                 extensive = T) %>%
  select(GEOID, value) %>%
  rename("units_2010" = "value")

# Get 2020 units (again)
new_units_2020 <- get_decennial(geography = "tract",
                                year = 2020,
                                state = 06,
                                #county = 037,
                                variables =  "H1_001N",
                                geometry = F) %>%
  select(GEOID, value) %>%
  rename("units_2020" = "value")

# Merge all years into a dataframe
df <- merge(new_2010_units,
            new_units_2020,
            by = "GEOID",
            all = T)

# Calculate change stats for each block group
df <- df %>%
  mutate(change = (units_2020 - units_2010)) %>%
  st_drop_geometry() %>%
  mutate(customGeo = as.numeric(GEOID))

# Read Replica VMT data from local path
VMT <- read_sf("/Users/Username/Downloads/replica-ca_vmt-02_07_25-vmt_layer/replica-ca_vmt-02_07_25-vmt_layer.shp") %>%
  mutate(state_code = substr(customGeo, 1, 1)) %>%
  filter(state_code == "6") %>%
  st_drop_geometry() %>%
  mutate(VMT_Category = as.character(cut(resVMT_pc, c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 1000),
                                     c("0 - 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25", "25 - 30", "30 - 35", "35 - 40", "40 - 45", "45 - 50", "50 +"))))

df <- merge(df,
            VMT,
            by = "customGeo",
            all.x = T) 

# For na values, define as "uncategorized"
df$VMT_Category[is.na(df$VMT_Category)] <- "Uncategorized"

# Aggregate by VMT category
df <- df %>%
  group_by(VMT_Category) %>%
  summarise(HousingUnits = sum(change, na.rm = T))

write.csv(df, "/Users/Username/Downloads/vmt_housing_ca.csv", na = "")
