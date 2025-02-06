library(tigris)
library(dplyr)
library(tidycensus)
library(mapview)
library(sf)

View(load_variables(year = 2010, dataset = "sf1"))

View(load_variables(year = 2020, dataset = "pl"))

sac_blocks <- blocks(state = 06,
                     county = 067,
                     year = 2010)


units_2010 <- get_decennial(geography = "block group",
                            year = 2010,
                            state = 06,
                            county = 037,
                            variables =  "H001001",
                            geometry = T) 

units_2020 <- get_decennial(geography = "block group",
                            year = 2020,
                            state = 06,
                            county = 037,
                            variables =  "H1_001N",
                            geometry = T) 

sac_blocks <- blocks(year = 2020,
                     state = 06,
                     county = 037)

new_2010_units <- interpolate_pw(from = units_2010,
                                 to = units_2020,
                                 to_id = "GEOID",
                                 weights = sac_blocks,
                                 weight_column = "POP20",
                                 crs = 3310,
                                 extensive = T) %>%
  select(GEOID, value) %>%
  rename("units_2010" = "value")

new_units_2020 <- get_decennial(geography = "block group",
                                year = 2020,
                                state = 06,
                                county = 037,
                                variables =  "H1_001N",
                                geometry = F) %>%
  select(GEOID, value) %>%
  rename("units_2020" = "value")


df <- merge(new_2010_units,
            new_units_2020,
            by = "GEOID",
            all = T) %>%
  mutate(change = units_2020 - units_2010,
         pct_change = (units_2020 - units_2010) / units_2010)

EQI <- readRDS("/Users/S152973/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/EQI_Project_Screening_App/data/EQI.rds") %>%
  st_drop_geometry() %>%
  filter(COUNTYFP20 == "037") %>%
  mutate(GEOID = substr(GEOID20, 1, 12)) %>%
  select(GEOID, TRANSIT_RATIO_JOBS, POP20) %>%
  group_by(GEOID) %>%
  summarise(access_ratio = weighted.mean(TRANSIT_RATIO_JOBS, POP20, na.rm = T))

df <- merge(df,
            EQI,
            by = "GEOID",
            all.x = T) %>%
  st_drop_geometry() %>%
  mutate(access_category = cut(access_ratio, c(0, .05, .1, .15, .20, .25, .30, .35, .4, .45, .5, .55, .6, .65, .7, .75, 1),
                               c("0 - 5%", "5 - 10%", "10 - 15%", "15 - 20%", "20 - 25%", "25 - 30%", "30 - 35%", "35 - 40%", "40 - 45%", "45 - 50%", "50 - 55%", "55 - 60%", "60 - 65%", "65 - 70%", "70 - 75%", "75% +"))) %>%
  group_by(access_category) %>%
  summarise(new_units = sum(change, na.rm = T))
  
  


write.csv(df, "/Users/S152973/Downloads/access_housing_la.csv", na = "")
