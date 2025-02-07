library(tigris)
library(dplyr)
library(tidycensus)
library(mapview)
library(sf)
library(ggplot2)
library(scales)

View(load_variables(year = 2010, dataset = "sf1"))

View(load_variables(year = 2020, dataset = "pl"))

units_2010 <- get_decennial(geography = "block group",
                            year = 2010,
                            state = 06,
                            #county = 037,
                            variables =  "H001001",
                            geometry = T) 

units_2020 <- get_decennial(geography = "block group",
                            year = 2020,
                            state = 06,
                            #county = 037,
                            variables =  "H1_001N",
                            geometry = T) 

sac_blocks <- blocks(year = 2020,
                     state = 06)

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
                                #county = 037,
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
  #filter(COUNTYFP20 == "037") %>%
  mutate(GEOID = substr(GEOID20, 1, 12)) %>%
  select(GEOID, TRANSIT_RATIO_JOBS, POP20) %>%
  group_by(GEOID) %>%
  summarise(access_ratio = weighted.mean(TRANSIT_RATIO_JOBS, POP20, na.rm = T))

df <- merge(df,
            EQI,
            by = "GEOID",
            all.x = T) %>%
  st_drop_geometry() %>%
  mutate(access_category = cut(access_ratio, c(0, .05, .1, .15, .20, .25, .30, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1),
                               c("0 - 5%", "5 - 10%", "10 - 15%", "15 - 20%", "20 - 25%", "25 - 30%", "30 - 35%", "35 - 40%", "40 - 45%", "45 - 50%", "50 - 55%", "55 - 60%", "60 - 65%", "65 - 70%", "70 - 75%", "75 - 80%", "80 - 85%", "85 - 90%", "90 - 95%", "95 - 100%"))) %>%
  group_by(access_category) %>%
  summarise(new_units = sum(change, na.rm = T))
  
  


write.csv(df, "/Users/S152973/Downloads/access_housing_CA.csv", na = "")


pop_2010 <- get_decennial(geography = "block group",
                          year = 2010,
                          state = 06,
                          #county = 037,
                          variables =  "P012001",
                          geometry = F) 

pop_2020 <- get_decennial(geography = "block group",
                          year = 2020,
                          state = 06,
                          #county = 037,
                          variables =  "P1_001N",
                          geometry = F) 

bg_2010 <- block_groups(year = 2010,
                        state = 06)

bg_2020 <- block_groups(year = 2020,
                        state = 06)

df_2010 <- merge(bg_2010,
                 pop_2010,
                 by.x = "GEOID10",
                 by.y = "GEOID",
                 all.x = T) %>%
  st_drop_geometry() %>%
  filter(ALAND10 > 0) %>%
  mutate(pop_density = value / (as.numeric(ALAND10) * 3.86102e-7)) %>%
  arrange(desc(pop_density)) %>%
  mutate(cum_pop_pct = cumsum(value) / sum(value)) %>%
  mutate(cum_area_pct = cumsum(ALAND10) / sum(ALAND10)) %>%
  mutate(Year = "2010") %>%
  select(cum_pop_pct, cum_area_pct, Year)

df_2020 <- merge(bg_2020,
                 pop_2020,
                 by.x = "GEOID",
                 by.y = "GEOID",
                 all.x = T) %>%
  st_drop_geometry() %>%
  filter(ALAND > 0) %>%
  mutate(pop_density = value / (as.numeric(ALAND) * 3.86102e-7)) %>%
  arrange(desc(pop_density)) %>%
  mutate(cum_pop_pct = cumsum(value) / sum(value)) %>%
  mutate(cum_area_pct = cumsum(ALAND) / sum(ALAND)) %>%
  mutate(Year = "2020") %>%
  select(cum_pop_pct, cum_area_pct, Year)

df <- rbind(df_2010, df_2020)

plot <- ggplot(df) +
  geom_line(aes(x = cum_pop_pct, y = cum_area_pct, color = Year), size = .1, alpha = 1) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = .1)) +
  scale_x_continuous(labels = percent, breaks = seq(0, 1, by = .1)) +
  labs(title = paste0("CA Population Lorenz Curve Comparison"), x = "% of Total Population", y = "% of Total Land Area")

View(load_variables(year = 2021, "acs1"))


B25001_001

units_2020 <- get_acs(geography = "block group",
                      survey = "acs5",
                      year = 2020,
                      state = 06,
                      #county = 037,
                      variables =  "B25001_001",
                      geometry = F) %>%
  select(GEOID, estimate) %>%
  rename("units_2020" = "estimate")

units_2023 <- get_acs(geography = "block group",
                      survey = "acs5",
                      year = 2023,
                      state = 06,
                      #county = 037,
                      variables =  "B25001_001",
                      geometry = F) %>%
  select(GEOID, estimate) %>%
  rename("units_2023" = "estimate")

df <- merge(units_2020,
            units_2023,
            by = "GEOID",
            all = T) %>%
  mutate(change = units_2023 - units_2020,
         pct_change = (units_2023 - units_2020) / units_2020)

EQI <- readRDS("/Users/S152973/OneDrive - California Department of Transportation/Documents/Sustainability_Data_Local/EQI_Data/EQI_Project_Screening_App/data/EQI.rds") %>%
  st_drop_geometry() %>%
  #filter(COUNTYFP20 == "037") %>%
  mutate(GEOID = substr(GEOID20, 1, 12)) %>%
  select(GEOID, TRANSIT_RATIO_JOBS, POP20) %>%
  group_by(GEOID) %>%
  summarise(access_ratio = weighted.mean(TRANSIT_RATIO_JOBS, POP20, na.rm = T))

df <- merge(df,
            EQI,
            by = "GEOID",
            all.x = T) %>%
  st_drop_geometry() %>%
  mutate(access_category = cut(access_ratio, c(0, .05, .1, .15, .20, .25, .30, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1),
                               c("0 - 5%", "5 - 10%", "10 - 15%", "15 - 20%", "20 - 25%", "25 - 30%", "30 - 35%", "35 - 40%", "40 - 45%", "45 - 50%", "50 - 55%", "55 - 60%", "60 - 65%", "65 - 70%", "70 - 75%", "75 - 80%", "80 - 85%", "85 - 90%", "90 - 95%", "95 - 100%"))) %>%
  group_by(access_category) %>%
  summarise(new_units = sum(change, na.rm = T))




write.csv(df, "/Users/S152973/Downloads/access_housing_CA_20202023.csv", na = "")
