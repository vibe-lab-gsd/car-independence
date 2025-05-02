library(tidyverse)
library(here)
library(tidycensus)
library(readxl)

mode_vars <- c(total_workers = "B08301_001",
               car = "B08301_002",
               transit = "B08301_010",
               walk = "B08301_019",
               bike = "B08301_018",
               total_hhs = "B08201_001",
               zero_veh_hhs = "B08201_002",
               poverty_hhs = "B17012_002",
               under_18_pop = "B09001_001",
               over_65_pop = "B16004_046",
               total_pop = "B01003_001")

county_data <- get_acs(geography = "county", 
                       variables = mode_vars,
                       output = "wide",
                       year = 2022)

# list1_2023.xlsx is from https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
# Accessed May 29, 2024
cbsas <- here("data",
              "list1_2023.xlsx") |>
  read_excel(skip=2) |>
  select(`CBSA Code`,
         `CBSA Title`,
         `County/County Equivalent`,
         `State Name`,
         `FIPS State Code`,
         `FIPS County Code`) |>
  filter(`State Name` != "Alaska" &
           `State Name` != "Hawaii" &
           `State Name` != "Puerto Rico") |>
  mutate(GEOID = paste0(`FIPS State Code`, `FIPS County Code`)) |>
  left_join(county_data) |>
  group_by(`CBSA Title`, `CBSA Code`) |>
  summarise(car2work = sum(carE),
            walk2work = sum(walkE),
            bike2work = sum(bikeE),
            transit2work = sum(transitE),
            total2work = sum(total_workersE),
            zero_veh = sum(zero_veh_hhsE),
            poverty = sum(poverty_hhsE),
            total_hhs = sum(total_hhsE),
            under_18_pop = sum(under_18_popE),
            over_65_pop = sum(over_65_popE),
            total_pop = sum(total_popE)) |>
  mutate(pct_car2work = car2work/total2work,
         pct_walk2work = walk2work/total2work,
         pct_bike2work = bike2work/total2work,
         pct_transit2work = transit2work/total2work,
         pct_zero_veh = zero_veh / total_hhs,
         pct_poverty = poverty / total_hhs,
         pct_under_18 = under_18_pop / total_pop,
         pct_over_65 = over_65_pop / total_pop) |>
  mutate(place = str_replace(`CBSA Title`, ", ", "-")) |>
  mutate(place = str_replace_all(place, " ", "-")) |>
  mutate(place = str_replace_all(place, "/", "-")) |>
  select(place, 
         `CBSA Code`, 
         pct_car2work,
         pct_walk2work,
         pct_bike2work,
         pct_transit2work,
         pct_zero_veh,
         pct_poverty,
         pct_under_18,
         pct_over_65)

write_csv(cbsas, file = here("data",
                             "region-data",
                             "region-validation-vars.csv"))
