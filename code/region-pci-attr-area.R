library(tidyverse)
library(here)
library(tidycensus)
library(sf)
library(tigris)

'%!in%' <- function(x,y)!('%in%'(x,y))

summ_msa <- function(name) {
  this_access <- here("data",
                      "block-point-access",
                      name,
                      "access_calc.csv") |>
    read_csv() |>
    mutate(id = as.character(id))
  
  state_ids <- substr(this_access$id, 1, 2) |>
    unique()
  
  county_ids <- substr(this_access$id, 1, 5) |>
    unique() 
  
  msa_counties <- counties(state = state_ids) |>
    st_drop_geometry() |>
    filter(GEOID %in% county_ids)
  
  total_land_area <- sum(msa_counties$ALAND)
  
  ## tigris is not returning shapes for New Hampshire, Nevada, or Arizona at
  ## the moment, so I've manually downloaded them from 
  ## https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2021&layergroup=Blocks+%282020%29
  ## They are in the .gitignore file, so others will have to download them to the
  ## appropriate folder for these to work.
  if(state_ids[1] == "33") { 
    block_areas <- here(
      "data",
      "new-hampshire",
      "tl_2021_33_tabblock20.shp") |>
      st_read() |>
      st_drop_geometry() |>
      rename(id = GEOID20) |>
      select(id, ALAND20) 
  } else if(state_ids[1] == "32") { 
    block_areas <- here(
      "data",
      "nevada",
      "tl_2021_32_tabblock20.shp") |>
      st_read() |>
      st_drop_geometry() |>
      rename(id = GEOID20) |>
      select(id, ALAND20) 
  } else if(state_ids[1] == "04") { 
    block_areas <- here(
      "data",
      "arizona",
      "tl_2021_04_tabblock20.shp") |>
      st_read() |>
      st_drop_geometry() |>
      rename(id = GEOID20) |>
      select(id, ALAND20) 
  } else {
    block_areas <- blocks(state = state_ids[1]) |>
      st_drop_geometry() |>
      rename(id = GEOID20) |>
      select(id, ALAND20)    
  }

  if(length(state_ids) > 1) {
    for(i in 2:length(state_ids)) {
      if(state_ids[i] == "33") { 
        more_block_areas <- here(
          "data",
          "new-hampshire",
          "tl_2021_33_tabblock20.shp") |>
          st_read() |>
          st_drop_geometry() |>
          rename(id = GEOID20) |>
          select(id, ALAND20) 
      } else if(state_ids[i] == "32") {
        more_block_areas <- here(
          "data",
          "nevada",
          "tl_2021_32_tabblock20.shp") |>
          st_read() |>
          st_drop_geometry() |>
          rename(id = GEOID20) |>
          select(id, ALAND20) 
      } else if(state_ids[i] == "04") {
        more_block_areas <- here(
          "data",
          "arizona",
          "tl_2021_04_tabblock20.shp") |>
          st_read() |>
          st_drop_geometry() |>
          rename(id = GEOID20) |>
          select(id, ALAND20) 
      } else {
        more_block_areas <- blocks(state = state_ids[i]) |>
          st_drop_geometry() |>
          rename(id = GEOID20) |>
          select(id, ALAND20)    
      }
      block_areas <- rbind(block_areas, more_block_areas)
    }
  }
  
  this_blocks <- here("data",
                      "block-point-access",
                      name,
                      "block-data.geojson") |>
    st_read() |>
    mutate(id = as.character(id)) |>
    select(id) |>
    inner_join(this_access) |>
    inner_join(block_areas) |>
    arrange(-total_attr) |>
    mutate(cumulative_attr = cumsum(total_attr)) |>
    mutate(cumul_pct_attr = cumulative_attr / sum(total_attr)) 
  
  this_blocks$cumul_pct_attr_before <- c(0, this_blocks$cumul_pct_attr[1:nrow(this_blocks)-1])
  
  half_of_attr_blocks <- this_blocks |>
    filter(cumul_pct_attr_before < 0.5)
  
  attr_conc <- (total_land_area/sum(half_of_attr_blocks$ALAND20)) * 
    (sum(half_of_attr_blocks$total_attr)/(0.5*sum(this_blocks$total_attr)))
  
  tibble(place = name,
         avg_car_access = sum(this_blocks$n_HHs * this_blocks$car_access) / sum(this_blocks$n_HHs),
         avg_no_car_access = sum(this_blocks$n_HHs * this_blocks$no_car_access) / sum(this_blocks$n_HHs),
         attr_conc = attr_conc,
         land_area_km2 = total_land_area/1000000,
         attr_dens = sum(this_blocks$total_attr) * 1000000 / total_land_area) |>
    mutate(pci = avg_no_car_access / avg_car_access)
}

summ_ny <- summ_msa("New-York-Newark-Jersey-City-NY-NJ")
summ_la <- summ_msa("Los-Angeles-Long-Beach-Anaheim-CA")
summ_chi <- summ_msa("Chicago-Naperville-Elgin-IL-IN")
summ_riv <- summ_msa("Riverside-San-Bernardino-Ontario-CA")
summ_los_al <- summ_msa("Los-Alamos-NM")

access_summary <- rbind(summ_ny,
                        summ_la,
                        summ_chi,
                        summ_riv,
                        summ_los_al)

point_folders <- here("data",
                      "block-point-access") |>
  dir()

for(i in 1:length(point_folders)) {

  if(point_folders[i] %!in% access_summary$place) {
    this_access <- summ_msa(point_folders[i])
    
    access_summary <- rbind(access_summary, this_access)
    
    write_csv(access_summary, file = here("data",
                                          "region-data",
                                          "region-pci-attr-area.csv"))
  }
}
