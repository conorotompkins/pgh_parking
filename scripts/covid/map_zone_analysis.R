#map zone analysis

library(tidyverse)
library(vroom)
library(sf)
library(leaflet)

neighborhood_sf <- st_read("data/Neighborhoods_-shp/Neighborhoods_.shp") %>% 
  select(hood)

neighborhood_sf %>% 
  st_drop_geometry() %>% 
  distinct(hood)

df_ts_neighborhood <- read_csv("data/summarized_parking_data_neighborhood.csv")

df_ts_neighborhood %>% 
  distinct(zone_region)

df_ts_neighborhood %>% 
  distinct(zone_region) %>% 
  semi_join(neighborhood_sf, by = c("zone_region" = "hood"))

df_ts_neighborhood %>% 
  distinct(zone_region) %>% 
  anti_join(neighborhood_sf, by = c("zone_region" = "hood"))

neighborhood_leaflet <- neighborhood_sf

labels <- sprintf(
  "<strong>%s</strong>",
  neighborhood_leaflet$hood
) %>% lapply(htmltools::HTML)

leaflet() %>% 
  addPolygons(data = neighborhood_leaflet,
              popup = .$hood,
              label = labels)

#downtown = central business district
#hill district = upper hill, middle hill, crawford-roberts, bedford dwellings, terrace village
