library(tidyverse)
library(vroom)
library(sf)

#geocode
parking_locations <- read_csv("data/parking_locations.csv")

glimpse(parking_locations)

parking_locations %>% 
  filter(is.na(longitude) | is.na(latitude))

geolocations <- parking_locations %>% 
  drop_na(longitude, latitude) %>% 
  select(id, zone, latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84")

neighborhood_sf <- st_read("data/Neighborhoods_-shp/Neighborhoods_.shp") %>% 
  select(hood)

neighborhood_sf %>% 
  ggplot() +
  geom_sf(aes(fill = hood), show.legend = FALSE) +
  geom_sf(data = geolocations, alpha = .2, size = .5)

neighborhood_sf %>% 
  distinct(hood) %>% 
  st_drop_geometry()

geocoded_parking_locations <- geolocations %>% 
  st_join(neighborhood_sf, join = st_covered_by)

geocoded_parking_locations %>% 
  ggplot() +
  geom_sf(aes(color = hood), show.legend = FALSE)

geocoded_parking_locations <- geocoded_parking_locations %>% 
  st_drop_geometry() %>% 
  select(zone, hood) %>% 
  rename(neighborhood = hood)

geocoded_parking_locations %>% 
  distinct(zone, neighborhood) %>% 
  count(zone, sort = TRUE)

geocoded_parking_locations %>% 
  filter(str_detect(zone, "Bloomfield")) %>% 
  distinct(neighborhood)

geocoded_parking_locations %>% 
  mutate(neighborhood_updated = case_when(str_detect(zone, "NorthSide") ~ "NorthSide",
                                          str_detect(zone, "Uptown") ~ "Uptown",
                                          TRUE ~ neighborhood)) %>% 
  distinct(zone, neighborhood_updated) %>% 
  #count(zone, sort = TRUE)
  View()

geocoded_parking_locations %>% 
  distinct(zone, neighborhood) %>% 
  write_csv("data/geocoded_parking_locations.csv")