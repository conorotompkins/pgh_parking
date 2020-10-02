library(tidyverse)
library(vroom)

#manual
zone_regions <- vroom("data/1ad5394f-d158-46c1-9af7-90a9ef4e0ce1.csv") %>% 
  distinct(zone) %>%
  separate(zone, into = c("zone_id", "zone_name"), sep = " - ", remove = FALSE) %>%
  mutate(zone_name = str_remove_all(zone_name,"[[:digit:]]$"),
         #zone_name = str_remove(zone_name, "Lot"),
         zone_name = str_remove(zone_name, "\\(+On-street+\\)"),
         #zone_name = str_remove(zone_name, "\\("),
         zone_name = str_squish(zone_name)) %>%
  mutate(zone_region = zone_name,
         zone_region = case_when(str_detect(zone_name, "Carson") ~ "SouthSide",
                                 str_detect(zone_name, "Sidney") ~ "SouthSide",
                                 str_detect(zone_name, "SS & SSW") ~ "SouthSide",
                                 
                                 str_detect(zone_name, "Butler") ~ "Lawrenceville",
                                 
                                 str_detect(zone_name, "Forbes Murray") ~ "Squirrel Hill",
                                 str_detect(zone_name, "Forbes Shady") ~ "Squirrel Hill",
                                 str_detect(zone_name, "JCC/Forbes") ~ "Squirrel Hill",
                                 str_detect(zone_name, "Beacon Bartlett") ~ "Squirrel Hill",
                                 str_detect(zone_name, "Douglas Phillips") ~ "Squirrel Hill",
                                 
                                 str_detect(zone_name, "Ivy Bellefonte") ~ "Shadyside",
                                 
                                 str_detect(zone_name, "Eva Beatty") ~ "East Liberty",
                                 str_detect(zone_name, "Ansley Beatty") ~ "East Liberty",
                                 str_detect(zone_name, "Harvard Beatty Lot") ~ "East Liberty",
                                 str_detect(zone_name, "Penn Circle NW Lot") ~ "East Liberty",
                                 str_detect(zone_name, "Sheridan Kirkwood Lot") ~ "East Liberty",
                                 str_detect(zone_name, "Sheridan Harvard Lot") ~ "East Liberty",
                                 str_detect(zone_name, "Tamello Beatty Lot") ~ "East Liberty",
                                 
                                 str_detect(zone_name, "Centre Craig") ~ "Oakland",
                                 str_detect(zone_name, "Technology Drive") ~ "Oakland",
                                 
                                 str_detect(zone_name, "Friendship Cedarville") ~ "Bloomfield",
                                 str_detect(zone_name, "Taylor Street Lot") ~ "Bloomfield",
                                 
                                 str_detect(zone_name, "Asteroid Warrington") ~ "Allentown",
                                 str_detect(zone_name, "Walter/Warrington Lot") ~ "Allentown",
                                 
                                 str_detect(zone_name, "Brownsville & Sandkey") ~ "Carrick",
                                 
                                 str_detect(zone_name, "Bakery Sq") ~ "Larimer",
                                 
                                 str_detect(zone_name, "Beechview Lot") ~ "Beechview",
                                 
                                 str_detect(zone_name, "Brookline Lot") ~ "Brookline",
                                 
                                 str_detect(zone_name, "East Ohio Street Lot") ~ "NorthSide",
                                 
                                 str_detect(zone_name, "Homewood Zenith Lot") ~ "Homewood",
                                 
                                 str_detect(zone_name, "Main/Alexander Lot") ~ "West End",
                                 
                                 str_detect(zone_name, "Oberservatory Hill Lot") ~ "Observatory Hill",
                                 
                                 str_detect(zone_name, "Shiloh Street Lot") ~ "Mt. Washington",
                                 
                                 TRUE ~ zone_region)) %>%
  select(zone, zone_region) %>%
  arrange(zone_region)

zone_regions %>%
  distinct(zone_region, zone) %>%
  arrange(zone_region) %>%
  View()

zone_regions %>%
  distinct(zone_region, zone) %>% 
  arrange(zone_region) %>% 
  write_csv("data/geocoded_parking_locations.csv")
