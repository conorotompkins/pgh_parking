zone_regions <- data %>% 
  distinct(zone) %>% 
  separate(zone, into = c("zone_id", "zone_name"), sep = " - ", remove = FALSE) %>% 
  mutate(zone_name = str_remove_all(zone_name,"[[:digit:]]$"),
         zone_name = str_remove(zone_name, "Lot"),
         zone_name = str_remove(zone_name, "(On-street)"),
         zone_name = str_squish(zone_name)) %>% 
  mutate(zone_region = zone_name,
         zone_region = case_when(str_detect(zone_name, "Carson") ~ "SouthSide",
                                 str_detect(zone_name, "Sidney") ~ "SouthSide",
                                 str_detect(zone_name, "SS & SSW") ~ "SouthSide",
                                 str_detect(zone_name, "Butler") ~ "Lawrenceville",
                                 str_detect(zone_name, "Forbes Murray") ~ "Squirrel Hill",
                                 str_detect(zone_name, "Forbes Shady") ~ "Squirrel Hill",
                                 str_detect(zone_name, "JCC/Forbes") ~ "Squirrel Hill",
                                 str_detect(zone_name, "Ivy Bellefonte") ~ "Shadyside",
                                 str_detect(zone_name, "Eva Beatty") ~ "East Liberty",
                                 str_detect(zone_name, "Centre Craig") ~ "Oakland",
                                 str_detect(zone_name, "Friendship Cedarville") ~ "Bloomfield",
                                 TRUE ~ zone_region)) %>% 
  select(zone, zone_region) %>% 
  arrange(zone_region)

zone_regions %>% 
  distinct(zone_region, zone) %>% 
  arrange(zone_region) %>% 
  View()

zone_regions %>% 
  count(zone_region, sort = TRUE) %>% 
  View()