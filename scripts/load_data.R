#load data

library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)

httpResponse <- GET("https://data.wprdc.org/api/3/action/datastore_search?resource_id=1ad5394f-d158-46c1-9af7-90a9ef4e0ce1&limit=5000")
httpResponse

results <- fromJSON(content(httpResponse, "text"), flatten = TRUE)
results

#url <- "https://data.wprdc.org/dataset/parking-transactions/resource/1ad5394f-d158-46c1-9af7-90a9ef4e0ce1?view_id=ad3e92b9-4c30-439b-87ba-b62f74cb23f2"
#data <- read_csv(url)

data <- results$result$records %>%
  flatten() %>% 
  mutate_at(c("start", "end"), ymd_hms) %>% 
  mutate(weekday = wday(start, label = TRUE),
         hour = hour(start)) %>% 
  rename(id = `_id`)

geolocations <- read_csv("https://data.wprdc.org/datastore/dump/9ed126cc-3c06-496e-bd08-b7b6b14b4109")
