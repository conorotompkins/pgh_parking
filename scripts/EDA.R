library(tidyverse)
library(lubridate)
library(tidyr)
library(ggrepel)
library(ggforce)
library(ggmap)
#devtools::install_github("tidyverse/tidyr")

theme_set(theme_bw())

source("scripts/load_data.R")

glimpse(data)

df <- data %>% 
  mutate(transactions = meter_transactions + mobile_transactions,
         payments = meter_payments + mobile_payments)

#df <- data %>% 
#  pivot_longer(cols = c("mobile_transactions", "meter_transactions"), 
#               names_to = "transaction_type", 
#               values_to = "transactions") #%>% 
#pivot_longer(cols = c("mobile_payments", "meter_payments"),
#             names_to = "payment_type",
#             values_to = "payments")
#df
df %>% 
  group_by(zone) %>% 
  summarize(transactions = sum(transactions),
            payments = sum(payments)) %>% 
  arrange(-transactions) %>% 
  ggplot(aes(transactions, payments, label = zone)) +
  geom_abline() +
  geom_label()

df %>% 
  ggplot(aes(start)) +
  geom_freqpoly()

df_tile <- df %>% 
  select(weekday, hour, transactions) %>% 
  complete(weekday, hour = c(0:23)) %>% 
  replace_na(list(transactions = 0)) %>% 
  group_by(weekday, hour) %>% 
  summarize(transactions = sum(transactions))

df_tile %>% 
  ggplot(aes(weekday, hour, fill = transactions)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

df %>% 
  group_by(start) %>% 
  summarize(meter_transactions = sum(meter_transactions)) %>% 
  ggplot(aes(start, meter_transactions)) +
  geom_point()

df_geo_labels <- geolocations %>% 
  group_by(zone) %>% 
  summarize(longitude_mean = mean(longitude, na.rm = TRUE),
         latitude_mean =mean(latitude, na.rm = TRUE))

geolocations %>% 
  ggplot(aes(longitude, latitude, color = zone)) +
  geom_point() +
  geom_label_repel(data = df_geo_labels, aes(longitude_mean, latitude_mean, label = zone)) +
  guides(color = FALSE)

geolocations %>% 
  group_by(zone) %>% 
  summarize_at(vars(c("longitude", "latitude")), mean, na.rm = TRUE) %>% 
  ggplot(aes(longitude, latitude, color = zone, label = zone)) +
  geom_label_repel() +
  guides(color = FALSE)

geolocations %>% 
  ggplot(aes(longitude)) +
  geom_density()

geolocations %>% 
  ggplot(aes(longitude, latitude)) +
  geom_point(size = .5) +
  facet_wrap(~zone)

ggmap::get_map(location = "Pittsburgh, PA", zoom = 12, maptype = "roadmap", source = "google") %>% 
  ggmap()

pgh_map <- ggmap::get_map(location = "North Oakland, Pittsburgh, PA", zoom = 12, maptype = "roadmap", source = "google")

top_locations <- df %>% 
  count(zone, sort = TRUE) %>% 
  head(20)

df_geo <- geolocations %>% 
  filter(location_type == "On street") %>% 
  semi_join(top_locations) %>% 
  filter(!is.infinite(longitude), !is.infinite(latitude),
         !is.na(longitude), !is.na(latitude)) 

ggmap(pgh_map) +
  geom_mark_ellipse(data = df_geo, aes(longitude, latitude, fill = zone, label = zone), 
                    expand = .01, label.buffer = unit(15, "mm")) +
  geom_point(data = df_geo, aes(longitude, latitude, color = zone), size = .3) +
  guides(color = FALSE, fill = FALSE) +
  facet_wrap(~location_type)
