library(tidyverse)
library(lubridate)
library(vroom)
library(hrbrthemes)

options(scipen = 999,
        digits = 4)

theme_set(theme_ipsum())

data <- vroom("data/1ad5394f-d158-46c1-9af7-90a9ef4e0ce1.csv")

source("scripts/covid/munge_parking_locations.R")

df_ts <- data %>% 
  left_join(zone_regions) %>% 
  select(zone_region, start, meter_transactions, mobile_transactions) %>% 
  separate(start, into = c("start_date", "start_time"), remove = TRUE, sep = " ") %>% 
  mutate(start_date = ymd(start_date))

df_ts <- df_ts %>% 
  group_by(zone_region, start_date) %>% 
  summarize(meter_transactions = sum(meter_transactions),
            mobile_transactions = sum(mobile_transactions)) %>% 
  ungroup()

df_ts <- df_ts %>% 
  rowwise() %>% 
  mutate(total_parking_events = meter_transactions + mobile_transactions) %>% 
  ungroup()

df_ts <- df_ts %>% 
  mutate(year = year(start_date),
         day_of_year = yday(start_date),
         week_of_year = week(start_date),
         weekday = wday(start_date, label = TRUE)) %>% 
  select(zone_region, start_date, day_of_year, week_of_year, weekday, everything())

df_ts %>% 
  group_by(zone_region) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  arrange(desc(total_parking_events)) %>% 
  View()

zone_fct <- df_ts %>% 
  group_by(zone_region) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  arrange(total_parking_events) %>% 
  pull(zone_region)
  

df_ts %>% 
  group_by(zone_region, week_of_year) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  mutate(zone_region = factor(zone_region, levels = zone_fct)) %>% 
  ggplot(aes(week_of_year, zone_region, fill = total_parking_events)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal()

sample_zones <- df_ts %>% 
  distinct(zone_region) %>% 
  sample_n(10)

df_ts %>% 
  semi_join(sample_zones) %>% 
  group_by(zone_region, start_date) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  ggplot(aes(start_date, total_parking_events, color = zone_region)) +
  geom_line()

#compare historic vs 2020
#time series with boxplot
df_historical <- df_ts %>% 
  arrange(zone_region, start_date) %>% 
  filter(start_date < "2020-01-01") %>% 
  mutate(year = year(start_date)) %>% 
  group_by(zone_region, year, week_of_year) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  ungroup()

df_historical <- df_historical %>% 
  group_by(zone_region, week_of_year) %>% 
  summarize(median_parking_events_historical = median(total_parking_events)) %>% 
  ungroup()

df_2020 <- df_ts %>% 
  filter(start_date >= "2020-01-01") %>% 
  complete(zone_region, week_of_year, fill = list(total_parking_events = 0)) %>% 
  group_by(zone_region, week_of_year) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  ungroup()

df_combined <- df_2020 %>% 
  left_join(df_historical, by = c("zone_region", "week_of_year")) %>%
  mutate(pct_difference = (total_parking_events - median_parking_events_historical) / median_parking_events_historical)

df_combined

#line chart
df_combined %>% 
  ggplot(aes(week_of_year, pct_difference, group = zone_region)) +
  geom_line(alpha = .3) +
  scale_y_percent()

#tile chart
tile_chart <- df_combined %>% 
  filter(!str_detect(zone_region, "Homewood")) %>% 
  mutate(zone_region = factor(zone_region, levels = zone_fct)) %>% 
  ggplot(aes(week_of_year, zone_region, fill = pct_difference)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  theme(panel.grid = element_blank())

tile_chart

plotly::ggplotly(tile_chart)

#with boxplots
df_combined %>% 
  ggplot(aes(week_of_year, pct_difference, group = week_of_year)) +
  geom_boxplot(outlier.alpha = .3, outlier.size = 1)
  