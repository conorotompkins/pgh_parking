library(tidyverse)
library(lubridate)
library(vroom)
library(hrbrthemes)
library(scales)
library(plotly)

options(scipen = 999,
        digits = 4)

theme_set(theme_ipsum())

data <- vroom("data/1ad5394f-d158-46c1-9af7-90a9ef4e0ce1.csv")

glimpse(data)

geocoded_parking_locations <- read_csv("data/geocoded_parking_locations.csv")

geocoded_parking_locations %>% 
  count(zone, sort = TRUE)

geocoded_parking_locations %>% 
  count(zone_region, sort = TRUE) %>% 
  View()

# df_ts_neighborhood <- data %>% 
#   left_join(geocoded_parking_locations) %>% 
#   select(zone_region, start, meter_transactions, mobile_transactions) %>% 
#   separate(start, into = c("start_date", "start_time"), remove = TRUE, sep = " ") %>% 
#   mutate(start_date = ymd(start_date)) %>% 
#   group_by(zone_region, start_date) %>% 
#   summarize(meter_transactions = sum(meter_transactions),
#             mobile_transactions = sum(mobile_transactions)) %>% 
#   ungroup() %>% 
#   rowwise() %>% 
#   mutate(total_parking_events = meter_transactions + mobile_transactions) %>% 
#   ungroup() %>% 
#   mutate(year = year(start_date),
#          day_of_year = yday(start_date),
#          week_of_year = week(start_date),
#          weekday = wday(start_date, label = TRUE)) %>% 
#   select(zone_region, start_date, day_of_year, week_of_year, weekday, everything())
# 
# df_ts_neighborhood %>% 
#   write_csv("data/summarized_parking_data_neighborhood.csv")

df_ts_neighborhood <- read_csv("data/summarized_parking_data_neighborhood.csv")


zone_fct <- df_ts_neighborhood %>% 
  group_by(zone_region) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  arrange(total_parking_events) %>% 
  pull(zone_region)
  

df_ts_neighborhood %>% 
  group_by(zone_region, week_of_year) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  mutate(zone_region = factor(zone_region, levels = zone_fct)) %>% 
  ggplot(aes(week_of_year, zone_region, fill = total_parking_events)) +
  geom_tile() +
  scale_fill_viridis_c(label = comma) +
  coord_equal() +
  labs(x = "Week of year",
       y = "Neighborhood",
       fill = "Total parking transactions") +
  theme(panel.grid = element_blank())

top_zone_regions <- df_ts_neighborhood %>% 
  group_by(zone_region) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  arrange(desc(total_parking_events)) %>% 
  select(zone_region) %>% 
  slice(1:13)

sample_zones <- df_ts_neighborhood %>% 
  distinct(zone_region) %>% 
  sample_n(10)

df_ts_neighborhood %>% 
  semi_join(top_zone_regions) %>% 
  group_by(zone_region, start_date) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  ggplot(aes(start_date, total_parking_events, color = zone_region)) +
  geom_line(show.legend = FALSE)

#compare historic vs 2020
#time series with boxplot
df_historical <- df_ts_neighborhood %>% 
  arrange(zone_region, start_date) %>% 
  filter(start_date < "2020-01-01") %>% 
  group_by(zone_region, year, week_of_year) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  ungroup()

df_historical <- df_historical %>% 
  group_by(zone_region, week_of_year) %>% 
  summarize(median_parking_events_historical = median(total_parking_events)) %>% 
  ungroup()

df_2020 <- df_ts_neighborhood %>% 
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
line_chart <- df_combined %>%
  semi_join(top_zone_regions) %>% 
  ggplot(aes(week_of_year, pct_difference, group = zone_region)) +
  geom_hline(yintercept = 0, lty = 2, alpha = .5) +
  geom_line(alpha = .3) +
  scale_y_percent() +
  labs(title = "2020 vs. historical average",
       subtitle = "Top 13 neighborhoods",
       x = "Week of year",
       y = "Percent difference")

line_chart

line_chart %>% 
  ggplotly()

#tile chart
tile_chart <- df_combined %>% 
  semi_join(top_zone_regions) %>% 
  mutate(zone_region = factor(zone_region, levels = zone_fct)) %>% 
  ggplot(aes(week_of_year, zone_region, fill = pct_difference)) +
  geom_tile() +
  scale_fill_viridis_c(labels = percent) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_equal() +
  labs(title = "2020 vs. historical average",
       x = "Week of year",
       y = "Neighborhood",
       fill = "Percent difference") +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

tile_chart

ggplotly(tile_chart) %>% 
  layout(xaxis = list(showgrid = F),
         yaxis = list(showgrid = F))

#with boxplots
df_combined %>% 
  semi_join(top_zone_regions) %>% 
  ggplot(aes(week_of_year, pct_difference, group = week_of_year)) +
  geom_boxplot(outlier.alpha = .3, outlier.size = 1) +
  geom_hline(yintercept = 0, lty = 2, alpha = .5) +
  scale_y_percent() +
  labs(title = "2020 vs. historical average",
       x = "Week of year",
       y = "Percent difference")

#difference in difference
df_combined %>% 
  semi_join(top_zone_regions) %>% 
  mutate(difference = total_parking_events - median_parking_events_historical) %>% 
  group_by(week_of_year) %>%
  summarize(difference = sum(difference)) %>% 
  mutate(difference_lag = difference - lag(difference)) %>% 
  ungroup() %>% 
  ggplot(aes(week_of_year, difference_lag)) +
  geom_line()
