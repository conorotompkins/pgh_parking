library(tidyverse)
library(lubridate)
library(vroom)
library(hrbrthemes)

options(scipen = 999,
        digits = 4)

theme_set(theme_ipsum())

data <- vroom("data/1ad5394f-d158-46c1-9af7-90a9ef4e0ce1.csv")

glimpse(data)

data %>% 
  distinct(zone) %>% 
  View()

df_ts <- data %>% 
  select(start, meter_transactions, mobile_transactions) %>% 
  separate(start, into = c("start_date", "start_time"), remove = TRUE, sep = " ") %>% 
  mutate(start_date = ymd(start_date))

df_ts <- df_ts %>% 
  group_by(start_date) %>% 
  summarize(meter_transactions = sum(meter_transactions),
            mobile_transactions = sum(mobile_transactions)) %>% 
  ungroup()

df_ts <- df_ts %>% 
  rowwise() %>% 
  mutate(total_parking_events = meter_transactions + mobile_transactions) %>% 
  ungroup()

df_ts %>% 
  ggplot(aes(start_date, total_parking_events)) +
  geom_point()

df_ts <- df_ts %>% 
  mutate(year = year(start_date),
         day_of_year = yday(start_date),
         week_of_year = week(start_date),
         weekday = wday(start_date, label = TRUE)) %>% 
  select(start_date, day_of_year, week_of_year, weekday, everything())

df_ts %>% 
  select(year, week_of_year, total_parking_events) %>% 
  group_by(year, week_of_year) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  group_by(week_of_year) %>% 
  mutate(week_median_parking_events = median(total_parking_events)) %>% 
  ungroup() %>% 
  mutate(year_type = case_when(year == 2020 ~ "2020",
                               year < 2020 ~ "Before times")) %>% 
  ggplot(aes(x = week_of_year, y = total_parking_events, color = year_type, group = year)) +
  geom_line() +
  geom_line(aes(y = week_median_parking_events), color = "black", size = 1) +
  scale_color_manual(values = c("red", "grey"))

#2020 vs historical
data_historical <- df_ts %>% 
  filter(start_date < "2020-01-01") %>% 
  select(year, week_of_year, total_parking_events) %>% 
  group_by(year, week_of_year) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  group_by(week_of_year) %>% 
  summarize(median_historical_events = median(total_parking_events),
            day_count = n()) %>% 
  ungroup()

data_2020 <- df_ts %>% 
  select(start_date, week_of_year, total_parking_events) %>% 
  filter(start_date >= "2020-01-01",
         week_of_year < week(Sys.Date())) %>% 
  group_by(week_of_year) %>% 
  summarize(total_parking_events = sum(total_parking_events))

df <- data_2020 %>% 
  left_join(data_historical)

df %>% 
  mutate(pct_difference = (total_parking_events - median_historical_events) / median_historical_events) %>% 
  ggplot(aes(week_of_year, pct_difference)) +
  geom_jitter(alpha = .3) +
  geom_smooth(span = .4, color = "black")

#analyze weekday vs weekend difference per week, historical vs 2020
df_ts %>% 
  select(start_date, week_of_year, weekday, total_parking_events) %>% 
  mutate(period = case_when(start_date >= "2020-01-01" ~ "2020",
                            start_date < "2020-01-01" ~ "Before times"),
         is_weekend = case_when(weekday %in% c("Sat", "Sun") ~ "weekend",
                                !(weekday %in% c("Sat", "Sun")) ~ "weekday")) %>% 
  mutate(period = fct_relevel(period, "Before times"),
         is_weekend = fct_relevel(is_weekend, "weekday")) %>% 
  group_by(period, is_weekend) %>% 
  summarize(total_parking_events = sum(total_parking_events)) %>% 
  mutate(pct_of_parking_events = total_parking_events / sum(total_parking_events)) %>% 
  ggplot(aes(period, pct_of_parking_events, fill = is_weekend)) +
  geom_col(position = position_dodge(width = 1), color = "black", alpha = .8) +
  scale_y_percent() +
  scale_fill_viridis_d()
  
