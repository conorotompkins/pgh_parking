library(tidyverse)
library(lubridate)
library(vroom)
library(hrbrthemes)
library(broom)
library(heatwaveR)

options(scipen = 999,
        digits = 4)

theme_set(theme_ipsum())

data <- vroom("data/1ad5394f-d158-46c1-9af7-90a9ef4e0ce1.csv")

glimpse(data)

data %>% 
  arrange(zone)
  
# df_ts <- data %>%
#   select(start, meter_transactions, mobile_transactions) %>%
#   separate(start, into = c("start_date", "start_time"), remove = TRUE, sep = " ") %>%
#   mutate(start_date = ymd(start_date)) %>%
#   group_by(start_date) %>%
#   summarize(meter_transactions = sum(meter_transactions),
#             mobile_transactions = sum(mobile_transactions)) %>%
#   ungroup() %>%
#   rowwise() %>%
#   mutate(total_parking_transactions = meter_transactions + mobile_transactions) %>%
#   ungroup() %>%
#   mutate(year = year(start_date),
#          day_of_year = yday(start_date),
#          week_of_year = week(start_date),
#          weekday = wday(start_date, label = TRUE)) %>%
#   group_by(year, week_of_year) %>%
#   mutate(first_date_of_week = min(start_date)) %>% 
#   ungroup() %>% 
#   select(start_date, year, week_of_year, day_of_year, weekday, everything())

# df_ts %>%
#   write_csv("data/summarized_parking_data.csv")

df_ts <- read_csv("data/summarized_parking_data.csv") %>% 
  group_by(year, week_of_year) %>%
  mutate(first_date_of_week = min(start_date)) %>% 
  ungroup() %>% 
  #select(-start_date) %>% 
  select(first_date_of_week, everything())
  

df_ts %>% 
  ggplot(aes(first_date_of_week, total_parking_transactions)) +
  geom_point(alpha = .2, size = .5) +
  labs(x = "Year",
       y = "Total parking transactions") +
  scale_y_comma() +
  scale_x_date(date_labels = "%b %d")

df_ts %>% 
  select(year, week_of_year, total_parking_transactions) %>% 
  group_by(year, week_of_year) %>% 
  summarize(total_parking_transactions = sum(total_parking_transactions)) %>% 
  group_by(week_of_year) %>% 
  mutate(week_median_parking_events = median(total_parking_transactions)) %>% 
  ungroup() %>% 
  mutate(period = case_when(year == 2020 ~ "2020",
                               year < 2020 ~ "Before times")) %>% 
  ggplot(aes(x = week_of_year, y = total_parking_transactions, color = period, group = year)) +
  geom_line() +
  #geom_line(aes(y = week_median_parking_events), color = "black", size = 1) +
  scale_color_manual(values = c("red", "grey")) +
  scale_y_comma() +
  labs(#x = "Week of year",
       y = "Total parking events",
       color = "Period")

#2020 vs historical
data_historical <- df_ts %>% 
  filter(start_date < "2020-01-01") %>% 
  select(year, week_of_year, total_parking_transactions) %>% 
  group_by(year, week_of_year) %>% 
  summarize(total_parking_transactions = sum(total_parking_transactions)) %>% 
  group_by(week_of_year) %>% 
  summarize(median_historical_transactions = median(total_parking_transactions),
            day_count = n()) %>% 
  ungroup()

data_2020 <- df_ts %>% 
  select(start_date, first_date_of_week, week_of_year, total_parking_transactions) %>% 
  filter(start_date >= "2020-01-01",
         #remove current week of data
         week_of_year < week(Sys.Date())) %>% 
  group_by(first_date_of_week, week_of_year) %>% 
  summarize(total_parking_transactions = sum(total_parking_transactions))

df <- data_2020 %>% 
  left_join(data_historical)

df %>% 
  glimpse()

df %>% 
  mutate(pct_difference = (total_parking_transactions - median_historical_transactions) / median_historical_transactions) %>% 
  ggplot(aes(first_date_of_week, pct_difference)) +
  geom_point(alpha = .3) +
  geom_smooth(span = .4, color = "black") +
  scale_y_percent()

df %>% 
  mutate(pct_difference = (total_parking_transactions - median_historical_transactions) / median_historical_transactions) %>% 
  ggplot(aes(x = first_date_of_week, y = 0, y2 = pct_difference)) +
  heatwaveR::geom_flame() +
  geom_line(aes(y = pct_difference), size = 1.5) +
  scale_y_percent() +
  labs(title = "2020 vs. historical average",
       x = "Date",
       y = "Percent difference")

smoothed_line_df <- df %>% 
  mutate(pct_difference = (total_parking_transactions - median_historical_transactions) / median_historical_transactions) %>% 
  select(week_of_year, pct_difference) %>% 
  nest(parking_data = everything()) %>% 
  mutate(model = map(parking_data, ~loess(pct_difference ~ week_of_year, data = .x, span = .3)),
         coeff = map(model, augment))

smoothed_line_df <- smoothed_line_df %>% 
  unnest(parking_data) %>% 
  left_join(unnest(smoothed_line_df, coeff)) %>% 
  select(first_date_of_week, .fitted) %>% 
  mutate(sign = .fitted > 0,
         population = "total")

smoothed_line_df %>% 
  ggplot(aes(x = first_date_of_week)) +
  heatwaveR::geom_flame(aes(y = 0, y2 = .fitted)) +
  geom_line(aes(y = .fitted), size = 1.5) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_percent() +
  labs(title = "2020 vs. historical average",
       x = "Date",
       y = "Percent difference")

smoothed_line_df %>% 
  ggplot(aes(x = first_date_of_week, y = .fitted, fill = sign, color = sign)) +
  geom_area(ymin = 0) +
  #geom_line(aes(group = population))
  geom_line() +
  geom_point()

smoothed_line_df %>% 
  ggplot() +
  geom_ribbon(aes(x = first_date_of_week, ymin = 0, ymax = .fitted,
                  color = sign, fill = sign))
  

#analyze weekday vs weekend difference per week, historical vs 2020
df_ts %>% 
  select(start_date, week_of_year, weekday, total_parking_transactions) %>% 
  mutate(period = case_when(start_date >= "2020-01-01" ~ "2020",
                            start_date < "2020-01-01" ~ "Before times"),
         is_weekend = case_when(weekday %in% c("Sat", "Sun") ~ "weekend",
                                !(weekday %in% c("Sat", "Sun")) ~ "weekday")) %>% 
  mutate(period = fct_relevel(period, "Before times"),
         is_weekend = fct_relevel(is_weekend, "weekday")) %>% 
  group_by(period, is_weekend) %>% 
  summarize(total_parking_transactions = sum(total_parking_transactions)) %>% 
  mutate(pct_of_parking_transactions = total_parking_transactions / sum(total_parking_transactions)) %>% 
  ggplot(aes(period, pct_of_parking_transactions, fill = is_weekend)) +
  geom_col(position = position_dodge(width = 1), color = "black", alpha = .8) +
  scale_y_percent() +
  scale_fill_viridis_d() +
  labs(x = NULL,
       y = "Percent of transactions",
       fill = "Day type")

weekly_pct_difference_df <- data_2020 %>% 
  mutate(weekly_difference = total_parking_transactions - lag(total_parking_transactions),
         weekly_pct_difference = weekly_difference / lag(total_parking_transactions))

weekly_pct_difference_df %>% 
  mutate(max_drop_flag = weekly_pct_difference == min(weekly_pct_difference, na.rm = TRUE),
         max_drop = case_when(max_drop_flag == TRUE ~ weekly_pct_difference,
                              max_drop_flag == FALSE ~ NA_real_)) %>% 
  ggplot(aes(first_date_of_week, weekly_pct_difference)) +
  geom_line() +
  geom_point() +
  geom_point(aes(y = max_drop), color = "red", size = 3) +
  ggrepel::geom_label_repel(aes(y = max_drop, label = scales::percent(max_drop)),
                            direction = "x") +
  scale_y_percent() +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Week-to-week difference",
       x = "Date",
       y = "Percent difference")
