library(tidyverse)
library(lubridate)
library(tidyr)
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
