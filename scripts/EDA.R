library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(tidyr)
#devtools::install_github("tidyverse/tidyr")

httpResponse <- GET("https://data.wprdc.org/api/3/action/datastore_search?resource_id=1ad5394f-d158-46c1-9af7-90a9ef4e0ce1&limit=5000")
httpResponse

results <- fromJSON(content(httpResponse, "text"), flatten = TRUE)
results

data <- results$result$records %>%
  flatten() %>% 
  mutate_at(c("start", "end"), ymd_hms) %>% 
  mutate(weekday = wday(start, label = TRUE),
         hour = hour(start))
glimpse(data)

df <- data %>% 
  mutate(transactions = meter_transactions + mobile_transactions,
         payments = meter_payments + mobile_payments)

df <- data %>% 
  pivot_longer(cols = c("mobile_transactions", "meter_transactions"), 
               names_to = "transaction_type", 
               values_to = "transactions") #%>% 
  #pivot_longer(cols = c("mobile_payments", "meter_payments"),
  #             names_to = "payment_type",
  #             values_to = "payments")
df
df %>% 
  ggplot(aes(start)) +
  geom_freqpoly()

df_tile <- df %>% 
  select(weekday, hour, transactions) %>% 
  complete(weekday, hour = seq(1:24)) %>% 
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
  complete(weekday, hour = seq(0:23)) %>% 
  replace_na(list(meter_transactions = 0)) %>% 
  group_by(weekday, hour) %>% 
  summarize(meter_transactions = sum(meter_transactions)) %>% 
  ggplot(aes(weekday, hour, fill = meter_transactions)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))


df %>% 
  group_by(start) %>% 
  summarize(meter_transactions = sum(meter_transactions)) %>% 
  ggplot(aes(start, meter_transactions)) +
  geom_point()
