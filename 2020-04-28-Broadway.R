# load packages
library(tidyverse)
library(lubridate)

# import data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

# show length
show_length <- grosses %>%
  mutate(month = month(week_ending),
         year = year(week_ending)) %>%
  left_join(cpi %>%
              mutate(month = month(year_month),
                     year = year(year_month))) %>%
  mutate(avg_price = avg_ticket_price / cpi,
         top_price = top_ticket_price / cpi) %>%
  group_by(show) %>%
  summarise(start_date_grosses = min(week_ending),
            end_date = max(week_ending),
            average_price = mean(avg_ticket_price, na.rm = T),
            max_price = max(top_ticket_price, na.rm = T),
            average_price_cpi = mean(avg_price, na.rm = T),
            max_price_cpi = max(top_price, na.rm = T)) %>%
  left_join(pre_1985_starts) %>%
  mutate(start_date = case_when(is.na(start_date) ~ start_date_grosses,
                                TRUE ~ start_date),
         show_length = end_date - start_date,
         max_price = case_when(max_price == -Inf ~ NA_real_,
                               TRUE ~ max_price),
         max_price_cpi = case_when(max_price_cpi == -Inf ~ NA_real_,
                               TRUE ~ max_price_cpi))

# figures
ggplot(show_length, aes(x = show_length, y = average_price)) +
  geom_point()

ggplot(show_length, aes(x = show_length, y = average_price_cpi)) +
  geom_point()

ggplot(show_length, aes(x = show_length, y = max_price)) +
  geom_point()

ggplot(show_length, aes(x = show_length, y = max_price_cpi)) +
  geom_point()

# select outlier
filter(show_length, average_price > 500)

# theater ticket prices
theaters <- grosses %>%
  mutate(month = month(week_ending),
         year = year(week_ending)) %>%
  left_join(cpi %>%
              mutate(month = month(year_month),
                     year = year(year_month))) %>%
  mutate(avg_price = avg_ticket_price / cpi,
         top_price = top_ticket_price / cpi) %>%
  group_by(theatre) %>%
  summarise(average_price = mean(avg_ticket_price, na.rm = T),
            max_price = max(top_ticket_price, na.rm = T),
            average_price_cpi = mean(avg_price, na.rm = T),
            max_price_cpi = max(top_price, na.rm = T)) %>%
  mutate(max_price = case_when(max_price == -Inf ~ NA_real_,
                               TRUE ~ max_price),
         max_price_cpi = case_when(max_price_cpi == -Inf ~ NA_real_,
                                   TRUE ~ max_price_cpi))

# figures
ggplot(theaters, aes(x = theatre, y = average_price)) + 
  geom_bar()
