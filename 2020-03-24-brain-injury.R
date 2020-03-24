# Get the Data

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

# Look at data

tbi_age
unique(tbi_age$type)
unique(tbi_age$injury_mechanism)

tbi_year
unique(tbi_year$type)
unique(tbi_year$injury_mechanism)


# Visualize
library(tidyverse)
library(ggplot2)

ggplot(tbi_year, aes(x = year, y = number_est, color = injury_mechanism)) +
  geom_line() +
  geom_point() +
  facet_grid(~type)

tbi_year %>%
  filter(type == "Deaths") %>%
  ggplot(aes(x = year, y = number_est, color = injury_mechanism)) +
  geom_line() +
  geom_point()

tbi_year %>%
  filter(type == "Hospitalizations") %>%
  ggplot(aes(x = year, y = number_est, color = injury_mechanism)) +
  geom_line() +
  geom_point()

tbi_year %>%
  filter(type == "Emergency Department Visit") %>%
  ggplot(aes(x = year, y = number_est, color = injury_mechanism)) +
  geom_line() +
  geom_point()


# unintentional falls

ufalls_year <- tbi_year %>%
  filter(injury_mechanism == "Unintentional falls") %>%
  group_by(year) %>%
  mutate(number_est = sum(number_est, na.rm = T),
         rate_est = sum(rate_est, na.rm = T))

ggplot(ufalls_year, aes(x = year, y = rate_est)) +
  geom_line()

ggplot(ufalls_year, aes(x = year, y = number_est)) +
  geom_line()

ufalls_age <- tbi_age %>%
  filter(injury_mechanism == "Unintentional Falls") %>%
  group_by(age_group)  %>%
  mutate(number_est = sum(number_est, na.rm = T),
         rate_est = sum(rate_est, na.rm = T)) %>%
  ungroup() %>%
  filter(age_group != "0-17") %>%
  mutate(age_group = fct_relevel(age_group, "0-4", "5-14"))

ggplot(ufalls_age, aes(x = age_group, y = rate_est)) +
  geom_bar(stat = "identity")

ggplot(ufalls_age, aes(x = age_group, y = number_est)) +
  geom_bar(stat = "identity")

# How has the age distribution changed over time?

# most likely injury
injury_age <- tbi_age %>%
  filter(age_group != "0-17") %>%
  group_by(age_group, injury_mechanism) %>%
  mutate(number_est = sum(number_est, na.rm = T),
         rate_est = sum(rate_est, na.rm = T)) %>%
  ungroup() %>%
  mutate(age_group = fct_relevel(age_group, "0-4", "5-14")) %>%
  group_by(age_group) %>%
  mutate(prop = number_est / sum(number_est)) %>%
  ungroup()

ggplot(injury_age, aes(x = age_group, y = rate_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity")

ggplot(injury_age, aes(x = age_group, y = prop, fill = injury_mechanism)) +
  geom_bar(stat = "identity")
