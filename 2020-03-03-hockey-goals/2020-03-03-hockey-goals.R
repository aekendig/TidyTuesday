#### set up ####

# clear all existing data
rm(list=ls())

# load packages
library(tidyverse)
library(mgcv)
library(glmmTMB)

# import data
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')


#### explore data ####

# is game goals a subset of season goals?
select(game_goals, player) %>% 
  unique() %>%
  left_join(select(season_goals, player) %>% mutate(season = 1) %>% unique()) %>%
  filter(is.na(season))
# yes

# are all the 250 players in the season goals?
select(season_goals, player) %>% 
  unique() %>%
  left_join(select(top_250, player) %>% mutate(top = 1) %>% unique()) %>%
  filter(is.na(top))
# yes


#### edit data ####

# add a proportion goals column
season <- season_goals %>%
  mutate(prop_goals = goals / season_games)


#### initial visualizations ####

# how do season goals relate to age?
ggplot(season, aes(x = age, y = prop_goals)) +
  geom_line(aes(color = player)) +
  geom_smooth(color = "black") +
  theme(legend.position = "none")


#### statistics ####

# simplified model
mod <- gam(prop_goals ~ age, data = season)
summary(mod)

# filter data for under 40
# standardize age
season_40 <- season %>%
  filter(age <= 40) %>%
  mutate(age.s = scale(age))

# check variance vs. mean
var(season_40$goals)
mean(season_40$goals)

# fit quadratic model
mod2 <- glmmTMB(goals ~ offset(log(season_games)) + age.s + I(age.s)^2 + (1|player), data = season_40, family = nbinom2)
# didn't converge
mod3 <- glmmTMB(goals ~ offset(log(season_games)) + age.s + I(age.s)^2 + (1|player), data = season_40, family = nbinom1)
# didn't converge
mod4 <- glmmTMB(goals ~ offset(log(season_games)) + age.s + I(age.s)^2 + (1|player), data = season_40, family = poisson)
# didn't converge

# left off looking at this: https://www.r-bloggers.com/explore-longitudinal-data-with-brolgar/