#### set-up ####

# description
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-25/readme.md

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
# install.packages("NutrienTrackeR")
library(NutrienTrackeR)

# import data
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')
data(food_composition_data)


#### judge scores ####

judges <- chopped %>%
  filter(!is.na(episode_rating)) %>%
  select(episode_rating, judge1, judge2, judge3) %>%
  pivot_longer(cols = starts_with("judge"), names_to = "judge_number", values_to = "judge") %>%
  mutate(judge = case_when(judge == "Aarón Sanchez" ~ "Aarón Sánchez",
                           judge %in% c("Amanda Freita", "Amanda Frietag") ~ "Amanda Freitag",
                           judge == "Chris Santo" ~ "Chris Santos",
                           judge == "Geoffrey Zacharian" ~ "Geoffrey Zakarian",
                           judge == "Jody William" ~ "Jody Williams",
                           judge == "Missy Robbin" ~ "Missy Robbins",
                           judge == "Maneet Chauhaun" ~ "Maneet Chauhan",
                           TRUE ~ judge))

judge_rating <- judges %>%
  group_by(judge) %>%
  summarise(rating = mean(episode_rating),
            episodes = n()) %>%
  ungroup()

# visualize
ggplot(judge_rating, aes(x = reorder(judge, rating), y = rating, fill = episodes)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Judge") +
  xlab("Episode rating") +
  theme(axis.text.y = element_text(size = 6))

ggplot(judges, aes(x = reorder(judge, episode_rating), y = episode_rating)) +
  stat_summary(fun = "mean", geom = "bar") +
  coord_flip() +
  ylab("Judge") +
  xlab("Episode rating") +
  theme(axis.text.y = element_text(size = 6))


#### edit data ####

# test: separate ingredients
app_test <- str_split(chopped$appetizer, ", ") %>% unlist()

# separate ingredients:
# select columns
# separate appetizer ingredients into 4 columns
# make dataset long
# remove NA's (fewer than 4 ingredients)
appetizers <- chopped %>%
  select(series_episode, appetizer) %>%
  separate(appetizer, c("a1", "a2", "a3", "a4"), sep = ", ") %>%
  pivot_longer(cols = starts_with("a"), names_to = "ingredient", values_to = "food") %>%
  filter(!is.na(food))

# extract food composition data
usda_foods <- food_composition_data[[1]] %>%
  unlist() %>%
  as_tibble() %>%
  rename(water = 'Water (g)',
         energy = 'Energy (kcal)',
         sugar = 'Sugars, total (g)') %>%
  select(food_id:food_group, water, energy, sugar)

# test: food name function
appetizers$food[10]
findFoodName(keywords = c("grapefruit"), food_database = "USDA")
name_test <- findFoodName(keywords = str_split(appetizers$food[10], " ") %>% unlist(), food_database = "USDA")
str(name_test)
names(name_test)
# not able to match ingredients with this food list

# do foods repeat?
appetizers %>%
  group_by(food) %>%
  summarise(episodes = length(series_episode)) %>%
  filter(episodes > 1)
# yes

# separate ingredients for entree and dessert
entrees <- chopped %>%
  select(series_episode, entree) %>%
  separate(entree, c("a1", "a2", "a3", "a4", "a5", "a6"), sep = ", ") %>%
  pivot_longer(cols = starts_with("a"), names_to = "ingredient", values_to = "food") %>%
  filter(!is.na(food))

desserts <- chopped %>%
  select(series_episode, dessert) %>%
  separate(dessert, c("a1", "a2", "a3", "a4", "a5"), sep = ", ") %>%
  pivot_longer(cols = starts_with("a"), names_to = "ingredient", values_to = "food") %>%
  filter(!is.na(food))

# combine ingredients across meal types
ingredients <- appetizers %>%
  select(-ingredient) %>%
  mutate(meal_type = "appetizer") %>%
  full_join(entrees %>%
              select(-ingredient) %>%
              mutate(meal_type = "entree")) %>%
  full_join(desserts %>%
              select(-ingredient) %>%
              mutate(meal_type = "dessert"))

# summarize number of repeats
repeats <- ingredients %>%
  group_by(food) %>%
  mutate(total_repeats = n()) %>%
  group_by(food, total_repeats, meal_type) %>%
  summarise(uses = n()) %>%
  ungroup()

# order food by total repeats
# save the order
repeat_levels <- repeats %>%
  select(food, total_repeats) %>%
  unique() %>%
  mutate(repeat_order = rank(total_repeats), ties.method = "random")

# subset for highest repeats
repeats_high <- repeats %>%
  filter(total_repeats > 10) %>%
  left_join(repeat_levels) %>%
  mutate(food = fct_reorder(food, repeat_order),
         meal_type = fct_relevel(meal_type, "appetizer", "entree"))


#### visualize ####

ggplot(repeats_high, aes(x = food, y = uses, fill = meal_type)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = "Course") +
  xlab("Ingredient") +
  ylab("Uses") +
  ggtitle("Frequently Used Chopped Ingredients") +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black", angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))
