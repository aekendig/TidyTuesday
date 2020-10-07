#### set-up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
# library(friends)

# import data from package
# data("friends")
# data("friends_emotions")
# data("friends_entities")
# data("friends_info")

# import data from github
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')


#### edit data ####

# character names
unique(friends$speaker)

# order by utterances
main_char <-  friends %>%
  count(speaker) %>%
  arrange(-n) %>%
  head(n = 6) %>%
  select(speaker)

# top characters
friends %>%
  count(speaker) %>%
  arrange(-n) %>%
  head(n = 20)

# emotions and characters
char_emotions <- friends %>%
  inner_join(friends_emotions)

# characters included
unique(char_emotions$speaker)

# main character utterances
friends_main <- friends %>%
  filter(speaker %in% main_char$speaker) %>%
  group_by(season, episode, speaker) %>%
  summarise(utterances = length(utterance)) %>%
  ungroup() %>%
  left_join(friends_info %>%
              select(season, episode, air_date, us_views_millions, imdb_rating))

# overall average
utterance_med <- median(friends_main$utterances)

# main character emotions
friends_main_emotions <- friends %>%
  filter(speaker %in% main_char) %>%
  inner_join(friends_emotions)

# median emotions
friends_emotions_med <- friends_main_emotions %>%
  count(speaker, emotion) %>%
  group_by(emotion) %>%
  summarise(med = median(n))

#### visualize ####

# utterances over time
ggplot(friends_main, aes(x = air_date, y = utterances, color = speaker)) +
  geom_line() +
  theme_bw()

# utterances by character
ggplot(friends_main, aes(x = speaker, y = utterances, color = speaker)) +
  geom_hline(yintercept = utterance_med, color = "blue", linetype = "dashed") +
  geom_boxplot(fill = NA) +
  theme_bw() +
  theme(legend.position = "none")

# views by character utterance
ggplot(friends_main, aes(x = utterances, y = us_views_millions, color = speaker)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~speaker, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

# rating by character utterance
ggplot(friends_main, aes(x = utterances, y = imdb_rating, color = speaker)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~speaker, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

# emotions by season
ggplot(friends_emotions, aes(x = season, fill = emotion)) +
  geom_bar(stat = "count") +
  theme_bw()

# emotions by character
friends_main_emotions %>%
  count(speaker, emotion) %>%
  ggplot(aes(n, emotion, fill = emotion)) +
  geom_vline(data = friends_emotions_med, aes(xintercept = med, color = emotion), linetype = "dashed") +
  geom_col() +
  facet_wrap(~speaker) +
  theme_bw() +
  theme(legend.position = "none")

# emotions by non-main characters
char_emotions %>%
  filter(!(speaker %in% main_char$speaker) & !is.na(speaker)) %>%
  count(emotion) %>%
  ggplot(aes(n, emotion, fill = emotion)) +
  geom_col() +
  theme_bw() +
  theme(legend.position = "none")
