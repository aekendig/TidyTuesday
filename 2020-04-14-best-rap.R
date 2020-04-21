# load packages
library(tidyverse)

# import data
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# look at year and gender of rankings
ggplot(data = rankings, aes(x = points, y = year, color = gender)) +
  geom_point()

# transform x-axis
ggplot(data = rankings, aes(x = log(points), y = year, color = gender)) +
  geom_point()

# barplot of year and rankings
ggplot(data = rankings, aes(x = points, fill = cut_interval(year, length = 10))) +
  geom_bar()

# format dataset with year bins
year_labels = c("1970-1980", "1981-1990", "1991-2000", "2001-2010", "2011-2020")

rankings_year <- rankings %>%
  mutate(year_bin = cut_interval(year, length = 10, labels = year_labels))

rankings_year %>%
  select(year, year_bin) %>%
  unique()

# barplot of year and rankings
ggplot(data = rankings, aes(x = points, fill = cut_interval(year, length = 10, labels = year_labels))) +
  geom_bar() +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Points") +
  ylab("Number of tracks") +
  scale_fill_discrete(name = "Year")

# filter data for top ranking tracks
polls_5 <- filter(polls, rank == 5) %>%
  group_by(artist) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

length(unique(polls_5$critic_country))
length(unique(polls_5$artist))

# look at artists by critic country
ggplot(data = polls_5, aes(x = artist, fill = critic_country)) +
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom")

# titles of songs
rankings %>%
  filter(points > 25) %>%
  select(title) %>%
  unique() %>%
  data.frame()

rankings %>%
  filter(points < 3) %>%
  select(title) %>%
  unique() %>%
  data.frame()

# number of 5 rankings and total points
ggplot(data = rankings, aes(x = n5, y = points)) +
  geom_point()

# make data long
rankings_long <- rankings %>%
  gather(key = "score", value = "votes", -c(ID:n))

# figure of points and scores
ggplot(rankings_long, aes(x = points, y = votes, fill = score)) +
  geom_bar(stat = "identity")

# combine data for critic role
unique(polls$critic_rols)
rankings_critic <- polls %>%
  filter(rank == 1) %>%
  select(title, critic_rols) %>%
  right_join(rankings) %>%
  mutate(critic_type = case_when(!(critic_rols %in% c("Critic", "Artist", "Academic", "Author", "DJ", "Producer")) ~ "Other",
                                     TRUE ~ critic_rols)) %>%
  group_by(critic_type) %>%
  summarise(avg_points = mean(points))

# points and critic roles
ggplot(data = rankings_critic, aes(x = critic_type, y = avg_points)) +
  geom_bar(stat = "identity")
