# Load packages
library(tidyverse)
library(ggrepel)


# Get the Data
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')


# Format date column
gdpr <- gdpr_violations %>%
  mutate(datef = as.Date(date, format = "%m/%d/%Y")) %>%
  rename(country = name)


# number of countries
length(unique(gdpr$country))


# plot violations over time, by country
ggplot(gdpr, aes(x = datef, fill = country)) +
  geom_bar()

# violation before 1980
filter(gdpr, datef < 1980) %>%
  ggplot(aes(x = datef, fill = country)) +
  geom_bar()

# check date conversion
filter(gdpr, datef < 1980) %>%
  select(date, datef, controller, price)

# remove 1970 data
gdpr2 <- filter(gdpr, datef > 1980)

# plot violations over time, by country
ggplot(gdpr2, aes(x = datef, fill = country)) +
  geom_bar()

# rank countries by number of violations
gdpr %>%
  group_by(country) %>%
  count() %>%
  ungroup() %>%
  mutate(country = reorder(country, n)) %>%
  ggplot(aes(x = country, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("number of violations")

# controllers per country
gdpr %>%
  select(country, controller) %>%
  unique() %>%
  group_by(country) %>%
  count() %>%
  ungroup() %>%
  mutate(country = reorder(country, n)) %>%
  ggplot(aes(x = country, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("number of data controllers")

# check for NA
sum(is.na(gdpr$controller))
sum(is.na(gdpr$price))

# violations by controller
gdpr_con <- gdpr %>%
  filter(controller != "Unknown" & price > 0) %>%
  group_by(country) %>%
  summarise(violations = n(),
            controllers = length(unique(controller)),
            cost = sum(price))

# violations by controller figure
ggplot(gdpr_con, aes(x = controllers, y = violations, color = country)) +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  geom_point(aes(size = cost)) +
  geom_text_repel(aes(label = country), size = 3) +
  theme_bw() +
  theme(axis.text = element_text(size = 10, color="black"),
        axis.title = element_text(size = 12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  xlab("Data controllers") +
  ylab("Data violations")
