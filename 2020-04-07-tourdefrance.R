# Load packages
library(tidyverse)

# Get the Data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
stage_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/stage_data.csv')
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')

# Examine data
tdf_stages
stage_data
unique(tdf_stages$Type)

# Winners for stages
ggplot(data = tdf_stages, aes(x = Winner_Country)) +
  geom_bar() +
  theme(axis.text = element_text(angle = 45, hjust = 1))

# Winners by country and type
ggplot(data = tdf_stages, aes(x = Winner_Country, fill = Type)) +
  geom_bar() +
  theme(axis.text = element_text(angle = 45, hjust = 1))

# Edit data to combine stage types
tdf_stages2 <- tdf_stages %>%
  mutate(Type = case_when(Type == "Flat Stage" ~ "Flat stage",
                          Type %in% c("Stage with mountain(s)", "Stage with mountain", "Mountain Stage") ~ "Mountain stage",
                          Type == "Half Stage" ~ "Half stage",
                          TRUE ~ Type),
         Elevation = case_when(Type %in% c("Medium mountain stage", "High mountain stage", "Mountain time trial", "Mountain stage", "Hilly stage", "Transition stage", "Intermediate stage") ~ 1,
                               TRUE ~ 0),
         Timed = case_when(Type %in% c("Individual time trial", "Mountain time trial", "Team time trial") ~ 1,
                           TRUE ~ 0)
  )

# Winners by country and type
ggplot(data = tdf_stages2, aes(x = Winner_Country, fill = Type)) +
  geom_bar() +
  theme(axis.text = element_text(angle = 45, hjust = 1))

# Distance by Date
ggplot(data = tdf_stages2, aes(x = Date, y = Distance, color = as.factor(Elevation))) +
  geom_point()

# Summarize data by country
tdf_sum <- tdf_stages2 %>%
  group_by(Winner_Country) %>%
  mutate(total = n()) %>%
  group_by(Winner_Country, total, Type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / total)

# Winners by country and type
ggplot(data = tdf_sum, aes(x = Winner_Country, y = prop, fill = Type)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 45, hjust = 1))
