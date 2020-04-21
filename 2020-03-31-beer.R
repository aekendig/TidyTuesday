# load packages
library(tidyverse)
library(usmap)
library(leaflet)
library(tigris)
library(RColorBrewer)

# other map packages
# library(sf)
# library(maps)
# library(mapdata)
# library(gf)
# library(gdal)
# library(maps)

# Get the Data
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# look at data
brewing_materials
str(brewing_materials)
beer_states

# state data
# us_states <- states(class = "sf") # dataframe structure
us_states <- states(cb = T)

# plot state data
# ggplot(data = us_states) +
#   geom_sf() +
#   coord_sf()

# subset data
beer_2017 <- filter(beer_states, year == 2017) %>%
  group_by(state) %>%
  summarise(total = sum(barrels)) %>%
  filter(state != "total")

# plot 2017 total beer
plot_usmap(data = beer_2017, values = "total")

# summarize all data
beer_states_sum <- beer_states %>%
  group_by(state, year) %>%
  summarise(total = sum(barrels)) %>%
  filter(state != "total")

# save data
write_csv(beer_states_sum, "./Google Drive/TidyTuesday/2020-03-31-beer/beer_states.csv")

# join beer and states data
beer_states_poly <- geo_join(us_states, beer_states_sum, by_sp = "STUSPS", by_df = "state", how = "inner")

# subset for one year
beer_states_poly_2017 <- geo_join(us_states, beer_2017, by_sp = "STUSPS", by_df = "state", how = "inner")
names(beer_states_poly_2017)

# total bins
pal <- colorBin("YlOrRd", domain = beer_states_poly_2017$total, bins = 7)

# leaflet map
leaflet(beer_states_poly_2017) %>%
  setView(-114, 41, 2.5) %>% 
  addTiles() %>%
  addPolygons(fillColor = ~pal(total),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal, 
            values = ~total, 
            opacity = 0.7, 
            title = "Total beer produced",
            position = "bottomleft")
