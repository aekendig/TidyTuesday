#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libraries
library(shiny)
library(leaflet)
library(tigris)
library(RColorBrewer)
library(tidyverse)
library(maptools)

# import data
dat <- read.csv("beer_states.csv", header = T)
us_states <- states(cb = T)

# create a vector of years
years <- unique(dat$year)

# subset the data
# create a new ID column
dat_new <- filter(dat, year == 2008) %>%
    mutate(ID = paste0(state, year))

# create a new ID column in spatial data
states_new <- us_states
states_new@data$ID <- paste0(states_new@data$STUSPS, 2008)

# merge data
sp_df <- geo_join(states_new, dat_new, by = "ID", how = "inner")
sp_df <- spChFIDs(sp_df, sp_df$ID)

# merge data by year
for(i in 2:length(years)){
    
    # subset the data
    # create a new ID column
    dat_new <- filter(dat, year == years[i]) %>%
        mutate(ID = paste0(state, year))
    
    # create a new ID column in spatial data
    states_new <- us_states
    states_new@data$ID <- paste0(states_new@data$STUSPS, years[i])
    
    # merge data
    # change ID
    sp_new <- geo_join(states_new, dat_new, by = "ID", how = "inner")
    sp_new <- spChFIDs(sp_new, sp_new$ID)
    
    # combine with previous data
    sp_df <- spRbind(sp_df, sp_new)
}

# create color palette
pal <- colorBin("YlOrRd", domain = dat$total, bins = 7)

# user interface
ui <- fluidPage(
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    absolutePanel(top = 10, right = 10,
                  
                  sliderInput("point", "Year:", min = min(sp_df$year), 
                              max = max(sp_df$year), value = min(sp_df$year), sep = ""
                  )
    )
)


server <- function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        sp_df[sp_df$year == input$point, ]
    })
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically
        leaflet(sp_df) %>%
            setView(-114, 41, 2.5) %>% 
            addTiles()  %>%
            addLegend(pal = pal, 
                      values = ~total, 
                      opacity = 0.7, 
                      title = "Total beer produced",
                      position = "bottomleft")
        })
    
    # Incremental changes to the map should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        leafletProxy("map", data = filteredData) %>%
            clearShapes() %>%
            addPolygons(fillColor = ~pal(total),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7)
    })
    
    # # Use a separate observer to recreate the legend as needed.
    # observe({
    #     proxy <- leafletProxy("map", data = quakes)
    #     
    #     # Remove any existing legend, and only if the legend is
    #     # enabled, create a new one.
    #     proxy %>% clearControls()
    #     if (input$legend) {
    #         pal <- colorpal()
    #         proxy %>% addLegend(position = "bottomright",
    #                             pal = pal, values = ~mag
    #         )
    #     }
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
