
# Load libraries ------------
library(RColorBrewer)
library(tidyverse)
library(leaflet)
library(rgeos)
library(sp)



# Some data input and cleaning -------------
rents <- read.csv("rents_to_2000.csv")
rents$price <- as.integer(rents$rent)
rents2 <- read_csv('rents_cat.csv')

map <-leaflet(data = rents) %>% addProviderTiles("Stamen.TonerLite") %>%
  addCircles()
map


# Set up some useful variables
# neighborhood_names <- names(table(rents$neighborhood))
#county_names <- c("All Counties", names(table(rents$countyname)))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("price", "Price", min(rents$price), max(10000),
                            value = range(rents$price), step = 500
                ),
                #selectInput("neighborhood", "Neighborhoods",
                #            neighborhood_names
                #selectInput("county", "County",
                           # county_names
                #),
               # checkboxInput("legend", "Show legend", TRUE)
  )
)


server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    # select these
    these <- (rents$price >= input$price[1] & rents$price <= input$price[2]) 
    
    
    # subset
    rents[these,]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  #colorpal <- reactive({
  #  colorNumeric(input$colors, rents$price)
  #  colorNumeric("RdYlBu", rents$price, domain = c(0,10000))
  #})
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(rents) %>% addProviderTiles("Stamen.TonerLite") %>%
      fitBounds(-122.659, 37.659645,-122.037708, 37.922)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    #pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles()
        
        #radius = ~price/10, weight = 1, color = "#777777",
                 #fillColor = ~pal(price), fillOpacity = 0.7, popup = ~paste(price))
      
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = rents)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = c(0,10000)
                          # Could you set color on like a log scale?
                          # So that red is 1k, yellow 10k, blue 100k?
      )
    }
  })
}

shinyApp(ui, server)
