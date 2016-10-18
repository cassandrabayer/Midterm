
# Load libraries ------------
library(RColorBrewer)
library(tidyr)
library(leaflet)
library(rgeos)
library(sp)



# Some data input and cleaning -------------
rents <- read.csv("rents_to_2000.csv")

map <-leaflet(data = rents) %>% addProviderTiles("Stamen.TonerLite") %>%
  addCircles(popup = ~neighborhood)
map


# Set up some useful variables
# neighborhood_names <- names(table(rents$neighborhood))
county_names <- c("All Counties", names(table(rents$countyname)))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("rent", "rent", min(rents$rent), max(10000),
                            value = range(rents$rent), step = 500
                ),
                #selectInput("neighborhood", "Neighborhoods",
                #            neighborhood_names
                selectInput("county", "County",
                            county_names
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    # select these
    if(input$county == "All Counties"){
      these <- (rents$rent >= input$rent[1] & rents$rent <= input$rent[2])
    }else{
      these <- (rents$rent >= input$rent[1] & rents$rent <= input$rent[2]) & 
               (rents$countyname == input$county)
    }
    
    # subset
    rents[these,]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    #colorNumeric(input$colors, rents$rent)
    colorNumeric("RdYlBu", rents$rent, domain = c(0,10000))
  })
  
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
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~rent/10, weight = 1, color = "#777777",
                 fillColor = ~pal(rent), fillOpacity = 0.7, popup = ~paste(rent)
      )
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
