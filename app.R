library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(scales)

# Load data and filter for Nebraska only
accident <- read.csv("accident.csv") %>%
  filter(STATENAME == "Nebraska")


ui <- fluidPage(
  
  titlePanel("Nebraska Car Accidents"),
  
  tabsetPanel(
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("County", "Select County", c("All", unique(accident$COUNTYNAME))),
                 
                 # Dynamic UI based on coloring option
                 uiOutput("filterOptions"),
                 
                 radioButtons("colorBy", "Color by:",
                              choices = c("Rural/Urban" = "RUR_URBNAME", "Fatalities" = "FATALS", "Weather" = "WEATHERNAME"),
                              selected = "RUR_URBNAME")
               ),
               
               mainPanel(
                 leafletOutput("map")
               )
             )
    ),
    tabPanel("Top Counties",
             mainPanel(
               girafeOutput("lollipopPlot")
             )
    )
  )
)


server <- function(input, output, session) {
  
  output$filterOptions <- renderUI({
    if (input$colorBy == "RUR_URBNAME") {
      checkboxGroupInput("LocationType", "Display Location Type:",
                         choices = c("Rural", "Urban"),
                         selected = c("Rural", "Urban"))
    } else if (input$colorBy == "FATALS") {
      checkboxGroupInput("FatalityCount", "Display Fatalities:",
                         choices = c("1 Fatality", "2+ Fatalities"),
                         selected = c("1 Fatality", "2+ Fatalities"))
    } else {
      checkboxGroupInput("WeatherType", "Select Weather Conditions:",
                         choices = unique(accident$WEATHERNAME),
                         selected = unique(accident$WEATHERNAME))
    }
  })
  
  filtered_accident <- reactive({
    filtered <- accident
    
    if (input$County != "All") {
      filtered <- filter(filtered, COUNTYNAME == input$County)
    }
    
    if (input$colorBy == "RUR_URBNAME") {
      if (!is.null(input$LocationType)) {
        filtered <- filter(filtered, RUR_URBNAME %in% input$LocationType)
      }
    } else if (input$colorBy == "FATALS") {
      if (!is.null(input$FatalityCount)) {
        filtered <- filter(filtered,
                           (FATALS == 1 & "1 Fatality" %in% input$FatalityCount) |
                             (FATALS >= 2 & "2+ Fatalities" %in% input$FatalityCount))
      }
    } else {
      if (!is.null(input$WeatherType)) {
        filtered <- filter(filtered, WEATHERNAME %in% input$WeatherType)
      }
    }
    
    filtered
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = filtered_accident(),
        lat = ~ LATITUDE,
        lng = ~ LONGITUD,
        radius = 5,
        
        color = ~ if (input$colorBy == "RUR_URBNAME") {
          ifelse(RUR_URBNAME == "Rural", "red", "blue")
        } else if (input$colorBy == "FATALS") {
          ifelse(FATALS == 1, "orange", "purple")
        } else {
          # Color by weather conditions
          ifelse(WEATHERNAME == "Clear", "darkgreen",
                 ifelse(WEATHERNAME == "Rain", "blue",
                        ifelse(WEATHERNAME == "Snow", "darkgray",
                               "hotpink")))
        },
        
        fillColor = ~ if (input$colorBy == "RUR_URBNAME") {
          ifelse(RUR_URBNAME == "Rural", "red", "blue")
        } else if (input$colorBy == "FATALS") {
          ifelse(FATALS == 1, "orange", "purple")
        } else {
          # Color by weather conditions
          ifelse(WEATHERNAME == "Clear", "darkgreen",
                 ifelse(WEATHERNAME == "Rain", "blue",
                        ifelse(WEATHERNAME == "Snow", "darkgray",
                               "hotpink")))
        },
        
        fillOpacity = 0.7,
        popup = ~ paste("Accident ID:", ST_CASE, "<br>",
                        "Description:", HARM_EVNAME, "<br>",
                        "Fatalities:", FATALS, "<br>",
                        "Weather:", WEATHERNAME)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = if (input$colorBy == "RUR_URBNAME") c("red", "blue") else if (input$colorBy == "FATALS") c("orange", "purple") else c("darkgreen", "blue", "darkgray", "hotpink"),
        labels = if (input$colorBy == "RUR_URBNAME") c("Rural", "Urban") else if (input$colorBy == "FATALS") c("1 Fatality", "2+ Fatalities") else c("Clear", "Rain", "Snow", "Other"),
        title = if (input$colorBy == "RUR_URBNAME") "Location Type" else if (input$colorBy == "FATALS") "Number of Fatalities" else "Weather Conditions"
      )
  })
  
  output$lollipopPlot <- renderGirafe({
    top_counties <- accident %>%
      group_by(COUNTYNAME) %>%
      summarise(AccidentCount = n()) %>%
      arrange(desc(AccidentCount)) %>%
      head(10)
    
    top_counties$color <- rescale(top_counties$AccidentCount, to = c(0, 1))
    
    p <- ggplot(top_counties, aes(x = reorder(COUNTYNAME, AccidentCount), y = AccidentCount)) +
      geom_segment(aes(xend = COUNTYNAME, yend = 0), color = "lightblue") +
      geom_point(aes(color = color), size = 4) +  # Use the color gradient
      scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 0.2, guide = "colourbar") +
      coord_flip() +
      labs(title = "Top 10 Counties with the Most Accidents",
           x = "County",
           y = "Number of Accidents") +
      theme_minimal()
    
    girafe(ggobj = p)
  })
}

shinyApp(ui = ui, server = server)
