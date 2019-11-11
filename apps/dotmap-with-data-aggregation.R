library(plotly)
library(shiny)
library(tidyr)
library(dplyr)

# Data -------------------------------------------------------------------------

df = datasets::quakes

g = list(
  scope = "world",
  lonaxis = list(range = c(min(df$long) - 10, max(df$long) + 2)),
  lataxis = list(range = c(min(df$lat) - 10, max(df$lat) + 2)),
  projection = NULL, # Alternative: list(type = "equirectangular")
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"), 
  subunitwidth = 0.5,
  countrycolor = toRGB("black")
)

# UI ---------------------------------------------------------------------------

ui = fluidPage(
  titlePanel("Dot Map with Data Aggregation"),
  mainPanel(br(), p("Depending on the number of specified x and y intervals, the 
                    magniture or depth data is discretized and summarized with
                    the mean for all points falling in the discretized interval.
                    This helps to reduce overlap of data points, giving a plot 
                    that captures the same data but is simpler to interpret."), 
            plotlyOutput("map")),
  sidebarPanel(sliderInput("lat", "Number of x-intervals (latitude):", 
                           min = 1, max = 100, value = 50, step = 1), 
               sliderInput("long", "Number of y-intervals (longitude):",
                           min = 1, max = 100, value = 50, step = 1), 
               radioButtons("colorvar", "Color variable:",
                            c("Magnitude" = "mag", "Depth" = "depth"),
                            selected = "mag"), 
               tags$hr(), # Create horizontal line
               checkboxInput("original", "Use original data", FALSE))
)

# Server -----------------------------------------------------------------------

server = function(input, output, session) {
  
  dataInput = reactive({
    
    # Use original data: FALSE 
    if (input$original == FALSE) {
      df %<>% mutate(long_int = cut_interval(long, as.integer(input$long)), 
                     lat_int = cut_interval(lat, as.integer(input$lat))) %>%
        group_by(long_int, lat_int) %>% 
        summarise(long = mean(long), 
                  lat = mean(lat), 
                  mag = mean(mag),
                  depth = mean(depth),
                  colorvar = mean(eval(as.symbol(input$colorvar))), 
                  n = n())
    } else {
      # Use original data: TRUE
      df$colorvar = df[, input$colorvar]
      df$n = 1
      df
    }
    
  })
  
  output$map = renderPlotly({
    
    # Retrieve input
    df = dataInput()
    
    # Create Plot 
    p = plot_geo(df, lat = ~lat, lon = ~long) %>%
      add_markers(
        text = ~paste(paste("Magnitude:", round(mag, 2)),
                      paste("Depth:", round(depth, 2)), 
                      paste("Latitude:", round(lat, 2)),
                      paste("Longitude:", round(long, 2)),
                      paste("Number of points:", n),
                      sep = "<br />"),
        hoverinfo = "text", symbol = I("diamond"), opacity = 0.9,
        color = ~colorvar, colors = colorRamp(c("yellow","red"))) %>%
      colorbar(title = "Color Variable") %>%
      add_annotations(x = 0.5, y = 1, xref = "paper", yref = "paper", showarrow = F, 
                      text = paste0("<b>n = ", nrow(df),"</b>")) %>%
      layout(geo = g)
    
  })
}

shinyApp(ui, server)
