library(plotly)
library(shiny)
library(tidyr)
library(dplyr)

# Data -------------------------------------------------------------------------

df_scaled = as.data.frame(scale(datasets::mtcars))
df_scaled$cars = rownames(df_scaled)

# UI ---------------------------------------------------------------------------

ui = fluidPage(
  titlePanel("Superimposed Radar Chart (mtcars)"),
  mainPanel(br(), plotlyOutput("radarchart"), 
            checkboxGroupInput("vars", inline = TRUE,
                               "Select variables:", 
                               c(colnames(df_scaled)[-ncol(df_scaled)]),
                               selected = c(colnames(df_scaled)[-ncol(df_scaled)]))), 
  sidebarPanel(checkboxGroupInput("cars", "Select cars:", c(rownames(df_scaled)), 
                                  selected = "Mazda RX4"))
)

# Server -----------------------------------------------------------------------

server = function(input, output) {
  
  # Reactive function that returns filtered data
  dataInput = reactive({
    validate(need(length(input$cars) > 0, "Please select a car."))
    validate(need(length(input$vars) > 0, "Please select variables."))
    df_sub = df_scaled %>% filter(rownames(.) %in% input$cars) # Filter cars
    df_sub = subset(df_sub, select = c("cars", input$vars)) # Select variables
  })
  
  output$radarchart = renderPlotly({
    
    # Retrieve, split and modify updated data 
    df_sub = dataInput()
    cars = df_sub$cars # Save car names
    df_sub = df_sub %>% dplyr::select(-cars) %>% as.matrix() # Remove cars
    
    # Create plotly object
    p = plot_ly(type='scatterpolar', fill = 'toself') 
    
    # Add all the traces to plotly object
    for (i in 1:nrow(df_sub)){
      p = p %>% add_trace(r=c(df_sub[i,], df_sub[i,1]),
                          theta=c(colnames(df_sub), colnames(df_sub)[1]),
                          name = cars[i])
    }
    
    # Add layout to plotly object with traces
    p = p %>% layout(polar = list(radialaxis = list(visible = T)))
    
  })
  
}

shinyApp(ui = ui, server = server)
