library(plotly)
library(dplyr)
library(scales)
library(shiny)

# Data -------------------------------------------------------------------------

df_scaled = as.data.frame(scale(mtcars))
df_scaled$cars = rownames(df_scaled)

# UI ---------------------------------------------------------------------------

ui = fluidPage(
  titlePanel("Juxtaposed Radar Chart (mtcars)"),
  mainPanel(br(), checkboxGroupInput("vars", inline = TRUE,
                               "Select variables:", 
                               c(colnames(df_scaled)[-ncol(df_scaled)]),
                               selected = c(colnames(df_scaled)[-ncol(df_scaled)])), 
            htmlOutput("radarchart")), 
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
  
  output$radarchart = renderUI({
    
    # Retrieve, split and modify updated data 
    df_sub = dataInput()
    
    Ps=list()
    
    for (i in 1:nrow(df_sub)){
      Ps[[i]] = htmltools::tags$div(
        plot_ly(type = 'scatterpolar', 
                r=as.numeric(df_sub[i,-1]),
                theta= colnames(df_sub)[-1], 
                fill="toself")%>%
          add_annotations(
            x= 0.5,
            y= 1,
            xref = "paper",
            yref = "paper",
            text = paste0("<b>",df_sub$cars[i],"</b>"),
            showarrow = F
          )%>%
          layout(#title=df_sub$group[i], 
            margin = list(l = 50, r = 50, b = 50, t = 50, pad = 0)), 
        style="width: 50%;")
    }
    
    h =htmltools::tags$div(style = "display: flex; flex-wrap: wrap", Ps)
    
    htmltools::browsable(h)
    
  })
  
}

shinyApp(ui = ui, server = server)