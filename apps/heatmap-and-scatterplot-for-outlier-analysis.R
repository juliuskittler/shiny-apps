library(plotly)
library(shiny)
library(tidyr)
library(dplyr)

# Data -------------------------------------------------------------------------

# Sample 50 observations 
df = GGally::psychademic
set.seed(1)
female = df %>% filter(sex == "female") %>% sample_n(25, replace = FALSE)
male = df %>% filter(sex == "male") %>% sample_n(25, replace = FALSE)
df_orig = rbind(female, male)

# Scale the numeric data 
df_scaled = scale(df_orig %>% dplyr::select(-sex, -motivation))

# Add ID to data
df = as.data.frame(df_scaled)# Convert to data frame  
df = cbind(.ID = 1:50, df)# Create .ID   

# UI ---------------------------------------------------------------------------

ui = fluidPage(
  titlePanel("Heatmap with Scatterplot for Outlier Analysis (psychademic)"),
  fluidRow(br(), mainPanel(plotlyOutput("heat")), 
           sidebarPanel(verbatimTextOutput("selection"))),
  fluidRow(mainPanel(plotlyOutput("scatterplot")), 
           sidebarPanel(selectInput("y", "y-variable: ",
                                    c("Locus of control" = "locus_of_control", 
                                      "Self concept" = "self_concept", 
                                      "Reading" = "read",
                                      "Writing" = "write",
                                      "Math" = "math", 
                                      "Science" = "science"), 
                                    selected = "math"),
                        selectInput("x", "x-variable: ",
                                    c("Locus of control" = "locus_of_control", 
                                      "Self concept" = "self_concept", 
                                      "Reading" = "read",
                                      "Writing" = "write",
                                      "Math" = "math", 
                                      "Science" = "science"), 
                                    selected = "write")))
)

# Server -----------------------------------------------------------------------

server = function(input, output, session) {
  output$heat = renderPlotly({
    
    # IMPORTANT: z needs to be a matrix!
    # IMPORTANT: specify the source for matching with event_data
    plot_ly(x = colnames(df[-1]), y = df$.ID, z = as.matrix(df[, -1]), 
            colors = colorRamp(c("yellow", "red")), showscale = FALSE, 
            type = "heatmap", source = "heatplot") %>%
      layout(title = "Heatmap", yaxis = list(title = "", showgrid = FALSE, 
                                             tickmode = "auto", 
                                             showticklabels = FALSE, ticks = ""))
  })
  
  output$selection = renderPrint({
    s = event_data("plotly_click", source = "heatplot")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      s_list = as.list(s)
      cat("x: ", s$x, "\n")
      cat("y: ", s$y, "\n")
      cat("z: ", s$z, "\n")
    }
  })
  
  output$scatterplot = renderPlotly({
    
    s = event_data("plotly_click", source = "heatplot")
    
    # Plot WITH highlight
    if (length(s$y) > 0) { 
      
      df_select = df %>% filter(.ID == s$y)
      df_notselect = df %>% filter(.ID != s$y)
      
      plot_ly(data = df_notselect[, -1], x = as.formula(paste0("~", input$x)), 
              y = as.formula(paste0("~", input$y))) %>%
        add_markers(name = "not-highlighed", hoverinfo = "text",
                    text = ~paste(paste0("ID: ", s$y),
                                  paste0("y [", input$y, "]: ", eval(as.symbol(input$y))),
                                  paste0("x [", input$x, "]: ", eval(as.symbol(input$x))),
                                  sep = "<br />")) %>%
        add_markers(data = df_select, x = as.formula(paste0("~", input$x)), 
                    y = as.formula(paste0("~", input$y)), symbol = I(23), size = I(40), 
                    color = I(toRGB("red")), name = "highlighed", opacity = 1, 
                    text = ~paste(paste0("ID: ", s$y), hoverinfo = "text",
                                  paste0("y [", input$y, "]: ", eval(as.symbol(input$y))),
                                  paste0("x [", input$x, "]: ", eval(as.symbol(input$x))),
                                  sep = "<br />")) %>%
        layout(xaxis=list(title=input$x), yaxis=list(title=input$y), 
               title = "Scatterplot", showlegend = FALSE)
    } else {
      
      # Plot WITHOUT highlight
      plot_ly(data = df[, -1], x = as.formula(paste0("~", input$x)), 
              y = as.formula(paste0("~", input$y))) %>%
        add_markers(name = "All", hoverinfo = "text",
                    text = ~paste(paste0("ID: ", s$y),
                                  paste0("y [", input$y, "]: ", eval(as.symbol(input$y))),
                                  paste0("x [", input$x, "]: ", eval(as.symbol(input$x))),
                                  sep = "<br />")) %>%
        layout(xaxis=list(title=input$x), yaxis=list(title=input$y),
               title = "Scatterplot")
    }
  })
  
}

shinyApp(ui, server)