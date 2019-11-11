library(shiny)
library(tm)
library(wordcloud)

# Data -------------------------------------------------------------------------

data("crude") # from tm library

mycorpus = tm_map(crude, removePunctuation)
mycorpus = tm_map(crude, function(x) removeWords(x, stopwords("english")))
tdm = TermDocumentMatrix(mycorpus) # Creating term-document matrix
m = as.matrix(tdm)

# Merge all rows, summing up the frequencies of each word
v1 = sort(rowSums(m),decreasing=TRUE)
v2 = v1[-1]

d1 = data.frame(word = names(v1),freq=v1) 
pal1 = brewer.pal(8,"Dark2")
pal1 = pal1[-(1:2)] # Create palette of colors
wordcloud(d1$word,d1$freq, scale=c(10,1),min.freq=2,max.words=100, random.order=F,
          rot.per=.15, colors=pal1, vfont=c("sans serif","plain"))


# UI ---------------------------------------------------------------------------

ui = fluidPage(

  titlePanel("Adjustable Word Cloud"),
  mainPanel(br(), p("Decrease minimum frequency and increase the maximum number
                    of words to display more words in the word cloud."), 
            plotOutput("plot")),
  sidebarPanel(sliderInput("freq", "Minimum Frequency:", min = 1, max = 50, 
                           value = 15),
               sliderInput("max","Maximum Number of Words:", min = 1, max = 300,  
                           value = 100)
  )

)

# Server -----------------------------------------------------------------------

server = function(input, output, session) {
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep = repeatable(wordcloud)
  
  output$plot = renderPlot({
    wordcloud_rep(d1$word, d1$freq, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}


shinyApp(ui = ui, server = server)