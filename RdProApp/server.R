#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    dataframe <- reactive({
        data <- read.csv(input$datafile$datapath)
    })

    output$plot2 <- renderPlot({
        data <- read.csv(input$datafile$datapath)
        k <- input$k
        km.out <- kmeans(data, centers=k,nstart=20)
        plot2 <- plot(data, col = km.out$cluster, 
                       main = km.out$tot.withinss, 
                       xlab = "", ylab = "")

    })

})

# gonzalolezma@gmail.com
# 982820063
