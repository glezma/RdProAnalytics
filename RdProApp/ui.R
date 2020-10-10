#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("RdPro Application"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            fileInput("datafile",'Escoger archivo CSV', accept = ".csv"),
            sliderInput("k",
                        "Number of clusters:",
                        min = 2,
                        max = 10,
                        value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot2")
        )
    )
))
