
library(shiny)
library(ggrepel)
library(gt)
library(infer)
library(skimr)
library(lubridate)
library(janitor)
library(tidytext)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Content Analysis of Presidential Speeches",
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")),
    tabPanel("Model",
             fluidPage(
                 titlePanel("Content Analysis of Presidential Speeches"), 
                 sidebarLayout( 
                     sidebarPanel( 
                         selectInput("hist", 
                                     "Content Category", 
                                     choices = c("Populism", 
                                                 "Immigration",
                                                 "Environment", 
                                                 "Progressivism",
                                                 "Conservatism"))
        ),
        mainPanel(
           plotOutput("histPlot"))),
        sidebarLayout(
            sidebarPanel(
                selectInput("box",
                            "Content Category",
                            choices = c("Populism", 
                                        "Immigration",
                                        "Environment", 
                                        "Progressivism",
                                        "Conservatism"))
                ),
            mainPanel(
                plotOutput("boxPlot"))))),
        tabPanel("Discussion",
                 titlePanel("Discussion Title"),
                 p("Tour of the modeling choices you made and 
              an explanation of why you made them")))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$histPlot <- renderImage({
        if(input$hist == "Populism"){
            filename <- normalizePath(file.path("pop_hist_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')}
        else if(input$hist == "Immigration") {
            filename <- normalizePath(file.path("img_hist_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')  
        }
        else if(input$hist == "Environment") {
            filename <- normalizePath(file.path("env_hist_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')  
        }
        else if(input$hist == "Progressivism") {
            filename <- normalizePath(file.path("pro_hist_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')  
        }
        else if(input$hist == "Conservatism") {
            filename <- normalizePath(file.path("con_hist_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')  
        }
    }, deleteFile = FALSE)
    
    output$boxPlot <- renderImage({
        if(input$box == "Populism"){
            filename <- normalizePath(file.path("pop_box_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')}
        else if(input$box == "Immigration") {
            filename <- normalizePath(file.path("img_box_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')  
        }
        else if(input$box == "Environment") {
            filename <- normalizePath(file.path("env_box_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')  
        }
        else if(input$box == "Progressivism") {
            filename <- normalizePath(file.path("pro_box_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')  
        }
        else if(input$box == "Conservatism") {
            filename <- normalizePath(file.path("con_box_plot.png"))
            list(src = filename,
                 height = 700,
                 width = 700,
                 alt = 'plot')
        }
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
