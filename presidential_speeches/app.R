
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("united"),
    "Content Analysis of Presidential Speeches",
    tabPanel("About", 
             column(7,
             h1("Background"),
             p("Project Background and Motivations"),
             h1("About the Data"),
             p("What the data is"),
             h1("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu."))),
    tabPanel("Model",
             column(3,
                    h3("Histogram Analysis"),
                    p("Explain some of the conclusions from the plots"),
                    p("Use this to make new paragraph")),
             column(8,
             selectInput("hist", 
                         "Select a Content Category", 
                         choices = c("Populism", 
                                     "Immigration",
                                     "Environment", 
                                     "Progressivism",
                                     "Conservatism")),
             plotOutput("histPlot")),
             column(3,
                    h3("Box Plot Analysis"),
                    p("Explain box plots here")),
             column(8,
                    selectInput("box", 
                                "Select a Content Category", 
                                choices = c("Populism", 
                                            "Immigration", 
                                            "Environment", 
                                            "Progressivism",
                                            "Conservatism")), 
                    plotOutput("boxPlot")),
             column(3,
                    h3("Linear Regression Plot Analysis"),
                    p("Explanation")),
             column(8,
                    selectInput("reg", 
                                "Select a Content Category", 
                                choices = c("Populism", 
                                            "Immigration", 
                                            "Environment", 
                                            "Progressivism",
                                            "Conservatism")),
                    plotOutput("regPlot"))),
    tabPanel("Analysis",
             h1("Conclusions")),
    tabPanel("Method",
             h1("Methodology"),
             p("This is where I got my method from, include links"),
             h1("Literature"),
             p("Discussion of literature and why it does not match perfectly"),
             h1("Data"),
             p("Where I got the data from")))
    
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$histPlot <- renderImage({
        if(input$hist == "Populism"){
            filename <- normalizePath(file.path("pop_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')}
        else if(input$hist == "Immigration") {
            filename <- normalizePath(file.path("img_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$hist == "Environment") {
            filename <- normalizePath(file.path("env_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$hist == "Progressivism") {
            filename <- normalizePath(file.path("pro_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$hist == "Conservatism") {
            filename <- normalizePath(file.path("con_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
    }, deleteFile = FALSE)
    
    output$boxPlot <- renderImage({
        if(input$box == "Populism"){
            filename <- normalizePath(file.path("pop_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')}
        else if(input$box == "Immigration") {
            filename <- normalizePath(file.path("img_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$box == "Environment") {
            filename <- normalizePath(file.path("env_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$box == "Progressivism") {
            filename <- normalizePath(file.path("pro_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$box == "Conservatism") {
            filename <- normalizePath(file.path("con_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')
        }
    }, deleteFile = FALSE)
    
    output$regPlot <- renderImage({
        if(input$reg == "Populism"){
            filename <- normalizePath(file.path("pop_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')}
        else if(input$reg == "Immigration") {
            filename <- normalizePath(file.path("img_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$reg == "Environment") {
            filename <- normalizePath(file.path("env_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$reg == "Progressivism") {
            filename <- normalizePath(file.path("pro_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
        else if(input$reg == "Conservatism") {
            filename <- normalizePath(file.path("con_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 1000,
                 alt = 'plot')  
        }
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
