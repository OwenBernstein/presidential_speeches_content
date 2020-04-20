
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("cerulean"),
    "Content Analysis of Presidential Speeches",
    tabPanel("About", 
             column(7,
             h1("Background"),
             p("In recent years, populism, and all of its associated meanings,
             have gained a prominent position in public discourse. In 2017, due
             to the installment of populist leaders around the world and
             increased discussion in international politics, the Cambridge
             Dictionary even declared populism the Word of the Year. Growing
             international concern has been mirrored in the United States as
             activists, media organizations, and politicians have begun to label
             each other, and entire institutions, as dangerous expressions of
             populism."),
             p("Inspired by this development, this project began as an attempt
             to identify and quantify populism in American political discourse.
             Using research methods outlined by Michael Laver and John Garry and
             later applied by Teun Pauwels, Roel Popping, and Pablo Ribera Payá,
             I decided to use the speeches of presidential candidates as sample
             for measuring populism. As I read this associated literature, I
             also decided to expand my project’s focus to include other content
             categories such as immigration, environmentalism, progressivism,
             and conservatism."),
             p("Ultimately, the goal of this project is to identify trends in
             American political discourse relating to my selected content
             categories. I hope to answer questions such as how the use of
             populist language has changed over time, which presidential
             candidates have talked about immigration the most, or which
             political party talks about the environment more frequently."),
             h1("About the Data"),
             p("The data for this project are the assembled campaign speeches of
               presidential candidates from the 2004 election to the 2016
               election. The transcripts of these speeches are publicly
               available. One resource which was incredibly helpful in
               collecting this speeches was the University of California, Santa
               Barbara’s American Presidency Project. This project was started
               in 1999 to develop resources for a university course and has
               since grown into a fairly comprehensive collection of
               presidential public documents. I also gathered speeches from Data
               Society on the website data.world. More details about the data
               can be found in the methods tab. "),
             h1("About Me"),
             p("My name is Owen Bernstein and I am currently an undergraduate at
             Harvard studying government with a specialization in data science. 
             You can reach me at owenbernstein@college.harvard.edu.")),
             tags$style(HTML("body, pre { font-size: 16pt; }"))),
    tabPanel("Model",
             column(3,
                    h3("Histogram Analysis"),
                    p("These histograms display the distribution of language
                      within each candidate’s speeches. Barack Obama, during his
                      2008 campaign, gave significantly more speeches than any
                      other candidate gave during their respective campaigns
                      which somewhat skews the scale of the y axis. However,
                      each histogram still displays interesting information
                      about the candidate’s speeches. For example, Bernie
                      Sanders’ use of populist language has a wide distribution
                      without any obvious concentration of observations. ")),
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
                    p("Box plots offer a depiction of each candidate’s language
                      usage in speeches that allows for easier comparison across
                      candidates. The box plot marks the 25th and 75th
                      percentiles as well as the median percent populist
                      language for each candidate. It also marks outliers,
                      speeches that contain an unusual amount of a certain
                      category of language, as dots. From these plots it is easy
                      to see that Bernie Sander typically uses significantly
                      more populist language than other candidates while Donald
                      Trump uses more language relating to immigration. ")),
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
                    p("The linear regression plots allow us to view the point
                      estimate as well as a 95% confidence interval for a
                      regression of candidate on each content category. These
                      plots show the expected difference in language between a
                      Barack Obama speech and another candidate’s speech. They
                      allow us to statistically compare the expected amount of a
                      certain category of language between candidates. For
                      example, a Bernie Sanders speech is expected to use about
                      1.9% more populist language than a Barack Obama speech and
                      we are 95% confident that this value is between about 1.5%
                      and 2.3%.")),
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
