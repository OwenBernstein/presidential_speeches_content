
library(shiny)
library(shinythemes)
library(gt)
library(skimr)
library(lubridate)
library(janitor)
library(dotwhisker)
library(tidytext)
library(ggthemes)
library(rsconnect)
library(tidyverse)


load(file = "sentiment_speeches.Rdata")

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("cerulean"),
    "Content Analysis of Presidential Speeches",
    tabPanel("About", 
             column(7,
             h2("Background"),
             p("In recent years, populism has gained a prominent position in
             public discourse. In 2017, due to the installment of populist 
             leaders around the world, the Cambridge Dictionary even declared
             populism the Word of the Year. Growing international concern has
             been mirrored in the United States as activists, media
             organizations, and politicians have begun to label each other, and
             entire institutions, as dangerous expressions of
             populism. While populism is often discussed, its meaning and
               identifiers are still frequently confused. In order to gain a
               more clear understanding of populism, academics from around the
               world have begun to use text analysis to measure populist
               language."),
             p("Inspired by this development, this project began as an attempt
             to identify and quantify populism in American political discourse.
             Using research methods outlined by Michael Laver and John Garry and
             later applied by Teun Pauwels, Roel Popping, and Pablo Ribera Payá,
             I decided to use the speeches of presidential candidates as a sample
             for measuring populism. I
             also decided to expand my project’s focus to include other content
             categories such as immigration, environmentalism, progressivism,
             and conservatism."),
             p("Ultimately, the goal of this project is to identify trends in
             American political discourse relating to these content
             categories. I hope to answer questions such as how the use of
             populist language has changed over time, which presidential
             candidates have talked about immigration the most, or which
             political party talks about the environment more frequently."),
             h2("About the Data"),
             p("To help answer my research question I used the campaign speeches of
               presidential candidates from the 2004 election to the 2016
               election. The transcripts of these speeches are publicly
               available. One resource which was incredibly helpful in
               collecting these speeches was the University of California, Santa
               Barbara’s American Presidency Project. This project was started
               in 1999 to develop resources for a university course and has
               since grown into a fairly comprehensive collection of
               presidential public documents. I also gathered speeches from Data
               Society on the website data.world."),
             h2("About Me"),
             p("My name is Owen Bernstein and I am currently an undergraduate at
             Harvard studying government with a specialization in data science. 
             You can reach me at owenbernstein@college.harvard.edu.")),
             tags$style(HTML("body, pre { color: black; font-size: 12pt; }")),
             column(5, 
             h3("Excerpt from Bernie Sanders speech in Concord, New Hampshire on
                February 9th, 2016:"),
             p("\"No, we will not allow huge tax breaks for billionaires, we
             will not allow packed -- huge cuts to", span("social",
             style = "color:blue"), "security, veterans needs, Medicare,
             MedicAid, and", span("education", style = "color:blue"), "No, we
             will not allow back into the White House a political party which is
             so beholden to the fossil fuel industry that they cannot even
             acknowledge the scientific reality of", span("climate",
             style = "color:green"), "change. The", span("people",
             style = "color:red"), "of New Hampshire have sent a profound
             message to the", span("political establishment,",
             style = "color:red"), "to the economic", span("establishment",
             style = "color:red"), "and by the way, to the media",
             span("establishment", style = "color:red"), "the", span("people",
             style = "color:red"), "here have said is given the enormous crises
             facing our country, it is just too late for the same old, same
             old", span("establishment politics,", style = "color:red"), "and",
             span("establishment ", style = "color:red"), "economics. The",
             span("people", style = "color:red"), "want real change. What the
             American", span("people", style = "color:red"), "are saying -- and,
             by the way, I hear this not just from", span("progressives,",
             style = "color:blue"), "but from", span("conservatives",
             style = "color:orange"), "and from moderates, is that we can no
             longer continue to have a campaign finances system in which Wall
             Street and the billionaire", span("class", style = "color:red"),
             "are able to buy elections. Americans, no matter what their",
             span("political", style = "color:red"), "view may be, understand
             that that is not what democracy is about.\"",
             style = "color:black"),
             h3("Excerpt from Mitt Romney speech at the Conservative Political
                Action Conference on  February 10th, 2012:"),
             p("\"The", span("values", style = "color:orange"), "that allowed my
               parents to achieve their dreams are the same", span("values",
               style = "color:orange"), "they instilled in my siblings and me.
               Those aren't", span("values", style = "color:orange"), "I just
               talk about; they are", span("values", style = "color:orange"),
               "that I live every day. My 42-year marriage to my wife, Ann; the
               life we've built with our five sons; and the faith that sustains
               us — these", span("conservative", style = "color:orange"),
               "constants have shaped my life. In business, if you're not
               fiscally", span("conservative,", style = "color:orange"), "you're
               bankrupt. I spent 25 years balancing budgets, eliminating waste,
               and keeping as far away from government as was humanly possible.
               I did things", span("conservatism", style = "color:orange"), "is
               designed for — I started new businesses and turned around broken
               ones. And I am not", span("ashamed", style = "color:red"), "to
               say that I was very successful at it. I know",
               span("conservatism", style = "color:orange"), "because I have
               lived", span("conservatism.", style = "color:orange"),"\"",
               stle = "color:black"))),
    tabPanel("By Candidate",
             column(4,
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
             column(7,
             selectInput("hist", 
                         "Select a Content Category", 
                         choices = c("Populism", 
                                     "Immigration",
                                     "Environment", 
                                     "Progressivism",
                                     "Conservatism")),
             plotOutput("histPlot")),
             column(4,
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
             column(7,
                    selectInput("box", 
                                "Select a Content Category", 
                                choices = c("Populism", 
                                            "Immigration", 
                                            "Environment", 
                                            "Progressivism",
                                            "Conservatism")), 
                    plotOutput("boxPlot")),
             column(4,
                    h3("Linear Regression Analysis"),
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
             column(7,
                    selectInput("reg", 
                                "Select a Content Category", 
                                choices = c("Populism", 
                                            "Immigration", 
                                            "Environment", 
                                            "Progressivism",
                                            "Conservatism")),
                    plotOutput("regPlot"))),
    tabPanel("By Time",
             column(4,
                    h3("Line Plot Analysis"),
                    p("These line plots of each content category over time allow
                      us to view changes in the usage of certain categories of
                      language over time. For example, the plot of percent
                      populist language over time has an upward slope thus
                      conveying that populist language has become more common
                      in presidential campaign speeches over time. This trend is
                      also true for language relating to immigration but is less
                      clear for language relating to the environment,
                      progressive language, and conservative language. The data
                      for these plots are grouped around election years because
                      these are the times that candidates typically give
                      campaign speeches.")),
             column(7,
                    selectInput("timeLine",
                                "Select a Content Category", 
                                choices = c("Populism", 
                                            "Immigration", 
                                            "Environment", 
                                            "Progressivism",
                                            "Conservatism")),
                    plotOutput("timeLinePlot")),
             column(4,
                    h3("Linear Regression Analysis"),
                    p("The linear regression plots of time on each content
                      category of language allow us to quanitfy the changes in
                      language over time. Included in each table is a point
                      estimate in the estimate column as well as a 95%
                      confidence interval for that estimate. The estimate can be
                      interpreted as the expected change in percent usage of
                      each content category of language per day. For example,
                      every day we expect a 0.0003 percent increase in the use
                      of populist language in a presidential campaign speech and
                      we are 95% confident that this increase is between 0.0002
                      and 0.0004. While this may seem like a small value, over
                      multiple years it becomes a sizable increase in the usage
                      of populist language. There is also a positive correlation
                      between time and language related to immigration as well
                      as between time and conservative language. Ultimately,
                      this means that over time these types of language have
                      become more common in campaign speeches.  ")),
             column(7,
                    selectInput("timeReg",
                                "Select a Content Category", 
                                choices = c("Populism", 
                                            "Immigration", 
                                            "Environment", 
                                            "Progressivism",
                                            "Conservatism")),
                    gt_output("timeRegPlot"))),
    tabPanel("Method",
             column(10,
             h1("Methodology"),
             p("I decided to follow the research model theoretically outlined by
             Michael Laver and John Garry in 2000 and 2003 and put into practice
             in Belgium by Teun Pauwels and Spain by Pablo Ribera Payá. This
             required gathering the transcripts of campaign speeches by each
             candidate during their bid for the presidency. The text of many of
             the speeches were transcribed with slight differences. Many of the
             transcriptions included audience member interjections written as
             “Audience Member: …” and some transcripts included crowd reactions
             such as “applause” or “laughing.” I removed as many of these
             instances as I could, however, I was not able to carefully read
             each speech and therefore there could be some modeling error from
             the differing transcription styles."),
             p("After removing the words not said by the candidate, I counted
             the number of words in each speech. I then counted the number of
             words relating to each content category. The dictionary of words
             I selected was modeled directly from Teun Pauwels dictionary and
             is displayed below. Pauwels study was primarily focused on
             populist ideologies and Flemish Nationalism and therefore the
             dictionaries for immigration, environment, progressivism, and
             conservatism are considerably less reliable and flushed out than
             the dictionary for populism. Furthermore, as Laver and Garry
             explicitly explain, the dictionaries needed for text analysis of
             this sort will vary drastically based on political context. For
             example, words such as wall, child separation, and quota relate to
             immigration in the United States but were not included in the
             dictionary designed for Belgium. Another difficulty in this method
             of content analysis is that each word in the dictionaries can be
             used with varying intention. However, the methods used in other
             literature frequently ignore this possible source of error. Some
             literature suggests hand coding each observation of a word as
             either positive or negative. This method is very labor intensive
             and would limit the number of speeches that could be included in
             the study. Furthermore, this method was not used in the Pauwel
             study that I used as inspiration and I therefore did not follow
             this suggestion. After counting words based on content category,
             I turned the count into a percentage of total words in the speech
             and used this percentage for analysis."),
             gt_output("dictionary"))))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$histPlot <- renderImage({
        if(input$hist == "Populism"){
            filename <- normalizePath(file.path("pop_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')}
        else if(input$hist == "Immigration") {
            filename <- normalizePath(file.path("img_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$hist == "Environment") {
            filename <- normalizePath(file.path("env_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$hist == "Progressivism") {
            filename <- normalizePath(file.path("pro_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$hist == "Conservatism") {
            filename <- normalizePath(file.path("con_hist_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
    }, deleteFile = FALSE)
    
    output$boxPlot <- renderImage({
        if(input$box == "Populism"){
            filename <- normalizePath(file.path("pop_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')}
        else if(input$box == "Immigration") {
            filename <- normalizePath(file.path("img_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$box == "Environment") {
            filename <- normalizePath(file.path("env_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$box == "Progressivism") {
            filename <- normalizePath(file.path("pro_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$box == "Conservatism") {
            filename <- normalizePath(file.path("con_box_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')
        }
    }, deleteFile = FALSE)
    
    output$regPlot <- renderImage({
        if(input$reg == "Populism"){
            filename <- normalizePath(file.path("pop_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')}
        else if(input$reg == "Immigration") {
            filename <- normalizePath(file.path("img_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$reg == "Environment") {
            filename <- normalizePath(file.path("env_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$reg == "Progressivism") {
            filename <- normalizePath(file.path("pro_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$reg == "Conservatism") {
            filename <- normalizePath(file.path("con_reg_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
    }, deleteFile = FALSE)
    
    output$timeLinePlot <- renderImage({
        if(input$timeLine == "Populism"){
            filename <- normalizePath(file.path("pop_time_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')}
        else if(input$timeLine == "Immigration") {
            filename <- normalizePath(file.path("img_time_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$timeLine == "Environment") {
            filename <- normalizePath(file.path("env_time_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$timeLine == "Progressivism") {
            filename <- normalizePath(file.path("pro_time_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
        else if(input$timeLine == "Conservatism") {
            filename <- normalizePath(file.path("con_time_plot.png"))
            list(src = filename,
                 height = 400,
                 width = 750,
                 alt = 'plot')  
        }
    }, deleteFile = FALSE)
    
    
    
    output$timeRegPlot <- render_gt({
        if(input$timeReg == "Populism"){
            expr <- lm(populism_percent ~ date, data = sentiment_speeches) %>% 
                tidy(conf.int = T) %>% 
                select(term, estimate, conf.low, conf.high) %>% 
                gt() %>% 
                tab_spanner(label = "Confidence Interval",
                            columns = c("conf.low", "conf.high")) %>% 
                tab_header("Linear Regression of Date of Speech on Percent
                           Populist Language") %>% 
                cols_label(term = "Term", estimate = "Estimate",
                           conf.low = "Lower Bound",
                           conf.high = "Upper Bound")}
        else if(input$timeReg == "Immigration"){
            expr <- lm(immigration_percent ~ date,
                       data = sentiment_speeches) %>% 
                tidy(conf.int = T) %>% 
                select(term, estimate, conf.low, conf.high) %>% 
                gt() %>% 
                tab_spanner(label = "Confidence Interval",
                            columns = c("conf.low", "conf.high")) %>% 
                tab_header("Linear Regression of Date of Speech on Percent
                           Language Relating to Immigration") %>% 
                cols_label(term = "Term", estimate = "Estimate",
                           conf.low = "Lower Bound",
                           conf.high = "Upper Bound")}
        else if(input$timeReg == "Environment"){
            expr <- lm(environment_percent ~ date,
                       data = sentiment_speeches) %>% 
                tidy(conf.int = T) %>% 
                select(term, estimate, conf.low, conf.high) %>% 
                gt() %>% 
                tab_spanner(label = "Confidence Interval",
                            columns = c("conf.low", "conf.high")) %>% 
                tab_header("Linear Regression of Date of Speech on Percent
                           Language Relating to the Environment") %>% 
                cols_label(term = "Term", estimate = "Estimate",
                           conf.low = "Lower Bound",
                           conf.high = "Upper Bound")}
        else if(input$timeReg == "Progressivism"){
            expr <- lm(progressive_percent ~ date,
                       data = sentiment_speeches) %>% 
                tidy(conf.int = T) %>% 
                select(term, estimate, conf.low, conf.high) %>% 
                gt() %>% 
                tab_spanner(label = "Confidence Interval",
                            columns = c("conf.low", "conf.high")) %>% 
                tab_header("Linear Regression of Date of Speech on Percent
                           Progressive Language") %>% 
                cols_label(term = "Term", estimate = "Estimate",
                           conf.low = "Lower Bound",
                           conf.high = "Upper Bound")}
        else if(input$timeReg == "Conservatism"){
            expr <- lm(conservatism_percent ~ date,
                       data = sentiment_speeches) %>% 
                tidy(conf.int = T) %>% 
                select(term, estimate, conf.low, conf.high) %>% 
                gt() %>% 
                tab_spanner(label = "Confidence Interval",
                            columns = c("conf.low", "conf.high")) %>% 
                tab_header("Linear Regression of Date of Speech on Percent
                           Conservative Language") %>% 
                cols_label(term = "Term", estimate = "Estimate",
                           conf.low = "Lower Bound",
                           conf.high = "Upper Bound")}},
        height = 400,
        width = 750)
    
    output$dictionary <- render_gt(
        expr <- tibble(content_category = c("Populism", "Environment",
                                            "Immigration", "Progressivism",
                                            "Conservatism"),
                       dictionary = c("*deceit*, *treason*, *betray*, *absurd*,
                                      *arrogant*, *promise*, *corrupt*,
                                      *direct*, *elite*, *establishment*,
                                      *ruling*, *caste*, *class*, *mafia*,
                                      *freedom of expression*, *undemocratic*,
                                      *politic*, *propoganda*, *referend*,
                                      *regime*, *admit*, *shame*, *tradition*,
                                      *people*", "*green*, *climate,
                                      *environment*, *heating*, *durable*",
                                      "*asylum*, *halal*, *scarf*, *illegal*,
                                      *immigra*, *Islam*, *Koran*, *Muslim*,
                                      *foreign*", "*progress*, *right*,
                                      *freedom*, *self-disposition*, *handicap*,
                                      *poverty*, *protection*, *honest*,
                                      *equal*, *education*, *pension*, *social*,
                                      *weak*", "*belief*, *famil*, *church*,
                                      *norm*, *porn*, *sex*, *values*,
                                      *conservative*, *custom*")) %>%
            gt() %>% 
            tab_header(title = "Dictionary for Content Analysis") %>% 
            cols_label(content_category = "Content Category",
                       dictionary = "Words"),
        height = 600,
        width = 750)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
