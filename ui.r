library(shiny)
library(shinydashboard)
library(knitr)


ui <- dashboardPage(
  dashboardHeader(title = "Diffusion Curve Generator"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home"),
    menuItem("Methods", tabName = "methods"),    
    menuItem("Tutorial", tabName = "tutorial"),
    menuItem(h2("DCG"), tabName = "main"),
    menuItem("Report", tabName = "report"),
    menuItem("FAQs", tabName = "faqs"),
    menuItem("About us", tabName = "info")
  )),
  
  dashboardBody(includeCSS('www/css/main.css'), tabItems(
    tabItem(tabName = "home", box(width = 12, h2("About DCG"), includeMarkdown("www/introduction.md"))),
    
    tabItem(tabName = "methods", 
            tabBox(width = 12, id = "tabset1", 
                   tabPanel("Methods overview", withMathJax(includeMarkdown("www/methods.md"))),
                   #tabPanel("Methods overview", includeHTML("www/methods.html")),
                   tabPanel("Expert elicitation", includeMarkdown("www/elicitation.md")),
                   tabPanel("Bass model", includeMarkdown("www/bass.md"))
            )
    ),
    
    tabItem(tabName = "tutorial", box(width = 12, h3("Tutorial"), includeMarkdown("www/tutorial.md"))),
    
    tabItem(tabName = "main", box(width = 12, 
                                  uiOutput("ElicitationBox"),
                                  box(width = 12, collapsible = TRUE, collapsed = TRUE, title = "Input Distributions", fluidRow(
                                    column(width = 4, numericInput("num_iter", label = "No. Iterations", value = 200, min=0, max=1000)),
                                    column(width = 4, selectInput('agg_method', 'Aggregation method', c("Mixture"="mixture", "Pooling"="average"))),
                                    column(width = 4, actionButton("updateBass", "Update", width="100%")),
                                    column(width = 12, plotOutput("PlotDistNT", height = "200px"))
                                  )),
                                  box(width = 12, collapsible = TRUE, collapsed = TRUE, title = "Output Distributions", fluidRow(
                                    #actionButton("updateBass", "Update"),
                                    plotOutput("PlotDistPQ", height = "200px")
                                  )),
                                  box(width = 12, collapsible = TRUE, collapsed = TRUE, title = "Bass Diffusion Curves", fluidRow(
                                    column(width = 3, 
                                           sliderInput("centVal", label = "Centile(%)", min = 0, max = 100, value = 95),
                                           checkboxInput("centShow", label = "Show centiles", value = TRUE),
                                           checkboxInput("meanShow", label = "Show means", value = TRUE),
                                           checkboxInput("medianShow", label = "Show medians", value = TRUE),
                                           checkboxInput("linesShow", label = "Show curves", value = TRUE),
                                           checkboxInput("dnShow", label = "Show new adoptions", value = TRUE),
                                           numericInput("projT", label = "Projection time", value = 10, min=0, step=1)
                                    ),
                                    column(width = 9, plotOutput("PlotBass", height = "400px"))
                                  )),
                                  box(width = 12, collapsible = TRUE, title = "Summary", verbatimTextOutput("SummaryBass"))
    )),
    tabItem(tabName = "report", box(width = 12, 
                                    h3("Download summary report"),
                                    p("This document contains all the tables and figures generated from the DifGen of your elicitation input."),
                                    radioButtons('format', 'Please select the document format you require', 
                                                 c('PDF', 'HTML', 'Word'),
                                                 inline = TRUE),
                                    downloadButton('downloadReport', 'Download summary report'),
                                    br(), br(), 
                                    p("NB generating the document can take some time.")
    )),
    
    tabItem(tabName = "faqs", box(width = 12, h2("FAQs"), includeMarkdown("www/faq.md"))),
    
    tabItem(tabName = "info", box(width = 12, h2("Contact us"), includeMarkdown("www/contact.md")))
  ))
  
)