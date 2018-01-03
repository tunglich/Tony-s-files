
# Robot advisor project

# import libraries

library(shinythemes)
library(shiny)

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Nomura Funds Performance", windowTitle = "Nomura"),
                #sidebar layout with a input and output definitions                
                sidebarLayout(
                  sidebarPanel(
                    h3("Performance Plotting"),
                    HTML(paste0("Funds selected between the following dates will be plotted,
                                Pick dates between:  <br/> ",
                                tags$b(min_date), " and " , tags$b(max_date), ".")),
                    br(), br(),
                    dateRangeInput(inputId = "date",
                                   label = h4("Select dates:"),
                                   start = "2006-12-29", end = "2017-10-31",
                                   min = "2006-12-29", max = "2017-10-31",
                                   startview = "month"),
                    numericInput(inputId = "n_rank", 
                                 label = h4("Number of funds to be plotted:"), 
                                 br(),
                                 min = 1, max = 10, 
                                 value = 5)
                    
                  ),
                  
                  mainPanel(
                    tabsetPanel(id = "tabspanel", type = "tabs",
                                tabPanel(title = "Performance Chart", 
                                         plotOutput(outputId = "perChart"),
                                         br(),
                                         h4(uiOutput(outputId = "n"))),
                                tabPanel(title = "Return/Std", 
                                         br(),
                                         plotOutput(outputId = "rStd")),
                                # New tab panel for Codebook
                                tabPanel(title = "Top 10 Cumulative Return", 
                                         br(),
                                         plotOutput(outputId = "cumR"))
                    ) 
                  )
                  
                  
                  
                  
                    )
)