# Robot advisor project

# import libraries

library(shinythemes)
library(shinydashboard)
library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(zoo)
library(xts)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(readxl)
library(scales)
library(dygraphs)
library(highcharter)
library(RColorBrewer)
library(tidyr)
library(PortfolioAnalytics)




# function: convert data type of char to double but keep Date as char, then tranform to xts object

data2xts <- function(x){
  Date_temp <- x$Date
  x$Date <- NULL
  x <- sapply(x, as.numeric)
  x <- as.data.frame(x)
  Date_temp <- as.Date(Date_temp)
  rownames(x) <- Date_temp
  x <- as.xts(x)
  x <- na.fill(x, 0)
  return(x)
  
}

# loading data from Nomura funds

Nomura_Return_All <- read_excel("data/Nomura_Return_All-1.xlsx"
                                ,  col_names = TRUE)
Nomura_Shape <- read_excel("data/Nomura_Sharp-1.xlsx"
                           , sheet = 1, col_names = TRUE)
index_all <- read_excel("data/index-1.xlsx"
                        , sheet = 1, col_names = TRUE)
fund_type <- read_excel("data/nomura_fund_type.xlsx"
                        , sheet = 1, col_names = TRUE) %>% mutate(i = row_number()) %>% 
  spread(Fund_Types_2018, Names) %>% select(-i)
equity_ons <- unlist(as.list(na.omit(fund_type["Equity_onshore"])), use.names = FALSE)
equity_offs <- unlist(as.list(na.omit(fund_type["Equity_offshore"])), use.names = FALSE)
fixedIncome_Gn <- unlist(as.list(na.omit(fund_type["Fixed_income_gn"])), 
                         use.names = FALSE)
fixedIncome_HY <- unlist(as.list(na.omit(fund_type["Fixed_income_hy"])),
                         use.names = FALSE)
balanced_ons <- unlist(as.list(na.omit(fund_type["Balanced_onshore"])),
                       use.names = FALSE)
balanced_offs <- unlist(as.list(na.omit(fund_type["Balanced_offshore"])),
                        use.names = FALSE)
moneyMarket_ons <- unlist(as.list(na.omit(fund_type["Money_market"])),
                          use.names = FALSE)
multiAsset_offs <- unlist(as.list(na.omit(fund_type["Multiasset_offshore"])),
                          use.names = FALSE)
reits <- unlist(as.list(na.omit(fund_type["REITs"])),
                use.names = FALSE)
combinedBalanced_offs <- unlist(as.list(na.omit(fund_type["FoF_balanced_offshore"])),
                                use.names = FALSE)
combinedFixed <- unlist(as.list(na.omit(fund_type["FoF_fixed_offshore"])),
                        use.names = FALSE)

# call fuction to convert to xts object and set NA = 0

Nomura_Return_All <- data2xts(Nomura_Return_All)
Nomura_Shape <- data2xts(Nomura_Shape)
index_all <- data2xts(index_all)

# covert % return to numeric return

Nomura_Return_All <- Nomura_Return_All / 100
index_all <- index_all / 100

# function: calculate cumulative return

cumReturn <- function(x, start_date = "2006-12-29", end_date = "2017-10-31") {
  period <- paste0(start_date, "/", end_date)
  result <- Return.cumulative(x[period, ], geometric = TRUE)
  return(result)
}

# function: calculate annulized return

annReturn <- function(x, start_date = "2006-12-29", end_date = "2017-10-31" ) {
  period <- paste0(start_date, "/", end_date)
  result <- Return.annualized(x[period,], geometric = TRUE)
  return(result)
}

annStd <- function(x, start_date = "2016-12-29", end_date = "2017-10-31") {
  period <- paste0(start_date, "/", end_date)
  result <- StdDev.annualized(x[period], geometric = TRUE)
  return(result)
}

# function: calculate periodical return
# default frequency: "monthly"
# alternative frequency: "weekly", quarterly", "yearly"

periodReturn <- function(x, frequency = "monthly"){
  period = paste0("apply.",frequency,"(x, cumReturn)")
  result <- eval(parse(text = period))
  return(result)
}

# function: calculate up-to-date periodical return
# default period: 3 months
# unit of time: month
# example: past-6-month return >> histPerform(x, monthly_period = 6)

histPerform <- function(x, backPeriod_m = 3) {
  date <- as.POSIXlt(as.Date("2017-10-31"))
  start_date <- date %m-% months(backPeriod_m)
  result <-  cumReturn(x, start_date = start_date, end_date = date)
  return(result)
  
}

# function: calculate the portfolio returns of a fund of funds
# default rebalance_on = NA, or "years", "quarters", "months", "weeks", "days"
# portfolio return is calculated within the same period of time
# weights: a vector. ex: c(0.5, 0.5)
# verbose parameters: returns, contribution, EOP.Weight, BOP.Weight
#                     BOP.Value, EOP.Value (End of Period, Beginning of Period)

fofReturn <- function(x, weights, rebalance_on = NA){
  result <- Return.portfolio(x, weights = weights, rebalance_on  = rebalance_on,
                             verbose = TRUE)
  return(result)
}

# function: output a table of one asset's return by Calendar year and month

tableReturn <- function(x) {
  result <- table.CalendarReturns(x, as.perc = TRUE, geometric = TRUE)
  return(result)
}

# function: find out top n funds in terms of the mean of sharp ratio in a given period
# currentDate: current date as the end of period
# backPeriod_M: # of months of back testing
# n_ranks: top n funds to be selected
# the result will be a 1 X n dataframe

findBestSharp_mean <- function(currentDate, backPeriod_M, n_ranks) {
  date_back <- as.POSIXlt(as.Date(currentDate))
  date_back <- date_back %m-% months(backPeriod_M)
  period <- paste0(as.character(date_back), "/", as.character(currentDate))
  result <- sort(colMeans(Nomura_Shape[period]), decreasing = TRUE)
  result <- as.data.frame(t(result))
  result <- result[,1:n_ranks]
  return(result)
}



allEquity <- union(equity_offs, equity_ons)
allFixed <- union(fixedIncome_Gn, fixedIncome_HY)
allBalanced <- union(balanced_offs, balanced_ons)
otherType <- setdiff(names(Nomura_Return_All), union(union(allEquity, allFixed), allBalanced))



ui <- tagList(
  
      themeSelector(),
                
                #sidebar layout with a input and output definitions                
                navbarPage("NTU EMBA",
                  tabPanel("Equity Funds",
                    sidebarPanel(
                    h4("Performance Plotting"),
                    HTML(paste0("Funds selected by types between the following dates will be plotted,
                                Pick dates between:  <br> ",
                                tags$b(min_date), " and " , tags$b(max_date), ".")),
                    br(),
                   
                    dateRangeInput(inputId = "date",
                                   label = h4("Select Dates:"),
                                   start = "2006-12-29", end = "2017-10-31",
                                   min = "2006-12-29", max = "2017-10-31",
                                   startview = "month"),
                    h4("Select Fund(s):"),
                    wellPanel(
                    
                   
            
                    radioButtons(inputId = "equityType",
                                       label = h4("Equity: "),
                                       choices = c("All On-Shore Equity Funds" = 1, "All Off-Shore Equity Funds" = 2,
                                                   "All Equity Funds" = 3, "Multiple Selections" = 4),
                                                  selected = 1),
                    conditionalPanel("input.equityType == 4",
                      selectInput(inputId = "equitySelected", 
                                 label = h4("Select Multiple Funds:"), 
                                 choices = allEquity, multiple = TRUE, selected = allEquity[1]))
                    
                    ) 
                  ),
                  mainPanel(
                    tabsetPanel(id = "tabspanel", type = "tabs",
                                tabPanel(title = "Cumulative Performance", 
                                         highchartOutput(outputId = "perChart")),
                                        
                                        
                                tabPanel(title = "Return-Risk", 
                                        
                                         highchartOutput(outputId = "rStd")),
                                # New tab panel for Codebook
                                tabPanel(title = "Cumulative Return Ranks", 
                                        
                                         highchartOutput(outputId = "cumR"))
                    ) 
                    
                  )
                  ),
                  tabPanel("Fixed Income Funds", 
                      sidebarPanel(
                      h4("Performance Plotting"),
                      HTML(paste0("Funds selected by types between the following dates will be plotted,
                                         Pick dates between:  <br/> ",
                                         tags$b(min_date), " and " , tags$b(max_date), ".")),
                             br(),
                             
                          dateRangeInput(inputId = "date",
                                            label = h4("Select Dates:"),
                                            start = "2006-12-29", end = "2017-10-31",
                                            min = "2006-12-29", max = "2017-10-31",
                                            startview = "month"),
                          h4("Select Fund(s):"),
                          wellPanel(
                          radioButtons(inputId = "fixedType",
                                               label = h4("Fixed Income: "),
                                               choices = c("General Bond Funds" = 1 ,"High Yield Bond Funds" = 2,
                                                           "All Bond funds" = 3), 
                                               selected = 1), br()
                          )
                         ),
                      mainPanel(tabsetPanel(id = "tabspanel1", type = "tabs",
                                  tabPanel(title = "Cumulative Performance", 
                                           highchartOutput(outputId = "perChart1")),
                                  tabPanel(title = "Return-Risk",
                                           highchartOutput(outputId = "rStd1")),
                                  # New tab panel for Codebook
                                  tabPanel(title = "Cumulative Return Ranks",
                                           highchartOutput(outputId = "cumR1"))
                      ) 
                      
                     )
                        ),
                      
                      
                  tabPanel("Balanced Funds",
                      sidebarPanel(
                      h4("Performance Plotting"),
                      HTML(paste0("Funds selected by types between the following dates will be plotted,
                                             Pick dates between:  <br> ",
                                             tags$b(min_date), " and " , tags$b(max_date), ".")),
                            br(),
                                 
                          dateRangeInput(inputId = "date",
                                            label = h4("Select Dates:"),
                                            start = "2006-12-29", end = "2017-10-31",
                                            min = "2006-12-29", max = "2017-10-31",
                                            startview = "month"),
                          h4("Select Fund(s):"),
                          wellPanel(
                          radioButtons(inputId = "balanceType",
                                             label = h4("Balanced: "),
                                             choices = c("On-Shore Equity & Fixed Income" = 1, "Off-shore Equity & Fixed Income" = 2,
                                                         "All Balanced Funds" = 3),
                                             selected = 1), br()
                           
                          )
                          ),
                      mainPanel(tabsetPanel(id = "tabspanel2", type = "tabs",
                                            tabPanel(title = "Cumulative Performance", 
                                                     highchartOutput(outputId = "perChart2")),
                                            tabPanel(title = "Return-Risk",
                                                     highchartOutput(outputId = "rStd2")),
                                            # New tab panel for Codebook
                                            tabPanel(title = "Cumulative Return Ranks",
                                                     highchartOutput(outputId = "cumR2"))
                      ) 
                      
                      )
                           ),
                  tabPanel("Other Funds",
                           sidebarPanel(
                             h4("Performance Plotting"),
                             HTML(paste0("Funds selected by types between the following dates will be plotted,
                                         Pick dates between:  <br> ",
                                         tags$b(min_date), " and " , tags$b(max_date), ".")),
                             br(),
                             
                             dateRangeInput(inputId = "date",
                                            label = h4("Select Dates:"),
                                            start = "2006-12-29", end = "2017-10-31",
                                            min = "2006-12-29", max = "2017-10-31",
                                            startview = "month"),
                             h4("Select Fund(s):"),
                             wellPanel(
                               selectInput(inputId = "otherSelected", 
                                           label = h4("Select Other Funds:"), 
                                           choices = otherType, multiple = TRUE, selected = otherType[1]),
                               br(), br(), br())
                           
                               
                  ),
                  mainPanel(tabsetPanel(id = "tabspanel3", type = "tabs",
                                        tabPanel(title = "Cumulative Performance", 
                                                 highchartOutput(outputId = "perChart3")),
                                        tabPanel(title = "Return-Risk",
                                                 highchartOutput(outputId = "rStd3")),
                                        # New tab panel for Codebook
                                        tabPanel(title = "Cumulative Return Ranks",
                                                 highchartOutput(outputId = "cumR3"))
                  ) 
                  
                  )
                  ),
                  
              
                  tabPanel("My Robot",
                          # Create a new row for the table.
                          fluidPage(
                            tabsetPanel(id = "myRobot", type = "tabs",
                                        tabPanel(title = "Data Table",
                                                 h4("Select Funds for Optimization(Click the table): "),br(),
                                                DT::dataTableOutput("table"),
                                                column(6, verbatimTextOutput("myPort"))
                        
                                       
                          ), 
                                        tabPanel(title = "Optimization",  br(),
                                                 sidebarPanel(
                                                   
                                                   h4("Enter inputs below and click 'Run!'"),
                                                   
                                                   actionButton("goButton", "Run!"),
                                                   
                                                   tags$hr(),
                                                   
                                                   
                                                   
                                                   ##### UI for data selection
                                                   
                                                   # Selecte a dataset or choose a file to upload
                                                   
                                                   h4("Select a dataset or choose a file to upload"),
                                                   
                                                   
                                                   
                                                   # choose a data set
                                                   
                                                   selectInput("dataset", "Choose a dataset:", 
                                                               
                                                               choices = c("My Portfolio", "All Equities", "All Fixed Incomes",
                                                                           "All Balanceds", "All MultiAssets","All funds"),
                                                               selected = "My Portfolio"),
                                                   
                                                   tags$hr(),
                                                   
                                                   # checkbox for header
                                                   
                                                   checkboxInput('userFile', 'Upload my own data', FALSE),
                                                   
                                                   
                                                   
                                                   conditionalPanel(
                                                     
                                                     condition = "input.userFile == true",
                                                     
                                                     fileInput('file1', 'Choose file to upload',
                                                               
                                                               accept = c(
                                                                 
                                                                 'text/csv',
                                                                 
                                                                 'text/comma-separated-values',
                                                                 
                                                                 'text/tab-separated-values',
                                                                 
                                                                 'text/plain',
                                                                 
                                                                 '.csv',
                                                                 
                                                                 '.tsv'
                                                                 
                                                               )
                                                               
                                                     ),
                                                     
                                                     # checkbox for header
                                                     
                                                     checkboxInput('header', 'Header', TRUE),
                                                     
                                                     
                                                     
                                                     # radio buttons for seperator type
                                                     
                                                     radioButtons('sep', 'Separator',
                                                                  
                                                                  c(Comma=',',
                                                                    
                                                                    Semicolon=';',
                                                                    
                                                                    Tab='\t'),
                                                                  
                                                                  ','),
                                                     
                                                     
                                                     
                                                     # text input for data format
                                                     
                                                     textInput("format", "Index/time Column Format", value = "%Y-%m-%d")
                                                     
                                                   ),
                                                   
                                                   #####
                                                   
                                                   
                                                   
                                                   h3("Constraints"),
                                                   
                                                   h4("Sum of Weights Constraint"),
                                                   
                                                   numericInput("weight_sum", 
                                                                
                                                                "Sum of weights constraint:", 
                                                                
                                                                value=1, 
                                                                
                                                                min=0, 
                                                                
                                                                max=2, 
                                                                
                                                                step=0.01),
                                                   
                                                   
                                                   
                                                   h4("Box Constraints"),
                                                   
                                                   # Box constraints
                                                   
                                                   checkboxInput('box_enabled', 'Enable Box Constraints', TRUE),
                                                   
                                                   
                                                   
                                                   sliderInput("box", 
                                                               
                                                               "Box Constraints:", 
                                                               
                                                               min = 0.05, 
                                                               
                                                               max = 1, 
                                                               
                                                               value = c(0, 1),
                                                               
                                                               step=0.05),
                                                   
                                                   
                                                   
                                                   h3("Objectives"),
                                                   
                                                   
                                                   
                                                   h4("Return Objective"),
                                                   
                                                   checkboxInput('return_enabled', 'Enable Return Objective', FALSE),
                                                   
                                                   selectInput("return_name", "Select a Return Objective:", 
                                                               
                                                               choices = c("mean")),
                                                   
                                                   numericInput("rb_multiplier", 
                                                                
                                                                "Return Multiplier:", 
                                                                
                                                                value=-1, 
                                                                
                                                                min=-10, 
                                                                
                                                                max=0, 
                                                                
                                                                step=1),
                                                   
                                                   
                                                   
                                                   h4("Risk Objective"),
                                                   
                                                   checkboxInput('risk_enabled', 'Enable Risk Objective', TRUE),
                                                   
                                                   selectInput("risk_name", "Select a Risk Objective:", 
                                                               
                                                               choices = c("StdDev", "VaR", "ES")),
                                                   
                                                   numericInput("risk_p", 
                                                                
                                                                "Confidence Level for VaR and ES Calculation:", 
                                                                
                                                                value=0.95, 
                                                                
                                                                min=0.5, 
                                                                
                                                                max=0.99, 
                                                                
                                                                step=0.01),
                                                   
                                                   numericInput("risk_aversion", 
                                                                
                                                                "Risk Aversion Parameter:", 
                                                                
                                                                value=1, 
                                                                
                                                                min=0, 
                                                                
                                                                max=100, 
                                                                
                                                                step=0.1),
                                                   
                                                   helpText("Note: The risk aversion parameter is only used for the quadratic
                                                            
                                                            utility optimization problem (i.e. when the return objective and 
                                                            
                                                            risk objective are enabled and ROI is the optimization method)."),
                                                   
                                                   
                                                   
                                                   numericInput("risk_multiplier", 
                                                                
                                                                "Risk Multiplier:", 
                                                                
                                                                value=1, 
                                                                
                                                                min=0, 
                                                                
                                                                max=10, 
                                                                
                                                                step=1),
                                                   
                                                   
                                                   
                                                   h4("Risk Budget Objective"),
                                                   
                                                   helpText("Note: Risk budget objectives cannot be solved with the ROI optimizatio method"),
                                                   
                                                   checkboxInput('risk_budget_enabled', 'Enable Risk Budget Objective', FALSE),
                                                   
                                                   selectInput("risk_budget_name", "Select a Risk Budget Objective:", 
                                                               
                                                               choices = c("StdDev", "VaR", "ES")),
                                                   
                                                   numericInput("risk_budget_p", 
                                                                
                                                                "Confidence Level for VaR and ES Calculation:", 
                                                                
                                                                value=0.95, 
                                                                
                                                                min=0.5, 
                                                                
                                                                max=0.99, 
                                                                
                                                                step=0.01),
                                                   
                                                   sliderInput("prisk", 
                                                               
                                                               "Percentage Risk:", 
                                                               
                                                               min = -1, 
                                                               
                                                               max = 1, 
                                                               
                                                               value = c(0, 1),
                                                               
                                                               step=0.05),
                                                   
                                                   checkboxInput('min_concentration', 'Minimize Concentration', FALSE),
                                                   
                                                   numericInput("rb_multiplier", 
                                                                
                                                                "Risk Budget Multiplier:", 
                                                                
                                                                value=1, 
                                                                
                                                                min=0, 
                                                                
                                                                max=10, 
                                                                
                                                                step=1),
                                                   
                                                   
                                                   
                                                   h3("Optimization"),
                                                   
                                                   selectInput("optimize_method", "Select Optimization Method:", 
                                                               
                                                               choices = c("ROI", "random", "DEoptim", "pso", "GenSA")),
                                                   
                                                   
                                                   
                                                   numericInput("search_size", 
                                                                
                                                                "Search Size:", 
                                                                
                                                                value=2000, 
                                                                
                                                                min=1000, 
                                                                
                                                                max=10000, 
                                                                
                                                                step=1000)
                                                   
                                                   ),
                                                 mainPanel(
                                                   
                                                   tabsetPanel(
                                                     
                                                     
                                                     
                                                     tabPanel("Optimization",
                                                              
                                                              # print the optimization output
                                                              
                                                              verbatimTextOutput("optimization")
                                                              
                                                     ),
                                                     
                                                     
                                                     
                                                     # plot tab
                                                     
                                                     tabPanel("Plot",
                                                              
                                                              # plot the optimal weights
                                                              
                                                              plotOutput("chart.RiskReward"),
                                                              
                                                              plotOutput("chart.Weights"),
                                                              
                                                              plotOutput("performanceSummary"),
                                                              
                                                              plotOutput("chart.RiskBudget")
                                                              
                                                     ),
                                                     
                                                     
                                                     
                                                     # summary tab
                                                     
                                                     tabPanel("Portfolio Specification", 
                                                              
                                                              # print the portfolio
                                                              
                                                              verbatimTextOutput("portfolio")
                                                              
                                                     ),
                                                     
                                                     tabPanel("Performance Summary")
                                                              
                                                     
                                                     
                                                   )
                                                   
                                                 )
                                                   
                                                 
                                                 )
                          )
                         
                          
                          
                          
                          
                          
                  )
                  
                  
                
                  
                 )
      
)       
                                  
)




