

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Nomura Funds Performance", windowTitle = "Nomura Funds"),
                #sidebar layout with a input and output definitions                
                sidebarLayout(
                  sidebarPanel(
                    h3("Performance Plotting"),
                    HTML(paste0("Funds selected by types between the following dates will be plotted,
                                Pick dates between:  <br/> ",
                                tags$b(min_date), " and " , tags$b(max_date), ".")),
                    br(),
                    dateRangeInput(inputId = "date",
                                   label = h4("Select dates:"),
                                   start = "2006-12-29", end = "2017-10-31",
                                   min = "2006-12-29", max = "2017-10-31",
                                   startview = "month"),
                    h4("Select fund types(s):"),
                    wellPanel(
                    
                   
            
                    checkboxGroupInput(inputId = "equityType",
                                       label = h4("Equity: "),
                                       choices = c("On-Shore Equity Funds", "Off-Shore Equity Funds"),
                                                  selected = "On-shore Equity Funds"
                                       ),
                    
                    
                    checkboxGroupInput(inputId = "fixedType",
                                       label = h4("Fixed Income: "),
                                       choices = c("General Bond Funds" ,"High Yield Bond Funds"), 
                                                  selected = "General Bond Funds"
                                       ),
                    
                    checkboxGroupInput(inputId = "balanceType",
                                       label = h4("Balanced: "),
                                       choices = c("On-Shore Equity/Fixed Income", "Off-shore Equity/Fixed Income"),
                                       selected = "On-Shore Equity/Fixed Income"
                                       ),
                    
                    checkboxGroupInput(inputId = "otherType",
                                       label = h4("Other Types: "),
                                       choices = c("Off-Shore Multi-assets", "Off-Shore Fund of Funds", "REITS",
                                                   "Money-market funds"),
                                       selected = "Money-Market funds"),
                    
                    numericInput(inputId = "n_rank", 
                                 label = h4("Select number of funds:"), 
                                 min = 1, max = ncol(Nomura_Return_All), 
                                 value = 10)
                    )
                  ),
                  
                  mainPanel(
                    tabsetPanel(id = "tabspanel", type = "tabs",
                                tabPanel(title = "Cumulative Performance", 
                                         highchartOutput(outputId = "perChart"),
                                         br(),
                                         h4(uiOutput(outputId = "n"))),
                                tabPanel(title = "Return-Risk", 
                                         br(),
                                         highchartOutput(outputId = "rStd")),
                                # New tab panel for Codebook
                                tabPanel(title = "Cumulative Return Ranks", 
                                         br(),
                                         highchartOutput(outputId = "cumR"))
                              ) 
                  
                  )
                  
                  
                )
                    
)