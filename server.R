


server <- function(input, output, session) {
  subset_date <- reactive({
    paste0(input$date[1], "/", input$date[2])
  })


  equity_selected <- eventReactive(input$equityType, {
    req(input$equityType)
    if(input$equityType == 1){
    equity_ons}
    else if (input$equityType == 2) {
    equity_offs} 
    else if (input$equityType == 3) {
    allEquity}
    else input$equitySelected
  })
  
 
  

  observe({
    if(input$equityType == 4){
      equity_selected <- reactive(input$equitySelected)
    }
    output$perChart <- renderHighchart({
      funds_selected_date_return <- Nomura_Return_All[subset_date(), equity_selected()] 
      funds_selected_date_price <-  round((cumprod((1+funds_selected_date_return))-1) * 100, 2)
      
      hc <-highchart(type = "stock") %>% hc_yAxis(labels = list(format = "{value}%"),
                                                  opposite = FALSE, min = -50) %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '{series.name}: <b>{point.y:.2f}%</b><br>')
      
      for (i in 1:length(equity_selected())){
        hc<- hc %>% hc_add_series(funds_selected_date_price[,i], name = colnames(funds_selected_date_price[,i]))
      }
      hc
      })
  })
  

  
  observe({
    if(input$equityType == 4){
      equity_selected <- reactive(input$equitySelected)
    }
  
  output$rStd <- renderHighchart({
    req(input$equityType)
    funds_selected_date <- Nomura_Return_All[subset_date(), equity_selected()]
    a_Return <- annReturn(funds_selected_date)
    a_Std <- annStd(funds_selected_date)
    fund_names <- dimnames(a_Std)[[2]]
    a_Return <- tapply(unname(a_Return), rep(1:nrow(a_Return), ncol(a_Return)), function(x)x)
    a_Return <- unlist(a_Return, use.names = FALSE)
    a_Std <- tapply(unname(a_Std), rep(1:nrow(a_Std), ncol(a_Std)), function(x)x)
    a_Std <- unlist(a_Std, use.names = FALSE)
    MV <- data.frame(Annualized_return = a_Return * 100, Annualized_std = a_Std * 100,
                     fund_names = factor(fund_names, level = unique(fund_names)), sharp = (a_Return - 0)/ a_Std)
    highchart() %>%
      hc_add_series(MV, type = "bubble", hcaes(x = Annualized_std, y = Annualized_return, size = sharp, color = fund_names)) %>%
      hc_yAxis(labels = list(format = "{value:.1f}%")) %>% hc_xAxis(labels = list(format = "{value:.1f}%"))  %>% hc_legend(enabled = FALSE) %>% 
      hc_tooltip(useHTML = TRUE, crosshairs = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Annualized Std:</th><td>{point.x:.2f}%</td></tr><tr><th>Annualized Return:</th><td>{point.y:.2f}%</td></tr>
                 <tr><th>Sharp Ratio:</th><td>{point.z:.2f}%</td></tr>', footerFormat = '</table>') %>% 
      hc_xAxis(title = list(text = "Annualized Std")) %>% hc_yAxis(title = list(text = "Annualized Return"))
      

  })
  })
  
  observe({
    if(input$equityType == 4){
      equity_selected <- reactive(input$equitySelected)
    }
  
  output$cumR <- renderHighchart({
    req(input$equityType)
    cumR <- cumReturn(Nomura_Return_All[, equity_selected()], input$date[1], input$date[2])
    cumR <- sort(colMeans(cumR), decreasing = TRUE)
    fund_names <- names(cumR)
    unname(cumR)
    cumR <- data.frame(culative_return = cumR, fund_names = factor(fund_names, level = unique(fund_names)) , row.names = NULL)
    highchart() %>%
      hc_add_series(cumR[1:length(equity_selected()),], type = "column", hcaes(x = fund_names, y = culative_return * 100,
                                                        color = fund_names)) %>% 
      hc_xAxis(title = list(text = 'Selected Funds', margin = 20)) %>% 
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(labels = "") %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Cumulative Return:</th><td>{point.y:.2f}%</td></tr>', footerFormat = '</table>')
   
                    
                            
  })
  })
  
  fixed_selected <- eventReactive(input$fixedType, {
    req(input$fixedType)
    if(input$fixedType == 1){
      fixedIncome_Gn}
    else if (input$equityType == 2) {
      fixedIncome_HY} 
    else allFixed
  })
  
  output$perChart1 <- renderHighchart({
    funds_selected_date_return <- Nomura_Return_All[subset_date(), fixed_selected()] 
    funds_selected_date_price <-  round((cumprod((1+funds_selected_date_return))-1) * 100, 2)
    
    hc <-highchart(type = "stock") %>% hc_yAxis(labels = list(format = "{value}%"),
                                               opposite = FALSE, min = -50) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '{series.name}: <b>{point.y:.2f}%</b><br>')
    for (i in 1:length(fixed_selected())){
      hc<- hc %>% hc_add_series(funds_selected_date_price[,i], name = colnames(funds_selected_date_price[,i]))
    }
    
    hc 
    
    
  })
  
  output$rStd1 <- renderHighchart({
    req(input$fixedType)
    funds_selected_date <- Nomura_Return_All[subset_date(), fixed_selected()]
    a_Return <- annReturn(funds_selected_date)
    a_Std <- annStd(funds_selected_date)
    fund_names <- dimnames(a_Std)[[2]]
    a_Return <- tapply(unname(a_Return), rep(1:nrow(a_Return), ncol(a_Return)), function(x)x)
    a_Return <- unlist(a_Return, use.names = FALSE)
    a_Std <- tapply(unname(a_Std), rep(1:nrow(a_Std), ncol(a_Std)), function(x)x)
    a_Std <- unlist(a_Std, use.names = FALSE)
    MV <- data.frame(Annualized_return = a_Return * 100, Annualized_std = a_Std * 100,
                     fund_names = factor(fund_names, level = unique(fund_names)), sharp = (a_Return - 0)/ a_Std)
    highchart() %>%
      hc_add_series(MV, type = "bubble", hcaes(x = Annualized_std, y = Annualized_return, size = sharp, color = fund_names)) %>%
      hc_yAxis(labels = list(format = "{value:.1f}%")) %>% hc_xAxis(labels = list(format = "{value:.1f}%"))  %>% hc_legend(enabled = FALSE) %>% 
      hc_tooltip(useHTML = TRUE, crosshairs = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Annualized Std:</th><td>{point.x:.2f}%</td></tr><tr><th>Annualized Return:</th><td>{point.y:.2f}%</td></tr>
                 <tr><th>Sharp Ratio:</th><td>{point.z:.2f}%</td></tr>', footerFormat = '</table>') %>% 
      hc_xAxis(title = list(text = "Annualized Std")) %>% hc_yAxis(title = list(text = "Annualized Return"))
    
    
  })
  
  output$cumR1 <- renderHighchart({
    req(input$fixedType)
    cumR <- cumReturn(Nomura_Return_All[, fixed_selected()], input$date[1], input$date[2])
    cumR <- sort(colMeans(cumR), decreasing = TRUE)
    fund_names <- names(cumR)
    unname(cumR)
    cumR <- data.frame(culative_return = cumR, fund_names = factor(fund_names, level = unique(fund_names)) , row.names = NULL)
    highchart() %>%
      hc_add_series(cumR[1:length(fixed_selected()),], type = "column", hcaes(x = fund_names, y = culative_return * 100,
                                                                               color = fund_names)) %>% 
      hc_xAxis(title = list(text = 'Selected Funds', margin = 20 )) %>% 
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(labels = "") %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Cumulative Return:</th><td>{point.y:.2f}%</td></tr>', footerFormat = '</table>')
  })
  
  balance_selected <- eventReactive(input$balanceType, {
    req(input$balanceType)
    if(input$balanceType == 1){
      balanced_ons}
    else if (input$balanceType == 2) {
      balanced_offs} 
    else allBalanced
  })
  
  output$perChart2 <- renderHighchart({
    funds_selected_date_return <- Nomura_Return_All[subset_date(), balance_selected()] 
    funds_selected_date_price <-  round((cumprod((1+funds_selected_date_return))-1) * 100, 2)
    
    hc <-highchart(type = "stock") %>% hc_yAxis(labels = list(format = "{value}%"),
                                                opposite = FALSE, min = -50) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '{series.name}: <b>{point.y:.2f}%</b><br>')
   
    for (i in 1:length(balance_selected())){
      hc<- hc %>% hc_add_series(funds_selected_date_price[,i], name = colnames(funds_selected_date_price[,i]))
    }
    
    hc 
    
    
  })
  
  output$rStd2 <- renderHighchart({
    req(input$balanceType)
    funds_selected_date <- Nomura_Return_All[subset_date(), balance_selected()]
    a_Return <- annReturn(funds_selected_date)
    a_Std <- annStd(funds_selected_date)
    fund_names <- dimnames(a_Std)[[2]]
    a_Return <- tapply(unname(a_Return), rep(1:nrow(a_Return), ncol(a_Return)), function(x)x)
    a_Return <- unlist(a_Return, use.names = FALSE)
    a_Std <- tapply(unname(a_Std), rep(1:nrow(a_Std), ncol(a_Std)), function(x)x)
    a_Std <- unlist(a_Std, use.names = FALSE)
    MV <- data.frame(Annualized_return = a_Return * 100, Annualized_std = a_Std * 100,
                     fund_names = factor(fund_names, level = unique(fund_names)), sharp = (a_Return - 0)/ a_Std)
    highchart() %>%
      hc_add_series(MV, type = "bubble", hcaes(x = Annualized_std, y = Annualized_return, size = sharp, color = fund_names)) %>%
      hc_yAxis(labels = list(format = "{value:.1f}%")) %>% hc_xAxis(labels = list(format = "{value:.1f}%"))  %>% hc_legend(enabled = FALSE) %>% 
      hc_tooltip(useHTML = TRUE, crosshairs = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Annualized Std:</th><td>{point.x:.2f}%</td></tr><tr><th>Annualized Return:</th><td>{point.y:.2f}%</td></tr>
                 <tr><th>Sharp Ratio:</th><td>{point.z:.2f}%</td></tr>', footerFormat = '</table>') %>% 
      hc_xAxis(title = list(text = "Annualized Std")) %>% hc_yAxis(title = list(text = "Annualized Return"))
    
    
  })
  
  output$cumR2 <- renderHighchart({
    req(input$balanceType)
    cumR <- cumReturn(Nomura_Return_All[, balance_selected()], input$date[1], input$date[2])
    cumR <- sort(colMeans(cumR), decreasing = TRUE)
    fund_names <- names(cumR)
    unname(cumR)
    cumR <- data.frame(culative_return = cumR, fund_names = factor(fund_names, level = unique(fund_names)) , row.names = NULL)
    highchart() %>%
      hc_add_series(cumR[1:length(balance_selected()),], type = "column", hcaes(x = fund_names, y = culative_return * 100,
                                                                              color = fund_names)) %>% 
      hc_xAxis(title = list(text = 'Selected Funds', margin = 20)) %>% 
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(labels = "") %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Cumulative Return:</th><td>{point.y:.2f}%</td></tr>', footerFormat = '</table>')
  })
  
  output$perChart3 <- renderHighchart({
    funds_selected_date_return <- Nomura_Return_All[subset_date(), input$otherSelected] 
    funds_selected_date_price <-  round((cumprod((1+funds_selected_date_return))-1) * 100, 2) 
    
    hc <-highchart(type = "stock") %>% hc_yAxis(labels = list(format = "{value}%"),
                                               opposite = FALSE, min = -50) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '{series.name}: <b>{point.y:.2f}%</b><br>')
    for (i in 1:length(input$otherSelected)){
      hc<- hc %>% hc_add_series(funds_selected_date_price[,i], name = colnames(funds_selected_date_price[,i]))
    }
    
    hc 
    
    
  })
  output$rStd3 <- renderHighchart({
    req(input$otherSelected)
    funds_selected_date <- Nomura_Return_All[subset_date(), input$otherSelected]
    a_Return <- annReturn(funds_selected_date)
    a_Std <- annStd(funds_selected_date)
    fund_names <- dimnames(a_Std)[[2]]
    a_Return <- tapply(unname(a_Return), rep(1:nrow(a_Return), ncol(a_Return)), function(x)x)
    a_Return <- unlist(a_Return, use.names = FALSE)
    a_Std <- tapply(unname(a_Std), rep(1:nrow(a_Std), ncol(a_Std)), function(x)x)
    a_Std <- unlist(a_Std, use.names = FALSE)
    MV <- data.frame(Annualized_return = a_Return * 100, Annualized_std = a_Std * 100,
                     fund_names = factor(fund_names, level = unique(fund_names)), sharp = (a_Return - 0)/ a_Std)
    highchart() %>%
      hc_add_series(MV, type = "bubble", hcaes(x = Annualized_std, y = Annualized_return, size = sharp, color = fund_names)) %>%
      hc_yAxis(labels = list(format = "{value:.1f}%")) %>% hc_xAxis(labels = list(format = "{value:.1f}%"))  %>% hc_legend(enabled = FALSE) %>% 
      hc_tooltip(useHTML = TRUE, crosshairs = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Annualized Std:</th><td>{point.x:.2f}%</td></tr><tr><th>Annualized Return:</th><td>{point.y:.2f}%</td></tr>
                 <tr><th>Sharp Ratio:</th><td>{point.z:.2f}%</td></tr>', footerFormat = '</table>') %>% 
      hc_xAxis(title = list(text = "Annualized Std")) %>% hc_yAxis(title = list(text = "Annualized Return"))
    
    
  })
  
  output$cumR3 <- renderHighchart({
    req(input$otherSelected)
    cumR <- cumReturn(Nomura_Return_All[, input$otherSelected], input$date[1], input$date[2])
    cumR <- sort(colMeans(cumR), decreasing = TRUE)
    fund_names <- names(cumR)
    unname(cumR)
    cumR <- data.frame(culative_return = cumR, fund_names = factor(fund_names, level = unique(fund_names)) , row.names = NULL)
    highchart() %>%
      hc_add_series(cumR[1:length(input$otherSelected),], type = "column", hcaes(x = fund_names, y = culative_return * 100,
                                                                                color = fund_names)) %>% 
      hc_xAxis(title = list(text = 'Selected Funds', margin = 20)) %>% 
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(labels = "") %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Cumulative Return:</th><td>{point.y:.2f}%</td></tr>', footerFormat = '</table>')
  })
  
  fund_names <- names(Nomura_Return_All)
  min_date <- index(Nomura_Return_All[1])
  max_date <- index(Nomura_Return_All[length(index(Nomura_Return_All))])
  
  day1 <- round(Nomura_Return_All[length(Nomura_Return_All[,1]), ]*100,2)
  rownames(day1) <- NULL
  day1 <- unname(t(day1))
  
  month3 <- round(histPerform(Nomura_Return_All, 3)*100,2)
  month3 <- tapply(unname(month3), rep(1:nrow(month3), ncol(month3)), function(x)x) %>% unlist(use.names = FALSE)
  
  
  month6 <- round(histPerform(Nomura_Return_All, 6)*100,2)
  month6 <- tapply(unname(month6), rep(1:nrow(month6), ncol(month6 )), function(x)x) %>% unlist(use.names = FALSE)
  
  year1 <- round(histPerform(Nomura_Return_All, 12)*100,2)
  year1 <- tapply(unname(year1), rep(1:nrow(year1), ncol(year1)), function(x)x) %>% unlist(use.names = FALSE)
  
  year3 <- round(histPerform(Nomura_Return_All, 36)*100,2)
  year3 <- tapply(unname(year3), rep(1:nrow(year3), ncol(year3)), function(x)x) %>% unlist(use.names = FALSE)
  
  year5 <- round(histPerform(Nomura_Return_All, 60)*100,2)
  year5 <- tapply(unname(year5), rep(1:nrow(year5), ncol(year5)), function(x)x) %>% unlist(use.names = FALSE)
  
  data_table <- data.frame(Daily_return = day1, Month_3 = month3, Month_6 = month6, 
                           Year_1 = year1, Year_3 = year3, Year_5 = year5)
  rownames(data_table) <- fund_names
  
  output$table = DT::renderDataTable({
   
    datatable(data_table, class = 'display', rownames = TRUE, filter = 'none', style = 'bootstrap', 
                            caption = 'Culumative Return(%)') %>% formatRound(names(data_table),2) %>% 
                            formatStyle(names(data_table[,1:6]), color = styleInterval(c(-0.000001,0.000001), c('green', 'black', 'red')))
                                        
    
    

  })

  output$myPort = renderPrint({
      s = input$table_rows_selected
      if (length(s)){
        cat('These funds were selected:\n\n')
        cat(rownames(data_table[s,]), sep = '\n')

      }
    })
  
  
  
  
  
  ##### Dynamically choose the data #####
  data(edhec)
  load("data/crsp.short.rda")
  
  my_portfolio <- reactive(
    Nomura_Return_All[,input$table_rows_selected])
  
  datasetSelect <- reactive({
    
    switch(input$dataset,
           
           "My Portfolio" = my_portfolio(),
           
           "All Equities" = Nomura_Return_All[,allEquity],
           
           "All Fixed Incomes" = Nomura_Return_All[,allFixed],
           
           "All Balanceds" = Nomura_Return_All[,allBalanced],
           
           "All MultiAssets" = Nomura_Return_all[,multiAsset_offs],
           "All funds" = Nomura_Return_All
           )
    
  })
  
  
  
  datasetInput <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    
    # and uploads a file, it will be a data frame with 'name',
    
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    
    # column will contain the local filenames where the data can
    
    # be found.
    
    
    
    inFile <- input$file1
    
    
    
    if (is.null(inFile))
      
      return(NULL)
    
    
    
    data <- read.csv(file=inFile$datapath, sep=input$sep, header=input$header, as.is=TRUE)
    
    data.xts <- xts(data[,-1], as.Date(data[,1], format=input$format))
    
    #summary(data.xts)
    
    data.xts
    
  })
  
  
  
  data <- reactive({
    
    if(input$userFile){
      
      if(!is.null(datasetInput())){
        
        data <- datasetInput()
        
      }
      
    } else {
      
      data <- datasetSelect()
      
    }
    
  })
  
  
  
  
  
  portf <- reactive({
    
    input$goButton
    
    isolate({
      
      R <- data()
      
      n <- ncol(R)
      
      funds <- colnames(R)
      
      
      
      if(input$optimize_method != "ROI"){
        
        min_sum <- input$weight_sum - 0.01
        
        max_sum <- input$weight_sum + 0.01
        
      } else {
        
        min_sum <- input$weight_sum
        
        max_sum <- input$weight_sum
        
      }
      
      
      
      init.portf <- portfolio.spec(funds)
      
      init.portf <- add.constraint(init.portf, "weight_sum", 
                                   
                                   min_sum=min_sum, 
                                   
                                   max_sum=max_sum)
      
      if(input$box_enabled){
        
        init.portf <- add.constraint(init.portf, "box", 
                                     
                                     min=input$box[1], 
                                     
                                     max=input$box[2])
        
      }
      
      if(input$return_enabled){
        
        init.portf <- add.objective(init.portf, type="return", 
                                    
                                    name=input$return_name)
        
      }
      
      if(input$risk_enabled){
        
        init.portf <- add.objective(init.portf, type="risk", 
                                    
                                    name=input$risk_name, 
                                    
                                    risk_aversion=input$risk_aversion,
                                    
                                    arguments=list(p=input$risk_p))
        
      }
      
      if(input$risk_budget_enabled){
        
        init.portf <- add.objective(init.portf, type="risk_budget", 
                                    
                                    name=input$risk_budget_name,
                                    
                                    arguments=list(p=input$risk_budget_p),
                                    
                                    min_prisk=input$prisk[1],
                                    
                                    max_prisk=input$prisk[2],
                                    
                                    min_concentration=input$min_concentration)
        
      }
      
      init.portf
      
    })
    
  })
  
  
  
  opt <- reactive({
    
    input$goButton
    
    isolate({
      
      optimize.portfolio(R=data(), portfolio=portf(), 
                         
                         optimize_method=input$optimize_method, 
                         
                         search_size=input$search_size, trace=TRUE)
      
    })
    
  })
  
  
  
  output$portfolio <- renderPrint({
    
    print(portf())
    
  })
  
  
  
  output$optimization <- renderPrint({
    
    print(opt())
    
  })
  
  
  
  output$chart.RiskReward <- renderPlot({
    
    input$goButton
    
    isolate({
      
      if(input$risk_enabled){
        
        risk_col <- input$risk_name
        
      } else if(input$risk_budget_enabled){
        
        risk_col <- input$risk_budget_name
        
      } else {
        
        risk_col <- "StdDev"
        
      }
      
      chart.RiskReward(opt(), return.col=input$return_name, risk.col=risk_col, 
                       
                       chart.assets=TRUE, main="Optimization")
      
    })
    
  })
  
  
  
  output$chart.Weights <- renderPlot({
    
    chart.Weights(opt(), main="Optimal Weights")
    
  })
  
  
  
  output$chart.RiskBudget <- renderPlot({
    
    input$goButton
    
    isolate({
      
      if(input$risk_budget_enabled){
        
        chart.RiskBudget(opt(), neighbors=10, risk.type="percentage", 
                         
                         main="Risk Budget", match.col=input$risk_budget_name)
        
      }
      
    })
    
  })
  
  
  
  # Show all available data
  
  output$rawData <- renderPrint({
    
    head(data())  
 
  })
 
  }




