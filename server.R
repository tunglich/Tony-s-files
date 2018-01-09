

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
    else {input$equitySelected}
    })
   


  output$perChart <- renderHighchart({
    funds_selected_date_return <- Nomura_Return_All[subset_date(), equity_selected()] 
    funds_selected_date_price <-  (cumprod((1+funds_selected_date_return)) - 1) * 100 
     
    hc <-highchart(type = "stock")
    for (i in 1:length(equity_selected())){
     hc<- hc %>% hc_add_series(funds_selected_date_price[,i], name = colnames(funds_selected_date_price[,i]))
    }
    
    hc %>% hc_yAxis(labels = list(format = "{value}%"))
                    
      
  })
  
  
  
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
      hc_xAxis(title = list(text = 'Selected Funds')) %>% 
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(labels = "") %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = '<table>', pointFormat = '<tr><th colspan="2"><h5>{point.fund_names}</h5></th></tr>
                 <tr><th>Cumulative Return:</th><td>{point.y:.2f}%</td></tr>', footerFormat = '</table>')
   
                    
                            
  })
}

