purSaleFore = function(){
  div(
    h3("Sales Forecasting"),
    helpText("Information about Sales Forecasting"),
    hr(),
    br(),
    h3("Summary"),
    hr(),
    fluidRow(
      column(width = 12,
             plotlyOutput("total_amount"))
    ),
    hr(),
    h4("Seasonality and Trends"),
    hr(),
    fluidRow(
      column(width = 6,
             plotlyOutput("total_amount_seasonality")),
      column(width = 6,
             plotlyOutput("total_amount_trends"))
    ),
    hr(),
    h4("Forecast for next 12 months"),
    hr(),
    fluidRow(
      column(width = 12,
             plotlyOutput("total_amount_forecast"))
    )
  )
}

loadForeAll = function(session, output){
  
  source("modules/utils/dbCon.R")
  sal_df = dbGetQuery(con,'Select "Date", "Amount" from sales')
  pur_df = dbGetQuery(con,'Select "Date", "Amount" from purchase')
  dbDisconnect(con)
  rm(con)
  
  sal_df$Date = as.Date(sal_df$Date, format = "%Y-%m-%d")
  pur_df$Date = as.Date(pur_df$Date, format = "%Y-%m-%d")
  
  df2 = pur_df %>% group_by(Date= floor_date(pur_df$Date, "month")) %>% summarize(Amount=sum(pur_df$Amount))
  df2$Amount = tapply(pur_df$Amount, format(pur_df$Date,'%m-%Y'), sum)
  pur_df = df2
  
  df2 = sal_df %>% group_by(Date= floor_date(sal_df$Date, "month")) %>% summarize(Amount=sum(sal_df$Amount))
  df2$Amount = tapply(sal_df$Amount, format(sal_df$Date,'%m-%Y'), sum)
  sal_df = df2
  rm(df2)
  
  salesTS = ts(sal_df$Amount, frequency = 12, start = c(year(min(sal_df$Date)),month(min(sal_df$Date))))
  purchaseTS = ts(pur_df$Amount, frequency = 12, start = c(year(min(pur_df$Date)),month(min(pur_df$Date))))
  salesDecompose = decompose(salesTS)
  purchaseDecompose = decompose(purchaseTS)
  
  salesLogHW = HoltWinters(salesTS)
  purchaseLogHW = HoltWinters(purchaseTS)
  
  salesLogHW = HoltWinters(salesTS)
  purchaseLogHW = HoltWinters(purchaseTS)
  nextYearSales <-  stats:::predict.HoltWinters(salesLogHW, n.ahead = 12)
  nextYearPurchase <-  stats:::predict.HoltWinters(purchaseLogHW, n.ahead = 12)
  
  d = max(as.Date(sal_df$Date))
  month(d) = month(d) + 1
  fore_x = format(seq(from = d, length = 12, by = "month"), '%b %y')
  fore_x = factor(fore_x, levels = fore_x)

  output$total_amount = renderPlotly({
    plot_ly(x = sal_df$Date, y = round(sal_df$Amount,2), name = "Sold Amount", type = "bar") %>%
      add_trace(y = pur_df$Amount, name = "Purchased Amount", type = "bar") %>%
      layout(title = "Sales and Purchase till now",
             xaxis = list(
               title = "Year",
               rangeslider = list(type = "date")
             ),
             yaxis = list(title = "Rupees"),
             barmode = 'group')
  })
  
  output$total_amount_seasonality = renderPlotly({
    plot_ly(x = sal_df$Date, y = round(salesDecompose$seasonal,0), type = "scatter", mode = "lines+markers", name = "Sales") %>%
      add_trace(y = round(purchaseDecompose$seasonal,0), name = 'Purchase', type = "scatter", mode = "lines") %>%
      layout(
        title = paste0("Seasonality"),
        xaxis = list(title = "Months"),
        yaxis = list(title = "Seasonality")
      )
  })
  
  output$total_amount_trends = renderPlotly({
    plot_ly(x = sal_df$Date, y = round(salesDecompose$trend,0), type = "scatter", mode = "lines+markers", name = "Sales") %>%
      add_trace(y = round(purchaseDecompose$trend,0), name = 'Purchase', type = "scatter", mode = "lines") %>%
      layout(
        title = paste0("Trends"),
        xaxis = list(title = "Months"),
        yaxis = list(title = "Trend")
      )
  })
  
  output$total_amount_forecast = renderPlotly({
    plot_ly(x = fore_x, y = round(nextYearSales,2), type = "bar", name = "Sales") %>%
      add_trace(y = round(nextYearPurchase,0), name = 'Purchase') %>%
      layout(
        title = paste0("Forecast for the next 12 months"),
        xaxis = list(title = "Months"),
        yaxis = list(title = "Units"),
        barmode = 'group'
      )
  })
}