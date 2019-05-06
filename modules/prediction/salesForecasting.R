purSaleFore = function(){
  # source("modules/utils/dbCon.R")
  # sal_df = dbGetQuery(con,'Select "Date", "Quantity", "Amount" from sales')
  # pur_df = dbGetQuery(con,'Select "Date", "Quantity", "Amount" from purchase')
  # dbDisconnect(con)
  # rm(con)
  # 
  # sal_df$Date = as.Date(sal_df$Date, format = "%Y-%m-%d")
  # pur_df$Date = as.Date(pur_df$Date, format = "%Y-%m-%d")
  # 
  # df2 = pur_df %>% group_by(Date= floor_date(pur_df$Date, "month")) %>% summarize(Amount=sum(pur_df$Amount, Quantity = sum(pur_df$Quantity)))
  # df2$Amount = tapply(pur_df$Amount, format(pur_df$Date,'%m-%Y'), sum)
  # df2$Quantity = tapply(pur_df$Quantity, format(pur_df$Date,'%m-%Y'), sum)
  # pur_df = df2
  # 
  # df2 = sal_df %>% group_by(Date= floor_date(sal_df$Date, "month")) %>% summarize(Amount=sum(sal_df$Amount, Quantity = sum(sal_df$Quantity)))
  # df2$Amount = tapply(sal_df$Amount, format(sal_df$Date,'%m-%Y'), sum)
  # df2$Quantity = tapply(sal_df$Quantity, format(sal_df$Date,'%m-%Y'), sum)
  # sal_df = df2
  # rm(df2)
}

# spQuantity = plot_ly(x = sal_df$Date, y = sal_df$Quantity, name = "Sold", type = "scatter", mode = "lines") %>%
#   add_lines(y = pur_df$Quantity, name = "Purchased", type = "scatter", mode = "lines") %>%
#   layout(title = "Quantities", yaxis = list(title = "Units"),
#          xaxis = list(
#            title = "Year",
#            rangeslider = list(type = "date")
#          )
#   )
# 
# spAmount = plot_ly(x = sal_df$Date, y = sal_df$Amount, name = "Sold Amount", type = "scatter", mode = "lines") %>%
#   add_lines(y = pur_df$Amount, name = "Purchased Amount", type = "scatter", mode = "lines") %>%
#   layout(title = "Sales and Purchase till now",
#          xaxis = list(
#            title = "Year",
#            rangeslider = list(type = "date")
#          ),
#          yaxis = list(title = "Rupees"))

# spQuantity
# spAmount

# Start date will be entered by user
# Plots are optional
# salesTS = ts(sal_df$Amount, frequency = 12, start = c(2017,5))
# salesDecompose = decompose(salesTS)
# plot(salesDecompose$seasonal)
# plot(salesDecompose$trend)
# 
# salesLog = log(salesTS)
# plot(salesLog)
# 
# salesLogHW = HoltWinters(salesTS)
# plot(salesLogHW)
# 
# nextSales <-  stats:::predict.HoltWinters(salesLogHW, n.ahead = 12)
# plot(nextSales, main = "Sales Forecast", xlab = "Years", ylab = "Amount")