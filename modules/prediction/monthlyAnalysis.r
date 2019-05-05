library(plotly)
library(lubridate)
library(dplyr)

source("modules/utils/dbCon.r")

# inv_df = data.frame(data_inventory["ITEM_NAME"])
# sal_df = data.frame(data_sales["Date"],data_sales["ProductName"],data_sales["Quantity"],data_sales["Amount"])
# pur_df = data.frame(data_purchase["Date"],data_purchase["ProductName"],data_purchase["Quantity"],data_purchase["Amount"])

inv_df = dbGetQuery(con,'Select "ITEM_NAME" from inventory order by "ITEM_NAME"' )
sal_df = dbGetQuery(con,'Select "Date", "ProductName", "Quantity", "Amount" from sales' )
pur_df = dbGetQuery(con,'Select "Date", "ProductName", "Quantity", "Amount" from purchase' )

sal_df$Date = as.Date(sal_df$Date, format = "%Y-%m-%d")
pur_df$Date = as.Date(pur_df$Date, format = "%Y-%m-%d")

df2 = pur_df %>% group_by(Date= pur_df$Date) %>% summarize(Quantity=sum(pur_df$Quantity), Amount=sum(pur_df$Amount))
df2$Quantity = tapply(pur_df$Quantity, format(pur_df$Date,'%d-%m-%Y'), sum)
df2$Amount = tapply(pur_df$Amount, format(pur_df$Date,'%d-%m-%Y'), sum)
pur_df = df2

df2 = sal_df %>% group_by(Date= sal_df$Date) %>% summarize(Quantity=sum(sal_df$Quantity), Amount=sum(sal_df$Amount))
df2$Quantity = tapply(sal_df$Quantity, format(sal_df$Date,'%d-%m-%Y'), sum)
df2$Amount = tapply(sal_df$Amount, format(sal_df$Date,'%d-%m-%Y'), sum)
sal_df = df2
rm(df2)

spQuantity = plot_ly(x = sal_df$Date, y = sal_df$Quantity, name = "Sold", type = "scatter", mode = "lines") %>%
  add_lines(y = pur_df$Quantity, name = "Purchased", type = "scatter", mode = "lines") %>%
  layout(title = "Quantities", yaxis = list(title = "Units"),
         xaxis = list(
           title = "Year",
           rangeslider = list(type = "date")
         )
  )

spAmount = plot_ly(x = sal_df$Date, y = sal_df$Amount, name = "Sold Amount", type = "scatter", mode = "lines") %>%
  add_lines(y = pur_df$Amount, name = "Purchased Amount", type = "scatter", mode = "lines") %>%
  layout(title = "Sales and Purchase till now",
         xaxis = list(
           title = "Year",
           rangeslider = list(type = "date")
         ),
         yaxis = list(title = "Rupees"))

spQuantity
spAmount

salesTS = ts(pur_df$Amount, frequency = 12, start = c(2017,2))
salesDecompose = decompose(salesTS)
plot(salesDecompose)
salesLog = log(salesTS)
plot(salesLog)
salesLogHW = HoltWinters(salesLog)
plot(salesLogHW)

nextYearSales <-  stats:::predict.HoltWinters(salesLogHW, n.ahead = 24)
plot(nextYearSales)