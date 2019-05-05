library(dplyr)

a = c(paste0("17_",c(7:12)), paste0("18_", c(1:12)), paste0("19_", c(1:2)))
pur_list = paste0("pur_", a)
sal_list = paste0("sal_", a)
rm(a)

filepath = "C:/Users/babu2/Downloads/BAaP/"

df = read.csv(paste0(filepath, "Sales", '/', sal_list[1], '.csv')) %>% na.omit()
df = df[order(as.Date(df$Date, format="%Y-%m-%d")),]

i = 2
while(i <= 20){
  df2 = read.csv(paste0(filepath, "Sales", '/', sal_list[i], '.csv')) %>% na.omit()
  df2 = df2[order(as.Date(df2$Date, format="%Y-%m-%d")),]
  df = rbind(df,df2)
  print(sal_list[i])
  i = i + 1
}

names(df) = c("InvoiceNo", "Date", "CustName", "ProductName", "HSN", "GST", "Quantity", "Rate", "PurchaseRate", "Amount")
write.csv(df, paste0(filepath, "Sales", '/consolidated1', '.csv'), row.names = F)