library(dplyr)

source("modules/utils/dbcon.r")

sal_df = dbGetQuery(con,'select "ProductName", TO_CHAR("Date" :: DATE, \'yyyy - mm\') as "Date", SUM("Quantity") as "Quantity" from sales group by "ProductName", "Date"')
pur_df = dbGetQuery(con,'select "ProductName", TO_CHAR("Date" :: DATE, \'yyyy - mm\') as "Date", SUM("Quantity") as "Quantity" from purchase group by "ProductName", "Date"')
inv_df = dbGetQuery(con,'select "ITEM_NAME" from inventory')
dbDisconnect(con)
rm(con)

row_names = inv_df$ITEM_NAME
row_names = sort(row_names)
col_names = pur_df %>% distinct(pur_df$Date)
col_names = sort(col_names[,])

pur_2df = data.frame(matrix(0, ncol = length(col_names), nrow = length(row_names)))
colnames(pur_2df) = col_names
row.names(pur_2df) = row_names

sal_2df = data.frame(matrix(0, ncol = length(col_names), nrow = length(row_names)))
colnames(sal_2df) = col_names
row.names(sal_2df) = row_names

rm(col_names, row_names, inv_df)

i = 1
l = nrow(pur_df)

while(i <= l){
  x = pur_df[i,]$ProductName
  y = pur_df[i,]$Date
  pur_2df[x,y] = pur_2df[x,y] + pur_df[i,]$Quantity
  i = i + 1
}

i = 1
l = nrow(sal_df)

while(i <= l){
  x = sal_df[i,]$ProductName
  y = sal_df[i,]$Date
  sal_2df[x,y] = sal_2df[x,y] + sal_df[i,]$Quantity
  i = i + 1
}

rm(pur_df, sal_df, i, l, x, y)

write.csv(pur_2df, "C:/Users/babu2/Downloads/purdf.csv")
write.csv(sal_2df, "C:/Users/babu2/Downloads/saldf.csv")

rm(pur_2df, sal_2df)