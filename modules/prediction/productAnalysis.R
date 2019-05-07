source("modules/utils/dbCon.R")
sal_df = dbGetQuery(con,'select "ProductName", TO_CHAR("Date" :: DATE, \'yyyy - mm\') as "Date", SUM("Quantity") as "Quantity", SUM("Amount") as "Amount" from sales group by "ProductName", "Date"')
pur_df = dbGetQuery(con,'select "ProductName", TO_CHAR("Date" :: DATE, \'yyyy - mm\') as "Date", SUM("Quantity") as "Quantity", SUM("Amount") as "Amount" from purchase group by "ProductName", "Date"')
inv_df = dbGetQuery(con,'select "ITEM_NAME" from inventory')
dbDisconnect(con)
rm(con)

prodAnlyss = function(){
  div(
    h3("Product Analysis"),
    icon("info-circle", lib = "font-awesome"),
    bsTooltip("tooltip", "Something has to happen"),
    fluidRow(
      column(width = 4,
             selectizeInput("prodAnl_prodName", "Product",
                            choices = loadProducts())
      ),
      column(width = 4,
             selectizeInput("prodAnl_years", "Year",
                            choices = loadYears())
      )
    ),
    fluidRow(
      column(width = 4,
             actionButton("prodAnl_show", "Show")
      )
    ),
    hr(),
    h3("Product Summary"),
    hr(),
    fluidRow(
      column(width = 6,
             plotlyOutput("total_amount_bar")),
      column(width = 6,
             plotlyOutput("total_quantity_bar"))
    ),
    hr(),
    h4("Seasonality and Trends"),
    hr(),
    fluidRow(
      column(width = 6,
             plotlyOutput("prod_seasonality")),
      column(width = 6,
             plotlyOutput("prod_trends"))
    ),
    hr(),
    h4("Forecast for next 12 months"),
    hr(),
    fluidRow(
      column(width = 12,
             plotlyOutput("prod_forecast"))
    )
    
    
  )
}

loadProducts = function(){
  source("modules/utils/dbCon.R")
  inv_df = dbGetQuery(con,'select "ITEM_NAME" from inventory')
  dbDisconnect(con)
  return(inv_df[,])
}

loadYears = function(){
  source("modules/utils/dbCon.R")
  years = dbGetQuery(con,'select distinct TO_CHAR("Date" :: DATE, \'yyyy\') as "Date" from sales')
  dbDisconnect(con)
  return(sort(years[,]))
}

loadProdAll = function(session, input, output){
  
  row_names = inv_df$ITEM_NAME
  row_names = sort(row_names)
  col_names = pur_df %>% distinct(pur_df$Date)
  col_names = sort(col_names[,])
  
  pur_2df_quantity = data.frame(matrix(0, ncol = length(col_names), nrow = length(row_names)))
  colnames(pur_2df_quantity) = col_names
  row.names(pur_2df_quantity) = row_names
  
  sal_2df_quantity = data.frame(matrix(0, ncol = length(col_names), nrow = length(row_names)))
  colnames(sal_2df_quantity) = col_names
  row.names(sal_2df_quantity) = row_names
  
  pur_2df_amount = data.frame(matrix(0, ncol = length(col_names), nrow = length(row_names)))
  colnames(pur_2df_amount) = col_names
  row.names(pur_2df_amount) = row_names
  
  sal_2df_amount = data.frame(matrix(0, ncol = length(col_names), nrow = length(row_names)))
  colnames(sal_2df_amount) = col_names
  row.names(sal_2df_amount) = row_names
  
  i = 1
  l = nrow(pur_df)
  
  while(i <= l){
    x = pur_df[i,]$ProductName
    y = pur_df[i,]$Date
    pur_2df_quantity[x,y] = pur_2df_quantity[x,y] + pur_df[i,]$Quantity
    pur_2df_amount[x,y] = pur_2df_amount[x,y] + pur_df[i,]$Amount
    i = i + 1
  }
  
  i = 1
  l = nrow(sal_df)
  
  while(i <= l){
    x = sal_df[i,]$ProductName
    y = sal_df[i,]$Date
    sal_2df_quantity[x,y] = sal_2df_quantity[x,y] + sal_df[i,]$Quantity
    sal_2df_amount[x,y] = sal_2df_amount[x,y] + sal_df[i,]$Amount
    i = i + 1
  }
  
  inpProdName = input$prodAnl_prodName
  inpYear = input$prodAnl_years
  
  # inpProdName = "Pad-48204000"
  # inpYear = 2017
  
  years = paste0(inpYear," - ",sprintf('%02d', 1:12))
  sal_data_quantity = c(1:12)
  pur_data_quantity = c(1:12)
  sal_data_amount = c(1:12)
  pur_data_amount = c(1:12)
  
  i = 1
  while(i <= 12){
    sal_data_amount[i] = if(is.null(sal_2df_amount[inpProdName, years[i]])) 0 else sal_2df_amount[inpProdName, years[i]]
    pur_data_amount[i] = if(is.null(pur_2df_amount[inpProdName, years[i]])) 0 else pur_2df_amount[inpProdName, years[i]]
    i = i + 1
  }
  
  i = 1
  while(i <= 12){
    sal_data_quantity[i] = if(is.null(sal_2df_quantity[inpProdName, years[i]])) 0 else sal_2df_quantity[inpProdName, years[i]]
    pur_data_quantity[i] = if(is.null(pur_2df_quantity[inpProdName, years[i]])) 0 else pur_2df_quantity[inpProdName, years[i]]
    i = i + 1
  }
  
  salesTS = ts(t(sal_2df_quantity[inpProdName,]), frequency = 12, start = c(2017,5))
  purchaseTS = ts(t(pur_2df_quantity[inpProdName,]), frequency = 12, start = c(2017,5))
  salesDecompose = decompose(salesTS)
  purchaseDecompose = decompose(purchaseTS)
  
  output$total_amount_bar = renderPlotly({
    plot_ly(x = factor(month.name[c(1:12)], levels = month.name[c(1:12)]), y = pur_data_amount, type = "bar", name = 'Purchase') %>%
      add_trace(y = sal_data_amount, name = 'Sales') %>%
      layout( title = paste0("Amount of ",inpProdName," puchased and sold in ",inpYear),
              xaxis = list(title = "Months"),
              yaxis = list(title = "Rupees"),
              barmode = 'group')
  })
  
  output$total_quantity_bar = renderPlotly({
    plot_ly(x = factor(month.name[c(1:12)], levels = month.name[c(1:12)]), y = pur_data_quantity, type = "bar", name = 'Purchase') %>%
      add_trace(y = sal_data_quantity, name = 'Sales') %>%
      layout( title = paste0("Quantity of ",inpProdName," puchased and sold in ",inpYear),
             xaxis = list(title = "Months"),
             yaxis = list(title = "Units"),
             barmode = 'group')
    
  })
  
  output$prod_seasonality = renderPlotly({
    plot_ly(x = col_names, y = round(salesDecompose$seasonal,0), type = "scatter", mode = "lines+markers", name = "Sales") %>%
      add_trace(y = round(purchaseDecompose$seasonal,0), name = 'Purchase', type = "scatter", mode = "lines") %>%
      layout(
        title = paste0("Seasonality of ",inpProdName),
        xaxis = list(title = "Months"),
        yaxis = list(title = "Seasonality")
      )
  })

  output$prod_trends = renderPlotly({
    plot_ly(x = col_names, y = round(salesDecompose$trend,0), type = "scatter", mode = "lines+markers", name = "Sales") %>%
      add_trace(y = round(purchaseDecompose$trend,0), name = 'Purchase', type = "scatter", mode = "lines") %>%
      layout(
        title = paste0("Trends of ",inpProdName),
        xaxis = list(title = "Months"),
        yaxis = list(title = "Trend")
      )
  })
  
  output$prod_forecast = renderPlotly({
    salesLogHW = HoltWinters(salesTS)
    purchaseLogHW = HoltWinters(purchaseTS)
    nextYearSales <-  stats:::predict.HoltWinters(salesLogHW, n.ahead = 12)
    nextYearPurchase <-  stats:::predict.HoltWinters(purchaseLogHW, n.ahead = 12)
    
    a = colnames(pur_2df_quantity)
    d = as.Date(paste0(a[length(a)]," - 01"), '%Y - %m - %d')
    month(d) = month(d) + 1
    fore_x = format(seq(from = d, length = 12, by = "month"), '%b %y')
    fore_x = factor(fore_x, levels = fore_x)
    
    plot_ly(x = fore_x, y = round(nextYearSales,0), type = "bar", name = "Sales") %>%
      add_trace(y = round(nextYearPurchase,0), name = 'Purchase') %>%
      layout(
        title = paste0("Forecast of ",inpProdName, " for the next 12 months"),
        xaxis = list(title = "Months"),
        yaxis = list(title = "Units")
      )
  })
}
