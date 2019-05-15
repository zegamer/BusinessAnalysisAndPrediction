dashUI = function(){
  div(
    uiOutput("helloUser"),
    hr(),
    br(),
    br(),
    fluidRow(
      h3("Inventory Summary"),
      hr(),
      br(),
      plotlyOutput("pie")
    ),
    br(),
    hr(),
    fluidRow(
      h3("Product Summary"),
      hr(),
      br(),
      column(width = 4,
             uiOutput("prodOOSText"),
             plotlyOutput("prodOOS")
      ),
      column(width = 5,
             uiOutput("prodLSText"),
             plotlyOutput("prodLS")
      )
    )
  )
}

dashServer = function(session, output){
  
  source("modules/utils/dbCon.R")
  data = dbGetQuery(con,'select "ITEM_NAME", "QUANTITY", "AMOUNT" from inventory')
  dbDisconnect(con)
  
  output$pie = renderPlotly({
    plot_ly() %>%
      add_pie(data, labels = data$ITEM_NAME, values = data$AMOUNT, name = "Amount", domain = list(row = 0, column = 0)) %>%
      add_pie(data, labels = data$ITEM_NAME, values = data$QUANTITY, name = "Quantity", domain = list(row = 0, column = 1))%>%
      layout(title = "Amount and Quanity per product",
             grid = list(rows = 1, columns = 2),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$prodOOSText = renderUI({
    h3(paste0('Out of Stocks : ', as.numeric(unname(data %>% filter(QUANTITY <= 0) %>% count()))))
  })
  
  output$prodLSText = renderUI({
    h3(paste0('Low Stocks : ', as.numeric(data %>% filter(QUANTITY %in% (1:7)) %>% count())))
  })
  
  output$prodOOS = renderPlotly({
    a = data %>% filter(QUANTITY <= 0) %>% select(ITEM_NAME)
    plot_ly(
      type = 'table',
      columnorder = c(1,2),
      columnwidth = c(80,170),
      header = list(
        values = c("<b>Sr No</b>", "<b>Products</b>"),
        line = list(color = "black", width = 1),
        fill = list(color = c('black')),
        font = list(size = 14, color = c('white'))
      ),
      cells = list(
        values = rbind(rownames(a),t(as.matrix(unname(a)))),
        line = list(color = "black", width = 1),
        fill = list(color = c('white')),
        font = list(size = 12)
      ))
  })

  output$prodLS = renderPlotly({
    b = data %>% filter(QUANTITY %in% (1:7)) %>% select(ITEM_NAME, QUANTITY)
    plot_ly(
      type = 'table',
      columnorder = c(1,2,3),
      columnwidth = c(80,170,80),
      header = list(
        values = c("<b>Sr No</b>", "<b>Products</b>", "<b>Quantity</b>"),
        line = list(color = "black", width = 1),
        fill = list(color = c('black')),
        font = list(size = 14, color = c('white'))
      ),
      cells = list(
        values = rbind(rownames(b),t(as.matrix(unname(b)))),
        line = list(color = "black", width = 1),
        fill = list(color = c('white')),
        font = list(size = 12)
      ))
  })
}