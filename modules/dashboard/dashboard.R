dashUI = function(){
  div(
    uiOutput("helloUser"),
    hr(),
    br(),
    br(),
    fluidRow(
      h3("Product Summary"),
      hr(),
      br(),
      column(width = 4,
             tags$label(`for` = "prodOOSText", "Out of stock : "),
             textOutput("prodOOSText", inline = T),br(),
             tags$label(`for` = "prodOOSText", "Low stock : "),
             textOutput("prodLSText", inline = T), br()
      ),
      column(width = 4,
        h3("Out of stocks"),
        plotlyOutput("prodOOS")
      ),
      column(width = 4,
        h3("Low stocks"),
        plotlyOutput("prodLS")
      )
    ),
    br(),
    br(),
    hr(),
    fluidRow(
      h3("Inventory Summary"),
      hr(),
      br(),
      fluidRow(
        plotlyOutput("pie_amount", height = "150%"),
        hr(),
        br(),
        plotlyOutput("pie_quantity", height = "150%")
      )
    )
  )
}

dashServer = function(session, output){
  
  source("modules/utils/dbCon.R")
  data = dbGetQuery(con,'select "ITEM_NAME", "QUANTITY", "AMOUNT" from inventory')
  dbDisconnect(con)
  
  output$pie_amount = renderPlotly({
    plot_ly(data, labels = ~ITEM_NAME, values = ~AMOUNT, name = "Amount", type = 'pie') %>%
      layout(title = "Amount per product",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$pie_quantity = renderPlotly({
    plot_ly(data, labels = ~ITEM_NAME, values = ~QUANTITY, name = "Quantity", type = 'pie') %>%
      layout(title = "Quantity per product",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$prodOOSText = renderText({
    as.numeric(unname(data %>% filter(QUANTITY <= 0) %>% count()))
  })
  
  output$prodLSText = renderText({
    as.numeric(data %>% filter(QUANTITY %in% (1:7)) %>% count())
  })
  
  output$prodOOS = renderPlotly({
    a = data %>% filter(QUANTITY <= 0) %>% select(ITEM_NAME)
    plot_ly(
      type = 'table',
      header = list(
        values = c("<b>Products</b>"),
        line = list(color = "black", width = 1),
        fill = list(color = c('black')),
        font = list(size = 14, color = c('white'))
      ),
      cells = list(
        values = rbind(t(as.matrix(unname(a)))),
        line = list(color = "black", width = 1),
        fill = list(color = c('white')),
        font = list(size = 12)
      ))
  })

  output$prodLS = renderPlotly({
    b = data %>% filter(QUANTITY %in% (1:7)) %>% select(ITEM_NAME, QUANTITY)
    plot_ly(
      type = 'table',
      header = list(
        values = c("<b>Products</b>", "<b>Quantity</b>"),
        line = list(color = "black", width = 1),
        fill = list(color = c('black')),
        font = list(size = 14, color = c('white'))
      ),
      cells = list(
        values = rbind(t(as.matrix(unname(b)))),
        line = list(color = "black", width = 1),
        fill = list(color = c('white')),
        font = list(size = 12)
      ))
  })
}