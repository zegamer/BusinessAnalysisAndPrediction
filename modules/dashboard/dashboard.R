library(shinydashboard)

dashUI = function(){
  
  dashboardPage(
    dashboardHeader(disable = T),
    dashboardSidebar(disable = T),
    dashboardBody(
      h1("Hello, user"),
      div(hr()),
      br(),
      br(),
      fluidRow(
        h3("Product Summary"),
        valueBoxOutput("prodOOSText"),
        valueBoxOutput("prodLSText")
      ),
      br(),
      br(),
      div(hr()),
      fluidRow(
        box(
          div(tableOutput("prodOOS"), class = "codebox"),
          title = "Out of Stocks",
          width = 4,
          collapsible = T,
          solidHeader = T,
          status = "danger"
        ),
        box(
          div(tableOutput("prodLS"), class = "codebox"),
          title = "Low Stocks",
          width = 4,
          collapsible = T,
          solidHeader = T,
          status = "warning"
        )
      ),
      br(),
      br(),
      div(hr()),
      fluidRow(
        h3("Inventory Summary"),
        div(hr()),
        br(),
        box(
          title = "Amount held in Inventory",
          status = "success",
          solidHeader = T,
          width = 6,
          plotlyOutput("pie_amount")
        ),
        box(
          title = "Quantity held in Inventory",
          status = "info",
          solidHeader = T,
          width = 6,
          plotlyOutput("pie_quantity")
        )
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
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$pie_quantity = renderPlotly({
    plot_ly(data, labels = ~ITEM_NAME, values = ~QUANTITY, name = "Quantity", type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$prodOOSText = renderValueBox({
    valueBox(
      subtitle = "Out of Stock",
      value = data %>% filter(QUANTITY <= 0) %>% count(),
      icon = icon("list"),
      color = "red"
    )
  })
  
  output$prodLSText = renderValueBox({
    valueBox(
      subtitle = "Low Stocks",
      value = data %>% filter(QUANTITY %in% (1:7)) %>% count(),
      icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$prodOOS = renderTable({
    data %>% filter(QUANTITY <= 0) %>% select(ITEM_NAME)
  })

  output$prodLS = renderTable({
    data %>% filter(QUANTITY %in% (1:7)) %>% select(ITEM_NAME, QUANTITY)
  })
}