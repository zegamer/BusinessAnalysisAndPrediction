source("modules/purchaseSales/purSale_purchaseTab.R")
source("modules/purchaseSales/purSale_saleTab.R")

addNewPurchase = function(input, output){
  div(
    h3("New entry"),
    hr(),
    form_purchase()
  )
}

addNewSale = function(input, output){
  div(
    h3("New entry"),
    hr(),
    form_sale()
  )
}

showAll = function(input, output){
  div(
    h3("Showing all items"),
    hr(),
    # uiOutput("show_inventory")
    h4("Fetch optimization needed, inventory takes much memory while calling")
  )
}

showTransactions = function(input, output){
  div(
    h3("Showing transactions"),
    hr(),
    tabsetPanel(id = "Transactions",
                tabPanel("Purchase transactions",br(),
                         uiOutput("show_purchase")),
                tabPanel("Sales transactions",br(),
                         uiOutput("show_sales"))
    )
  )
}