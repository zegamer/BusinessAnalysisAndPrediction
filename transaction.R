source("purSale_purchaseTab.R")
source("purSale_saleTab.R")

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
    uiOutput("show_inventory")
  )
}

showTransactions = function(input, output){
  div(
    h3("Showing transactions"),
    hr(),
    fluidRow(column(width = 6,
                    dataTableOutput("show_purchases")),
             column(width = 6,
                    dataTableOutput("show_sales"))
    )
  )
}