library(shiny)
library(shinythemes)
library(shinyjs)

source("modules/utils/DTedit.R")
source("modules/purchaseSales/purSale.R")
source("modules/supplier/supplier.R")

###
# Dashboard using default Shiny UI 
###

ui<- tagList(
    useShinyjs(),
    navbarPage(
    theme = shinytheme("flatly"),
    "BA & P",
    tabPanel("Dashboard"
             ),
    tabPanel("Purchase/ Sales",
             navlistPanel(
                 widths = c(3,9),
                 tabPanel("New sales entry", addNewSale()),
                 tabPanel("New purchase entry", addNewPurchase()),
                 tabPanel("Show transactions", showTransactions()),
                 tabPanel("Catalogue", showAll())
             )
    ),
    tabPanel("Suppliers",
             navlistPanel(
               widths = c(3,9),
               tabPanel("Add new supplier", add_supplier()),
               tabPanel("Show all suppliers", show_supplier())
             )
    ),
    tabPanel("Prediction",
             navlistPanel(
               widths = c(3,9),
               tabPanel("Requirements",
                        h1("Requirements")),
               tabPanel("Sales prediction",
                        h1("Sales prediction")),
               tabPanel("Monthly prediction",
                        h1("Monthly prediction"))
             )
    ),
    tabPanel("Preferences",
             fluidRow(
               column(width = 4,
                      h1("Preferences"),
                      hr(),
                      themeSelector()
                )
             )
    )
  )
)

server <- function(input, output, session) {
  
  source("modules/utils/dbCon.R")
  
  # data_inventory = dbReadTable(con, "inventory")
  data_supplier = dbReadTable(con, "supplier")
  data_purchase = dbReadTable(con, "purchase")
  data_sales = dbReadTable(con, "sales")
  
  dbDisconnect(con)
  
  # dtedit(input, output, "show_inventory", data_inventory,
  #        show.insert = F,
  #        show.update = F,
  #        show.copy = F,
  #        callback.delete = deleteInInventory)
  
  dtedit(input, output, "show_supplier", data_supplier,
         show.insert = F,
         show.copy = F,
         callback.update = NULL,
         callback.delete = deleteInSales)

  dtedit(input, output, "show_sales", data_sales,
         show.insert = F,
         show.update = F,
         show.copy = F,
         callback.delete = deleteInSales)

  dtedit(input, output, "show_purchase", data_purchase,
         show.insert = F,
         show.update = F,
         show.copy = F,
         callback.delete = deleteInSales)

  deleteInSales = function(data, row){
    # data_sales = data[-row,]
    return(data[-row,])
  }
  
  # deleteInInventory = function(data, row){
  #   data_inventory = data[-row,]
  #   return(data_inventory)
  # }
  
  # deleteInSupplier = function(data, row){
  #   data_supplier = data[-row,]
  #   return(data_supplier)
  # }
  # 
  # deleteInPurchase = function(data, row){
  #   data_purchase = data[-row,]
  #   return(data_purchase)
  # }
  
  # Purchase Buttons
  observeEvent(input$pur_verify, pur_verify_button(session, input, output))
  observeEvent(input$pur_submit, pur_submit_button(session, input, output))
  observeEvent(input$pur_reset_all, pur_reset_all_button(session, input, output))
  observeEvent(input$pur_reset_product, pur_reset_product_button(session, input, output))
  observeEvent(input$pur_reset_transaction, pur_reset_transaction_button(session, input, output))
  
  # Sales Buttons
  observeEvent(input$sal_verify, sal_verify_button(session, input, output))
  observeEvent(input$sal_submit, sal_submit_button(session, input, output))
  observeEvent(input$sal_reset_all, sal_reset_all_button(session, input, output))
  observeEvent(input$sal_reset_product, sal_reset_product_button(session, input, output))
  observeEvent(input$sal_reset_transaction, sal_reset_transaction_button(session, input, output))
  observeEvent(input$sal_reset_custDets, sal_reset_custDets_button(session, input, output))
  
  # Supplier Buttons
  observeEvent(input$supplier_reset, supplier_reset_button(session, input, output))
  observeEvent(input$supplier_submit, supplier_submit_button(session, input, output))
  
}

shinyApp(ui, server, onStart = function(){
           print("Started")
           onStop(function(){
             print("Stopped")
              try({
                dbDisconnect(con)
              })
             })
         })

