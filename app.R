library(shiny)
library(shinythemes)

source("modules/utils/DTedit.R")
source("modules/purchaseSales/purSale.R")
source("modules/supplier/supplier.R")

###
# Dashboard using default Shiny UI 
###

ui<- tagList(
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
  
  data_inventory = dbReadTable(con, "inventory")
  data_supplier = dbReadTable(con, "supplier")
  data_purchase = dbReadTable(con, "purchase")
  data_sales = dbReadTable(con, "sales")
  
  dbDisconnect(con)
  
  dtedit(input, output, "show_inventory", data_inventory,
         show.insert = F,
         show.update = F,
         show.copy = F,
         callback.delete = deleteInDB)
  
  dtedit(input, output, "show_supplier", data_supplier,
         show.insert = F,
         show.copy = F,
         callback.update = NULL,
         callback.delete = deleteInDB)
  
  dtedit(input, output, "show_sales", data_sales,
         show.insert = F,
         show.update = F,
         show.copy = F,
         callback.delete = deleteInDB)
  
  dtedit(input, output, "show_purchase", data_purchase,
         show.insert = F,
         show.update = F,
         show.copy = F,
         callback.delete = deleteInDB)
  
  deleteInDB = function(data, row, table = "inventory"){
    
    # print(table)
    # 
    # switch (table,
    #   "inv" = {
    #     dat = data_inventory
    #     sql = paste0('delete from inventory where "ITEM_NAME" = \'',data_inventory[row,]["ITEM_NAME"],"'")
    #     },
    #   "sal" = {
    #     dat = data_sales
    #     sql = paste0('delete from inventory where "InvoiceNo" = \'',data_sales[row,]["InvoiceNo"],"'")
    #     },
    #   "sup" = {
    #     dat = data_supplier
    #     sql = paste0('delete from inventory where "Name" = \'',data_supplier[row,]["Name"],"'")
    #     },
    #   "pur" = {
    #     dat = data_purchase
    #     sql = paste0('delete from inventory where "InvoiceNo" = \'',data_purchase[row,]["InvoiceNo"],"'")
    #     }
    # )
    # print(sql)
    
    return(data[-row,])
  }
  
  # deleteInSupplier = function(data, row){
  #   # dbExecute(con, paste0('delete from supplier where "Name" = \'',data_inventory[row,]["Name"],"'"))
  #   return(data_inventory[-row,])
  # }
  
  # Purchase Buttons
  observeEvent(input$pur_submit, pur_submit_button(session, input, output))
  # observeEvent(input$pur_submit, pur_submitFinal_button(session, input, output))
  observeEvent(input$pur_reset_all, pur_reset_all_button(session, input, output))
  observeEvent(input$pur_reset_product, pur_reset_product_button(session, input, output))
  observeEvent(input$pur_reset_transaction, pur_reset_transaction_button(session, input, output))
  
  # Sales Buttons
  observeEvent(input$sal_submit, sal_submit_button(session, input, output))
  # observeEvent(input$sal_submit, sal_submitFinal_button(session, input, output))
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

