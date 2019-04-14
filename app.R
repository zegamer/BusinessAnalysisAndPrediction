library(shiny)
library(shinythemes)

source("DTedit.R")
source("transaction.R")
source("supplier.R")
source("dbCon.R")

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
  
  data_inventory = dbReadTable(con,"inventory")
  data_supplier = dbReadTable(con,"supplier")
  
  dtedit(input, output, "show_inventory", data_inventory,
         show.insert = F,
         show.update = F,
         show.copy = F,
         callback.delete = deleteInDB
         )
  dtedit(input, output, "show_supplier", data_supplier,
         show.insert = F,
         show.copy = F,
         callback.update = NULL,
         callback.delete = deleteInDB)
  
  deleteInDB = function(data, row){
    print(data_inventory[-row,])
    # dbExecute(con,paste0("UPDATE"))
    return(data_inventory[-row,])
  }
  
  # Purchase Buttons
  observeEvent(input$pur_submit, pur_submit_form_button(session, input, output))
  observeEvent(input$pur_reset_all, pur_reset_all_button(session, input, output))
  observeEvent(input$pur_reset_product, pur_reset_product_button(session, input, output))
  observeEvent(input$pur_reset_transaction, pur_reset_transaction_button(session, input, output))
  
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

