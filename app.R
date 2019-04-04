library(shiny)
library(shinythemes)

source("transaction.R")
source("supplier.R")

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
                 tabPanel("New sales entry"),
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
   
  # output$show_inventory = renderDataTable(
  #   {
  #     source("dbCon.R")
  #     dbReadTable(con, 'inventory')
  #     dbDisconnect(con)
  #   },
  #   options = list(lengthMenu = c(25, 15, 10, 5))
  # )
  
  # output$show_supplier = renderDataTable(
  #   {
  #     source("dbCon.R")
  #     dbReadTable(con, 'supplier')
  #     dbDisconnect(con)
  #   },
  #   options = list(lengthMenu = c(25, 15, 10, 5))
  # )
  
  # Submit Button
  observeEvent(input$pur_submit, pur_submit_form_button(session, input, output))
  
  # Reset All
  observeEvent(input$pur_reset_all, pur_reset_all_button(session, input, output))
  
  # Reset Product Tab
  observeEvent(input$pur_reset_product, pur_reset_product_button(session, input, output))
  
  # Reset Transaction Tab
  observeEvent(input$pur_reset_transaction, pur_reset_transaction_button(session, input, output))
}

shinyApp(ui, server, onStart = function(){
           print("Started")
           onStop(function(){
             print("Stopped")
              try({
                # dbDisconnect(con)
              })
             })
         })

