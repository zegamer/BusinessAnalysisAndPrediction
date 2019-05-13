library(shiny)
library(dplyr)
library(plotly)
library(shinyjs)
library(openssl)
library(lubridate)
library(shinyalert)
library(shinythemes)
library(RPostgreSQL)
source("modules/utils/DTedit.R")
source("modules/supplier/supplier.R")
source("modules/dashboard/dashboard.R")
source("modules/purchaseSales/purSale.R")
source("modules/prediction/prediction.R")

ui<- tagList(
  useShinyjs(),
  useShinyalert(),
  uiOutput("uiLogin"),
  uiOutput("showBap")
)

server <- function(input, output, session) {
  
  USER = reactiveValues(Logged = F, session = session$user)
  
  navbar_tabs = reactive({
    input$navbar
  })
  
  nav_pursale_tabs = reactive({
    input$nav_pursale
  })
  
  nav_supplier_tabs = reactive({
    input$nav_supplier
  })
  
  nav_prediction_tabs = reactive({
    input$nav_prediction
  })
  
  pursale = function(){
    switch(nav_pursale_tabs(),
         "Show transactions" = loadPurSaleTable(session, input, output),
         "Catalogue" = loadCatalogTable(session, input, output)
    )
  }
  
  supplier = function(){
    switch(nav_supplier_tabs(),
         "Show all suppliers" = loadSupplierTable(session, input, output)
    )
  }
  
  prediction = function(){
    switch(nav_prediction_tabs(),
         "Calculate EOQ" = NULL,
         "Demand Planning" = loadDemandAll(session, output),
         "Purchase and Sales Forecast" = loadForeAll(session, output),
         "Product Analysis" = loadProdAll(session, input, output)
    )
  }
  
  observe({
    if(USER$Logged){
      nt = if(is.null(navbar_tabs())) "None" else navbar_tabs()
      switch(nt,
           "Dashboard" = dashServer(session, output),
           "Purchase/ Sales" = pursale(),
           "Suppliers" = supplier(),
           "Prediction" = prediction(),
           "Preferences" = NULL,
           "None" = NULL
      )
    }
  })

  # Purchase Buttons
  observeEvent(input$pur_product_next, updateTabsetPanel(session, "addNewPurchase", "Purchase Details"))
  observeEvent(input$pur_purchase_next, updateTabsetPanel(session, "addNewPurchase", "Summary"))
  observeEvent(input$pur_verify, pur_verify_button(session, input, output))
  observeEvent(input$pur_submit, pur_submit_button(session, input, output))
  observeEvent(input$pur_reset_all, pur_reset_all_button(session, input, output))
  observeEvent(input$pur_reset_product, pur_reset_product_button(session, input, output))
  observeEvent(input$pur_reset_transaction, pur_reset_transaction_button(session, input, output))

  # Sales Buttons
  observeEvent(input$sal_product_next, updateTabsetPanel(session, "addNewSale", "Sale Details"))
  observeEvent(input$sal_sale_next, updateTabsetPanel(session, "addNewSale", "Summary"))
  observeEvent(input$sal_verify, sal_verify_button(session, input, output))
  observeEvent(input$sal_submit, sal_submit_button(session, input, output))
  observeEvent(input$sal_reset_all, sal_reset_all_button(session, input, output))
  observeEvent(input$sal_reset_product, sal_reset_product_button(session, input, output))
  observeEvent(input$sal_reset_transaction, sal_reset_transaction_button(session, input, output))

  # Supplier Buttons
  observeEvent(input$supplier_reset, supplier_reset_button(session, input, output))
  observeEvent(input$supplier_verify, supplier_verify_button(session, input, output))
  observeEvent(input$supplier_submit, supplier_submit_button(session, input, output))

  # Prediction Buttons
  observeEvent(input$eoq_calc, calculateEOQ(session, input, output))
  observeEvent(input$prodAnl_show, loadProdAll(session, input, output))
  
  # User buttons
  output$uiLogin <- renderUI({
    if (USER$Logged == FALSE) {
      fluidPage(
        h3("Login to BA & P"),
        br(),
        fluidRow(
          column(
            width = 6,
            wellPanel(
              textInput("userName", "Username"),
              passwordInput("pass", "Password"),
              br(),
              actionButton("Login", "Log in")
            )
          )  
        )
        
      )
    }
  })
  output$showBap <- renderUI({
    if (USER$Logged) {
      navbarPage(
        id = "navbar",
        "BA & P",
        theme = shinytheme("flatly"),
        tabPanel("Dashboard", dashUI()),
        tabPanel("Purchase/ Sales",
                 navlistPanel(
                   id = "nav_pursale",
                   widths = c(3,9),
                   tabPanel("New sales entry", addNewSale()),
                   tabPanel("New purchase entry", addNewPurchase()),
                   tabPanel("Show transactions", showTransactions()),
                   tabPanel("Catalogue", showInventory())
                 )
        ),
        tabPanel("Suppliers",
                 navlistPanel(
                   id = "nav_supplier",
                   widths = c(3,9),
                   tabPanel("Add new supplier", add_supplier()),
                   tabPanel("Show all suppliers", show_supplier())
                 )
        ),
        tabPanel("Prediction",
                 navlistPanel(
                   id = "nav_prediction",
                   widths = c(3,9),
                   tabPanel("Calculate EOQ", eoqUi()),
                   tabPanel("Demand Planning", demPlans()),
                   tabPanel("Purchase and Sales Forecast",purSaleFore()),
                   tabPanel("Product Analysis", prodAnlyss())
                 )
        ),
        tabPanel(USER$name,
                h3("Settings"),
                hr(),
                br(),
                actionButton("logout", "Logout"),
                br(),
                br(),
                passwordInput("oldPass", "Old password"),
                passwordInput("passChange", "New password"),
                actionButton("passChangeBtn", "Change"),
                themeSelector()
       )
      )
    }
  })
  
  observeEvent(input$Login , {
    user = isolate(input$userName)
    
    source("modules/utils/dbCon.R")
    role = any(dbGetQuery(con, paste0(' select "role" from users where "username" = \'', user, '\' and "userpass" = \'',sha256(md5(isolate(input$pass))),"'")) == "admin")
    dbDisconnect(con)
    
    if(role){
      USER$Logged = TRUE
      USER$name = user
    } 
    else {
      shinyalert("Unsuccessful", "Username or password is invalid", "error")
    }
  })
  observeEvent(input$logout , {
    USER$Logged = FALSE
  })
  observeEvent(input$passChangeBtn, {
    newPass = sha256(md5(isolate(input$passChange)))
    oldPasswd = sha256(md5(isolate(input$oldPass)))

    source("modules/utils/dbCon.R")
    dbWithTransaction(con,{
      if(dbExecute(con, paste0('update users set "userpass" = \'', newPass, '\' where "username" = \'',USER$name,'\' and "userpass" = \'',oldPasswd,"'")) == 1 )
         shinyalert("Success!", "Password updated", "success")
      else{
        print(paste0('update users set "userpass" = \'', newPass, '\' where "username" = \'',USER$name,'\' and "userpass" = \'',oldPasswd,"'"))
        shinyalert("Failed", "Password failed to update. Try again", "error")
        dbBreak()
      }
    })

  })
  
  output$helloUser = renderUI({
    h1(paste0('Hello ', USER$name))
  })
  
}

shinyApp(ui, server, onStart = function(){
           print("Started")
           onStop(function(){
              try({
                dbDisconnect(con)
                rm(list = ls())
                print("Stopped")
              })
             })
         })
