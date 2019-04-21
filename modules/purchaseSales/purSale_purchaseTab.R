loadSuppliers = function(){
  source("modules/utils/dbCon.R")
  suppliersList = dbGetQuery(con,"select \"Name\" from supplier")
  dbDisconnect(con)
  return(suppliersList[,])
}

form_purchase = function(input, output){
  div(
    tabsetPanel(id = "addNewPurchase",
                tabPanel("Product", 
                         div(
                           br(),
                           fluidRow(
                             column(width = 6,
                                    dateInput("pur_date", label = "Date", format = "dd-mm-yyyy"),
                                    tags$style(HTML(".datepicker {z-index:99999 !important;}"))
                             ),
                             column(width = 6,
                                    textInput("pur_invNo", label = "Invoice Number")
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(width = 6,
                                    textInput("pur_prodID", label = "Product ID", placeholder = "Enter product ID"),
                                    numericInput("pur_hsn", label = "HSN", value = 0)
                             ),
                             column(width = 6,
                                    textInput("pur_prodName", label = "Product name *", placeholder = "Enter product name")
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(width = 2,
                                    actionButton("pur_product_next", "Next", class = "btn btn-primary"),
                                    br()),
                             column(width = 2,
                                    actionButton("pur_reset_product", "Reset"))
                           )
                         )
                ),
                tabPanel("Purchase Details", 
                         div(
                           br(),
                           fluidRow(
                             column(width = 6,
                                    numericInput("pur_rate", label = "Cost", value = 0),
                                    selectizeInput("pur_gst", label = "GST",
                                                   c("18%" = 18,
                                                     "28%" = 28)),
                                    checkboxInput("pur_inclGst", label = "Inclusive of GST?")
                             ),
                             column(width = 6,
                                    textAreaInput("pur_desc", label = "Purchase Description", placeholder = "Optional description", rows = "5", cols = "50")
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(width = 6,
                                    numericInput("pur_qty", "Quantity", value = 1)
                             ),
                             column(width = 6,
                                    selectInput("pur_supplier", "Supplier", choices = loadSuppliers())
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(width = 2,
                                    actionButton("pur_purchase_next", "Next", class = "btn btn-primary"),
                                    br()),
                             column(width = 2,
                                    actionButton("pur_reset_transaction", "Reset"))
                           )
                         )
                ),
                tabPanel("Summary", 
                         div(
                           br(),
                           fluidRow(
                             column(width = 6,
                                    tags$label("Date : ", `for`="pur_dateSum"),
                                    textOutput("pur_dateSum", inline = TRUE),br(),
                                    tags$label("Invoice No. : ", `for`="pur_invNoSum"),
                                    textOutput("pur_invNoSum", inline = TRUE),br()
                             ),
                             column(width = 6,
                                    tags$label("Product ID : ", `for`="pur_prodIDSum"),
                                    textOutput("pur_prodIDSum", inline = TRUE),br(),
                                    tags$label("Product Name : ", `for`="pur_prodNameSum"),
                                    textOutput("pur_prodNameSum", inline = TRUE),br(),
                                    tags$label("HSN Code : ", `for`="pur_hsnSum"),
                                    textOutput("pur_hsnSum", inline = TRUE),br()
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(width = 6,
                                    tags$label("Cost price : ", `for`="pur_rateSum"),
                                    textOutput("pur_rateSum", inline = TRUE),br(),
                                    tags$label("Price with GST : ", `for`="pur_priceSum"),
                                    textOutput("pur_priceSum", inline = TRUE)
                             ),
                             column(width = 6,
                                    tags$label("Quantity : ", `for`="pur_qtySum"),
                                    textOutput("pur_qtySum", inline = TRUE),br(),
                                    tags$label("Total Amount : ", `for`="pur_totAmtSum"),
                                    textOutput("pur_totAmtSum", inline = TRUE),br()
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(width = 6,
                                    tags$label("Purchase description : ", `for`="pur_descSum"),
                                    textOutput("pur_descSum", inline = TRUE),br()
                             ),
                             column(width = 6,
                                    tags$label("Supplier : ", `for`="pur_supplierSum"),
                                    textOutput("pur_supplierSum", inline = TRUE),br())
                           ),
                           hr(),
                           fluidRow(
                             column(width = 2,
                                    actionButton("pur_verify", "Verify", class = "btn btn-primary"),
                                    hidden(actionButton("pur_submit", "Submit", class = "btn btn-primary")),
                                    br()),
                             column(width = 2,
                                    actionButton("pur_reset_all", "Reset"))
                           )
                         )
                )
    )
  )
}

pur_reset_all_button = function(session, input, output){
  updateTabsetPanel(session, "addNewPurchase", selected = "Product")
  
  pur_reset_product_button(session, input, output, inline = F)
  pur_reset_transaction_button(session, input, output, inline = F)
  
  showElement("pur_verify")
  hideElement("pur_submit")
  
  output$pur_invNoSum = renderText("")
  output$pur_hsnSum = renderText("")
  output$pur_qtySum = renderText("")
  output$pur_dateSum = renderText("")
  output$pur_prodNameSum = renderText("")
  output$pur_prodIDSum = renderText("")
  output$pur_descSum = renderText("")
  output$pur_priceSum = renderText("")
  output$pur_totAmtSum = renderText("")
  output$pur_rateSum = renderText("")
  output$pur_supplierSum = renderText("")
}

pur_reset_product_button = function(session, input, output, inline = T){
  updateTextInput(session, "pur_invNo", value = "")
  updateNumericInput(session, "pur_hsn", value = 0)
  updateTextInput(session, "pur_prodName", value = "")
  updateTextInput(session, "pur_prodID", value = "")
  updateDateInput(session, "pur_date", value = as.Date(Sys.Date()))
  
  if(inline){
    showElement("pur_verify")
    hideElement("pur_submit")
  }
}

pur_reset_transaction_button = function(session, input, output, inline = T){
  updateTextAreaInput(session, "pur_desc", value = "")
  updateNumericInput(session, "pur_rate", value = 0)
  updateCheckboxInput(session, "pur_inclGst", value = FALSE)
  updateSelectizeInput(session, "pur_gst", selected = 18)
  updateSelectizeInput(session, "pur_supplier", selected = NULL)
  updateNumericInput(session, "pur_qty", value = 1)
  
  if(inline){
    showElement("pur_verify")
    hideElement("pur_submit")
  }
}

getPurchasePrices = function(input){
  price = 0
  costPrice = as.numeric(input$pur_rate)
  
  if(input$pur_inclGst){
    price = costPrice
    costPrice = costPrice / (1 + as.numeric(input$pur_gst) / 100)
  }
  else {
    price = costPrice + (costPrice * as.numeric(input$pur_gst) / 100)
  }
  
  totalPrice = price * as.numeric(input$pur_qty)
  
  return (c(costPrice, price, totalPrice))
}

validate_purchase = function(session, input, output){
  
  test_error = ""
  
  if(input$pur_invNo == "")
    test_error = paste0(test_error,"<li> Invoice Number cannot be empty</li>")
  if(input$pur_prodID == "")
    test_error = paste0(test_error,"<li> Product ID cannot be empty</li>")
  if(input$pur_prodName == "")
    test_error = paste0(test_error,"<li> Product Name cannot be empty</li>")
  
  if(!is.numeric(input$pur_rate))
    test_error = paste0(test_error,"<li> Rate of product is invalid</li>")
  else if(input$pur_rate <= 0)
    test_error = paste0(test_error,"<li> Rate of product cannot be 0 or less</li>")
  
  if(!is.numeric(input$pur_qty))
    test_error = paste0(test_error,"<li> Quantity is invalid</li>")
  else if(input$pur_qty < 1)
    test_error = paste0(test_error,"<li> Quantity cannot be 0 or less</li>")
  
  if(!is.numeric(input$pur_hsn))
    test_error = paste0(test_error,"<li> HSN Code is invalid</li>")
  else if(input$pur_hsn < 1)
    test_error = paste0(test_error,"<li> HSN Code be 0 or less</li>")
  
  if(!input$pur_supplier %in% loadSuppliers())
    test_error = paste0(test_error,"<li> Supplier is unregistered</li>")
  if(!input$pur_gst %in% c(18, 28))
    test_error = paste0(test_error,"<li> GST rate is unrecognized</li>")
  
  
  if(test_error == "")
    return (T)
  else
    return (paste0("<ul>", test_error, "</ul>"))
  
}

pur_verify_button = function(session, input, output){
  
  valid_form = validate_purchase(session, input, output)
  if(valid_form == T){
    hideElement("pur_verify")
    showElement("pur_submit")
  } else{
    showModal(modalDialog(HTML(paste0("<ul>",valid_form,"</ul>")), title = "Error", size = "m"))
    return()
  }
  
  output$pur_invNoSum = renderText(input$pur_invNo)
  output$pur_hsnSum = renderText(input$pur_hsn)
  output$pur_qtySum = renderText(input$pur_qty)
  output$pur_dateSum = renderText(as.character(input$pur_date))
  output$pur_prodNameSum = renderText(input$pur_prodName)
  output$pur_prodIDSum = renderText(input$pur_prodID)
  output$pur_descSum = renderText(input$pur_desc)
  output$pur_supplierSum = renderText(input$pur_supplier)
  
  prices = getPurchasePrices(input)
  output$pur_rateSum = renderText(prices[1])
  output$pur_priceSum = renderText(prices[2])
  output$pur_totAmtSum = renderText(prices[3])
}

pur_submit_button = function(session, input, output){
  source("modules/utils/dbCon.R")
   
  prev = as.numeric(dbGetQuery(con, paste0("SELECT \"QUANTITY\" from inventory where \"PRODUCT_ID\"=\'",input$pur_prodID, "'")))
  
  prev_qty = if(length(prev) == 0) 0 else prev
  
  updated_qty = prev_qty + as.numeric(input$pur_qty)
  
  prices = getPurchasePrices(input)
    
  sql_insert_purchase = paste0("INSERT INTO purchase VALUES (",
               "'",input$pur_invNo,"',",
               "'",input$pur_date,"',",
               "'",input$pur_prodID,"',",
               "'",input$pur_prodName,"',",
               prices[2],",",
               "'",input$pur_supplier,"',",
               input$pur_qty,",",
               prices[3],",",
               "'",input$pur_desc,"')")

  
  sql_insert_inventory = paste0("INSERT INTO inventory VALUES (",
               "'",input$pur_prodName,"',",
               "'",input$pur_prodID,"',",
               "'",input$pur_hsn,"',",
               prices[1],",",
               as.numeric(input$pur_gst),",",
               prices[2],",",
               updated_qty,",",
               prices[3],",",
               "'",input$pur_desc,"')")


  sql_update_inventory = paste0('UPDATE inventory',
                                ' SET "QUANTITY"=\'', updated_qty, "'",
                                ' WHERE "PRODUCT_ID"=\'', input$pur_prodID,"'")
  
  dbWithTransaction(con,
  {
    if(any(dim(dbGetQuery(con, paste0('Select * from inventory where "PRODUCT_ID" = \'', input$pur_prodID, "'"))) == 0)){
      dbExecute(con, sql_insert_inventory)
      if(dbExecute(con, sql_insert_purchase) == 1){
        showModal(modalDialog("Transaction added successfully!", title = "Success!", size = "m"))
      }
      else{
        showModal(modalDialog("Transaction failed to add", title = "Failed", size = "m"))
        dbBreak()
      }
    } else if(dbExecute(con, sql_update_inventory) == 1){
      if(dbExecute(con, sql_insert_purchase) == 1){
        showModal(modalDialog( br(),
                               p(paste("Previous Quantity:",prev_qty)),
                               p(paste0("Current Quantity:",updated_qty)),
                               title = "Success",
                               size = "m"))
      }
      else{
        showModal(modalDialog("Transaction failed to add", title = "Failed", size = "m"))
        dbBreak()
      }
    }

    rm(sql_insert_purchase, sql_update_inventory, sql_insert_inventory)
  })
  
  dbDisconnect(con)
  
  pur_reset_all_button(session,input,output)
}