form_sale = function(input, output){
  div(
    tabsetPanel(id = "addNewSale",
                tabPanel("Product",
                         br(),
                         fluidRow(
                           column(width = 6,
                                  dateInput("sal_date",  label = "Date", format = "dd-mm-yyyy"),
                                  tags$style(HTML(".datepicker {z-index:99999 !important;}"))
                           ),
                           column(width = 6,
                                  textInput("sal_invNo", label = "Invoice Number")
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 6,
                                  numericInput("sal_hsn", label = "HSN Code", value = 0),
                                  textInput("sal_custName", "Customer's Name", placeholder = "Enter customer's name")
                           ),
                           column(width = 6,
                                  textInput("sal_prodName", label = "Product Name", placeholder = "Enter product name")
                                  
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 2,
                                  actionButton("sal_product_next", "Next", class = "btn btn-primary"),
                                  br()),
                           column(width = 2,
                                  actionButton("sal_reset_product", "Reset"))
                         )
                ),
                tabPanel("Sale Details",
                         br(),
                         fluidRow(
                           column(width = 4,
                                  numericInput("sal_rate", label = "Sales price/rate", value = 0),
                                  selectizeInput("sal_gst", label = "GST",
                                                 c("18%" = 18,
                                                   "28%" = 28)),
                                  checkboxInput("sal_inclGst", label = "Inclusive of GST?")
                           ),
                           column(width = 8,
                                  textAreaInput("sal_desc", label = "Sales Description", placeholder = "Sales description (optional)", rows = "5", cols = "50")
                           )
                         ),
                         hr(),
                         numericInput("sal_qty", "Quantity", value = 1),
                         hr(),
                         fluidRow(
                           column(width = 2,
                                  actionButton("sal_sale_next", "Next", class = "btn btn-primary"),
                                  br()),
                           column(width = 2,
                                  actionButton("sal_reset_transaction", "Reset"))
                         )
                ),
                tabPanel("Summary",
                         br(),
                         fluidRow(
                           column(width = 6,
                                  tags$label("Date : ", `for`="sal_dateSum"),
                                  textOutput("sal_dateSum", inline = TRUE),br(),
                                  tags$label("Invoice No. : ", `for`="sal_invNoSum"),
                                  textOutput("sal_invNoSum", inline = TRUE),br(),
                                  tags$label("Customer name : ", `for`="sal_custNameSum"),
                                  textOutput("sal_custNameSum", inline = TRUE),br()
                           ),
                           column(width = 6,
                                  tags$label("Product Name : ", `for`="sal_prodNameSum"),
                                  textOutput("sal_prodNameSum", inline = TRUE),br(),
                                  tags$label("HSN Code : ", `for`="sal_hsnSum"),
                                  textOutput("sal_hsnSum", inline = TRUE),br()
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 6,
                                  tags$label("Sales price : ", `for`="sal_rateSum"),
                                  textOutput("sal_rateSum", inline = TRUE),br(),
                                  tags$label("Price with GST : ", `for`="sal_priceSum"),
                                  textOutput("sal_priceSum", inline = TRUE)
                                  
                           ),
                           column(width = 6,
                                  tags$label("Quantity : ", `for`="sal_qtySum"),
                                  textOutput("sal_qtySum", inline = TRUE),br(),
                                  tags$label("Total Amount : ", `for`="sal_totAmt"),
                                  textOutput("sal_totAmtSum", inline = TRUE),br()
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 12,
                                  tags$label("Sales description : ", `for`="sal_descSum"),
                                  textOutput("sal_descSum", inline = TRUE),br()
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 2,
                                  actionButton("sal_verify", "Verify", class = "btn btn-primary"),
                                  hidden(actionButton("sal_submit", "Submit", class = "btn btn-primary")),
                                  br()),
                           column(width = 2,
                                  actionButton("sal_reset_all", "Reset"))
                         )
                )
    )
  )
}

sal_reset_all_button = function(session, input, output){
  updateTabsetPanel(session, "addNewSale", selected = "Product")
  
  sal_reset_product_button(session, input, output, inline = F)
  sal_reset_transaction_button(session, input, output, inline = F)
  
  showElement("sal_verify")
  hideElement("sal_submit")
  
  output$sal_invNoSum = renderText("")
  output$sal_hsnSum = renderText("")
  output$sal_qtySum = renderText("")
  output$sal_dateSum = renderText("")
  output$sal_prodNameSum = renderText("")
  output$sal_prodIDSum = renderText("")
  output$sal_descSum = renderText("")
  output$sal_priceSum = renderText("")
  output$sal_totAmtSum = renderText("")
  output$sal_rateSum = renderText("")
  output$sal_custNameSum = renderText("")
  output$sal_custMiscDetsSum = renderText("")
}

sal_reset_product_button = function(session, input, output, inline = T){
  updateNumericInput(session, "sal_invNo", value = 0)
  updateNumericInput(session, "sal_hsn", value = 0)
  updateTextInput(session, "sal_prodName", value = "")
  updateDateInput(session, "sal_date", value = as.Date(Sys.Date()))
  updateTextInput(session,"sal_custName", value = "")
  
  if(inline){
    showElement("sal_verify")
    hideElement("sal_submit")
  }
}

sal_reset_transaction_button = function(session, input, output, inline = T){
  updateTextInput(session,"sal_rate", value = "")
  updateSelectizeInput(session,"sal_gst", selected = "18%")
  updateTextAreaInput(session,"sal_desc", value = "")
  updateCheckboxInput(session, "sal_inclGst", value = F)
  updateNumericInput(session, "sal_qty", value = 1)
  
  if(inline){
    showElement("sal_verify")
    hideElement("sal_submit")
  }
}

getSalesPrices = function(input){
  price = 0
  salePrice = as.numeric(input$sal_rate)
  
  if(input$sal_inclGst){
    price = salePrice
    salePrice = salePrice / (1 + as.numeric(input$sal_gst) / 100)
  }
  else {
    price = salePrice + (salePrice * as.numeric(input$sal_gst) / 100)
  }
  
  totalPrice = price * as.numeric(input$sal_qty)
  
  return (c(salePrice, price, totalPrice))
}

validate_sales = function(session, input, output){
  
  test_error = ""
  
  if(input$sal_invNo == "")
    test_error = paste0(test_error,"<li> Invoice Number cannot be empty</li>")
  
  if(input$sal_prodName == "")
    test_error = paste0(test_error,"<li> Product Name cannot be empty</li>")
  
  if(input$sal_custName == "")
    test_error = paste0(test_error,"<li> Customer Name cannot be empty</li>")
  
  if(!input$sal_gst %in% c(18, 28))
    test_error = paste0(test_error,"<li> GST rate is unrecognized</li>")
  
  if(!is.numeric(input$sal_rate))
    test_error = paste0(test_error,"<li> Rate of product is invalid</li>")
  else if(input$sal_rate <= 0)
    test_error = paste0(test_error,"<li> Rate of product cannot be 0 or less</li>")
  
  if(!is.numeric(input$sal_hsn))
    test_error = paste0(test_error,"<li> Rate of product is invalid</li>")
  else if(input$sal_hsn <= 0)
    test_error = paste0(test_error,"<li> Rate of product cannot be 0 or less</li>")
  
  if(!is.numeric(input$sal_qty))
    test_error = paste0(test_error,"<li> Quantity is invalid</li>")
  else if(input$sal_qty < 1)
    test_error = paste0(test_error,"<li> Quantity cannot be 0 or less</li>")
  
  if(test_error == "")
    return (T)
  else
    return (paste0("<ul>", test_error, "</ul>"))
  
}

sal_verify_button = function(session, input, output){
  
  valid_form = validate_sales(session, input, output)
  if(valid_form == T){
    hideElement("sal_verify")
    showElement("sal_submit")
  } else{
    showModal(modalDialog(HTML(paste0("<ul>",valid_form,"</ul>")), title = "Error", size = "m"))
    return()
  }
  
  output$sal_invNoSum = renderText(input$sal_invNo)
  output$sal_hsnSum = renderText(input$sal_hsn)
  output$sal_qtySum = renderText(input$sal_qty)
  output$sal_dateSum = renderText(as.character(input$sal_date))
  output$sal_prodNameSum = renderText(input$sal_prodName)
  output$sal_descSum = renderText(input$sal_desc)
  output$sal_custNameSum = renderText(input$sal_custName)
  
  prices = getSalesPrices(input)
  output$sal_rateSum = renderText(prices[1])
  output$sal_priceSum = renderText(prices[2])
  output$sal_totAmtSum = renderText(prices[3])
}

sal_submit_button = function(session, input, output){
  
  source("modules/utils/dbCon.R")

  if(any(dim(dbGetQuery(con, paste0("SELECT \"InvoiceNo\" from sales where \"InvoiceNo\"=\'",input$sal_invNo, "' and \"Date\" = '",input$sal_date, "' and \"HSN\" = '",input$sal_hsn, "' and \"ProductName\" = '",input$sal_prodName,"'")))!=0)){
    shinyalert(text = "An invoice number with this specifications already exists. Either delete that invoice or change the invoice number", title = "Invoice exists", type = "warning")
    dbDisconnect(con)
    return()
  }
  
  if(any(dim(dbGetQuery(con, paste0("SELECT \"ITEM_NAME\" from inventory where \"ITEM_NAME\"=\'",input$sal_prodName, "' and \"HSN\" = '",input$sal_hsn,"'")))==0)){
    shinyalert(text = "Product not in inventory. You need to purchase it first.", title = "Failed", type = "error")
    dbDisconnect(con)
    return()
  }
  
  prev = as.numeric(dbGetQuery(con, paste0("SELECT \"QUANTITY\" from inventory where \"ITEM_NAME\"=\'",input$sal_prodName, "' and \"HSN\" = '",input$sal_hsn,"'")))

  prev_qty = if(length(prev) == 0) 0 else prev

  updated_qty = prev_qty - as.numeric(input$sal_qty)
  
  if(updated_qty < 0){
    shinyalert(type = "warning",
               text = HTML(paste0("<p>Product quantity is insufficient</p>",
                                  "<p>Available: ",prev_qty,"</p>")),
               title = "Unable to add transaction",
               html = T)
    return ()
  }
  else {

    prices = getSalesPrices(input)
  
    sql_insert_sales = paste0("INSERT INTO sales VALUES (",
                                 "'",input$sal_invNo,"',",
                                 "'",input$sal_date,"',",
                                 "'",input$sal_custName,"',",
                                 "'",input$sal_prodName,"',",
                                 "'",input$sal_hsn,"',",
                                 "'",input$sal_gst,"',",
                                 input$sal_qty,",",
                                 prices[2],",",
                                 prices[3],",",
                                 "'",input$sal_desc,"')")
  
    sql_update_inventory = paste0('UPDATE inventory',
                                  ' SET "QUANTITY"=\'', updated_qty, "'",
                                  ' WHERE "ITEM_NAME"=\'', input$sal_prodName,"'",
                                  ' AND "HSN" = \'',input$sal_hsn,"'")
  
    dbWithTransaction(con,
    {
      if(dbExecute(con, sql_update_inventory) == 1){
        if(dbExecute(con, sql_insert_sales) == 1){
          shinyalert( 
            title = "Success",
            html = T,
            text = HTML(paste0("<br/> <p>Previous Quantity : ",prev_qty, "</p><p>Current Quantity : ",updated_qty, "</p>")),
            type = "success"
          )
        }
        else{
          shinyalert(text = "Transaction failed to add", title = "Failed", type = "error")
          dbBreak()
        }
      }
      rm(sql_insert_sales, sql_update_inventory)
    })
  }
  dbDisconnect(con)
  sal_reset_all_button(session,input,output)
}