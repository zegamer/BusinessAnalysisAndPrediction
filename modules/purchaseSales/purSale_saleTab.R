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
                                  textInput("sal_prodID", label = "Product ID", placeholder = "Enter product ID")
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
                tabPanel("Customer Details",
                         br(),
                         fluidRow(
                           column(width = 6,
                                  textInput("sal_custName", "Name", placeholder = "Enter customer's name"),
                                  textInput("sal_custAddress", "Address", placeholder = "Enter customer's address"),
                                  numericInput("sal_custPhone", "Phone Number", value = 0)
                           ),
                           column(width = 6,
                                  textAreaInput("sal_custMiscDets", "Misc. details (Optional)", rows = "5", cols = "50")
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 2,
                                  actionButton("sal_customer_next", "Next", class = "btn btn-primary"),
                                  br()),
                           column(width = 2,
                                  actionButton("sal_reset_custDets", "Reset"))
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
                                  textOutput("sal_invNoSum", inline = TRUE),br()
                           ),
                           column(width = 6,
                                  tags$label("Product ID : ", `for`="sal_prodIDSum"),
                                  textOutput("sal_prodIDSum", inline = TRUE),br(),
                                  tags$label("Product Name : ", `for`="sal_prodNameSum"),
                                  textOutput("sal_prodNameSum", inline = TRUE),br(),
                                  tags$label("HSN Code : ", `for`="sal_hsnSum"),
                                  textOutput("sal_hsnSum", inline = TRUE),br()
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 6,
                                  tags$label("Customer name : ", `for`="sal_custNameSum"),
                                  textOutput("sal_custNameSum", inline = TRUE),br(),
                                  tags$label("Customer Address : ", `for`="sal_custAddressSum"),
                                  textOutput("sal_custAddressSum", inline = TRUE),br(),
                                  tags$label("Phone : ", `for`="sal_custPhoneSum"),
                                  textOutput("sal_custPhoneSum", inline = TRUE),br()
                                  
                                  
                           ),
                           column(width = 6,
                                  tags$label("Misc. Details : ", `for`="sal_custMiscDetsSum"),
                                  textOutput("sal_custMiscDetsSum", inline = TRUE),br()
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
  sal_reset_custDets_button(session, input, output, inline = F)
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
  output$sal_custAddressSum = renderText("")
  output$sal_custPhoneSum = renderText("")
  output$sal_custMiscDetsSum = renderText("")
}

sal_reset_product_button = function(session, input, output, inline = T){
  updateNumericInput(session, "sal_invNo", value = 0)
  updateNumericInput(session, "sal_hsn", value = 0)
  updateTextInput(session, "sal_prodName", value = "")
  updateTextInput(session, "sal_prodID", value = "")
  updateDateInput(session, "sal_date", value = as.Date(Sys.Date()))
  
  if(inline){
    showElement("sal_verify")
    hideElement("sal_submit")
  }
}

sal_reset_custDets_button = function(session, input, output, inline = T){
  updateTextInput(session,"sal_custName", value = "")
  updateTextAreaInput(session,"sal_custAddress", value = "")
  updateNumericInput(session,"sal_custPhone", value = 0)
  updateTextAreaInput(session,"sal_custMiscDets", value = "")
  
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
  
  if(input$sal_prodID == "")
    test_error = paste0(test_error,"<li> Product ID cannot be empty</li>")
  
  if(input$sal_custName == "")
    test_error = paste0(test_error,"<li> Customer Name cannot be empty</li>")
  
  if(input$sal_custAddress == "")
    test_error = paste0(test_error,"<li> Customer Address cannot be empty</li>")
  
  if(!any(grep("[1-9][0-9]{9}$", input$sal_custPhone)))
    test_error = paste0(test_error,"<li> Enter a valid phone number</li>")
  
  if(!input$sal_gst %in% c(18, 28))
    test_error = paste0(test_error,"<li> GST rate is unrecognized</li>")
  
  if(!is.numeric(input$sal_rate))
    test_error = paste0(test_error,"<li> Rate of product is invalid</li>")
  else if(input$sal_rate <= 0)
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
  output$sal_prodIDSum = renderText(input$sal_prodID)
  output$sal_descSum = renderText(input$sal_desc)
  output$sal_custNameSum = renderText(input$sal_custName)
  output$sal_custAddressSum = renderText(input$sal_custAddress)
  output$sal_custPhoneSum = renderText(input$sal_custPhone)
  output$sal_custMiscDetsSum = renderText(input$sal_custMiscDets)
  
  prices = getSalesPrices(input)
  output$sal_rateSum = renderText(prices[1])
  output$sal_priceSum = renderText(prices[2])
  output$sal_totAmtSum = renderText(prices[3])
}

sal_submit_button = function(session, input, output){
  # source("modules/utils/dbCon.R")
  #
  # prev_qty = as.numeric(dbGetQuery(con, paste0("SELECT \"QUANTITY\" from inventory where \"PRODUCT_ID\"=\'",input$sal_prodID, "'")))
  # 
  # sql_insert_sales = paste0("INSERT INTO purchase VALUES (",
  #              "'",output$sal_invNoSum,"',",
  #              "'",output$sal_dateSum,"',",
  #              "'",output$sal_prodIDSum,"',",
  #              "'",output$sal_custNameSum,"',",
  #              "'",output$sal_custAddressSum,"',",
  #              "'",output$sal_custPhoneSum,"',",
  #              "'",output$sal_custMiscDetsSum,"',",
  #              output$sal_priceSum,",",
  #              output$sal_qtySum,",",
  #              output$sal_totAmtSum,",",
  #              "'",output$sal_descSum,"'",
  #              ")")
  # 
  # 
  # sql_update_inventory = paste0('UPDATE inventory',
  #                               ' SET "QUANTITY"=\'',prev_qty - as.numeric(output$sal_qtySum),"'",
  #                               'WHERE "PRODUCT_ID"=\'',output$pur_prodIDSum,"'")
  # 
  # tryCatch({
  #   if(is.null(dbGetQuery(con, sql_update_inventory))) {
  #     if(is.null(dbGetQuery(con, sql_insert_sales)))
  #     showModal(modalDialog(title = "Records updated successfully", size = "m", fade = T))
  #   else
  #     showModal(modalDialog(title = "Error occurred while adding", size = "m", fade = T))
  # } else
  #     showModal(modalDialog(title = "Error occurred while adding", size = "m", fade = T))
  # 
  #   rm(sql_insert_purchase)
  #   rm(sql_update_inventory)
  # 
  #   pur_reset_all_button(session, input, output)
  # }, finally = {dbDisconnect(con)})
  
  ##################################################################################
  # New implementation below
  ##################################################################################
  
  # source("modules/utils/dbCon.R")
  # 
  # prev = as.numeric(dbGetQuery(con, paste0("SELECT \"QUANTITY\" from inventory where \"PRODUCT_ID\"=\'",input$pur_prodID, "'")))
  # 
  # prev_qty = if(length(prev) == 0) 0 else prev
  # 
  # updated_qty = prev_qty + as.numeric(input$pur_qty)
  # 
  # prices = getPurchasePrices(input)
  # 
  # sql_insert_purchase = paste0("INSERT INTO purchase VALUES (",
  #                              "'",input$pur_invNo,"',",
  #                              "'",input$pur_date,"',",
  #                              "'",input$pur_prodID,"',",
  #                              "'",input$pur_prodName,"',",
  #                              prices[2],",",
  #                              "'",input$pur_supplier,"',",
  #                              input$pur_qty,",",
  #                              prices[3],",",
  #                              "'",input$pur_desc,"')")
  # 
  # sql_update_inventory = paste0('UPDATE inventory',
  #                               ' SET "QUANTITY"=\'', updated_qty, "'",
  #                               ' WHERE "PRODUCT_ID"=\'', input$pur_prodID,"'")
  # 
  # dbWithTransaction(con,
  # {
  #   if(any(dim(dbGetQuery(con, paste0('Select * from inventory where "PRODUCT_ID" = \'', input$pur_prodID, "'"))) == 0)){
  #     print("Product not in inventory")
  #     print(paste0('Select * from inventory where "PRODUCT_ID" = \'', input$pur_prodID, "'"))
  #     if(dbExecute(con, sql_insert_purchase) == 1){
  #       print(sql_insert_purchase)
  #       showModal(modalDialog("Transaction added successfully!", title = "Success!", size = "m"))
  #     }
  #     else{
  #       showModal(modalDialog("Transaction failed to add", title = "Failed", size = "m"))
  #       dbBreak()
  #     }
  #   } else if(dbExecute(con, sql_update_inventory) == 1){
  #     print("Item found and updating in inventory")
  #     print(sql_update_inventory)
  #     if(dbExecute(con, sql_insert_purchase) == 1){
  #       print(sql_insert_purchase)
  #       showModal(modalDialog( br(),
  #                              p(paste("Previous Quantity:",prev_qty)),
  #                              p(paste0("Current Quantity:",updated_qty)),
  #                              title = "Success",
  #                              size = "m"))
  #     }
  #     else{
  #       showModal(modalDialog("Transaction failed to add", title = "Failed", size = "m"))
  #       dbBreak()
  #     }
  #   }
  #   
  #   rm(sql_insert_purchase, sql_update_inventory, sql_insert_inventory)
  # })
  
  showModal(modalDialog(title = "Row added successfully",
                        # br(),
                        # p(paste("Previous Quantity:",prev_qty)),
                        # p(paste0("Current Quantity:",prev_qty - as.numeric(input$sal_qty))),
                        size = "m",
                        fade = T))
  
  # showModal(modalDialog(title = "Row added successfully", size = "m", fade = T))
  
  dbDisconnect(con)
  
  sal_reset_all_button(session,input,output)
}