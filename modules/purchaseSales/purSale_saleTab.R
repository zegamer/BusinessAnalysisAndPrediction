form_sale = function(input, output){
  div(
    tabsetPanel(id = "addNewSale",
                tabPanel("Product",
                         br(),
                         fluidRow(
                           column(width = 6,
                                  dateInput("sal_date", label = "Date", format = "dd/mm/yyyy")
                           ),
                           column(width = 6,
                                  numericInput("sal_invNo", label = "Invoice Number", value = 0)
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 6,
                                  textInput("sal_prodID", label = "Product ID", placeholder = "Enter product ID"),
                                  numericInput("sal_hsn", label = "HSN Code", value = 0)
                           ),
                           column(width = 6,
                                  textInput("sal_prodName", label = "Product name", placeholder = "Enter product name"),br(),
                                  actionButton("sal_reset_product", "Reset")
                           )
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
                         actionButton("sal_reset_custDets", "Reset")
                         
                ),
                tabPanel("Sale Details",
                         br(),
                         fluidRow(
                           column(width = 4,
                                  textInput("sal_rate", label = "Sales price/rate", placeholder = "Sales price/rate"),
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
                         numericInput("sal_qty", "Quantity", value = 0),
                         hr(),
                         actionButton("sal_reset_transaction", "Reset")
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
                                  textOutput("sal_rateSum", inline = TRUE),br()
                                  
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
                                  # shinyjs::showElement(actionButton("sal_submit", "Verify", class = "btn btn-primary")),
                                  # shinyjs::hideElement(actionButton("sal_submitFinal", "Submit", class = "btn btn-primary")),
                                  actionButton("sal_submit", "Verify", class = "btn btn-primary"),
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
  # shinyjs::toggleElementElement("sal_submit")
  # shinyjs::toggleElement("sal_submitFinal")
  
  sal_reset_product_button(session)
  sal_reset_custDets_button(session)
  sal_reset_transaction_button(session)
  
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

sal_reset_product_button = function(session, input, output){
  updateNumericInput(session, "sal_invNo", value = 0)
  updateNumericInput(session, "sal_hsn", value = 0)
  updateTextInput(session, "sal_prodName", value = "")
  updateTextInput(session, "sal_prodID", value = "")
  updateDateInput(session, "sal_date", value = as.Date(Sys.Date()))
}

sal_reset_custDets_button = function(session, input, output){
  updateTextInput(session,"sal_custName", value = "")
  updateTextAreaInput(session,"sal_custAddress", value = "")
  updateNumericInput(session,"sal_custPhone", value = 0)
  updateTextAreaInput(session,"sal_custMiscDets", value = "")
}

sal_reset_transaction_button = function(session, input, output){
  updateTextInput(session,"sal_rate", value = "")
  updateSelectizeInput(session,"sal_gst", selected = 0)
  updateTextAreaInput(session,"sal_desc", value = "")
  updateCheckboxInput(session, "sal_inclGst", value = F)
  updateNumericInput(session, "sal_qty", value = 0)
}

sal_submit_button = function(session, input, output){
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
  
  price = 0
  salePrice = as.numeric(input$sal_rate)
  
  if(input$sal_inclGst){
    price = salePrice
    salePrice = salePrice / (1 + as.numeric(input$sal_gst) / 100)
  }
  else {
    price = salePrice + (salePrice * as.numeric(input$sal_gst) / 100)
  }  
  
  output$sal_rateSum = renderText(salePrice)
  output$sal_priceSum = renderText(price)
  output$sal_totAmtSum = renderText(as.numeric(price * as.numeric(input$sal_qty)))
  
  # shinyjs::toggleElement("sal_submit")
  # shinyjs::toggleElement("sal_submitFinal")
  
  sal_submitFinal_button(session, input, output)
}

sal_submitFinal_button = function(session, input, output){
  # source("modules/utils/dbCon.R")
  # 
  # sql = paste0("INSERT INTO purchase VALUES (",
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
  # tryCatch({
  #   if(dbExecute(con, sql) != 0){
  #     showModal(modalDialog(title = "Row added successfully", size = "m"))
  #   }
  #   rm(sql)
  #   supplier_reset_button(session, input, output)
  # }, finally = {dbDisconnect(con)})
  
  showModal(modalDialog(title = "Row added successfully", size = "m"))
  sal_reset_all_button(session, input, output)
}