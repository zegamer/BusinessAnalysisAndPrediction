form_purchase = function(input, output){
  div(
    tabsetPanel(id = "addNewPurchase",
      tabPanel("Product",
               br(),
               fluidRow(
                 column(width = 6,
                        dateInput("pur_date", label = "Date", format = "dd/mm/yyyy")
                 ),
                 column(width = 6,
                        numericInput("pur_invNo", label = "Invoice Number", value = 0)
                 )
               ),
               hr(),
               fluidRow(
                 column(width = 6,
                        textInput("pur_prodID", label = "Product ID", placeholder = "Enter product ID"),
                        numericInput("pur_hsn", label = "HSN Code", value = 0)
                 ),
                 column(width = 6,
                        textInput("pur_prodName", label = "Product name", placeholder = "Enter product name"),br(),
                        actionButton("pur_reset_product", "Reset")
                 )
               )
      ),
      tabPanel("Purchase Details",
               br(),
               fluidRow(
                 column(width = 4,
                        textInput("pur_rate", label = "Cost", placeholder = "Cost"),
                        selectizeInput("pur_gst", label = "GST",
                                       c("18%" = 18,
                                         "28%" = 28)),
                        checkboxInput("pur_inclGst", label = "Inclusive of GST?")
                 ),
                 column(width = 8,
                        textAreaInput("pur_desc", label = "Purchase Description", placeholder = "Purchase description (optional)", rows = "5", cols = "50")
                 )
               ),
               hr(),
               numericInput("pur_qty", "Quantity", value = 0),
               hr(),
               actionButton("pur_reset_transaction", "Reset")
      ),
      tabPanel("Summary",
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
                        tags$label("Total Amount : ", `for`="pur_totAmt"),
                        textOutput("pur_totAmt", inline = TRUE),br()
                 )
               ),
               hr(),
               fluidRow(
                 column(width = 12,
                        tags$label("Purchase description : ", `for`="pur_descSum"),
                        textOutput("pur_descSum", inline = TRUE),br()
                 )
               ),
               hr(),
               fluidRow(
                 column(width = 2,
                        actionButton("pur_submit", "Submit", class = "btn btn-primary"),br()),
                 column(width = 2,
                        actionButton("pur_reset_all", "Reset"))
               )
      )
    )
  )
}

addNewPurchase = function(input, output, sales = FALSE){
  div(
    h3("New entry"),
    hr(),
    form_purchase()
  )
}

pur_reset_all_button = function(session, input, output){
  updateTabsetPanel(session, "addNewPurchase", selected = "Product")
  pur_reset_product_button(session)
  pur_reset_transaction_button(session)
  
  output$pur_invNoSum = renderText("")
  output$pur_hsnSum = renderText("")
  output$pur_qtySum = renderText("")
  output$pur_dateSum = renderText("")
  output$pur_prodNameSum = renderText("")
  output$pur_prodIDSum = renderText("")
  output$pur_descSum = renderText("")
  output$pur_price = renderText("")
  output$pur_totAmt = renderText("")
  output$pur_rateSum = renderText("")
}

pur_reset_product_button = function(session, input, output){
  updateNumericInput(session, "pur_invNo", value = 0)
  updateNumericInput(session, "pur_hsn", value = 0)
  updateNumericInput(session, "pur_qty", value = 0)
  updateTextInput(session, "pur_prodName", value = "")
  updateTextInput(session, "pur_prodID", value = "")
  updateDateInput(session, "pur_date", value = as.Date(Sys.Date()))
}

pur_reset_transaction_button = function(session, input, output){
  updateTextAreaInput(session, "pur_desc", value = "")
  updateTextInput(session, "pur_rate", value = "")
  updateCheckboxInput(session, "pur_inclGst", value = FALSE)
  updateSelectizeInput(session, "pur_gst", selected = "18%")
  updateNumericInput(session, "pur_qty", value = 0)
}

pur_submit_form_button = function(session, input, output){
  output$pur_invNoSum = renderText(input$pur_invNo)
  output$pur_hsnSum = renderText(input$pur_hsn)
  output$pur_qtySum = renderText(input$pur_qty)
  output$pur_dateSum = renderText(as.character(input$pur_date))
  output$pur_prodNameSum = renderText(input$pur_prodName)
  output$pur_prodIDSum = renderText(input$pur_prodID)
  output$pur_descSum = renderText(input$pur_desc)
  
  priceVal = if(input$pur_inclGst) input$pur_rate else as.numeric(input$pur_rate) + (as.numeric(input$pur_rate) * as.numeric(input$pur_gst) / 100)
  output$pur_price = renderText(priceVal)
  output$pur_totAmt = renderText(as.numeric(priceVal * input$pur_qty))
}

showAll = function(input, output){
  div(
    h3("Showing all items"),
    hr(),
    dataTableOutput("show_all")
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