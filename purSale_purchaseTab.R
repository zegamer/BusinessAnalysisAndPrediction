loadSuppliers = function(){
  source("dbCon.R")
  suppliersList = dbGetQuery(con,"select \"Name\" from supplier")
  dbDisconnect(con)
  return(suppliersList[,])
}

form_purchase = function(input, output){
  div(
    tabsetPanel(id = "addNewPurchase",
                tabPanel("Product", product_tab),
                tabPanel("Purchase Details", purDets_tab),
                tabPanel("Summary", id = "pur_summary", summary_tab)
    )
  )
}

product_tab <- div(
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
)

purDets_tab <- div(
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
           textAreaInput("pur_desc", label = "Purchase Description", placeholder = "Purchase description (optional)", rows = "5", cols = "50")
    )
  ),
  hr(),
  fluidRow(
    column(width = 6,
           numericInput("pur_qty", "Quantity", value = 0)
    ),
    column(width = 6,
           selectInput("pur_supplier", "Supplier", choices = loadSuppliers())
    )
  ),
  hr(),
  actionButton("pur_reset_transaction", "Reset")
)

summary_tab <- div(
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
  output$pur_priceSum = renderText("")
  output$pur_totAmtSum = renderText("")
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
  
  price = 0
  costPrice = as.numeric(input$pur_rate)
  
  if(input$pur_inclGst){
    price = costPrice
    costPrice = costPrice / (1 + as.numeric(input$pur_gst) / 100)
  }
  else {
    price = costPrice + (costPrice * as.numeric(input$pur_gst) / 100)
  }  
  
  output$pur_rateSum = renderText(costPrice)
  output$pur_priceSum = renderText(price)
  output$pur_totAmtSum = renderText(as.numeric(price * as.numeric(input$pur_qty)))
}