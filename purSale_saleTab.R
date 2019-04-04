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
                                  textInput("sal_custAdress", "Address", placeholder = "Enter customer's address"),
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
                                  textInput("sRate", label = "Sales price/rate", placeholder = "Sales price/rate"),
                                  selectizeInput("sGst", label = "GST",
                                                 c("18%" = 18,
                                                   "28%" = 28)),
                                  checkboxInput("inclsGst", label = "Inclusive of GST?")
                           ),
                           column(width = 8,
                                  textAreaInput("sDesc", label = "Sales Description", placeholder = "Sales description (optional)", rows = "5", cols = "50")
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
                                  tags$label("Sales price : ", `for`="sal_rateSum"),
                                  textOutput("sal_rateSum", inline = TRUE),br()
                                  
                           ),
                           column(width = 6,
                                  tags$label("Quantity : ", `for`="sal_qtySum"),
                                  textOutput("sal_qtySum", inline = TRUE),br(),
                                  tags$label("Total Amount : ", `for`="sal_totAmt"),
                                  textOutput("sal_totAmt", inline = TRUE),br()
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
                                  actionButton("sal_submit", "Submit", class = "btn btn-primary"),br()),
                           column(width = 2,
                                  actionButton("sal_reset_all", "Reset"))
                         )
                )
    )
  )
}