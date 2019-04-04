add_supplier = function(input, output){
  div(
    h3("New Supplier"),
    hr(),
    br(),
    textInput("suppName", "Supplier name", placeholder = "Name of the supplier"),
    hr(),
    fluidRow(
      column(width = 6,
               textInput("address1", "Street name", placeholder = "Flat / House No. / Floor / Building"),
               textInput("address2", "", placeholder = "Colony / Street/ Locality"),
               numericInput("addrPin", "Pincode", min = 100000, max = 999999, value = 100000),
               textInput("addrCity", "City", placeholder = "City")
             ),
      column(width = 6,
               textInput("addrDist", "District"),
               textInput("addrState", "State"),
               textInput("addrCountry", "Country")
             )
    ),
    hr(),
    fluidRow(
      column(width = 6,
               numericInput("phoneNo", "Phone number", value = 0)
             ),
      column(width = 6,
               textInput("emailId", "Email Address")
             )
    ),
    hr(),
    fluidRow(
      column(width = 2,
              actionButton("submit", "Submit", class = "btn btn-primary"),br()
             ),
      column(width = 2,
              actionButton("reset_all", "Reset")
             )
    )
                
  )
}

show_supplier = function(input, output){
  div(
    h3("Showing all suppliers"),
    hr(),
    dataTableOutput("showSuppliers")
  )
}
