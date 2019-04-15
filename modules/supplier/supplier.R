add_supplier = function(input, output){
  div(
    h3("New Supplier"),
    hr(),
    br(),
    fluidRow(
      column(
        width = 6,
        textInput("suppName", "Supplier name", placeholder = "Name of the supplier")
      ),
      column(
        width = 6,
        textInput("gstNo", "GST Number", placeholder = "GST Number"),
        selectizeInput("gstType", "GST Type",
                       c("Regular", "Composite"))
      )
    ),
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
              actionButton("supplier_submit", "Submit", class = "btn btn-primary"),br()
             ),
      column(width = 2,
              actionButton("supplier_reset", "Reset")
             )
    )
                
  )
}

show_supplier = function(input, output){
  div(
    h3("Showing all suppliers"),
    hr(),
    uiOutput("show_supplier")
  )
}

supplier_submit_button = function(session, input, output){
  
  # Validate all fields first
  
  source("modules/utils/dbCon.R")
  
  sql = paste0("INSERT INTO supplier VALUES (",
               "'", input$suppName,"',",
               "'",input$addrPin,"',",
               "'",input$addrCity,"',",
               "'",input$addrDist,"',",
               "'",input$addrState,"',",
               "'",input$addrCountry,"',",
               input$phoneNo,",",
               "'",input$emailId,"',",
               "'",input$address1,"',",
               "'",input$address2,"',",
               "'",input$gstNo,"',",
               "'",input$gstType,"'",
               ")")
  
  tryCatch({
    if(dbExecute(con, sql) != 0){
      showModal(modalDialog(title = "Row added successfully", size = "m"))
    }
    rm(sql)
    supplier_reset_button(session, input, output)
  }, finally = {dbDisconnect(con)})
  
  
}

supplier_edit_button = function(data, row){
  
}

supplier_delete_button = function(data, row){
  
}

supplier_reset_button = function(session, input, output){
  updateTextInput(session,"suppName", value = "")
  updateTextInput(session,"gstNo", value = "")
  updateTextInput(session,"address1", value = "")
  updateTextInput(session,"address2", value = "")
  updateNumericInput(session,"addrPin", value = 0)
  updateTextInput(session,"addrCity", value = "")
  updateTextInput(session,"addrDist", value = "")
  updateTextInput(session,"addrState", value = "")
  updateTextInput(session,"addrCountry", value = "")
  updateTextInput(session,"emailId", value = "")
  updateNumericInput(session, "phoneNo", value = 0)
  updateSelectizeInput(session, "gstType", selected = "Regular")
}
