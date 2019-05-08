add_supplier = function(input, output){
  div(
    h3("New Supplier"),
    hr(),
    br(),
    fluidRow(
      column(
        width = 6,
        textInput("sup_name", "Supplier name", placeholder = "Name of the supplier")
      ),
      column(
        width = 6,
        textInput("sup_gstNo", "GST Number", placeholder = "GST Number"),
        selectizeInput("sup_gstType", "GST Type",
                       c("Regular", "Composite"))
      )
    ),
    hr(),
    fluidRow(
      column(width = 6,
               textInput("sup_address1", "Street name", placeholder = "Flat / House No. / Floor / Building"),
               textInput("sup_address2", "", placeholder = "Colony / Street/ Locality"),
               numericInput("sup_addrPin", "Pincode", min = 100000, max = 999999, value = 100000),
               textInput("sup_addrCity", "City", placeholder = "City")
             ),
      column(width = 6,
               textInput("sup_addrDist", "District"),
               textInput("sup_addrState", "State"),
               textInput("sup_addrCountry", "Country")
             )
    ),
    hr(),
    fluidRow(
      column(width = 6,
               numericInput("sup_phoneNo", "Phone number", value = 0)
             ),
      column(width = 6,
               textInput("sup_emailId", "Email Address")
             )
    ),
    hr(),
    fluidRow(
      column(width = 2,
             actionButton("supplier_verify", "Verify", class = "btn btn-primary"),
             hidden(actionButton("supplier_submit", "Submit", class = "btn btn-primary")),br()
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

supplier_reset_button = function(session, input, output){
  updateTextInput(session, "sup_name", value = "")
  updateTextInput(session, "sup_gstNo", value = "")
  updateTextInput(session, "sup_address1", value = "")
  updateTextInput(session, "sup_address2", value = "")
  updateNumericInput(session, "sup_addrPin", value = 0)
  updateTextInput(session, "sup_addrCity", value = "")
  updateTextInput(session, "sup_addrDist", value = "")
  updateTextInput(session, "sup_addrState", value = "")
  updateTextInput(session, "sup_addrCountry", value = "")
  updateTextInput(session, "sup_emailId", value = "")
  updateNumericInput(session, "sup_phoneNo", value = 0)
  updateSelectizeInput(session, "sup_gstType", selected = "Regular")
  
  hideElement("supplier_submit")
  showElement("supplier_verify")
}

validate_supplier = function(input){
  test_error = ""
  
  if(input$sup_name == "")
    test_error = paste0(test_error,"<li> Name is empty</li>")
  if(!input$sup_gstType %in% c("Regular", "Composite"))
    test_error = paste0(test_error,"<li> GST Type is unrecognized</li>")
  if(input$sup_address1 == "")
    test_error = paste0(test_error,"<li> Address Line 1 should not be empty</li>")
  if(input$sup_addrCity == "")
    test_error = paste0(test_error,"<li> City should not be empty</li>")
  if(input$sup_addrDist == "")
    test_error = paste0(test_error,"<li> District should not be empty</li>")
  if(input$sup_addrState == "")
    test_error = paste0(test_error,"<li> State should not be empty</li>")
  if(input$sup_addrCountry == "")
    test_error = paste0(test_error,"<li> Country should not be empty")
  
  if(!any(grep("^[0-9]{2}[A-Z]{5}[0-9]{4}[A-Z]{1}[1-9A-Z]{1}Z[0-9A-Z]{1}$", input$sup_gstNo)))
    test_error = paste0(test_error,"<li> Invalid GST Number</li>")
  if(!any(grep("[1-9][0-9]{9}$", input$sup_phoneNo)))
    test_error = paste0(test_error,"<li> Enter a valid phone number</li>")
  if(!any(grep("^[1-9][0-9]{5}$", input$sup_addrPin)))
    test_error = paste0(test_error,"<li> Invalid pincode</li>")
  
  if(test_error == "")
    return (T)
  else
    return (paste0("<ul>",test_error,"</ul>"))
}

supplier_verify_button = function(sesion, input, output){
  
  valid_run = validate_supplier(input)
  
  if(valid_run == TRUE){
    hideElement("supplier_verify")
    showElement("supplier_submit")
  } else{
    showModal(modalDialog(title = h3("Errors"), HTML(valid_run)))
  }
  
}

supplier_submit_button = function(session, input, output){
  
  source("modules/utils/dbCon.R")

  sql = paste0("INSERT INTO supplier VALUES (",
               "'", input$sup_name,"',",
               "'",input$sup_addrPin,"',",
               "'",input$sup_addrCity,"',",
               "'",input$sup_addrDist,"',",
               "'",input$sup_addrState,"',",
               "'",input$sup_addrCountry,"',",
               input$sup_phoneNo,",",
               "'",input$sup_emailId,"',",
               "'",input$sup_address1,"',",
               "'",input$sup_address2,"',",
               "'",input$sup_gstNo,"',",
               "'",input$sup_gstType,"'",
               ")")

  tryCatch({
    dbWithTransaction(
      con,
      {
        if(dbExecute(con, sql) == 1)
          shinyalert(title = "Success", text = "Supplier added", type = "success")
        else{
          shinyalert(title = "Failed to add", text = "Error uploading to Database. Try Again", type = "error")
          dbBreak()
        }
      }
    )
  },finally = {
    rm(sql)
    supplier_reset_button(session, input, output)
    dbDisconnect(con)
  })
  
}

loadSupplierTable = function(session, input, output){
  source("modules/utils/dbCon.R")
  data_supplier = dbReadTable(con, "supplier")
  dbDisconnect(con)
  
  dtedit(input, output, "show_supplier", data_supplier,
         show.insert = F,
         show.copy = F,
         callback.delete = supplier_delete_button,
         callback.update = supplier_edit_button)
}

supplier_edit_button = function(data, olddata, row){
  source("modules/utils/dbCon.R")
  sql = sprintf('update supplier set "Name" = \'%s\', "Pincode" = \'%s\', "City" = \'%s\', "District" = \'%s\', "State" = \'%s\', "Country" = \'%s\', "PhoneNo" = \'%s\', "Email"  = \'%s\', "Address1"  = \'%s\', "Address2" = \'%s\', "GstNo" = \'%s\', "GstType" = \'%s\' where "GstNo" = \'%s\'',
          data[row,"Name"], data[row,"Pincode"], data[row, "City"], data[row, "District"], data[row,"State"], data[row,"Country"], data[row, "PhoneNo"], data[row, "Email"],data[row,"Address1"], data[row,"Address2"], data[row, "GstNo"], data[row, "GstType"], olddata[row, "GstNo"])
  dbGetQuery(con, sql)
  rm(sql)
  dbDisconnect(con)
  olddata[row,] = data[row,]
  return (olddata)
}

supplier_delete_button = function(data, row){
  source("modules/utils/dbCon.R")
  dbGetQuery(con, paste0("Delete from supplier where \"Name\" = '",data[row,"Name"],"'"))
  dbDisconnect(con)
  return (data[-row,])
}