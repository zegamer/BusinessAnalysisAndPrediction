eoqUi = function(input){
  
  div(
    h3("Calculate Economic Order Quantity"),
    hr(),
    numericInput("demand", label = "Demand in units", value = 0),
    numericInput("order", label = "Order cost" , value = 0),
    numericInput("holding", label = "Holding cost", value = 0),
    actionButton("eoq_calc", "Calculate", class = "btn btn-primary"),
    hr(),
    br(),
    tags$label("EOQ result: ", `for` = "eoqUnits"),
    textOutput("eoqUnits", inline = T),
    br()
  )
}

calculateEOQ = function(session, input, output){
  D = as.numeric(input$demand)
  S = as.numeric(input$order)
  H = as.numeric(input$holding)
  
  eoq = round(sqrt(2*D*S/H), digits = 2)
  
  output$eoqUnits = renderText(eoq)
}
