reqsUi = function(){
  
  months_frame = c("May", "June", "July")
  inv_actual = sample(1:50,3)
  inv_predicted = inv_actual + sample(-2:5, 3)
  data1 = data.frame(months_frame, inv_actual, inv_predicted)
  
  data1$months_frame = factor(data1$months_frame, levels = data1[["months_frame"]])
  
  months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  sold <- c(20, 14, 25, 16, 18, 22, 19, 15, 12, 16, 14, 17)
  inInv <- c(19, 14, 22, 14, 16, 19, 15, 14, 10, 12, 12, 16)
  data2 <- data.frame(months, sold, inInv)
  
  data2$months <- factor(data2$months, levels = data2[["months"]])
  
  div(
    h3("Demand Planning"),
    hr(),br(),
    fluidRow(
      column(width = 2,
             selectizeInput("selMonth",
                            label = "Month",
                            choices = months)
             ),
      column(width = 2,
             selectizeInput("selYear",
                            label = "Year",
                            choices = c(seq(2014,2019,1)))
             )
    ),
    hr(),br(),
    fluidRow(
      column(width = 12,
             h3("Previous data"),
             plot_ly(data2, x = ~months, y = ~sold, type = 'bar', name = 'Sales', marker = list(color = 'rgb(49,130,189)')) %>%
               add_trace(y = ~inInv, name = 'Inventory', marker = list(color = 'rgb(204,204,204)')) %>%
               layout(xaxis = list(title = "Month", tickangle = -60),
                      yaxis = list(title = "Values"),
                      margin = list(b = 100),
                      barmode = 'group')
      )
    ),
    fluidRow(
      column(width = 9,
             h3("Future predictions"),
               plot_ly(data1, x = ~months_frame, y = ~inv_actual, type = 'bar', name = 'Actual Inventory') %>%
                 add_trace(y = ~inv_predicted, name = 'Predicted Inventory') %>%
                 layout(xaxis = list(title = "Month"),
                        yaxis = list(title = 'Count'), barmode = 'group')
      )
    )
  )
}