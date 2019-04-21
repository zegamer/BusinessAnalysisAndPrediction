source("modules/purchaseSales/purSale_purchaseTab.R")
source("modules/purchaseSales/purSale_saleTab.R")


loadCatalogTable = function(session, input, output){
  if(!exists("data_inventory")){
    source("modules/utils/dbCon.R")
    data_inventory = dbReadTable(con, "inventory")
    dbDisconnect(con)
  }
  
  output$show_inventory <- renderDataTable({
    data_inventory
  })
  # dtedit(input, output, "show_inventory", is.df.empty(data_inventory),
  #        show.update = F,
  #        show.insert = F,
  #        show.copy = F,
  #        show.delete = F)
}

loadPurSaleTable = function(session, input, output){
  if(!(exists("data_sales")) || !(exists("data_purchase"))){
    source("modules/utils/dbCon.R")
    if(!exists("data_sales"))
      data_sales = dbReadTable(con, "sales")
    if(!exists("data_purchase"))
      data_purchase = dbReadTable(con, "purchase")
    dbDisconnect(con)
  }
  
  dtedit(input, output, "show_purchase", is.df.empty(data_purchase),
         show.update = F,
         show.insert = F,
         show.copy = F,
         callback.delete = purchase_delete_button)
  
  dtedit(input, output, "show_sales", is.df.empty(data_sales),
         show.update = F,
         show.insert = F,
         show.copy = F,
         callback.delete = sales_delete_button)
}

addNewPurchase = function(input, output){
  div(
    h3("New entry"),
    hr(),
    form_purchase()
  )
}

addNewSale = function(input, output){
  div(
    h3("New entry"),
    hr(),
    form_sale()
  )
}

showInventory = function(input, output){
  div(
    h3("Showing all items"),
    hr(),
    dataTableOutput("show_inventory")
    # h4("Fetch optimization needed, inventory takes much memory while calling")
  )
}

showTransactions = function(input, output){
  div(
    h3("Showing transactions"),
    hr(),
    tabsetPanel(id = "Transactions",
                tabPanel("Purchase transactions",br(),
                         uiOutput("show_purchase")),
                tabPanel("Sales transactions",br(),
                         uiOutput("show_sales"))
    )
  )
}

purchase_delete_button = function(data,row){
  source("modules/utils/dbCon.R")
  
  prev = as.numeric(dbGetQuery(con, paste0('SELECT "QUANTITY" FROM inventory WHERE "PRODUCT_ID" = \'',data[row,"ProductID"],"'")))
  
  prev_qty = if(length(prev) == 0) 0 else prev
  
  updated_qty = prev_qty - as.numeric(data[row,"Quantity"])
  
  dbWithTransaction(con,
  {
    if(dbExecute(con, paste0("DELETE from purchase WHERE \"InvoiceNo\" = '", data[row,"InvoiceNo"], "'")) != 1){
      showModal(modalDialog("Failed to delete in Purchase Table", title = "Error", size = "s"))
      dbBreak()
    } else{
      if(dbExecute(con, paste0("UPDATE inventory SET \"QUANTITY\" = '", updated_qty, "' WHERE \"PRODUCT_ID\" = '",data[row,"ProductID"],"'")) != 1){
          showModal(modalDialog("Failed to update quantity in Inventory Table", title = "Error", size = "s"))
          dbBreak()
      }
    }
    dbCommit(con)
    dbDisconnect(con)
    return (data[-row,])
  })
  dbDisconnect(con)
  return (data)
}

sales_delete_button = function(data,row){
  source("modules/utils/dbCon.R")
  dbWithTransaction(con,
  {
    if(dbExecute(con, paste0("DELETE from sales WHERE \"InvoiceNo\" = '", data[row,"InvoiceNo"], "'")) != 1){
      showModal(modalDialog("Failed to delete in Sales Table", title = "Error", size = "s"))
      dbBreak()
    } else{
      if(!any(is.data.frame((prev_qty = as.numeric(dbGetQuery(con, paste0('SELECT "QUANTITY" FROM inventory WHERE "PRODUCT_ID" = \'',data[row,"ProductID"],"'"))))))){
        if(dbExecute(con, paste0("UPDATE inventory SET \"QUANTITY\" = '",prev_qty + as.numeric(data[row,"Quantity"]), "' WHERE \"PRODUCT_ID\" = '",data[row,"ProductID"],"'")) != 1){
          showModal(modalDialog("Failed to update quantity in Inventory Table", title = "Error", size = "s"))
          dbBreak()
        } else {
          showModal(modalDialog("Product not found in Inventory", size = "m", title = "Error"))
          dbBreak()
        }
      }
    }
    return (data[-row,])
  })
  
  dbDisconnect(con)
  # return (data)
  return (data[-row,])
}