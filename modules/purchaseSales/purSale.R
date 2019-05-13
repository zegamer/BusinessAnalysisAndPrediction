source("modules/purchaseSales/purSale_purchaseTab.R")
source("modules/purchaseSales/purSale_saleTab.R")

loadCatalogTable = function(session, input, output){
  source("modules/utils/dbCon.R")
  data_inventory = dbReadTable(con, "inventory")
  dbDisconnect(con)
  
  output$show_inventory <- renderDataTable({
    data_inventory
  })
}

loadPurSaleTable = function(session, input, output){
  source("modules/utils/dbCon.R")
  data_sales = dbReadTable(con, "sales")
  data_purchase = dbReadTable(con, "purchase")
  dbDisconnect(con)
  
  dtedit(input, output, "show_purchase", data_purchase,
         show.update = F,
         show.insert = F,
         show.copy = F,
         callback.delete = purchase_delete_button)
  
  dtedit(input, output, "show_sales", data_sales,
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

  tryCatch({
    if(any(dim(dbGetQuery(con, paste0("SELECT \"ITEM_NAME\" from inventory where \"ITEM_NAME\"=\'",data[row,"ProductName"],  "' and \"HSN\" = '",data[row,"HSN"],"'")))==0)){
      shinyalert(text = "Product not in inventory. You need to purchase it first.", title = "Failed", type = "error")
      return(data)
    }
    
    prev = as.numeric(dbGetQuery(con, paste0('SELECT "QUANTITY" FROM inventory WHERE "ITEM_NAME" = \'',data[row,"ProductName"],"' and \"HSN\" = '",data[row,"HSN"],"'")))
    prev_qty = if(length(prev) == 0) 0 else prev
    updated_qty = prev_qty - as.numeric(if(data[row,"Quantity"] == "") 0 else data[row,"Quantity"])
    
    
    if(updated_qty < 0){
      shinyalert(type = "warning",
                 text = HTML(paste0("<p>Product quantity is insufficient</p>",
                                    "<p>Available: ",prev_qty,"</p>")),
                 title = "Unable to delete",
                 html = T)
      
      return (data)
    }
    else {
      dbWithTransaction(con,
      {
        if(dbExecute(con, paste0("DELETE from purchase WHERE \"InvoiceNo\" = '", data[row,"InvoiceNo"], "' and \"HSN\" = '",data[row,"HSN"],"' and \"ProductName\" = '",data[row,"ProductName"],"'")) != 1){
          shinyalert(title = "Error", text = "Failed to delete in Purchase Table", type = "error")
          dbBreak()
        } else{
          if(dbExecute(con, paste0("UPDATE inventory SET \"QUANTITY\" = '", updated_qty, "' WHERE \"HSN\" = '",data[row,"HSN"],"' and \"ITEM_NAME\" = '",data[row,"ProductName"],"'")) != 1){
            shinyalert(text = "Failed to update quantity in Inventory Table", title = "Error", type = "error")
            dbBreak()
          }
        }
        dbCommit(con)
        dbDisconnect(con)
        return (data[-row,])
      })
    }
  },
  error = function(e){
    shinyalert(title="Error occured", text = paste0(e, "<br/><p>But row Deleted</p>"),type = "error", html = T)  
  },
  finally = dbDisconnect(con))
  
  dbDisconnect(con)
  return (data)
}

sales_delete_button = function(data,row){
  source("modules/utils/dbCon.R")

  tryCatch({
    if(any(dim(dbGetQuery(con, paste0("SELECT \"ITEM_NAME\" from inventory where \"ITEM_NAME\"=\'",data[row,"ProductName"],  "' and \"HSN\" = '",data[row,"HSN"],"'")))==0)){
      shinyalert(text = "Product not in inventory. You need to purchase it first.", title = "Failed", type = "error")
      return(data)
    }
    
    prev = as.numeric(dbGetQuery(con, paste0('SELECT "QUANTITY" FROM inventory WHERE "ITEM_NAME" = \'',data[row,"ProductName"],"' and \"HSN\" = '",data[row,"HSN"],"'")))
    prev_qty = if(length(prev) == 0) 0 else prev
    updated_qty = prev_qty + as.numeric(if(data[row,"Quantity"] == "") 0 else data[row,"Quantity"])
    
    if(updated_qty < 0){
      shinyalert(type = "warning",
                 text = HTML(paste0("<p>Product quantity is insufficient</p>",
                                    "<p>Available: ",prev_qty,"</p>")),
                 title = "Unable to delete",
                 html = T)
      
      return (data)
    }
    else{
      dbWithTransaction(con,
      {
        if(dbExecute(con, paste0("DELETE from sales WHERE \"InvoiceNo\" = '", data[row,"InvoiceNo"], "' and \"HSN\" = '",data[row,"HSN"],"' and \"ProductName\" = '",data[row,"ProductName"],"'")) != 1){
          shinyalert(title = "Error", text = "Failed to delete in Sales Table", type = "error")
          dbBreak()
        } else{
          if(dbExecute(con, paste0("UPDATE inventory SET \"QUANTITY\" = '", updated_qty, "' WHERE \"ITEM_NAME\" = '", data[row,"ProductName"], "' and \"HSN\" = '",data[row,"HSN"],"'")) != 1){
            shinyalert(text = "Failed to update quantity in Inventory Table", title = "Error", type = "error")
            dbBreak()
          }
        }
        dbCommit(con)
        dbDisconnect(con)
        return (data[-row,])
      })
    }
  },
  error = function(e){
    shinyalert(title="Error occured", text = paste0(e, "<br/><p>But row Deleted</p>"),type = "error", html = T)
  },
  finally = dbDisconnect(con))
  return (data)
}