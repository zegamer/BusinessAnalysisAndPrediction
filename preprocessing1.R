library(dplyr)
library(readxl)

file_path = "C:/Users/babu2/Downloads/BAaP/"
path_folder = c("17-18 purchase", "18-19 purchase", "17-18 sales", "18-19 sales")

filename1718 = c("january18","february18","march18","july17","august17","september17","october17","november17","december17")
op_filename1718 = c(paste0("18_",c(1:3)), paste0("17_",c(7:12)))
filename1819 = c("january19","february19","march19", "may18", "june18","july18","august18","september18","october18","november18","december18")
op_filename1819 = c(paste0("19_",c(1:3)), paste0("18_",c(5:12)))

pur_oldnames = c("Voucher Number", "Date", "Party Name", "Item Name", "Item HSN", "Item rate of GST", "Billed Quantity", "Rate", "Amount")
pur_newcolnames = c("Invoice Number", "Date", "Supplier", "Item Name", "HSN", "GST Rate", "Quantity", "Rate", "Amount")

sal_oldnames = c("Voucher Number", "Date", "Party Name", "Item Name", "Item HSN", "Item rate of GST", "Billed Quantity", "Rate", "Purchase Rate", "Amount")
sal_newcolnames = c("Invoice Number", "Date", "Customer Name", "Item Name", "HSN", "GST Rate", "Quantity", "Rate", "Purchase Rate", "Amount")

f = function(filename, op_filename, folder, oldnames, newnames, pursale){
  i = 0
  e = simpleError("")
  for(fname in filename){
    i = i + 1
    cat(paste0("START --- ", pursale, op_filename[i]))
    tryCatch({
        df2 = read_xlsx(paste0(file_path, folder,'/',fname, '.xlsx'))
        newdf = df2 %>% select(oldnames)
        names(newdf) = newnames
        write.csv(newdf, file = paste0(file_path, folder,'/New/', pursale, op_filename[i], '.csv'), row.names = F)
        cat("\r --- DONE")
      },
      error = function(e) print(paste0("************ ERROR HERE ****** CHECK MANUAL ***** - ", pursale, op_filename[i]))
    )
  }
}

f(filename1718, op_filename1718, "17-18 purchase", pur_oldnames, pur_newcolnames, "pur_")
f(filename1718, op_filename1718, "17-18 sales", sal_oldnames, sal_newcolnames, "sal_")
f(filename1819, op_filename1819, "18-19 purchase", pur_oldnames, pur_newcolnames, "pur_")
f(filename1819, op_filename1819, "18-19 sales", sal_oldnames, sal_newcolnames, "sal_")