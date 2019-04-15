library(RPostgreSQL)

pw = {"madhsaya#$%&"}
  
con = dbConnect(dbDriver("PostgreSQL"),
                  dbname = "madhsaya",
                  host = "dbbaap.cedpulxkmcte.ap-south-1.rds.amazonaws.com",
                  port = 5432,
                  user = "madhsaya",
                  password = pw
  )
  
rm(pw)