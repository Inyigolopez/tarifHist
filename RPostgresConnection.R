#install.packages("RPostgreSQL")

require("RPostgreSQL")
require("DBI")
require("odbc")

connect_to_db <- function(){
  driver <- "PostgreSQL" 
  host <- "10.28.8.78" 
  dbname <- "neinordb"
  user <- "neinor"
  pw <- { "neinor" }
  port <- 5432
  dbhandle <- dbConnect(driver, host = host, dbname = dbname, user = user, password = pw, port = port)
  dbhandle
}

connect_to_sqlserver_db <- function(){
  driver <- "SQLServer" 
  server <- "NHSRVBD01.corp.neinorhomes.com" 
  #server <- "10.28.8.69"
  database <- "NH_DWH_DDS"
  uid <- "sa"
  pwd <- "Sal3r0-NH"
  port <- "1433"
  dbhandle <- dbConnect(odbc(), Driver = driver, Server = server, Database = database, UID = uid, PWD = pwd, Port = port)
  dbhandle
}

query_db <- function(query){
  dbhandle <- connect_to_db()
  
  exec_query <- dbGetQuery(dbhandle, query) 

  dbDisconnect(dbhandle)
  exec_query
}

query_sqlserver_db <- function(query){
  dbhandle <- connect_to_sqlserver_db()
  if(dbIsValid(dbhandle))
  {
    exec_query <- dbGetQuery(dbhandle, query) 
  } else {
    print("No se ha podido conectar a la base de datos. Revisa los datos introducidos e intentalo mas tarde.")
  }
  dbDisconnect(dbhandle)
  exec_query
}

read_table <- function(tablename){
  dbhandle <- connect_to_db()

  exec_query <- dbReadTable(dbhandle, tablename) 

  dbDisconnect(dbhandle)
  exec_query
}

read_sqlserver_table <- function(tablename){
  dbhandle <- connect_to_sqlserver_db()
  if(dbIsValid(dbhandle))
  {
    exec_query <- dbReadTable(dbhandle, tablename) 
  } else {
    print("No se ha podido conectar a la base de datos. Revisa los datos introducidos e intentalo mas tarde.")
  }
  dbDisconnect(dbhandle)
  exec_query
}


write_df_as_table <- function(df, tablename, append = T, overwrite = F){
  dbhandle <- connect_to_db()

  dbWriteTable(conn = dbhandle, name = tablename, value = df, row.names = F, overwrite=overwrite, append = append) 

  dbDisconnect(dbhandle)
}

write_df_as_sqlserver_table <- function(df, tablename, append = T, overwrite = F){
  dbhandle <- connect_to_sqlserver_db()
  if(dbIsValid(dbhandle))
  {
    dbWriteTable(conn = dbhandle, name = tablename, value = df, row.names = F, overwrite=overwrite, append = append) 
  } else {
    print("No se ha podido conectar a la base de datos. Revisa los datos introducidos e intentalo mas tarde.")
  }
  dbDisconnect(dbhandle)
}

update_db <- function(query){
  dbhandle <- connect_to_db()

  exec_query <- dbSendQuery(dbhandle, query) 

  dbDisconnect(dbhandle)
  exec_query
}

update_sqlserver_db <- function(query){
  dbhandle <- connect_to_sqlserver_db()
  if(dbIsValid(dbhandle))
  {
    exec_query <- dbSendQuery(dbhandle, query) 
  } else {
    print("No se ha podido conectar a la base de datos. Revisa los datos introducidos e intentalo mas tarde.")
  }
  dbDisconnect(dbhandle)
  exec_query
}























