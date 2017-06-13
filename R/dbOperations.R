library(RMySQL)

settings <- list(
  dbname = "mqm_db",
  username = "MQMuser",
  password = "NA",
  host = "145.23.219.9",
  port = 3306
)

setup.db <- function(settings) {
  dbConnect(RMySQL::MySQL(), 
            dbname = settings$dbname, 
            username = settings$username,
            password = settings$password, 
            host = settings$host, 
            port = settings$port)
}


query.hourly <- function(db) {
  
  # This is just a test example for now
  query <- sprintf("SELECT * FROM %t", "types")
  
  ref <- dbEscapeStrings(db, query)
  data = fetch(ref)
  
}