library(RMySQL)

setup.db <- function() {
  
  cfg <- config::get(file = "config/config.yml")
  
  dbConnect(RMySQL::MySQL(), 
            dbname = cfg$dbname, 
            username = cfg$username,
            password = cfg$password, 
            host = cfg$host, 
            port = cfg$port)
}

query.hourly <- function(db) {
  
  # This is just a test example for now
  query <- sprintf("SELECT * FROM %t", "types")
  
  ref <- dbEscapeStrings(db, query)
  data = fetch(ref)
  
}