
#' @title Merge two data containers
#' @description Note that behavior for merging two idential time intervals is not well defined! I.e. 1day with 1day or 1hour with 1hour, etc...
#' @param a Fist data container
#' @param b Second data container
#' @return A new data container containing the meta data from both containers and their respective time interval data
#' #@example 1hour.and.1day.data <- merge.mqm.data.container(1hour.data, 1day.data)
#' @author Jurian
merge.mqm.data.container <- function(a, b) {
  
  cfg <- config::get(file = "config/config.yml")
  
  if(class(a) != cfg$data.container.main.class | class(b) != cfg$data.container.main.class) {
    stop(paste("One or both arguments not of class", cfg$data.container.main.class))
  }
  
  c <- append(a, b)
  
  class(c) <- cfg$data.container.main.class
  
  return(c)
}

