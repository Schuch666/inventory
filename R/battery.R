#' Agrup sequencial emissions for record
#'
#' @description create stack of grinded emissions for record in a NetCDF file with multiple dates
#'
#' @param grid list of ouputs from griding funcion
#' @param variable name(s) of the pollutant(s)
#' @param verbose display additional information
#'
#' @import sf
#' @import units
#'
#' @export
#'
#' @examples
#' grinded_so2 <- sf::read_sf(paste0(system.file("extdata",package="inventory"),"/grid_so2.gpkg"))
#' two_times_grinded_so2 <- battery(list(a = grinded_so2, b = grinded_so2))

battery <- function(grid, varible = NA, verbose = TRUE){
  if(is.list(grid)){
    if(!"sf" %in% class(grid[[1]]))
      stop("griding must be a list of griding outputs")
  }else{
    stop("griding must be a list")
  }

  stack <- grid[[1]]
  for(i in 2:length(grid)){
    stack <- rbind(stack,grid[[i]])
  }
  if(verbose)
    cat(paste("battery of",i,"emissions"))

  return(stack)
}
