#' Agroup sequencial emissions for record
#'
#' @description Create a stack of multiple grinded emissions for record in a NetCDF file with multiple times (dates)
#'
#' @param grid list of sf from griding function
#' @param variable name(s) of the pollutant(s)
#' @param verbose display additional information
#'
#' @return a sf with the grid geometry and the emission by cell for multiple times
#'
#' @import sf
#' @import units
#'
#' @export
#'
#' @examples
#' grinded_so2 <- readRDS(paste0(system.file("extdata",package="inventory"),"/grid_so2.Rds"))
#' two_times_grinded_so2 <- stacker(list(a = grinded_so2, b = grinded_so2))

stacker <- function(grid, variable = NA, verbose = TRUE){
  if(is.list(grid)){
    if(!"sf" %in% class(grid[[1]]))
      stop("griding must be a list of griding outputs")  # nocov
  }else{
    stop("griding must be a list")                       # nocov
  }

  stack <- grid[[1]]
  for(i in 2:length(grid)){
    stack <- rbind(stack,grid[[i]])
  }
  if(verbose)
    cat(paste("stack of",i,"emissions"))

  return(stack)
}
