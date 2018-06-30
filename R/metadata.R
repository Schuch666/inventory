#' Read and write metadata
#'
#' @description Read, write and erase metadata information of a NetCDF inventoty file
#'
#' @param filename file name
#' @param variable variable name, 0 to global and "?" to ask all names
#' @param attname area names
#' @param action Read, write or erase (NA for get all attnames)
#' @param verbose display additional information
#'
#' @import ncdf4
#' @importFrom utils choose.files
#'
#' @export
#'
#' @examples
#'

metadata <- function(filename = NA,variable = 0, attname = NA,action = "read",verbose = F){
  if(is.na(filename)){
    cat("choose a file:\n")
      filename <- utils::choose.files()
    cat(paste(filename,"\n"))
  }

  meta <- ncdf4::nc_open(filename = filename, verbose = verbose)

  if(variable == "?"){
    ncdf4::nc_close(meta)
    return(cat(names(meta$var)))
  }

  if(action == "read"){
    if(is.na(attname)){
      ATR <- ncatt_get(meta,variable,attname,verbose=verbose )
      if(variable ==0)
        cat("global attributes:\n")
      else
        cat(paste0("variable ",variable,"attritutes:\n"))
      cat(paste(names(ATR),sep = ","))
    }else{
      ATR <- ncatt_get(meta,variable,attname,verbose=verbose )
      if(variable == 0)
        variable <- "global"
      cat(paste0(variable," attribute ",attname,":\n"))
      cat(ATR$value)
    }
  }

  if(action == "write"){
    cat("fazer!")
  }

  if(action == "erase"){
    cat("fazer!")
  }
  ncdf4::nc_close(meta)
}
