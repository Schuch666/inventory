#' Read and write metadata
#'
#' @description Read and write metadata information of a NetCDF inventoty file
#'
#' @param filename file name
#' @param variable variable name, 0 to global and "?" to ask all names
#' @param attname attribute names
#' @param action Read or write attribute (NA for get all attnames)
#' @param value value to write
#' @param verbose display additional information
#'
#' @import ncdf4
#' @importFrom utils choose.files
#'
#' @export
#'
#' @examples
#' nc <- paste0(system.file("extdata",package="inventory"),"/small.nc")
#' metadata(nc)
#' metadata(nc,attname = "Title")
#' metadata(nc,variable = "?")
#' metadata(nc,variable = "so2")
#' metadata(nc,variable = "so2",attname = "long_name")
#'

metadata <- function(filename = NA,variable = 0, attname = NA, action="read", value=NA, verbose=F){
  if(is.na(filename)){
    cat("choose a file:\n")
      filename <- utils::choose.files()
    cat(paste(filename,"\n"))
  }
  if(action != "read")
    to_write <- T
  else
    to_write <- F

  meta <- ncdf4::nc_open(filename = filename, verbose = verbose, write = to_write)

  if(variable == "?"){
    ncdf4::nc_close(meta)
    return(cat(names(meta$var)))
  }

  if(action == "read"){
    if(is.na(attname)){
      ATR <- ncdf4::ncatt_get(meta,variable,attname,verbose=verbose)
      if(variable ==0)
        cat("global attributes:\n")
      else
        cat(paste0("variable ",variable," attritutes:\n"))
      cat(paste(names(ATR),sep = ","))
    }else{
      ATR <- ncdf4::ncatt_get(meta,variable,attname,verbose=verbose)
      if(variable == 0)
        variable <- "global"
      cat(paste0(variable," attribute ",attname,":\n"))
      cat(ATR$value)
    }
  }

  if(action == "write"){
    if(is.na(value))
      stop("nothing to write")
    cat(paste("writing",value,"on attribute",attname,"of",variable,"at file",filename))
    ncdf4::ncatt_put(meta,varid = variable,attname = attname,attval = value,verbose = verbose)
  }

  ncdf4::nc_close(meta)
}
