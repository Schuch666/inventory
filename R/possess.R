#' Read a georeference image and extract values for using in griding
#'
#' @description extract values from a image to be used on each region
#'
#' @param geoemiss a output from geoemiss
#' @param filename image filename
#' @param plots plot individual regions
#' @param verbose display additional information
#'
#' @import sf
#' @import raster
#'
#' @export
#'
#' @examples
#' so2_no_2010 <- readRDS(paste0(system.file("extdata",package="inventory"),"/so2_no.Rds"))
#' file <- paste(system.file("extdata", package = "EmissV"),"/dmsp.tiff",sep="")
#' # a_raster <- possess(so2_no_2010,file)
#'

possess <- function(geoemiss, filename = NA, plots = T,verbose = T){
  if(is.na(filename)){
    cat("choose a file:\n")
    filename <- utils::choose.files()
    cat(paste(filename,"\n"))
  }

  r  <- crop(raster::raster(filename),sf::as_Spatial(geoemiss$geometry))
  s  <- sf::as_Spatial(geoemiss$geometry)
  l  <- list()
  geoemiss$image <- NULL

  for(j in 1:nrow(geoemiss)){
    cro <- raster::mask(r,s[j])
    cro <- crop(cro,s[j])
    if(plots)
      plot(cro,main =  paste(geoemiss$region[j]))
    l[[j]] <- cro
  }

  geoemiss$image <- l
  return(geoemiss)
}
