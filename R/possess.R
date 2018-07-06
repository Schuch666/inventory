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
#' states <- sf::st_read(paste(system.file("extdata", package = "inventory"),"/states.shp",sep=""),
#'                       quiet = TRUE)
#' Nox    <- geoemiss(geom = states,variable = "Nox",names = c("sp","rj"),values = c(1000,200))
#' image  <- paste(system.file("extdata", package = "inventory"),"/tiny.tif",sep="")
#' ras    <- possess(Nox,image,plots = FALSE)
#' test   <- griding(ras,variable = "Nox", res = 0.1, type = "local", plot = TRUE)


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
