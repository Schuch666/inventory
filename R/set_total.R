#' prepate data for inventoryes
#'
#' @description Organize inventory data
#'
#' @return a sfc with the names, total of pollutants and geometry to make the inventory
#'
#'
#' @param geom geo
#' @param variable list
#' @param names names
#' @param unit unit, defoult is ton by year "t/year"
#' @param verbose display additional information
#'
#' @import sf
#' @import units
#'
#' @export
#'
#' @examples
#' continents <- sf::read_sf(paste0(system.file("extdata",package="inventory"),"/continent.shp"))
#' continents <- continents[1:5,]
#' so2 <- read.csv(paste0(system.file("extdata",package="inventory"),"/SO2.csv"))
#' names(so2) <- c("region","year","mass")
#' so2 <- so2[so2$year == 2010,]
#' totals <- set_total(geom     = continents,
#'                     variable = so2$mass,
#'                     names    = so2$region,
#'                     var_name = "so2")

set_total <- function(geom, variable,var_name = NA, names =NA, unit = NA, verbose = T){
  if(!"sf" %in% class(geom))
    stop("class of geom must to be a sf")
  if(nrow(geom) != length(variable))
    stop("variable length must be the same nrow(geom)")
  if(is.na(names[1])){
    if(length(variable) != length(names))
      stop("names must have the same length of variables")
  }

  variable <- as.numeric(variable)
  if(is.na(names[1])){
    names <- paste("area",1:length(variable))
  }
  else{
    names <- as.character(names)
  }

  df <- st_sf(region   = names,
              var      = set_units(variable,"t/year"),
              geometry = sf::st_geometry(geom))
  if(!is.na(var_name))
    names(df) <- c("region",var_name,"geometry")

  return(df)
}
