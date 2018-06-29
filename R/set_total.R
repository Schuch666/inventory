#' prepate data for inventoryes
#'
#' @description Organize inventory data
#'
#' @return a sfc with the area shapes and the total emission for each area
#'
#'
#' @param geom sf object describin the geometry of each area
#' @param values vector (for one variable) or list (for multiple variables) of total mass
#' @param variable name(s) of the pollutant(s)
#' @param names area names
#' @param m_unit mass unit, defoult is "t" (ton)
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
#' # for a single pollutant
#' so2_2010 <- set_total(geom     = continents,
#'                       values   = so2$mass,
#'                       names    = so2$region,
#'                       variable = "so2")
#' # for multiples pollutants, NO values is twice for ilustration
#' so2_NO_2010 <- set_total(geom     = continents,
#'                          values   = list(so2$mass,2 * so2$mass),
#'                          names    = so2$region,
#'                          variable = c("so2","NO"))

set_total <- function(geom, values, variable, names = NA, m_unit = "t", verbose = T){
  if(!"sf" %in% class(geom))
    stop("class of geom must to be a sf")
  if(is.na(names[1])){
    if(length(values) != length(names))
      stop("names must have the same length of valuess")
  }
  if(is.list(values)){
    if(nrow(geom) != length(values[[1]]))
      stop("values length must be the same nrow(geom)")
    if(length(values) != length(variable))
      stop("values need to be the same length of variable")
  }else{
    if(nrow(geom) != length(values))
      stop("values length must be the same nrow(geom)")
  }

  if(is.na(names[1])){
    names <- paste("area",1:length(values))
  }
  df    <- data.frame(region   = as.character(names))

  for(i in 1:length(variable)){
    if(verbose)
      cat(paste("calculating",variable[i],"for",nrow(geom),"areas\n"))
    if(is.list(values)){
      values_n <- as.numeric(values[[i]])
    }else{
      values_n <- as.numeric(values)
    }
    u     <- m_unit
    df    <- cbind(df, units::set_units(values_n,"t"))
  }
  df    <- st_sf(df,geometry = sf::st_geometry(geom))

  names(df) <- c("region",variable,"geometry")

  return(df)
}
