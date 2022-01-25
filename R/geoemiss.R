#' Gather data for calculate the inventory
#'
#' @description create a sf object with the emission and the geographic area which this emission
#' will be distributed.
#'
#' @return a sf with the area shapes and the total emission for each area
#'
#' @param geom sf object describing the geometry of each area
#' @param values numeric vector (for one variable) or list of vectors (for multiple variables) of total mass
#' @param variable name(s) of the pollutant(s)
#' @param names area names
#' @param mass_unit mass unit, default is "t" (ton)
#' @param verbose display additional information
#'
#' @import sf
#' @import units
#'
#' @export
#'
#' @examples
#' continents <- sf::st_read(paste0(system.file("extdata",package="inventory"),"/continent.shp"),
#'                           quiet = TRUE)
#' continents <- continents[1:5,]
#' so2 <- read.csv(paste0(system.file("extdata",package="inventory"),"/SO2.csv"))
#' names(so2) <- c("region","year","mass")
#' so2 <- so2[so2$year == 2010,]
#' # for a single pollutant
#' so2_2010 <- geoemiss(geom     = continents,
#'                      values   = so2$mass,
#'                      names    = so2$region,
#'                      variable = "so2")
#' # for multiples pollutants, NO values is twice so2 emissions for exercice purpose
#' so2_NO_2010 <- geoemiss(geom     = continents,
#'                         values   = list(so2$mass,2 * so2$mass),
#'                         names    = so2$region,
#'                         variable = c("so2","NO"))

geoemiss <- function(geom, values, variable, names = NA, mass_unit = "t", verbose = T){
  if(!"sf" %in% class(geom))                                  # nocov start
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
  }                                                           # nocov end

  if(is.na(names[1])){
    names <- paste("area",1:length(values)) # nocov
  }
  df    <- data.frame(region   = as.character(names))

  for(i in 1:length(variable)){
    if(verbose)
      cat(paste("calculating",variable[i],"for",nrow(geom),"areas\n"))
    if(is.list(values)){
      values_n <- as.numeric(values[[i]])   # nocov
    }else{
      values_n <- as.numeric(values)
    }
    values_n <- units::set_units(values_n,mass_unit,mode = "standard")
    df       <- cbind(df, values_n)
  }
  df    <- st_sf(df,geometry = sf::st_geometry(geom))

  names(df) <- c("region",variable,"geometry")

  return(df)
}
