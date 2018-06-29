#' Distribute emissions between areas and grids
#'
#' @description calculate a inventory
#'
#' @return a sfc with the grid geomatry and the emission by cell
#'
#'
#' @param totals a output from set_total
#' @param variable name(s) of the pollutant(s)
#' @param t_unit time unit, defoult is "year"
#' @param verbose display additional information
#' @param plot true for plot separate regions and final inventory
#' @param ... aditional plot parameters
#'
#' @import sf
#' @import units
#'
#' @export
#'
#' @examples
#' # multiples areas
#' so2_2010   <- readRDS("C:/Users/Schuch/Desktop/test_inventarios/totals_01.Rds")
#' inventory_so2_2010 <- inventory(so2_2010[1:2,], plot = TRUE)
#' # multiple pollutants
#' so2_no_2010   <- readRDS("C:/Users/Schuch/Desktop/test_inventarios/so2_no.Rds")
#' inventory_so2_2010 <- inventory(so2_no_2010[1,], variable = c("so2","NO"), plot = TRUE)
#' # multiple areas and pollutants
#' so2_no_2010   <- readRDS("C:/Users/Schuch/Desktop/test_inventarios/so2_no.Rds")
#' inventory_so2_2010 <- inventory(so2_no_2010[1:3,], variable = c("so2","NO"), plot = TRUE)
#'

inventory <- function(totals, variable = NA, t_unit = NA, verbose = T, plot = F, ...){
  lat                <- c(-90,90)
  lon                <- c(-180,180)
  world              <- sf::st_multipoint(x = matrix(c(lon,lat),2),dim = "XY")
  grid               <- sf::st_make_grid(world, n = c(50,50))
  sf::st_crs(grid)   <- sf::st_crs(totals)
  center             <- sf::st_make_grid(world, n = c(50,50),what = "centers")
  sf::st_crs(center) <- sf::st_crs(totals)

  all_areas <- list()
  if(is.na(variable[1])){
    cat(paste("variable is NA, using:",colnames(totals)[2],"\n"))
    variable <- colnames(totals)[2]
    for(j in 1:nrow(totals)){
      if(verbose)
        cat(paste("processing",as.character(totals$region[j]),"area ...\n"))
      test <- totals[j,]
      test$area <- sf::st_area(test)
      test <- test[-1]
      test <- suppressWarnings( sf::st_interpolate_aw(st_buffer(test,dist = 0.0001),
                                                      grid,extensive = T) )
      test$fraction <- units::drop_units(test$area / sum(test$area))

      names(test) <- c("Id",names(test)[-1])

      outro <- sf::st_sf(Id = 1:length(grid), value = 0, geometry = sf::st_geometry(grid))
      for(i in 1:dim(test)[1]){
        outro$value[test$Id[i]] <- test$fraction[i]
      }
      total <- sf::st_set_geometry(totals[j,2], NULL)[[1]]
      period <- 1 * units::as_units("year")
      taxa   <- total / period
      outro$value  <- outro$value * taxa
      if(plot)
        graphics::plot(outro["value"],axes = T, pal = sf.colors, ...)
      all_areas[[j]] <- outro
    }

    soma       <- outro
    soma$value <- soma$value * 0.0
    for(j in 1:nrow(totals))
      soma$value <- soma$value + all_areas[[j]]$value
    if(plot & j > 1)
      graphics::plot(soma["value"],axes = T, pal = sf.colors, ...)
  }else{
    for(j in 1:nrow(totals)){
      if(verbose)
        cat(paste("processing",as.character(totals$region[j]),"area ...\n"))
      test <- totals[j,]
      test$area <- sf::st_area(test)
      test <- test[-1]
      test <- suppressWarnings( sf::st_interpolate_aw(st_buffer(test,dist = 0.0001),
                                                      grid,extensive = T) )
      test$fraction <- units::drop_units(test$area / sum(test$area))

      names(test) <- c("Id",names(test)[-1])

      outro <- sf::st_sf(Id = 1:length(grid), value = 0, geometry = sf::st_geometry(grid))
      for(i in 1:dim(test)[1]){
        outro$value[test$Id[i]] <- test$fraction[i]
      }

      if(length(variable) == 1){
        total <- sf::st_set_geometry(totals[j,variable], NULL)[[1]]

        period <- 1 * units::as_units("year")
        taxa   <- total / period
        outro$value  <- outro$value * taxa
        if(plot)
          graphics::plot(outro["value"],axes = T, pal = sf.colors, ...)
        all_areas[[j]] <- outro
      }else{
        for(k in 1:length(variable)){
          variable_k <- variable[k]
          total      <- sf::st_set_geometry(totals[j,variable_k], NULL)[[1]]

          period <- 1 * units::as_units("year")
          taxa   <- total / period
          outro  <- cbind(outro,outro$value * taxa)
          names(outro)[k+2] <- variable_k
          if(plot)
            graphics::plot(outro[2+k],axes = T, pal = sf.colors, ...)
        }
        all_areas[[j]] <- outro
      }
    }
    soma       <- outro
    soma[,1:k + 2] = sf::st_set_geometry(soma[,1:k + 2],NULL) * 0.0
    for(j in 1:nrow(totals)){
       soma[,1:k + 2] <- sf::st_set_geometry(soma[,1:k + 2],NULL) +
                         sf::st_set_geometry(all_areas[[j]][,1:k + 2],NULL)
    }
    soma <- soma[,c(-1,-2)]
    if(plot){
      for(k in 1:length(variable)){
        graphics::plot(soma[,k],axes = T, pal = sf.colors, ...)
      }
    }
    return(soma)
  }

  names(soma) <- c("Id",variable,"geometry")
  return(soma[,-1])
}
