#' Distribute emissions into grids
#'
#' @description calculate the inventory from a geoemiss ouput, there is 3 grid types:
#' - global: cover all latitudes and longitudes
#' - local: cover the latitudes and longitudes of the input data
#' - custom: with custom limits
#'
#' res argument contol the resolution of the final grid, grids whith hight resolution take more
#' time to calculate
#'
#' @note inventorys with multiple dates need to be combined with rbind function
#'
#' @return a sfc with the grid geomatry and the emission by cell
#'
#' @param geoemiss a output from geoemiss
#' @param variable name(s) of the pollutant(s)
#' @param t_unit time unit, defoult is "year"
#' @param res inventary resolution in degrees
#' @param type global, local or custon inventary type
#' @param lat latitude for custon inventory
#' @param lon lontitude for custon inventory
#' @param tol param passing to dist of sf::st_buffer
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
#' so2_no_2010 <- readRDS(paste0(system.file("extdata",package="inventory"),"/so2_no.Rds"))
#' # inventory for multiples areas
#' inventory_so2_2010 <- griding(so2_no_2010[1:2,], plot = TRUE)
#' # inventory for a single area and multiple pollutants
#' inventory_so2_2010 <- griding(so2_no_2010[1,], variable = c("so2","NO"), plot = TRUE)
#'

griding <- function(geoemiss, variable = NA,  t_unit = NA,
                    res = 5, type = "global", lat = c(-90,90), lon = c(-180,180),
                    tol = res * 0.00001, verbose = T, plot = F, ...){

  if(type == "global"){
    lat                <- c(-90,90)
    lon                <- c(-180,180)
    n_lat              <- as.integer((max(lat) - min(lat))/ res)
    n_lon              <- as.integer((max(lon) - min(lon))/ res)
    world              <- sf::st_multipoint(x = matrix(c(lon,lat),2),dim = "XY")
    grid               <- sf::st_make_grid(world, n = c(n_lon,n_lat))
    sf::st_crs(grid)   <- sf::st_crs(geoemiss)
  }
  if(type == "local"){
    box                <- sf::st_bbox(geoemiss)
    lat                <- c(box[[2]],box[[4]])
    lon                <- c(box[[1]],box[[3]])
    n_lat              <- as.integer((max(lat) - min(lat))/ res)
    n_lon              <- as.integer((max(lon) - min(lon))/ res)
    grid               <- sf::st_make_grid(geoemiss, n = c(n_lon,n_lat))
    sf::st_crs(grid)   <- sf::st_crs(geoemiss)
  }
  if(type == "custom"){
    if(length(lat) != 2)
      stop("invalid lat")
    if(length(lon) != 2)
      stop("invalid lon")
    n_lat              <- as.integer((max(lat) - min(lat))/ res)
    n_lon              <- as.integer((max(lon) - min(lon))/ res)
    world              <- sf::st_multipoint(x = matrix(c(lon,lat),2),dim = "XY")
    grid               <- sf::st_make_grid(world, n = c(n_lon,n_lat))
    sf::st_crs(grid)   <- sf::st_crs(geoemiss)
  }

  all_areas <- list()
  if(is.na(variable[1])){
    cat(paste("variable is NA, using:",colnames(geoemiss)[2],"\n"))
    variable <- colnames(geoemiss)[2]
    for(j in 1:nrow(geoemiss)){
      if(verbose)
        cat(paste("processing",as.character(geoemiss$region[j]),"area ...\n"))
      test <- geoemiss[j,]
      test$area <- sf::st_area(test)
      test <- test[-1]
      test <- suppressWarnings( sf::st_interpolate_aw(st_buffer(test,dist = tol),
                                                      grid,extensive = T) )
      test$fraction <- units::drop_units(test$area / sum(test$area))

      names(test) <- c("Id",names(test)[-1])

      outro <- sf::st_sf(Id = 1:length(grid), value = 0, geometry = sf::st_geometry(grid))
      for(i in 1:dim(test)[1]){
        outro$value[test$Id[i]] <- test$fraction[i]
      }
      total <- sf::st_set_geometry(geoemiss[j,2], NULL)[[1]]
      period <- 1 * units::as_units("year")
      taxa   <- total / period
      outro$value  <- outro$value * taxa
      if(plot)
        graphics::plot(outro["value"],axes = T, pal = sf.colors, ...)
      all_areas[[j]] <- outro
    }

    soma       <- outro
    soma$value <- soma$value * 0.0
    for(j in 1:nrow(geoemiss))
      soma$value <- soma$value + all_areas[[j]]$value
    if(plot & j > 1)
      graphics::plot(soma["value"],axes = T, pal = sf.colors, ...)
  }else{
    for(j in 1:nrow(geoemiss)){
      if(verbose)
        cat(paste("processing",as.character(geoemiss$region[j]),"area ...\n"))
      test <- geoemiss[j,]
      test$area <- sf::st_area(test)
      test <- test[-1]
      test <- suppressWarnings( sf::st_interpolate_aw(st_buffer(test,dist = tol),
                                                      grid,extensive = T) )
      test$fraction <- units::drop_units(test$area / sum(test$area))

      names(test) <- c("Id",names(test)[-1])

      outro <- sf::st_sf(Id = 1:length(grid), value = 0, geometry = sf::st_geometry(grid))
      for(i in 1:dim(test)[1]){
        outro$value[test$Id[i]] <- test$fraction[i]
      }

      if(length(variable) == 1){
        total <- sf::st_set_geometry(geoemiss[j,variable], NULL)[[1]]

        period <- 1 * units::as_units("year")
        taxa   <- total / period
        outro  <- cbind(outro,outro$value * taxa)
        names(outro)[k+2] <- variable
        if(plot)
          graphics::plot(outro["value"],axes = T, pal = sf.colors, ...)
        all_areas[[j]] <- outro
        k <- 1
      }else{
        for(k in 1:length(variable)){
          variable_k <- variable[k]
          total      <- sf::st_set_geometry(geoemiss[j,variable_k], NULL)[[1]]

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
    for(j in 1:nrow(geoemiss)){
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
