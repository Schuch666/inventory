#' Distribute emissions into grids
#'
#' @description calculate the inventory from a geoemiss ouput, there is 3 grid types:
#' - global: cover all latitudes and longitudes
#' - local: cover the latitudes and longitudes of the input data
#' - custom: with custom limits
#'
#' res argument controls the resolution of the final grid, grids whith higher resolution take more
#' time to calculate
#'
#' @note inventories with multiple dates need to be combined with stacker function
#'
#' @return a sfc with the grid geometry and the emission by cell
#'
#' @param geoemiss a output from geoemiss
#' @param variable name(s) of the pollutant(s)
#' @param area_unit area unit, default is "km^2"
#' @param res inventory resolution in degrees
#' @param type global, local or custom inventary type
#' @param lat latitude for custom inventory
#' @param lon lontitude for custom inventory
#' @param tol param passing to dist of sf::st_buffer
#' @param verbose display additional information
#' @param plot true for plot separate regions and the final inventory
#' @param ... additional plot parameters
#'
#' @import sf
#' @import units
#' @import raster
#'
#' @export
#'
#' @examples
#' so2_no_2010 <- readRDS(paste0(system.file("extdata",package="inventory"),"/so2_no.Rds"))
#' inventory_so2_2010 <- griding(so2_no_2010[5,], variable = c("so2","NO"), plot = TRUE)
#'

griding <- function(geoemiss, variable = NA, area_unit = "km^2", res = 5,
                    type = "global", lat = c(-90,90), lon = c(-180,180),
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
  if(type == "custom"){                                  # nocov start
    if(length(lat) != 2)
      stop("invalid lat")
    if(length(lon) != 2)
      stop("invalid lon")
    n_lat              <- as.integer((max(lat) - min(lat))/ res)
    n_lon              <- as.integer((max(lon) - min(lon))/ res)
    world              <- sf::st_multipoint(x = matrix(c(lon,lat),2),dim = "XY")
    grid               <- sf::st_make_grid(world, n = c(n_lon,n_lat))
    sf::st_crs(grid)   <- sf::st_crs(geoemiss)
  }                                                      # nocov end

  all_areas <- list()
  if(is.na(variable[1])){
    cat(paste("variable is NA, using:",colnames(geoemiss)[2],"\n"))
    variable <- colnames(geoemiss)[2]
    for(j in 1:nrow(geoemiss)){
      if(verbose)
        cat(paste("processing",as.character(geoemiss$region[j]),"area ...\n"))
      if("image" %in% names(geoemiss)){
        test <- geoemiss[j,-ncol(geoemiss)] # nocov
      }else{
        test <- geoemiss[j,]
      }

      test$area <- sf::st_area(test)
      test <- test[-1]

      area_total <- set_units(test$area,area_unit,mode = "standard")
      unidades <- rep(NA,ncol(test))
      for(i in 1:length(unidades)){
        a <- test[1,i,drop = T]
        if(!("sfc_MULTIPOLYGON" %in% class(a))){
          unidades[i] <- deparse_unit(a)
        }
      }
      unidades <- unidades[!is.na(unidades)]

      test <- suppressWarnings( sf::st_interpolate_aw(st_buffer(test,dist = tol),
                                                      grid,extensive = T) )

      for(i in 1:length(unidades)){
        a <- test[,i+1, drop = T]
        units(a) <- NULL
        units(a) <- unidades[i]
        test[,i+1] <- a
      }

      test$area <- set_units(test$area,area_unit,mode = "standard")
      test$fraction <- units::drop_units(test$area / sum(test$area))

      names(test) <- c("Id",names(test)[-1])

      outro <- sf::st_sf(Id = 1:length(grid), value = 0, geometry = sf::st_geometry(grid))
      for(i in 1:dim(test)[1]){
        outro$value[test$Id[i]] <- test$fraction[i]
      }

      if("image" %in% names(geoemiss)){                                      # nocov start
        box   <- raster::raster(nrows=n_lat,ncols=n_lon,
                                xmn=lon[1],xmx=lon[2],ymn=lat[1],ymx=lat[2])
        crs(box) <- raster::crs(sf::as_Spatial(geoemiss$geometry))
        total_box <- raster::cellStats(geoemiss$image[[j]],"sum",na.rm=TRUE)

        X    <- raster::resample(geoemiss$image[[j]],box,method = "bilinear")
        X    <- raster::flip(X,2)
        X    <- raster::t(X)
        X    <- raster::as.matrix(X)
        X[is.na(X)] <- 0
        X    <- X * total_box/sum(X)

        outro$value = outro$value * c(X)                                     # nocov end
      }

      total <- sf::st_set_geometry(geoemiss[j,2], NULL)[[1]]
      taxa   <- total / area_total
      outro$value  <- outro$value * taxa
      if(plot)
        graphics::plot(outro["value"],axes = T, pal = sf.colors, ...) # nocov
      all_areas[[j]] <- outro
    }

    soma       <- outro
    soma$value <- soma$value * 0.0
    for(j in 1:nrow(geoemiss))
      soma$value <- soma$value + all_areas[[j]]$value
    if(plot & j > 1)
      graphics::plot(soma["value"],axes = T, pal = sf.colors, ...) # nocov
  }else{
    for(j in 1:nrow(geoemiss)){
      if(verbose)
        cat(paste("processing",as.character(geoemiss$region[j]),"area ...\n"))
      if("image" %in% names(geoemiss)){
        test <- geoemiss[j,-ncol(geoemiss)]
      }else{
        test <- geoemiss[j,]
      }
      test$area <- sf::st_area(test)
      test <- test[-1]

      area_total <- set_units(test$area,area_unit,mode = "standard")
      unidades <- rep(NA,ncol(test))
      for(i in 1:length(unidades)){
        a <- test[1,i,drop = T]
        if(!("sfc_MULTIPOLYGON" %in% class(a))){
          unidades[i] <- deparse_unit(a)
        }
      }
      unidades <- unidades[!is.na(unidades)]

      test <- suppressWarnings( sf::st_interpolate_aw(st_buffer(test,dist = tol),
                                                      grid,extensive = T) )

      for(i in 1:length(unidades)){
        a <- test[,i+1, drop = T]
        units(a) <- NULL
        units(a) <- unidades[i]
        test[,i+1] <- a
      }

      test$area <- set_units(test$area,area_unit,mode = "standard")
      test$fraction <- units::drop_units(test$area / sum(test$area))

      names(test) <- c("Id",names(test)[-1])

      outro <- sf::st_sf(Id = 1:length(grid), value = 0, geometry = sf::st_geometry(grid))
      for(i in 1:dim(test)[1]){
        outro$value[test$Id[i]] <- test$fraction[i]
      }

      if("image" %in% names(geoemiss)){
        box   <- raster::raster(nrows=n_lat,ncols=n_lon,
                                xmn=lon[1],xmx=lon[2],ymn=lat[1],ymx=lat[2])
        crs(box) <- raster::crs(sf::as_Spatial(geoemiss$geometry))
        total_box <- raster::cellStats(geoemiss$image[[j]],"sum",na.rm=TRUE)

        X    <- raster::resample(geoemiss$image[[j]],box,method = "bilinear")
        X    <- raster::flip(X,2)
        X    <- raster::t(X)
        X    <- raster::as.matrix(X)
        X[is.na(X)] <- 0
        X    <- X * total_box/sum(X)

        outro$value = outro$value * c(X)
      }

      if(length(variable) == 1){
        total <- sf::st_set_geometry(geoemiss[j,variable], NULL)[[1]]
        taxa   <- total / area_total
        outro$value  <- outro$value * taxa
        outro  <- cbind(outro,outro$value)
        k <- 1
        names(outro)[k+2] <- variable
        if(plot)
          graphics::plot(outro["value"],axes = T, pal = sf.colors, ...) # nocov
        all_areas[[j]] <- outro
      }else{
        for(k in 1:length(variable)){
          variable_k <- variable[k]
          total      <- sf::st_set_geometry(geoemiss[j,variable_k], NULL)[[1]]
          taxa   <- total / area_total
          outro  <- cbind(outro,outro$value * taxa)
          names(outro)[k+2] <- variable_k
          if(plot)
            graphics::plot(outro[2+k],axes = T, pal = sf.colors, ...) # nocov
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
    if("image" %in% names(geoemiss)){
      if(plot){                                                   # nocov
        for(k in 1:length(variable)){                             # nocov
          graphics::plot(soma[,k],axes = T, pal = sf.colors, ...) # nocov
        }
      }
    }
    return(soma)
  }
  names(soma) <- c("Id",variable,"geometry")
  return(soma[,-1])
}
