#' Save the inventory in NetCDF format
#'
#' @description Create a NetCDF file from a 'inventoty' output and can add new variables to
#'
#' @param gi a output from inventory of battery (multiple times)
#' @param variable name(s) of the pollutant(s) to write
#' @param filename name of the file
#' @param dates date(s) for the data
#' @param unit inventory unit
#' @param mw molecular weight
#' @param time_unit time unit, defoult is "year"
#' @param force_ncdf4 force NetCDF4 format
#' @param COMPRESS compression level from 1 to 9, or NA for no compression
#' @param verbose display additional information
#'
#' @import sf
#' @import units
#' @import ncdf4
#'
#' @export
#'
#' @examples
#' grinded_so2 <- readRDS(paste0(system.file("extdata",package="inventory"),"/grid_so2.Rds"))
#' dir.create(file.path(tempdir(), "INV"))
#' saveInventory(grinded_so2,filename = paste0(file.path(tempdir(), "INV"),"test.nc"),
#'                 variable = "so2", dates = '2010-01-01')

saveInventory <- function(gi,filename = NA,dates,variable,unit = NA,mw = 1, time_unit = "year",
                            COMPRESS = NA, force_ncdf4 = F, verbose = T){

  box    <- sf::st_bbox(gi)
  lat    <- c(box[[2]],box[[4]])
  lon    <- c(box[[1]],box[[3]])
  res    <- (dim(gi)[1]/length(dates)) / (abs(lat[1]-lat[2])* abs(lon[1]-lon[2]))
  res    <- 1 / res^(1/2)
  n_lat  <- as.integer((max(lat) - min(lat))/ res)
  n_lon  <- as.integer((max(lon) - min(lon))/ res)
  # center             <- sf::st_make_grid(gi, n = c(n_lon-1,n_lat-1),what = "centers")
  # sf::st_crs(center) <- sf::st_crs(gi)
  # for(i in 1:((n_lat-1) * (n_lon-1))){
  #   center_lon[i] <- as.numeric(center[[i]])[1]
  #   center_lat[i] <- as.numeric(center[[i]])[2]
  # }
  # Vlon <- unique(center_lon)
  # Vlat <- unique(center_lat)

  d1 = as.Date(dates)
  d2 = as.Date('1850-01-01')
  n_datas = as.numeric(d1-d2)

  period <- 1
  units(period) <- set_units(period,time_unit,mode = "standard")
  gi[,1:(length(gi)-1)] <- gi[,1:(length(gi)-1),drop = T] / period

  if(is.na(filename)){
    cat("file name ([enter] to choose a file):")
    filename <- readline()
    if(filename == "")
      filename <- choose.files()
  }
  if(file.exists(filename)){
    cat(paste("open",filename,"\n"))
  }else{
    cat(paste("creating",filename,"\n"))

    zeros   <- array(rep(0,length(n_datas) * abs(lon[1]-lon[2]) * abs(lat[1]-lat[2]) / res^2),
                     c(n_lon,n_lat,length(n_datas)))

    Vlat <- seq(min(lat) + res/2, max(lat) - res/2, length.out = n_lat)
    Vlon <- seq(min(lon) + res/2, max(lon) - res/2, length.out = n_lon)

    times   <- ncdf4::ncdim_def("time",
                                longname = "time",
                                units = "days since 1850-01-01 00:00",
                                calendar = "gregorian",
                                vals = 1:length(dates),
                                unlim=TRUE)

    lat <- ncdf4::ncdim_def("lat",
                            units = "degrees_north",
                            longname = "latitude",
                            vals = 1:(180/res))

    lon <- ncdf4::ncdim_def("lon",
                            units = "degrees_east",
                            longname = "longitude",
                            vals = 1:(360/res))

    g_atributos  <- c( list(Title       = "Emissions",
                            Author      = Sys.getenv("USERNAME"),
                            Creation    = paste("created on",format(Sys.time(),"%Y-%m-%d at %H:%M"),
                                                "using the R-package inventory",
                                                utils::packageVersion("inventory")),
                            Description = "add description",
                            Reference   = "add reference",
                            institution = "add institution",
                            projection  = "latlon",
                            Conventions = "CF-1.0",
                            source      = "no source provided"
    ) )

    Time <- ncdf4::ncvar_def(name = "date",
                             dim = list(times),
                             units = "days since 1850-01-01 00:00",
                             prec = "integer",
                             compression = COMPRESS)
    # from Macccity
    # Time <- ncdf4::ncvar_def(name = "date",
    #                          units = "",
    #                          dim = list(times),
    #                          prec = "integer",
    #                          compression = COMPRESS)

    # WRF style
    # XLONG <- ncdf4::ncvar_def(name = "XLONG",
    #                           units = "",
    #                           dim = list(lon,lat),
    #                           prec = "float",
    #                           compression = COMPRESS)
    # XLAT <- ncdf4::ncvar_def(name = "XLAT" ,
    #                          units = "",
    #                          dim = list(lon,lat),
    #                          prec = "float",
    #                          compression = COMPRESS)

    for(i in 1:length(variable)){
      assign(variable[i],
             ncdf4::ncvar_def(name          = variable[i],
                              longname      = paste(variable[i],"emissions"),
                              dim           = list(lon,lat,times),
                              units         = deparse_unit(gi[,i,drop=T]), # "kg m-2 s-1"
                              prec          = "float",
                              compression   = COMPRESS))
    }

    inv_file <- ncdf4::nc_create(filename = filename,
                                 vars = c(list('date' = Time),
                                          mget(variable)),
                                 force_v4 = force_ncdf4)

    for(i in 1:length(g_atributos)){
      ncdf4::ncatt_put(inv_file,
                       varid = 0,
                       attname = names(g_atributos)[i],
                       attval = g_atributos[[i]])
    }

    ncdf4::ncvar_put(inv_file,"date", n_datas)
    ncdf4::ncatt_put(inv_file,
                     varid = "time",
                     attname = "standard_name",
                     attval = "time")
    # from Macccity
    # ncdf4::ncatt_put(inv_file,
    #                  varid = "date",
    #                  attname = "long_name",
    #                  attval = "Date as integer : year*10000 + month*100 + day")
    # ncdf4::ncatt_put(inv_file,
    #                  varid = "date",
    #                  attname = "fortmat",
    #                  attval = "YYYYMMDD")

    ncdf4::ncvar_put(inv_file,"lat",Vlat)
    ncdf4::ncatt_put(inv_file,
                     varid = "lat",
                     attname = "standard_name",
                     attval = "latitude")
    ncdf4::ncatt_put(inv_file,
                     varid = "lat",
                     attname = "comment",
                     attval = "center_of_cell")

    ncdf4::ncvar_put(inv_file,"lon",Vlon)
    ncdf4::ncatt_put(inv_file,
                     varid = "lon",
                     attname = "standard_name",
                     attval = "longitude")
    ncdf4::ncatt_put(inv_file,
                     varid = "lon",
                     attname = "comment",
                     attval = "center_of_cell")

    # WRF style
    # ncdf4::ncvar_put(inv_file,
    #                  "XLONG",
    #                  center_lon)
    # ncdf4::ncatt_put(inv_file,
    #                  varid = "XLONG",
    #                  attname = "units",
    #                  attval = "degree east")
    # ncdf4::ncvar_put(inv_file,
    #                  "XLAT",
    #                  center_lat)
    # ncdf4::ncatt_put(inv_file,
    #                  varid = "XLAT",
    #                  attname = "units",
    #                  attval = "degree north")

    for(i in 1:length(variable)){
      ncdf4::ncvar_put(inv_file,
                       varid = variable[i],
                       zeros)
      ncdf4::ncatt_put(inv_file,
                       varid = variable[i],
                       attname = "standard_name",
                       attval = paste0("tendency_of_atmosphere_mass_of_",variable[i]))
      ncdf4::ncatt_put(inv_file,
                       varid = variable[i],
                       attname = "molecular_weight",
                       attval = mw)
      ncdf4::ncatt_put(inv_file,
                       varid = variable[i],
                       attname = "molecular_weight_units",
                       attval = "g mole-1")
    }
    if(verbose){
      cat("\n")
      print(inv_file)
    }
    ncdf4::nc_close(inv_file)
    cat(paste("\nfile:",filename,"written successfully!\n"))
  }

  inv <- ncdf4::nc_open(filename = filename,write = T)
  for(i in 1:length(variable)){
    if(variable[i] %in% names(inv$var)){
      cat(paste("writing",variable[i],"\n"))
      EMI <- array(0, dim = c(n_lon,n_lat,length(n_datas)))
      EMI <- c(gi[[variable[i]]])
      ncvar_put(inv, varid = variable[i],vals = EMI)
    }else{
      cat(paste("addining and writing",variable[i],"\n"))
      dim_lat <- inv$dim[['lat']]
      dim_lon <- inv$dim[['lon']]
      dim_time <- inv$dim[['time']]

      EMI <- array(0, dim = c(n_lon,n_lat,length(n_datas)))
      EMI <- c(gi[[variable[i]]])

      VAR <- ncdf4::ncvar_def(name          = variable[i],
                              longname      = paste(variable[i],"emissions"),
                              dim           = list(dim_lon,dim_lat,dim_time),
                              units         = "kg m-2 s-1", # deparse_unit(gi)
                              prec          = "float",
                              compression   = COMPRESS)
      inv <- ncdf4::ncvar_add(inv,VAR)
      ncdf4::ncvar_put(inv, varid = variable[i],vals = EMI)
      ncdf4::ncatt_put(inv,
                       varid = variable[i],
                       attname = "standard_name",
                       attval = paste0("tendency_of_atmosphere_mass_of_",variable[i]))
      ncdf4::ncatt_put(inv,
                       varid = variable[i],
                       attname = "molecular_weight",
                       attval = mw)
      ncdf4::ncatt_put(inv,
                       varid = variable[i],
                       attname = "molecular_weight_units",
                       attval = "g mole-1")
    }
  }
  ncdf4::nc_close(inv)
  cat("done!\n")
}
