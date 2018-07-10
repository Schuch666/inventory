context("saveInventory")

test_that("saving the NetCDF", {

  grinded_so2 <- readRDS(paste0(system.file("extdata",package="inventory"),"/grid_so2.Rds"))
  dir.create(file.path(tempdir(), "INV"))
  saveInventory(grinded_so2,filename = paste0(file.path(tempdir(), "INV"),"test.nc"),
                variable = "so2", dates = '2010-01-01')

  netcdef <- ncdf4::nc_open(paste0(file.path(tempdir(), "INV"),"test.nc"))
  so2     <- ncdf4::ncvar_get(netcdef,"so2")

  soma <- grinded_so2
  sf::st_geometry(soma) <- NULL
  soma <- sum(soma)

  expect_equal(sum(so2), soma)

})
