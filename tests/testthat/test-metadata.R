context("metadata")

test_that("metadating NetCDfs", {

  nc <- paste0(system.file("extdata",package="inventory"),"/small.nc")
  metadata(nc)
  metadata(nc,attname = "Title")
  metadata(nc,variable = "?")
  metadata(nc,variable = "so2")
  metadata(nc,variable = "so2",attname = "long_name")

  grinded_so2 <- readRDS(paste0(system.file("extdata",package="inventory"),"/grid_so2.Rds"))
  dir.create(file.path(tempdir(), "INV2"))
  saveInventory(grinded_so2,filename = paste0(file.path(tempdir(), "INV2"),"test.nc"),
                variable = "so2", dates = '2010-01-01')

  metadata(paste0(file.path(tempdir(), "INV2"),"test.nc"),
           variable = "so2",attname = "new_attribute",value = "I'm Alive!")


  expect_equal(2 * 2, 4)
})
