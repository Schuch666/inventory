context("meta")

test_that("metadating NetCDfs", {

  nc <- paste0(system.file("extdata",package="inventory"),"/small.nc")
  meta(nc)
  meta(nc,attname = "Title")
  meta(nc,variable = "?")
  meta(nc,variable = "so2")
  meta(nc,variable = "so2",attname = "long_name")

  grinded_so2 <- readRDS(paste0(system.file("extdata",package="inventory"),"/grid_so2.Rds"))
  dir.create(file.path(tempdir(), "INV2"))
  saveInventory(grinded_so2,filename = paste0(file.path(tempdir(), "INV2"),"test.nc"),
                variable = "so2", dates = '2010-01-01')

  meta(paste0(file.path(tempdir(), "INV2"),"test.nc"),
           variable = "so2",attname = "new_attribute",
           value = "I'm Alive!",action = "w")

  expect_equal(2 * 2, 4)
})
