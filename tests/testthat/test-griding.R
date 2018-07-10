context("griding")

test_that("griding methods are working", {

  so2_no_2010 <- readRDS(paste0(system.file("extdata",package="inventory"),"/so2_no.Rds"))
  inventory_so2_no <- griding(so2_no_2010[5,], variable = c("so2","NO"), plot = F)
  inventory_no     <- griding(so2_no_2010[5,], variable = "NO", plot = F)
  inventory_so2    <- griding(so2_no_2010[5,], plot = F)

  expect_equal(inventory_so2_no["so2"], inventory_so2["so2"])

  expect_equal(nrow(inventory_so2_no), nrow(inventory_no))
})
