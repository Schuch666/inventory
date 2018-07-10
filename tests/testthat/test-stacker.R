context("stacker")

test_that("put thinks on the stack", {

  grinded_so2 <- readRDS(paste0(system.file("extdata",package="inventory"),"/grid_so2.Rds"))
  two_times_grinded_so2 <- stacker(list(a = grinded_so2, b = grinded_so2))

  expect_equal(2*nrow(grinded_so2), nrow(two_times_grinded_so2))
})
