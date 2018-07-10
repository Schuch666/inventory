context("possess")

test_that("Ugh, can't you feeling? you are a peace of, no way to set me free", {

  states <- sf::st_read(paste(system.file("extdata", package = "inventory"),"/states.shp",sep=""),
                        quiet = TRUE)
  Nox    <- geoemiss(geom = states,variable = "Nox",names = c("sp","rj"),values = c(1000,20))
  image  <- paste(system.file("extdata", package = "inventory"),"/tiny.tif",sep="")
  ras    <- possess(Nox,image,plots = FALSE)

  test   <- griding(geoemiss = ras,variable = "Nox", res = 0.5, type = "local",
                    plot = FALSE,verbose = F)

  expect_equal(names(ras), c("region","Nox","geometry","image"))
})
