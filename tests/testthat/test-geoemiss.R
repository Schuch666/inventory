context("geoemiss")

test_that("geoemiss works", {

  continents <- sf::st_read(paste0(system.file("extdata",package="inventory"),"/continent.shp"),
                            quiet = TRUE)
  continents <- continents[1:5,]
  so2 <- read.csv(paste0(system.file("extdata",package="inventory"),"/SO2.csv"))
  names(so2) <- c("region","year","mass")
  so2 <- so2[so2$year == 2010,]
  # for a single pollutant
  so2_2010 <- geoemiss(geom     = continents,
                       values   = so2$mass,
                       names    = so2$region,
                       variable = "so2")

  so2_no_2010 <- readRDS(paste0(system.file("extdata",package="inventory"),"/so2_no.Rds"))
  so2_2010b   <- so2_no_2010[,1:2]

  expect_equal(so2_2010[1,1,drop = T], so2_2010b[1,1,drop = T])
})
