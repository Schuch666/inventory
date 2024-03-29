% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/griding.R
\name{griding}
\alias{griding}
\title{Distribute emissions into grids}
\usage{
griding(
  geoemiss,
  variable = NA,
  area_unit = "km^2",
  res = 5,
  type = "global",
  lat = c(-90, 90),
  lon = c(-180, 180),
  tol = res * 1e-05,
  verbose = T,
  plot = F,
  ...
)
}
\arguments{
\item{geoemiss}{a sf from geoemiss}

\item{variable}{name(s) of the pollutant(s)}

\item{area_unit}{area unit, default is "km^2"}

\item{res}{inventory resolution in degrees}

\item{type}{global, local or custom inventary type}

\item{lat}{latitude for custom inventory}

\item{lon}{lontitude for custom inventory}

\item{tol}{param passing to dist of sf::st_buffer}

\item{verbose}{display additional information}

\item{plot}{true for plot separate regions and the final inventory}

\item{...}{additional plot parameters}
}
\value{
a sf with the grid geometry and the emission by cell
}
\description{
calculate the inventory from a geoemiss ouput, there is 3 grid types:
- global: cover all latitudes and longitudes
- local: cover the latitudes and longitudes of the input data
- custom: with custom limits

res argument controls the resolution of the final grid, grids whith higher resolution take more
time to calculate
}
\note{
inventories with multiple dates need to be combined with stacker function
}
\examples{
so2_no_2010 <- readRDS(paste0(system.file("extdata",package="inventory"),"/so2_no.Rds"))
inventory_so2_2010 <- griding(so2_no_2010[5,], variable = c("so2","NO"), plot = TRUE)

}
