# Inventory
[![Licence:MIT](https://img.shields.io/github/license/hyperium/hyper.svg)](https://opensource.org/licenses/MIT)

Create NetCDF emission inventories.

![hex_logo](https://raw.githubusercontent.com/schuch666/inventary/master/example.png)

## Installation

### System dependencies 

`inventory` import functions from [ncdf4](http://cran.r-project.org/package=ncdf4) for reading model information, [raster](http://cran.r-project.org/package=raster) and [sf](https://cran.r-project.org/web/packages/sf/index.html) to process grinded/geographic information and [units](https://github.com/edzer/units/). These packages need some aditional libraries: 

### To Ubuntu
The following steps are required for installation on Ubuntu:
```bash
  sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable --yes
  sudo apt-get --yes --force-yes update -qq
  # netcdf dependencies:
  sudo apt-get install --yes libnetcdf-dev netcdf-bin
  # units/udunits2 dependency:
  sudo apt-get install --yes libudunits2-dev
  # sf dependencies (without libudunits2-dev):
  sudo apt-get install --yes libgdal-dev libgeos-dev libproj-dev
```

### To Fedora
The following steps are required for installation on Fedora:
```bash
  sudo dnf update
  # netcdf dependencies:
  sudo yum install netcdf-devel
  # units/udunits2 dependency:
  sudo yum install udunits2-devel
  # sf dependencies (without libudunits2-dev):
  sudo yum install gdal-devel proj-devel proj-epsg proj-nad geos-devel
```

### To Windows
No additional steps for windows installation.

Detailed instructions can be found at [netcdf](https://www.unidata.ucar.edu/software/netcdf/), [libudunits2-dev](https://r-quantities.github.io/units/) and [sf](https://r-spatial.github.io/sf/#installing) developers page.

### Package installation

```r
# install.packages("devtools")
devtools::install_github("schuch666/inventory")
```

#### License

EmissV is published under the terms of the [MIT License](https://opensource.org/licenses/MIT). Copyright [(c)](https://raw.githubusercontent.com/atmoschem/emissv/master/LICENSE) 2018 Daniel Schuch.
