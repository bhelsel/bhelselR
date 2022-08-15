# bhelselR <img src="man/figures/logo.png" align="right" height="139" />
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/bhelselR)](https://CRAN.R-project.org/package=bhelselR)
[![R-CMD-check](https://github.com/bhelsel/bhelselR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bhelsel/bhelselR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
My functions that I use in physical activity epidemiological research.

### Functions for Physical Activity Epidemiology and Weight Management Research

### Install the package from GitHub

Install the devtools package if it is not already installed

```
install.packages("devtools")
```

Use the install_github function to download bhelselR from GitHub

```
library(devtools)
install_github("bhelsel/bhelselR")
devtools::install_github("bhelsel/bhelselR")
```

### Load the bhelselR package

```
library(bhelselR)
```

### Read Actigraph .agd files into R

The default returns the data set, but you can also extract the meta data from
the file if settings = TRUE. If both settings and data are set to TRUE, a
list will be returned.

```
read_agd("path to agd file", settings = FALSE, data = TRUE)

```
