
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PITcleanr <a href='https://github.com/BiomarkABS/PITcleanr'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/BiomarkABS/PITcleanr/master?urlpath=rstudio)
<!-- badges: end -->

## Description

`PITcleanr` is an R package for preparing PIT tag data for further
analysis. It was originally conceived as a companion to the
[DABOM](https://github.com/BiomarkABS/DABOM) package for estimating
abundance of returning anadromous adult fish moving upstream.
`PITcleanr` was designed to prepare the raw PIT tag observations from
[PTAGIS](http://www.ptagis.org) for use in the DABOM package.

## Installation instructions

To install `PITcleanr` you can use Hadley Wickham’s `devtools` package.
To install and load the devtools package use:

``` r
install.packages("devtools")
library(devtools)
```

NOTE: To use devtools, you may also have to download and install Rtools
(although you shouldn’t). The latest version on Rtools can be found at
<https://cran.r-project.org/bin/windows/Rtools/>

You can download the compendium as a zip from from this URL:
<https://github.com/BiomarkABS/PITcleanr/archive/master.zip>

Or you can install this compendium as an R package, DABOM, from GitHub
with:

``` r
# install.packages("devtools")
remotes::install_github("BiomarkABS/PITcleanr", 
                        dependencies = TRUE,
                        build_vignettes = TRUE)
```

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
