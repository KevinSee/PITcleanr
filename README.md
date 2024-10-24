
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PITcleanr <a href='https://github.com/KevinSee/PITcleanr'><img src='man/figures/logo.png' align="right" width="110" /></a>

<!-- badges: start -->

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/KevinSee/PITcleanr/master?urlpath=rstudio)
<!-- badges: end -->

## Description

`PITcleanr` is an R package for preparing PIT tag data for further
analysis. The package can help import complete tag histories from
[PTAGIS](https://www.ptagis.org/), build a configuration file to help
assign each detection to a “node”, and compress those detections into a
smaller file. It contains functions to determine which detection
locations are upstream or downstream of each other, build a parent-child
relationship table describing whether detection locations are upstream
or downstream in relation to each other, and assign directionality of
movement between each detection location. For analyses that focus on
one-way directional movement (e.g., straightforward CJS models),
`PITcleanr` can help determine which detections fail to meet that
one-way movement assumption and should be examined more closely, and
which detections can be kept.

It was originally conceived as a companion to the
[DABOM](https://github.com/KevinSee/DABOM) package for estimating
abundance of returning anadromous adult fish moving upstream.
`PITcleanr` was designed to prepare the raw PIT tag observations from
[PTAGIS](https://www.ptagis.org/) for use in the `DABOM` package.

***The user can find more information related to installation and use of
this package on the [package
website](https://kevinsee.github.io/PITcleanr/),
<https://kevinsee.github.io/PITcleanr>.***

## Installation Instructions

The `PITcleanr` package can be installed as an R package from GitHub by
using Hadley Wickham’s `devtools` package:

``` r
# install and load remotes, if necessary
install.packages("devtools")
remotes::install_github("KevinSee/PITcleanr", 
                         build_vignettes = TRUE)
```

`devtools` may require a working development environment. Further
details around the installation of `devtools` can be found
[here](https://www.r-project.org/nosvn/pandoc/devtools.html).

-   For Windows, that will involve the downloading and installation of
    Rtools. The latest version of Rtools can be found
    [here](https://cran.r-project.org/bin/windows/Rtools/).
-   For Mac OS, that may involve installing Xcode from teh Mac App
    Store.
-   For Linux, that may involve installing a compiler and various
    development libraries, depending on the specific version of Linux.

To install the latest development version of `PITcleanr`:

``` r
remotes::install_github("KevinSee/PITcleanr@develop")
```

Alternatively, the `PITcleanr` compendium can be downloaded as a zip
file from from this URL:
<https://github.com/KevinSee/PITcleanr/archive/master.zip> Once
extracted, the functions can be sourced individually, or a user can
build the R package locally.

Be sure to use the `build_vignettes = TRUE` argument, as this will build
all of the vignettes (i.e., user manuals) that are included with the
package. Further instructions on how to use `PITcleanr` can be found in
the vignettes, which can be accessed using:

``` r
browseVignettes(package = "PITcleanr")
```

## Authors

PITcleanr is a collaborative project, with the primary contributors
being:

-   Kevin See (Washington Department of Fish & Wildlife)
-   Ryan N. Kinzer (Nez Perce Tribe - Fisheries Resources Management)
-   Rick Orme (Nez Perce Tribe - Fisheries Resources Management)
-   Mike Ackerman (Nez Perce Tribe - Fisheries Resources Management)

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

## Questions?

Please feel free to post an issue to this repository for requested
features, bug fixes, errors in documentation, etc.
