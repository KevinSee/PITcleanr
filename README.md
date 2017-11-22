# PITcleanr

## Description

The PITcleanr package was developed to help query the necessary data to fit a DABOM model (**D**am **A**dult **B**ranch **O**ccupancy **M**odel) in order to estimate adult escapement to various tributaries above a tagging location. A key assumption of a DABOM model is that fish travel along a single upstream route. Therefore, the model will fail if presented with detections by a single fish in multiple tributaries above a branching node. PITcleanr is designed to help clean the PIT tag detection data to identify non-linear upstream pathways and help the user determine which observations to keep. It also summarises final spawning location, and can be combined with biological data from the Lower Granite adult fish trap database for fish tagged there.

## Installation instructions

`PITcleanr` requires several packages that are available through the `tidyverse` package. You can install all the necessary packages by using:

```{r}
install.packages("tidyverse")
```

To install `PITcleanr` you can use Hadley Wickham's `devtools` package. To install and load the devtools package use:

```{r}
install.packages("devtools")
library(devtools)
```

NOTE: To use devtools, you may also have to download and install Rtools (although you shouldn't). The latest version on Rtools can be found at https://cran.r-project.org/bin/windows/Rtools/

Once devtools is successfully installed, use the following to install PITcleanr:

`devtools::install_github("KevinSee/PITcleanr", build_vignettes = TRUE)`

Further instructions on how to use PITcleanr can be found in the package vignette, accessed by typing `browseVignettes(package = 'PITcleanr')`

## Authors

PITcleanr is a collaborative project, with the primary contributors being:

* Kevin See (Quantitative Consultants Inc.)
* Ryan N. Kinzer (Nez Perce Tribe)
* Rick Orme (Nez Perce Tribe)
* Mike Ackerman (Quantitative Consultants Inc.)
