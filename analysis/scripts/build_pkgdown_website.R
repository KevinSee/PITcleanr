# Author: Kevin See
# Purpose: Build a website for this package using pkgdown
# Created: 5/17/2021
# Last Modified: 5/17/2021
# Notes: Based on instructions found here: https://pkgdown.r-lib.org/index.html

#-----------------------------------------------------------------
# load needed libraries
library(pkgdown)


#-----------------------------------------------------------------
# Run once to configure your package to use pkgdown
usethis::use_pkgdown()

# Run to build the website
pkgdown::build_site()

# to look at the site
preview_site()

#-----------------------------------------------------------------
# deploy site to gh-pages branch on GitHub
deploy_to_branch()
