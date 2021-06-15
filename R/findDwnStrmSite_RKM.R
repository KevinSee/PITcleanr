#' @title Find downstream site
#'
#' @description For a given site, uses the configuration file to determine which site is encountered next moving downstream,
#' using the report river kilometer (RKM) from the configuration file.
#'
#' @author Kevin See
#'
#' @param site character value of site code
#' @param configurationFile data.frame with column called SiteID with site codes, a column called RKM with the river mask codes,
#' and a column called SiteType with values of either \code{INT} for interrogation sites or \code{MMR} for mark/recapture/recovery sites
#' @param include.MMR.sites \code{TRUE/FALSE} should MMR sites be included. Default is \code{FALSE}
#' @param rootSite character value of the site code for the root site for which the path should be traced from
#'
#' @import dplyr stringr
#' @return NULL
#' @export

findDwnStrmSite_RKM = function(site,
                               configurationFile,
                               include.MMR.sites = F,
                               rootSite = 'GRA') {
  if(!site %in% configurationFile$SiteID) {
    stop('Site code not found in configuraiton file.')
  }

  siteRKM = configurationFile %>%
    filter(SiteID == site) %>%
    pull(RKM) %>%
    unique()

  dwnstrmSites = configurationFile %>%
    filter(grepl(stringr::str_sub(siteRKM, end = -5), RKM)) %>%
    mutate(rmRKM = stringr::str_remove(RKM, stringr::str_sub(siteRKM, end = -4))) %>%
    filter(nchar(rmRKM) <= 3 | SiteID == site) %>%
    mutate(rmRKM = as.numeric(rmRKM)) %>%
    filter(rmRKM < as.numeric(stringr::str_sub(siteRKM, start = -3)))

  if(!include.MMR.sites) {
    dwnstrmSites = dwnstrmSites %>%
      filter(SiteType == 'INT')
  }

  test = if_else(nrow(dwnstrmSites) == 0, F, T)

  while(!test) {
    siteRKM = str_sub(siteRKM, end = -5)

    if(nchar(siteRKM) <= 3) {
      dwnstrmSites = configurationFile %>%
        filter(SiteID == rootSite) %>%
        mutate(rmRKM = 0)
    } else {
      dwnstrmSites = configurationFile %>%
        filter(grepl(str_sub(siteRKM, end = -5), RKM)) %>%
        mutate(rmRKM = str_remove(RKM, str_sub(siteRKM, end = -4))) %>%
        filter(nchar(rmRKM) <= 3 | SiteID == site) %>%
        mutate(rmRKM = as.numeric(rmRKM)) %>%
        filter(rmRKM < as.numeric(str_sub(siteRKM, start = -3)))
    }


    if(!include.MMR.sites) {
      dwnstrmSites = dwnstrmSites %>%
        filter(SiteType == 'INT')
    }

    test = if_else(nrow(dwnstrmSites) == 0, F, T)
  }

  dwnstrmSites %>%
    filter(rmRKM == max(rmRKM)) %>%
    pull(SiteID)
}
