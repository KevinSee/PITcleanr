#' @title Build configuration file
#'
#' @description Compile metadata from all MRR and interogation sites from PTAGIS
#'
#' @author Kevin See
#'
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples buildConfig()
#'
buildConfig = function() {

  config_all = queryPtagisMeta()


  # clean things up a bit
  config = config_all %>%
    mutate(Node = NA,
           ValidNode = NA,
           ModelMainBranch = NA,
           Comment = NA,
           ArrayOrder = NA) %>%
    select(SiteID = siteCode,
           ConfigID = configurationSequence,
           AntennaID = antennaID,
           Node,
           ValidNode,
           StartDate = startDate,
           EndDate = endDate,
           Comment,
           SiteType = Type,
           SiteName = siteName,
           ModelMainBranch,
           AntennaGroup = antennaGroupName,
           ArrayOrder,
           SiteDescription = siteDescription,
           SiteTypeName = siteType,
           RKM = rkm,
           RKMTotal,
           Latitude = latitude,
           Longitude = longitude) %>%
    mutate(Node = ifelse(grepl('^LGR', SiteID),
                         'GRA',
                         Node),
           Node = ifelse(grepl('UPSTREAM', AntennaGroup, ignore.case = T) |
                           grepl('UPPER', AntennaGroup, ignore.case = T) |
                           grepl('TOP', AntennaGroup, ignore.case = T),
                         paste0(SiteID, 'A0'),
                         Node),
           Node = ifelse(grepl('DOWNSTREAM', AntennaGroup, ignore.case = T) |
                           grepl('DNSTREAM', AntennaGroup, ignore.case = T) |
                           grepl('LOWER', AntennaGroup, ignore.case = T) |
                           grepl('BOTTOM', AntennaGroup, ignore.case = T),
                         paste0(SiteID, 'B0'),
                         Node),
           Node = ifelse(grepl('MIDDLE', AntennaGroup, ignore.case = T) |
                           grepl('MIDDLE', AntennaGroup, ignore.case = T),
                         paste0(SiteID, 'A0'),
                         Node),
           Node = ifelse(is.na(Node), SiteID, Node))

  return(config)
}
