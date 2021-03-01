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
  configuration = config_all %>%
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

  # for any site that has some nodes with "A0", "B0", but some configurations with a single node, make that node "B0"
  configuration = configuration %>%
    group_by(SiteID) %>%
    mutate(node_site = sum(Node == SiteID),
           node_site_b0 = sum(Node == paste0(SiteID, "B0"))) %>%
    ungroup() %>%
    mutate(Node = if_else(node_site > 0 & node_site_b0 > 0,
                          paste0(SiteID, 'B0'),
                          Node)) %>%
    select(-node_site,
           -node_site_b0)

  return(configuration)
}
