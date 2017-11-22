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

  # get metadata for interrogation sites
  print('Querying INT sites\' metadata')
  int_meta = queryInterrogationMeta()
  # get configuration details for interrogation sites
  print('Querying INT sites\' configuration information')
  int_config = queryInterrogationConfig()

  # get metadata for MRR sites
  print('Querying MRR sites\' metadata')
  mrr_meta = queryMRRMeta()

  # put it all together
  config_all = int_meta %>%
    dplyr::full_join(int_config) %>%
    dplyr::mutate(Type = 'INT') %>%
    dplyr::full_join(mrr_meta %>%
                       mutate(Type = 'MRR') %>%
                       mutate(configurationSequence = 0,
                              antennaID = as.character(NA)))

  # clean things up a bit
  config = config_all %>%
    dplyr::mutate(Node = NA,
                  ValidNode = NA,
                  ModelMainBranch = NA,
                  Comment = NA,
                  ArrayOrder = NA,
                  RKMTotal = as.integer(stringr::str_split(rkm, '\\.', simplify = T)) %>%
                    matrix(nrow = nrow(config_all)) %>%
                    rowSums(na.rm = T)) %>%
    dplyr::select(SiteID = siteCode,
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
    dplyr::mutate(Node = ifelse(grepl('^LGR', SiteID),
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
