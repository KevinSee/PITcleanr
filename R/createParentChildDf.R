#' @title Build Parent-Child table
#'
#' @description Build parent-child table describing which nodes are upstream of which nodes
#'
#' @author Kevin See
#'
#'
#' @param sites_df dataframe containing SiteID, path, and various columns breaking path into Step1, Step2, etc.
#' @param configuration a configuration file built by \code{buildConfig}
#' @param startSite site code for the initial tagging site. Used to arrange the parent-child table with this site at top. Default is \code{GRA} for Lower Granite Dam.
#' @param startDate configurations that ended before this date (YYYYMMDD format) will not be included.
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr lubridate
#' @export
#' @return NULL
#' @examples createParentChildDf()


createParentChildDf = function(sites_df,
                               configuration = NULL,
                               startSite = 'GRA',
                               startDate = NULL) {

  # if no configuration table provided, build one
  if(is.null(configuration)) configuration = buildConfig()

  # create data.frame of each node, matching up with siteID and path to that siteID
  node_df = sites_df %>%
    dplyr::rename(EndSite = SiteID) %>%
    dplyr::mutate(Step0 = startSite) %>%
    tidyr::gather(stepOrder, site, matches('Step')) %>%
    dplyr::mutate(stepOrder = stringr::str_replace(stepOrder, '^Step', ''),
                  stepOrder = as.integer(stepOrder)) %>%
    dplyr::arrange(EndSite, stepOrder) %>%
    dplyr::filter(site != '',
                  site %in% c(startSite, as.character(sites_df$SiteID))) %>%
    dplyr::group_by(EndSite) %>%
    dplyr::mutate(stepOrder = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(configuration %>%
                       dplyr::filter(!EndDate < ymd(startDate) | is.na(EndDate)) %>%
                       dplyr::select(SiteID, Node, SiteType, matches('RKM')) %>%
                       dplyr::distinct() %>%
                       dplyr::bind_rows(dplyr::anti_join(configuration %>%
                                                           dplyr::group_by(SiteID) %>%
                                                           dplyr::filter(StartDate == max(StartDate, na.rm = T)) %>%
                                                           dplyr::ungroup() %>%
                                                           dplyr::select(SiteID, Node, SiteType, matches('RKM')) %>%
                                                           dplyr::distinct(),
                                                         .,
                                                         by = c('SiteID', 'Node'))),
                     by = c('site' = 'SiteID')) %>%
    dplyr::arrange(EndSite, stepOrder, desc(Node)) %>%
    dplyr::group_by(EndSite) %>%
    dplyr::mutate(nodeOrder = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(EndSite, site, SiteType, Node, nodeOrder, matches('RKM'), path)

  parent_child = node_df %>%
    dplyr::group_by(EndSite) %>%
    dplyr::mutate(prevNode = lag(Node)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(prevNode)) %>%
    dplyr::select(ParentNode = prevNode,
                  ChildNode = Node,
                  SiteType, matches('RKM'),
                  nodeOrder) %>%
    dplyr::bind_rows(dplyr::tibble(ParentNode = startSite,
                                   ChildNode = startSite,
                                   nodeOrder = 1) %>%
                       dplyr::left_join(configuration %>%
                                          dplyr::filter(SiteID == startSite) %>%
                                          dplyr::select(ChildNode = SiteID,
                                                        SiteType, matches('RKM')) %>%
                                          dplyr::distinct(),
                                        by = 'ChildNode')) %>%
    dplyr::distinct() %>%
    # arrange mosty by RKM
    dplyr::mutate(initParent = ifelse(ChildNode == startSite, 'A',
                                      ifelse(ParentNode == startSite, 'B', 'C'))) %>%
    dplyr::arrange(initParent, RKM) %>%
    dplyr::select(-initParent) %>%
    dplyr::group_by(ChildNode) %>%
    dplyr::filter(RKMTotal == max(RKMTotal)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  return(parent_child)
}
