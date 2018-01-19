#' @title Build Parent-Child table
#'
#' @description Build parent-child table describing which nodes are upstream of which nodes
#'
#' @author Kevin See
#'
#'
#' @param sites_df dataframe containing SiteID, path, and various columns breaking path into Step1, Step2, etc.
#' @param configuration a configuration file built by \code{buildConfig}
#' @param startDate configurations that ended before this date (YYYYMMDD format) will not be included.
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr lubridate
#' @importFrom stringr str_replace
#' @export
#' @return NULL
#' @examples createParentChildDf()


createParentChildDf = function(sites_df,
                               configuration = NULL,
                               startDate = NULL) {

  # if no configuration table provided, build one
  if(is.null(configuration)) configuration = buildConfig()

  # get site code of initial marking site
  root_site = sites_df %>%
    filter(nchar(path) == min(nchar(path))) %>%
    select(SiteID) %>%
    as.matrix() %>%
    as.character()

  # create data.frame of each node, matching up with siteID and path to that siteID
  node_df = sites_df %>%
    rename(EndSite = SiteID) %>%
    mutate(Step0 = root_site) %>%
    tidyr::gather(stepOrder, site, matches('Step')) %>%
    mutate(stepOrder = stringr::str_replace(stepOrder, '^Step', ''),
           stepOrder = as.integer(stepOrder)) %>%
    arrange(EndSite, stepOrder) %>%
    filter(site != '',
           site %in% c(root_site, as.character(sites_df$SiteID))) %>%
    group_by(EndSite) %>%
    mutate(stepOrder = 1:n()) %>%
    ungroup() %>%
    left_join(configuration %>%
                filter(!EndDate < lubridate::ymd(startDate) | is.na(EndDate)) %>%
                select(SiteID, Node, SiteType, matches('RKM')) %>%
                distinct() %>%
                bind_rows(anti_join(configuration %>%
                                      group_by(SiteID) %>%
                                      filter(StartDate == max(StartDate, na.rm = T)) %>%
                                      ungroup() %>%
                                      select(SiteID, Node, SiteType, matches('RKM')) %>%
                                      distinct(),
                                    .,
                                    by = c('SiteID', 'Node'))) %>%
                mutate(site = stringr::str_replace(Node, 'A0$', ''),
                       site = stringr::str_replace(site, 'B0$', '')) %>%
                group_by(Node, site) %>%
                filter(RKMTotal == min(RKMTotal)) %>%
                ungroup(),
              by = c('site')) %>%
    arrange(EndSite, stepOrder, desc(Node)) %>%
    group_by(EndSite) %>%
    mutate(nodeOrder = 1:n()) %>%
    group_by(Node) %>%
    filter(nodeOrder == min(nodeOrder)) %>%
    ungroup() %>%
    group_by(EndSite) %>%
    mutate(nodeOrder = 1:n()) %>%
    ungroup() %>%
    select(EndSite, NodeSite = site, SiteID, SiteType, Node, nodeOrder, matches('RKM'), path)

  parent_child = node_df %>%
    group_by(EndSite) %>%
    mutate(prevNode = lag(Node)) %>%
    ungroup() %>%
    filter(!is.na(prevNode)) %>%
    select(ParentNode = prevNode,
           ChildNode = Node,
           SiteType, matches('RKM'),
           nodeOrder) %>%
    bind_rows(tibble(ParentNode = root_site,
                     ChildNode = root_site,
                     nodeOrder = 1) %>%
                left_join(configuration %>%
                            filter(SiteID == root_site) %>%
                            select(ChildNode = SiteID,
                                   SiteType, matches('RKM')) %>%
                            distinct(),
                          by = 'ChildNode')) %>%
    distinct() %>%
    # arrange mosty by RKM
    mutate(initParent = ifelse(ChildNode == root_site, 'A',
                               ifelse(ParentNode == root_site, 'B', 'C'))) %>%
    arrange(initParent, RKM) %>%
    select(-initParent) %>%
    group_by(ChildNode) %>%
    filter(RKMTotal == max(RKMTotal)) %>%
    slice(1) %>%
    ungroup()

  return(parent_child)
}
