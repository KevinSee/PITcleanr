#' @title Build Parent-Child table
#'
#' @description Build parent-child table describing which nodes are upstream of which nodes
#'
#' @author Kevin See
#'
#'
#' @param sites_df dataframe containing SiteID, path, and various columns breaking path into Step1, Step2, etc.
#' @param config a configuration file built by \code{buildConfig}
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
                               config = NULL,
                               startSite = 'GRA',
                               startDate = NULL) {

  # if no configuration table provided, build one
  if(is.null(config)) config = buildConfig()

  # create data.frame of each node, matching up with siteID and path to that siteID
  node_df = sites_df %>%
    mutate(SiteID = as.character(SiteID)) %>%
    left_join(config %>%
                filter(!EndDate < ymd(startDate) | is.na(EndDate)) %>%
                select(SiteID, Node, SiteType, matches('RKM')) %>%
                distinct(),
              by = 'SiteID') %>%
    select(SiteID, SiteType, Node, matches('RKM'), everything()) %>%
    mutate(Node = ifelse(is.na(Node), SiteID, Node)) %>%
    gather(step, value, matches('^Step')) %>%
    group_by(Node) %>%
    filter(value != '') %>%
    mutate(stepOrder = str_replace(step, '^Step', ''),
           stepOrder = as.integer(stepOrder)) %>%
    mutate(stepOrder = 1:n()) %>%

    filter(stepOrder == max(stepOrder[value != ''])) %>%
    ungroup() %>%
    mutate(RKM = ifelse(grepl('\\*', RKM), NA, RKM))

  node_df = node_df %>%
    filter(is.na(RKM)) %>%
    select(-matches('RKM'), -SiteType) %>%
    left_join(config %>%
                select(SiteID, StartDate, matches('RKM'), SiteType) %>%
                group_by(SiteID) %>%
                filter(StartDate == max(StartDate, na.rm = T)) %>%
                ungroup() %>%
                select(-StartDate) %>%
                distinct()) %>%
    bind_rows(anti_join(node_df,
                        .,
                        by = c('Node'))) %>%
    arrange(RKM, stepOrder)

  # initial parent-child, containing parent nodes that aren't siteIDs
  init_pc = tibble(ParentNode = startSite,
                   ChildNode = startSite) %>%
    bind_rows(sites_df %>%
                mutate(ParentNode = startSite) %>%
                select(ParentNode,
                       ChildNode = Step1)) %>%
    distinct()

  for(i in 1:(sum(grepl('^Step', names(sites_df))) - 1) ) {
    init_pc = init_pc %>%
      bind_rows(sites_df %>%
                  select(ParentNode = grep(paste0('Step', i), names(sites_df)),
                         ChildNode = grep(paste0('Step', i + 1), names(sites_df))) %>%
                  distinct() %>%
                  filter(ChildNode != '',
                         !ParentNode %in% sites_df$SiteID,
                         !ChildNode %in% sites_df$SiteID))
  }

  # add minimum RKM total, and RKM associated with that site
  init_pc = init_pc %>%
    left_join(node_df %>%
                mutate(Step1 = str_split_fixed(path, '\\.', n = 4)[,1],
                       Step2 = str_split_fixed(path, '\\.', n = 4)[,2],
                       Step3 = str_split_fixed(path, '\\.', n = 4)[,3]) %>%
                select(Node, RKM, RKMTotal, path, starts_with('Step', ignore.case = F)) %>%
                gather(step, ChildNode, starts_with('Step')) %>%
                filter(!ChildNode %in% sites_df$SiteID,
                       ChildNode != '',
                       RKMTotal > 0) %>%
                group_by(ChildNode) %>%
                summarise_at(vars(RKMTotal),
                             funs(min),
                             na.rm = T) %>%
                left_join(node_df %>%
                            mutate(Step1 = str_split_fixed(path, '\\.', n = 4)[,1],
                                   Step2 = str_split_fixed(path, '\\.', n = 4)[,2],
                                   Step3 = str_split_fixed(path, '\\.', n = 4)[,3]) %>%
                            select(starts_with('Step', ignore.case = F), RKM, RKMTotal) %>%
                            distinct() %>%
                            gather(step, ChildNode, matches('^Step')) %>%
                            select(-step))) %>%
    mutate(RKMTotal = ifelse(ChildNode == startSite,
                             config$RKMTotal[config$SiteID == startSite],
                             RKMTotal),
           RKM = ifelse(ChildNode == startSite,
                        config$RKM[config$SiteID == startSite],
                        RKM)) %>%
    mutate(initParent = ifelse(ChildNode == startSite, 'A',
                               ifelse(ParentNode == startSite, 'B', 'C'))) %>%
    group_by(initParent) %>%
    arrange(RKM, .by_group = T) %>%
    ungroup() %>%
    select(-initParent) %>%
    distinct()

  # create full parent-child table
  parent_child = init_pc %>%
    bind_rows(node_df %>%
                filter(!grepl('A0$', Node)) %>%
                select(ParentNode = value,
                       ChildNode = Node,
                       RKM, RKMTotal, SiteType) %>%
                bind_rows(node_df %>%
                            filter(grepl('A0$', Node)) %>%
                            mutate(ParentNode = str_replace(Node, 'A0$', 'B0')) %>%
                            select(ParentNode,
                                   ChildNode = Node,
                                   RKM, RKMTotal, SiteType))) %>%
    mutate(SiteType = ifelse(ChildNode == startSite,
                             config$SiteType[config$SiteID == startSite],
                             SiteType)) %>%
    distinct()

  # correct a few parent nodes
  parent_child = parent_child %>%
    inner_join(node_df %>%
                 select(SiteID, Node),
              by = c('ParentNode' = 'SiteID')) %>%
    filter(Node != ParentNode,
           grepl('A0', Node)) %>%
    mutate(ParentNode = Node) %>%
    select(-Node) %>%
    bind_rows(anti_join(parent_child,
                        .,
                        by = c('ChildNode'))) %>%
    # arrange mosty by RKM
    left_join(node_df %>%
                select(ChildNode = Node,
                       RKM,
                       stepOrder)) %>%
    mutate(stepOrder = ifelse(is.na(stepOrder), 1, stepOrder)) %>%
    mutate(RKM = ifelse(grepl('\\*', RKM), NA, RKM)) %>%
    mutate(initParent = ifelse(ChildNode == startSite, 'A',
                               ifelse(ParentNode == startSite, 'B', 'C'))) %>%
    group_by(initParent) %>%
    arrange(RKM, stepOrder, desc(ChildNode), .by_group = T) %>%
    ungroup() %>%
    select(-initParent, -stepOrder) %>%
    distinct()

  return(parent_child)
}
