#' @title Build Parent-Child table
#'
#' @description Build parent-child table describing which nodes are upstream of which nodes
#'
#' @author Kevin See
#'
#'
#' @param sites_df dataframe containing SiteID, path, and various columns breaking path into Step1, Step2, etc.
#' @param config a configuration file built by \code{buildConfig}
#' @param startSite site code for the initial tagging site. Used to arrange the parent-child table with this site at top
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

  node_df = sites_df %>%
    mutate(SiteID = as.character(SiteID)) %>%
    left_join(config %>%
                filter(!EndDate < ymd(startDate) | is.na(EndDate)) %>%
                select(SiteID, Node, SiteType, matches('RKM')) %>%
                distinct(),
              by = 'SiteID') %>%
    select(SiteID, SiteType, Node, matches('RKM'), everything()) %>%
    mutate(Node = ifelse(is.na(Node), SiteID, Node))

  parent_child = tibble(ParentNode = startSite,
                        ChildNode = startSite) %>%
    bind_cols(config %>%
                filter(SiteID == startSite) %>%
                select(RKM, SiteType) %>%
                distinct()) %>%
    bind_rows(tibble(ParentNode = startSite,
                        ChildNode = node_df %>%
                          select(Step1) %>%
                          distinct() %>%
                          as.matrix() %>%
                          as.character(),
                        RKM = config %>%
                          filter(SiteID == startSite) %>%
                          select(RKM) %>%
                          distinct() %>%
                          as.matrix() %>%
                          as.character()))

  brk_vec = 1:sum(grepl('^Step', names(node_df)))
  for(i in brk_vec[brk_vec >= 2]) {
    parent_child = parent_child %>%
      bind_rows(node_df %>%
                  select(ParentNode = grep(paste0('Step', i-1), names(node_df)),
                         ChildNode = grep(paste0('Step', i), names(node_df)),
                         RKM, RKMTotal) %>%
                  filter(ChildNode != '') %>%
                  group_by(ParentNode, ChildNode) %>%
                  filter(RKMTotal == min(RKMTotal, na.rm = T)) %>%
                  ungroup() %>%
                  select(-RKMTotal) %>%
                  distinct() %>%
                  filter(ChildNode != ''))
  }

  parent_child = parent_child %>%
    filter(!ChildNode %in% unique(node_df$SiteID))

  brk_vec = 1:sum(grepl('^Step', names(node_df)))
  for(i in brk_vec[brk_vec >= 2]) {
    parent_child = parent_child %>%
      bind_rows(node_df %>%
                  slice(which(node_df[,paste0('Step', i)] == '')) %>%
                  select(ParentNode = grep(paste0('Step', i-1), names(node_df)),
                         ChildNode = SiteID,
                         RKM,
                         SiteType) %>%
                  filter(ParentNode != '') %>%
                  distinct())
  }

  for(my_site in parent_child$ChildNode) {

    myNode = node_df %>%
      filter(SiteID == my_site)
    if(nrow(myNode) < 2) next

    parent_child$ChildNode[match(my_site, parent_child$ChildNode)] = myNode$Node[grep('B0$', myNode$Node)]
    parent_child = parent_child %>%
      bind_rows(tibble(ParentNode = myNode$Node[grep('B0$', myNode$Node)],
                       ChildNode = myNode$Node[grep('A0$', myNode$Node)]) %>%
                  bind_cols(myNode %>%
                              filter(grepl('A0$', Node)) %>%
                              select(RKM, SiteType)))

    tmp = parent_child %>%
      filter(ParentNode == unique(myNode$SiteID)) %>%
      mutate(ParentNode = paste0(ParentNode, 'A0'))

    parent_child = parent_child %>%
      anti_join(tmp,
                by = 'ChildNode') %>%
      bind_rows(tmp)

    rm(myNode, tmp)

  }

  # arrange mostly by RKM
  parent_child = parent_child %>%
    mutate(RKM = ifelse(grepl('\\*', RKM), NA, RKM)) %>%
    mutate(initParent = ifelse(ParentNode == startSite, 'A', 'B')) %>%
    group_by(initParent) %>%
    arrange(RKM, .by_group = T) %>%
    ungroup() %>%
    select(-initParent)

  return(parent_child)
}
