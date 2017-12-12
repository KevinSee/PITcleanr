#' @title Node Order
#'
#' @description Based on valid paths, returns a dataframe with a row for each node, showing the path to get to that node, and how many nodes a tag must pass to get there.
#'
#' @author Kevin See
#'
#' @param valid_paths a dataframe created by the \code{getValidPaths} function.
#'
#' @param configuration a configuration file built by \code{buildConfig} function, and possibly modified by the user.
#'
#' @param site_df Optional. A data frame created by one of the \code{write___NodeNetwork}.
#'
#' @param step_num in the \code{site_df} data frame, which step should be used to group various nodes? Default value is 1.
#'
#' @import dplyr stringr forcats
#' @export
#' @return NULL
#' @examples #createNodeOrder()

createNodeOrder = function(valid_paths = NULL,
                           configuration = NULL,
                           site_df = NULL,
                           step_num = 1) {

  stopifnot(!is.null(valid_paths))

  if(is.null(configuration)) configuration = buildConfig()

  # get initial marking site
  root_site = valid_paths %>%
    filter(nchar(Path) == min(nchar(Path))) %>%
    select(Node) %>%
    as.matrix() %>%
    as.character()

  # construct node order, and add some RKM info
  node_order = valid_paths %>%
    split(list(.$Node)) %>%
    map_df(.id = 'EndNode',
           .f = function(x) {
             tibble(Node = str_split(x$Path, ' ')[[1]])

           }) %>%
    group_by(EndNode) %>%
    mutate(NodeOrder = 1:n()) %>%
    ungroup() %>%
    select(Node, NodeOrder) %>%
    mutate(Node = ifelse(NodeOrder == 1 & Node == '',
                         root_site,
                         Node)) %>%
    full_join(valid_paths,
              by = c('Node')) %>%
    distinct() %>%
    filter(!(Node == root_site & NodeOrder > 1)) %>%
    mutate(NodeSite = Node) %>%
    mutate(NodeSite = ifelse(nchar(Node) > 3,
                             stringr::str_replace(NodeSite, 'A0$', ''),
                             NodeSite)) %>%
    mutate(NodeSite = ifelse(nchar(Node) > 3,
                             stringr::str_replace(NodeSite, 'B0$', ''),
                             NodeSite)) %>%
    left_join(configuration %>%
                mutate(NodeSite = gsub('A0$', '',
                                       gsub('B0$', '', Node)),
                       NodeSite = ifelse(nchar(NodeSite) == 1 & grepl('A0$', SiteID),
                                         SiteID,
                                         NodeSite)) %>%
                select(NodeSite, matches('RKM')) %>%
                distinct() %>%
                group_by(NodeSite) %>%
                filter(RKMTotal == min(RKMTotal[RKMTotal > 0])) %>%
                slice(1) %>%
                ungroup())

  if(!is.null(site_df)) {
    init_brch = node_order %>%
      filter(NodeOrder == 2) %>%
      left_join(site_df %>%
                  select(NodeSite = SiteID,
                         Group = paste0('Step', step_num)) %>%
                  mutate(Group = factor(Group,
                                        levels = unique(Group)))) %>%
      filter(!is.na(Group)) %>%
      mutate(Group = forcats::fct_reorder(Group, RKMTotal, min)) %>%
      arrange(Group, RKM)

    init_brch = init_brch %>%
      mutate(BranchNum = 1:n())

    node_order$BranchNum = NA
    for(i in 1:nrow(init_brch)) {
      node_order$BranchNum[grep(init_brch$Node[i], node_order$Path)] = i
    }

    node_order = node_order %>%
      left_join(init_brch %>%
                  select(BranchNum,
                         Group)) %>%
      arrange(BranchNum, RKM) %>%
      filter(!(is.na(BranchNum) & NodeOrder != 1))

  }


  return(node_order)
}
