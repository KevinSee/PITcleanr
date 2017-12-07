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
#' @param parent_child is a data frame created by \code{createParentChildDf}.
#'
#' @param site_df a data frame created by one of the \code{write___NodeNetwork}.
#'
#' @param step_num in the \code{site_df} data frame, which step should be used to group various nodes? Default value is 1.
#'
#' @import dplyr stringr forcats
#' @export
#' @return NULL
#' @examples #createNodeOrder()

createNodeOrder = function(valid_paths = NULL,
                           configuration = NULL,
                           parent_child = NULL,
                           site_df = NULL,
                           step_num = 1) {

  stopifnot(!is.null(valid_paths),
            !is.null(parent_child),
            !is.null(site_df))

  if(is.null(configuration)) configuration = buildConfig()

  node_order = valid_paths %>%
    split(list(.$Node)) %>%
    map_df(.id = 'EndNode',
           .f = function(x) {
             tibble(Node = str_split(x$Path, ' ')[[1]])

           }) %>%
    filter(Node %in% unique(parent_child$ChildNode[!is.na(parent_child$SiteType)])) %>%
    group_by(EndNode) %>%
    mutate(NodeOrder = 1:n(),
           PrevNode = lag(Node)) %>%
    ungroup() %>%
    select(Node, NodeOrder) %>%
    distinct(Node, .keep_all=TRUE) %>%
    full_join(valid_paths,
              by = c('Node')) %>%
    filter(!is.na(Path)) %>%
    left_join(configuration %>%
                select(SiteID, Node, matches('RKM')) %>%
                rowwise() %>%
                filter(grepl(SiteID, Node)) %>%
                ungroup() %>%
                distinct())

  init_brch = node_order %>%
    filter(NodeOrder == 2) %>%
    left_join(configuration %>%
                select(SiteID,
                       Node,
                       RKM,
                       RKMTotal) %>%
                distinct()) %>%
    group_by(Node) %>%
    filter(RKMTotal == min(RKMTotal, na.rm = T)) %>%
    ungroup() %>%
    left_join(site_df %>%
                select(SiteID,
                       Group = paste0('Step', step_num)) %>%
                mutate(Group = factor(Group,
                                      levels = unique(Group)))) %>%
    filter(!is.na(Group)) %>%
    mutate(Group = forcats::fct_reorder(Group, RKMTotal, min)) %>%
    arrange(Group, RKM)

  init_brch = init_brch %>%
    # filter(Group %in% Group[duplicated(Group)]) %>%
    # group_by(Group) %>%
    # slice(1) %>%
    # ungroup() %>%
    # bind_rows(anti_join(init_brch,
    #                     .,
    #                     by = 'Group')) %>%
    # arrange(Group, RKM) %>%
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

  return(node_order)
}
