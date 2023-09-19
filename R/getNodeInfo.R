#' @title Compile information about each node
#'
#' @description Based on a parent-child table and a configuration file, this function compiles how many nodes each site has, what the parent site is, and what child number of that parent the site in question is
#'
#' @author Kevin See
#'
#'
#' @inheritParams defineCapHistCols
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples #getNodeInfo()

getNodeInfo = function(parent_child = NULL,
                       configuration = NULL) {

  # construct node order
  node_order <- buildNodeOrder(parent_child)

  # determine starting point (root_site)
  root_site <- node_order %>%
    filter(node_order == 1) %>%
    pull(node)

  # add nodes to parent-child table
  pc_nodes = addParentChildNodes(parent_child,
                                 configuration = configuration)


  # how many child sites does each parent site have?
  parent_info = parent_child %>%
    # group_by(parent, parent_rkm) %>%
    group_by(parent) %>%
    mutate(n_child = n_distinct(child))

  # get the column names of the capture history matrix
  col_nms = defineCapHistCols(parent_child = parent_child,
                              configuration = configuration)

  # how many nodes does each site have, what are their names and what column are they contained in?
  node_info = configuration %>%
    filter(node %in% unique(c(pc_nodes$parent, pc_nodes$child))) %>%
    # filter(node %in% pc_nodes$child) %>%
    mutate(node_site = if_else(nchar(node) >= 5 & (grepl("_U$", node) | grepl("_D$", node)),
                               stringr::str_remove(stringr::str_remove(node, "_U$"), "_D$"),
                               node)) %>%
    group_by(site_code = node_site) %>%
    summarise(n_nodes = n_distinct(node),
              node = list(unique(node)),
              .groups = "drop") %>%
    unnest(cols = node) %>%
    left_join(tibble(node = col_nms) |>
                mutate(matrix_col = 1:n()),
              by = join_by(node)) |>
    arrange(matrix_col) %>%
    left_join(parent_child %>%
                select(site_code = child,
                       parent_site = parent),
              by = "site_code") %>%
    left_join(parent_child %>%
                left_join(node_order,
                          by = join_by(child == node)) |>
                arrange(path, node_order) %>%
                split(list(.$parent)) %>%
                map_df(.id = "parent_site",
                       .f = function(x) {
                         x %>%
                           mutate(child_num = 1:n()) %>%
                           select(site_code = child,
                                  child_num)
                       }),
              by = c("site_code", "parent_site"))

  return(node_info)

}
