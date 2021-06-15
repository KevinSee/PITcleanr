#' @title Build Graph of Nodes
#'
#' @description Create a graph object consisting of detection nodes and the edges that connect them.
#'
#' @author Kevin See
#'
#' @param parent_child The output of `buildParentChild()`.
#'
#' @import dplyr tidyr tidygraph
#' @importFrom rlang set_names
#' @export
#' @return A `tbl_graph` or `igraph` object

buildNodeGraph = function(parent_child = NULL) {

  requireNamespace("tidygraph", quietly = TRUE)

  # build table of nodes
  nodes = parent_child %>%
    select(starts_with("parent")) %>%
    rename(label = parent) %>%
    rlang::set_names(nm  = str_remove,
                     "parent_") %>%
    bind_rows(parent_child %>%
                select(starts_with("child")) %>%
                rename(label = child) %>%
                rlang::set_names(nm  = str_remove,
                                 "child_")) %>%
    distinct() %>%
    tibble::rowid_to_column('index')

  # build table of edges (connecting nodes)
  edges <- parent_child %>%
    left_join(nodes, by = c('parent' = 'label')) %>%
    rename(from = index) %>%
    left_join(nodes, by = c('child' = 'label')) %>%
    rename(to = index) %>%
    select(from, to)

  # one graph with all sites
  node_graph = tidygraph::tbl_graph(nodes = nodes,
                                    edges = edges)

  return(node_graph)

}
