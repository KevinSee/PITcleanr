#' @title Detection Node Schematic
#'
#' @description Create a schematic plot showing the connections
#' between each detection node in a parent-child table.
#'
#' @author Kevin See
#'
#' @param parent_child The output of `buildParentChild()`.
#' @inheritParams ggraph::ggraph
#' @param label_points `TRUE/FALSE` should the nodes be labeled with their names? Default is `TRUE`.
#' @param point_size size for points on graph (passed to `size` argument of `geom_node_point()`).
#' @param label_size size for node labels on graph (passed to `size` argument of `geom_node_label()`).
#' @param ... other arguments for `geom_node_point()` or `geom_node_label()`.
#'
#' @import dplyr tidyr tidygraph ggraph
#' @importFrom rlang set_names
#' @export
#' @return

plotNodes = function(parent_child = NULL,
                     layout = "auto",
                     point_size = 5,
                     label_points = T,
                     label_size = 2,
                     ...) {

  requireNamespace("tidygraph", quietly = TRUE)
  requireNamespace("ggraph", quietly = TRUE)

  # build table of nodes
  nodes = parent_child %>%
    select(starts_with("parent")) %>%
    rename(node = parent) %>%
    rlang::set_names(nm  = str_remove,
                     "parent_") %>%
    bind_rows(parent_child %>%
                select(starts_with("child")) %>%
                rename(node = child) %>%
                rlang::set_names(nm  = str_remove,
                                 "child_")) %>%
    distinct() %>%
    mutate(index = 1:n()) %>%
    select(index, label = node, everything())

  # build table of edges (connecting nodes)
  edges = parent_child %>%
    filter(parent != child) %>%
    select(from = parent,
           to = child) %>%
    distinct() %>%
    mutate(edge_num = 1:n()) %>%
    tidyr::pivot_longer(cols = -edge_num,
                        names_to = "direction",
                        values_to = "label") %>%
    left_join(nodes %>%
                select(index, label)) %>%
    select(-label) %>%
    tidyr::pivot_wider(names_from = "direction",
                       values_from = "index") %>%
    select(-edge_num)

  # one graph with all sites
  node_graph = tidygraph::tbl_graph(nodes = nodes,
                                    edges = edges)

  # create ggraph plot
  node_p = node_graph %>%
    ggraph(layout = layout) +
    geom_edge_diagonal() +
    geom_node_point(size = point_size) +
    theme_graph(base_family = 'Times') +
    theme(legend.position = 'bottom')

  if(label_points) {
    node_p = node_p +
      geom_node_label(aes(label = label),
                      size = label_size,
                      label.padding = unit(0.1, 'lines'),
                      label.size = 0.1)
  }

  return(node_p)

}
