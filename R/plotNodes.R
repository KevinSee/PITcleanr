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
#' @param ... other arguments for `geom_edge_diagonal`, `geom_node_point()` or `geom_node_label()`.
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

  node_graph = buildNodeGraph(parent_child)

  # create ggraph plot
  node_p = node_graph %>%
    ggraph(layout = layout) +
    geom_edge_diagonal(...) +
    geom_node_point(size = point_size,
                    ...) +
    theme_graph(base_family = 'Times') +
    theme(legend.position = 'bottom')

  if(label_points) {
    node_p = node_p +
      geom_node_label(aes(label = label),
                      size = label_size,
                      label.padding = unit(0.1, 'lines'),
                      label.size = 0.1,
                      ...)
  }

  return(node_p)

}
