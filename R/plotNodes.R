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
#' @return a ggplot object

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
    ggraph::ggraph(layout = layout) +
    ggraph::geom_edge_diagonal(...) +
    ggraph::geom_node_point(size = point_size,
                            ...) +
    ggraph::theme_graph(base_family = 'Times') +
    ggplot2::theme(legend.position = 'bottom')

  if(label_points) {
    node_p = node_p +
      ggraph::geom_node_label(
        ggplot2::aes(label = label),
        size = label_size,
        label.padding = ggplot2::unit(0.1, 'lines'),
        label.size = 0.1,
        ...)
  }

  return(node_p)

}
