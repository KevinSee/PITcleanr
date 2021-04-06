#' @title Build Node Order
#'
#' @description Creates a data frame showing the path (through various detection sites) to
#' reach each detection location in a parent-child table, and the node order of that location
#'
#' @author Kevin See
#'
#' @inheritParams buildPaths
#'
#' @import dplyr
#' @export
#' @return data frame containing a column,`end_loc`, showing the possible final locations,
#' a column, `node_order`, showing the order of that node along the path,
#' and a column, `path`, showing all the detection locations to pass on the way
#' to that final location.
#' @examples buildNodeOrder()

buildNodeOrder = function(parent_child = NULL,
                          direction = 'u') {

  paths_df = buildPaths(parent_child = parent_child,
                        direction = direction)

  node_order = paths_df %>%
    mutate(node_order = map_dbl(path,
                                str_count,
                                pattern = " "),
           node_order = node_order + 1) %>%
    rename(node = end_loc) %>%
    select(node,
           node_order,
           path)

  # add initial nodes
  first_nodes = parent_child %>%
    filter(parent != child) %>%
    filter(! parent %in% child) %>%
    pull(parent) %>%
    unique()

  node_order = tibble(node = first_nodes,
         node_order = 1,
         path = first_nodes) %>%
    bind_rows(node_order) %>%
    arrange(path)

  return(node_order)

}
