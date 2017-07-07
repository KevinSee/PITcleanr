#' @title Child Nodes
#'
#' @description Recursive function for finding all child nodes of parent node
#'
#' @author Kevin See
#'
#' @param parent_node Site code from parent node
#'
#'
#' @import dplyr purrr
#' @export
#' @return NULL
#' @examples listChildNodes()

listChildNodes = function(parent_node, parent_child_df) {

  child_nodes = parent_child_df %>%
    filter(ParentArray %in% parent_node,
           !ChildArray %in% parent_node) %>%
    select(ChildArray) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  test = as.list(child_nodes) %>%
    map_int(.f = function(x) {
      parent_child_df %>%
        filter(ParentArray == x,
               ChildArray != x) %>%
        select(ChildArray) %>%
        distinct() %>%
        nrow()
    })

  if(sum(test) == 0 ) return(child_nodes)
  if(sum(test) > 0 ) return(c(child_nodes, listChildNodes(child_nodes, parent_child_df)))

}
