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
    filter(ParentNode %in% parent_node,
           !ChildNode %in% parent_node) %>%
    select(ChildNode) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  test = as.list(child_nodes) %>%
    purrr::map_int(.f = function(x) {
      parent_child_df %>%
        filter(ParentNode == x,
               ChildNode != x) %>%
        select(ChildNode) %>%
        distinct() %>%
        nrow()
    })

  if(sum(test) == 0 ) return(child_nodes)
  if(sum(test) > 0 ) return(c(child_nodes, listChildNodes(child_nodes, parent_child_df)))

}
