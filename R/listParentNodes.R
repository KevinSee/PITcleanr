#' @title Parent Nodes
#'
#' @description Recursive function for finding all parent nodes of child node
#'
#' @author Kevin See
#'
#' @param end_node Code of final node
#' @param parent_child_df dataframe with at least \code{ParentNode} and \code{ChildNode} columns
#' @param root_site initial site; dictates how far backwards this function should search. Default value is \code{GRA} for Lower Granite Dam.
#'
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples listParentNodes()

listParentNodes = function(end_node,
                           parent_child_df,
                           root_site = 'GRA') {

  if(!end_node %in% parent_child_df$ChildNode) {
    stop(paste(end_node, 'not found in parent-child table.'))
  }

  parent_nodes = parent_child_df %>%
    filter(ChildNode == end_node) %>%
    select(ParentNode) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  test = ifelse(parent_nodes == 'GRA', T, F)

  if(test) return(parent_nodes)
  if(!test) return(c(listParentNodes(parent_nodes, parent_child_df), parent_nodes))

}
