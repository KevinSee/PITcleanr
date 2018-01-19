#' @title Valid Paths
#'
#' @description Construct data.frame of valid paths to each node
#'
#' @author Kevin See
#'
#' @param parent_child_df dataframe with at least \code{ParentNode} and \code{ChildNode} columns
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples getValidPaths()

getValidPaths = function(parent_child_df = NULL,
                         root_site = NULL) {

  stopifnot(!is.null(parent_child_df))

  if(is.null(root_site)) root_site = parent_child_df %>%
      filter(ParentNode == ChildNode) %>%
      select(ParentNode) %>%
      distinct() %>%
      slice(1) %>%
      as.matrix() %>%
      as.character()


  valid_paths = NULL
  num_err = 0
  err_vec = NULL

  for(myNode in unique(parent_child_df$ChildNode)) {
    path = try(listParentNodes(end_node = myNode,
                               parent_child_df = parent_child_df,
                               root_site = root_site) %>%
                 paste(collapse = ' '))

    if(class(path) == 'try-error') {
      num_err = num_err + 1
      err_vec = c(err_vec, myNode)
      next
    }

    if(path == myNode) path = NULL

    valid_paths  = valid_paths %>%
      bind_rows(tibble(Node = myNode,
                                     Path = paste(path, myNode, collapse = ' ')))
  }

  if(num_err > 0) {
    print('Some errors occurred. Vector of error nodes returned')
    return(err_vec)
  }

  if(num_err == 0) {
    return(valid_paths)
  }

}
