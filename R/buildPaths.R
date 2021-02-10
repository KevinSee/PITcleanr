#' @title Build Possible Paths
#'
#' @description Creates a data frame showing the path (through various detection sites) to
#' reach each detection location in a parent-child table.
#'
#' @author Kevin See
#'
#' @param parent_child data frame with at least `parent` and `child` columns.
#' Can be created with `buildParentChild()` function.
#'
#' @import dplyr
#' @export
#' @return data frame containing a column,`end_loc`, showing the possible final locations,
#' and a column, `path`, showing all the detection locations to pass on the way
#' to that final location.
#' @examples buildPaths()

buildPaths = function(parent_child = NULL) {

  stopifnot(!is.null(parent_child_df))

  path_df = parent_child %>%
    pull(child) %>%
    as.list() %>%
    rlang::set_names() %>%
    map_df(.id = 'end_loc',
           .f = function(x) {
             tibble(path = paste(listParents(x, parent_child), collapse = ' '))
           })
  path_df %<>%
    mutate(path = paste(path, end_loc))
  return(path_df)

}

