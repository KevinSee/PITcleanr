#' @title Build Possible Paths
#'
#' @description Creates a data frame showing the path (through various detection sites) to
#' reach each detection location in a parent-child table.
#'
#' @author Kevin See
#'
#' @param parent_child data frame with at least `parent` and `child` columns.
#' Can be created with `buildParentChild()` function.
#' @param the direction of movement. Parent-child tables are generally built imagining parents as downstream from children. Assuming that's the case, should the paths be upstream (`u` or `upstream`), the default, or downstream (`d` or `downstream`)?
#'
#' @import dplyr purrr
#' @importFrom magrittr %<>%
#' @export
#' @return data frame containing a column,`end_loc`, showing the possible final locations,
#' and a column, `path`, showing all the detection locations to pass on the way
#' to that final location.
#' @examples buildPaths()

buildPaths = function(parent_child = NULL,
                      direction = c("u",
                                    "d")) {

  stopifnot(!is.null(parent_child))

  direction = str_sub(direction, 1, 1)
  direction = match.arg(direction)

  # make sure parent != child for every row
  prob_rows = parent_child %>%
    filter(parent == child)
  if(nrow(prob_rows) > 0) {
    cat(paste("These child locations were listed as their own parent:\n",
              paste(prob_rows$parent, collapse = ", "),
              "\nThese rows were removed.\n"))
    parent_child = parent_child %>%
      filter(parent != child)
  }

  path_df = parent_child %>%
    pull(child) %>%
    as.list() %>%
    rlang::set_names() %>%
    purrr::map_df(.id = 'end_loc',
           .f = function(x) {
             tibble(path = paste(listParents(x, parent_child), collapse = ' '))
           })
  # add the end location to each path
  path_df %<>%
    mutate(path = paste(path, end_loc))

  if(direction == "d") {
    path_df = map_df(path_df$path,
                     .f = function(x) {
                       path_locs = x %>%
                         stringr::str_split(" ", simplify = T) %>%
                         as.vector() %>%
                         enframe(name = "index",
                                 value = "loc") %>%
                         arrange(desc(index)) %>%
                         mutate(path = NA)

                       for(i in 1:nrow(path_locs)) {
                         path_locs$path[i] = paste(path_locs$loc[path_locs$index >= path_locs$index[i]], collapse = ' ')
                       }

                       path_locs %>%
                         # filter(index < max(index)) %>%
                         select(end_loc = loc,
                                path) %>%
                         distinct()
                     })
  }
  return(path_df)

}

