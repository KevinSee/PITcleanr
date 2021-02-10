#' @title List Parent Locations
#'
#' @description Recursive function for finding all parents of child location
#'
#' @author Kevin See
#'
#' @param loc_code Code of child location, contained in `parent_child`
#' @param parent_child dataframe with at least `parent` and `child` columns.
#' Can be created with `buildParentChild()` function.
#'
#' @import dplyr
#' @export
#' @return character vector of all sites leading to `loc_code` site.
#' @examples listParents()

listParents = function(loc_code = NULL,
                       parent_child = NULL) {

  stopifnot(!is.null(loc_code),
            !is.null(parent_child))

  if(!loc_code %in% parent_child$child) {
    stop(paste(loc_code, 'not found in parent-child table.'))
  }

  nxt_parent = parent_child %>%
    filter(child == loc_code) %>%
    pull(parent)

  if(! nxt_parent %in% parent_child$child) {
    return(c(nxt_parent))
  } else {
    return(c(listParents(nxt_parent, parent_child), nxt_parent))
  }
}
