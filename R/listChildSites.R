#' @title Child Sites
#'
#' @description Recursive function for finding all child sites of parent site
#'
#' @author Kevin See
#'
#' @param parent_site Site code from parent
#'
#'
#' @import dplyr purrr
#' @export
#' @return NULL
#' @examples listChildSites()

listChildSites = function(parent_site, parent_child_df) {

  child_sites = parent_child_df %>%
    filter(ParentSite %in% parent_site,
           !ChildSite %in% parent_site) %>%
    select(ChildSite) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  test = as.list(child_sites) %>%
    purrr::map_int(.f = function(x) {
      parent_child_df %>%
        filter(ParentSite == x,
               ChildSite != x) %>%
        select(ChildSite) %>%
        distinct() %>%
        nrow()
    })

  if(sum(test) == 0 ) return(child_sites)
  if(sum(test) > 0 ) return(c(child_sites, listChildSites(child_sites, parent_child_df)))

}
