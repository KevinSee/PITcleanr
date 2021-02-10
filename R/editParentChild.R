#' @title Edit a Parent-Child table
#'
#' @description Allow the user to easily make manual changes to a parent child table.
#'
#' @author Kevin See
#'
#'
#' @param parent_child dataframe produced by `buildParentChild()`.
#' @param parent_locs
#' @param child_locs
#' @param new_parent_locs
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr
#' @importFrom stringr str_replace
#' @importFrom rlang set_names
#' @importFrom magrittr %<>%
#' @export
#' @return NULL
#' @examples createParentChildDf()


editParentChild = function(parent_child = NULL,
                           parent_locs = NULL,
                           child_locs = NULL,
                           new_parent_locs = NULL) {

  parent_child %<>%
    inner_join(tibble(parent = parent_locs,
                      child = child_locs,
                      new_parent = new_parent_locs)) %>%
    mutate(parent = new_parent) %>%
    select(-starts_with("parent_"),
           -new_parent) %>%
    left_join(parent_child %>%
                select(starts_with("child")) %>%
                rlang::set_names(str_replace,
                                 pattern = "child",
                                 replacement = "parent")) %>%
    select(all_of(names(parent_child))) %>%
    bind_rows(anti_join(parent_child,
                        .,
                        by = c("child"))) %>%
    arrange(parent_hydro,
            child_hydro)

  if(sum(is.na(parent_child$parent_hydro)) > 0) {
    parent_child %<>%
      filter(is.na(parent_hydro)) %>%
      select(-starts_with("parent_")) %>%
      left_join(parent_child %>%
                  filter(!is.na(parent_hydro)) %>%
                  select(starts_with("parent")) %>%
                  distinct()) %>%
      select(all_of(names(parent_child))) %>%
      bind_rows(anti_join(parent_child,
                          .,
                          by = c("parent", "child"))) %>%
      arrange(parent_hydro,
              child_hydro)
  }

  return(parent_child)

}
