#' @title Edit a Parent-Child table
#'
#' @description Allow the user to easily make manual changes to a parent child table.
#'
#' @author Kevin See
#'
#'
#' @param parent_child dataframe produced by `buildParentChild()`.
#' @param fix_list a list of vectors to be changed. Each vector must be length 3,
#' where the first two elements contain the parent and child locations to be edited,
#' and the third element is the new parent location.
#' @param switch_parent_child list of vectors to switch parent/child designations, where
#' each vector consists of a current parent location and a child location, in that order.
#' This is primarily intended to help with sites downstream of a tagging/marking location.
#' Adding a parent/child pair here will not fix parent/child relationships with upstream or
#' downstream locations of the parent/child pair (use the `fix_list` for that).
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr purrr
#' @importFrom stringr str_replace
#' @importFrom rlang set_names
#' @export
#' @return NULL
#' @examples createParentChildDf()


editParentChild = function(parent_child = NULL,
                           fix_list = NULL,
                           switch_parent_child = NULL) {

  stopifnot(!is.null(parent_child))
  stopifnot(sum(map_dbl(fix_list, length) != 3) == 0)


  # pull out some info from original parent/child table
  pc_old = parent_child
  pc_meta =  pc_old %>%
    select(starts_with("parent")) %>%
    rename(site_code = parent) %>%
    rlang::set_names(nm  = str_remove,
                     "parent_") %>%
    bind_rows(pc_old %>%
                select(starts_with("child")) %>%
                rename(site_code = child) %>%
                rlang::set_names(nm  = str_remove,
                                 "child_")) %>%
    distinct()

  # switch old and new parents
  if(!is.null(fix_list)) {
    parent_child <-
      parent_child |>
      inner_join(fix_list %>%
                   purrr::map_df(.f = function(x) {
                     tibble::tibble(parent = x[1],
                                    child = x[2],
                                    new_parent = x[3])
                   }),
                 by = c("parent", "child")) %>%
      mutate(parent = new_parent) %>%
      select(-starts_with("parent_"),
             -new_parent) %>%
      left_join(parent_child %>%
                  select(starts_with("child")) %>%
                  rlang::set_names(str_replace,
                                   pattern = "child",
                                   replacement = "parent") %>%
                  bind_rows(parent_child %>%
                              select(starts_with("parent"))) %>%
                  distinct(),
                by = "parent") %>%
      select(all_of(names(parent_child))) %>%
      bind_rows(anti_join(parent_child,
                          .,
                          by = c("child")))
  }

  # switch some parent/child pairs
  if(!is.null(switch_parent_child)) {
    switch_df = switch_parent_child %>%
      map_df(.f = function(x) {
        tibble(parent = x[1],
               child = x[2])
      })

    parent_child = parent_child %>%
      anti_join(switch_df,
                by = c("parent", "child")) %>%
      bind_rows(parent_child %>%
                  inner_join(switch_df,
                             by = c("parent", "child")) %>%
                  rename(parent = child,
                         child = parent,
                         parent_hydro = child_hydro,
                         child_hydro = parent_hydro) %>%
                  select(all_of(names(parent_child))))
  }

  # fix the parent and child hyrdo sequences (and other meta-data)
  parent_child <-
    parent_child |>
    select(parent, child) %>%
    left_join(pc_meta %>%
                rlang::set_names(nm =  ~ paste("parent", ., sep = "_")) %>%
                rename(parent = parent_site_code),
              by = "parent") %>%
    left_join(pc_meta %>%
                rlang::set_names(nm =  ~ paste("child", ., sep = "_")) %>%
                rename(child = child_site_code),
              by = "child") %>%
    select(any_of(names(pc_old))) %>%
    distinct() %>%
    arrange(parent_hydro,
            child_hydro)

  # issue a warning is a child location now has more than one parent
  if(sum(duplicated(parent_child$child)) > 0) {
    dup_child = parent_child %>%
      filter(child %in% child[duplicated(child)])

    dup_child %>%
      group_by(child) %>%
      summarise(message = paste(child, "has multiple parents:", paste(parent, collapse = ","), "\n")) %>%
      distinct() %>%
      pull(message) %>%
      warning()

  }

  return(parent_child)

}
