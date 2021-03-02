#' @title Edit a Parent-Child table
#'
#' @description Allow the user to easily make manual changes to a parent child table.
#'
#' @author Kevin See
#'
#'
#' @param parent_child dataframe produced by `buildParentChild()`.
#' @param parent_locs vector of parent locations to be changed, which must match length of `child_locs` and `new_parent_locs`.
#' @param child_locs vector of child locations whose parents need to change, which must match length of `parent_locs` and `new_parent_locs`.
#' @param new_parent_locs vector of revised parent locations, which must match length of `parent_locs` and `child_locs`.
#' @param switch_parent_child list of vectors to switch parent/child designations, where each vector consists of a current parent location and a child location, in that order. This is primarily intended to help with sites downstream of a tagging/marking location. Adding a parent/child pair here will not fix parent/child relationships with upstream or downstream locations of the parent/child pair (use the `parent_locs`, `child_locs` and `new_parent_locs` for that).
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
                           new_parent_locs = NULL,
                           switch_parent_child = NULL) {

  stopifnot(!is.null(parent_child))
  stopifnot((is.null(parent_locs) & is.null(child_locs) & is.null(new_parent_locs)) |
              !(is.null(parent_locs) & is.null(child_locs) & is.null(new_parent_locs)))
  stopifnot(length(parent_locs) == length(child_locs),
            length(child_locs) == length(new_parent_locs))

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
  if(!is.null(parent_locs)) {
  parent_child %<>%
    inner_join(tibble(parent = parent_locs,
                      child = child_locs,
                      new_parent = new_parent_locs),
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
  parent_child %<>%
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
      summarise(message = paste(child, "has multiple parents:", paste(parent, collapse = ","))) %>%
      distinct() %>%
      pull(message) %>%
      warning()

  }

  return(parent_child)

}
