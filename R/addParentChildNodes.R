#' @title Add Nodes to Parent-Child Table
#'
#' @description When some sites in the parent-child table have multiple nodes, this function
#' incorporates those nodes into the parent-child table, expanding it so that the parent and
#' child locations now refer to nodes, not sites.
#'
#' @author Kevin See
#'
#' @param parent_child dataframe produced by `buildParentChild()`.
#' @param configuration a configuration dataframe, such as one built by `buildmy_config()`.
#'
#' @import dplyr tidyr
#' @importFrom magrittr %<>%
#' @return NULL
#' @export
#' @examples addParentChildNodes()

addParentChildNodes = function(parent_child = NULL,
                               configuration = NULL) {
  stopifnot(!is.null(parent_child),
            !is.null(configuration))

  # get the nodes for all site codes in the parent-child table
  node_long = tibble(site_code = union(parent_child$child,
                                       parent_child$parent)) %>%
    left_join(configuration %>%
                select(node) %>%
                distinct() %>%
                mutate(site_code = if_else(grepl("B0$", node) &
                                             nchar(node) >= 5,
                                           str_remove(node, "B0"),
                                           node),
                       site_code = if_else(grepl("A0$", site_code) &
                                             nchar(site_code) >= 5,
                                           str_remove(site_code, "A0"),
                                           site_code)),
              by = "site_code") %>%
    distinct() %>%
    arrange(site_code, desc(node)) %>%
    group_by(site_code) %>%
    mutate(n_nodes = n_distinct(node)) %>%
    mutate(node_num = paste("node", 1:n(), sep = "_")) %>%
    ungroup() %>%
    left_join(parent_child %>%
                select(matches("child")) %>%
                rename(site_code = child) %>%
                rlang::set_names(nm = str_remove,
                                 pattern = "child_") %>%
                bind_rows(parent_child %>%
                            select(matches("parent")) %>%
                            distinct() %>%
                            rename(site_code = parent) %>%
                            rlang::set_names(nm = str_remove,
                                             pattern = "parent_")) %>%
                distinct(),
              by = "site_code")

  if(sum(is.na(node_long$node)) > 0) {
    node_long %>%
      filter(is.na(node)) %>%
      mutate(message = paste(site_code, "has a node that is NA.\n")) %>%
      pull(message) %>%
      warning()
  }

  node_wide = node_long %>%
    tidyr::pivot_wider(names_from = "node_num",
                       values_from = "node")


  if("node_3" %in% names(node_wide)) {
    node_wide %>%
      filter(!is.na(node_3)) %>%
      pull(site_code) %>%
      paste(paste(., collapse = " and "), "have 3 nodes, causing errors.\n Consider updating configuration file.\n")
  }

  pc_nodes = parent_child %>%
    left_join(node_wide %>%
                rename(n_parent_nodes = n_nodes),
              by = c("parent" = "site_code")) %>%
    left_join(node_wide %>%
                rename(n_child_nodes = n_nodes),
              by = c("child" = "site_code")) %>%
    group_by(parent, child) %>%
    tidyr::nest(node_info = -any_of(names(parent_child))) %>%
    ungroup() %>%
    mutate(pc = map(node_info,
                    .f = function(x) {
                      if(x$n_parent_nodes == 1) {
                        pc_new = x %>%
                          select(parent = node_1.x,
                                 child = node_1.y)
                      } else if(x$n_parent_nodes == 2) {
                        pc_new = x %>%
                          select(parent = node_1.x,
                                 child = node_2.x) %>%
                          bind_rows(x %>%
                                      select(parent = node_2.x,
                                             child = node_1.y))
                      }

                      if(x$n_child_nodes == 2) {
                        pc_new = pc_new %>%
                          bind_rows(x %>%
                                      select(parent = node_1.y,
                                             child = node_2.y))
                      }

                      return(pc_new)
                    })) %>%
    select(-parent, -child) %>%
    tidyr::unnest(cols = pc) %>%
    select(parent,
           child) %>%
    distinct()

  if(sum(grepl("hydro", names(parent_child))) > 0) {
  pc_nodes %<>%
    left_join(node_long %>%
                select(parent = node,
                       parent_hydro = hydro),
              by = "parent") %>%
    left_join(node_long %>%
                select(child = node,
                       child_hydro = hydro),
              by = "child") %>%
    arrange(parent_hydro,
            child_hydro)
  }

  if(sum(grepl("rkm", names(parent_child))) > 0) {
    pc_nodes %<>%
      select(-matches('rkm')) %>%
      left_join(node_long %>%
                  select(parent = node,
                         parent_rkm = rkm) %>%
                  distinct(),
                by = "parent") %>%
      left_join(node_long %>%
                  select(child = node,
                         child_rkm = rkm),
                by = "child") %>%
      select(any_of(names(parent_child))) %>%
      distinct()
  }
    return(pc_nodes)
}
