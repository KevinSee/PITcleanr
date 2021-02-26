#' @title Add Nodes to Parent-Child Table
#'
#' @description When some sites in the parent-child table have multiple nodes, this function
#' incorporates those nodes into the parent-child table, expanding it so that the parent and
#' child locations now refer to nodes, not sites.
#'
#' @author Kevin See
#'
#' @param parent_child dataframe produced by `buildParentChild()`.
#' @param my_config a my_configuration dataframe, such as one built by `buildmy_config()`.
#'
#' @import dplyr tidyr
#' @importFrom magrittr %<>%
#' @return NULL
#' @examples addParentChildNodes()

addParentChildNodes = function(parent_child = NULL,
                               my_config = NULL) {
  stopifnot(!is.null(parent_child),
            !is.null(my_config))

  # get the nodes for all site codes in the parent-child table
  node_long = my_config %>%
    filter(SiteID %in% union(parent_child$child,
                             parent_child$parent)) %>%
    select(site_code = SiteID,
           node = Node,
           rkm = RKM) %>%
    distinct() %>%
    arrange(site_code, desc(node)) %>%
    group_by(site_code) %>%
    mutate(n_nodes = n_distinct(node)) %>%
    mutate(node_num = paste("node", 1:n(), sep = "_")) %>%
    ungroup() %>%
    left_join(parent_child %>%
                select(site_code = child,
                       hydro = child_hydro) %>%
                bind_rows(parent_child %>%
                            select(site_code = parent,
                                   hydro = parent_hydro) %>%
                            distinct()) %>%
                distinct()) %>%
    select(site_code, hydro, rkm, everything())

  node_wide = node_long %>%
    tidyr::pivot_wider(names_from = "node_num",
                       values_from = "node")

  pc_nodes = parent_child %>%
    left_join(node_wide %>%
                rename(n_parent_nodes = n_nodes),
              by = c("parent" = "site_code")) %>%
    left_join(node_wide %>%
                rename(n_child_nodes = n_nodes),
              by = c("child" = "site_code")) %>%
    group_by(parent, child) %>%
    tidyr::nest(node_wide = -any_of(names(parent_child))) %>%
    ungroup() %>%
    mutate(pc = map(node_wide,
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
    distinct() %>%
    left_join(node_long %>%
                select(parent = node,
                       parent_hydro = hydro),
              by = "parent") %>%
    left_join(node_long %>%
                select(child = node,
                       child_hydro = hydro),
              by = "child")

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
