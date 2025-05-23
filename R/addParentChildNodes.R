#' @title Add Nodes to Parent-Child Table
#'
#' @description When some sites in the parent-child table have multiple nodes, this function
#' incorporates those nodes into the parent-child table, expanding it so that the parent and
#' child locations now refer to nodes, not sites.
#'
#' @author Kevin See
#'
#' @param parent_child dataframe produced by `buildParentChild()`.
#' @param configuration a configuration dataframe, such as one built by `buildConfig()`.
#' @inheritParams buildConfig
#'
#' @import dplyr tidyr stringr
#' @return NULL
#' @export
#' @examples addParentChildNodes()

addParentChildNodes = function(parent_child = NULL,
                               configuration = NULL,
                               array_suffix = c("UD",
                                                "UMD",
                                                "A0B0")) {
  stopifnot(!is.null(parent_child),
            !is.null(configuration))

  array_suffix = match.arg(array_suffix)

  # get the nodes for all site codes in the parent-child table
  if(array_suffix %in% c("UD", "UMD")) {
   node_site <-
     configuration %>%
     select(node) %>%
     distinct() %>%
     mutate(site_code = case_when(stringr::str_detect(node, "_D$") &
                                    nchar(node) >= 5 ~ stringr::str_remove(node, "_D$"),
                                  stringr::str_detect(node, "_U$") &
                                    nchar(node) >= 5 ~ stringr::str_remove(node, "_U$"),
                                  stringr::str_detect(node, "_M$") &
                                    nchar(node) >= 5 ~ stringr::str_remove(node, "_M$"),
                                  .default = node))
  } else {
    node_site <-
      configuration %>%
      select(node) %>%
      distinct() %>%
      mutate(site_code = case_when(stringr::str_detect(node, "B0$") &
                                     nchar(node) >= 5 ~ stringr::str_remove(node, "B0$"),
                                   stringr::str_detect(node, "A0$") &
                                     nchar(node) >= 5 ~ stringr::str_remove(node, "A0$"),
                                   .default = node))
  }

  node_long <-
    tibble(site_code = union(parent_child$child,
                                       parent_child$parent)) %>%
    left_join(node_site,
              by = "site_code") %>%
    distinct() %>%
    arrange(site_code, node) %>%
    group_by(site_code) %>%
    mutate(n_nodes = n_distinct(node)) %>%
    mutate(node_num = paste("node", 1:n(), sep = "_")) %>%
    ungroup() %>%
    left_join(parent_child %>%
                select(matches("child")) %>%
                rename(site_code = child) %>%
                rlang::set_names(nm = stringr::str_remove,
                                 pattern = "child_") %>%
                bind_rows(parent_child %>%
                            select(matches("parent")) %>%
                            distinct() %>%
                            rename(site_code = parent) %>%
                            rlang::set_names(nm = stringr::str_remove,
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
    site_3_nodes <-
      node_wide %>%
      filter(!is.na(node_3)) %>%
      pull(site_code)

    site_message = if_else(length(site_3_nodes) == 1,
                           site_3_nodes,
                           paste(site_3_nodes, collapse = " and "))

    message(paste(site_message, "have 3 nodes, causing errors.\n Consider updating configuration file.\n"))
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
                    .f = try(function(x) {
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
                      } else {
                        pc_new = NULL
                      }

                      if(x$n_child_nodes == 2) {
                        pc_new = pc_new %>%
                          bind_rows(x %>%
                                      select(parent = node_1.y,
                                             child = node_2.y))
                      }

                      return(pc_new)
                    }))) %>%
    select(-parent, -child) %>%
    tidyr::unnest(cols = pc) %>%
    select(parent,
           child) %>%
    distinct()

  if(sum(stringr::str_detect(names(parent_child), "hydro")) > 0) {
    pc_nodes <- pc_nodes %>%
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

  if(sum(stringr::str_detect(names(parent_child), "rkm")) > 0) {
    pc_nodes <- pc_nodes %>%
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
