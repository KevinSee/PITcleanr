#' @title Define Column Names for Capture History Matrix
#'
#' @description Based on a parent-child table, this returns a vector of column names to help create a consistent capture history matrix. By default, it will sort using the paths constructed from `buildNodeOrder()`. If river kilometer (`rkm`) is included as a column in the configuration file, this function could use that to help determine the column order.
#'
#' @author Kevin See
#'
#' @param parent_child parent-child table. Could be created from `buildParentChild()` from `PITcleanr` package.
#' @param configuration configuration file. Could be created from `buildConfig()` from `PITcleanr` package.
#' @param use_rkm if the river kilometer code exists in the configuration file (in a column named `rkm`), use those codes to help establish the order of capture history columns. Default value is `FALSE`.
#' @param bottom_sites If provided, represents a character vector of sites to use as "bottom sites". All upstream sites of each bottom site will be grouped just after that bottom site in the capture history matrix.
#' @param incl_all_nodes if some `bottom_sites` are supplied, should all the nodes in the parent-child table be included (default is `TRUE`), or only nodes upstream of those bottom sites be returned (`FALSE`). The initial site will always be returned. If no `bottom_sites` are supplied, this is set to `TRUE`.
#'
#' @import dplyr purrr stringr tidyr
#' @export
#' @return NULL
#' @examples defineCapHistCols()

defineCapHistCols = function(parent_child = NULL,
                             configuration = NULL,
                             use_rkm = FALSE,
                             bottom_sites = NULL,
                             incl_all_nodes = TRUE) {


  # if no bottom sites supplied, set incl_all_nodes == TRUE
  if(is.null(bottom_sites)) {
    incl_all_nodes = TRUE
  }

  # if "rkm" is not a column in the configuration file, set use_rkm == FALSE
  if(!"rkm" %in% names(configuration)) {
    if(use_rkm) {
      message("No column 'rkm' present in the configuration file. use_rkm set to FALSE.")
    }
    use_rkm = FALSE
  }

  # build node order
  node_order = parent_child |>
    addParentChildNodes(configuration) |>
    buildNodeOrder(direction = "u")

  if(use_rkm) {
    config_rkm <-
      configuration |>
      dplyr::group_by(site_code) |>
      dplyr::filter(config_id == max(config_id)) |>
      dplyr::ungroup() |>
      dplyr::select(site_code,
                    site_type,
                    node,
                    dplyr::contains("rkm")) |>
      dplyr::distinct() |>
      dplyr::mutate(rkm_total = stringr::str_split(rkm, "\\.")) %>%
      dplyr::mutate(rkm_total = purrr::map_dbl(rkm_total,
                                               .f = function(x) {
                                                 x %>%
                                                   as.numeric() %>%
                                                   suppressWarnings() %>%
                                                   sum(na.rm = T)
                                               }))

    node_order <-
      node_order |>
      dplyr::left_join(config_rkm |>
                         dplyr::group_by(node) |>
                         dplyr::mutate(n_int = sum(site_type == "INT"),
                                       rkm_keep = if_else(n_int > 0,
                                                          suppressWarnings(min(rkm_total[site_type == "INT"])),
                                                          min(rkm_total))) |>
                         dplyr::filter(rkm_keep == rkm_total) |>
                         dplyr::slice(1) |>
                         dplyr::ungroup() |>
                         dplyr::select(node,
                                       rkm) |>
                         dplyr::distinct(),
                       by = dplyr::join_by(node))

    col_nms <-
      node_order |>
      dplyr::filter(node_order == 1) |>
      dplyr::bind_rows(node_order |>
                         dplyr::filter(node_order != 1) |>
                         dplyr::arrange(rkm,
                                        path)) |>
      dplyr::pull(node)

  } else {

    # default orders by path (alphabetically)
    col_nms <-
      node_order |>
      dplyr::arrange(path) |>
      dplyr::pull(node)
  }


  if( !is.null(bottom_sites) ) {

    site_order <-
      parent_child |>
      buildNodeOrder(direction = "u")

    if(incl_all_nodes) {
      site_levels <-
        c(site_order$node[site_order$node_order == 1],
          bottom_sites,
          site_order$node[site_order$node_order == 2]) |>
        unique()
    } else {
      site_levels <-
        c(site_order$node[site_order$node_order == 1],
          bottom_sites) |>
        unique()
    }

    site_nodes <-
      configuration |>
      dplyr::filter(site_code %in% site_levels) |>
      dplyr::mutate(
        dplyr::across(
          site_code,
          ~ factor(., levels = site_levels))) |>
      dplyr::arrange(site_code,
                     node) |>
      dplyr::select(site_code, node) |>
      dplyr::distinct() |>
      dplyr::mutate(
        dplyr::across(
          node,
          as_factor)) |>
      dplyr::arrange(node) |>
      dplyr::select(node)

    bottom_nodes <-
      site_nodes |>
      dplyr::mutate(site_code = stringr::str_remove(node, "_U"),
                    dplyr::across(
                      site_code,
                      ~ stringr::str_remove(., "_D")),
                    d_node = stringr::str_detect(node, "_D")) |>
      dplyr::group_by(site_code) |>
      dplyr::mutate(n_nodes = dplyr::n(),
                    d_node_exist = dplyr::if_else(sum(d_node) > 0,
                                                  T, F)) |>
      dplyr::mutate(keep_node = dplyr::if_else(n_nodes == 1,
                                               node,
                                               dplyr::if_else(d_node,
                                                              node,
                                                              NA_character_))) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(keep_node)) |>
      dplyr::select(keep_node) |>
      dplyr::distinct() |>
      dplyr::rename(node = keep_node)

    if("rkm" %in% names(configuration)) {

      col_nms <-
        c(bottom_nodes$node[1],
          bottom_nodes |>
            dplyr::slice(-1) |>
            dplyr::mutate(paths = purrr::map(node,
                                             .f = function(x) {
                                               node_order |>
                                                 dplyr::filter(stringr::str_detect(path, x)) %>%
                                                 dplyr::arrange(rkm,
                                                                path,
                                                                node_order) |>
                                                 dplyr::select(node_nm = node)
                                             })) |>
            tidyr::unnest(paths) |>
            dplyr::pull(node_nm) |>
            unique())

    } else {

      col_nms <-
        c(bottom_nodes$node[1],
          bottom_nodes |>
            dplyr::slice(-1) |>
            dplyr::mutate(paths = purrr::map(node,
                                             .f = function(x) {
                                               node_order |>
                                                 dplyr::filter(stringr::str_detect(path, x)) %>%
                                                 dplyr::arrange(path,
                                                                node_order) |>
                                                 dplyr::select(node_nm = node)
                                             })) |>
            tidyr::unnest(paths) |>
            dplyr::pull(node_nm) |>
            unique())
    }

  }

  return(col_nms)

}
