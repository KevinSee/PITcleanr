#' @title Build configuration file
#'
#' @description Compile metadata from all MRR and interogation sites from PTAGIS
#'
#' @author Kevin See
#'
#' @param node_assign should nodes by assigned by \code{array}, \code{site} or \code{antenna}? Default is \code{array}.
#' @param array_suffix if \code{node_assign = "array"}, should nodes uses the suffixes of _U & _D (\code{UD}),
#' _U, _M, & _D (\code{UMD}), or _A0 & _B0 (\code{A0B0}) to denote upstream, middle, and downstream arrays, respectively.
#' Default is \code{UD}. If \code{array_suffix = "UD"} or \code{array_suffix = "A0B0"}, the middle array is grouped
#' with the upstream array.
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples buildConfig()
#'
buildConfig = function(node_assign = c("array",
                                       "site",
                                       "antenna"),
                       array_suffix = c("UD",
                                        "UMD",
                                        "A0B0")) {

  node_assign = match.arg(node_assign)
  array_suffix = match.arg(array_suffix)

  config_all = PITcleanr::queryPtagisMeta()


  # clean things up a bit
  configuration = config_all %>%
    dplyr::mutate(node = NA_character_) %>%
    dplyr::rename(site_type_name = site_type,
                  site_type = type,
                  config_id = configuration_sequence,
                  antenna_group = antenna_group_name) %>%
    dplyr::select(site_code,
                  config_id,
                  antenna_id,
                  node,
                  start_date,
                  end_date,
                  site_type,
                  site_name,
                  antenna_group,
                  site_description,
                  site_type_name,
                  rkm,
                  rkm_total,
                  latitude,
                  longitude)

  if(node_assign == "array") {
    configuration = configuration %>%
      mutate(node = case_when(grepl('UPSTREAM', antenna_group, ignore.case = T) |
                                grepl('UPPER', antenna_group, ignore.case = T) |
                                grepl('TOP', antenna_group, ignore.case = T)
                              ~ paste0(site_code, "_U"),
                              grepl('MIDDLE', antenna_group, ignore.case = T) |
                                grepl('MIDDLE', antenna_group, ignore.case = T)
                              ~ paste0(site_code, "_M"),
                              grepl('DOWNSTREAM', antenna_group, ignore.case = T) |
                                grepl('DNSTREAM', antenna_group, ignore.case = T) |
                                grepl('LOWER', antenna_group, ignore.case = T) |
                                grepl('BOTTOM', antenna_group, ignore.case = T)
                              ~paste0(site_code, "_D"),
                              is.na(node) ~ site_code,
                              TRUE ~ node))

    # for any site that has some nodes with "_U", "_D", but some configurations with a single node, make that node "_D"
    configuration = configuration %>%
      group_by(site_code) %>%
      mutate(node_site = sum(node == site_code),
             node_site_D = sum(node == paste0(site_code, "_D"))) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(node = if_else(node_site > 0 & node_site_D > 0 & !(grepl("_D$", node) | grepl("_M", node) | grepl("_U$", node)),
                            paste0(site_code, "_D"),
                            node)) %>%
      ungroup() %>%
      select(-node_site,
             -node_site_D)

    # change node suffixes according to array_suffix
    if(array_suffix == "UD") {
      configuration = configuration %>%
        mutate(node = case_when(
          stringr::str_detect(node, "_M$") ~ stringr::str_replace(node, "_M$", "_U"),
          TRUE ~ node))
    }
    if(array_suffix == "UMD") {
      configuration = configuration
    }
    if(array_suffix == "A0B0") {
      configuration = configuration %>%
        mutate(node = case_when(
          stringr::str_detect(node, "_U$") ~ stringr::str_replace(node, "_U$", "_A0"),
          stringr::str_detect(node, "_M$") ~ stringr::str_replace(node, "_M$", "_A0"),
          stringr::str_detect(node, "_D$") ~ stringr::str_replace(node, "_D$", "_B0"),
          TRUE ~ node))
    } # end if node_assign == "array"
  } else if(node_assign == "antenna") {
    configuration <- configuration %>%
      dplyr::rowwise() %>%
      dplyr::mutate(node = paste(site_code,
                                 antenna_id,
                                 sep = "_")) %>%
      dplyr::ungroup()

  } else if(node_assign == "site") {
    configuration <- configuration %>%
      dplyr::mutate(node = site_code)

  }

  return(configuration)
}
