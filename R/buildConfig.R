#' @title Build configuration file
#'
#' @description Compile metadata from all MRR and interogation sites from PTAGIS
#'
#' @author Kevin See
#'
#' @param node_assign should nodes by assigned by \code{array}, \code{site} or \code{antenna}? Default is \code{array}.
#' @param nodes_2_dmu if \code{node_assign = array} should nodes be assigned downstream (_D), middle (_M), and
#' upstream (_U) suffixes, rather than the default _A0 and _B0.
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples buildConfig()
#'
buildConfig = function(node_assign = c("array", "site", "antenna"),
                       nodes_2_dmu = FALSE) {

  node_assign = match.arg(node_assign)

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
    if(nodes_2_dmu) {
      configuration = configuration %>%
        mutate(#node = ifelse(grepl('^LGR', site_code),
          #               'GRA',
          #              node),
          node = ifelse(grepl('UPSTREAM', antenna_group, ignore.case = T) |
                          grepl('UPPER', antenna_group, ignore.case = T) |
                          grepl('TOP', antenna_group, ignore.case = T),
                        paste0(site_code, '_U'),
                        node),
          node = ifelse(grepl('DOWNSTREAM', antenna_group, ignore.case = T) |
                          grepl('DNSTREAM', antenna_group, ignore.case = T) |
                          grepl('LOWER', antenna_group, ignore.case = T) |
                          grepl('BOTTOM', antenna_group, ignore.case = T),
                        paste0(site_code, '_D'),
                        node),
          node = ifelse(grepl('MIDDLE', antenna_group, ignore.case = T) |
                          grepl('MIDDLE', antenna_group, ignore.case = T),
                        paste0(site_code, '_M'),
                        node),
          node = ifelse(is.na(node), site_code, node))

      # for any site that has some nodes with "A0", "B0", but some configurations with a single node, make that node "B0"
      configuration = configuration %>%
        group_by(site_code) %>%
        mutate(node_site = sum(node == site_code),
               node_site_D = sum(node == paste0(site_code, "_D"))) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(node = if_else(node_site > 0 & node_site_D > 0 & !(grepl("_D$", node) | grepl("_M", node) | grepl("_U$", node)),
                              paste0(site_code, '_D'),
                              node)) %>%
        ungroup() %>%
        select(-node_site,
               -node_site_D)
    } else {
      # if nodes_2_dmu = FALSE
      configuration <- configuration %>%
        mutate(node = ifelse(grepl('^LGR', site_code),
                             'GRA',
                             node),
               node = ifelse(grepl('UPSTREAM', antenna_group, ignore.case = T) |
                               grepl('UPPER', antenna_group, ignore.case = T) |
                               grepl('TOP', antenna_group, ignore.case = T),
                             paste0(site_code, 'A0'),
                             node),
               node = ifelse(grepl('DOWNSTREAM', antenna_group, ignore.case = T) |
                               grepl('DNSTREAM', antenna_group, ignore.case = T) |
                               grepl('LOWER', antenna_group, ignore.case = T) |
                               grepl('BOTTOM', antenna_group, ignore.case = T),
                             paste0(site_code, 'B0'),
                             node),
               node = ifelse(grepl('MIDDLE', antenna_group, ignore.case = T) |
                               grepl('MIDDLE', antenna_group, ignore.case = T),
                             paste0(site_code, 'A0'),
                             node),
               node = ifelse(is.na(node), site_code, node))

      # for any site that has some nodes with "A0", "B0", but some configurations with a single node, make that node "B0"
      configuration = configuration %>%
        dplyr::group_by(site_code) %>%
        dplyr::mutate(node_site = sum(node == site_code),
                      node_site_b0 = sum(node == paste0(site_code, "B0"))) %>%
        dplyr::ungroup() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(node = if_else(node_site > 0 & node_site_b0 > 0 & !(grepl("A0$", node) | grepl("B0$", node)),
                                     paste0(site_code, 'B0'),
                                     node)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-node_site,
                      -node_site_b0)
    }
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
