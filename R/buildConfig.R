#' @title Build configuration file
#'
#' @description Compile metadata from all MRR and interogation sites from PTAGIS
#'
#' @author Kevin See
#'
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples buildConfig()
#'
buildConfig = function() {

  config_all = queryPtagisMeta()


  # clean things up a bit
  configuration = config_all %>%
    dplyr::mutate(node = NA) %>%
    dplyr::rename(site_type_name = site_type,
                  site_type = type,
                  config_id = configuration_sequence,
                  antenna_group = antenna_group_name) %>%
    # ensure that all antenna_id are at least 2 characters
    dplyr::mutate(
      dplyr::across(
        antenna_id,
        ~ stringr::str_pad(.,
                           pad = "0",
                           width = 2,
                           side = "left")
      )
    ) %>%
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
                  longitude) %>%
    dplyr::mutate(node = ifelse(grepl('^LGR', site_code),
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

  return(configuration)
}
