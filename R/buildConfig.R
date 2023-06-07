#' @title Build configuration file
#'
#' @description Compile metadata from all MRR and interogation sites from PTAGIS
#'
#' @author Kevin See
#'
#' @param node_assign should nodes by assigned by \code{array}, \code{site} or \code{antenna}? Default is \code{array}.
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
                                       "antenna")) {

  node_assign = match.arg(node_assign)

  config_all = queryPtagisMeta()


  # clean things up a bit
  configuration = config_all %>%
    mutate(node = NA_character_) %>%
    rename(site_type_name = site_type,
           site_type = type,
           config_id = configuration_sequence,
           antenna_group = antenna_group_name) %>%
    select(site_code,
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
      group_by(site_code) %>%
      mutate(node_site = sum(node == site_code),
             node_site_b0 = sum(node == paste0(site_code, "B0"))) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(node = if_else(node_site > 0 & node_site_b0 > 0 & !(grepl("A0$", node) | grepl("B0$", node)),
                            paste0(site_code, 'B0'),
                            node)) %>%
      ungroup() %>%
      select(-node_site,
             -node_site_b0)

  } else if(node_assign == "antenna") {
    configuration <- configuration %>%
      rowwise() |>
      mutate(node = paste(site_code,
                          antenna_id,
                          sep = "_")) |>
      ungroup()

  } else if(node_assign == "site") {
    configuration <- configuration %>%
      mutate(node = site_code)

  }

  return(configuration)
}
