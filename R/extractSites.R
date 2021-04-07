#' @title Extract Sites
#'
#' @description The function takes complete capture histories downloaded from PTAGIS
#' and extracts all the detection sites from them. It matches those sites with
#' information from PTAGIS metadata, being sure to pull the metadata from the
#' correct configuration file.
#'
#' @author Kevin See
#'
#' @inheritParams compress
#' @param as_sf Should the results be returned as an `sf` object (`TRUE`), or a tibble
#' (`FALSE`, and the default)?
#' @param crs if `as_sf = TRUE`, what CRS should the resulting `sf` object be
#' transformed to? Default is 5070.
#' @param min_date character in the format `YYYYMMDD` that describes a minimum detection date.
#' If supplied, sites with detections prior to this date will be excluded from the results.
#' @param max_date character in the format `YYYYMMDD` that describes a maximum detection date.
#' If supplied, sites with detections after this date will be excluded from the results.
#'
#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @export
#' @return a tibble
#' @examples compress()

extractSites = function(ptagis_file = NULL,
                        as_sf = F,
                        crs = 5070,
                        min_date = NULL,
                        max_date = NULL,
                        configuration = NULL) {

  stopifnot(!is.null(ptagis_file))

  # read in observations
  if(class(ptagis_file)[1] == "character") {
    observations = readCTH(ptagis_file)
  } else if(class(ptagis_file)[1] %in% c("spec_tbl_df", "tbl_df", "tbl", "data.frame")) {
    observations = ptagis_file %>%
      as_tibble()
  } else {
    stop("Trouble reading in ptagis_file.\n")
  }

  if(!is.null(min_date)) {
    observations %<>%
      filter(event_date_time_value >= lubridate::ymd(min_date))
  }

  if(!is.null(max_date)) {
    observations %<>%
      filter(event_date_time_value <= lubridate::ymd(max_date))
  }


  # pull out all observation sites, and attach some metadata
  if(is.null(configuration)) {
    all_meta = queryPtagisMeta()

    obs_site = observations %>%
      select(site_code = event_site_code_value,
             antenna_id,
             configuration_sequence = antenna_group_configuration_value) %>%
      distinct() %>%
      left_join(all_meta %>%
                  select(site_code,
                         site_name,
                         site_type,
                         type,
                         configuration_sequence,
                         antenna_id,
                         antenna_group_name,
                         latitude,
                         longitude,
                         rkm,
                         site_description)) %>%
      select(-antenna_id,
             -configuration_sequence,
             -antenna_group_name) %>%
      distinct()
  } else {
    obs_site = observations %>%
      select(site_code = event_site_code_value,
             antenna_id,
             config_id = antenna_group_configuration_value) %>%
      distinct() %>%
      left_join(configuration %>%
                  select(site_code,
                         site_name,
                         site_type = site_type_name,
                         type = site_type,
                         config_id,
                         antenna_id,
                         node,
                         latitude,
                         longitude,
                         rkm,
                         site_description)) %>%
      select(-antenna_id,
             -config_id) %>%
      distinct() %>%
      # delete sites with another node where the node name differs from the site code
      mutate(node_site = str_remove(node, "A0$"),
             node_site = str_remove(node_site, "B0$")) %>%
      # select(-node) %>%
      distinct() %>%
      group_by(node_site) %>%
      mutate(n_sites = n_distinct(site_code),
             node_site_name = if_else(node_site == site_code,
                                      T, F)) %>%
      ungroup() %>%
      filter(node_site_name | n_sites == 1) %>%
      select(-node, -n_sites, -node_site_name) %>%
      distinct()
}

  # return as data frame or sf object
  if(!as_sf) {
    return(obs_site)
  } else {

    no_lat_sites =
      filter(obs_site,
             is.na(latitude)) %>%
      pull(site_code)
    if(length(no_lat_sites) > 0) {
      no_lat_sites %>%
        paste(collapse = ',') %>%
        paste("These sites don't have a latitude, and are excluded from results:\n",
              ., "\n") %>%
        warning()
    }

    obs_site %>%
      filter(!is.na(latitude)) %>%
      st_as_sf(coords = c("longitude",
                          "latitude"),
               crs = 4326) %>%
      st_transform(crs = crs) %>%
      return()
  }

}
