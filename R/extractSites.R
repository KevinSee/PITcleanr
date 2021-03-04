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
                        crs = 5070) {

  stopifnot(!is.null(ptagis_file))

  # get PTAGIS metadata
  all_meta = queryPtagisMeta()

  # read in observations
  if(class(ptagis_file)[1] == "character") {
    observations = suppressMessages(read_csv(ptagis_file)) %>%
      janitor::clean_names() %>%
      mutate(across(c(event_date_time_value,
                      event_release_date_time_value),
                    lubridate::mdy_hms))
  } else if(class(ptagis_file)[1] %in% c("spec_tbl_df", "tbl_df", "tbl", "data.frame")) {
    observations = ptagis_file %>%
      as_tibble()
  } else {
    stop("Trouble reading in ptagis_file.\n")
  }

  # pull out all observation sites, and attach some metadata
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
