#' @title Compress Detections from DART Query
#'
#' @description For certain locations (currently Lower Granite Dam and Priest Rapids Dam),
#' query DART to return all PIT tags observed there from a given species in a given spawn year,
#' as well as any subsequent detections upstream, and then compress them.
#'
#' @inheritParams queryObsDART
#' @inheritParams compress
#'
#' @author Kevin See
#'
#' @import dplyr
#' @export
#' @return A tibble with particular information from a complete tag history PTAGIS query.
#' @examples compressDART(species = 'Chinook',
#'                        loc = "GRA",
#'                        spawn_year = 2019)

compressDART <- function(species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                         loc = c('GRA', 'PRA'),
                         spawn_year = NULL,
                         configuration = NULL) {

  # what is the species code for each species?
  spp_code = tibble(mark_species_name = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                    t_species = 1:4)


  dart_obs = queryObsDART(species = species,
                          loc = loc,
                          spawn_year = spawn_year) %>%
    left_join(spp_code,
              by = "t_species") %>%
    mutate(obs_type = recode(obs_type,
                             "INT" = "Observation",
                             "MRT" = "Mark",
                             "REC" = "Recapture")) %>%
    select(tag_code = tag_id,
           mark_species_name,
           mark_rear_type_name = t_rear_type,
           event_type_name = obs_type,
           event_site_code_value = obs_site,
           event_date_time_value = obs_time,
           antenna_id = coil_id,
           antenna_group_configuration_value = config,
           # event_release_site_code_code,
           # event_release_date_time_value,
           everything())

  comp_obs = compress(dart_obs,
                      ignore_event_vs_release = T)

  list('compress_obs' = comp_obs,
       'dart_obs' = dart_obs) %>%
    return()

}
