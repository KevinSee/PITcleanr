#' @title Compress PTAGIS Data
#'
#' @description The function takes complete capture histories downloaded from PTAGIS
#' and compresses them. This involves assigning nodes to the detection locations (e.g.
#' sites, arrays, antennas, etc.), and summarizing the first and last detection time on
#' each node as well as how many detections were made during that slot. Slots can be
#' defined to include a maximum number of minutes, or not.
#'
#' @author Kevin See
#'
#' @inheritParams readCTH
#' @param max_minutes maximum number of minutes between detections of a tag before it's considered a
#' different "slot" of detections. Default is `NA`, which means a new slot is only defined by
#' detections on a new node.
#' @param configuration is a data frame which assigns node names to unique SiteID, AntennaID, and
#' site configuration ID combinations. One example can be built with the function `buildConfig`. If
#' no configuration file is provided, nodes are considered site codes by default. If nodes are assigned,
#' the column name should be `node`.
#' @param ignore_event_vs_release Should the function attempt to choose whether to use the event time value
#'  or the event release time value for different release batches? Default is `FALSE`. If set to `TRUE`,
#'  the event time value will be used
#'
#' @inheritParams base::difftime
#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tidyr replace_na
#' @export
#' @return a tibble
#' @examples
#' ptagis_file = system.file("extdata", "PRO_Steelhead_2019.csv", package = "PITcleanr")
#' compress(ptagis_file)

compress = function(ptagis_file = NULL,
                    max_minutes = NA,
                    configuration = NULL,
                    units = c("mins",
                              "auto", "secs", "hours",
                              "days", "weeks"),
                    ignore_event_vs_release = FALSE) {

  stopifnot(!is.null(ptagis_file))

  units = match.arg(units)

  observations = readCTH(ptagis_file)

  # perform some QC checks
  qc_list = qcTagHistory(observations,
                         ignore_event_vs_release = ignore_event_vs_release)

  if(!ignore_event_vs_release) {
  # identify batches of fish where lots of replicated event times or release times
  rel_time_batches = qc_list$rel_time_batches %>%
    filter(event_rel_ratio < 1 |
             (event_rel_ratio == 1 &
                rel_greq_event > rel_ls_event)) %>%
    select(mark_species_name,
           year,
           event_site_type_description,
           event_type_name,
           event_site_code_value) %>%
    mutate(use_release_time = T)

  # set event time to the release time for selected
  observations %<>%
    mutate(year = year(event_date_time_value)) %>%
    left_join(rel_time_batches,
              by = c("mark_species_name",
                     "event_type_name",
                     "event_site_type_description",
                     "event_site_code_value",
                     "year")) %>%
    tidyr::replace_na(replace = list(use_release_time = F)) %>%
    mutate(event_date_time_value = if_else(use_release_time & !is.na(event_release_date_time_value),
                                           event_release_date_time_value,
                                           event_date_time_value)) %>%
    select(-year,
           -use_release_time)
  }

  # filter out disowned and orphan tags, and
  # put observations in correct order in time
  observations %<>%
    filter(! tag_code %in% unique(c(qc_list$disown_tags,
                                  qc_list$orphan_tags))) %>%
    arrange(tag_code, event_date_time_value)

  if(!is.null(configuration)) {
    observations %<>%
      left_join(configuration %>%
                  select(event_site_code_value = site_code,
                         antenna_group_configuration_value = config_id,
                         antenna_id,
                         node),
                by = c("event_site_code_value",
                       "antenna_group_configuration_value",
                       "antenna_id")) %>%
      mutate(node = if_else(is.na(node),
                            event_site_code_value,
                            node))
  } else {
    observations %<>%
      mutate(node = event_site_code_value)
  }

  slot_df = observations %>%
    group_by(tag_code) %>%
    mutate(prev_node = lag(node),
           prev_time = lag(event_date_time_value),
           prev_type = lag(event_type_name),
           time_diff = as.numeric(difftime(event_date_time_value, prev_time, units = 'mins')),
           new_slot = if_else(node != prev_node | is.na(prev_node) |
                                prev_type != event_type_name | is.na(prev_type) |
                                time_diff > max_minutes,
                             T, F)) %>%
    filter(new_slot) %>%
    mutate(slot = 1:n()) %>%
    select(tag_code,
           event_type_name,
           event_date_time_value,
           node,
           event_site_code_value,
           antenna_id,
           new_slot,
           slot)

  compress_summ = observations %>%
    left_join(slot_df,
              by = c("tag_code", "event_type_name", "event_date_time_value", "event_site_code_value", "antenna_id", "node")) %>%
    tidyr::fill(slot, .direction = "down") %>%
    group_by(tag_code,
             node,
             slot,
             event_type_name) %>%
    summarise(n_dets = n(),
              min_det = min(event_date_time_value),
              max_det = max(event_date_time_value),
              .groups = "drop") %>%
    arrange(tag_code, slot) %>%
    mutate(duration = difftime(max_det, min_det, units = units)) %>%
    group_by(tag_code) %>%
    mutate(travel_time = difftime(min_det, lag(max_det),
                                  units = units)) %>%
    ungroup()

  return(compress_summ)
}
