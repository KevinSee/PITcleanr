#' @title Quality Check Complete Tag Histories
#'
#' @description The function takes complete capture histories downloaded from PTAGIS
#' and performs several QA/QC checks on them. It returns the results in the form of a list.
#'
#' @author Kevin See
#'
#' @inheritParams readCTH
#' @inheritParams compress
#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @export
#' @return a list consisting of a vector of tag codes that are listed as "Disown" in PTAGIS (`disown_tags`),
#' a vector of tag codes that are listed as "Orphan" in PTAGIS (`orphan_tags`), and,
#' if `ignore_event_vs_release` is `TRUE`,
#' a tibble containing information about sites with release information to help the user
#' determine with event time or release time should be used for those sites.
#' @examples
#' ptagis_file = system.file("extdata", "PRO_Steelhead_2019.csv", package = "PITcleanr")
#' qcTagHistory(ptagis_file)

qcTagHistory = function(ptagis_file = NULL,
                        ignore_event_vs_release = F) {

  stopifnot(!is.null(ptagis_file))

  observations = readCTH(ptagis_file)

  # find all "disowned" tags
  disowned_tags = observations %>%
    filter(event_type_name == "Disown") %>%
    pull(tag_code)

  # find all "orphan" tags
  orphan_tags = observations %>%
    filter(event_type_name == "Orphan") %>%
    pull(tag_code)

  result_list = list(disown_tags = disowned_tags,
                     orphan_tags = orphan_tags)

  # identify batches of fish where lots of replicated event times or release times
  if(!ignore_event_vs_release) {
    if(!"event_site_type_description" %in% names(observations)) {
      all_meta = queryPtagisMeta()
      observations = observations %>%
        left_join(all_meta %>%
                    select(event_site_code_value = site_code,
                           antenna_id,
                           antenna_group_configuration_value = configuration_sequence,
                           event_site_type_description = site_description) %>%
                    distinct(),
                  by = c("event_site_code_value", "antenna_id", "antenna_group_configuration_value"))
    }


  rel_time_batches = observations %>%
    mutate(year = lubridate::year(event_date_time_value)) %>%
    group_by(mark_species_name,
             year,
             event_site_type_description,
             event_type_name,
             event_site_code_value) %>%
    summarise(n_tags = n_distinct(tag_code),
              n_events = n_distinct(event_date_time_value, na.rm = T),
              n_release = n_distinct(event_release_date_time_value, na.rm = T),
              rel_greq_event = sum(event_release_date_time_value >= event_date_time_value, na.rm = T),
              rel_ls_event = sum(event_release_date_time_value < event_date_time_value, na.rm = T),
              .groups = "drop") %>%
    filter(n_release > 0) %>%
    # filter(n_release != n_events) %>%
    mutate(event_rel_ratio = n_events / n_release)

  result_list = c(result_list,
                  list(rel_time_batches = rel_time_batches))
  }

  return(result_list)

}
