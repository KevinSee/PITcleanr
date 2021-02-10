#' @title Quality Check Complete Tag Histories
#'
#' @description The function takes complete capture histories downloaded from PTAGIS
#' and performs several QA/QC checks on them. It returns the results in the form of a list.
#'
#' @author Kevin See
#'
#' @inheritParams compress
#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @export
#' @return a list
#' @examples qcTagHistory()

qcTagHistory = function(ptagis_file = NULL) {

  stopifnot(!is.null(ptagis_file))

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


  # find all "disowned" tags
  disowned_tags = observations %>%
    filter(event_type_name == "Disown") %>%
    pull(tag_code)

  # find all "orphan" tags
  orphan_tags = observations %>%
    filter(event_type_name == "Orphan") %>%
    pull(tag_code)

  # identify batches of fish where lots of replicated event times or release times
  rel_time_batches = observations %>%
    mutate(year = year(event_date_time_value)) %>%
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
    filter(n_release > 0,
           n_release != n_events) %>%
    mutate(event_rel_ratio = n_events / n_release)

  list(disown_tags = disowned_tags,
       orphan_tags = orphan_tags,
       rel_time_batches = rel_time_batches)

}
