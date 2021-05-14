#' @title Read In PTAGIS Complete Tag History
#'
#' @description The function reads in complete tag histories from PTAGIS that
#' have been downloaded and coverts the date time values from character
#' vectors into date-time vectors
#'
#' @author Kevin See
#'
#' @param ptagis_file is the path to the PTAGIS observation file downloaded as a csv from PTAGIS.
#' This must be the output from a Complete Tag History query (part of the Advanced Reporting).
#' This query should contain: Tag, Mark Species, Mark Rear Type, Event Type, Event Site Type,
#' Event Site Code, Event Date Time, Antenna, Antenna Group Configuration,
#' Event Release Site Code, and Event Release Date Time.

#' @import dplyr lubridate
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @export
#' @return a tibble containing the data downloaded from PTAGIS through a complete capture history query.
#' @examples
#' ptagis_file = system.file("extdata", "PRO_Steelhead_2019.csv", package = "PITcleanr")
#' readCTH(ptagis_file)

readCTH = function(ptagis_file = NULL) {

  if(class(ptagis_file)[1] == "character") {
    observations = suppressMessages(read_csv(ptagis_file)) %>%
      janitor::clean_names()

    # determine format of event date time value
    n_colons = observations %>%
      mutate(event_time = str_split(event_date_time_value, " ", simplify = T)[,2],
             n_colon = str_count(event_time, "\\:")) %>%
      pull(n_colon) %>%
      max()

    if(n_colons == 2) {
      observations %<>%
        mutate(across(c(event_date_time_value,
                        event_release_date_time_value),
                      lubridate::mdy_hms))
    } else if(n_colons == 1) {
      observations %<>%
        mutate(across(c(event_date_time_value,
                        event_release_date_time_value),
                      lubridate::mdy_hm))
    } else {
      warning("Event Date Time Value has strange format.")
    }

  } else if(class(ptagis_file)[1] %in% c("spec_tbl_df", "tbl_df", "tbl", "data.frame")) {
    observations = ptagis_file %>%
      as_tibble()
  } else {
    stop("Trouble reading in ptagis_file.\n")
  }

  return(observations)
}
