#' @title Read In Complete Tag History
#'
#' @description The function reads in complete tag histories from either PTAGIS or Biologic that
#' have been downloaded and coverts the date time values from character vectors into date-time vectors
#'
#' @author Kevin See
#'
#' @param cth_file is the path to the complete tag history file
#' @param file_type describes the source of the complete tag history file.
#'
#' If \code{PTAGIS}, then \code{cth_file} should be a PTAGIS observation file downloaded as a csv from PTAGIS.
#' This must be the output from a Complete Tag History query (part of the Advanced Reporting).
#' This query should contain: Tag, Mark Species, Mark Rear Type, Event Type, Event Site Type,
#' Event Site Code, Event Date Time, Antenna, Antenna Group Configuration,
#' Event Release Site Code, and Event Release Date Time.
#'
#' If \code{Biologic_csv}, that implies the data was downloaded from Biologic software in a .csv format.
#'
#' If \code{raw}, that implies the data was downloaded directly from the reader, in either a .log or .xlsx format. In this case, the largest string containing alphabetic characters in the file name will be assigned as the site code.
#' @param test_tag_prefix The prefix that designates a tag code as a test tag. These detections are filtered out of the returned tibble

#' @import dplyr lubridate readr readxl stringr purrr
#' @importFrom janitor clean_names
#' @export
#' @return a tibble containing the data downloaded from PTAGIS through a complete capture history query.
#' @examples
#' ptagis_file = system.file("extdata", "PRO_Steelhead_2019.csv", package = "PITcleanr")
#' readCTH(ptagis_file)

readCTH = function(cth_file = NULL,
                   file_type = c("PTAGIS",
                                 "Biologic_csv",
                                 "raw"),
                   test_tag_prefix = "3E7") {

  file_type = match.arg(file_type)

  # vector of required column names. These should all be included in a PTAGIS data request
  req_col_nms <- c("tag_code",
                   "mark_species_name",
                   "mark_rear_type_name",
                   "event_type_name",
                   "event_site_type_description",
                   "event_site_code_value",
                   "event_date_time_value",
                   "antenna_id",
                   "antenna_group_configuration_value",
                   "event_release_site_code_code",
                   "event_release_date_time_value",
                   "cth_count")

  if(class(cth_file)[1] == "character") {

    if(file_type == "PTAGIS") {
      observations <-
        readr::read_csv(cth_file,
                        show_col_types = F) %>%
        janitor::clean_names() |>
        dplyr::select(dplyr::any_of(req_col_nms),
                      dplyr::everything())

      # determine format of event date time value
      n_colons <-
        observations %>%
        dplyr::mutate(event_time = stringr::str_split(event_date_time_value,
                                                      " ",
                                                      simplify = T)[,2],
                      n_colon = stringr::str_count(event_time, "\\:")) %>%
        dplyr::pull(n_colon) %>%
        max()

      if(n_colons == 2) {
        observations <- observations |>
          dplyr::mutate(
            dplyr::across(
              dplyr::any_of(c(event_date_time_value,
                              event_release_date_time_value)),
              lubridate::mdy_hms))
      } else if(n_colons == 1) {
        observations <- observations |>
          dplyr::mutate(
            dplyr::across(
              dplyr::any_of(c(event_date_time_value,
                              event_release_date_time_value)),
              lubridate::mdy_hm))
      } else {
        warning("Event Date Time Value has strange format.")
      }

    } else if(file_type == "Biologic_csv") {
      observations <-
        readr::read_csv(cth_file,
                        show_col_types = F) %>%
        janitor::clean_names() |>
        dplyr::rename(tag_code = tag,
                      event_site_code_value = site,
                      event_date_time_value = detected,
                      antenna_id = reader,
                      antenna_number = antenna) |>
        dplyr::mutate(event_type_name = "Observation",
                      event_site_type_description = "Instream Remote Detection System",
                      antenna_group_configuration_value = 100,
                      cth_count = 1) |>
        dplyr::select(dplyr::any_of(req_col_nms),
                      dplyr::everything()) |>
        dplyr::arrange(tag_code,
                       event_date_time_value)

      # add any missing columns
      observations[,req_col_nms[!req_col_nms %in% names(observations)]] <- NA

      observations <- observations[,req_col_nms]

    } else if(file_type == "raw") {
      if(str_detect(cth_file, "\\.log$")) {
        observations <-
          suppressWarnings(readr::read_table(cth_file,
                                             col_names = F,
                                             show_col_types = F)) |>
          dplyr::filter(stringr::str_detect(X1, "TAG")) |>
          dplyr::mutate(event_date_time_value = lubridate::mdy_hms(paste(X4, X5))) |>
          dplyr::select(tag_code = X6,
                        antenna_id = X2,
                        antenna_number = X3,
                        event_date_time_value)

      } else if(str_detect(cth_file, "\\.xlsx$")) {
        observations <-
          readxl::excel_sheets(cth_file) |>
          purrr::map_df(.f = function(x) {
            readxl::read_excel(cth_file,
                               sheet = x)
          }) |>
          dplyr::mutate(event_date_time_value = lubridate::mdy_hms(paste(`Scan Date`, `Scan Time`))) |>
          dplyr::select(tag_code = `HEX Tag ID`,
                        antenna_id = `Reader ID`,
                        antenna_number = `Antenna ID`,
                        event_date_time_value)
      } else if(str_detect(cth_file, "\\.txt$")) {
        observations <-
          suppressWarnings(readr::read_delim(cth_file,
                                             skip = 6,
                                             delim = " ",
                                             col_names = F,
                                             col_select = c(1, 3, 5, 10, 17, 18, 38),
                                             show_col_types = F)) |>
          dplyr::mutate(event_date_time_value = lubridate::mdy_hms(paste(X1, X3))) |>
          dplyr::select(tag_code = X38,
                        antenna_id = X17,
                        antenna_number = X18,
                        event_date_time_value)

      } else {
        stop("File format not recognized")
      }

      # add a few other columns
      observations <- observations |>
        dplyr::mutate(event_type_name = "Observation",
                      event_site_type_description = "Instream Remote Detection System",
                      antenna_group_configuration_value = 100,
                      cth_count = 1) |>
        dplyr::select(dplyr::any_of(req_col_nms),
                      dplyr::everything()) |>
        dplyr::arrange(tag_code,
                       event_date_time_value)

      # determine site code based on file name
      file_nm_strs <- stringr::str_split(cth_file, "/") |>
        magrittr::extract2(1) %>%
        magrittr::extract(length(.)) |>
        stringr::str_split("_") |>
        magrittr::extract2(1) |>
        stringr::str_remove("\\.xlsx$") |>
        stringr::str_remove("\\.log$") |>
        stringr::str_remove("\\.csv$")
      n_alpha = stringr::str_count(file_nm_strs, "[:alpha:]")
      site_code <- file_nm_strs[which.max(n_alpha)]

      observations$event_site_code_value = site_code

      # add any missing columns
      observations[,req_col_nms[!req_col_nms %in% names(observations)]] <- NA

      observations <- observations[,req_col_nms]
    }

  } else if(class(cth_file)[1] %in% c("spec_tbl_df", "tbl_df", "tbl", "data.frame")) {
    observations = cth_file %>%
      dplyr::as_tibble()
  } else {
    stop("Trouble reading in CTH file\n")
  }

  # filter out test tags
  observations <- observations |>
    dplyr::filter(stringr::str_detect(tag_code,
                                      paste0("^", test_tag_prefix),
                                      negate = T))

  return(observations)
}
