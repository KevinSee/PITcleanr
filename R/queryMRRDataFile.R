#' @title PTAGIS MRR Data File
#'
#' @description Queries a raw MRR data file for PTAGIS MRR sites, based upon a known file name. NOTE: only the latest, corrected, version of a MRR file is returned
#'
#' @author Kevin See
#'
#' @param file_nm PTAGIS file name.
#' @param file_path if the user has downloaded the PTAGIS file, this function can read in the file, if the file path is included here. The default value, \code{NULL}, indicates that the file should be queried from the PTAGIS API interface.
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr readr purrr stringr lubridate XML xml2 tidyr
#' @importFrom janitor clean_names
#' @export
#' @return NULL
#' @examples queryMRRDataFile(file_nm = "NBD-2014-188-PRD.xml")

queryMRRDataFile = function(file_nm = NULL,
                            file_path = NULL) {

  stopifnot(!is.null(file_nm))

  # set the PTAGIS API url, based on https://api.ptagis.org/index.html

  ptagis_api_site = "https://api.ptagis.org/files/mrr"

  # determine type of file
  if(stringr::str_detect(file_nm, "xml$")) {
    if(!is.null(file_path)) {
      my_content <- xml2::read_xml(paste(file_path, file_nm, sep = "/"))
    } else {
      my_content <- xml2::read_xml(paste(ptagis_api_site,
                                         file_nm,
                                         sep = "/"))
    }
    doc <- XML::xmlParse(my_content)
    col_nms <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//DetailProjectDefinedField")) %>%
      as_tibble()
    tag_data <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//MRREvent")) %>%
      as_tibble()
    names(tag_data)[stringr::str_detect(names(tag_data), "PDV")] <- col_nms$Label[match(names(tag_data)[stringr::str_detect(names(tag_data), "PDV")], col_nms$PDVColumn)]

    tag_data <- tag_data |>
      janitor::clean_names() |>
      dplyr::mutate(
        dplyr::across(
          sequence_number,
          as.numeric
        )
      )

    # fix any date fields
    if(sum(str_detect(names(tag_data), "date")) > 0) {
      tag_data <- tag_data |>
        dplyr::mutate(
          dplyr::across(
            dplyr::contains("date"),
            lubridate::ymd_hms))
    }
  } else {

    if(!is.null(file_path)) {
      my_content = paste(file_path,
                         file_nm,
                         sep = "/")
    } else {
      my_content <- httr::GET(paste(ptagis_api_site,
                                    file_nm,
                                    sep = "/")) |>
        httr::warn_for_status() |>
        httr::content("text",
                      encoding = "UTF-8")
    }

    first_tag_row <-
      my_content |>
      readr::read_tsv(col_names = F,
                      trim_ws = T,
                      show_col_types = FALSE) |>
      dplyr::summarize(row_num = which(stringr::str_detect(X1, "^1 "))) |>
      dplyr::pull(row_num)

    tag_data <- my_content |>
      readr::read_tsv(col_names = F,
                      skip = first_tag_row - 1,
                      trim_ws = T,
                      show_col_types = FALSE) |>
      dplyr::mutate(split_text = stringr::str_split(X1, "  ")) |>
      dplyr::mutate(id = purrr::map_chr(split_text,
                                        .f = function(x) x[1]),
                    dplyr::mutate(
                      dplyr::across(id,
                                    ~ as.numeric(.) |>
                                      suppressWarnings())),
                    pit_tag = purrr::map_chr(split_text,
                                             .f = function(x) x[2]),
                    comments = purrr::map_chr(split_text,
                                              .f = function(x) x[12])
      )

    dates <- tag_data |>
      dplyr::filter(is.na(id),
                    stringr::str_detect(X1, "^CLOSE DATE",
                                        negate = T)) |>
      dplyr::select(X1) |>
      dplyr::mutate(grp_num = stringr::str_split(X1, "=", simplify = T)[,1],
                    event_date = stringr::str_split(X1, "=", simplify = T)[,2]) |>
      dplyr::mutate(
        dplyr::across(grp_num,
                      ~ stringr::str_remove(., "^V")),
        dplyr::across(event_date,
                      ~ lubridate::mdy_hm(.))
      ) |>
      dplyr::select(grp_num,
                    event_date) |>
      dplyr::mutate(
        dplyr::across(event_date,
                      ~ lubridate::floor_date(., unit = "days")))

    tag_data <- tag_data |>
      dplyr::filter(!is.na(id),
                    !is.na(pit_tag)) |>
      dplyr::mutate(comments_split = stringr::str_split(comments, "\\|"),
                    srr = purrr::map_chr(comments_split,
                                         .f = function(x) x[1]),
                    grp_num = stringr::str_sub(srr, -2),
                    dplyr::across(srr,
                                  ~ stringr::str_sub(., 1, 3)),
                    conditional_comments = purrr::map_chr(comments_split,
                                                          .f = function(x) x[2]),
                    text_comments = purrr::map_chr(comments_split,
                                                   .f = function(x) x[3])) |>
      dplyr::mutate(event_type = dplyr::if_else(stringr::str_detect(conditional_comments, "RE"),
                                                "Recapture",
                                                "Mark")) |>
      dplyr::left_join(dates,
                by = dplyr::join_by(grp_num)) |>
      dplyr::select(sequence_number = id,
                    pit_tag,
                    species_run_rear_type = srr,
                    event_date,
                    event_type,
                    conditional_comments,
                    text_comments)


    meta_data <- my_content |>
      readr::read_delim(delim = ":",
                        trim_ws = T,
                        n_max = first_tag_row - 1,
                        col_names = c("name",
                                      "value"),
                        show_col_types = FALSE) |>
      dplyr::filter(stringr::str_detect(name, "- - -", negate = T)) |>
      tidyr::pivot_wider() |>
      janitor::clean_names() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::contains("date"),
          lubridate::mdy_hm))

    tag_data <- tag_data |>
      dplyr::bind_cols(meta_data)

  }

  return(tag_data)

}
