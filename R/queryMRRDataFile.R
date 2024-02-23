#' @title PTAGIS MRR Data File
#'
#' @description Queries a raw MRR data file for PTAGIS MRR sites, based upon a known file name. NOTE: only the latest, corrected, version of a MRR file is returned
#'
#' @author Kevin See
#'
#' @param file_nm PTAGIS file name.
#' @param file_path if the user has downloaded the PTAGIS file, this function can read in the file, if the file path is included here. The default value, \code{NULL}, indicates that the file should be queried from the PTAGIS API interface.
#' @param text_only should this function return a file only containing the text of the MRR file? Default is \code{FALSE}, which will parse the MRR data file into a data.frame.
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr readr purrr stringr lubridate XML xml2 tidyr
#' @importFrom janitor clean_names
#' @export
#' @return NULL
#' @examples queryMRRDataFile(file_nm = "NBD-2014-188-PRD.xml")

queryMRRDataFile = function(file_nm = NULL,
                            file_path = NULL,
                            text_only = FALSE) {

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
      dplyr::as_tibble()
    tag_data <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//MRREvent")) %>%
      dplyr::as_tibble()
    names(tag_data)[stringr::str_detect(names(tag_data), "PDV")] <- col_nms$Label[match(names(tag_data)[stringr::str_detect(names(tag_data), "PDV")], col_nms$PDVColumn)]

    if(text_only) {
      txt_df <-
        dplyr::tibble(X1 = paste(names(tag_data),
                                 collapse = "\t")) |>
        bind_rows(tidyr::unite(tag_data,
                               col = "X1",
                               everything(),
                               sep = "\t"))
      return(txt_df)
    }

    tag_data <-
      tag_data |>
      janitor::clean_names() |>
      dplyr::mutate(
        dplyr::across(c(sequence_number),
                      ~ as.numeric(.) |>
                        suppressWarnings()))

    # if length is a field, make it numeric
    if("length" %in% names(tag_data)) {
      tag_data <-
        tag_data |>
        dplyr::mutate(
          dplyr::across(c(length),
                        ~ as.numeric(.) |>
                          suppressWarnings()))
    }

    # fix any date fields
    if(sum(str_detect(names(tag_data), "date")) > 0) {
      tag_data <-
        tag_data |>
        dplyr::mutate(
          dplyr::across(
            dplyr::contains("date"),
             ~ lubridate::ymd_hms(stringr::str_sub(., 1, 19))))
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

    text_file <-
      my_content |>
      readr::read_tsv(col_names = F,
                      trim_ws = T,
                      show_col_types = FALSE)

    if(text_only) {
      return(text_file)
    }

    # what row does tag data start?
    first_tag_row <-
      text_file |>
      dplyr::summarize(row_num = which(stringr::str_detect(X1, "^1 "))) |>
      dplyr::pull(row_num)

    # what row does tag data end?
    last_tag_row <-
      text_file |>
      dplyr::summarize(row_num = min(which(stringr::str_detect(X1, "^V") |
                                             stringr::str_detect(X1, "^CLOSE DATE")))) |>
      dplyr::pull(row_num) - 1

    # get meta data from header
    meta_data <-
      my_content |>
      readr::read_delim(delim = ":",
                        trim_ws = T,
                        n_max = first_tag_row - 1,
                        col_names = c("name",
                                      "value"),
                        show_col_types = FALSE) |>
      suppressWarnings() |>
      dplyr::filter(stringr::str_detect(name, "- - -", negate = T))

    if(is.na(meta_data$value[3]) & !is.na(meta_data$name[3])) {
      meta_data$value[3] <- meta_data$name[3]
      meta_data$name[3] <- "FILE DESCRIPTION"
    }

    meta_data <-
      meta_data |>
      dplyr::filter(!is.na(value)) |>
      tidyr::pivot_wider() |>
      janitor::clean_names() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::contains("date"),
          lubridate::mdy_hm))

    if(! "release_date" %in% names(meta_data) & "tag_date" %in% names(meta_data)) {
      meta_data$release_date = meta_data$tag_date
    }

    # pull out tag data
    tag_data <-
      text_file |>
      dplyr::slice(first_tag_row:last_tag_row) |>
      dplyr::mutate(split_text = stringr::str_split(X1, "[:space:][:space:]+")) |>
      dplyr::mutate(split_text_df = purrr::map(split_text,
                                               .f = function(x) {
                                                 if(length(x) == 3) {
                                                   dplyr::tibble(id = x[1],
                                                                 pit_tag = x[2],
                                                                 length = NA_real_,
                                                                 comments = x[3]) |>
                                                     dplyr::mutate(
                                                       dplyr::across(id,
                                                                     ~ as.numeric(.) |>
                                                                       suppressWarnings()))
                                                 } else if(length(x) == 4) {
                                                   if(stringr::str_detect(x[4], "^\\|")) {
                                                     x[4] <- paste0(x[3], x[4])
                                                     x[3] <- NA_real_
                                                   }

                                                   dplyr::tibble(id = x[1],
                                                                 pit_tag = x[2],
                                                                 length = x[3],
                                                                 comments = x[4]) |>
                                                     dplyr::mutate(
                                                       dplyr::across(c(id,
                                                                       length),
                                                                     ~ as.numeric(.) |>
                                                                       suppressWarnings()))
                                                 }
                                               })) |>
      tidyr::unnest(split_text_df) |>
      dplyr::select(-c(X1,split_text)) |>
      dplyr::filter(stringr::str_detect(pit_tag,
                                        "\\.\\.\\.",
                                        negate = T))

    # extract which dates match which tags
    dates <-
      tryCatch(text_file |>
                 filter(stringr::str_detect(X1, "^V")),
               error =
                 function(cond) {
                   message("Error message:")
                   message(cond)
                   return(dplyr::tibble(X1 = NULL))
                 },
               warning =
                 function(cond) {
                   message("Warning message:")
                   message(cond)
                   return(dplyr::tibble(X1 = NULL))
                 })

    if(nrow(dates) > 0) {
      dates <-
        dates |>
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
    } else {
      dates <- dplyr::tibble(grp_num = "",
                             event_date = meta_data$release_date) |>
        dplyr::mutate(
          dplyr::across(event_date,
                        ~ lubridate::floor_date(., unit = "days")))
    }


    tag_data <-
      tag_data |>
      dplyr::mutate(comments_split = stringr::str_split(comments, "\\|"),
                    srr = purrr::map_chr(comments_split,
                                         .f = function(x) x[1]),
                    dplyr::across(srr,
                                  ~ stringr::str_pad(.,
                                                     width = 5,
                                                     side = "right",
                                                     pad = " ")),
                    grp_num = stringr::str_sub(srr, 4, 5),
                    dplyr::across(grp_num,
                                  ~ stringr::str_trim(.)),
                    dplyr::across(srr,
                                  ~ stringr::str_sub(., 1, 3)),
                    conditional_comments = purrr::map_chr(comments_split,
                                                          .f = function(x) x[2]),
                    text_comments = purrr::map_chr(comments_split,
                                                   .f = function(x) x[3])) |>
      dplyr::mutate(event_type = dplyr::if_else(stringr::str_detect(conditional_comments,
                                                                    "RE"),
                                                "Recapture",
                                                "Mark")) |>
      dplyr::left_join(dates,
                       by = dplyr::join_by(grp_num)) |>
      dplyr::select(sequence_number = id,
                    pit_tag,
                    species_run_rear_type = srr,
                    event_date,
                    event_type,
                    length,
                    conditional_comments,
                    text_comments)


    # add meta data to tag data
    tag_data <- tag_data |>
      dplyr::bind_cols(meta_data)

  }

  return(tag_data)

}
