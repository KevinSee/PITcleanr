#' @title Prepare Data for DABOM
#'
#' @description Filters the compressed detections for those that occur at or after
#' the `start_node`. Adds directionality and columns to indicate whether detections
#' at each node should be retained for DABOM. If desired, can create the compressed
#' detections with a PTAGIS file path and a configuration file, before doing the rest.
#'
#' @author Kevin See
#'
#' @inheritParams addDirection
#' @inheritParams compress
#' @param start_node character of the node where detection histories should begin.
#' If `NULL`, the node order from the parent-child table will be constructed, and
#' the node with node order of 1 will be used.
#' @param min_obs_date Character string in the format "YYYYMMDD". If included, the
#' output will filter out observations prior to this date.
#' @param max_obs_date Character string in the format "YYYYMMDD". If included, the
#' output will suggest that observations after this date should be deleted.
#' @param add_tag_detects Should a column be added that lists all the sites a tag was
#' detected on, regardless of node? Default is `FALSE`.
#' @param save_file Should the output be saved to a csv or Excel workbook? Default
#' is `FALSE`.
#' @param file_name if `save_file` is `TRUE`, the file name and path to save the
#' output to. Should end in either ".csv" or ".xlsx".
#' @param ... other inputs to `compress`, if not using the default values
#'
#' @import dplyr tidyr lubridate purrr writexl
#' @export
#' @return a tibble
#' @examples prepWrapper()

prepWrapper = function(compress_obs = NULL,
                       cth_file = NULL,
                       file_type = c("PTAGIS",
                                     "Biologic_csv",
                                     "raw"),
                       configuration = NULL,
                       parent_child = NULL,
                       start_node = NULL,
                       min_obs_date = NULL,
                       max_obs_date = NULL,
                       ignore_event_vs_release = FALSE,
                       add_tag_detects = FALSE,
                       save_file = F,
                       file_name = NULL,
                       ...) {

  stopifnot(exprs = {
    (!is.null(compress_obs)) | (!is.null(cth_file) & !is.null(configuration))
  })

  if(add_tag_detects & is.null(cth_file) & is.null(compress_obs)) {
    stop("Original detection file needed to add tag detections.")
  }

  if(is.null(compress_obs)) {
    message("Compressing detections\n")
    raw_obs <- readCTH(cth_file = cth_file,
                          file_type = file_type)

    compress_obs = compress(raw_obs,
                            configuration = configuration,
                            ignore_event_vs_release = ignore_event_vs_release,
                            ...)
  }

  if(is.null(start_node) & !is.null(parent_child)) {
    message("Determining starting node\n")
    node_order = try(buildNodeOrder(parent_child = parent_child))
    if(class(node_order)[1] == "try-error") {
      start_node = NULL
    } else {
      start_node = node_order %>%
        filter(node_order == 1) %>%
        pull(node)
    }
  }

  # filter all compressed observations before the min_obs_date
  if(!is.null(min_obs_date)) {
    message(paste("Filtering observations prior to", format(lubridate::ymd(min_obs_date), "%b %d, %Y"), "\n"))
    compress_obs = compress_obs %>%
      filter(min_det >= lubridate::ymd(min_obs_date))
  }

  # filter all the compressed observations to start at the start_node
  if(!is.null(start_node)) {
    message(paste("Filtering observations prior to", start_node, "\n"))

    obs = compress_obs %>%
      left_join(compress_obs %>%
                  filter(node == start_node,
                         event_type_name %in% c("Mark", "Recapture")) %>%
                  group_by(tag_code) %>%
                  filter(min_det == min(min_det)) %>%
                  slice(1) %>%
                  summarise(start_date = min_det,
                            .groups = "drop"),
                by = "tag_code") %>%
      left_join(compress_obs %>%
                  filter(node == start_node) %>%
                  group_by(tag_code) %>%
                  filter(min_det == min(min_det)) %>%
                  slice(1) %>%
                  summarise(min_root_date = min_det,
                            .groups = "drop"),
                by = "tag_code") %>%
      mutate(start_date = if_else(is.na(start_date),
                                  min_root_date,
                                  start_date)) %>%
      select(-min_root_date) %>%
      filter(min_det >= start_date | is.na(start_date)) %>%
      group_by(tag_code) %>%
      mutate(slot = slot - min(slot) + 1) %>%
      ungroup()
  } else {
    obs = compress_obs %>%
      group_by(tag_code) %>%
      mutate(start_date = min(min_det)) %>%
      ungroup()
  }

  # determine which detections to keep
  message("Determining which detections to retain\n")
  if(!is.null(parent_child)) {
  keep_obs = filterDetections(compress_obs = obs,
                              parent_child = parent_child,
                              max_obs_date = max_obs_date)
  } else {
    if(is.null(max_obs_date)) {
      max_obs_date = (max(obs$max_det, na.rm = T) + lubridate::days(1)) %>%
        format("%Y%m%d") %>%
        as.character()
    }

    keep_obs <- obs %>%
      mutate(auto_keep_obs = if_else(min_det <= lubridate::ymd(max_obs_date),
                                     T, F),
             user_keep_obs = if_else(min_det <= lubridate::ymd(max_obs_date),
                                     T, F))
  }

  if(add_tag_detects) {
    tag_obs <- extractTagObs(obs)

    keep_obs = keep_obs %>%
      dplyr::left_join(tag_obs,
                       by = "tag_code") %>%
      dplyr::relocate(tag_detects,
                      .before = ends_with("keep_obs"))
  }


  if(save_file) {
    if(is.null(file_name)) file_name = "PITcleanr_output.xlsx"

    stopifnot(grepl("csv$", file_name) | grepl("xlsx$", file_name) | grepl("xls$", file_name))

    if(grepl("xlsx$", file_name) | grepl("xls$", file_name)) {
      writexl::write_xlsx(x = keep_obs,
                          path = file_name,
                          col_names = T,
                          format_headers = T)
    }
    if(grepl("csv$", file_name)) {
      readr::write_csv(keep_obs,
                       path = file_name)
    }

  }

  return(keep_obs)
}
