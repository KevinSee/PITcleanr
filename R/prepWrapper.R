#' @title Prepare Data for DABOM
#'
#' @description Filters the compressed detections for those that occur at or after
#' the `start_node`. Adds directionality and columns to indicate whether detections
#' at each node should be retained for DABOM.
#'
#' @author Kevin See
#'
#' @inheritParams addDirection
#' @param start_node character of the node where detection histories should begin.
#' If `NULL`, the node order from the parent-child table will be constructed, and
#' the node with node order of 1 will be used.
#' @param max_obs_date Character string in the format "YYYYMMDD". If included, the
#' output will suggest that observations after this date should be deleted.
#' @param save_file Should the output be saved to a csv or Excel workbook? Default
#' is `FALSE`.
#' @param file_name if `save_file` is `TRUE`, the file name and path to save the
#' output to. Should end in either ".csv" or ".xlsx".
#'
#' @import dplyr tidyr lubridate purrr writexl
#' @export
#' @return a tibble
#' @examples prepWrapper()

prepWrapper = function(compress_obs = NULL,
                       parent_child = NULL,
                       start_node = NULL,
                       max_obs_date = NULL,
                       save_file = F,
                       file_name = NULL) {

  stopifnot(!is.null(compress_obs),
            !is.null(parent_child))

  if(is.null(start_node)) {
    cat("Determining starting node\n")
    node_order = try(buildNodeOrder(parent_child = parent_child))
    start_node = node_order %>%
      filter(node_order == 1) %>%
      pull(node)
  }

  # filter all the compressed observations to start at the start_node
  cat(paste("Filtering observations prior to", start_node, "\n"))
  obs = compress_obs %>%
    left_join(compress_obs %>%
                filter(node == start_node,
                       event_type_name %in% c("Mark", "Recapture")) %>%
                group_by(tag_code) %>%
                filter(max_det == max(max_det)) %>%
                summarise(start_date = max_det,
                          .groups = "drop"),
              by = "tag_code") %>%
    filter(min_det >= start_date) %>%
    group_by(tag_code) %>%
    mutate(slot = slot - min(slot) + 1) %>%
    ungroup()

  # determine which detections to keep
  cat("Determining which detections to retain\n")
  keep_obs = filterDetections(compress_obs = obs,
                              parent_child = parent_child,
                              max_obs_date = max_obs_date)


  if(save_file) {
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
