#' @title Determine Detections to Keep
#'
#' @description After determining the direction of movement using `addDirection()`,
#' this function provides some indication of whether each detection
#' should be kept for an analysis analyzing movement in a single direction
#'
#' @author Kevin See
#'
#' @inheritParams addDirection
#' @param max_obs_date Character string in the format "YYYYMMDD". If included, the output will suggest that observations after this date should be deleted.
#'
#' @import dplyr tidyr lubridate purrr progress
#' @export
#' @return a tibble
#' @examples filterDetections()

filterDetections = function(compress_obs = NULL,
                            parent_child = NULL,
                            max_obs_date = NULL) {

  stopifnot(!is.null(compress_obs),
            !is.null(parent_child))

  obs_direct = addDirection(compress_obs = compress_obs,
                            parent_child = parent_child)

  if(is.null(max_obs_date)) {
    max_obs_date = (max(obs_direct$max_det, na.rm = T) + days(1)) %>%
      format("%Y%m%d") %>%
      as.character()
  }

  # pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
  #                                  total = n_distinct(obs_direct$tag_code))

  pb = dplyr::progress_estimated(n = n_distinct(obs_direct$tag_code))

  keep_obs = obs_direct %>%
    group_by(tag_code) %>%
    tidyr::nest() %>%
    mutate(proc = purrr::map(data,
                             .f = function(x) {
                               pb$tick()$print()

                               if(sum(x$direction %in% c("backward", "unknown")) == 0) {
                                 x %>%
                                   mutate(auto_keep_obs = if_else(min_det <= lubridate::ymd(max_obs_date),
                                                                T, F),
                                          user_keep_obs = if_else(min_det <= lubridate::ymd(max_obs_date),
                                                                T, F)) %>%
                                   return()
                               } else {
                                 spwn_loc = x %>%
                                   filter(slot == max(slot[direction %in% c("start",
                                                                            "forward",
                                                                            "unknown") &
                                                             min_det <= lubridate::ymd(max_obs_date)]))

                                 # see if tag was seen further upstream but along the same path
                                 spwn_loc = x %>%
                                   filter(grepl(spwn_loc$node, path)) %>%
                                   filter(node_order == max(node_order)) %>%
                                   filter(slot == max(slot))


                                 x %>%
                                   group_by(node) %>%
                                   mutate(max_slot = max(1, slot[slot <= spwn_loc$slot])) %>%
                                   ungroup() %>%
                                   rowwise() %>%
                                   mutate(in_spawn_path = if_else(grepl(node, spwn_loc$path),
                                                                  T, F)) %>%
                                   # select(-travel_time, -start_date) %>%
                                   mutate(auto_keep_obs = if_else((in_spawn_path & slot == max_slot) | direction == "start",
                                                                T, F),
                                          user_keep_obs = NA) %>%
                                   select(-max_slot, - in_spawn_path) %>%
                                   return()
                               }
                             })) %>%
    select(-data) %>%
    tidyr::unnest(proc) %>%
    ungroup()

  return(keep_obs)

}
