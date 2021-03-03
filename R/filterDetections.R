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
#' @import dplyr tidyr lubridate
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


  keep_obs = obs_direct %>%
    group_by(tag_code) %>%
    tidyr::nest() %>%
    mutate(proc = map(data,
                      .f = function(x) {
                        if(sum(x$direction %in% c("backward", "unknown")) == 0) {
                          x %>%
                            mutate(AutoKeepObs = if_else(min_det <= lubridate::ymd(max_obs_date),
                                                         T, F),
                                   UserKeepObs = if_else(min_det <= lubridate::ymd(max_obs_date),
                                                         T, F)) %>%
                            return()
                        } else if(sum(x$direction %in% c("backward")) > 0 |
                                  sum(x$direction %in% c("unknown")) > 0) {
                          spwn_loc = x %>%
                            filter(slot == max(slot[direction %in% c("forward",
                                                                     "unknown") &
                                                      min_det <= lubridate::ymd(max_obs_date)]))

                          x %>%
                            group_by(node) %>%
                            mutate(max_slot = max(slot[slot <= spwn_loc$slot])) %>%
                            rowwise() %>%
                            mutate(in_spawn_path = if_else(grepl(node, spwn_loc$path),
                                                           T, F)) %>%
                            # select(-travel_time, -start_date) %>%
                            mutate(AutoKeepObs = if_else((in_spawn_path & slot == max_slot) | direction == "start",
                                                         T, F),
                                   UserKeepObs = NA) %>%
                            select(-max_slot, - in_spawn_path) %>%
                            return()
                        }
                      })) %>%
    select(-data) %>%
    unnest(proc) %>%
    ungroup()

  return(keep_obs)

}
