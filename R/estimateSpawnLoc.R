#' @title Final spawning location
#'
#' @description Based on cleaned capture history, determines further upstream location of each tag. Assumption is that this is the spawning location.
#'
#' @author Kevin See
#'
#' @param filtered_obs Tibble returned by `filterDetections()`.
#' @param spawn_site Should the PTAGIS site code of spawning detection be returned? Default is `FALSE`.
#' @inheritParams compress
#'
#'
#' @import dplyr tidyr
#' @export
#' @return NULL
#' @examples estimateSpawnLoc()

estimateSpawnLoc = function(filtered_obs = NULL,
                            spawn_site = F,
                            ptagis_file = NULL) {

  stopifnot(!is.null(filtered_obs))

  if(sum(is.na(filtered_obs$user_keep_obs)) > 0) {
    stop('user_keep_obs must be defined for each observation.')
  }

  # # if the user processed column is blank, use the auto processed column
  # filtered_obs = filtered_obs %>%
  #   mutate(user_keep_obs = if_else(is.na(user_keep_obs),
  #                                  auto_keep_obs,
  #                                  user_keep_obs))

  # filter for observations that should be kept
  spawn_loc = filtered_obs %>%
    filter(user_keep_obs) %>%
    group_by(tag_code) %>%
    filter(node_order == max(node_order),
           min_det == max(min_det)) %>%
    ungroup() %>%
    select(tag_code,
           spawn_node = node,
           event_type_name,
           min_det,
           max_det) %>%
    left_join(filtered_obs %>%
                filter(user_keep_obs) %>%
                group_by(tag_code) %>%
                summarize(tag_detects = toString(node),
                          .groups = "drop") %>%
                distinct(),
              by = "tag_code")

  if(spawn_site) {
    stopifnot(!is.null(ptagis_file))

    ptagis_obs = suppressMessages(read_csv(ptagis_file)) %>%
      janitor::clean_names()

    # determine format of event date time value
    n_colons = ptagis_obs %>%
      mutate(event_time = str_split(event_date_time_value, " ", simplify = T)[,2],
             n_colon = str_count(event_time, "\\:")) %>%
      pull(n_colon) %>%
      max()

    if(n_colons == 2) {
      ptagis_obs = ptagis_obs %>%
        mutate(across(c(event_date_time_value,
                        event_release_date_time_value),
                      lubridate::mdy_hms))
    } else if(n_colons == 1) {
      ptagis_obs = ptagis_obs %>%
        mutate(across(c(event_date_time_value,
                        event_release_date_time_value),
                      lubridate::mdy_hm))
    } else {
      warning("Event Date Time Value has strange format.")
    }

    spawn_loc = spawn_loc %>%
      left_join(ptagis_obs %>%
                  select(tag_code, event_type_name,
                         spawn_site = event_site_code_value,
                         matches('date_time_value')) %>%
                  tidyr::pivot_longer(cols = matches('date_time_value'),
                                      names_to = "name",
                                      values_to = "max_det") %>%
                  filter(!is.na(max_det)) %>%
                  select(-name)) %>%
      select(tag_code,
             starts_with("spawn"),
             everything())
  }

  return(spawn_loc)
}
