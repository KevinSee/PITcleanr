#' @title Final location
#'
#' @description Based on cleaned capture history, determines furthest location of each tag along possible paths.
#'
#' @author Kevin See
#'
#' @param filtered_obs Tibble returned by `filterDetections()`.
#' @param incl_site_code Should the PTAGIS site code of final location be returned? Default is `FALSE`.
#' @inheritParams compress
#'
#'
#' @import dplyr tidyr
#' @export
#' @return NULL
#' @examples #estimateFinalLoc()

estimateFinalLoc = function(filtered_obs = NULL,
                            incl_site_code = F,
                            cth_file = NULL,
                            file_type = c("PTAGIS",
                                          "Biologic_csv",
                                          "raw")) {

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
  final_loc = filtered_obs %>%
    filter(user_keep_obs) %>%
    group_by(tag_code) %>%
    filter(node_order == max(node_order)) %>%
    filter(min_det == max(min_det)) %>%
    slice(1) %>%
    ungroup() %>%
    select(tag_code,
           final_node = node,
           event_type_name,
           min_det,
           max_det) %>%
    left_join(filtered_obs %>%
                filter(user_keep_obs) %>%
                group_by(tag_code) %>%
                summarize(tag_detects = toString(node),
                          .groups = "drop") %>%
                distinct(),
              by = "tag_code") %>%
    arrange(tag_code)

  if(incl_site_code) {
    stopifnot(!is.null(cth_file))

    observations = readCTH(cth_file,
                           file_type = file_type)

    final_loc = final_loc %>%
      left_join(observations %>%
                  select(tag_code, event_type_name,
                         final_site = event_site_code_value,
                         matches('date_time_value')) %>%
                  tidyr::pivot_longer(cols = matches('date_time_value'),
                                      names_to = "name",
                                      values_to = "max_det") %>%
                  filter(!is.na(max_det)) %>%
                  select(-name),
                by = c("tag_code", "event_type_name", "max_det")) %>%
      select(tag_code,
             starts_with("final"),
             everything())
  }

  return(final_loc)
}
