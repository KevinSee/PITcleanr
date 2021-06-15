#' @title Add Direction
#'
#' @description Based on the compressed PIT tag observations, and a
#' parent-child table, determines the direction of movement leading to
#' each detection.
#'
#' @author Kevin See
#'
#' @param compress_obs The result of `compress()`.
#' @inheritParams buildPaths
#'
#' @import dplyr
#' @export
#' @return a tibble
#' @examples addDirection()

addDirection = function(compress_obs = NULL,
                        parent_child = NULL) {

  stopifnot(!is.null(compress_obs),
            !is.null(parent_child))

  node_order = try(buildNodeOrder(parent_child = parent_child))

  if(class(node_order)[1] == "try-error") {
    stop("Something went wrong building the node order")
  }

  # which observation locations are not in node_order?
  dropped_locs = compress_obs %>%
    left_join(node_order,
              by = "node") %>%
    filter(is.na(node_order)) %>%
    pull(node) %>%
    unique()

  paste("Detections from the following nodes were dropped,
        because they were not in the parent-child table:\n",
        paste(dropped_locs, collapse = ", "), "\n") %>%
    message()

  # filter out observations at sites not included in the node order
  # determine direction of movement
  obs_direct = compress_obs %>%
    left_join(node_order,
              by = "node") %>%
    filter(!is.na(node_order)) %>%
    arrange(tag_code, slot) %>%
    group_by(tag_code) %>%
    mutate(lead_node_order = lead(node_order),
           lag_node_order = lag(node_order),
           lag_node = lag(node),
           lead_node = lead(node),
           lead_path = lead(path),
           lag_path = lag(path)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(direction = if_else(node_order == 1 & is.na(lag_node_order),
                               "start",
                               if_else(node_order > lag_node_order &
                                         (stringr::str_detect(path, paste0(" ", lag_node)) |
                                            stringr::str_detect(path, paste0("^", lag_node))),
                                       "forward",
                                       if_else(node_order < lag_node_order &
                                                 (stringr::str_detect(lag_path, paste0(" ", node)) |
                                                    stringr::str_detect(lag_path, paste0("^", node))),
                                               "backward",
                                               if_else(node == lag_node,
                                                       "no movement",
                                                       "unknown"))))) %>%
    ungroup() %>%
    select(-starts_with(c("lead", "lag")))

  return(obs_direct)
}
