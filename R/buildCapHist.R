#' @title Build capture history
#'
#' @description Based on a compressed and filtered capture history, translate into a capture history matrix.
#'
#' @author Kevin See
#'
#'
#' @param filter_ch filtered capture history as returned by the `filterDetections()` function, which has then been verified by a user and all blank or NA `user_keep_obs` entries have been completed.
#' @param ... other inputs to the `defineCapHistCols` if desired.
#'
#' @inheritParams addParentChildNode
#'
#'
#'
#' @import dplyr stringr tidyr
#' @export
#' @return NULL
#' @examples buildConfig()
#'
buildCapHist = function(filter_ch = NULL,
                        parent_child = NULL,
                        configuration = NULL,
                        ...) {

  stopifnot(exprs = {
    !is.null(filter_ch)
    !is.null(parent_child)
    !is.null(configuration)
  })


  col_nms <- defineCapHistCols(parent_child = parent_child,
                               configuration = configuration,
                               ...)


  # include nodes that had no observations, to match the indexing in the DABOM JAGS model
  cap_hist_df <-
    filter_ch %>%
    dplyr::mutate(across(user_keep_obs,
                         ~ if_else(is.na(.) | . == "",
                                   auto_keep_obs,
                                   .))) |>
    dplyr::filter(user_keep_obs) |>
    dplyr::select(tag_code, node) %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(node,
                  ~ factor(., levels = col_nms))) %>%
    dplyr::mutate(seen = 1) %>%
    tidyr::pivot_wider(names_from = "node",
                       names_sort = T,
                       names_expand = T,
                       values_from = "seen",
                       values_fill = 0)

  return(cap_hist_df)
}
