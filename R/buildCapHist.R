#' @title Build capture history
#'
#' @description Based on a compressed and filtered capture history, translate into a capture history matrix.
#'
#' @author Kevin See
#'
#'
#' @param filter_ch filtered capture history as returned by the `filterDetections()` function, which has then been verified by a user and all blank or NA `user_keep_obs` entries have been completed.
#' @param keep_cols what columns from `filter_ch` should be kept in the final output. These should be columns that are consistent for each tag. Default value is `tag_code`.
#' @param drop_nodes After constructing a capture history with one row per tag, should the various columns containing specific node detections be dropped (`TRUE`) or retained (`FALSE`).
#' @param ... other inputs to the `defineCapHistCols` if desired.
#'
#' @inheritParams defineCapHistCols
#'
#' @import dplyr stringr tidyr
#' @export
#' @return A tibble or data.frame with one row per tag, containing at least a column called `cap_hist` where each entry is a string of 0s and 1s corresponding to whether that tag was detected at each node or not. The order of the columns is determined by `defineCapHistCols`, and the user can input arguments to that function here.
#' @examples buildConfig()
#'
buildCapHist = function(filter_ch = NULL,
                        parent_child = NULL,
                        configuration = NULL,
                        keep_cols = c("tag_code"),
                        drop_nodes = T,
                        ...) {

  stopifnot(exprs = {
    !is.null(filter_ch)
    !is.null(parent_child)
    !is.null(configuration)
  })

  # drop any columns to keep that aren't part of filter_ch
  if(sum(!keep_cols %in% names(filter_ch)) > 0) {
    keep_cols <- keep_cols[keep_cols %in% names(filter_ch)]
  }


  # define the columns of the capture histories
  col_nms <- defineCapHistCols(parent_child = parent_child,
                               configuration = configuration,
                               ...)

  # create capture histories
  # include nodes that had no observations, to ensure all columns are included
  cap_hist_df <-
    filter_ch %>%
    dplyr::mutate(across(user_keep_obs,
                         ~ if_else(is.na(.) | . == "",
                                   auto_keep_obs,
                                   .))) |>
    dplyr::filter(user_keep_obs) |>
    dplyr::select(any_of(keep_cols),
                  node) %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(node,
                         ~ factor(., levels = col_nms))) %>%
    dplyr::mutate(seen = 1) %>%
    dplyr::group_by(tag_code,
    # dplyr::group_by({{ keep_cols }},
                    node) %>%
    dplyr::summarise(across(seen,
                            sum),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "node",
                       names_sort = T,
                       names_expand = T,
                       values_from = "seen",
                       values_fill = 0) |>
    tidyr::unite(col = "cap_hist",
                 -any_of(keep_cols),
                 sep = "",
                 remove = drop_nodes)

  return(cap_hist_df)
}
