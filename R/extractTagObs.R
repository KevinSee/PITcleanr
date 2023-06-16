#' @title Tag Detections
#'
#' @description List all detection sites for each tag code
#'
#' @author Kevin See
#'
#' @param obs_file Either a data.frame of compressed detections (output of `compress()`), or the complete tag history file path and name (same format to `cth_file` argument in `readCTH()` function).
#' @param obs_type what type of observation file is being fed in? `compressed` is the output of `compress()`, while `cth` is a raw complete tag history file path and name. Default is `compressed`.
#' @inheritParams readCTH
#' @import dplyr
#' @export
#' @return NULL
#'
extractTagObs = function(obs_file = NULL,
                         obs_type = c("compressed",
                                      "cth"),
                         file_type = c("PTAGIS",
                                       "Biologic_csv",
                                       "raw")) {

  stopifnot(!is.null(obs_file))

  obs_type = match.arg(obs_type)

  if(obs_type == "cth") {
    tag_obs = PITcleanr::compress(cth_file = obs_file,
                                  file_type = file_type) %>%
      dplyr::group_by(tag_code) %>%
      dplyr::summarise(tag_detects = paste(node,
                                           collapse = " "))
  } else {
    tag_obs <- obs_file %>%
      dplyr::group_by(tag_code) %>%
      dplyr::summarise(tag_detects = paste(node,
                                           collapse = " "))
  }

  return(tag_obs)

}
