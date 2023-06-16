#' @title Tag Detections
#'
#' @description List all detection sites for each tag code
#'
#' @author Kevin See
#'
#'
#' @inheritParams readCTH
#' @import dplyr
#' @export
#' @return NULL
#'
extractTagObs = function(cth_file = NULL,
                         file_type = c("PTAGIS",
                                       "Biologic_csv",
                                       "raw")) {

  stopifnot(!is.null(cth_file))

  tag_obs = PITcleanr::compress(cth_file = cth_file,
                                file_type = file_type) %>%
    dplyr::group_by(tag_code) %>%
    dplyr::summarise(tag_detects = paste(node,
                                         collapse = " "))


  return(tag_obs)

}
