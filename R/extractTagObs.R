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
extractTagObs = function(ptagis_file = NULL) {

  stopifnot(!is.null(ptagis_file))

  observations = PITcleanr::readCTH(ptagis_file)

  tag_obs = PITcleanr::compress(observations,
                                configuration = PITcleanr::buildConfig() %>%
                                  dplyr::mutate(node = site_code)) %>%
    dplyr::group_by(tag_code) %>%
    dplyr::summarise(tag_detects = paste(node,
                                         collapse = " "))

  return(tag_obs)

}
