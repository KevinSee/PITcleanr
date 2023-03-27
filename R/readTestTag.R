#' @title Read In Test Tag Data
#'
#' @description The function reads in complete tag histories and filters out any tag not designated a
#' test tag by the tag code prefix.
#'
#' @author Kevin See
#'
#' @inheritParams readCTH
#'
#' @export
#' @return a tibble containing the raw detections of test tags
#' @examples
#' ptagis_file = system.file("extdata", "PRO_Steelhead_2019.csv", package = "PITcleanr")
#' readTestTag(ptagis_file)

readTestTag <- function(cth_file = NULL,
                        file_type = c("PTAGIS",
                                      "Biologic_csv",
                                      "raw"),
                        test_tag_prefix = "3E7") {


  observations <- readCTH(cth_file = cth_file,
                          file_type = file_type,
                          test_tag_prefix = NA)

  test_tag_obs <- observations |>
    dplyr::filter(stringr::str_detect(tag_code,
                                      paste0("^", test_tag_prefix)))

  return(test_tag_obs)

}
