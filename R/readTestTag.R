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
#' ptagis_file = system.file("extdata", "PRO_sthd_cth_2012.csv", package = "PITcleanr")
#' readTestTag(ptagis_file)

readTestTag <- function(cth_file = NULL,
                        file_type = c("PTAGIS",
                                      "Biologic_csv",
                                      "raw"),
                        test_tag_prefix = "3E7") {


  observations <- readCTH(cth_file = cth_file,
                          file_type = file_type,
                          test_tag_prefix = NA)

  # collect all test tag codes
  for(i in 1:length(test_tag_prefix)) {
    if(i == 1) {
      test_tags <- observations %>%
        dplyr::filter(stringr::str_detect(tag_code,
                                          paste0("^", test_tag_prefix[i]))) %>%
        dplyr::pull(tag_code) %>%
        unique()
    } else {
      test_tags <- c(test_tags,
                     observations %>%
                       dplyr::filter(stringr::str_detect(tag_code,
                                                         paste0("^", test_tag_prefix[i]))) %>%
                       dplyr::pull(tag_code) %>%
                       unique())
    }
  }


  test_tag_obs <- observations %>%
    dplyr::filter(tag_code %in% test_tags)

  return(test_tag_obs)

}
