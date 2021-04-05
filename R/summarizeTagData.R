#' @title Tag Summary
#'
#' @description For each tag, summarises final spawning location as well as important pieces of biological information.
#'
#' @author Kevin See
#'
#' @inheritParams estimateSpawnLoc
#' @param bio_data Dataframe containing `tag_code` and relevant biological data from each fish, often gathered when they were tagged.
#' @param save_csv should the resulting dataframe be saved as a csv file? Default value is `FALSE`.
#' @param file_name if `save_csv` is `TRUE`, this is the file path and name, with .csv extension, where the file should be save.
#'
#' @import dplyr readr
#' @export
#' @return NULL
#' @examples summarizeTagData()

summarizeTagData = function(filtered_obs = NULL,
                            bio_data = NULL,
                            saveCSV = F,
                            file_name = NULL,
                            ...) {

  stopifnot(!is.null(filtered_obs))

  tag_summ = estimateSpawnLoc(filtered_obs,
                              ...)

  if(!is.null(bio_data)) {
    tag_summ = tag_summ %>%
    full_join(bio_data)
  }

  if(saveCSV) {
    readr::write_csv(tag_summ,
                     file_name)
  }

  return(tag_summ)
}
