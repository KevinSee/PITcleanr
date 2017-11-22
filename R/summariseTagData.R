#' @title Tag Summary
#'
#' @description For each tag, summarises final spawning location as well as important pieces of biological information.
#'
#' @author Kevin See
#'
#' @param proc_list list returned by \code{processCapHist_LGD}.
#' @param saveCSV should the resulting dataframe be saved as a csv file? Default value is \code{FALSE}.
#' @param file_name if \code{saveCSV} is \code{TRUE}, this is the file path and name, with .csv extension, where the file should be save.
#'
#' @import dplyr readr
#' @export
#' @return NULL
#' @examples summariseTagData()

summariseTagData = function(proc_list = NULL,
                            saveCSV = F,
                            file_name = NULL) {

  stopifnot(!is.null(proc_list))

  trap_data = proc_list$ValidTrapData

  spwn_loc = estimateSpawnLoc(proc_list$ProcCapHist) %>%
    dplyr::rename(LastObs = ObsDate,
                  LastObsSite = SiteID,
                  LastObsNode = Node)

  tag_df = spwn_loc %>%
    dplyr::full_join(trap_data,
                     by = c('TagID' = 'LGDNumPIT'))

  if(saveCSV) {
    readr::write_csv(tag_df,
                     file_name)
  }

  return(tag_df)
}
