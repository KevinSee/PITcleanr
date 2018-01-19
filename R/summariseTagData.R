#' @title Tag Summary
#'
#' @description For each tag, summarises final spawning location as well as important pieces of biological information.
#'
#' @author Kevin See
#'
#' @param capHist_proc Dataframe returned by \code{processCapHist_LGD}, under the name \code{ProcCapHist}.
#' @param trap_data Dataframe containing tag ID and relevant biological data from the adult trap. Part of the list returned by \code{processCapHist_LGD}, under the name \code{ValidTrapData}.
#' @param saveCSV should the resulting dataframe be saved as a csv file? Default value is \code{FALSE}.
#' @param file_name if \code{saveCSV} is \code{TRUE}, this is the file path and name, with .csv extension, where the file should be save.
#'
#' @import dplyr readr
#' @export
#' @return NULL
#' @examples summariseTagData()

summariseTagData = function(capHist_proc = NULL,
                            trap_data = NULL,
                            saveCSV = F,
                            file_name = NULL) {

  stopifnot(!is.null(capHist_proc))

  spwn_loc = estimateSpawnLoc(capHist_proc) %>%
    rename(LastObs = ObsDate,
           AssignSpawnSite = SiteID,
           AssignSpawnNode = Node)

  if('LGDNumPIT' %in% names(trap_data) ) {
    trap_data = trap_data %>%
      rename(TagID = LGDNumPIT)
  }

  tag_df = spwn_loc %>%
    full_join(trap_data)

  if(saveCSV) {
    readr::write_csv(tag_df,
                     file_name)
  }

  return(tag_df)
}
