#' @title Final spawning location
#'
#' @description Based on cleaned capture history, determines further upstream location of each tag. Assumption is that this is the spawning location.
#'
#' @author Kevin See
#'
#' @param capHist_proc Dataframe returned by \code{processCapHist_LGD}, under the name \code{ProcCapHist}.
#'
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples estimateSpawnLoc()

estimateSpawnLoc = function(capHist_proc = NULL) {

  stopifnot(!is.null(capHist_proc))

  # if the user processed column is blank, use the auto processed column
  capHist_proc = capHist_proc %>%
    dplyr::mutate(UserProcStatus = ifelse(UserProcStatus == '',
                                          AutoProcStatus,
                                          UserProcStatus))

  finalLoc = capHist_proc %>%
    dplyr::group_by(TagID) %>%
    dplyr::filter(NodeOrder == max(NodeOrder)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(TagID, ObsDate, SiteID, Node)

  return(finalLoc)

}
