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

  if(sum(capHist_proc$UserProcStatus == '') > 0) {
    stop('UserProcStatus must be defined for each observation.')
  }

  # # if the user processed column is blank, use the auto processed column
  # capHist_proc = capHist_proc %>%
  #   mutate(UserProcStatus = ifelse(UserProcStatus == '',
  #                                  AutoProcStatus,
  #                                  UserProcStatus),
  #          UserProcStatus = as.logical(UserProcStatus))

  # filter for observations that should be kept
  capHist_proc = capHist_proc %>%
    filter(UserProcStatus)

  # create tag_path field for each tag
  tag_path <- capHist_proc %>%
    select(TagID, Node) %>%
    group_by(TagID) %>%
    summarise(TagPath = toString(Node)) %>%
    ungroup()

  finalLoc = capHist_proc %>%
    group_by(TagID) %>%
    filter(NodeOrder == max(NodeOrder)) %>%
    slice(1) %>%
    ungroup() %>%
    select(TagID,
           LastObs = lastObsDate,
           BranchNum,
           Group,
           AssignSpawnSite = SiteID,
           AssignSpawnNode = Node) %>%
    full_join(tag_path)

  return(finalLoc)

}
