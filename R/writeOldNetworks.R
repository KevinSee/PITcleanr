#' @title Old Node Networks
#'
#' @description Calls old functions to create data.frames describing all the observation nodes in various versions of DABOM, including how they are related to one another. Kept for backwards compatibility checks.
#'
#' @author Kevin See
#'
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples writeLGRNodeNetwork()

writeOldNetworks = function() {

  list("LowerGranite" = writeLGRNodeNetwork(),
       "PriestRapids" = writePRDNodeNetwork(),
       "Prosser" = writePRONodeNetwork(),
       "Tumwater" = writeTUMNodeNetwork(),
       "Tumwater_noUWE" = writeTUMNodeNetwork_noUWE())
}
