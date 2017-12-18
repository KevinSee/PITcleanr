#' @title Capture History Output
#'
#' @description This function combines the results from \code{writeFishPaths} and \code{writeSpwnPaths} functions and saves it as an excel file if desired.
#'
#' @inheritParams writeFishPaths
#' @inheritParams writeSpwnPaths
#' @param save_file Should output be written to an Excel file? Default value is \code{FALSE}.
#'
#' @param file_name If \code{save_file == TRUE}, this is the file name (with possible extension) to be saved to.
#'
#' @author Kevin See
#' @import dplyr WriteXLS
#' @export

writeCapHistOutput = function(valid_obs = NULL,
                              valid_paths = NULL,
                              node_order = NULL,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(valid_obs),
            !is.null(valid_paths),
            !is.null(node_order))

  if(is.null(file_name) & save_file) {
    file_name = 'CapHistOutput.xlsx'
  }

  fish_paths = writeFishPaths(valid_obs,
                              valid_paths)

  spwn_paths = writeSpwnPaths(valid_obs,
                              valid_paths,
                              node_order)


  save_df = fish_paths %>%
    dplyr::rename(ObsDate = MinObsDate) %>%
    dplyr::full_join(spwn_paths %>%
                       select(TagID, TrapDate, ObsDate:SiteID, Node, SiteName, SiteDescription, NodeOrder:Migration)) %>%
    dplyr::arrange(TrapDate, TagID, ObsDate) %>%
    dplyr::select(TagID, TrapDate, ObsDate, SiteID, Node,
                  AutoProcStatus, UserProcStatus, ModelObs,
                  NodeOrder:ValidPath, maxUpDate, Migration,
                  SiteDescription, UserComment) %>%
    group_by(TagID) %>%
    mutate(UserProcStatus = ifelse(sum(!ModelObs) > 0, '', UserProcStatus)) %>%
    ungroup()

  if(save_file) {
    WriteXLS::WriteXLS('save_df',
                       file_name,
                       SheetNames = c('ProcCapHist'),
                       AdjWidth = T,
                       AutoFilter = T,
                       BoldHeaderRow = T,
                       FreezeCol = 1,
                       FreezeRow = 1)
  }

  return(save_df)
}
