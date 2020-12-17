#' @title Capture History Output
#'
#' @description This function combines the results from \code{writeFishPaths} and \code{writeSpwnPaths} functions and saves it as a csv, excel or delimited file if desired.
#'
#' @inheritParams writeFishPaths
#' @inheritParams writeSpwnPaths
#' @param last_obs_date Character string in the format "YYYYMMDD". If included, the output will suggest that observations after this date should be deleted.
#'
#' @param save_file Should output be written to an file? Default value is \code{FALSE}.
#'
#' @param file_name If \code{save_file == TRUE}, this is the file name (with possible extension) to be saved to.
#'
#' @author Kevin See
#' @import dplyr readr openxlsx lubridate
#' @importFrom tidyr replace_na
#' @export

writeCapHistOutput = function(valid_obs = NULL,
                              valid_paths = NULL,
                              node_order = NULL,
                              last_obs_date = NULL,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(valid_obs),
            !is.null(valid_paths),
            !is.null(node_order))

  if(!is.null(file_name) ) {
    if( !(require('WriteXLS', quietly = T)) & grepl('xls', file_name) ) {
      stop("Package \"WriteXLS\" needed to save an Excel file. Please install it, or save as .csv file instead.",
           call. = FALSE)
    }
  }

  if(is.null(file_name) & save_file) {
    file_name = 'CapHistOutput.csv'
  }

  fish_paths = writeFishPaths(valid_obs = valid_obs,
                              valid_paths = valid_paths)

  spwn_paths = writeSpwnPaths(valid_obs = valid_obs,
                              valid_paths = valid_paths,
                              node_order = node_order)


  save_df = fish_paths %>%
    rename(ObsDate = MinObsDate) %>%
    full_join(spwn_paths %>%
                select(TagID, TrapDate, ObsDate:SiteID, BranchNum, Group, Node, SiteName, SiteDescription, NodeOrder:Migration),
              by = c('TagID', 'ObsDate', 'Node')) %>%
    arrange(TrapDate, TagID, ObsDate) %>%
    select(TagID, TrapDate, ObsDate, lastObsDate,
           BranchNum, Group, SiteID, Node, NodeOrder, Direction, Migration, AutoProcStatus, UserProcStatus, ModelObs, ValidPath, UserComment) #%>%
    # group_by(TagID) %>%
    # mutate(UserProcStatus = ifelse(sum(!AutoProcStatus) > 0, '', UserProcStatus)) %>%
    # # mutate(UserProcStatus = ifelse(sum(!ModelObs) > 0, '', UserProcStatus)) %>%
    # ungroup()

  if(!is.null(last_obs_date)) {
    save_df = save_df %>%
      # mutate(across(UserProcStatus,
      #               as.logical)) %>%
      mutate_at(vars(AutoProcStatus, UserProcStatus),
                list(~ if_else(ObsDate > lubridate::ymd(last_obs_date),
                               F, .)))
  }

  # make all the NAs in UserProcStatus blanks, to encourage someone to fill them in
  save_df = save_df %>%
    mutate(across(UserProcStatus,
                  tidyr::replace_na,
                  replace = ''))


  if(save_file) {
    if(grepl('\\.xls', file_name)) {
      # WriteXLS::WriteXLS(save_df,
      #                    file_name,
      #                    SheetNames = c('ProcCapHist'),
      #                    AdjWidth = T,
      #                    AutoFilter = T,
      #                    BoldHeaderRow = T,
      #                    FreezeCol = 1,
      #                    FreezeRow = 1)

      hs <- openxlsx::createStyle(textDecoration = "BOLD")
      sht_nm = 'ProcCapHist'
      wb = openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb,
                             sheetName = sht_nm)
      openxlsx::writeData(wb,
                          sheet = sht_nm,
                          x = save_df,
                          withFilter = T,
                          headerStyle = hs)
      openxlsx::freezePane(wb,
                           sheet = sht_nm,
                           firstRow = T,
                           firstCol = T)
      setColWidths(wb,
                   sheet = sht_nm,
                   cols=1:ncol(save_df),
                   widths = "auto")
      openxlsx::setColWidths(wb,
                             sheet = sht_nm,
                             cols = c(grep("TagID", names(save_df)),
                                      grep("Date", names(save_df)),
                                      grep('Comment', names(save_df))),
                             widths = c(15, rep(20, sum(grepl("Date", names(save_df)))), 100))
      openxlsx::saveWorkbook(wb,
                             file = file_name,
                             overwrite = T)

    }

    if(grepl('\\.csv', file_name)) {
      write_csv(save_df,
                file_name)
    }

    if(grepl('\\.txt', file_name)) {
      write_delim(save_df,
                  file_name)
    }
  }

  return(save_df)
}
