#' @title Fish Paths
#'
#' @description The function builds a data frame of all the observed fish paths in the
#' observation file based on the parent child table. The function was originally called
#' FishPath_truncateToLastLeg() and was built by Greg K. Ryan K. altered the function
#' to work solely in R and doesn't require the SQLite backend, and called in fishPaths(). Kevin S. edited it further
#'
#' @param valid_obs dataframe built by the function \code{assignNodes}.
#'
#' @param valid_paths dataframe built by the function \code{getValidPaths}
#'
#' @author Greg Kliewer, Ryan Kinzer, Kevin See
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples writeFishPaths()

writeFishPaths = function(valid_obs,
                          valid_paths) {

  tags <- distinct(valid_obs, TagID) %>%
    as.matrix() %>%
    as.character()

  print(paste0(Sys.time(), "  ", 1, " of ", length(tags), " tags. Current tag: ",tags[1]))

  # cycle over each tag
  alltagObs <- NULL
  for (tag in tags) {
    if( which(tags == tag) %% 500 == 0){ print(paste0(Sys.time(), "  ", which(tags == tag), " of ", length(tags), " tags. Current tag: ",tag))  }

    tagObs <- valid_obs %>%
      filter(TagID == tag) %>%
      select(TagID, Node, ObsDate) %>%
      arrange(ObsDate)

    finalNode = tagObs %>%
      filter(ObsDate == max(ObsDate)) %>%
      select(Node) %>%
      slice(n()) %>%
      as.matrix() %>%
      as.character()


    pathNodes <- valid_paths %>%
      filter(Node == finalNode) %>%
      select(Path) %>%
      as.matrix() %>%
      as.character() %>%
      stringr::str_split(' ') %>%
      unlist()

    tagObs = tagObs %>%
      mutate(InMainPath = ifelse(Node %in% pathNodes, T, F))

    # check out observed nodes not in main path
    # determine if they are in an extended path
    if(sum(!tagObs$InMainPath) > 0) {

      lastPath = valid_paths %>%
        filter(Node == finalNode) %>%
        select(Path) %>%
        as.matrix() %>%
        as.character() %>%
        stringr::str_trim()

      quesObs = tagObs %>%
        filter(!InMainPath) %>%
        select(Node)

      extendedPaths = valid_paths %>%
        filter(grepl(finalNode, Path)) %>%
        filter(Node %in% quesObs$Node)

      tagObs$InExtendedPath = F

      for(i in rev(which(!tagObs$InMainPath))) {

        obsNode = tagObs$Node[i]

        tmp = valid_paths %>%
          filter(Node == obsNode)

        if(grepl(lastPath, tmp$Path) | grepl(tmp$Path, lastPath)) {
          tagObs$InExtendedPath[i] = T
          lastPath = tmp$Path
        }
        rm(obsNode, tmp)
      }

    }

    alltagObs = alltagObs %>%
      bind_rows(tagObs)

  }

  proc_obs <- alltagObs %>%
    mutate(AutoProcStatus = ifelse(InMainPath, TRUE,
                                   ifelse(InExtendedPath, TRUE, FALSE)))

  proc_obs <- proc_obs %>%
    # left_join(proc_obs %>%
    #             filter(AutoProcStatus == FALSE) %>%
    #             distinct(TagID) %>%
    #             mutate(UserProcStatus = ''),
    #           by = 'TagID') %>%
    # mutate(UserProcStatus = ifelse(is.na(UserProcStatus), TRUE, ''),
    #        UserComment = '') %>%
    group_by(TagID) %>%
    mutate(UserProcStatus = if_else(sum(!AutoProcStatus) > 0,
                                    NA,
                                    AutoProcStatus)) %>%
    mutate(UserComment = '') %>%
    ungroup() %>%
    select(TagID, MinObsDate = ObsDate, Node, AutoProcStatus, UserProcStatus, UserComment)

  return(proc_obs)

}
