#' @title Assign Observation Nodes
#'
#' @description The function assigns PIT-tag observation node site names to each
#' tag detection history record generated from a PTAGIS 'complete tag history query'.  The
#' complete tag history query is completed by running the tag list outputted from the
#' validTagList() function. Observation node site names are assigned from joining a configuration
#' file with the PTAGIS query results on 'Site_Code' and 'AntennaID' fields. The configuration
#' file, 'siteDescription.csv', is distributed and maintained within the DABOM package.
#'
#' An observation node is a single or group of PIT-tag antenna that, at the finest resolution,
#' act as a unique tag detection location.  Often, a node consists of multiple antennas at a unique
#' PTAGIS interogation site, which together form a single array.  In addition, multiple nodes
#' may exist at one PTAGIS interogation site or mark-release-recapture (MRR) site when more than
#' antenna array are assigned to the same SiteID. A node may also be a single antenna or coil
#' located in an adult ladder or trap entry or the single (MRR) SiteID for fish handled and
#' scanned at a weir location.
#'
#' @author Kevin See
#'
#' @param valid_tag_df is a data frame containing the PIT tag ID in a column called \code{TagID}, and the date that fish was caught at the trap, \code{TrapDate}.
#'
#' @param observation is the PTAGIS observation file inputted as a data frame containing the complete tag history for each of the tagIDs in valid_tags
#'
#' @param configuration is a data frame which assigns node names to unique SiteID, AntennaID, and site configuration ID combinations. It can be built with the function \code{buildConfig}.
#'
#' @param parent_child_df is a data frame created by \code{createParentChildDf}.
#'
#' @param truncate logical, subsets observations to those with valid nodes, observations dates greater than trapping date at LGD and then to the minimum observation date of each set of observation events at a node, multiple observation events can occur at one node if the observations are split by detections at other nodes. Default is \code{TRUE}.
#'
#' @import dplyr lubridate
#' @export
#' @return NULL
#' @examples assignNodes()

assignNodes = function(valid_tag_df = NULL,
                       observation = NULL,
                       configuration = NULL,
                       parent_child_df = NULL,
                       truncate = T) {

  stopifnot(!is.null(valid_tag_df) |
              !is.null(observation) |
              !is.null(parent_child_df))

  if(is.null(configuration)) {
    print('Building configuration file')
    configuration = buildConfig()
  }

  if(!'Event Release Date Time Value' %in% names(observation)) {
    observation$`Event Release Date Time Value` = NA
  }

  obs_df <- valid_tag_df %>%
    dplyr::select(TagID, TrapDate) %>%
    dplyr::left_join(observation %>%
                       dplyr::left_join(configuration %>%
                                          dplyr::select(SiteID, SiteType, SiteTypeName) %>%
                                          dplyr::distinct(),
                                        by = c('Event Site Code Value' = 'SiteID')) %>%
                       dplyr::mutate(ObsDate = ifelse(is.na(`Event Release Date Time Value`) &
                                                        is.na(`Antenna ID`) &
                                                        SiteType == 'MRR' &
                                                        SiteTypeName %in% c('Acclimation Pont', 'Hatchery', 'Hatchery Returns', 'Trap or Weir'),
                                                      `Event Release Date Time Value`,
                                                      `Event Date Time Value`)) %>%
                       dplyr::select(TagID = `Tag Code`,
                                     ObsDate,
                                     SiteID = `Event Site Code Value`,
                                     AntennaID = `Antenna ID`,
                                     ConfigID = `Antenna Group Configuration Value`) %>%
                       dplyr::mutate(ObsDate = lubridate::mdy_hms(ObsDate)),
                     by = c('TagID')) %>%
    dplyr::mutate(ValidDate = ifelse(ObsDate >= TrapDate, T, F)) %>%
    dplyr::filter(!is.na(SiteID))

  # which sites are not in the configuration file
  tmp_df <- obs_df %>%
    dplyr::select(SiteID, AntennaID, ConfigID) %>%
    dplyr::distinct() %>%
    dplyr::anti_join(configuration %>%
                       dplyr::select(SiteID, AntennaID, ConfigID) %>%
                       dplyr::distinct())

  if( nrow(tmp_df) > 0 ){

    cat( "The following SiteID - AntennaID - ConfigID combinations are in the observation file
         but not listed in the site configuration file.\n")

    for( i in 1: nrow(tmp_df) ){

      print( paste0(tmp_df$SiteID[i], " - ", tmp_df$AntennaID[i], " - ", tmp_df$ConfigID[i]))
    }

    cat("Observation records with these combinations are flagged with an 'ERROR' in the Node field\n")
  }

  obs_dat <- obs_df %>%
    dplyr::left_join(configuration %>%
                       dplyr::select(SiteID,
                                     AntennaID,
                                     ConfigID,
                                     Node,
                                     ValidNode,
                                     AntennaGroup,
                                     SiteName,
                                     SiteDescription),
                     by = c('SiteID', 'AntennaID', 'ConfigID')) %>%
    dplyr::mutate(Node = ifelse(Node %in% union(unique(parent_child_df$ParentNode), unique(parent_child_df$ChildNode)), Node, NA),
                  Node = ifelse(is.na(Node), 'ERROR', Node),
                  ValidNode = ifelse(Node == 'ERROR', F, T)) %>%
    dplyr::arrange(TagID, ObsDate)

  if(truncate){

    obs_dat = obs_dat %>%
      dplyr::filter(ValidDate == TRUE,
                    ValidNode == TRUE) %>%
      dplyr::group_by(TagID) %>%
      dplyr::mutate(prev_node = lag(Node)) %>%
      dplyr::filter(Node != prev_node | is.na(prev_node)) %>%
      dplyr::select(-prev_node) %>%
      dplyr::ungroup()

  } # truncate if statement

  return(obs_dat)

}
