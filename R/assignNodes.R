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

  obs_df <- valid_tag_df %>%
    select(TagID, TrapDate) %>%
    left_join(observation %>%
                select(TagID = `Tag Code`,
                       ObsDate = `Event Date Time Value`,
                       SiteID = `Event Site Code Value`,
                       AntennaID = `Antenna ID`,
                       ConfigID = `Antenna Group Configuration Value`) %>%
                mutate(ObsDate = mdy_hms(ObsDate)),
              by = c('TagID')) %>%
    mutate(ValidDate = ifelse(ObsDate >= TrapDate, T, F))

  # which sites are not in the configuration file
  tmp_df <- obs_df %>%
    select(SiteID, AntennaID, ConfigID) %>%
    distinct() %>%
    anti_join(configuration %>%
                select(SiteID, AntennaID, ConfigID) %>%
                distinct())

  if( nrow(tmp_df) > 0 ){

    cat( "The following SiteID - AntennaID - ConfigID combinations are in the observation file
         but not listed in the site configuration file.\n")

    for( i in 1: nrow(tmp_df) ){

      print( paste0(tmp_df$SiteID[i], " - ", tmp_df$AntennaID[i], " - ", tmp_df$ConfigID[i]))
    }

    cat("Observation records with these combinations are flagged with an 'ERROR' in the Node field")
  }

  obs_dat <- obs_df %>%
    left_join(configuration %>%
                select(SiteID,
                       AntennaID,
                       ConfigID,
                       Node,
                       ValidNode,
                       AntennaGroup,
                       # ModelMainBranch,
                       SiteName,
                       SiteDescription), #%>%
                # filter(Node %in% union(unique(parent_child_df$ParentNode), unique(parent_child_df$ChildNode))),
              by = c('SiteID', 'AntennaID', 'ConfigID')) %>%
    mutate(Node = ifelse(Node %in% union(unique(parent_child_df$ParentNode), unique(parent_child_df$ChildNode)), Node, NA),
           Node = ifelse(is.na(Node), 'ERROR', Node),
           ValidNode = ifelse(Node == 'ERROR', F, T)) %>%
    arrange(TagID, ObsDate)

  if(truncate){

    obs_dat = obs_dat %>%
      filter(ValidDate == TRUE,
             ValidNode == TRUE) %>%
      group_by(TagID) %>%
      mutate(prev_node = lag(Node)) %>%
      filter(Node != prev_node | is.na(prev_node)) %>%
      select(-prev_node) %>%
      ungroup()

  } # truncate if statement

  return(obs_dat)

}
