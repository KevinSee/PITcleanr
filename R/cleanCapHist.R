#' @title Remove Duplicate Detections
#'
#' @description Simplify cature histories and remove multiple sequential detections at same node
#'
#' @author Kevin See
#'
#' @param ch capture history file
#' @param config configuration file
#' @param drop_nodes which nodes should be dropped from capture histories?
#'
#'
#' @import dplyr
#' @export
#' @return NULL

cleanCapHist = function(ch,
                        config,
                        drop_nodes = NULL) {

  cleanHist = ch %>%
    left_join(config %>%
                mutate(nodeID = as.numeric(node)) %>%
                select(site, EventSiteCodeValue, node, nodeID, AntennaID)) %>%
    select(TagCode, node, nodeID, EventDateMMDDYYYY) %>%
    filter(!node %in% drop_nodes) %>%
    distinct() %>%
    arrange(TagCode, EventDateMMDDYYYY) %>%
    group_by(TagCode) %>%
    mutate(node_diff = c(NA, diff(nodeID))) %>%
    ungroup() %>%
    filter(node_diff != 0 | is.na(node_diff)) %>%
    select(-node_diff, -nodeID)

  return(cleanHist)
}
