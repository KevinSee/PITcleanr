#' @title Spawning Paths
#'
#' @description The function builds a data frame of all the observed fish paths in the
#' observation file based on the parent child table. It works by checking node observations
#' versus the previous node observation to see if the previous node was a parent or grandparent
#' great grand-parent..... The function was originally built by Ryan K. (called spawnerPaths). Kevin S. edited it further
#'
#' @param valid_obs dataframe built by the function \code{assignNodes}.
#'
#' @param valid_paths dataframe built by the function \code{getValidPaths}.
#'
#' @param node_order dataframe built by the function \code{createNodeOrder}.
#'
#' @author Greg Kliewer, Ryan Kinzer, Kevin See
#' @import dplyr purrr
#' @export
#' @return NULL
#' @examples writeSpwnPaths()

writeSpwnPaths = function(valid_obs,
                          valid_paths,
                          node_order) {

  # node_order = valid_paths %>%
  #   split(list(.$Node)) %>%
  #   purrr::map_df(.id = 'EndNode',
  #                 .f = function(x) {
  #                   tibble(Node = str_split(x$Path, ' ')[[1]])
  #
  #                 }) %>%
  #   dplyr::filter(Node %in% unique(parent_child$ChildNode[!is.na(parent_child$SiteType)])) %>%
  #   dplyr::group_by(EndNode) %>%
  #   dplyr::mutate(NodeOrder = 1:n()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(Node, NodeOrder) %>%
  #   dplyr::distinct(Node, .keep_all=TRUE) %>%
  #   dplyr::full_join(valid_paths,
  #                    by = c('Node')) %>%
  #   dplyr::filter(!is.na(Path))


  allObs = valid_obs %>%
    dplyr::left_join(node_order %>%
                       group_by(Node) %>%
                       slice(1) %>%
                       ungroup(),
                     by = c('Node')) %>%
    dplyr::group_by(TagID) %>%
    dplyr::mutate(previous_node = lag(Node),
                  next_node = lead(Node),
                  prev_string = lag(Path),
                  next_node = ifelse(is.na(next_node), Node, next_node)) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Up = ifelse(is.na(previous_node), 'Up',
                              ifelse(grepl(previous_node, Path),'Up', NA)),
                  Down = ifelse(grepl(Node, prev_string),'Down', NA ),
                  Hold = ifelse(Node == previous_node, 'Hold', NA),
                  Direction = ifelse(!is.na(Up), Up, Down),
                  Direction = ifelse(!is.na(Hold), 'Hold', Direction)) %>%
    dplyr::ungroup() %>%
    dplyr::select(one_of(names(valid_obs)), NodeOrder, Direction)

  # identical(nrow(valid_obs), nrow(allObs))

  naDir_tags = allObs %>%
    dplyr::filter(is.na(Direction)) %>%
    dplyr::select(TagID) %>%
    dplyr::distinct() %>%
    as.matrix() %>%
    as.character()

  allObs = allObs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ValidPath = ifelse(TagID %in% naDir_tags, F, T)) %>%
    dplyr::ungroup()

  modObs <- allObs %>%
    # filter(TagID == '3D9.1C2DDC96C9') %>%
    dplyr::filter(Direction == 'Up') %>%
    dplyr::group_by(TagID) %>%
    dplyr::distinct(Node, .keep_all=TRUE) %>%
    dplyr::slice(1:which.max(NodeOrder)) %>%
    dplyr::mutate(ModelObs = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(TagID, Node, ObsDate, ModelObs)

  allObs = allObs %>%
    dplyr::left_join(modObs,
                     by = c('TagID', 'Node', 'ObsDate')) %>%
    dplyr::mutate(ModelObs = ifelse(is.na(ModelObs), F, ModelObs))

  # RK added this for migration direction
  migObs <- allObs %>%
    filter(Direction == 'Up') %>%
    group_by(TagID) %>%
    slice(which.max(ObsDate)) %>%
    select(TagID, maxUpDate = ObsDate)

  allObs <- allObs %>%
    left_join(migObs) %>%
    mutate(Migration = ifelse(ObsDate <= maxUpDate, 'Upstream', 'Downstream'))


  return(allObs)
}
