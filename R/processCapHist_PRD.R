#' @title Process capture histories for Priest Rapids
#'
#' @description Using a site/node configuration file, a list of modeled sites, a parent-child table of nodes, and the raw capture histories from PTAGIS, process them. This function is specific to fish tagged at Priest Rapids dam.
#'
#' @author Kevin See
#'
#' @param startDate observations that occurred before this date (YYYYMMDD format) will not be included.
#' @inheritParams getValidPaths
#' @inheritParams assignNodes
#' @inheritParams writeCapHistOutput
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples processCapHist_PRD()

processCapHist_PRD = function(startDate = NULL,
                              configuration = NULL,
                              parent_child = NULL,
                              observations = NULL,
                              truncate = T,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(startDate) |
              !is.null(parent_child) |
              !is.null(observations))

  # construct valid paths
  valid_paths = getValidPaths(parent_child)

  # pull out tag ID and trap date at Priest Rapids
  valid_tag_df = observations %>%
    dplyr::filter(`Event Site Code Value` %in% c('PRA')) %>%
    dplyr::mutate_at(vars(`Event Date Time Value`),
                     funs(mdy_hms)) %>%
    dplyr::filter(`Event Date Time Value` >= ymd(startDate)) %>%
    dplyr::group_by(TagID = `Tag Code`) %>%
    dplyr::summarise(TrapDate = min(`Event Date Time Value`) - dminutes(1)) %>%
    dplyr::ungroup()


  # translate in nodes and simplify consecutive hits on the same node
  valid_obs = assignNodes(valid_tag_df,
                          observations,
                          configuration,
                          parent_child,
                          truncate)

  # drop observations at Wells dam for tags that were observed downstream of Wells later
  wells_tags_all = valid_obs %>%
    dplyr::filter(Node == 'WEA') %>%
    dplyr::select(TagID, ObsDate) %>%
    dplyr::group_by(TagID) %>%
    dplyr::summarise(WellsDate = max(ObsDate)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  wen_nodes = valid_paths %>%
    dplyr::filter(grepl('LWEB0', Path) |
                    grepl('CLK', Path)) %>%
    dplyr::select(Node) %>%
    dplyr::distinct() %>%
    as.matrix() %>%
    as.character()

  ent_nodes = valid_paths %>%
    dplyr::filter(grepl('ENL', Path) |
                    grepl('WVT', Path)) %>%
    dplyr::select(Node) %>%
    dplyr::distinct() %>%
    as.matrix() %>%
    as.character()

  above_wells_nodes = valid_paths %>%
    dplyr::filter(grepl('WEA', Path)) %>%
    dplyr::select(Node) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  wells_tags_dwnstm = valid_obs %>%
    dplyr::inner_join(wells_tags_all,
                      by = 'TagID') %>%
    dplyr::filter(Node %in% c(wen_nodes, ent_nodes)) %>%
    dplyr::filter(ObsDate > WellsDate) %>%
    dplyr::select(TagID) %>%
    dplyr::distinct() %>%
    as.matrix() %>%
    as.character()

  valid_obs = valid_obs %>%
    dplyr::filter(!(TagID %in% wells_tags_dwnstm & Node %in% above_wells_nodes))


  save_df = writeCapHistOutput(valid_obs,
                               valid_paths,
                               save_file,
                               file_name)

  return(list('ValidPaths' = valid_paths,
              # 'ValidTrapData' = trap_df,
              'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))

}
