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
    filter(`Event Site Code Value` %in% c('PRA')) %>%
    mutate_at(vars(`Event Date Time Value`),
              funs(mdy_hms)) %>%
    filter(`Event Date Time Value` >= ymd(startDate)) %>%
    group_by(TagID = `Tag Code`) %>%
    summarise(TrapDate = min(`Event Date Time Value`) - dminutes(1)) %>%
    ungroup()


  # translate in nodes and simplify consecutive hits on the same node
  valid_obs = assignNodes(valid_tag_df,
                          observations,
                          configuration,
                          parent_child,
                          truncate)

  # drop observations at Wells dam for tags that were observed downstream of Wells later
  wells_tags_all = valid_obs %>%
    filter(Node == 'WEA') %>%
    select(TagID, ObsDate) %>%
    group_by(TagID) %>%
    summarise(WellsDate = max(ObsDate)) %>%
    ungroup() %>%
    distinct()

  wen_nodes = valid_paths %>%
    filter(grepl('LWEB0', Path) |
             grepl('CLK', Path)) %>%
    select(Node) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  ent_nodes = valid_paths %>%
    filter(grepl('ENL', Path) |
             grepl('WVT', Path)) %>%
    select(Node) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  above_wells_nodes = valid_paths %>%
    filter(grepl('WEA', Path)) %>%
    select(Node) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  wells_tags_dwnstm = valid_obs %>%
    inner_join(wells_tags_all,
               by = 'TagID') %>%
    filter(Node %in% c(wen_nodes, ent_nodes)) %>%
    filter(ObsDate > WellsDate) %>%
    select(TagID) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  valid_obs = valid_obs %>%
    filter(!(TagID %in% wells_tags_dwnstm & Node %in% above_wells_nodes))


  save_df = writeCapHistOutput(valid_obs,
                               valid_paths,
                               save_file,
                               file_name)

  return(list('ValidPaths' = valid_paths,
              # 'ValidTrapData' = trap_df,
              'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))

}
