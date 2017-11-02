#' @title Process capture histories for Tumwater
#'
#' @description Using a site/node configuration file, a list of modeled sites, a parent-child table of nodes, and the raw capture histories from PTAGIS, process them. This function is specific to fish tagged at Tumwater dam.
#'
#' @author Kevin See
#'
#' @inheritParams getValidPaths
#' @inheritParams assignNodes
#' @inheritParams writeCapHistOutput
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples processCapHist_TUM()

processCapHist_TUM = function(species = c('Chinook', 'Steelhead'),
                              spawnYear = NULL,
                              configuration = NULL,
                              parent_child = NULL,
                              observations = NULL,
                              truncate = T,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(spawnYear) |
              !is.null(parent_child) |
              !is.null(observations))

  # set some default values
  species = match.arg(species)

  # construct valid paths
  valid_paths = getValidPaths(parent_child)

  # pull out tag ID and trap date at Tumwater
  valid_tag_df = observations %>%
    filter(`Event Site Code Value` %in% c('TUF', 'TUM', 'TUMFBY')) %>%
    mutate_at(vars(`Event Date Time Value`),
              funs(mdy_hms)) %>%
    group_by(TagID = `Tag Code`) %>%
    summarise(TrapDate = min(`Event Date Time Value`) - dminutes(1)) %>%
    ungroup()


  # translate in nodes and simplify consecutive hits on the same node
  valid_obs = assignNodes(valid_tag_df,
                          observations,
                          configuration,
                          parent_child,
                          truncate)

  save_df = writeCapHistOutput(valid_obs,
                               valid_paths,
                               save_file,
                               file_name)

  return(list('ValidPaths' = valid_paths,
              # 'ValidTrapData' = trap_df,
              'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))

}
