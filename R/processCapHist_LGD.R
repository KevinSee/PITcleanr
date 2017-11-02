#' @title Process capture histories for Lower Granite
#'
#' @description Using a site/node configuration file, a list of modeled sites, a parent-child table of nodes, and the raw capture histories from PTAGIS, process them. This function is specific to fish tagged at Lower Granite dam.
#'
#' @author Kevin See
#'
#' @inheritParams getValidPaths
#' @inheritParams filterLGRtrapDB
#' @inheritParams assignNodes
#' @inheritParams writeCapHistOutput
#' @param filter_by_PBT Should fish identified as hatchery origin be filtered out of the sample, based on PBT results? Default value is \code{TRUE}.
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples processCapHist_LGD()

processCapHist_LGD = function(species = c('Chinook', 'Steelhead'),
                              spawnYear = NULL,
                              configuration = NULL,
                              parent_child = NULL,
                              trap_path = NULL,
                              filter_by_PBT = T,
                              observations = NULL,
                              truncate = T,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(spawnYear) |
              !is.null(parent_child) |
              !is.null(observations) |
              !is.null(trap_path))

  # set some default values
  species = match.arg(species)

  # construct valid paths
  valid_paths = getValidPaths(parent_child)

  # get trap data
  trap_df = filterLGRtrapDB(trap_path,
                            species,
                            spawnYear)

  if(filter_by_PBT) {
    trap_df %>%
      filter(!grepl('H$', SRR))
  }

  # pull valid tags from trap database, get trap date
  valid_tag_df = trap_df %>%
    group_by(TagID = LGDNumPIT) %>%
    summarise(TrapDate = min(CollectionDate, na.rm = T))

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
              'ValidTrapData' = trap_df,
              'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))


}
