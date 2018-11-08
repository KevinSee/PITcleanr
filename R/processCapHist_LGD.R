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
#' @inheritParams createNodeOrder
#' @param filter_by_PBT Should fish identified as hatchery origin be filtered out of the sample, based on PBT results? Default value is \code{TRUE}.
#'
#' @import dplyr lubridate
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
                              last_obs_date = NULL,
                              truncate = T,
                              site_df,
                              step_num = 1,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(spawnYear) |
              !is.null(parent_child) |
              !is.null(observations) |
              !is.null(trap_path))

  # set some default values
  species = match.arg(species)

  # construct valid paths
  cat('Constructing valid pathways\n')
  valid_paths = getValidPaths(parent_child)

  if(class(valid_paths) == 'character') {
    print(paste('The following nodes returned an error:', paste(valid_paths, collapse = ', ')))
    return(NULL)
  }

  # get trap data
  cat('Filtering trap database\n')
  trap_df = filterLGRtrapDB(trap_path,
                            species,
                            spawnYear)

  if(filter_by_PBT) {
    trap_df = trap_df %>%
      filter(!grepl('H$', SRR))
  }

  # pull valid tags from trap database, get trap date
  cat('Getting valid tags\n')
  valid_tag_df = trap_df %>%
    group_by(TagID = LGDNumPIT) %>%
    summarise(TrapDate = min(CollectionDate, na.rm = T))

  # translate in nodes and simplify consecutive hits on the same node
  cat('Assigning nodes\n')
  valid_obs = assignNodes(valid_tag_df,
                          observations,
                          configuration,
                          parent_child,
                          truncate)

  cat('Creating node order\n')
  node_order = createNodeOrder(valid_paths = valid_paths,
                               configuration = configuration,
                               site_df = site_df,
                               step_num = step_num)

  cat('Processing assigned nodes\n')
  save_df = writeCapHistOutput(valid_obs,
                               valid_paths,
                               node_order,
                               last_obs_date,
                               save_file,
                               file_name)


  return(list('ValidPaths' = valid_paths,
              'NodeOrder' = node_order,
              'ValidTrapData' = trap_df,
              'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))


}
