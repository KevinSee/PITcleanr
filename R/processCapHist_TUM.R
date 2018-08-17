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

  if(!c('Event Release Date Time Value') %in% names(observations)) {
    observations$`Event Release Date Time Value` = NA
  }

  # pull out tag ID and trap date at Tumwater
  valid_tag_df = observations %>%
    filter(`Event Site Code Value` %in% c('TUF', 'TUM', 'TUMFBY')) %>%
    left_join(configuration %>%
                select(SiteID, SiteType, SiteTypeName) %>%
                distinct(),
              by = c('Event Site Code Value' = 'SiteID')) %>%
    mutate(ObsDate = ifelse(!is.na(`Event Release Date Time Value`) &
                              is.na(`Antenna ID`) &
                              SiteType == 'MRR' &
                              SiteTypeName %in% c('Dam'),
                            `Event Release Date Time Value`,
                            `Event Date Time Value`)) %>%
    mutate_at(vars(ObsDate),
              funs(mdy_hms)) %>%
    group_by(TagID = `Tag Code`) %>%
    summarise(TrapDate = min(ObsDate) - dminutes(1)) %>%
    ungroup()

  # valid_tag_df = observations %>%
  #   filter(`Event Site Code Value` %in% c('TUF', 'TUM', 'TUMFBY')) %>%
  #   mutate_at(vars(`Event Date Time Value`),
  #             funs(mdy_hm)) %>%
  #   group_by(TagID = `Tag Code`) %>%
  #   summarise(TrapDate = min(`Event Date Time Value`) - dminutes(1)) %>%
  #   ungroup()


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
                               step_num = 2)

  cat('Processing assigned nodes\n')
  save_df = writeCapHistOutput(valid_obs,
                               valid_paths,
                               node_order,
                               save_file,
                               file_name)

  # for Chinook, mark AutoProcStatus == F for any observation in the year after the trap date
  if(species == 'Chinook') {
    save_df = save_df %>%
      mutate(AutoProcStatus = if_else(year(ObsDate) > year(TrapDate),
                                      F,
                                      AutoProcStatus))
  }

  return(list('ValidPaths' = valid_paths,
              'NodeOrder' = node_order,
              # 'ValidTrapData' = valid_tag_df %>%
              #   left_join(observations %>%
              #               select(TagID = `Tag Code`,
              #                      Origin = `Mark Rear Type Name`)),
              'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))

}
