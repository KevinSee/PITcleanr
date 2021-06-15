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
#' @import dplyr lubridate
#' @export
#' @return NULL
#' @examples processCapHist_TUM()

processCapHist_TUM = function(start_date = NULL,
                              tagging_site = c('TUF', 'TUM', 'TUMFBY'),
                              configuration = NULL,
                              parent_child = NULL,
                              observations = NULL,
                              last_obs_date = NULL,
                              truncate = T,
                              site_df = NULL,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(parent_child) |
              !is.null(observations))


  if(!c('Event Release Date Time Value') %in% names(observations)) {
    observations$`Event Release Date Time Value` = NA
  }

  # pull out tag ID and trap date at Tumwater
  # valid_tag_df = observations %>%
  #   filter(`Event Site Code Value` %in% tagging_site) %>%
  #   left_join(configuration %>%
  #               select(SiteID, SiteType, SiteTypeName) %>%
  #               distinct(),
  #             by = c('Event Site Code Value' = 'SiteID')) %>%
  #   mutate(ObsDate = ifelse(!is.na(`Event Release Date Time Value`) &
  #                             is.na(`Antenna ID`) &
  #                             SiteType == 'MRR' &
  #                             SiteTypeName %in% c('Dam'),
  #                           `Event Release Date Time Value`,
  #                           `Event Date Time Value`)) %>%
  #   mutate_at(vars(ObsDate),
  #             funs(mdy_hms)) %>%
  #   group_by(TagID = `Tag Code`) %>%
  #   summarise(TrapDate = min(ObsDate) - dminutes(1)) %>%
  #   ungroup()

  # pull out tag ID and trap date at Tumwater
  cat('Getting trap date.\n')
  valid_tag_df = observations %>%
    filter(`Event Site Code Value` %in% tagging_site) %>%
    mutate_at(vars(`Event Date Time Value`, `Event Release Date Time Value`),
              list(lubridate::mdy_hms)) %>%
    mutate(ObsDate = if_else(!is.na(`Event Release Date Time Value`) &
                               is.na(`Antenna ID`),
                             `Event Release Date Time Value`,
                             `Event Date Time Value`)) %>%
    filter(ObsDate >= lubridate::ymd(start_date)) %>%
    group_by(TagID = `Tag Code`) %>%
    summarise(TrapDate = min(lubridate::floor_date(ObsDate,
                                                   unit = 'days'))) %>%
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
  # construct valid paths
  valid_paths = getValidPaths(parent_child)
  if(is.null(site_df)) {
    site_df = writeTUMNodeNetwork_noUWE()
  }
  node_order = createNodeOrder(valid_paths = valid_paths,
                               configuration = configuration,
                               site_df = site_df,
                               step_num = 2)

  # this could be used to create different matrices to feed to DABOM
  node_order = node_order %>%
    mutate(Group = fct_recode(Group,
                              Peshastin = "PES",
                              Icicle = "ICL",
                              Chiwaukum = "CHW",
                              Chiwawa = "CHL")) %>%
    mutate_at(vars(Group),
              list(as.character)) %>%
    mutate(Group = if_else(grepl('NAL', Path) | Node == 'UWE',
                           "Nason",
                           Group),
           Group = if_else(grepl('WTL', Path),
                           "WhiteRiver",
                           Group),
           Group = if_else(grepl('LWN', Path),
                           "LittleWenatchee",
                           Group)) %>%
    mutate(Group = factor(Group,
                          levels = c('Peshastin',
                                     'Icicle',
                                     'Chiwaukum',
                                     'Chiwawa',
                                     'Nason',
                                     'WhiteRiver',
                                     'LittleWenatchee'))) %>%
    mutate(BranchNum = as.integer(Group))

  cat('Processing assigned nodes\n')
  save_df = writeCapHistOutput(valid_obs,
                               valid_paths,
                               node_order,
                               last_obs_date,
                               save_file,
                               file_name)

  return(list(#'ValidPaths' = valid_paths,
              'NodeOrder' = node_order,
              # 'ValidTrapData' = valid_tag_df %>%
              #   left_join(observations %>%
              #               select(TagID = `Tag Code`,
              #                      Origin = `Mark Rear Type Name`)),
              # 'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))

}
