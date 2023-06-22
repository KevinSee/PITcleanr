#' @title Process capture histories for Prosser Dam
#'
#' @description Using a site/node configuration file, a list of modeled sites, a parent-child table of nodes, and the raw capture histories from PTAGIS, process them. This function is specific to fish tagged at Prosser dam.
#'
#' @author Kevin See
#'
#' @param start_date observations that occurred before this date (YYYYMMDD format) will not be included.
#' @param tagging_site vector of site codes used to determine when fish were caught in the fish trap
#' @inheritParams getValidPaths
#' @inheritParams assignNodes
#' @inheritParams createNodeOrder
#' @inheritParams writeCapHistOutput
#'
#' @import dplyr lubridate
#' @return list
#' @examples processCapHist_PRO()

processCapHist_PRO = function(start_date = NULL,
                              tagging_site = c('PRO', "PROTRP", "PROSRD"),
                              configuration = NULL,
                              parent_child = NULL,
                              observations = NULL,
                              last_obs_date = NULL,
                              truncate = T,
                              site_df,
                              step_num = 2,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(start_date) |
              !is.null(parent_child) |
              !is.null(observations))

  # construct valid paths
  cat('Constructing valid paths.\n')
  valid_paths = getValidPaths(parent_child)

  # create dataframe describing node order
  cat('Creating node order')
  node_order = createNodeOrder(valid_paths = valid_paths,
                               configuration = configuration,
                               site_df = site_df,
                               step_num = step_num)

  # this could be used to create different matrices to feed to DABOM
  node_order = node_order %>%
    select(-Group) %>%
    left_join(tibble(BranchNum = 1:8,
                     Group = c(rep("Downstream", 5),
                               'Status',
                               'Toppenish',
                               'Sunnyside')))

  # pull out tag ID and trap date at Prosser
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


  # translate in nodes and simplify consecutive hits on the same node
  cat('Assigning nodes.\n')
  valid_obs = assignNodes(valid_tag_df,
                          observations,
                          configuration,
                          parent_child,
                          truncate)

  # drop detecions of BelowJD1 if other detections exist
  # this eliminates detections of kelts moving down through the mainstem Columbia
  onlyBelowJD1_tags = anti_join(valid_obs,
                                valid_obs %>%
                                  group_by(TagID) %>%
                                  filter(!Node %in% c('BelowJD1', tagging_site)) %>%
                                  ungroup() %>%
                                  select(TagID) %>%
                                  distinct()) %>%
    pull(TagID) %>%
    unique()

  valid_obs = valid_obs %>%
    filter(TagID %in% onlyBelowJD1_tags |
             (!TagID %in% onlyBelowJD1_tags & Node != 'BelowJD1'))


  if(!is.null(last_obs_date)) {
    valid_obs = valid_obs %>%
      filter(ObsDate <= lubridate::ymd(last_obs_date))
  }

  # check if any tags have been dropped incorrectly along the way
  if(n_distinct(valid_obs$TagID) != n_distinct(observations$`Tag Code`)) {
    warning('Error: some tags being dropped')
  }


  cat('Processing assigned nodes\n')
  save_df = writeCapHistOutput(valid_obs,
                               valid_paths,
                               node_order,
                               last_obs_date = NULL,
                               save_file,
                               file_name)

  return(list(#'ValidPaths' = valid_paths,
              'NodeOrder' = node_order,
              # 'ValidTrapData' = trap_df,
              # 'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))

}
