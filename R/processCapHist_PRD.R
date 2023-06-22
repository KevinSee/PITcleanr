#' @title Process capture histories for Priest Rapids
#'
#' @description Using a site/node configuration file, a list of modeled sites, a parent-child table of nodes, and the raw capture histories from PTAGIS, process them. This function is specific to fish tagged at Priest Rapids dam.
#'
#' @author Kevin See
#'
#' @param startDate observations that occurred before this date (YYYYMMDD format) will not be included.
#' @inheritParams getValidPaths
#' @inheritParams assignNodes
#' @inheritParams createNodeOrder
#' @inheritParams writeCapHistOutput
#'
#' @import dplyr lubridate
#' @return NULL
#' @examples processCapHist_PRD()

processCapHist_PRD = function(startDate = NULL,
                              configuration = NULL,
                              parent_child = NULL,
                              observations = NULL,
                              last_obs_date = NULL,
                              truncate = T,
                              site_df,
                              step_num = 1,
                              save_file = F,
                              file_name = NULL) {

  stopifnot(!is.null(startDate) |
              !is.null(parent_child) |
              !is.null(observations))

  # # drop observations from greater than one year from startDate
  # observations = observations %>%
  #   filter(mdy_hms(`Event Date Time Value`) < ymd(startDate) + years(1) | mdy_hms(`Event Release Date Time Value`) < ymd(startDate) + years(1))

  # construct valid paths
  cat('Constructing valid paths.\n')
  valid_paths = getValidPaths(parent_child)

  # create dataframe describing node order
  cat('Creating node order')
  node_order = createNodeOrder(valid_paths = valid_paths,
                               configuration = configuration,
                               site_df = site_df,
                               step_num = step_num)

  # pull out tag ID and trap date at Priest Rapids
  cat('Getting trap date.\n')
  valid_tag_df = observations %>%
    filter(`Event Site Code Value` %in% c('PRDLD1', 'PRD')) %>%
    mutate(across(c(`Event Date Time Value`,
                    `Event Release Date Time Value`),
                  lubridate::mdy_hms)) %>%
    mutate(ObsDate = if_else(!is.na(`Event Release Date Time Value`) &
                               is.na(`Antenna ID`),
                             `Event Release Date Time Value`,
                             `Event Date Time Value`)) %>%
    filter(ObsDate >= lubridate::ymd(startDate)) %>%
    group_by(TagID = `Tag Code`) %>%
    summarise(TrapDate = min(lubridate::floor_date(ObsDate,
                                                   unit = 'days'))) %>%
    ungroup()

  # if missing any tags, grab some
  if(n_distinct(observations$`Tag Code`) > n_distinct(valid_tag_df$TagID)) {
    alt_tag_df = observations %>%
      select(TagID = `Tag Code`) %>%
      distinct() %>%
      anti_join(valid_tag_df) %>%
      left_join(observations %>%
                  rename(TagID = `Tag Code`)) %>%
      filter(`Event Site Code Value` %in% c('PRA')) %>%
      mutate(across(c(`Event Date Time Value`,
                      `Event Release Date Time Value`),
                    lubridate::mdy_hms)) %>%
      mutate(ObsDate = if_else(!is.na(`Event Release Date Time Value`) &
                                 is.na(`Antenna ID`),
                               `Event Release Date Time Value`,
                               `Event Date Time Value`)) %>%
      filter(ObsDate >= lubridate::ymd(startDate)) %>%
      group_by(TagID) %>%
      summarise(TrapDate = min(lubridate::floor_date(ObsDate,
                                                     unit = 'days'))) %>%
      ungroup()

    valid_tag_df <- valid_tag_df %>%
      bind_rows(alt_tag_df)
  }


  # translate in nodes and simplify consecutive hits on the same node
  cat('Assigning nodes.\n')
  valid_obs = assignNodes(valid_tag_df,
                          observations,
                          configuration,
                          parent_child,
                          truncate)

  # add observations at trap back to some tags where it had been removed due to wonky date entries
  valid_obs = valid_obs %>%
    bind_rows(anti_join(valid_tag_df,
                        valid_obs) %>%
                mutate(Node = 'PRA',
                       ObsDate = TrapDate) %>%
                left_join(valid_obs %>%
                            filter(Node == 'PRA') %>%
                            select(Node,
                                   SiteName,
                                   SiteDescription) %>%
                            distinct()))

  # add observations at trap back to some tags where it had been removed due to wonky date entries
  valid_obs = valid_obs %>%
    bind_rows(valid_obs %>%
                group_by(TagID) %>%
                filter(!'PRA' %in% Node) %>%
                ungroup() %>%
                select(TagID, TrapDate) %>%
                distinct() %>%
                mutate(ObsDate = TrapDate,
                       Node = 'PRA',
                       ValidDate = T,
                       ValidNode = T) %>%
                left_join(valid_obs %>%
                            filter(Node == 'PRA') %>%
                            select(Node,
                                   SiteName,
                                   SiteDescription) %>%
                            distinct()))


  # # drop observations at Wells dam for tags that were observed downstream of Wells later
  # wells_tags_all = valid_obs %>%
  #   filter(Node == 'WEA') %>%
  #   select(TagID, ObsDate) %>%
  #   group_by(TagID) %>%
  #   summarise(WellsDate = max(ObsDate)) %>%
  #   ungroup() %>%
  #   distinct()
  #
  # wen_nodes = valid_paths %>%
  #   filter(grepl('LWEB0', Path) |
  #                   grepl('CLK', Path)) %>%
  #   select(Node) %>%
  #   distinct() %>%
  #   as.matrix() %>%
  #   as.character()
  #
  # ent_nodes = valid_paths %>%
  #   filter(grepl('ENL', Path) |
  #                   grepl('WVT', Path)) %>%
  #   select(Node) %>%
  #   distinct() %>%
  #   as.matrix() %>%
  #   as.character()
  #
  # above_wells_nodes = valid_paths %>%
  #   filter(grepl(' WEA', Path)) %>%
  #   select(Node) %>%
  #   distinct() %>%
  #   as.matrix() %>%
  #   as.character()
  #
  # wells_tags_dwnstm = valid_obs %>%
  #   inner_join(wells_tags_all,
  #                     by = 'TagID') %>%
  #   filter(Node %in% c(wen_nodes, ent_nodes)) %>%
  #   filter(ObsDate > WellsDate) %>%
  #   select(TagID) %>%
  #   distinct() %>%
  #   as.matrix() %>%
  #   as.character()
  #
  # valid_obs = valid_obs %>%
  #   filter(!(TagID %in% wells_tags_dwnstm & Node %in% above_wells_nodes))

  # # drop downstream detections for obvious kelts
  # dwnStrm_nodes = node_order %>%
  #   mutate(initRKM = as.integer(stringr::str_split(RKM, '\\.', simplify = T)[,1])) %>%
  #   filter(initRKM < initRKM[NodeOrder == 1]) %>%
  #   select(Node) %>%
  #   distinct() %>%
  #   filter(Node != 'BelowJD1') %>%
  #   as.matrix() %>%
  #   as.character()
  #
  # upStrm_nodes = node_order %>%
  #   mutate(initRKM = as.integer(stringr::str_split(RKM, '\\.', simplify = T)[,1])) %>%
  #   filter(initRKM >= initRKM[NodeOrder == 1],
  #          Node != Node[NodeOrder == 1]) %>%
  #   select(Node) %>%
  #   distinct() %>%
  #   as.matrix() %>%
  #   as.character()
  #
  # tagPos_df = valid_obs %>%
  #   mutate(belowJD1 = ifelse(Node == 'BelowJD1',
  #                            T, F),
  #          dwnStrm = ifelse(Node %in% dwnStrm_nodes,
  #                           T, F),
  #          upStrm = ifelse(Node %in% upStrm_nodes,
  #                          T, F)) %>%
  #   group_by(TagID) %>%
  #   summarise_at(vars(belowJD1, dwnStrm, upStrm),
  #                funs(sum),
  #                na.rm = T) %>%
  #   ungroup()
  #
  # upStrm_tags = tagPos_df %>%
  #   filter(upStrm > 0) %>%
  #   select(TagID) %>%
  #   distinct() %>%
  #   as.matrix() %>%
  #   as.character()
  #
  # dwnStrm_tags = tagPos_df %>%
  #   filter(dwnStrm > 0 & upStrm == 0)
  #
  #
  # dwnStrm_tags = tagPos_df
  #   filter((dwnStrm > 0 & upStrm == 0) |
  #            (belowJD1 > 0 & upstrm == 0)) %>%
  #   select(TagID) %>%
  #   distinct() %>%
  #   as.matrix() %>%
  #   as.character()
  #
  # valid_obs = valid_obs %>%
  #   filter(TagID %in% dwnStrm_tags |
  #          (!TagID %in% dwnStrm_tags & !Node %in% dwnStrm_nodes) |
  #            (TagID %in% dwnStrm_tags & ))


  # drop detecions of BelowJD1 if other detections exist
  onlyBelowJD1_tags = anti_join(valid_obs,
                                valid_obs %>%
                                  group_by(TagID) %>%
                                  filter(!Node %in% c('BelowJD1', 'PRA')) %>%
                                  ungroup() %>%
                                  select(TagID) %>%
                                  distinct()) %>%
    select(TagID) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  valid_obs = valid_obs %>%
    filter(TagID %in% onlyBelowJD1_tags |
             (!TagID %in% onlyBelowJD1_tags & Node != 'BelowJD1'))

  # check if any tags have been dropped incorrectly along the way
  if(n_distinct(valid_obs$TagID) != n_distinct(observations$`Tag Code`)) {
    warning('Error: some tags being dropped')
  }


  cat('Processing assigned nodes\n')
  save_df = writeCapHistOutput(valid_obs,
                               valid_paths,
                               node_order,
                               last_obs_date,
                               save_file,
                               file_name)

  return(list('ValidPaths' = valid_paths,
              'NodeOrder' = node_order,
              # 'ValidTrapData' = trap_df,
              'ValidObs' = valid_obs,
              'ProcCapHist' = save_df))

}
