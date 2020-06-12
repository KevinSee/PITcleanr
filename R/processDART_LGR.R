#' @title Process tags observed at Lower Granite and any subsequent detections upstream.
#'
#' @description Using the raw import from DART's observation query, and the configuration files the function process the data into a PITcleanr capture history dataframe.
#'
#' @author Ryan N. Kinzer
#'
#'
#' @import dplyr lubridate
#' @export
#' @return NULL
#' @examples processDART_LGR()
processDART_LGR <- function(species = c('Chinook', 'Steelhead'),
                            spawnyear = NULL,
                            observations = NULL,
                            configuration = NULL,
                            truncate = T){

  if(species == 'Chinook'){
    spp_code = 1
    start_date <- paste0(spawnyear,'0301')
  }

  if(species == 'Steelhead'){
    spp_code = 3
    start_date <- paste0(spawnyear-1,'0701')
  }

  # load data
  dart_file <- paste0('http://www.cbr.washington.edu/dart/cs/data/nezperce/nptspawn_GRA_',spawnyear,'_',spp_code,'.csv')

  observations <- read_csv(dart_file)

  my_config <- configuration %>% mutate(ConfigID = as.character(ConfigID))

  # hard coded network of sites upstream of LGR
  site_df = writeLGRNodeNetwork()

  # maps the order of nodes upstream of LGR - ONLY WORKS FOR CHINOOK NOW
  parent_child <- createParentChildDf(site_df,
                                      my_config,
                                      startDate = start_date)

  # creates all valid detection paths upstream of LGR
  valid_paths <- getValidPaths(parent_child)

  node_order = createNodeOrder(valid_paths,
                               configuration = my_config,
                               site_df,
                               step_num = 3)

  # Get tag list ----
  lgr_tags <- observations %>%
    filter(obs_site %in% c('GRA', 'LGRLDR')) %>%
    group_by(tag_id) %>%
    slice(which.min(obs_time)) %>%
    select(TagID = tag_id, TrapDate = obs_time)

  # Assign Nodes ----
  valid_obs <- assignNodes(valid_tag_df = lgr_tags,
                           observation = observations,
                           configuration = my_config,
                           parent_child_df = parent_child,
                           truncate = truncate,
                           obs_input = 'dart')

  # Process Observations ----
  proc_ch <- writeCapHistOutput(valid_obs,
                                 valid_paths,
                                 node_order,
                                 save_file = FALSE)

  return(list(proc_ch, observations))
  }
