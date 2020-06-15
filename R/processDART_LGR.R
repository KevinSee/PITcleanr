#' @title Process tags observed at Lower Granite and any subsequent detections upstream.
#'
#' @description Using the raw import from DART's observation query, and the configuration files the function process the data into a PITcleanr capture history dataframe. A list is returned that contains the raw DART query export and the cleaned capture history dataset.
#'
#' @param species Chinook, Coho, Steelhead and Sockeye
#' @param spawnYear available years include spawn year 2010 to present
#' @param configuration output from buildConfig function
#' @param truncate should repeat observations at the same IPTDS node be removed from cleaned capture history dataset?
#'
#' @author Ryan N. Kinzer
#'
#' @import dplyr readr
#' @export
#' @return NULL
#' @examples
#'
#' my_config <- buildConfig()
#' dart_obs <- processDART_LGR(species = 'Chinook',
#'                            spawnYear = 2020,
#'                            configuration = my_config,
#'                            truncate = T)

processDART_LGR <- function(species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                            spawnYear = NULL,
                            configuration = NULL,
                            truncate = T){

  species <- match.arg(species)

  stopifnot(!is.null(spawnYear) |
              !is.null(configuration))

  tmp_df <- tibble(species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                   spp_code = 1:4)

  spp_code <- tmp_df$spp_code[tmp_df$species == species]

  if(species != 'Steelhead'){
    start_date <- paste0(spawnYear,'0301')
  }

  if(species == 'Steelhead'){
    start_date <- paste0(spawnYear-1,'0701')
  }

  # load data
  dart_file <- paste0('http://www.cbr.washington.edu/dart/cs/data/nezperce/nptspawn_GRA_',spawnYear,'_',spp_code,'.csv')

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

  return(list('proc_ch' = proc_ch, 'dart_obs' = observations))
  }
