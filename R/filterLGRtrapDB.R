#' @title Read Trap Database
#'
#' @description Read and filter LGR adult trap database to only include fish considered part of the valid sample
#'
#' @author Kevin See
#'
#' @param path file path including name of file where .csv file of trap database is stored
#' @param species either Chinook or Steelhead
#' @param spawnYear spawn year
#'
#' @import dplyr readr
#' @export
#' @return NULL
#' @examples filterLGRtrapDB(spawnYear = 2014)

filterLGRtrapDB = function(path = '.',
                           species = c('Chinook', 'Steelhead'),
                           spawnYear = NULL) {

  # need a year
  stopifnot(!is.null(spawnYear))

  # set some default values
  species = match.arg(species)

  sppCode = ifelse(species == 'Chinook', 1,
                   ifelse(species == 'Steelhead', 3, NA))
  if(is.na(sppCode)) stop('Species name not found')

  if(is.null(path)) trap_df = trap_chnk2015

  if(!is.null(path)) trap_df = read_csv(path)

  # keep only correct species, spawnyear and adults (returning fish),
  # as well as fish determined to be valid, with ad intact adipose fins and non-missing PIT tags
  valid_df = trap_df %>%
    filter(LGDSpecies == sppCode,
           SpawnYear == paste0('SY', spawnYear),
           LGDLifeStage == 'RF',
           LGDValid == 1,
           LGDMarkAD == 'AI',
           !is.na(LGDNumPIT))

  # drop Chinook jacks
  if(species == 'Chinook') {
    valid_df = valid_df %>%
      filter(grepl('5', SRR))
  }

  return(valid_df)
}
