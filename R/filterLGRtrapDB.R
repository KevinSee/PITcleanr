#' @title Read Lower Granite Trap Database
#'
#' @description Read and filter LGR adult trap database to only include fish considered part of the valid sample for a given species (spring/summer Chinook salmon or steelhead) and given spawn year.
#'
#' @author Kevin See, Mike Ackerman, Ryan Kinzer, Rick Orme
#'
#' @param trap_path file path including name of file where .csv file of trap database is stored
#' @param species either Chinook or Steelhead
#' @param spawnYear spawn year, as integer value, e.g. \code{2015}
#' @param saveValidTagList Should text file of valid tags be saved in a format able to be uploaded to PTAGIS for a batch query? Default value is \code{FALSE}.
#' @param validTagFileNm if \code{saveValidTagList} is \code{TRUE}, this is the path and filename (including .txt) to save that file as.
#' @param saveCSV Should csv file of all biological information from the trapping database for the valid tags be saved? Default value is \code{FALSE}.
#' @param validTagDataFileNm if \code{saveCSV} is \code{TRUE}, this is the path and filename (including .csv) to save that file as.
#'
#' @import dplyr readr
#' @export
#' @return NULL
#' @examples filterLGRtrapDB(trap_path = NULL, species == 'Chinook', spawnYear = 2015)

filterLGRtrapDB = function(trap_path = '.',
                           species = c('Chinook', 'Steelhead'),
                           spawnYear = NULL,
                           saveValidTagList = F,
                           validTagFileNm = NULL,
                           saveCSV = F,
                           validTagDataFileNm = NULL) {

  # need a year
  stopifnot(!is.null(spawnYear))

  # set some default values
  species = match.arg(species)

  # determine species code (Chinook == 1, Steelhead == 3)
  sppCode = ifelse(species == 'Chinook', 1,
                   ifelse(species == 'Steelhead', 3, NA))
  if(is.na(sppCode)) stop('Species name not found')

  if(is.null(trap_path)) trap_df = trap_chnk2015

  if(!is.null(trap_path)) trap_df = read_csv(trap_path)

  if(class(trap_df$CollectionDate) == 'Date') {
    trap_df$CollectionDate = as.POSIXct(trap_df$CollectionDate,
                                        tz = 'UTC') %>%
      lubridate::floor_date(unit = 'days')
  }

  # keep only correct species, spawnyear and adults (returning fish),
  # as well as fish determined to be valid, with ad intact adipose fins and non-missing PIT tags
  valid_df = trap_df %>%
    dplyr::filter(grepl(paste0('^', sppCode), SRR),      # keep only the desired species
                  SpawnYear == paste0('SY', spawnYear),  # keep only the desired spawn year
                  LGDLifeStage == 'RF',                  # keep only adults (returning fish)
                  LGDValid == 1,                         # keep only records marked valid
                  LGDMarkAD == 'AI',                     # keep only adipose-intact records
                  !is.na(LGDNumPIT))                     # remove any records with missing PIT tag code

  # drop Fall Chinook
  if(species == 'Chinook') {
    valid_df = valid_df %>%
      dplyr::filter(grepl('5', SRR))
  }

  # select only columns we're interested in
  valid_df = valid_df %>%
    dplyr::select(MasterID, LGDNumPIT, CollectionDate, SpawnYear, BioSamplesID, LGDFLmm, SRR, GenRear, LGDLifeStage,
                  GenSex, GenStock, GenStockProb, GenParentHatchery, GenBY, GenPBT_ByHat, GenPBT_RGroup,
                  BioScaleFinalAge, PtagisEventSites, PtagisLastEventSite, PtagisLastEventDate,
                  PtagisEventLastSpawnSite, RepeatSpawner, BiosamplesValid, LGDValid, LGDInjuryiesAll, LGDMarksAll,
                  LGDMarkAD)

  if(saveValidTagList) {
    valid_df %>%
      dplyr::select(LGDNumPIT) %>%
      write.table(validTagFileNm,
                  quote = F,
                  row.names = F,
                  col.names = F,
                  sep = '\t')
  }

  if(saveCSV) {
    valid_df %>%
      readr::write_csv(validTagDataFileNm,
                       row.names = F)
  }


  return(valid_df)
}
