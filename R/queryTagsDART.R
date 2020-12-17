#' @title Query DART for list of PIT tags
#'
#' @description For certain locations, query DART to return all PIT tags observed there from a given species in a given spawn year.
#'
#' @param species Chinook, Coho, Steelhead and Sockeye
#' @param loc code for initial location. Currently includes options for Lower Granite (\code{GRA}) and Priest Rapids (\code{PRA}) dams.
#' @param spawnYear available years includes spawn year 2010 to present
#'
#' @author Kevin See
#'
#' @import dplyr
#' @importFrom readr read_csv
#' @export
#' @return NULL
#' @examples
#'
queryTagsDART <- function(species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                          loc = c('GRA', 'PRA'),
                          spawnYear = NULL) {

  species <- match.arg(species)
  loc <- match.arg(loc)

  stopifnot(!is.null(spawnYear))

  spp_code = tibble(spp_nm = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                    spp_code = 1:4)  %>%
    filter(spp_nm == species) %>%
    pull(spp_code)

  # file path to query
  dart_path <- paste0("http://www.cbr.washington.edu/dart/cs/data/nezperce/", paste("nptspawn", loc, spawnYear, spp_code, sep = "_"), ".tagids")
  # read in data from DART
  tag_codes <- readr::read_delim(dart_path,
                                 delim = '\n')

  return(tag_codes)
}

