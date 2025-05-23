#' @title Query DART for observations of PIT tags
#'
#' @description For certain locations (currently Lower Granite Dam and Priest Rapids Dam),
#' query DART to return all PIT tags observed there from a given species in a given spawn year,
#' as well as any subsequent detections upstream.
#'
#' @param species Chinook, Coho, Steelhead and Sockeye
#' @param loc code for initial location. Currently includes options for Lower Granite (\code{GRA}) and Priest Rapids (\code{PRA}) dams.
#' @param spawn_year available years includes spawn year 2010 to present
#'
#' @author Kevin See
#'
#' @import dplyr
#' @importFrom readr read_csv
#' @export
#' @return A tibble with particular information from a complete tag history PTAGIS query.
#' @examples queryObsDART(species = 'Chinook',
#'                        loc = "GRA",
#'                        spawn_year = 2019)

queryObsDART <- function(species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                         loc = c('GRA', 'PRA'),
                         spawn_year = NULL) {

  species <- match.arg(species)
  loc <- match.arg(loc)

  stopifnot(!is.null(spawn_year))

  spp_code = tibble(spp_nm = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                    spp_code = 1:4)  %>%
    filter(spp_nm == species) %>%
    pull(spp_code)

  # file path to query
  dart_path <- paste0("https://www.cbr.washington.edu/dart/cs/data/nezperce/", paste("nptspawn", loc, spawn_year, spp_code, sep = "_"), ".csv")
  # read in data from DART
  dart_obs <- readr::read_csv(dart_path,
                              guess_max = 1e6) %>%
    suppressMessages()

  return(dart_obs)
}

