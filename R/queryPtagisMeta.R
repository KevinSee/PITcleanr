#' @title Query PTAGIS Site Metadata
#'
#' @description Compile metadata from all MRR and interogation sites from PTAGIS
#'
#' @author Kevin See
#'
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples queryPtagisMeta()
#'
queryPtagisMeta = function() {

  # get metadata for interrogation sites
  print('Querying INT sites\' metadata')
  int_meta = queryInterrogationMeta()
  # get configuration details for interrogation sites
  print('Querying INT sites\' configuration information')
  int_config = queryInterrogationConfig()

  # get metadata for MRR sites
  print('Querying MRR sites\' metadata')
  mrr_meta = queryMRRMeta()

  # put it all together
  all_meta = int_meta %>%
    dplyr::full_join(int_config) %>%
    dplyr::mutate(Type = 'INT') %>%
    dplyr::full_join(mrr_meta %>%
                       dplyr::mutate(Type = 'MRR') %>%
                       dplyr::mutate(configurationSequence = 0,
                                     antennaID = as.character(NA))) %>%
    tibble::add_column("RKMTotal" = NA, .after = "rkm") %>%
    dplyr::mutate(RKMTotal = stringr::str_split(rkm, "\\.")) %>%
    dplyr::mutate(RKMTotal = purrr::map_dbl(RKMTotal,
                                            .f = function(x) {
                                              sum(as.numeric(x))
                                            }))

  return(all_meta)
}

