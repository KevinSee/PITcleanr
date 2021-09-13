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
  cat('Querying INT sites\' metadata\n')
  int_meta = queryInterrogationMeta()
  # get configuration details for interrogation sites
  cat('Querying INT sites\' configuration information\n')
  int_config = queryInterrogationConfig()

  # get metadata for MRR sites
  cat('Querying MRR sites\' metadata\n')
  mrr_meta = queryMRRMeta()

  # put it all together
  all_meta = int_meta %>%
    dplyr::rename(siteName = name,
                  siteDescription = description) %>%
    dplyr::full_join(int_config,
                     by = c("siteCode", "siteName")) %>%
    dplyr::mutate(Type = 'INT') %>%
    dplyr::bind_rows(mrr_meta %>%
                       dplyr::mutate(Type = 'MRR') %>%
                       dplyr::mutate(configurationSequence = 0,
                                     antennaID = as.character(NA)) %>%
                       dplyr::rename(siteDescription = type,
                                     siteName = name)) %>%
    tibble::add_column("RKMTotal" = NA, .after = "rkm") %>%
    dplyr::mutate(RKMTotal = stringr::str_split(rkm, "\\.")) %>%
    dplyr::mutate(RKMTotal = purrr::map_dbl(RKMTotal,
                                            .f = function(x) {
                                              x %>%
                                                as.numeric() %>%
                                                suppressWarnings() %>%
                                                sum(na.rm = T)
                                            })) %>%
    janitor::clean_names(case = "snake")

  return(all_meta)
}

