#' @title Tumwater Node Network
#'
#' @description Create data.frame describing all the observation nodes in the Tumwater dam version of DABOM, including how they are related to one another. This function does not include the detection site UWE
#'
#' @author Kevin See
#'
#'
#' @import dplyr tibble stringr
#' @export
#' @return NULL
#' @examples writeTUMNodeNetwork_noUWE()

writeTUMNodeNetwork_noUWE = function() {
  bin_all = list('TUM' =
                   list('TUM',
                        list('PES' =
                               list('PES',
                                    'PEU')),
                        list('ICL' =
                               list('ICL',
                                    'LNF' =
                                      list('LNF',
                                           'LEAV'),
                                    'ICM' =
                                      list('ICM',
                                           'ICU'))),
                        'CHW',
                        list('CHL' =
                               list('CHL',
                                    'CHU')),
                        'NAL' =
                          list('NAL',
                               'NAU'),
                        'WTL',
                        'LWN'))

  site_df_init = unlist(bin_all) %>%
    tibble::enframe(name = 'path',
                    value = 'SiteID') %>%
    dplyr::select(SiteID, path) %>%
    mutate_at(vars(path),
              list(~ str_remove_all(., '[[:digit:]]+$'))) %>%
    rowwise() %>%
    mutate(path = if_else(stringr::str_sub(path, start = -nchar(SiteID)) != SiteID,
                          paste(path, SiteID, sep = '.'),
                          path)) %>%
    ungroup()

  network_descrip = stringr::str_split(site_df_init$path,
                                       '\\.',
                                       simplify = T) %>%
    as_tibble() %>%
    rlang::set_names(paste0('Step', 1:ncol(.)))

  site_df = site_df_init %>%
    bind_cols(network_descrip) %>%
    tidyr::gather(brk, upstrm_site, matches('Step')) %>%
    mutate(upstrm_site = ifelse(upstrm_site == '', NA, upstrm_site)) %>%
    tidyr::spread(brk, upstrm_site,
                  fill = '') %>%
    mutate(SiteID = factor(SiteID,
                           levels = site_df_init$SiteID)) %>%
    arrange(SiteID)

  return(site_df)
}
