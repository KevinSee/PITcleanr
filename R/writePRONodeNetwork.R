#' @title Prosser Dam Node Network
#'
#' @description Create data.frame describing all the observation nodes in the Prosser dam version of DABOM, including how they are related to one another
#'
#' @author Kevin See
#'
#'
#' @import dplyr tibble stringr tidyr
#' @return tibble
#' @examples writePRONodeNetwork()

writePRONodeNetwork = function() {

  bin_all = list('PRO' =
                   list('PRO',
                        'SAT',
                        list('TOP' =
                               list('TOP',
                                    'TP2',
                                    'SM1',
                                    'MD')),
                        list('SUN' =
                               list("SUN",
                                    "AH1",
                                    "LNR" =
                                      list("LNR",
                                           "TTN"),
                                    "LWC",
                                    "ROZ" =
                                      list("ROZ",
                                           "LMT",
                                           "TAN",
                                           "SWK",
                                           "LMC" =
                                             list("LMC",
                                                  "UMC")))),
                        'BelowJD1',
                        'JD1',
                        'MCN',
                        'ICH',
                        'PRA'))

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
    as_tibble(.name_repair = "minimal") %>%
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
