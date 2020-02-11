#' @title Prosser Dam Node Network
#'
#' @description Create data.frame describing all the observation nodes in the Prosser dam version of DABOM, including how they are related to one another
#'
#' @author Kevin See
#'
#'
#' @import dplyr tibble stringr tidyr
#' @export
#' @return tibble
#' @examples writePRONodeNetwork()

writePRONodeNetwork = function() {
  bin_names = c('Status',
                'Toppenish',
                'Sunnyside',
                'BelowProsser')
  bin_list = vector('list', length(bin_names)) %>%
    rlang::set_names()

  bin_list[['Status']] = list('SAT')

  bin_list[['Toppenish']] = list('TOP' =
                                list('TOP',
                                     'TP2',
                                     'SM1',
                                     'MD'))

  bin_list[['Sunnyside']] = list('SUN' =
                                   list("SUN",
                                        "AH1",
                                        "LNR" =
                                          list("LNR",
                                               "TTN"),
                                        "WNS",
                                        "ROZ" =
                                          list("ROZ",
                                               "TEAN",
                                               "TAN",
                                               "SWK",
                                               "LMC" =
                                                 list("LMC",
                                                      "UMC"))))

  bin_list[['BelowProsser']] = list('BelowJD1',
                                       'JD1',
                                       'MCN',
                                       'ICH',
                                       'PRA')

  bin_all = list('PRO' =
                   list('PRO',
                        bin_list))

  site_df_init = tibble(SiteID = unlist(bin_all),
                        path = names(unlist(bin_all))) %>%
    mutate(path = stringr::str_replace(path,
                                       '[[:digit:]]$',
                                       '')) %>%
    rowwise() %>%
    mutate(path = ifelse(stringr::str_sub(path, start = -nchar(SiteID)) != SiteID,
                         paste(path, SiteID, sep = '.'),
                         path)) %>%
    ungroup()

  network_descrip = stringr::str_split(site_df_init$path,
                                       '\\.',
                                       simplify = T)
  colnames(network_descrip) = paste0('Step', 1:ncol(network_descrip))

  site_df = site_df_init %>%
    bind_cols(network_descrip %>%
                as.data.frame()) %>%
    tidyr::gather(brk, upstrm_site, matches('Step')) %>%
    mutate(upstrm_site = ifelse(upstrm_site == '', NA, upstrm_site)) %>%
    tidyr::spread(brk, upstrm_site,
                  fill = '') %>%
    mutate(SiteID = factor(SiteID,
                           levels = site_df_init$SiteID)) %>%
    arrange(SiteID)


  return(site_df)
}
