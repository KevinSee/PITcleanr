#' @title Tumwater Node Network
#'
#' @description Create data.frame describing all the observation nodes in the Tumwater dam version of DABOM, including how they are related to one another
#'
#' @author Kevin See
#'
#'
#' @import dplyr tibble stringr
#' @export
#' @return NULL
#' @examples writeTUMNodeNetwork()

writeTUMNodeNetwork = function() {
  bin_names = c('Peshastin',
                'Icicle',
                'Chiwaukum',
                'Chiwawa',
                'UpperWenatchee')
  bin_list = vector('list', length(bin_names))
  names(bin_list) = bin_names

  bin_list[['Peshastin']] = list('PES')

  bin_list[['Icicle']] = list('ICL' =
                                list('ICL',
                                     'LEAV',
                                     'LNF',
                                     'ICM' =
                                       list('ICM',
                                            'ICU')))

  bin_list[['Chiwaukum']] = list('CHW')

  bin_list[['Chiwawa']] = list('CHL' =
                                 list('CHL',
                                      'CHU'))

  bin_list[['UpperWenatchee']] = list('UWE' =
                                        list('UWE',
                                             'WTL',
                                             'LWN',
                                             'NAL' =
                                               list('NAL',
                                                    'NAU')))


  site_df_init = tibble(SiteID = unlist(bin_list),
                        path = names(unlist(bin_list))) %>%
    mutate(path = str_replace(path,
                              '[[:digit:]]$',
                              ''),
           path = str_replace(path,
                              'SC$',
                              'SC1'),
           path = str_replace(path,
                              'USI1',
                              'USI'),
           path = str_replace(path,
                              'LRW1',
                              'LRW'),
           path = ifelse(SiteID %in% c('IR1', 'IR4'),
                         str_replace(path,
                                     'IR$',
                                     SiteID),
                         path))

  network_descrip = str_split(site_df_init$path,
                              '\\.',
                              simplify = T)
  colnames(network_descrip) = paste0('Step', 1:ncol(network_descrip))

  site_df = site_df_init %>%
    bind_cols(network_descrip %>%
                as.data.frame()) %>%
    gather(brk, upstrm_site, matches('Step')) %>%
    mutate(upstrm_site = ifelse(upstrm_site == '', NA, upstrm_site),
           upstrm_site = ifelse(upstrm_site == SiteID, NA, upstrm_site)) %>%
    spread(brk, upstrm_site,
           fill = '') %>%
    mutate(SiteID = factor(SiteID,
                           levels = site_df_init$SiteID)) %>%
    arrange(SiteID)


  return(site_df)
}
