#' @title LGR Node Network
#'
#' @description Create data.frame describing all the observation nodes in the Lower Granite dam version of DABOM, including how they are related to one another
#'
#' @author Kevin See
#'
#'
#' @import dplyr tibble stringr
#' @export
#' @return NULL
#' @examples writeLGRNodeNetwork()

writeLGRNodeNetwork = function() {
  bin_names = c('Clearwater',
                'SW_Washington',
                'NE_Oregon',
                'Imnaha',
                'Salmon')
  bin_list = vector('list', length(bin_names))
  names(bin_list) = bin_names

  bin_list[['Clearwater']] = list('Lapwai' = list('LAP' = list('LAP',
                                                               'MIS',
                                                               'SWT' = list('SWT', 'WEB'))),
                                  'Potlatch' = list('JUL' = list('JUL',
                                                                 'KHS' = list('KHS',
                                                                              'LBEARC', 'BIGBEC'),
                                                                 'HLM' = list('HLM',
                                                                              'POTRWF', 'POTREF'),
                                                                 'PCM')),
                                  'Lolo' = list('LC1' = list('LC1', 'LC2')),
                                  'Lochsa' = list('LRL' = list('LRL',
                                                               'FISTRP')),
                                  'SFClearwater' = list('SC1' = list('SC1',
                                                                     'SC2' = list('SC2',
                                                                                  'CROTRP',
                                                                                  'CRT',
                                                                                  'REDTRP',
                                                                                  'REDR',
                                                                                  'RRT'))),
                                  'ClearCreek' = list('CLC' = list('CLC',
                                                                   'KOOS')),
                                  'Selway' = list('SW1'))

  bin_list[['SW_Washington']] = list('Tucannon' = list('LTR' = list('LTR',
                                                                    'MTR' = list('MTR',
                                                                                 'UTR' = list('UTR',
                                                                                              'TUCH',
                                                                                              'TFH')))),
                                     'Almota' = list('ALMOTC'),
                                     'Penawawa' = list('PENAWC'),
                                     'Asotin' = list('ACM' = list('ACM',
                                                                  'GEORGC',
                                                                  'ASOTIC' = list('ASOTIC',
                                                                                  'ACB' = list('ACB',
                                                                                               'AFC',
                                                                                               'CCA')))))

  bin_list[['NE_Oregon']] = list('JosephCreek' =
                                   list('JOC' = list('JOC',
                                                     'JOSEPC')),
                                 'GrandeRonde' =
                                   list('UpperGrandeRonde' =
                                          list('UGR' =
                                                 list('UGR',
                                                      'GRANDW',
                                                      'CCW' =
                                                        list('CCW',
                                                             'CATHEW')))),
                                 'Wallowa' = list('LOOKGC',
                                                  'LOOH',
                                                  'WR1' =
                                                    list('WR1',
                                                         'BCANF',
                                                         'LOSTIW',
                                                         'WALH')))

  bin_list[['Imnaha']] = list('CowCreek' =
                                list('COC'),
                              'ImnahaRiver' =
                                list('IR1' =
                                       list('IR1',
                                            'IR2' =
                                              list('IR2',
                                                   'CMP',
                                                   'BSC',
                                                   'LSHEEF',
                                                   'HORS3C',
                                                   'IR3' =
                                                     list('IR3',
                                                          'CZY',
                                                          'FREEZC',
                                                          'MAHOGC',
                                                          'IR4' =
                                                            list('IR4',
                                                                 'IML' = list('IML',
                                                                              'IMNAHW' =
                                                                                list('IMNAHW',
                                                                                     'IR5' =
                                                                                       list('IR5',
                                                                                            'GUMBTC',
                                                                                            'DRY2C')))))))))

  bin_list[['Salmon']] = list('SFSalmon' =
                                list('SFG' =
                                       list('SFG',
                                            'ZEN' =
                                              list('ZEN',
                                                   'LAKEC'),
                                            'KRS' =
                                              list('KRS',
                                                   'STR'),
                                            'ESS' =
                                              list('ESS',
                                                   'JOHNSC'))),
                              'BigCreek' =
                                list('TAY',
                                     'BIG2C'),
                              'NFSalmon' =
                                list('NFS'),
                              'CarmenCreek' =
                                list('CRC'),
                              'Lemhi' =
                                list('LLR' =
                                       list('LLR',
                                            'AGC',
                                            'KEN',
                                            'HYC' =
                                              list('HYC',
                                                   'HBC'),
                                            'WPC',
                                            'BHC',
                                            'LRW' =
                                              list('LRW',
                                                   'LLS',
                                                   'BTL' =
                                                     list('BTL',
                                                          'BTM' =
                                                            list('BTM',
                                                                 'BTU')),
                                                   'LCL',
                                                   'LB8',
                                                   'LBS',
                                                   'CAC',
                                                   '18M',
                                                   'HEC'))),
                              'UpperSalmon' = list('USE' =
                                                     list('USE',
                                                          'USI' =
                                                            list('USI',
                                                                 'VC2' =
                                                                   list('VC2',
                                                                        'VC1'),
                                                                 'YFK' =
                                                                   list('YFK',
                                                                        'YANKFK',
                                                                        'CEY'),
                                                                 'STL' =
                                                                   list('STL',
                                                                        'SAWT'),
                                                                 'SALEFT',
                                                                 'PAHH'))))

  # SC1 (SC), IR1 (IR), IR4 (IR), LRW (LRW1), USI (USI1)

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
