#' @title Define Report Groups
#'
#' @description Define which detection sites fall into which report groups, and what the lowest array is. These reporting gropus differ by species.
#'
#' @author Kevin See
#'
#' @param spp Species, either "Chinook" or "Steelhead"
#' @param node_order output of function \code{createNodeOrder}.
#' @param root_site initial tagging site. Currently only defined for \code{LGD} (Lower Granite dam)
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples defineRepGrps()

defineRepGrps = function(spp = c('Chinook', 'Steelhead'),
                         node_order = NULL,
                         root_site = 'LGD') {

  stopifnot(!is.null(node_order))

  spp = match.arg(spp)

  if(spp == 'Chinook') {

    report_df = list('Upper Salmon River' = 'USE',
                     'Sawtooth Hatchery Weir' = 'STL',
                     'Valley Creek' = 'VC2',
                     'Yankee Fork Salmon River' = 'YFKB0',
                     'East Fork Salmon River' = 'SALEFT',
                     'Pahsimeroi River' = 'PAHH',
                     'Lemhi River' = 'LLRB0',
                     'Carmen Creek' = 'CRCB0',
                     'North Fork Salmon River' = 'NFSB0',
                     'Bear Valley Creek' = NA,
                     'Big Creek' = 'TAYB0',
                     'South Fork Salmon River' = 'SFG',
                     'McCall Hatchery Weir' = 'STR',
                     'South Fork Salmon River mainstem' = 'KRS',
                     'EFSF Salmon River' = 'ESSB0',
                     'Secesh River' = 'ZENB0',
                     'Rapid River' = 'RAPH',
                     'Lolo River' = 'LC1',
                     'South Fork Clearwater' = 'SC1',
                     'Clear Creek' = 'CLC',
                     'Imnaha River' = 'IR1',
                     'Cow Creek' = 'COCB0',
                     'Big Sheep Creek' = c('BSCB0', 'LSHEEF'),
                     'Grande Ronde River upper mainstem' = 'UGR',
                     'Catherine Creek' = 'CCWB0',
                     'Wallowa River' = 'WR1',
                     'Lookingglass Creek' = 'LOOKGC',
                     'Asotin Creek' = 'ACMB0',
                     'Tucannon River' = 'LTR',
                     'Tucannon Hatchery Weir' = 'TUCH_TFH') %>%
      stack() %>%
      tbl_df() %>%
      select(ReportGrp = ind,
             lowNode = values)

  }

  if(spp == 'Steelhead') {

    report_df = list('Upper Salmon River' = 'USE',
                     'Sawtooth Hatchery Weir' = 'STL',
                     'Upper Salmon River mainstem' = c('YFKB0', 'VC2', 'STL'),
                     'Valley Creek' = 'VC2',
                     'Yankee Fork Salmon River' = 'YFKB0',
                     'East Fork Salmon River' = 'SALEFT',
                     'Pahsimeroi River' = 'PAHH',
                     'Lemhi River' = 'LLRB0',
                     'Carmen Creek' = 'CRCB0',
                     'Big Creek' = 'TAYB0',
                     'South Fork Salmon River' = 'SFG',
                     'South Fork Salmon River mainstem' = c('KRS', 'ESSB0'),
                     'Secesh River' = 'ZENB0',
                     'Rapid River' = 'RAPH',
                     'Fish Creek' = 'FISTRP',
                     'Lolo Creek' = 'LC1',
                     'South Fork Clearwater' = 'SC1',
                     'Clear Creek' = 'CLC',
                     'Potlatch River' = 'JUL',
                     'Potlatch above HLM' = 'HLMB0',
                     'Potlatch above KHS' = 'KHSB0',
                     'Lapwai Creek' = 'LAPB0',
                     'Imnaha River' = 'IR1',
                     'Cow Creek' = 'COCB0',
                     'Big Sheep Creek' = 'BSCB0',
                     'Grande Ronde River upper mainstem' = 'UGR',
                     'Lookingglass Creek' = 'LOOKGC',
                     'Wallowa River' = 'WR1',
                     'Joseph Creek' = 'JOCB0',
                     'Asotin Creek' = 'ACMB0',
                     'Tenmile Creek' = 'TENMC2',
                     'Alpowa Creek' = 'ALPOWC',
                     'Tucannon River' = 'LTR',
                     'Tucannon Hatchery Weir' = 'TUCH_TFH') %>%
      stack() %>%
      tbl_df() %>%
      select(ReportGrp = ind,
             lowNode = values)

  }

  report_df = report_df %>%
    left_join(node_order,
              by = c('lowNode' = 'Node'))

  return(report_df)

}

