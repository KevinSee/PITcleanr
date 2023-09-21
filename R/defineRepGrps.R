#' @title Define Report Groups
#'
#' @description Define which detection sites fall into which report groups, and what the lowest array is. These reporting groups differ by species.
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
                     'Yankee Fork Salmon River' = 'YFK_D',
                     'East Fork Salmon River' = 'SALEFT',
                     'Pahsimeroi River' = 'PAHH',
                     'Lemhi River' = 'LLR_D',
                     'Carmen Creek' = 'CRC_D',
                     'North Fork Salmon River' = 'NFS_D',
                     'Bear Valley Creek' = NA,
                     'Big Creek' = 'TAY_D',
                     'South Fork Salmon River' = 'SFG',
                     'McCall Hatchery Weir' = 'STR',
                     'South Fork Salmon River mainstem' = 'KRS',
                     'EFSF Salmon River' = 'ESS_D',
                     'Secesh River' = 'ZEN_D',
                     'Rapid River' = 'RAPH',
                     'Lolo River' = 'LC1',
                     'South Fork Clearwater' = 'SC1',
                     'Clear Creek' = 'CLC',
                     'Imnaha River' = 'IR1',
                     'Cow Creek' = 'COC_D',
                     'Big Sheep Creek' = c('BSC_D', 'LSHEEF'),
                     'Grande Ronde River upper mainstem' = 'UGR',
                     'Catherine Creek' = 'CCW_D',
                     'Wallowa River' = 'WR1',
                     'Lookingglass Creek' = 'LOOKGC',
                     'Asotin Creek' = 'ACM_D',
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
                     'Upper Salmon River mainstem' = c('YFK_D', 'VC2', 'STL'),
                     'Valley Creek' = 'VC2',
                     'Yankee Fork Salmon River' = 'YFK_D',
                     'East Fork Salmon River' = 'SALEFT',
                     'Pahsimeroi River' = 'PAHH',
                     'Lemhi River' = 'LLR_D',
                     'Carmen Creek' = 'CRC_D',
                     'Big Creek' = 'TAY_D',
                     'South Fork Salmon River' = 'SFG',
                     'South Fork Salmon River mainstem' = c('KRS', 'ESS_D'),
                     'Secesh River' = 'ZEN_D',
                     'Rapid River' = 'RAPH',
                     'Fish Creek' = 'FISTRP',
                     'Lolo Creek' = 'LC1',
                     'South Fork Clearwater' = 'SC1',
                     'Clear Creek' = 'CLC',
                     'Potlatch River' = 'JUL',
                     'Potlatch above HLM' = 'HLM_D',
                     'Potlatch above KHS' = 'KHS_D',
                     'Lapwai Creek' = 'LAP_D',
                     'Imnaha River' = 'IR1',
                     'Cow Creek' = 'COC_D',
                     'Big Sheep Creek' = 'BSC_D',
                     'Grande Ronde River upper mainstem' = 'UGR',
                     'Lookingglass Creek' = 'LOOKGC',
                     'Wallowa River' = 'WR1',
                     'Joseph Creek' = 'JOC_D',
                     'Asotin Creek' = 'ACM_D',
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

