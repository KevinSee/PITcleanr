# Author: Kevin See
# Purpose: Test new functions for processing PTAGIS data for DABOM
# Created: 2/10/2021
# Last Modified: 9/13/2023
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(sf)
library(nhdplusTools)
library(magrittr)
library(here)
# library(PITcleanr)
devtools::load_all()

#-----------------------------------------------------------------
# download all the metadata for all sites from PTAGIS
org_config = buildConfig()

#-----------------------------------------------------------------
# designate a starting point
root_site = c("LGR",
              "PRA",
              "PRO",
              "TUM")[1]

#-----------------------------------------------------------------
# update configuration file depending on which DABOM model is being run

#-----------------------------------------------------------------
# Lower Granite - LGR
#-----------------------------------------------------------------
if(root_site == "LGR") {
  # configuration = org_config %>%
  #   mutate(node = if_else(site_code == "GRA",
  #                         "LGR",
  #                         node),
  #          node = ifelse(site_code %in% c('VC2', 'VC1', 'LTR', 'MTR', 'UTR'),
  #                        site_code,
  #                        node),
  #          node = ifelse(site_code == 'SC2',
  #                        'SC2_D',
  #                        node),
  #          node = ifelse(site_code %in% c('CROTRP',
  #                                         'CRT',
  #                                         'REDTRP',
  #                                         'REDR',
  #                                         'RRT'),
  #                        'SC2_U',
  #                        node),
  #          node = ifelse(node == 'ACB',
  #                        'ACB_D',
  #                        node),
  #          node = ifelse(node == 'CCA',
  #                        'CCA_D',
  #                        node),
  #          node = ifelse(site_code == 'AFC',
  #                        ifelse(grepl('MAINSTEM', antenna_group),
  #                               'AFC_D',
  #                               'AFC_U'),
  #                        node),
  #          node = ifelse(site_code == 'HBC',
  #                        'HYC_U',
  #                        node),
  #          node = ifelse(site_code == 'MCCA',
  #                        'STR',
  #                        node),
  #          node = ifelse(site_code == 'CARMEC',
  #                        'CRC_U',
  #                        node),
  #          node = ifelse(site_code == 'BIG2C',
  #                        'TAY_U',
  #                        node),
  #          node = ifelse(site_code == 'WIMPYC',
  #                        'WPC_U',
  #                        node),
  #          node = ifelse(site_code == 'IML' & config_id == 130 & antenna_id == '09',
  #                        'IML_U',
  #                        node),
  #          node = str_replace(node, '^BTC', 'BTL'),
  #          node = ifelse(site_code %in% c('YANKFK', 'CEY'),
  #                        'YFK_U',
  #                        node),
  #          node = ifelse(site_code == 'SAWT',
  #                        'STL',
  #                        node),
  #          node = ifelse(site_code == 'LOOH',
  #                        'LOOKGC',
  #                        node),
  #          node = ifelse(site_code == 'RPDTRP',
  #                        'RAPH',
  #                        node),
  #          node = ifelse(site_code == 'CHARLC',
  #                        'CCA_D',
  #                        node),
  #          node = ifelse(node == 'KEN',
  #                        'KEN_D',
  #                        node),
  #          node = ifelse(node == 'HYC',
  #                        'HYC_D',
  #                        node),
  #          node = ifelse(node == 'YFK',
  #                        'YFK_D',
  #                        node),
  #          node = ifelse(node == 'LLR',
  #                        'LLR_D',
  #                        node),
  #          node = ifelse(node == 'LRW',
  #                        'LRW_D',
  #                        node),
  #          node = ifelse(site_code == '18M',
  #                        str_replace(node, '18M', 'HEC'),
  #                        node),
  #          node = if_else(site_code == "LC1",
  #                         "LC1_D",
  #                         node),
  #          node = if_else(site_code == "LC2",
  #                         "LC1_U",
  #                         node),
  #          node = if_else(site_code == "VC1",
  #                         "VC1_D",
  #                         node),
  #          node = if_else(site_code == "VC2",
  #                         "VC1_U",
  #                         node),
  #          node = if_else(site_code == "SW1",
  #                         "SW1_D",
  #                         node),
  #          node = if_else(site_code == "SW2",
  #                         "SW1_U",
  #                         node),
  #          node = if_else(site_code == "IR1",
  #                         "IR1_D",
  #                         node),
  #          node = if_else(site_code == "IR2",
  #                         "IR1_U",
  #                         node),
  #          node = if_else(site_code == "IMNAHW",
  #                         "IML_U",
  #                         node),
  #          node = if_else(site_code == "SC1",
  #                         "SC1_D",
  #                         node),
  #          node = if_else(site_code == "SC2",
  #                         "SC1_U",
  #                         node),
  #          node = if_else(site_code == "LRL",
  #                         "LRL_D",
  #                         node),
  #          node = if_else(site_code == "LRU",
  #                         "LRL_U",
  #                         node),
  #          node = if_else(site_code == "CLC",
  #                         "CLC_D",
  #                         node),
  #          node = if_else(site_code == "KOOS",
  #                         "CLC_U",
  #                         node),
  #          node = if_else(site_code == "CATHEW",
  #                         "CCW_U",
  #                         node),
  #          node = if_else(site_code == "GRANDW",
  #                         "UGS_U",
  #                         node),
  #          node = if_else(site_code == "TUCH",
  #                         "TFH_D",
  #                         node))
  #
  #   sites_df = writeOldNetworks()$LowerGranite |>
  #     select(site_code = SiteID,
  #            path) |>
  #     mutate(across(path,
  #                   ~ str_replace_all(path, "\\.", " ")),
  #            across(site_code,
  #                   ~ recode(.,
  #                            "GRA" = "LGR")))
  #

  configuration = org_config %>%
    mutate(
      node = case_when(
        # Lower Snake and Columbia dams, upstream to downstream
        site_code %in% c("GRA", "LGRLDR", "LGR") ~ "LGR",             # LGR Adults
        site_code %in% c("GRJ", "GRX", "GRS") ~ "GRS",                # LGR Juveniles; GRJ, GRX = LGR Juvenile Bypass; GRS = LGR Spillway
        site_code %in% c("LGS", "GOJ", "GOA") ~ "GOA",                # Little Goose Dam
        site_code %in% c("LMN", "LMJ", "LMA") ~ "LMA",                # Lower Monumental Dam
        site_code %in% c("IHR", "ICH", "IHA") ~ "IHR",                # Ice Harbor Dam
        site_code %in% c("MCN", "MCJ", "MCX", "MC1", "MC2") ~ "MCN",  # McNary Dam
        site_code %in% c("JDJ", "JDA", "JO1", "JO2") ~ "JDA",         # John Day Dam
        site_code %in% c("TDA", "TD1", "TD2") ~ "TDA",                # The Dalles Dam
        site_code %in% c("B2A", "BONAFF", "BO1", "BO2", "BON", "BVJ",
                         "B1J", "BVX", "B2J", "BO4", "BWL", "BO3",
                         "BCC") ~ "BON",                              # Bonneville Dam
        TRUE ~ node)) %>%
    mutate(
      node = case_when(
        # recode primary subbasins and tributaries outside the Snake River basin; using the "\\d+" ensures that we search for
        # all consecutive digits prior to a "." i.e., some sites in the Upper Columbia have a 4-digit start to their rkm.
        # Then "\\.\\d+" ensures we search for consecutive digits after a ".".
        as.numeric(str_extract(rkm, "\\d+")) > 539 ~ "UpperColumbia",
        as.numeric(str_extract(rkm, "\\d+")) == 539 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "Yakima",
        as.numeric(str_extract(rkm, "\\d+")) == 509 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "WallaWalla",
        as.numeric(str_extract(rkm, "\\d+")) == 465 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "Umatilla",
        as.numeric(str_extract(rkm, "\\d+")) == 351 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "JohnDay",
        as.numeric(str_extract(rkm, "\\d+")) == 328 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "Deschutes",
        as.numeric(str_extract(rkm, "\\d+")) == 290 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "Klickitat",
        as.numeric(str_extract(rkm, "\\d+")) == 273 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "HoodRiver",
        as.numeric(str_extract(rkm, "\\d+")) == 271 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "WhiteSalmon",
        as.numeric(str_extract(rkm, "\\d+")) == 261 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "LittleWhite",
        as.numeric(str_extract(rkm, "\\d+")) == 251 & as.numeric(sub(".","", str_extract(rkm, "\\.\\d+"))) > 0 ~ "WindRiver",
        TRUE ~ node)) %>%
    mutate(
      node = case_when(
        site_code %in% c("DWL", "DWOR")                            ~ "DWL",    # Dworshak National Fish Hatchery
        site_code %in% c("CRT", "CROTRP")                          ~ "CRT",    # Crooked River Trap
        site_code %in% c("REDTRP", "REDR")                         ~ "RRT",    # Red River Trap
        site_code == "AFC" &  grepl("MAINSTEM", antenna_group)     ~ "AFC_D",  # mainstem Asotin becomes _D
        site_code == "AFC" & !grepl("MAINSTEM", antenna_group)     ~ "AFC_U",  # south and north forks become _U
        site_code == "TUCH"                                        ~ "TFH_U",  # change Tucannon Hatchery to _U; still need to sort this one
        site_code %in% c("MCCA", "SALSFW")                         ~ "SALSFW", # South Fork Salmon Weir
        site_code == "CARMEC"                                      ~ "CRC_U",  # Carmen Creek; differentiates creek from weir; still need to sort A0s, B0s, etc.
        site_code == "BIG2C"                                       ~ "TAY_U",  # Big Creek
        site_code == "WIMPYC"                                      ~ "WPC_U",  # Wimpey Creek (Lemhi)
        site_code == "IML" & config_id == 130 & antenna_id == "09" ~ "IML_U",  # Imnaha River Work Room Antenna
        site_code %in% c("YANKFK", "CEY")                          ~ "YFK_U",  # Yankee Fork and Cearley Creek
        site_code == "LOOKGC"                                      ~ "LOOH",   # Group Lookingglass Creek w/ Lookingglass Hatchery
        site_code == "RPDTRP"                                      ~ "RAPH",   # Group Rapid trap with Rapid Hatchery
        site_code == "CHARLC"                                      ~ "CCA_U",  # Change Charley Creek observations to CCA_U
        site_code == "BEARVC"                                      ~ "BRC",    # Group Bear Valley adult weir w/ BRC
        site_code == "POTREF"                                      ~ "EFPW",   # Group EF Potlatch River w/ weir
        TRUE ~ node
      )) %>%
    mutate(
      node = str_replace(node, "^BTC", "BTL"),                                   # Group together Big Timber Creek
      node = ifelse(site_code == "18M", str_replace(node, "^18M", "HEC"), node)  # Group together Hawley Creek and 18-mile Creek
    )

  # assign some downstream tribs to a PTAGIS site
  configuration <-
    configuration |>
    mutate(
      across(
        node,
        ~ recode(.,
                 "Deschutes" = "DRM",
                 "HoodRiver" = "FID",
                 "JohnDay" = "JD1",
                 "Klickitat" = "KLR",
                 "LittleWhite" = "LWL",
                 "Umatilla" = "UMW",
                 "UpperColumbia" = "PRA",
                 "WallaWalla" = "WWB",
                 "WhiteSalmon" = "RCX",
                 "WindRiver" = "WRA",
                 "Yakima" = "PRO")))

  pc <- read_csv(here("inst/extdata/updated_data/parent_child_GRA.csv"),
                 show_col_types = F) |>
    mutate(
      across(
        c(parent, child),
        ~ recode(.,
                 "GRA" = "LGR",
                 "Deschutes" = "DRM",
                 "HoodRiver" = "FID",
                 "JohnDay" = "JD1",
                 "Klickitat" = "KLR",
                 "LittleWhite" = "LWL",
                 "Umatilla" = "UMW",
                 "UpperColumbia" = "PRA",
                 "WallaWalla" = "WWB",
                 "WhiteSalmon" = "RCX",
                 "WindRiver" = "WRA",
                 "Yakima" = "PRO")))

  sites_df <-
    buildPaths(pc, direction = "u") |>
    rename(site_code = end_loc) |>
    bind_rows(tibble(site_code = "LGR",
                     path = "LGR"))

}

#-----------------------------------------------------------------
# Priest Rapids - PRA
#-----------------------------------------------------------------
if(root_site == "PRA") {
  configuration = org_config %>%
    # # manually add site for Colockum Creek (not in PTAGIS)
    # bind_rows(tibble(site_code = 'CLK',
    #                  config_id = 100,
    #                  antenna_id = 'A1',
    #                  node = 'CLK',
    #                  # making these up
    #                  start_date = as.POSIXct(lubridate::ymd('20100101')),
    #                  site_type = 'INT',
    #                  site_name = 'Colockum Creek',
    #                  antenna_group = 'Single Colockum Ck',
    #                  site_description = 'Temporary single antenna.',
  #                  site_type_name = 'Instream Remote Detection System',
  #                  rkm = '740.001',
  #                  rkm_total = 741,
  #                  # this puts CLK upstream of RIA
  #                  latitude = 47.3707357269787,
  #                  longitude = -120.25617371760839)) %>%
  filter(!(site_code == 'WAN' & site_type == 'MRR'),
         !(site_code == 'TMF' & site_type == 'MRR'),
         !(site_code == 'PRO' & site_type == 'MRR')) %>%
    mutate(node = if_else(site_code %in% c('RIA', 'RRF', 'WEA', 'PRV'),
                          site_code,
                          node)) %>%
    mutate(node = if_else(site_code == 'PRDLD1',
                          'PRA',
                          node)) %>%
    mutate(node = if_else(site_code == "LWB",
                          "LWE_D",
                          node),
           node = if_else(site_code %in% c('TUF', 'TUMFBY', 'TUM'),
                          'TUM',
                          node),
           node = if_else(site_code == 'LNF' & antenna_id %in% c('01'),
                          'LNF_U',
                          node),
           node = if_else(site_code == 'LEAV',
                          'LNF_U',
                          node),
           node = if_else(site_code == 'CHIWAC',
                          'CHW_U',
                          node),
           node = if_else(site_code == 'CHIWAR',
                          'CHL_U',
                          node),
           node = if_else(site_code == 'CHIKAC',
                          'CHU_U',
                          node),
           node = if_else(site_code == 'WHITER',
                          'WTL_U',
                          node),
           node = if_else(site_code == 'LWENAT',
                          'LWN_U',
                          node),
           node = if_else(site_code == 'NASONC',
                          'NAL_U',
                          node),
           # any fish seen at Dryden dam should also be seen at LWE
           node = if_else(site_code == 'DRY',
                          'LWE_U',
                          node),
           node = if_else(site_code == 'RRJ',
                          'RRF',
                          node),
           node = if_else(site_code == "MAD" & config_id == 110 & antenna_id == "01",
                          "MAD_U",
                          node),
           node = if_else(site_code == 'EHL',
                          case_when(config_id == 100 & antenna_id == '02' ~ "EHL_D",
                                    config_id == 100 & antenna_id == '01' ~ "EHL_U",
                                    config_id == 110 & antenna_id == '03' ~ "EHL_D",
                                    config_id == 110 & antenna_id %in% c('01', '02') ~ "EHL_U"),
                          node),
           node = if_else(site_code == "WEH",
                          if_else(antenna_id == "A2",
                                  "WEH_D",
                                  "WEH_U"),
                          node),
           node = if_else(site_code == "LMB",
                          "LMR_D",
                          node),
           node = if_else(site_code == 'LBC' & config_id == 100,
                          'LBC_D',
                          node),
           node = if_else(site_code == 'MRC',
                          'MRC_D',
                          node),
           node = if_else(site_code %in% c('SSC', '18N', 'MHB', 'M3R', 'MWF'),
                          'MRC_U',
                          node),
           node = if_else(site_code == 'MSH' & antenna_id == '00',
                          'METH_D',
                          node),
           node = if_else(site_code == 'METH',
                          'METH_U',
                          node),
           node = if_else(site_code == 'LLC' & config_id == 100,
                          if_else(antenna_id == 'D3',
                                  'LLC_D',
                                  'LLC_U'),
                          node),
           node = if_else(site_code == 'ZSL',
                          if_else(grepl('Weir 3', antenna_group, ignore.case = T),
                                  'ZSL_D',
                                  'ZSL_U'),
                          node),
           node = if_else(site_code == 'BPC' & config_id == 100,
                          if_else(antenna_id %in% c('C3'),
                                  'BPC_D',
                                  'BPC_U'),
                          node),
           node = if_else(site_code == 'PRH',
                          case_when(antenna_id %in% c('F1', 'F2', 'F3', 'F4') ~ "PRH_D",
                                    antenna_id %in% c('F5', 'F6', '01', '02') ~ "PRH_U"),
                          node),
           node = if_else(site_code == 'PRO' & site_type == 'INT',
                          'PRO_D',
                          node),
           # grab all sites upstream of Prosser dam, and assign them to PRO_U
           node = if_else(site_code != "PRO" &
                            as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) == 539 &
                            as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,2]) >= 76,
                          "PRO_U",
                          node),
           node = if_else(site_code == 'ICH',
                          'ICH_D',
                          node),
           node = if_else(grepl('522\\.', rkm) & rkm_total > 538,
                          'ICH_U',
                          node),
           node = if_else(site_code == 'MDR',
                          'MDR_D',
                          node),
           node = if_else(site_code %in% c('LWD', 'BGM', 'NBA', 'MCD'),
                          "MDR_U",
                          node),
           node = if_else(site_code == 'HST',
                          'HST_D',
                          node),
           node = if_else(site_code %in% c('BBT', 'COP', 'PAT'),
                          'HST_U',
                          node),
           # node = if_else(site_code %in% c('30M', 'BR0', 'JDM', 'SJ1', 'SJ2', 'MJ1', 'RCJ'),
           #                'JD1_U',
           #                node),
           node = if_else(as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) == 351,
                          "JD1_U",
                          node),
           node = if_else(site_code == 'JD1',
                          'JD1_D',
                          node),
           node = if_else(site_code != 'JD1' & as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) < 351,
                          'JDA',
                          node)) %>%
    distinct() %>%
    # correct a couple rkm values
    mutate(rkm = if_else(site_code == 'SA1',
                         '858.041.003',
                         rkm),
           rkm_total = if_else(site_code == 'SA1',
                               902,
                               rkm_total)) %>%
    mutate(rkm = if_else(site_code == 'TON',
                         '858.133.001',
                         rkm),
           rkm_total = if_else(site_code == 'TON',
                               992,
                               rkm_total)) %>%
    mutate(rkm = if_else(grepl('WEH', node),
                         '829.001',
                         rkm),
           rkm_total = if_else(grepl('WEH', node),
                               830,
                               rkm_total)) %>%
    mutate(rkm = if_else(site_code == "MSH",
                         '843.082',
                         rkm),
           rkm_total = if_else(site_code == "MSH",
                               925,
                               rkm_total),
           rkm = if_else(site_code == "METH",
                         '843.083',
                         rkm),
           rkm_total = if_else(site_code == "METH",
                               926,
                               rkm_total))

  sites_df = writeOldNetworks()$PriestRapids |>
    select(site_code = SiteID,
           path) |>
    mutate(across(path,
                  ~ str_replace_all(path, "\\.", " ")),
           across(site_code,
                  ~ recode(.,
                           "BelowJD1" = "JDA")),
           across(path,
                  ~ str_replace_all(., "BelowJD1", "JDA")))
}


#-----------------------------------------------------------------
# Prosser - PRO
#-----------------------------------------------------------------
if(root_site == "PRO") {
  # customize some nodes based on DABOM framework
  configuration = org_config %>%
    mutate(node = if_else(site_code %in% c('PRO'),
                          'PRO',
                          node),
           node = if_else(site_code %in% c("NFTEAN", "TEANAR", "TEANM", "TEANWF"),
                          "LMT_U",
                          node),
           node = if_else(site_code == 'ROZ',
                          if_else(antenna_id %in% c('01', '02', '03'),
                                  node,
                                  as.character(NA)),
                          node),
           node = if_else(site_code == 'TAN' & config_id %in% c(120, 130),
                          "TAN_D",
                          node),
           node = if_else(site_code %in% c('MC1', 'MC2', 'MCJ', 'MCN'),
                          'MCN',
                          node),
           node = if_else(site_code == 'ICH',
                          'ICH_D',
                          node),
           node = if_else(grepl('522\\.', rkm) & rkm_total > 538,
                          'ICH_U',
                          node),
           node = if_else(site_code == 'JD1',
                          'JD1_D',
                          node),
           node = if_else(site_code %in% c('30M', 'BR0', 'JDM', 'SJ1', 'SJ2', 'MJ1'),
                          'JD1_U',
                          node),
           node = if_else(site_code != 'JD1' & as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) < 351,
                          'JDA',
                          node),
           node = if_else(site_code == 'PRA',
                          'PRA_D',
                          node),
           node = if_else(site_code != 'PRA' & as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) >= 639,
                          'PRA_U',
                          node)) %>%
    # add a missing lat/long
    mutate(latitude = if_else(site_code == "SWK",
                              47.210348,
                              latitude),
           longitude = if_else(site_code == "SWK",
                               -120.699021,
                               longitude))

  sites_df = writeOldNetworks()$Prosser |>
    select(site_code = SiteID,
           path) |>
    mutate(across(path,
                  ~ str_replace_all(path, "\\.", " ")),
           across(site_code,
                  ~ recode(.,
                           "BelowJD1" = "JDA")),
           across(path,
                  ~ str_replace_all(., "BelowJD1", "JDA")))
}

#-----------------------------------------------------------------
# Tumwater - TUM
#-----------------------------------------------------------------
if(root_site == "TUM") {
  configuration = org_config %>%
    mutate(node = ifelse(site_code %in% c('LNF', 'LEAV'),
                         'LNF',
                         node),
           node = ifelse(site_code %in% c('TUF', 'TUMFBY', 'TUM'),
                         'TUM',
                         node),
           node = ifelse(site_code == 'CHIWAC',
                         'CHW_U',
                         node),
           node = ifelse(site_code == 'CHIWAR',
                         'CHL_U',
                         node),
           node = ifelse(site_code == 'CHIW',
                         'CHL_U',
                         node),
           node = ifelse(site_code == 'CHIKAC',
                         'CHU_U',
                         node),
           node = ifelse(site_code == 'WHITER',
                         'WTL_U',
                         node),
           node = ifelse(site_code == 'LWENAT',
                         'LWN_U',
                         node),
           node = ifelse(node == 'ICL',
                         'ICL_D',
                         node),
           node = ifelse(site_code == 'NASONC',
                         'NAL_U',
                         node)) %>%
    distinct()

  sites_df = writeOldNetworks()$Tumwater |>
    select(site_code = SiteID,
           path) |>
    mutate(across(path,
                  ~ str_replace_all(path, "\\.", " "))) |>
    filter(site_code != "LEAV")
}

#-----------------------------------------------------------------
# make sites spatial
sites_sf <- sites_df |>
  select(site_code) |>
  left_join(configuration) %>%
  group_by(site_code) %>%
  filter(config_id == max(config_id)) %>%
  ungroup() %>%
  select(site_code,
         site_name,
         site_type = site_type_name,
         type = site_type,
         rkm,
         site_description = site_description,
         latitude, longitude) %>%
  distinct() %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude",
                      "latitude"),
           crs = 4326) %>%
  st_transform(crs = 5070)

#-----------------------------------------------------------------
# download the NHDPlus v2 flowlines
# do you want flowlines downstream of root site? Set to TRUE if you have downstream sites
dwn_flw = T
nhd_list = queryFlowlines(sites_sf = sites_sf,
                          root_site_code = root_site,
                          min_strm_order = 2,
                          dwnstrm_sites = dwn_flw,
                          dwn_min_stream_order_diff = 4)

# compile the upstream and downstream flowlines
flowlines = nhd_list$flowlines
if(dwn_flw) {
  flowlines %<>%
    rbind(nhd_list$dwn_flowlines)
}

if(root_site %in% c("LGR", "PRA")) {
  # upstream extent of study area (cut off areas further upstream)
  upstrm_loc = case_when(root_site == "PRA" ~ "Chief Joseph Dam",
                         root_site == "LGR" ~ "Hells Canyon Dam")

  library(ggmap)

  upstrm_comid = ggmap::geocode(upstrm_loc, output = "latlon") %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = 4326) %>%
    nhdplusTools::discover_nhdplus_id()

  nhd_upstrm_lst = nhdplusTools::plot_nhdplus(outlets = list(upstrm_comid),
                                              streamorder = min(nhd_list$flowlines$StreamOrde),
                                              actually_plot = F)

  flowlines <-
    flowlines |>
    anti_join(nhd_upstrm_lst$flowline |>
                st_drop_geometry() |>
                select(Hydroseq))
}

#-----------------------------------------------------------------
# plot the flowlines and the sites
ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(StreamOrde),
              size = StreamOrde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        name = "Stream\nOrder",
                        end = 0.8) +
  scale_size_continuous(range = c(0.2, 1.2),
                        guide = 'none') +
  # geom_sf(data = nhd_list$basin,
  #         fill = NA,
  #         lwd = 2) +
  # # this cuts out parts of the basin upstream of upstrm_loc
  # geom_sf(data = flowlines %>%
  #           filter(!Hydroseq %in% nhd_list$dwn_flowlines$Hydroseq) %>%
  #           summarise(bndry = 'basin') %>%
  #           select(bndry) %>%
  #           st_convex_hull(),
  #         fill = NA,
  #         lwd = 2) +
geom_sf(data = sites_sf,
        size = 3,
        color = "black") +
  # geom_sf_label(data = sites_sf,
  #               size = 1.5,
  #               aes(label = site_code)) +
  ggrepel::geom_label_repel(
    data = sites_sf |>
      filter(site_code != root_site),
    aes(label = site_code,
        geometry = geometry),
    size = 1.5,
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = 100
  ) +
  geom_sf_label(data = sites_sf %>%
                  filter(site_code == root_site),
                aes(label = site_code),
                color = "red") +
  theme_bw() +
  theme(axis.title = element_blank())

#-----------------------------------------------------------------
# build parent child table
if(root_site == "PRO") {
  parent_child = sites_sf %>%
    buildParentChild(flowlines,
                     rm_na_parent = F,
                     add_rkm = F) |>
    editParentChild(fix_list =
                      list(c(NA, "JDA", "PRO"),
                           c("JDA", "MCN", "PRO"),
                           c("JDA", "JD1", "PRO"),
                           c("MCN", "ICH", "PRO"),
                           c("MCN", "PRA", "PRO")),
                    switch_parent_child = list(c("MCN", "PRO")))
} else if(root_site == "TUM") {
  parent_child = sites_sf %>%
    buildParentChild(flowlines,
                     rm_na_parent = F,
                     add_rkm = F) %>%
    editParentChild(fix_list = list(c(NA, "ICL", "TUM"),
                                    c(NA, "PES", "TUM"),
                                    c(NA, "LNF", "ICL")),
                    switch_parent_child = list(c("ICL", 'TUM')))
} else if(root_site == "PRA") {
  parent_child = sites_sf %>%
    buildParentChild(flowlines,
                     rm_na_parent = F,
                     add_rkm = F) %>%
    editParentChild(fix_list = list(c("JDA", 'ICH', "PRA"),
                                    c("JDA", 'RSH', "PRA"),
                                    c("JDA", 'JD1', "PRA"),
                                    c("JDA", 'PRO', "PRA"),
                                    c("JDA", 'TMF', "PRA"),
                                    c("JDA", 'PRV', "PRA"),
                                    c(NA, "JDA", 'PRA'),
                                    c("RSH", 'PRH', 'PRA'),
                                    c("ICL", 'TUM', "LWE"),
                                    c("LNF", 'ICM', "ICL"),
                                    c(NA, "LNF", "ICL"),
                                    c("RIA", "WEA", 'RRF'),
                                    c("RIA", "WEH", 'RRF'),
                                    c("RIA", "ENL", "RRF"),
                                    c("RIA", "EBO", "RRF"),
                                    c("EBO", "WEH", 'RRF'),
                                    c("EBO", "WEA", 'RRF'),
                                    c("EBO", "ENL", 'RRF'),
                                    c("WEH", "LMR", "WEA"),
                                    c("WEH", "OKL", "WEA"),
                                    c("WEH", "FST", 'WEA'),
                                    c("EHL", 'ENA', 'ENL'),
                                    c("EHL", 'MAD', 'ENL'),
                                    c("METH", "MRW", "MRC"),
                                    c("SCP", "METH", "MSH"),
                                    c("SCP", 'MSH', 'MRC'),
                                    c("SCP", "WINT", "MRC"),
                                    c("WHS", "OKC", "ZSL"),
                                    c("WHS", "ZSL", "OKL"),
                                    c("OMK", "OMF", "OBF"),
                                    c("OBF", "OMH", "OMF"),
                                    c("ZSL", 'OKV', 'OKC'),
                                    c("ZSL", "OKM", "OKC"),
                                    c("OKC", "SKA", "OKM"),
                                    c("OKC", "OKW", "OKM"),
                                    c("OKC", "OKS", "SKA"),
                                    c("OKC", "OKP", "SKA"),
                                    c("JOH", 'WHS', 'OKL'),
                                    c("JOH", 'BPC', 'OKL'),
                                    c("JOH", 'ANT', 'OKL'),
                                    c("JOH", 'TNK', 'OKL'),
                                    c("JOH", 'AEN', 'OKL')),
                    switch_parent_child = list(c("RSH", "PRA"))) %>%
    filter(!parent %in% c("WEH", "PRH"))
} else if(root_site == "LGR") {
  parent_child = sites_sf %>%
    buildParentChild(flowlines,
                     rm_na_parent = F,
                     add_rkm = F) |>
    editParentChild(fix_list =
                      list(c(NA, "LTR", "LGR"),
                           c(NA, "ALMOTC", "LGR"),
                           c(NA, "PENAWC", "LGR"),
                           c("IR4", "IR5", "IMNAHW"),
                           c("IML", "IR5", "IMNAHW"),
                           c("IR3", "IML", "IR4"),
                           c("IR3", "IMNAHW", "IML"),
                           c("ASOTIC", "AFC", "ACB"),
                           c("ASOTIC", "CCA", "ACB"),
                           c("ACM", "ACB", "ASOTIC"),
                           c("ACB", "ASOTIC", "ACM"),
                           c("KEN", "LRW", "LLR"),
                           c("KEN", "HYC", "LLR"),
                           c("KEN", "AGC", "LLR"),
                           c("LLS", "LBS", "LRW"),
                           c("LLS", "LB8", "LRW"),
                           c("LLS", "LCL", "LRW"),
                           c("LBS", "BTL", "LRW"),
                           c("LBS", "CAC", "LRW"),
                           c("LBS", "HEC", "LRW"))) |>
    filter(!is.na(parent))

  parent_child = pc
}


# plotNodes(parent_child,
#           layout = "tree")
#
# site_paths <- buildPaths(parent_child)
# site_paths |>
#   filter(str_detect(path, "ACM"))

#-----------------------------------------------------------------
# write configuration & parent-child table
configuration |>
  write_csv(here("inst/extdata/updated_data",
                 paste0(root_site, "_configuration.csv")))

parent_child |>
  write_csv(here("inst/extdata/updated_data",
                 paste0(root_site, "_parent_child.csv")))

save(configuration,
     sites_sf,
     flowlines,
     parent_child,
     file = here("inst/extdata/updated_data",
                 paste0(root_site, "_site_config.Rdata")))
