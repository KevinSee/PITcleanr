# Author: Kevin See
# Purpose: Test new functions for processing PTAGIS data for DABOM
# Created: 2/10/2021
# Last Modified: 4/19/2021
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(sf)
library(nhdplusTools)
library(magrittr)
# library(PITcleanr)
devtools::load_all()

#-----------------------------------------------------------------
# download all the metadata for all sites from PTAGIS
org_config = buildConfig()

#-----------------------------------------------------------------
# designate a starting point
root_site = "GRA"
# root_site = "PRA"
# root_site = "TUM"
# root_site = "PRO"

#-----------------------------------------------------------------
# Lower Granite
#-----------------------------------------------------------------
# make a few change
if(root_site == "GRA") {
  configuration = org_config %>%
    mutate(node = ifelse(site_code %in% c('VC2', 'VC1', 'LTR', 'MTR', 'UTR'),
                         site_code,
                         node),
           node = ifelse(site_code == 'SC2',
                         'SC2B0',
                         node),
           node = ifelse(site_code %in% c('CROTRP',
                                          'CRT',
                                          'REDTRP',
                                          'REDR',
                                          'RRT'),
                         'SC2A0',
                         node),
           node = ifelse(node == 'ACB',
                         'ACBB0',
                         node),
           node = ifelse(node == 'CCA',
                         'CCAB0',
                         node),
           node = ifelse(site_code == 'AFC',
                         ifelse(grepl('MAINSTEM', antenna_group),
                                'AFCB0',
                                'AFCA0'),
                         node),
           node = ifelse(site_code == 'HBC',
                         'HYCA0',
                         node),
           node = ifelse(site_code %in% c('TUCH', 'TFH'),
                         'TUCH',
                         node),
           node = ifelse(site_code == 'MCCA',
                         'STR',
                         node),
           node = ifelse(site_code == 'CARMEC',
                         'CRCA0',
                         node),
           node = ifelse(site_code == 'BIG2C',
                         'TAYA0',
                         node),
           node = ifelse(site_code == 'WIMPYC',
                         'WPCA0',
                         node),
           node = ifelse(site_code == 'IML' & config_id == 130 & antenna_id == '09',
                         'IMLA0',
                         node),
           node = str_replace(node, '^BTC', 'BTL'),
           node = ifelse(site_code %in% c('YANKFK', 'CEY'),
                         'YFKA0',
                         node),
           node = ifelse(site_code == 'SAWT',
                         'STL',
                         node),
           node = ifelse(site_code == 'LOOH',
                         'LOOKGC',
                         node),
           node = ifelse(site_code == 'RPDTRP',
                         'RAPH',
                         node),
           node = ifelse(site_code == 'CHARLC',
                         'CCAB0',
                         node),
           node = ifelse(node == 'KEN',
                         'KENB0',
                         node),
           node = ifelse(node == 'HYC',
                         'HYCB0',
                         node),
           node = ifelse(node == 'YFK',
                         'YFKB0',
                         node),
           node = ifelse(node == 'LLR',
                         'LLRB0',
                         node),
           node = ifelse(node == 'LRW',
                         'LRWB0',
                         node),
           node = ifelse(site_code == '18M',
                         str_replace(node, '18M', 'HEC'),
                         node)) %>%
    distinct()

  # a few more modifications to make it work better with generic DABOM functions
  configuration %<>%
    mutate(node = if_else(site_code == "LC1",
                          "LC1B0",
                          node),
           node = if_else(site_code == "LC2",
                          "LC1A0",
                          node),
           node = if_else(site_code == "VC1",
                          "VC1B0",
                          node),
           node = if_else(site_code == "VC2",
                          "VC1A0",
                          node),
           node = if_else(site_code == "SW1",
                          "SW1B0",
                          node),
           node = if_else(site_code == "SW2",
                          "SW1A0",
                          node),
           node = if_else(site_code == "IR1",
                          "IR1B0",
                          node),
           node = if_else(site_code == "IR2",
                          "IR1A0",
                          node),
           node = if_else(site_code == "IMNAHW",
                          "IMLA0",
                          node),
           node = if_else(site_code == "SC1",
                          "SC1B0",
                          node),
           node = if_else(site_code == "SC2",
                          "SC1A0",
                          node),
           node = if_else(site_code == "LRL",
                          "LRLB0",
                          node),
           node = if_else(site_code == "LRU",
                          "LRLA0",
                          node),
           node = if_else(site_code == "CLC",
                          "CLCB0",
                          node),
           node = if_else(site_code == "KOOS",
                          "CLCA0",
                          node),
           node = if_else(site_code == "CATHEW",
                          "CCWA0",
                          node),
           node = if_else(site_code == "GRANDW",
                          "UGSA0",
                          node),
           node = if_else(site_code == "TUCH",
                          "TFHB0",
                          node)) %>%
    filter(site_code != "TFH") %>%
    bind_rows(org_config %>%
                filter(site_code == 'TFH'))
}



#-----------------------------------------------------------------
# Priest Rapids
#-----------------------------------------------------------------
# customize some nodes based on DABOM framework
if(root_site == "PRA") {
  configuration = org_config %>%
    # manually add site for Colockum Creek (not in PTAGIS)
    bind_rows(tibble(site_code = 'CLK',
                     config_id = 100,
                     antenna_id = 'A1',
                     node = 'CLK',
                     # making these up
                     start_date = as.POSIXct(lubridate::ymd('20100101')),
                     site_type = 'INT',
                     site_name = 'Colockum Creek',
                     antenna_group = 'Single Colockum Ck',
                     site_description = 'Temporary single antenna.',
                     site_type_name = 'Instream Remote Detection System',
                     rkm = '740.001',
                     rkm_total = 741,
                     # this puts CLK upstream of RIA
                     latitude = 47.3707357269787,
                     longitude = -120.25617371760839)) %>%
    # this puts CLK on Colockum Creek, but between PRA and RIA
    # latitude = 47.29722788926544,
    # longitude = -120.10577913008702)) %>%
    filter(!(site_code == 'WAN' & site_type == 'MRR'),
           !(site_code == 'TMF' & site_type == 'MRR'),
           !(site_code == 'PRO' & site_type == 'MRR')) %>%
    mutate(node = if_else(site_code %in% c('RIA', 'RRF', 'WEA', 'PRV'),
                          site_code,
                          node)) %>%
    mutate(node = if_else(site_code == 'PRDLD1',
                          'PRA',
                          node)) %>%
    mutate(node = if_else(node == "LWE",
                          'LWEB0',
                          node),
           node = if_else(site_code %in% c('TUF', 'TUMFBY', 'TUM'),
                          'TUM',
                          node),
           node = if_else(site_code == 'LNF' & antenna_id %in% c('01', '02'),
                          'LNFA0',
                          node),
           node = if_else(site_code == 'LNF' & antenna_id %in% c('03', '04'),
                          'LNFB0',
                          node),
           node = if_else(site_code == 'LEAV',
                          'LNFA0',
                          node),
           node = if_else(site_code == 'ICL' & config_id == 100,
                          'ICLB0',
                          node),
           node = if_else(site_code == 'CHIWAC',
                          'CHWA0',
                          node),
           node = if_else(site_code == 'CHIWAR',
                          'CHLA0',
                          node),
           node = if_else(site_code == 'CHIKAC',
                          'CHUA0',
                          node),
           node = if_else(site_code == 'WHITER',
                          'WTLA0',
                          node),
           node = if_else(site_code == 'LWENAT',
                          'LWNA0',
                          node),
           node = if_else(site_code == 'NASONC',
                          'NALA0',
                          node),
           # any fish seen at Dryden dam should also be seen at LWE
           node = if_else(site_code == 'DRY',
                          'LWEA0',
                          node),
           # any fish seen at Chiwawa acclimation pond gets moved to CHL
           node = if_else(site_code == 'CHP',
                          'CHLA0',
                          node),
           node = if_else(site_code == 'EBO',
                          'RRF',
                          node),
           node = if_else(site_code == 'RRJ',
                          'RRF',
                          node),
           node = if_else(site_code == 'EHL' & config_id == 100 & antenna_id == '02',
                          'EHLB0',
                          node),
           node = if_else(site_code == 'EHL' & config_id == 100 & antenna_id == '01',
                          'EHLA0',
                          node),
           node = if_else(site_code == 'EHL' & config_id == 110 & antenna_id == '03',
                          'EHLB0',
                          node),
           node = if_else(site_code == 'EHL' & config_id == 110 & antenna_id %in% c('01', '02'),
                          'EHLA0',
                          node),
           node = if_else(site_code == "WEH" & antenna_id == "A2",
                          "WEHB0",
                          node),
           node = if_else(site_code == "WEH" & antenna_id != "A2",
                          "WEHA0",
                          node),
           node = if_else(node == "LMR",
                          'LMRB0',
                          node),
           node = if_else(site_code == 'LBC' & config_id == 100,
                          'LBCB0',
                          node),
           node = if_else(site_code == 'MRC',
                          'MRCB0',
                          node),
           node = if_else(site_code %in% c('SSC', '18N', 'MHB', 'M3R', 'MWF'),
                          'MRCA0',
                          node),
           node = if_else(site_code == 'MSH' & antenna_id %in% c('02', '03'),
                          'MSHB0',
                          node),
           node = if_else(site_code == 'MSH' & antenna_id %in% c('01'),
                          'MSHA0',
                          node),
           node = if_else(site_code == 'MSH' & antenna_id == '00',
                          'METHB0',
                          node),
           node = if_else(site_code == 'METH',
                          'METHA0',
                          node),
           node = if_else(site_code == 'LLC' & config_id == 100,
                          if_else(antenna_id == 'D3',
                                  'LLCB0',
                                  'LLCA0'),
                          node),
           node = if_else(node == "SCP",
                          'SCPB0',
                          node),
           # node = if_else(node == "OMK",
           #               'OMKB0',
           #               node),
           # node = if_else(site_code %in% c('OBF', 'OMF'),
           #               'OMKA0',
           #               node),
           node = if_else(site_code == "OMF",
                          "OBF",
                          node),
           node = if_else(site_code == 'ZSL',
                          if_else(grepl('Weir 3', antenna_group, ignore.case = T),
                                  'ZSLB0',
                                  'ZSLA0'),
                          node),
           node = if_else(site_code == 'SA1' & config_id == 110,
                          'SA1B0',
                          node),
           node = if_else(site_code == 'OKC' & config_id == 100,
                          'OKCB0',
                          node),
           node = if_else(site_code == 'OMK' & config_id == 100,
                          'OMKB0',
                          node),
           # combine some sites above OKV into the upstream array at OKV
           node = if_else(site_code %in% c("OKS", "OKW"),
                          "OKVA0",
                          node),
           node = if_else(site_code == 'RCT' & config_id == 100,
                          'RCTB0',
                          node),
           node = if_else(site_code == 'BPC' & config_id == 100,
                          if_else(antenna_id %in% c('C3'),
                                  'BPCB0',
                                  'BPCA0'),
                          node),
           node = if_else(site_code == 'PRH' & antenna_id %in% c('F1', 'F2', 'F3', 'F4'),
                          'PRHB0',
                          node),
           node = if_else((site_code == 'PRH' & antenna_id %in% c('F5', 'F6', '01', '02')) | site_code %in% c('DDM', 'DM', 'UM', 'UUM', 'UP'),
                          'PRHA0',
                          node),
           node = if_else(site_code == 'PRO' & site_type == 'INT',
                          'PROB0',
                          node),
           # grab all sites upstream of Prosser dam, and assign them to PROA0
           # node = if_else(site_code %in% c('CHANDL', 'SAT', 'TOP', 'SM1', 'TP2', 'SUN', 'AH1', 'LNR', 'TTN', 'WNS', 'ROZ', 'TEAN', 'LMC', 'UMC', 'TAN', 'SWK', "LMT", 'UMT') | site_code == 'PRO' & site_type == 'MRR',
           #                'PROA0',
           #                node),
           node = if_else(site_code != "PRO" &
                            as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) == 539 &
                            as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,2]) > 76,
                          "PROA0",
                          node),
           node = if_else(site_code == 'ICH',
                          'ICHB0',
                          node),
           node = if_else(grepl('522\\.', rkm) & rkm_total > 538,
                          'ICHA0',
                          node),
           node = if_else(site_code == 'MDR',
                          'MDRB0',
                          node),
           node = if_else(site_code %in% c('LWD', 'BGM', 'NBA', 'MCD'),
                          'MDRA0',
                          node),
           node = if_else(site_code == 'HST',
                          'HSTB0',
                          node),
           node = if_else(site_code %in% c('BBT', 'COP', 'PAT'),
                          'HSTA0',
                          node),
           node = if_else(site_code == 'JD1',
                          'JD1B0',
                          node),
           node = if_else(site_code %in% c('30M', 'BR0', 'JDM', 'SJ1', 'SJ2', 'MJ1'),
                          'JD1A0',
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
    mutate(rkm = if_else(grepl('WVT', node),
                         '829.001',
                         rkm),
           rkm_total = if_else(grepl('WVT', node),
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
}

#-----------------------------------------------------------------
# Tumwater
#-----------------------------------------------------------------
# customize some nodes based on DABOM framework
if(root_site == 'TUM') {
  configuration = org_config %>%
    mutate(node = ifelse(site_code %in% c('LNF', 'LEAV'),
                         'LNF',
                         node),
           node = ifelse(site_code %in% c('TUF', 'TUMFBY', 'TUM'),
                         'TUM',
                         node),
           node = ifelse(site_code == 'CHIWAC',
                         'CHWA0',
                         node),
           node = ifelse(site_code == 'CHIWAR',
                         'CHLA0',
                         node),
           node = ifelse(site_code == 'CHIW',
                         'CHLA0',
                         node),
           node = ifelse(site_code == 'CHIKAC',
                         'CHUA0',
                         node),
           node = ifelse(site_code == 'WHITER',
                         'WTLA0',
                         node),
           node = ifelse(site_code == 'LWENAT',
                         'LWNA0',
                         node),
           node = ifelse(node == 'ICL',
                         'ICLB0',
                         node),
           node = ifelse(site_code == 'NASONC',
                         'NALA0',
                         node),
           node = if_else(site_code %in% c("LWE",
                                           "RRF",
                                           "WEA"),
                          "LWE",
                          node)) %>%
    distinct()
}

#-----------------------------------------------------------------
# Prosser Dam (Yakima River)
#-----------------------------------------------------------------
# make a few change
if(root_site == "PRO") {
  # customize some nodes based on DABOM framework
  configuration = org_config %>%
    mutate(node = if_else(site_code %in% c('PRO'),
                          'PRO',
                          node),
           node = if_else(site_code %in% c("NFTEAN", "TEANAR", "TEANM", "TEANWF"),
                          "LMTA0",
                          node),
           node = if_else(site_code == 'ROZ',
                          if_else(antenna_id %in% c('01', '02', '03'),
                                  node,
                                  as.character(NA)),
                          node),
           node = if_else(site_code == 'TAN' & config_id %in% c(120, 130),
                          "TANB0",
                          node),
           node = if_else(site_code %in% c('MC1', 'MC2', 'MCJ', 'MCN'),
                          'MCN',
                          node),
           node = if_else(site_code == 'ICH',
                          'ICHB0',
                          node),
           node = if_else(grepl('522\\.', rkm) & rkm_total > 538,
                          'ICHA0',
                          node),
           node = if_else(site_code == 'JD1',
                          'JD1B0',
                          node),
           node = if_else(site_code %in% c('30M', 'BR0', 'JDM', 'SJ1', 'SJ2', 'MJ1'),
                          'JD1A0',
                          node),
           node = if_else(site_code != 'JD1' & as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) < 351,
                          'JDA',
                          node),
           node = if_else(site_code == 'PRA',
                          'PRAB0',
                          node),
           node = if_else(site_code != 'PRA' & as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) >= 639,
                          'PRAA0',
                          node)) %>%
    # add a missing lat/long
    mutate(latitude = if_else(site_code == "SWK",
                              47.210348,
                              latitude),
           longitude = if_else(site_code == "SWK",
                              -120.699021,
                              longitude))
}



#-----------------------------------------------------------------
# read in observations
# find ptagis file, and generate old list of sites
if(root_site == "GRA") {
  ptagis_file = system.file("extdata",
                            'LGR_Chinook_2014.csv',
                            package = "PITcleanr",
                            mustWork = TRUE)
  sites_df = writeOldNetworks()$LowerGranite %>%
    rename(site_code = SiteID)
} else if(root_site == "PRA") {
  ptagis_file = system.file("extdata",
                            'UC_Sthd_2015.csv',
                            package = "PITcleanr",
                            mustWork = TRUE)
  sites_df = writeOldNetworks()$PriestRapids %>%
    mutate(across(c(SiteID, Step3),
                  recode,
                  "BelowJD1" = "JDA"),
           path = str_replace(path, "BelowJD1", "JDA")) %>%
    rename(site_code = SiteID)
} else if(root_site == 'TUM') {
  ptagis_file = system.file("extdata",
                            "TUM_Chinook_2015.csv",
                            package = "PITcleanr",
                            mustWork = TRUE)
  sites_df = writeOldNetworks()$Tumwater_noUWE %>%
    rename(site_code = SiteID)
} else if(root_site == "PRO") {
  ptagis_file = system.file("extdata",
                            "PRO_Steelhead_2019.csv",
                            package = "PITcleanr",
                            mustWork = TRUE)
  sites_df = writeOldNetworks()$Prosser %>%
    mutate(across(c(SiteID, Step2),
                  recode,
                  "BelowJD1" = "JDA"),
           path = str_replace(path, "BelowJD1", "JDA")) %>%
    rename(site_code = SiteID)
}


comp_obs = compress(cth_file = ptagis_file,
                    #max_minutes = 5,
                    max_minutes = NA,
                    configuration = configuration,
                    units = "hours")


# how many tags have a Mark or Recapture at the root_site?
comp_obs %>%
  filter(node == root_site,
         event_type_name %in% c("Mark", "Recapture")) %>%
  summarise(n_tags = n_distinct(tag_code)) %>%
  pull(n_tags)

# how many distinct tags in the observation data?
n_distinct(comp_obs$tag_code)

# determine trap date, and remove detections prior to that
spwn_yr = ptagis_file %>%
  str_split("/") %>%
  unlist() %>%
  last() %>%
  str_extract("[:digit:]+") %>%
  as.numeric()

min_obs_date = if_else(root_site %in% c("TUM", 'GRA'),
                       paste0(spwn_yr, "0301"),
                       if_else(root_site %in% c("PRA", 'PRO'),
                               paste0(spwn_yr-1, "0701"),
                               NA_character_))

obs = comp_obs %>%
  left_join(comp_obs %>%
              filter(min_det >= lubridate::ymd(min_obs_date)) %>%
              filter(node == root_site,
                     event_type_name %in% c("Mark", "Recapture")) %>%
              group_by(tag_code) %>%
              filter(min_det == min(min_det)) %>%
              summarise(start_date = max_det,
                        .groups = "drop"),
            by = "tag_code") %>%
  filter(min_det >= start_date) %>%
  group_by(tag_code) %>%
  mutate(slot = slot - min(slot) + 1) %>%
  ungroup()

n_distinct(obs$tag_code)

# pull out the site codes and nodes for all observations
obs_site_codes = obs %>%
  select(node) %>%
  distinct() %>%
  left_join(configuration %>%
              select(node,
                     site_code) %>%
              distinct())

# add a couple other sites (based on sites_df)
obs_site_codes %<>%
  full_join(configuration %>%
              filter(end_date >= min(obs$start_date) |
                       is.na(end_date)) %>%
              filter(site_code %in% sites_df$site_code) %>%
              select(any_of(names(obs_site_codes))) %>%
              distinct())

if(root_site == "GRA") {
  obs_site_codes %<>%
    full_join(configuration %>%
                filter(site_code %in% "JUL") %>%
                select(any_of(names(obs_site_codes))) %>%
                distinct())

}

#-----------------------------------------------------------------
# which sites do we care about for this exercise?
# based on which ones are in the PTAGIS file
sites_sf = extractSites(ptagis_file,
                        as_sf = T) %>%
  filter(site_code %in% unique(obs_site_codes$site_code))

# add a few other sites from other years
sites_sf %<>%
  bind_rows(obs_site_codes %>%
              anti_join(sites_sf) %>%
              select(site_code) %>%
              distinct() %>%
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
              st_transform(crs = st_crs(sites_sf)))

# drop an MRR site with the same code as an INT site
if(root_site %in% c("PRA", "PRO")) {
  sites_sf %<>%
    filter(!(site_code == "PRO" & type == "MRR"))
}

if(root_site %in% c("PRO")) {
  sites_sf %<>%
    filter(!(site_code == "ROZ" & type == "MRR"))
}

# for sites that are assigned to another node, delete those sites
sites_sf %<>%
  left_join(obs_site_codes %>%
              mutate(node_site = str_remove(node, "A0$"),
                     node_site = str_remove(node_site, "B0$")) %>%
              select(-node) %>%
              distinct() %>%
              group_by(node_site) %>%
              mutate(n_sites = n_distinct(site_code),
                     node_site_name = if_else(node_site == site_code,
                                              T, F))) %>%
  filter(node_site_name) %>%
  select(all_of(names(sites_sf)))

# which MRR sites have only a single site assigned to them?
single_site = sites_sf %>%
  filter(type == "MRR") %>%
  st_drop_geometry() %>%
  select(site_code) %>%
  left_join(obs_site_codes) %>%
  left_join(obs_site_codes %>%
              group_by(node) %>%
              summarize(n_sites = n_distinct(site_code),
                        .groups = "drop")) %>%
  filter(n_sites == 1)

# look at sites that may have a legitimate DABOM node
single_site %>%
  filter(grepl("A0$", node) |
           grepl("B0$", node))

single_site %>%
  left_join(sites_df %>%
              select(site_code = site_code,
                     path)) %>%
  filter(!is.na(path))
# xtabs(~ is.na(path), .)

# drop sites that are in the sites_df
single_site %<>%
  anti_join(sites_df %>%
              select(site_code))


# get some info about those sites
configuration %>%
  select(site_code,
         site_type,
         site_name,
         site_description) %>%
  inner_join(single_site) %>%
  distinct() %>%
  left_join(obs %>%
              filter(node %in% single_site$node) %>%
              group_by(node) %>%
              summarise(n_tags = n_distinct(tag_code),
                        .groups = "drop")) %>%
  arrange(desc(n_tags))

# drop these sites/nodes for various reasons
sites_sf %<>%
  anti_join(single_site %>%
              select(site_code))

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

# upstream extent of study area
upstrm_loc = if_else(root_site == "PRA",
                     "Chief Joseph Dam",
                     if_else(root_site == "GRA",
                             "Hells Canyon Dam",
                             NA_character_))
if(!is.na(upstrm_loc)) {
  library(ggmap)

  upstrm_comid = ggmap::geocode(upstrm_loc, output = "latlon") %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = 4326) %>%
    nhdplusTools::discover_nhdplus_id()

  nhd_upstrm_lst = nhdplusTools::plot_nhdplus(outlets = list(upstrm_comid),
                                              streamorder = min(nhd_list$flowlines$StreamOrde),
                                              actually_plot = F)

  flowlines %<>%
    anti_join(nhd_upstrm_lst$flowline %>%
                st_drop_geometry() %>%
                select(Hydroseq))

}


# plot the flowlines and the sites
ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(StreamOrde),
              size = StreamOrde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        end = 0.8) +
  scale_size_continuous(range = c(0.2, 1.2),
                        guide = 'none') +
  geom_sf(data = nhd_list$basin,
          fill = NA,
          lwd = 2) +
  # this cuts out parts of the basin upstream of upstrm_loc
  # geom_sf(data = flowlines %>%
  #           filter(!Hydroseq %in% nhd_list$dwn_flowlines$Hydroseq) %>%
  #           summarise(bndry = 'basin') %>%
  #           select(bndry) %>%
  #           st_convex_hull(),
  #         fill = NA,
  #         lwd = 2) +
  geom_sf(data = sites_sf,
          size = 4,
          color = "black") +
  geom_sf_label(data = sites_sf,
                aes(label = site_code)) +
  geom_sf_label(data = sites_sf %>%
                  filter(site_code == root_site),
                aes(label = site_code),
                color = "red") +
  theme_bw() +
  theme(axis.title = element_blank()) +
  labs(color = "Stream\nOrder")

#-----------------------------------------------------------------
# build parent child table
# use new functions, and not the configuration file
if(root_site == 'TUM') {
  parent_child = sites_sf %>%
    filter(! site_code %in% c("UWE", "LWE")) %>%
    buildParentChild(flowlines,
                     add_rkm = T) %>%
    editParentChild(fix_list = list(c("LNF", "ICM", "ICL"),
                                    c("LWE", "PES", "TUM"),
                                    c("LWE", "ICL", "TUM"),
                                    c(NA, "LWE", "TUM"),
                                    c(NA, "PES", "TUM")),
                    switch_parent_child = list(c("ICL", 'TUM'))) %>%
    filter(!(child == "ICL" & parent == "LWE"))
} else if(root_site == 'PRA') {
  parent_child = sites_sf %>%
    filter(! site_code %in% c("MC1", "MC2", "MCJ",
                              "FDD",
                              "MWC",
                              "YHC",
                              "HN1", "HN3",
                              "HSM", "HSU",
                              "LOR",
                              "EWC",
                              "LBT",
                              "LTP",
                              "BBP", "BCP",
                              "MRT", "HSL",
                              "ENM", 'ENS', 'TY4', '3D4')) %>%
    buildParentChild(flowlines,
                     # rm_na_parent = T,
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
                                    c("RIA", 'ENL', 'RRF'),
                                    c("RIA", "WEA", 'RRF'),
                                    c("RIA", "WEH", 'RRF'),
                                    c("EHL", 'ENA', 'ENL'),
                                    c("EHL", 'MAD', 'ENL'),
                                    c("METH", "MRW", "MRC"),
                                    c("SCP", "METH", "MSH"),
                                    c("SCP", 'MSH', 'MRC'),
                                    c("WHS", "OKC", "ZSL"),
                                    c("WHS", "ZSL", "OKL"),
                                    c("ZSL", 'OKV', 'OKC'),
                                    c("JOH", 'WHS', 'OKL'),
                                    c("JOH", 'BPC', 'OKL'),
                                    c("JOH", 'ANT', 'OKL'),
                                    c("JOH", 'TNK', 'OKL'),
                                    c("JOH", 'AEN', 'OKL')),
                    switch_parent_child = list(c("RSH", "PRA"))) %>%
    filter(!parent %in% c("WEH", "PRH"))
  # if("child_rkm" %in% names(parent_child)) {
  #   parent_child %<>%
  #     filter(!(child == "WAN" & child_rkm == "669"))
  # }
  # add RKMs from configuration file (since we had to fix at least one from PTAGIS)
  parent_child %<>%
    left_join(configuration %>%
                select(parent = site_code,
                       parent_rkm = rkm) %>%
                distinct(),
              by = "parent") %>%
    left_join(configuration %>%
                select(child = site_code,
                       child_rkm = rkm) %>%
                distinct(),
              by = "child") %>%
    distinct()
} else if(root_site == 'GRA') {
  parent_child = sites_sf %>%
    filter(! site_code %in% c("RRF", "PRA", 'PRO',
                              "JD1", "MJ1", 'TMF',
                              "MC1", "MC2",
                              "ICH", "LMA", "LMJ",
                              "GOA", "GOJ",
                              "GRJ",
                              "DWL")) %>%
    buildParentChild(flowlines,
                     rm_na_parent = F,
                     add_rkm = T) %>%
    editParentChild(fix_list = list(c("IR3", "IML", "IR4"),
                                    c("IR4", "IR5", "IML"),
                                    # c("IML", "IR5", "IMNAHW"),
                                    # c("IR3", "IMNAHW", "IML"),
                                    # c("GRA", "KOOS", "CLC"),
                                    # c("UGR", "GRANDW", "UGS"),
                                    # c("UGR", "CATHEW", "CCW"),
                                    c(NA, "LTR", "GRA"),
                                    c(NA, "PENAWC", "GRA"),
                                    c(NA, "ALMOTC", "GRA"),
                                    c("KEN", "LRW", "LLR"),
                                    c("KEN", "HYC", "LLR"),
                                    c("KEN", "AGC", "LLR"),
                                    c("LLS", "LBS", "LRW"),
                                    c("LLS", "LB8", "LRW"),
                                    c("LLS", "LCL", "LRW"),
                                    c("LBS", "BTL", "LRW"),
                                    c("LBS", "CAC", "LRW"),
                                    c("LBS", "HEC", "LRW"),
                                    c("ACM", "ACB", "ASOTIC"),
                                    c("ACB", "ASOTIC", "ACM"),
                                    c("ASOTIC", "AFC", "ACB"),
                                    c("ASOTIC", "CCA", "ACB"))) %>%
    filter(!(is.na(parent) & child == "GRA"))
} else if(root_site == 'PRO') {
  parent_child = sites_sf %>%
    # filter(! site_code %in% c("MC1", "MC2", "MCJ")) %>%
    buildParentChild(flowlines,
                     # rm_na_parent = T,
                     add_rkm = T) %>%
    editParentChild(fix_list = list(c("JDA", "MCN", "PRO"),
                                    c("JDA", "JD1", "PRO"),
                                    c("MCN", "ICH", "PRO"),
                                    c("MCN", "PRA", "PRO"),
                                    c(NA, "JDA", "PRO")),
                    switch_parent_child = list(c("MCN", "PRO")))
}

ques_locs = c("CLC", "KOOS")
ques_locs = c("ACB", 'ASOTIC')
ques_locs = sites_df %>%
  # filter(grepl("Okanogan", path)) %>%
  # filter(grepl("Entiat", path)) %>%
  filter(grepl('Methow', path)) %>%
  # filter(grepl('Wenatchee', path)) %>%
  pull(site_code)

ques_locs = sites_df %>%
  # filter(Step3 == "Potlatch") %>%
  # filter(Step3 == "Lapwai") %>%
  # filter(Step2 == "Imnaha") %>%
  # filter(Step3 == "GrandeRonde") %>%
  filter(Step3 == "Asotin") %>%
  pull(site_code)

parent_child %>%
  filter(parent %in% ques_locs |
           child %in% ques_locs) %>%
  left_join(sites_df %>%
              select(child = site_code,
                     path)) %>%
  distinct()

parent_child %>%
  filter(parent %in% ques_locs |
           child %in% ques_locs) %>%
  buildPaths() %>%
  left_join(sites_df %>%
              select(end_loc = site_code,
                     org_path = path))

# no child should appear more than once (i.e. each child only has one parent)
parent_child %>%
  filter(child %in% child[duplicated(child)])

# any parent-child combo's that are also child-parent combo's?
parent_child %>%
  inner_join(parent_child %>%
               select(child = parent,
                      parent = child))

# any sites in the parent/child table that aren't in original sites_df?
parent_child %>%
  anti_join(sites_df %>%
              select(child = site_code))

# any sites in the original sites_df that aren't in parent/child table?
sites_df %>%
  filter(site_code != root_site) %>%
  select(child = site_code) %>%
  anti_join(parent_child) %>%
  left_join(configuration %>%
              select(child = site_code,
                     node) %>%
              distinct()) %>%
  as.data.frame()



parent_child %>%
  # filter(parent == "PENAWC")
  # filter(child == "WCT")
  # filter(parent == "PRA")
  filter(parent == "GRA")

#------------------------------------------------------
# save a few things to share with package users
write_csv(configuration,
          paste0("inst/extdata/configuration_", root_site, ".csv"))
write_csv(parent_child,
          paste0("inst/extdata/parent_child_", root_site, ".csv"))

#------------------------------------------------------

# look at paths to each location
buildPaths(parent_child)

# add nodes to the parent child table
parent_child_nodes = addParentChildNodes(parent_child,
                                         configuration)


sites_df %>%
  select(site_code,
         path) %>%
  left_join(configuration %>%
              select(site_code,
                     node) %>%
              distinct()) %>%
  rename(child = site_code) %>%
  anti_join(parent_child) %>%
  left_join(parent_child_nodes %>%
              mutate(in_pc_tab = T) %>%
              select(node = child,
                     in_pc_tab))


# test against old versions of parent-child table
if(root_site == "GRA") {
  parent_child_df = createParentChildDf(sites_df %>%
                                          rename(SiteID = site_code),
                                        configuration %>%
                                          janitor::clean_names(case = "big_camel") %>%
                                          rename(SiteID = SiteCode,
                                                 ConfigID = ConfigId,
                                                 AntennaID = AntennaId,
                                                 RKM = Rkm,
                                                 RKMTotal = RkmTotal),
                                        startDate = "20140301") %>%
    rename(parent = ParentNode,
           child = ChildNode)
} else if(root_site == "PRA") {
  parent_child_df = createParentChildDf(sites_df %>%
                                          rename(SiteID = site_code),
                                        configuration %>%
                                          janitor::clean_names(case = "big_camel") %>%
                                          rename(SiteID = SiteCode,
                                                 ConfigID = ConfigId,
                                                 AntennaID = AntennaId,
                                                 RKM = Rkm,
                                                 RKMTotal = RkmTotal),
                                        startDate = "20140701") %>%
    rename(parent = ParentNode,
           child = ChildNode)
} else if(root_site == 'TUM') {
  parent_child_df = createParentChildDf(sites_df %>%
                                          rename(SiteID = site_code),
                                        configuration %>%
                                          janitor::clean_names(case = "big_camel") %>%
                                          rename(SiteID = SiteCode,
                                                 ConfigID = ConfigId,
                                                 AntennaID = AntennaId,
                                                 RKM = Rkm,
                                                 RKMTotal = RkmTotal),
                                        startDate = "20150701") %>%
    rename(parent = ParentNode,
           child = ChildNode)
}

anti_join(parent_child_nodes,
          parent_child_df)

anti_join(parent_child_df,
          parent_child_nodes)

anti_join(parent_child_nodes,
          parent_child_df) %>%
  select(new_parent = parent,
         child) %>%
  left_join(parent_child_df %>%
              select(old_parent = parent,
                     child))


#--------------------------------------------------------
# build a node order, with paths to each node
node_order = buildNodeOrder(parent_child_nodes)

# which observation locations are not in node_order?
obs %>%
  left_join(node_order) %>%
  filter(is.na(node_order)) %>%
  janitor::tabyl(node) %>%
  janitor::adorn_pct_formatting() %>%
  arrange(n)

obs %>%
  left_join(node_order) %>%
  filter(is.na(node_order)) %>%
  select(tag_code,
         unmodeled_node = node) %>%
  distinct() %>%
  left_join(obs %>%
              filter(node != root_site) %>%
              left_join(node_order) %>%
              filter(!is.na(node_order))) %>%
  select(tag_code, unmodeled_node,
         node) %>%
  distinct() %>%
  group_by(tag_code,
           unmodeled_node) %>%
  summarise(n_modeled_nodes = n_distinct(node[!is.na(node)]),
            not_seen_elsewhere = if_else(sum(is.na(node)) > 0, T, F),
            .groups = 'drop') %>%
  # filter(n_modeled_nodes == 0)
  janitor::tabyl(unmodeled_node, not_seen_elsewhere)

# filter out observations at sites not included in the node order
# determine direction of movement
obs_direct = obs %>%
  addDirection(parent_child_nodes %>%
                 select(parent, child))

obs_direct %>%
  janitor::tabyl(direction)

obs_direct %>%
  filter(direction == "unknown") %>%
  # filter(direction == "no movement",
  #        node != "TUM") %>%
  select(tag_code) %>%
  distinct() %>%
  slice(3) %>%
  left_join(obs_direct) %>%
  select(tag_code,
         event_type_name,
         matches("node"),
         direction,
         min_det,
         path)

if(root_site == "TUM") {
  max_obs_date = paste0(as.numeric(str_extract(ptagis_file, "[:digit:]+")), "0930")
} else if(root_site == "PRA") {
  max_obs_date = paste0(as.numeric(str_extract(ptagis_file, "[:digit:]+")), "0630")
} else if(root_site == "GRA") {
  max_obs_date = paste0(as.numeric(str_extract(ptagis_file, "[:digit:]+")), "0930")
} else if(root_site == "PRO") {
  max_obs_date = paste0(as.numeric(str_extract(ptagis_file, "[:digit:]+")), "0630")
}

proc_obs = filterDetections(obs,
                            parent_child_nodes,
                            max_obs_date)

proc_obs %>%
  mutate(user_keep_obs = auto_keep_obs) %>%
  estimateSpawnLoc(spawn_site = T,
                   ptagis_file = ptagis_file) %>%
  slice(11:30)

if(root_site == "GRA") {
  bio_file = system.file("extdata",
                         'Chnk2014_TrapDatabase.csv',
                         package = "PITcleanr",
                         mustWork = TRUE)
  bio_df = read_csv(bio_file) %>%
    rename(tag_code = LGDNumPIT) %>%
    group_by(tag_code) %>%
    slice(1) %>%
    ungroup() %>%
    filter(tag_code %in% unique(proc_obs$tag_code))
  proc_obs %>%
    mutate(user_keep_obs = auto_keep_obs) %>%
    filter(user_keep_obs) %>%
    summarizeTagData(bio_data = bio_df,
                     spawn_site = T,
                     ptagis_file = ptagis_file)
}

proc_obs %>%
  summarise(n_tags = n_distinct(tag_code),
            n_weird_tags = n_distinct(tag_code[is.na(user_keep_obs)])) %>%
  mutate(prop_weird = n_weird_tags / n_tags)

# look at examples of "weird" capture histories
proc_obs %>%
  filter(is.na(user_keep_obs)) %>%
  filter(direction == 'unknown') %>%
  # filter(grepl('WTL', node)) %>%
  select(tag_code) %>%
  distinct() %>%
  slice_sample(n = 1) %>%
  # slice(119) %>%
  left_join(proc_obs) %>%
  select(-(duration:start_date),
         -user_keep_obs) %>%
  select(-max_det) %>%
  as.data.frame()

#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
x = obs_direct %>%
  group_by(tag_code) %>%
  # filter(sum(direction == "unknown") > 0) %>%
  filter(sum(direction == "backward") > 0) %>%
  ungroup() %>%
  select(tag_code) %>%
  distinct() %>%
  slice(1) %>%
  left_join(obs_direct) %>%
  select(-tag_code)



obs_direct %>%
  group_by(tag_code) %>%
  # filter(sum(direction == "unknown") > 0) %>%
  filter(sum(direction == "backward") > 0) %>%
  ungroup() %>%
  select(tag_code) %>%
  distinct() %>%
  slice(9) %>%
  left_join(proc_obs) %>%
  select(-c(duration:start_date),
         -max_det,
         -path) %>%
  slice(1:12)
# filter(AutoProcStatus)
as.data.frame()


obs_direct %>%
  group_by(tag_code) %>%
  summarise(strange_dir = if_else(sum(direction %in% c("backward", "unknown")) > 0,
                                  T, F),
            back_dir = if_else(sum(direction %in% c("backward")) > 0,
                               T, F),
            unkwn_dir = if_else(sum(direction %in% c("unknown")) > 0,
                                T, F),
            no_prob = if_else(sum(direction %in% c("backward", "unknown")) == 0,
                              T, F)) %>%
  ungroup() %>%
  summarise(across(c(strange_dir:no_prob),
                   sum))



# compare with former functions
parent_child_df

# get raw observations from PTAGIS
# These come from running a saved query on the list of tags to be used
observations = read_csv(ptagis_file)

# add some padding to the antenna codes if needed
observations %<>%
  mutate(`Antenna ID` = if_else(nchar(`Antenna ID`) == 1,
                                stringr::str_pad(`Antenna ID`, 2,
                                                 side = "left",
                                                 pad = "0"),
                                `Antenna ID`))

# process those observations with PITcleanr, using Tumwater-specific function
proc_list = processCapHist_TUM(start_date = "20150501",
                               configuration = configuration,
                               parent_child = parent_child_df %>%
                                 rename(Parentnode = parent,
                                        Childnode = child),
                               observations = observations,
                               last_obs_date = paste0(as.numeric(str_extract(ptagis_file, "[:digit:]+")), "0930"),
                               truncate = T,
                               save_file = F)

identical(sort(unique(proc_list$ProcCapHist$TagID)),
          sort(unique(proc_obs$tag_code)))

proc_list$ProcCapHist %>%
  select(tag_code = TagID,
         TrapDate,
         node = node,
         site_code,
         min_det = ObsDate,
         max_det = lastObsDate,
         AutoProcStatus) %>%
  full_join(proc_obs %>%
              mutate(across(start_date,
                            floor_date,
                            unit = "days")),
            by = c('tag_code','node',
                   "min_det", "max_det")) %>%
  filter(tag_code == "3DD.00773DBC00") %>%
  as.data.frame()
#
filter(!is.na(slot)) %>%
  filter(AutoProcStatus != AutoKeepObs,
         AutoKeepObs) %>%
  select(tag_code)
group_by(tag_code) %>%
  summarize(n_new = sum(is.na(UserKeepObs)),
            n_old = sum(!AutoProcStatus))
xtabs(~ is.na(UserKeepObs) + UserProcStatus, .)
select(tag_code) %>%
  distinct() %>%
  left_join(proc_obs)

filter(tag_code %in% union(sort(unique(proc_list$ProcCapHist$TagID)),
                           sort(unique(proc_obs$tag_code)))) %>%
  # filter(is.na(slot)) %>%
  # janitor::tabyl(site_code)
  # filter(is.na(site_code)) %>%
  # select(tag_code) %>%
  # distinct() %>%
  # slice(3) %>%
  # left_join(proc_list$ProcCapHist %>%
  #             rename(tag_code = TagID))
  filter(TrapDate != start_date) %>%
  select(tag_code,
         TrapDate,
         start_date) %>%
  distinct() %>%
  left_join(comp_obs %>%
              filter(node == 'TUM') %>%
              select(tag_code, event_type_name, min_det, max_det))
select(-duration, -travel_time)
