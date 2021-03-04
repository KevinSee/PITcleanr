# Author: Kevin See
# Purpose: Test new functions for processing PTAGIS data for DABOM
# Created: 2/10/2021
# Last Modified: 3/3/2021
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
# root_site = "GRA"
# root_site = "PRA"
root_site = "TUM"

#-----------------------------------------------------------------
# Lower Granite
#-----------------------------------------------------------------
# make a few change
if(root_site == "GRA") {
  configuration = org_config %>%
    mutate(Node = ifelse(SiteID %in% c('VC2', 'VC1', 'LTR', 'MTR', 'UTR'),
                         SiteID,
                         Node),
           Node = ifelse(SiteID == 'SC2',
                         'SC2B0',
                         Node),
           Node = ifelse(SiteID %in% c('CROTRP',
                                       'CRT',
                                       'REDTRP',
                                       'REDR',
                                       'RRT'),
                         'SC2A0',
                         Node),
           Node = ifelse(Node == 'ACB',
                         'ACBB0',
                         Node),
           Node = ifelse(Node == 'CCA',
                         'CCAB0',
                         Node),
           Node = ifelse(SiteID == 'AFC',
                         ifelse(grepl('MAINSTEM', AntennaGroup),
                                'AFCB0',
                                'AFCA0'),
                         Node),
           Node = ifelse(SiteID == 'HBC',
                         'HYCA0',
                         Node),
           Node = ifelse(SiteID %in% c('TUCH', 'TFH'),
                         'TUCH',
                         Node),
           Node = ifelse(SiteID == 'MCCA',
                         'STR',
                         Node),
           Node = ifelse(SiteID == 'CARMEC',
                         'CRCA0',
                         Node),
           Node = ifelse(SiteID == 'BIG2C',
                         'TAYA0',
                         Node),
           Node = ifelse(SiteID == 'WIMPYC',
                         'WPCA0',
                         Node),
           Node = ifelse(SiteID == 'IML' & ConfigID == 130 & AntennaID == '09',
                         'IMLA0',
                         Node),
           Node = str_replace(Node, '^BTC', 'BTL'),
           Node = ifelse(SiteID %in% c('YANKFK', 'CEY'),
                         'YFKA0',
                         Node),
           Node = ifelse(SiteID == 'SAWT',
                         'STL',
                         Node),
           Node = ifelse(SiteID == 'LOOH',
                         'LOOKGC',
                         Node),
           Node = ifelse(SiteID == 'RPDTRP',
                         'RAPH',
                         Node),
           Node = ifelse(SiteID == 'CHARLC',
                         'CCAB0',
                         Node),
           Node = ifelse(Node == 'KEN',
                         'KENB0',
                         Node),
           Node = ifelse(Node == 'HYC',
                         'HYCB0',
                         Node),
           Node = ifelse(Node == 'YFK',
                         'YFKB0',
                         Node),
           Node = ifelse(Node == 'LLR',
                         'LLRB0',
                         Node),
           Node = ifelse(Node == 'LRW',
                         'LRWB0',
                         Node),
           Node = ifelse(SiteID == '18M',
                         str_replace(Node, '18M', 'HEC'),
                         Node)) %>%
    distinct()
}

#-----------------------------------------------------------------
# Priest Rapids
#-----------------------------------------------------------------
# customize some nodes based on DABOM framework
if(root_site == "PRA") {
  configuration = org_config %>%
    # manually add site for Colockum Creek (not in PTAGIS)
    bind_rows(tibble(SiteID = 'CLK',
                     ConfigID = 100,
                     AntennaID = 'A1',
                     Node = 'CLK',
                     ValidNode = T,
                     # making these up
                     StartDate = as.POSIXct(lubridate::ymd('20100101')),
                     SiteType = 'INT',
                     SiteName = 'Colockum Creek',
                     AntennaGroup = 'Single Colockum Ck',
                     SiteDescription = 'Temporary single antenna.',
                     SiteTypeName = 'Instream Remote Detection System',
                     RKM = '740.001',
                     RKMTotal = 741,
                     # this puts CLK upstream of RIA
                     Latitude = 47.3707357269787,
                     Longitude = -120.25617371760839)) %>%
                     # this puts CLK on Colockum Creek, but between PRA and RIA
                     # Latitude = 47.29722788926544,
                     # Longitude = -120.10577913008702)) %>%
    filter(!(SiteID == 'WAN' & SiteType == 'MRR'),
           !(SiteID == 'TMF' & SiteType == 'MRR'),
           !(SiteID == 'PRO' & SiteType == 'MRR')) %>%
    mutate(Node = if_else(SiteID %in% c('RIA', 'RRF', 'WEA', 'PRV'),
                          SiteID,
                          Node)) %>%
    mutate(Node = if_else(SiteID == 'PRDLD1',
                          'PRA',
                          Node)) %>%
    mutate(Node = if_else(Node == "LWE",
                          'LWEB0',
                          Node),
           Node = if_else(SiteID %in% c('TUF', 'TUMFBY', 'TUM'),
                          'TUM',
                          Node),
           Node = if_else(SiteID == 'LNF' & AntennaID %in% c('01', '02'),
                          'LNFA0',
                          Node),
           Node = if_else(SiteID == 'LNF' & AntennaID %in% c('03', '04'),
                          'LNFB0',
                          Node),
           Node = if_else(SiteID == 'LEAV',
                          'LNFA0',
                          Node),
           Node = if_else(SiteID == 'ICL' & ConfigID == 100,
                          'ICLB0',
                          Node),
           Node = if_else(SiteID == 'CHIWAC',
                          'CHWA0',
                          Node),
           Node = if_else(SiteID == 'CHIWAR',
                          'CHLA0',
                          Node),
           Node = if_else(SiteID == 'CHIKAC',
                          'CHUA0',
                          Node),
           Node = if_else(SiteID == 'WHITER',
                          'WTLA0',
                          Node),
           Node = if_else(SiteID == 'LWENAT',
                          'LWNA0',
                          Node),
           Node = if_else(SiteID == 'NASONC',
                          'NALA0',
                          Node),
           # any fish seen at Dryden dam should also be seen at LWE
           Node = if_else(SiteID == 'DRY',
                          'LWEA0',
                          Node),
           # any fish seen at Chiwawa acclimation pond gets moved to CHL
           Node = if_else(SiteID == 'CHP',
                          'CHLA0',
                          Node),
           Node = if_else(SiteID == 'EBO',
                          'RRF',
                          Node),
           Node = if_else(SiteID == 'RRJ',
                          'RRF',
                          Node),
           Node = if_else(SiteID == 'EHL' & ConfigID == 100 & AntennaID == '02',
                          'EHLB0',
                          Node),
           Node = if_else(SiteID == 'EHL' & ConfigID == 100 & AntennaID == '01',
                          'EHLA0',
                          Node),
           Node = if_else(SiteID == 'EHL' & ConfigID == 110 & AntennaID == '03',
                          'EHLB0',
                          Node),
           Node = if_else(SiteID == 'EHL' & ConfigID == 110 & AntennaID %in% c('01', '02'),
                          'EHLA0',
                          Node),
           Node = if_else(SiteID == "WEH" & AntennaID == "A2",
                          "WEHB0",
                          Node),
           Node = if_else(SiteID == "WEH" & AntennaID != "A2",
                          "WEHA0",
                          Node),
           Node = if_else(Node == "LMR",
                          'LMRB0',
                          Node),
           Node = if_else(SiteID == 'LBC' & ConfigID == 100,
                          'LBCB0',
                          Node),
           Node = if_else(SiteID == 'MRC',
                          'MRCB0',
                          Node),
           Node = if_else(SiteID %in% c('SSC', '18N', 'MHB', 'M3R', 'MWF'),
                          'MRCA0',
                          Node),
           Node = if_else(SiteID == 'MSH' & AntennaID %in% c('02', '03'),
                          'MSHB0',
                          Node),
           Node = if_else(SiteID == 'MSH' & AntennaID %in% c('01'),
                          'MSHA0',
                          Node),
           Node = if_else(SiteID == 'MSH' & AntennaID == '00',
                          'METHB0',
                          Node),
           Node = if_else(SiteID == 'METH',
                          'METHA0',
                          Node),
           Node = if_else(SiteID == 'LLC' & ConfigID == 100,
                          if_else(AntennaID == 'D3',
                                  'LLCB0',
                                  'LLCA0'),
                          Node),
           Node = if_else(Node == "SCP",
                          'SCPB0',
                          Node),
           # Node = if_else(Node == "OMK",
           #               'OMKB0',
           #               Node),
           # Node = if_else(SiteID %in% c('OBF', 'OMF'),
           #               'OMKA0',
           #               Node),
           Node = if_else(SiteID == "OMF",
                          "OBF",
                          Node),
           Node = if_else(SiteID == 'ZSL',
                          if_else(grepl('Weir 3', AntennaGroup, ignore.case = T),
                                  'ZSLB0',
                                  'ZSLA0'),
                          Node),
           Node = if_else(SiteID == 'SA1' & ConfigID == 110,
                          'SA1B0',
                          Node),
           Node = if_else(SiteID == 'OKC' & ConfigID == 100,
                          'OKCB0',
                          Node),
           Node = if_else(SiteID == 'OMK' & ConfigID == 100,
                          'OMKB0',
                          Node),
           # combine some sites above OKV into the upstream array at OKV
           Node = if_else(SiteID %in% c("OKS", "OKW"),
                          "OKVA0",
                          Node),
           Node = if_else(SiteID == 'RCT' & ConfigID == 100,
                          'RCTB0',
                          Node),
           Node = if_else(SiteID == 'BPC' & ConfigID == 100,
                          if_else(AntennaID %in% c('C3'),
                                  'BPCB0',
                                  'BPCA0'),
                          Node),
           Node = if_else(SiteID == 'PRH' & AntennaID %in% c('F1', 'F2', 'F3', 'F4'),
                          'PRHB0',
                          Node),
           Node = if_else((SiteID == 'PRH' & AntennaID %in% c('F5', 'F6', '01', '02')) | SiteID %in% c('DDM', 'DM', 'UM', 'UUM', 'UP'),
                          'PRHA0',
                          Node),
           Node = if_else(SiteID == 'PRO' & SiteType == 'INT',
                          'PROB0',
                          Node),
           # grab all sites upstream of Prosser dam, and assign them to PROA0
           # Node = if_else(SiteID %in% c('CHANDL', 'SAT', 'TOP', 'SM1', 'TP2', 'SUN', 'AH1', 'LNR', 'TTN', 'WNS', 'ROZ', 'TEAN', 'LMC', 'UMC', 'TAN', 'SWK', "LMT", 'UMT') | SiteID == 'PRO' & SiteType == 'MRR',
           #                'PROA0',
           #                Node),
           Node = if_else(SiteID != "PRO" &
                            as.integer(stringr::str_split(RKM, '\\.', simplify = T)[,1]) == 539 &
                            as.integer(stringr::str_split(RKM, '\\.', simplify = T)[,2]) > 76,
                          "PROA0",
                          Node),
           Node = if_else(SiteID == 'ICH',
                          'ICHB0',
                          Node),
           Node = if_else(grepl('522\\.', RKM) & RKMTotal > 538,
                          'ICHA0',
                          Node),
           Node = if_else(SiteID == 'MDR',
                          'MDRB0',
                          Node),
           Node = if_else(SiteID %in% c('LWD', 'BGM', 'NBA', 'MCD'),
                          'MDRA0',
                          Node),
           Node = if_else(SiteID == 'HST',
                          'HSTB0',
                          Node),
           Node = if_else(SiteID %in% c('BBT', 'COP', 'PAT'),
                          'HSTA0',
                          Node),
           Node = if_else(SiteID == 'JD1',
                          'JD1B0',
                          Node),
           Node = if_else(SiteID %in% c('30M', 'BR0', 'JDM', 'SJ1', 'SJ2', 'MJ1'),
                          'JD1A0',
                          Node),
           Node = if_else(SiteID != 'JD1' & as.integer(stringr::str_split(RKM, '\\.', simplify = T)[,1]) < 351,
                          'JDA',
                          Node)) %>%
    distinct() %>%
    # correct a couple RKM values
    mutate(RKM = if_else(SiteID == 'SA1',
                         '858.041.003',
                         RKM),
           RKMTotal = if_else(SiteID == 'SA1',
                              902,
                              RKMTotal)) %>%
    mutate(RKM = if_else(SiteID == 'TON',
                         '858.133.001',
                         RKM),
           RKMTotal = if_else(SiteID == 'TON',
                              992,
                              RKMTotal)) %>%
    mutate(RKM = if_else(grepl('WVT', Node),
                         '829.001',
                         RKM),
           RKMTotal = if_else(grepl('WVT', Node),
                              830,
                              RKMTotal))
}

#-----------------------------------------------------------------
# Tumwater
#-----------------------------------------------------------------
# customize some nodes based on DABOM framework
if(root_site == 'TUM') {
  configuration = org_config %>%
    mutate(Node = ifelse(SiteID %in% c('LNF', 'LEAV'),
                         'LNF',
                         Node),
           Node = ifelse(SiteID %in% c('TUF', 'TUMFBY', 'TUM'),
                         'TUM',
                         Node),
           Node = ifelse(SiteID == 'CHIWAC',
                         'CHWA0',
                         Node),
           Node = ifelse(SiteID == 'CHIWAR',
                         'CHLA0',
                         Node),
           Node = ifelse(SiteID == 'CHIW',
                         'CHLA0',
                         Node),
           Node = ifelse(SiteID == 'CHIKAC',
                         'CHUA0',
                         Node),
           Node = ifelse(SiteID == 'WHITER',
                         'WTLA0',
                         Node),
           Node = ifelse(SiteID == 'LWENAT',
                         'LWNA0',
                         Node),
           Node = ifelse(Node == 'ICL',
                         'ICLB0',
                         Node),
           Node = ifelse(SiteID == 'NASONC',
                         'NALA0',
                         Node),
           Node = if_else(SiteID %in% c("LWE",
                                        "RRF",
                                        "WEA"),
                          "LWE",
                          Node)) %>%
    distinct()
}

#-----------------------------------------------------------------
# read in observations

if(root_site == "GRA") {
  ptagis_file = 'inst/extdata/LGR_Chinook_2014.csv'
  sites_df = writeLGRNodeNetwork()
} else if(root_site == "PRA") {
  ptagis_file = 'inst/extdata/UC_Sthd_2015.csv'
  sites_df = writePRDNodeNetwork() %>%
    mutate(across(c(SiteID, Step3),
                  recode,
                  "BelowJD1" = "JDA"),
           path = str_replace(path, "BelowJD1", "JDA"))
} else if(root_site == 'TUM') {
  ptagis_file = 'inst/extdata/TUM_Chinook_2015.csv'
  sites_df = writeTUMNodeNetwork_noUWE()
}


comp_obs = compress(ptagis_file = ptagis_file,
                    #max_minutes = 5,
                    max_minutes = NA,
                    configuration = configuration,
                    units = "hours")

# # get detections by site, not node
# obs_site = compress(ptagis_file = ptagis_file,
#                     #max_minutes = 5,
#                     max_minutes = NA,
#                     units = "hours") %>%
#   left_join(comp_obs %>%
#               filter(node == root_site,
#                      event_type_name %in% c("Mark", "Recapture")) %>%
#               group_by(tag_code) %>%
#               filter(max_det == max(max_det)) %>%
#               summarise(start_date = max_det,
#                         .groups = "drop"),
#             by = "tag_code") %>%
#   filter(min_det >= start_date) %>%
#   group_by(tag_code) %>%
#   mutate(slot = slot - min(slot) + 1) %>%
#   ungroup()
#
# obs %>%
#   left_join(configuration %>%
#               select(SiteID, node = Node) %>%
#               distinct() %>%
#               group_by(node) %>%
#               summarise(n_sites = n_distinct(SiteID),
#                         site_list = list(unique(SiteID)),
#                         .groups = "drop")) %>%
#   filter(n_sites > 1,
#          node != root_site)
#
# test = obs %>%
#   mutate(time_interval = interval(min_det, max_det)) %>%
#   select(tag_code, node, slot, time_interval) %>%
#   left_join(obs_site %>%
#               mutate(site_interval = interval(min_det, max_det)) %>%
#               select(tag_code, node, slot, site_interval) %>%
#               nest(site_intvl = -tag_code)) %>%
#   nest(intvl_data = c(time_interval, site_intvl)) %>%
#   mutate(site_list = map(.x = intvl_data,
#                           .f = function(x) {
#                             x %>%
#                               unnest(site_intvl) %>%
#                               mutate(ovrlp = int_overlaps(site_interval,
#                                                           time_interval)) %>%
#                               filter(ovrlp) %>%
#                               pull(node)
#                           })) %>%
#   ungroup()


# how many tags have a Mark or Recapture at the root_site?
comp_obs %>%
  filter(node == root_site,
         event_type_name %in% c("Mark", "Recapture")) %>%
  summarise(n_tags = n_distinct(tag_code)) %>%
  pull(n_tags)

# how many distinct tags in the observation data?
n_distinct(comp_obs$tag_code)

# determine trap date, and remove detections prior to that
obs = comp_obs %>%
  left_join(comp_obs %>%
              filter(node == root_site,
                     event_type_name %in% c("Mark", "Recapture")) %>%
              group_by(tag_code) %>%
              filter(max_det == max(max_det)) %>%
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
              select(node = Node,
                     site_code = SiteID) %>%
              distinct())

# add a couple other sites (based on sites_df)
obs_site_codes %<>%
  full_join(configuration %>%
              filter(EndDate >= min(obs$start_date) |
                       is.na(EndDate)) %>%
              filter(SiteID %in% sites_df$SiteID) %>%
              select(node = Node,
                     site_code = SiteID) %>%
              distinct())

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
              left_join(configuration %>%
                          rename(site_code = SiteID)) %>%
              group_by(site_code) %>%
              filter(ConfigID == max(ConfigID)) %>%
              ungroup() %>%
              select(site_code,
                     site_name = SiteName,
                     site_type = SiteTypeName,
                     type = SiteType,
                     rkm = RKM,
                     site_description = SiteDescription,
                     Latitude, Longitude) %>%
              distinct() %>%
              filter(!is.na(Latitude)) %>%
              st_as_sf(coords = c("Longitude",
                                  "Latitude"),
                       crs = 4326) %>%
              st_transform(crs = st_crs(sites_sf)))

if(root_site == "PRA") {
  sites_sf %<>%
    filter(!(site_code == "PRO" & type == "MRR"))
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
              select(site_code = SiteID,
                     path)) %>%
  filter(!is.na(path))
  # xtabs(~ is.na(path), .)

single_site %<>%
  anti_join(sites_df %>%
              select(site_code = SiteID))


# get some info about those sites
configuration %>%
  select(site_code = SiteID,
         SiteType,
         SiteName,
         SiteDescription) %>%
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

# join sites to nearest hydro sequence
sites_NHDseg = st_join(sites_sf,
                       flowlines %>%
                         select(gnis_name, Hydroseq),
                       join = st_nearest_feature)

# which sites were joined to the same hydrosequence?
sites_NHDseg %>%
  filter(Hydroseq %in% Hydroseq[duplicated(Hydroseq)]) %>%
  arrange(Hydroseq, rkm, site_code)

sites_NHDseg %>%
  filter(Hydroseq %in% Hydroseq[duplicated(Hydroseq)]) %>%
  arrange(Hydroseq, rkm) %>%
  st_drop_geometry() %>%
  split(list(.$Hydroseq))

#-----------------------------------------------------------------
# build parent child table
# use new functions, and not the configuration file
if(root_site == 'TUM') {
  parent_child = sites_sf %>%
    filter(! site_code %in% c("UWE", "LWE")) %>%
    buildParentChild(flowlines,
                     add_rkm = T) %>%
    editParentChild(child_locs = c("ICM", "PES", 'ICL', 'LWE', "PES"),
                    parent_locs = c("LNF", "LWE", "LWE", NA, NA),
                    new_parent_locs = c("ICL", "TUM", 'TUM', "TUM", "TUM"),
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
                     rm_na_parent = T) %>%
    editParentChild(child_locs = c("WEA",
                                   'ICH', 'JD1', 'PRO', 'TMF', 'PRV', 'RSH',
                                   'PRH',
                                   'PRA',
                                   "ENL",
                                   'ENA', 'MAD',
                                   'ENF',
                                   'METH',
                                   'MRW',
                                   'OKC',
                                   'OKV',
                                   'WHS', 'BPC', 'ANT', 'TNK', "AEN",
                                   'ZSL',
                                   'WEH',
                                   'SCP', 'CRW',
                                   'MSH',
                                   'TUM',
                                   'ICM'),
                    parent_locs = c("RIA",
                                    rep('JDA', 6),
                                    "RSH",
                                    'RSH',
                                    'RIA',
                                    rep("EHL", 2),
                                    '3D4',
                                    'SCP',
                                    'MSH',
                                    'WHS',
                                    'ZSL',
                                    rep("JOH", 5),
                                    'WHS',
                                    'RIA',
                                    rep('MRT', 2),
                                    'SCP',
                                    'ICL',
                                    'LNF'),
                    new_parent_locs = c("RRF",
                                        rep('PRA', 6),
                                        "PRA",
                                        'JDA',
                                        'RRF',
                                        rep("ENL", 2),
                                        'ENA',
                                        'MSH',
                                        'MRC',
                                        'ZSL',
                                        'OKC',
                                        rep('OKL', 5),
                                        'OKL',
                                        'RRF',
                                        rep('MRC', 2),
                                        'MRC',
                                        'LWE',
                                        'ICL'),
                    switch_parent_child = list(c("JDA", "PRA"),
                                               c("RSH", "PRA"))) %>%
    # because PRH and PRA are on the same hydrosequence, need to remove this row
    filter(!(parent == 'PRH' & child %in% c("RIA", "CLK")),
           !(parent == "METH" & child == "MRW"),
           !(parent == 'WEH'))
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
    editParentChild(parent_locs = c("GRA",
                                    "UGR", "UGR",
                                    "IR3", 'IR3',
                                    NA, NA,
                                    rep("KEN", 3),
                                    rep("LLS", 3),
                                    rep("LBS", 3),
                                    'ACM', 'ACB', 'ASOTIC', 'ASOTIC'),
                    child_locs = c("KOOS",
                                   "GRANDW", "CATHEW",
                                   'IMNAHW', 'IML',
                                   "LTR", "PENAWC",
                                   "LRW", "HYC", "AGC",
                                   "LBS", "LB8", "LCL",
                                   "BTL", "CAC", 'HEC',
                                   'ACB', 'ASOTIC', 'AFC', 'CCA'),
                    new_parent_locs = c("CLC",
                                        "UGS", "CCW",
                                        'IML', 'IR4',
                                        "GRA", "GRA",
                                        rep("LLR", 3),
                                        rep("LRW", 3),
                                        rep("LRW", 3),
                                        'ASOTIC', 'ACM', 'ACB', 'ACB'),
                    switch_parent_child = list(c("PENAWC", "GRA"))) %>%
    filter(!(child == 'IR5' & parent %in% c("IR4", "IML")))
}

ques_locs = c("CATHEW", "CCW")
ques_locs = c("IMNAHW", "IML", 'IR4', 'IR5')
ques_locs = c("GRANDW", "UGS")
ques_locs = c("CLC", "KOOS")
ques_locs = c("ACB", 'ASOTIC')
ques_locs = c("PRA", 'PRH')
ques_locs = c("WEA", 'WEH')
ques_locs = c("MSH", 'METH')
ques_locs = c("RIA", "RRF", 'WEA')
ques_locs = c("CLK")
ques_locs = c("PRO")
ques_locs = c("JDA")
ques_locs = sites_df %>%
  # filter(grepl("Okanogan", path)) %>%
  # filter(grepl("Entiat", path)) %>%
  # filter(grepl('Methow', path)) %>%
  filter(grepl('Wenatchee', path)) %>%
  pull(SiteID)

parent_child %>%
  filter(parent %in% ques_locs |
           child %in% ques_locs) %>%
  left_join(sites_df %>%
              select(child = SiteID,
                     path))

parent_child %>%
  filter(parent %in% ques_locs |
           child %in% ques_locs) %>%
  buildPaths() %>%
  left_join(sites_df %>%
              select(end_loc = SiteID,
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
              select(child = SiteID))

parent_child %>%
  # filter(parent == "PENAWC")
  # filter(child == "WCT")
  filter(parent == "PRA")

# look at paths to each location
buildPaths(parent_child)

# add nodes to the parent child table
parent_child_nodes = addParentChildNodes(parent_child,
                                         configuration)


sites_df %>%
  select(SiteID,
         path) %>%
  left_join(configuration %>%
              select(SiteID,
                     Node) %>%
              distinct()) %>%
  rename(child = SiteID) %>%
  anti_join(parent_child) %>%
  left_join(parent_child_nodes %>%
              mutate(in_pc_tab = T) %>%
              select(Node = child,
                     in_pc_tab))


# test against old versions of parent-child table
if(root_site == "GRA") {
  parent_child_df = createParentChildDf(sites_df,
                                        configuration,
                                        startDate = "20140301") %>%
    rename(parent = ParentNode,
           child = ChildNode)
} else if(root_site == "PRA") {
  parent_child_df = createParentChildDf(sites_df,
                                        configuration,
                                        startDate = "20140701") %>%
    rename(parent = ParentNode,
           child = ChildNode)
} else if(root_site == 'TUM') {
  parent_child_df = createParentChildDf(sites_df,
                                        configuration,
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
}
proc_obs = filterDetections(obs,
                            parent_child_nodes,
                            max_obs_date)

proc_obs %>%
  summarise(n_tags = n_distinct(tag_code),
            n_weird_tags = n_distinct(tag_code[is.na(UserKeepObs)])) %>%
  mutate(prop_weird = n_weird_tags / n_tags)

proc_obs %>%
  filter(is.na(UserKeepObs)) %>%
  filter(direction == 'unknown') %>%
  # filter(grepl('WTL', node)) %>%
  select(tag_code) %>%
  distinct() %>%
  slice_sample(n = 1) %>%
  # slice(119) %>%
  left_join(proc_obs) %>%
  select(-(duration:start_date),
         -UserKeepObs) %>%
  select(-max_det) %>%
  as.data.frame()


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
                                 rename(ParentNode = parent,
                                        ChildNode = child),
                               observations = observations,
                               last_obs_date = paste0(as.numeric(str_extract(ptagis_file, "[:digit:]+")), "0930"),
                               truncate = T,
                               save_file = F)

identical(sort(unique(proc_list$ProcCapHist$TagID)),
          sort(unique(proc_obs$tag_code)))

proc_list$ProcCapHist %>%
  select(tag_code = TagID,
         TrapDate,
         node = Node,
         SiteID,
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
  # janitor::tabyl(SiteID)
  # filter(is.na(SiteID)) %>%
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
