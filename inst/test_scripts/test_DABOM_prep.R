# Author: Kevin See
# Purpose: Test new functions for processing PTAGIS data for DABOM
# Created: 2/10/2021
# Last Modified: 2/25/2021
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
# Lower Granite
#-----------------------------------------------------------------
# make a few change
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

#-----------------------------------------------------------------
# Priest Rapids
#-----------------------------------------------------------------
# customize some nodes based on DABOM framework
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
                   SiteDescription = 'Tempoary single antenna.',
                   SiteTypeName = 'Instream Remote Detection System',
                   RKM = '740.001',
                   RKMTotal = 741)) %>%
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
         Node = if_else(SiteID %in% c('CHANDL', 'SAT', 'TOP', 'SUN', 'LNR', 'ROZ', 'LMC', 'TAN') | SiteID == 'PRO' & SiteType == 'MRR',
                        'PROA0',
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


#-----------------------------------------------------------------
# Tumwater
#-----------------------------------------------------------------
# customize some nodes based on DABOM framework
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


#-----------------------------------------------------------------
# read in observations
# root_site = "GRA"
# root_site = "PRA"
root_site = "TUM"

if(root_site == "GRA") {
  ptagis_file = 'inst/extdata/LGR_Chinook_2014.csv'
} else if(root_site == "PRA") {
  ptagis_file = 'inst/extdata/UC_Sthd_2015_CTH.csv'
} else if(root_site == 'TUM') {
  ptagis_file = 'inst/extdata/TUM_Chinook_2015.csv'
}


comp_obs = compress(ptagis_file = ptagis_file,
                    #max_minutes = 5,
                    max_minutes = NA,
                    configuration = configuration,
                    units = "hours")

comp_obs %>%
  filter(node == root_site,
         event_type_name %in% c("Mark", "Recapture")) %>%
  summarise(n_tags = n_distinct(tag_code)) %>%
  pull(n_tags)

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
  filter(min_det >= start_date)# %>%
  # filter(!(node == root_site &
  #            event_type_name == "Observation"))

obs_site_codes = obs %>%
  select(node) %>%
  distinct() %>%
  left_join(configuration %>%
              select(node = Node,
                     site_code = SiteID) %>%
              distinct())

# add a couple other sites
if(root_site == "TUM") {
  obs_site_codes %<>%
    bind_rows(tibble(site_code = c("PEU",
                                   "ICU")) %>%
                left_join(configuration %>%
                            select(node = Node,
                                   site_code = SiteID) %>%
                            distinct()))
}

if(root_site == "PRA") {
  obs_site_codes %<>%
    bind_rows(configuration %>%
                filter(SiteID %in% c("HST",
                                     "RSH") |
                         Node %in% c("JD1A0",
                                     "HSTA0",
                                     "ICHA0")) %>%
                select(node = Node,
                       site_code = SiteID) %>%
                distinct())
}




#-----------------------------------------------------------------
# build parent child table
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
  # filter(! node_site %in% sites_sf$site_code)



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

if(root_site == "PRA") {
  single_site %<>%
    filter(! site_code %in% c("PRO", "METH"))
}


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

# download the NHDPlus v2 flowlines
# do you want flowlines downstream of root site? Set to TRUE if you have downstream sites
dwn_flw = T
nhd_list = queryFlowlines(sites_sf = sites_sf,
                          root_site_code = root_site,
                          min_strm_order = 2,
                          dwnstrm_sites = dwn_flw,
                          dwn_min_stream_order_diff = 4)


flowlines = nhd_list$flowlines
if(dwn_flw) {
  flowlines %<>%
    rbind(nhd_list$dwn_flowlines)
}


# plot the flowlines and the sites
ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(StreamOrde),
              size = StreamOrde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        end = 0.8) +
  scale_size_continuous(range = c(0.2, 1.2)) +
  geom_sf(data = nhd_list$basin,
          fill = NA,
          lwd = 2) +
  geom_sf(data = sites_sf,
          size = 4,
          color = "black") +
  geom_sf_label(data = sites_sf,
                aes(label = site_code)) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  labs(color = "Stream\nOrder")



# join sites to nearest hydro sequence
sites_NHDseg = st_join(sites_sf,
                       flowlines %>%
                         select(gnis_name, Hydroseq),
                       join = st_nearest_feature)

sites_NHDseg %>%
  filter(Hydroseq %in% Hydroseq[duplicated(Hydroseq)]) %>%
  arrange(Hydroseq, site_code)

# sites_NHDseg %<>%
#   mutate(Hydroseq = if_else(site_code %in% c("LWE", 'RRF', 'WEA'),
#                             Hydroseq[site_code == "LWE"],
#                             Hydroseq))

# # which sites were joined to the same hydrosequence?
# sites_sf %<>%
#   anti_join(sites_NHDseg %>%
#               filter(Hydroseq %in% Hydroseq[duplicated(Hydroseq)]) %>%
#               arrange(Hydroseq, rkm) %>%
#               as_tibble() %>%
#               select(site_code)) %>%
#   rbind(sites_NHDseg %>%
#           filter(Hydroseq %in% Hydroseq[duplicated(Hydroseq)]) %>%
#           arrange(Hydroseq, rkm) %>%
#           group_by(Hydroseq) %>%
#           slice(1) %>%
#           ungroup %>%
#           select(any_of(names(sites_sf))))

# if(root_site == "TUM") {
#   sites_sf %<>%
#     mutate(site_code = recode(site_code,
#                               "TUF" = "TUM"))
# }


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
}

if(root_site == 'PRA') {
  parent_child = sites_sf %>%
    filter(! site_code %in% c("MC1", "MC2", "MCJ",
                              "FDD",
                              "MWC",
                              "LMT",
                              "NFT",
                              "UMT",
                              "YHC",
                              # "PRH",
                              "HN1", "HN3",
                              "HSM", "HSU",
                              "LOR",
                              "EWC",
                              "LBT",
                              "LTP")) %>%
    buildParentChild(flowlines,
                     rm_na_parent = T) %>%
    editParentChild(child_locs = c("WEA",
                                   'ICH', 'JD1', 'PRO', 'TMF', 'PRV', 'RSH',
                                   'PRH',
                                   'PRA',
                                   "ENL"),
                    parent_locs = c("RIA",
                                    rep('JDA', 6),
                                    "RSH",
                                    'RSH',
                                    'RIA'),
                    new_parent_locs = c("RRF",
                                        rep('PRA', 6),
                                        "PRA",
                                        'JDA',
                                        'RRF'),
                    switch_parent_child = list(c("JDA", "PRA"),
                                               c("RSH", "PRA"))) %>%
    # because PRH and PRA are on the same hydrosequence, need to remove this row
    filter(!(parent == 'PRH' & child == "RIA"))
}

parent_child %>%
  filter(child %in% child[duplicated(child)])

parent_child %>%
  inner_join(parent_child %>%
               select(child = parent,
                      parent = child))

parent_child %>%
  # filter(parent == "RRF")
  # filter(child == "WCT")
  filter(child == "JDA")

buildPaths(parent_child)

sites_df = writePRDNodeNetwork() %>%
  mutate(across(c(SiteID, Step3),
                recode,
                "BelowJD1" = "JDA"),
         path = str_replace(path, "BelowJD1", "JDA"))
sites_sf %>%
  filter(as.numeric(str_sub(rkm, 0, 3)) < 639) %>%
  anti_join(sites_df %>%
              filter(Step2 == "BelowPriest") %>%
              select(site_code = SiteID))

sites_df %>%
  filter(Step2 == "BelowPriest",
         Step4 == '') %>%
  select(site_code = SiteID) %>%
  anti_join(parent_child %>%
              filter(parent == 'PRA') %>%
              rename(site_code = child))

parent_child %>%
  filter(parent == 'PRA') %>%
  rename(site_code = child) %>%
  anti_join(sites_df %>%
              filter(Step2 == "BelowPriest",
                     Step4 == '') %>%
              select(site_code = SiteID))


parent_child_nodes = addParentChildNodes(parent_child,
                                         configuration)


parent_child %>%
  select(child) %>%
  left_join(configuration %>%
              select(child = SiteID,
                     Node) %>%
              distinct())


buildPaths(parent_child_nodes)

node_order = buildNodeOrder(parent_child_nodes)

# test against old versions of parent-child table
parent_child_df = createParentChildDf(writeLGRNodeNetwork(),
                                      configuration,
                                      startDate = "20140301") %>%
  rename(parent = ParentNode,
         child = ChildNode)


parent_child_df = createParentChildDf(writePRDNodeNetwork(),
                                      configuration,
                                      startDate = "20140701") %>%
  rename(parent = ParentNode,
         child = ChildNode)

parent_child_df = createParentChildDf(writeTUMNodeNetwork_noUWE(),
                                      configuration,
                                      startDate = "20150701") %>%
  rename(parent = ParentNode,
         child = ChildNode)


anti_join(parent_child_nodes,
          parent_child_df)

anti_join(parent_child_df,
          parent_child_nodes)


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
              filter(node != "TUM") %>%
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
  slice(5) %>%
  left_join(obs_direct) %>%
  select(tag_code,
         event_type_name,
         matches("node"),
         direction,
         min_det,
         path)


proc_obs = obs_direct %>%
  group_by(tag_code) %>%
  nest() %>%
  mutate(proc = map(data,
                    .f = function(x) {
                      if(sum(x$direction %in% c("backward", "unknown")) == 0) {
                        x %>%
                          mutate(AutoProcStatus = T,
                                 UserProcStatus = T) %>%
                          return()
                      } else if(sum(x$direction %in% c("backward")) > 0 |
                                sum(x$direction %in% c("unknown")) > 0) {
                        spwn_loc = x %>%
                          filter(slot == max(slot[direction %in% c("forward",
                                                                   "unknown")]))

                        dbl_nodes = x %>%
                          group_by(node) %>%
                          summarise(n_node_dets = n_distinct(slot),
                                    min_slot = min(slot),
                                    max_slot = max(slot),
                                    last_det = max(min_det),
                                    .groups = "drop") %>%
                          filter(n_node_dets > 1)

                        if(nrow(dbl_nodes) > 0) {

                          x %>%
                            # select(-c(duration:start_date)) %>%
                            left_join(dbl_nodes,
                                      by = "node") %>%
                            tidyr::fill(min_slot, max_slot,
                                        .direction = "updown") %>%
                            rowwise() %>%
                            filter(slot < min_slot | slot >= max_slot) %>%
                            mutate(in_spawn_path = if_else(grepl(node, spwn_loc$path),
                                                           T, F)) %>%
                            filter(in_spawn_path) %>%
                            filter(slot <= spwn_loc$slot) %>%
                            group_by(node) %>%
                            mutate(max_slot = max(slot),
                                   max_min_det = max(min_det)) %>%
                            ungroup() %>%
                            mutate(AutoProcStatus = if_else(in_spawn_path &
                                                              slot == max_slot,
                                                            T, F),
                                   UserProcStatus = NA) %>%
                            ungroup() %>%
                            select(-c(in_spawn_path:max_slot)) %>%
                            full_join(x,
                                      by = c("node", "slot", "event_type_name", "n_dets", "min_det", "max_det", "duration", "travel_time", "start_date", "node_order", "path", "direction")) %>%
                            tidyr::replace_na(list(AutoProcStatus = F)) %>%
                            arrange(slot) %>%
                            # select(-c(duration:start_date)) %>%
                            select(any_of(names(x)),
                                   ends_with("ProcStatus"))%>%
                            return()
                        } else {
                          x %>%
                            # select(-c(duration:start_date)) %>%
                            rowwise() %>%
                            mutate(in_spawn_path = if_else(grepl(node, spwn_loc$path),
                                                           T, F)) %>%
                            filter(in_spawn_path) %>%
                            filter(slot <= spwn_loc$slot) %>%
                            group_by(node) %>%
                            mutate(max_slot = max(slot),
                                   max_min_det = max(min_det)) %>%
                            ungroup() %>%
                            mutate(AutoProcStatus = if_else(in_spawn_path &
                                                              slot == max_slot,
                                                            T, F),
                                   UserProcStatus = NA) %>%
                            ungroup() %>%
                            select(-c(in_spawn_path:max_slot)) %>%
                            full_join(x,
                                      by = c("node", "slot", "event_type_name", "n_dets", "min_det", "max_det", "duration", "travel_time", "start_date", "node_order", "path", "direction")) %>%
                            tidyr::replace_na(list(AutoProcStatus = F)) %>%
                            arrange(slot) %>%
                            # select(-c(duration:start_date)) %>%
                            select(any_of(names(x)),
                                   ends_with("ProcStatus")) %>%
                            return()
                        }
                      }
                    })) %>%
  select(-data) %>%
  unnest(proc) %>%
  ungroup()


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
