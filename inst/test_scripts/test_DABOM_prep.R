# Author: Kevin See
# Purpose: Test new functions for processing PTAGIS data for DABOM
# Created: 2/10/2021
# Last Modified: 2/10/2021
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


# for Priest Rapid version
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
                        'BelowJD1',
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
# read in observations
ptagis_file = 'inst/extdata/LGR_Chinook_2014.csv'
ptagis_file = 'inst/extdata/UC_Sthd_2015_CTH.csv'

comp_obs = compress(ptagis_file = ptagis_file,
                    #max_minutes = 5,
                    max_minutes = NA,
                    configuration = configuration,
                    units = "hours")

comp_obs %>%
  filter(node == "PRA",
         event_type_name %in% c("Mark", "Recapture")) %>%
  summarise(n_tags = n_distinct(tag_code)) %>%
  pull(n_tags)

n_distinct(comp_obs$tag_code)

# find trap data at Lower Granite, and remove detections prior to that
obs = comp_obs %>%
  left_join(comp_obs %>%
              filter(node == "PRA",
                     event_type_name %in% c("Mark", "Recapture")) %>%
              group_by(tag_code) %>%
              filter(max_det == max(max_det)) %>%
              summarise(start_date = max_det,
                        .groups = "drop"),
            by = "tag_code") %>%
  filter(min_det >= start_date) %>%
  filter(!(node == "PRA" &
             event_type_name == "Observation"))

obs_site_codes = obs %>%
  select(node) %>%
  distinct() %>%
  left_join(configuration %>%
              select(node = Node,
                     site_code = SiteID) %>%
              distinct())

#-----------------------------------------------------------------
# build parent child table
# which sites do we care about for this exercise?
sites_sf = extractSites(ptagis_file,
                        as_sf = T)

sites_sf %<>%
  filter(site_code %in% unique(obs_site_codes$site_code))

# join sites to nearest hydro sequence
sites_NHDseg = st_join(sites_sf,
                       flowlines %>%
                         select(gnis_name, Hydroseq),
                       join = st_nearest_feature)

# which sites were joined to the same hydrosequence?
sites_sf %<>%
  anti_join(sites_NHDseg %>%
              filter(Hydroseq %in% Hydroseq[duplicated(Hydroseq)]) %>%
              arrange(Hydroseq, rkm) %>%
              as_tibble() %>%
              select(site_code)) %>%
  rbind(sites_NHDseg %>%
          filter(Hydroseq %in% Hydroseq[duplicated(Hydroseq)]) %>%
          arrange(Hydroseq, rkm) %>%
          group_by(Hydroseq) %>%
          slice(1) %>%
          ungroup %>%
          select(any_of(names(sites_sf))))


# build it from list of exisiting sites in LGR DABOM
all_meta = queryPtagisMeta()
sites_sf = writeLGRNodeNetwork() %>%
  select(site_code = SiteID) %>%
  left_join(all_meta %>%
              select(site_code = siteCode,
                   site_name = siteName,
                   site_type = siteType,
                   type = Type,
                   latitude,
                   longitude,
                   rkm,
                   site_description = siteDescription) %>%
              distinct()) %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude",
                      "latitude"),
           crs = 4326) %>%
  st_transform(crs = 5070)

# get the flowlines to snap those sites to
dwn_flw = T
nhd_list = queryFlowlines(sites_sf = sites_sf,
                          root_site_code = "GRA",
                          min_strm_order = 2,
                          dwnstrm_sites = dwn_flw,
                          dwn_min_stream_order_diff = 6)

# pull out the flowlines
flowlines = nhd_list$flowlines
# add downstream flowlines if necessary
if(dwn_flw) {
  flowlines %<>%
    rbind(nhd_list$dwn_flowlines)
}

# plot the flowlines and the sites
ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(StreamOrde))) +
  scale_color_viridis_d(direction = -1,
                        end = 0.7) +
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



parent_child = buildParentChild(sites_sf,
                                flowlines,
                                add_rkm = T)

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


anti_join(parent_child,
          parent_child_df)

anti_join(parent_child_df,
          parent_child)

parent_child_df %>%
  filter(child == "GRA")


obs %>%
  filter(node %in% parent_child_df$child)

node_order = buildNodeOrder(parent_child_df)

# which observation locations are not in node_order?
obs %>%
  left_join(node_order) %>%
  filter(is.na(node_order)) %>%
  janitor::tabyl(node) %>%
  janitor::adorn_pct_formatting() %>%
  arrange(n)

# filter out observations at sites not included in the node order
# determine direction of movement
obs_direct = obs %>%
# obs_direct = comp_obs %>%
  addDirection(parent_child_df %>%
                 select(parent, child))

obs_direct %>%
  filter(direction == "unknown") %>%
  select(tag_code) %>%
  distinct() %>%
  slice(2) %>%
  left_join(obs_direct) %>%
  # rowwise() %>%
  # filter(!grepl(lag_node, path)) %>%
  # filter(tag_code == "384.3B239B64C9") %>%
  # filter(tag_code == "384.3B23AB9CF7") %>%
  # filter(tag_code == "384.3B23AB9246") %>%
  select(tag_code,
         event_type_name,
         matches("node"),
         direction,
         min_det,
         path)

obs_direct %>%
  group_by(tag_code) %>%
  filter(sum(direction == "unknown") > 0) %>%
  # filter(sum(direction == "backward") > 0) %>%
  ungroup() %>%
  select(tag_code) %>%
  distinct() %>%
  slice(7) %>%
  left_join(obs_direct) %>%
  select(-start_date,
         -path)

test = obs_direct %>%
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
                          filter(slot == max(slot[direction == "forward"]))

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
                            left_join(dbl_nodes,
                                      by = "node") %>%
                            tidyr::fill(min_slot, max_slot,
                                        .direction = "updown") %>%
                            rowwise() %>%
                            mutate(AutoProcStatus = if_else(grepl(node, spwn_loc$path) &
                                                              (slot < min_slot | slot >= max_slot) &
                                                              slot <= spwn_loc$slot,
                                                            T, F),
                                   UserProcStatus = NA) %>%
                            ungroup() %>%
                            select(-c(duration:start_date))
                          select(-c(n_node_dets:last_det)) %>%
                            return()
                        } else {
                          x %>%
                            rowwise() %>%
                            mutate(AutoProcStatus = if_else(grepl(node, spwn_loc$path) &
                                                              slot <= spwn_loc$slot,
                                                            T, F),
                                   UserProcStatus = NA) %>%
                            ungroup() %>%
                            # select(-c(duration:start_date))
                            return()
                        }
                        # } else if(sum(x$direction %in% c("unknown")) > 0) {
                        #   spwn_loc = x %>%
                        #     filter(slot == max(slot[direction == "forward"]))
                        #
                        #   x %>%
                        #     rowwise() %>%
                        #     mutate(AutoProcStatus = if_else(grepl(node, spwn_loc$path) &
                        #                                       slot <= spwn_loc$slot,
                        #                                     T, F),
                        #            UserProcStatus = NA) %>%
                        #     ungroup() %>%
                        #     # select(-c(duration:start_date))
                        #     return()
                      }
                    }))


x = obs_direct %>%
  group_by(tag_code) %>%
  filter(sum(direction == "unknown") > 0) %>%
  # filter(sum(direction == "backward") > 0) %>%
  ungroup() %>%
  select(tag_code) %>%
  distinct() %>%
  slice(1) %>%
  left_join(obs_direct) %>%
  select(-tag_code)


proc_obs = test %>%
  select(-data) %>%
  unnest(proc) %>%
  ungroup()


obs_direct %>%
  group_by(tag_code) %>%
  filter(sum(direction == "unknown") > 0) %>%
  # filter(sum(direction == "backward") > 0) %>%
  ungroup() %>%
  select(tag_code) %>%
  distinct() %>%
  slice(1) %>%
  left_join(proc_obs) %>%
  select(-c(duration:start_date))


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
