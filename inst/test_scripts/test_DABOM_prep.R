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

#-----------------------------------------------------------------
# read in observations
ptagis_file = 'inst/extdata/LGR_Chinook_2014.csv'

comp_obs = compress(ptagis_file = ptagis_file,
                    #max_minutes = 5,
                    max_minutes = NA,
                    configuration = configuration,
                    units = "mins")

comp_obs %>%
  filter(node == "GRA",
         event_type_name %in% c("Mark", "Recapture")) %>%
  summarise(n_tags = n_distinct(tag_code)) %>%
  pull(n_tags)

n_distinct(comp_obs$tag_code)

# find trap data at Lower Granite, and remove detections prior to that
obs = comp_obs %>%
  left_join(comp_obs %>%
              filter(node == "GRA",
                     event_type_name %in% c("Mark", "Recapture")) %>%
              group_by(tag_code) %>%
              filter(max_det == max(max_det)) %>%
              summarise(start_date = max_det,
                        .groups = "drop"),
            by = "tag_code") %>%
  filter(min_det >= start_date) %>%
  filter(!(node == "GRA" &
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
  mutate(across(c(ParentNode, ChildNode),
                str_remove,
                "A0$"),
         across(c(ParentNode, ChildNode),
                str_remove,
                "B0$")) %>%
  filter(ParentNode != ChildNode) %>%
  rename(parent = ParentNode,
         child = ChildNode)

anti_join(parent_child,
          parent_child_df)

anti_join(parent_child_df,
          parent_child)
