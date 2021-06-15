# Author: Kevin See
# Purpose: Test functions related to downloading and using NHDPlus v2 layer
# Created: 2/1/2021
# Last Modified: 2/1/2021
# Notes: some new functions are based on those written by Ryan Kinzer.

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
all_meta = queryPtagisMeta()

# which sites do we care about for this exercise?
model_sites = writeLGRNodeNetwork()
xtabs(~ Step3, model_sites)

# model_sites = writeTUMNodeNetwork()
# model_sites = writePRONodeNetwork()
# model_sites = writePRDNodeNetwork()

# create sf points of a subset of sites
sub_sites = model_sites %>%
  # filter(Step3 == "SFSalmon") %>%
  # filter(SiteID == "GRA" | Step3 == "SFSalmon") %>%
  filter(SiteID == "GRA" | Step3 == "Potlatch") %>%
  # filter(SiteID == "GRA" | Step2 == "NE_Oregon") %>%
  # filter(Step3 == "UpperSalmon") %>%
  # filter(Step3 == "UpperSalmon" | SiteID == "GRA") %>%
  select(SiteID) %>%
  left_join(all_meta %>%
              select(SiteID = siteCode,
                     siteName,
                     siteType,
                     coordinateType,
                     latitude,
                     longitude,
                     rkm) %>%
              distinct()) %>%
  filter(!is.na(latitude)) %>%
  group_by(SiteID) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf(coords = c("longitude",
                      "latitude"),
           crs = 4326) %>%
  st_transform(5070)

# which site is furthest downstream?
# root_site = sub_sites %>%
#   slice(1)
root_site = sub_sites %>%
  filter(SiteID == "USE")

# download the NHDPlus v2 flowlines
# do you want flowlines downstream of root site? Set to TRUE if you have downstream sites
dwn_flw = T
nhd_list = queryFlowlines(sites_sf = sub_sites,
                          root_site_code = root_site$SiteID,
                          min_strm_order = 2,
                          dwnstrm_sites = dwn_flw,
                          dwn_min_stream_order_diff = 2)


flowlines = nhd_list$flowlines
if(dwn_flw) {
  flowlines %<>%
  rbind(nhd_list$dwn_flowlines)
}

# join sites to nearest hydro sequence
sites_NHDseg = st_join(sub_sites,
                       flowlines %>%
                         select(gnis_name, Hydroseq),
                       join = st_nearest_feature)


# plot the flowlines and the sites
ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(StreamOrde))) +
  scale_color_viridis_d(direction = -1,
                        end = 0.7) +
  geom_sf(data = nhd_list$basin,
          fill = NA,
          lwd = 2) +
  geom_sf(data = sites_NHDseg,
          size = 4,
          color = "black") +
  geom_sf_label(data = sub_sites,
                aes(label = SiteID)) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  labs(color = "Stream\nOrder")



#-----------------------------------------------------------------
# Build parent-child tables
# these are site-based, not node-based

# Original parent child table, based on older functions
# use this to compare new output
# build an initial configuration file
org_config = buildConfig()
parent_child_df = createParentChildDf(model_sites %>%
                                        filter(SiteID %in% sub_sites$SiteID),
                                      org_config,
                                      startDate = "20190101") %>%
  mutate(across(c(ParentNode, ChildNode),
                str_remove,
                "A0$"),
         across(c(ParentNode, ChildNode),
                str_remove,
                "B0$")) %>%
  filter(ParentNode != ChildNode) %>%
  rename(parent = ParentNode,
         child = ChildNode)

# use new functions, and not the configuration file
parent_child_test = buildParentChild(sub_sites,
                                     flowlines,
                                     add_rkm = T)
# make a few edits
parent_child_test %<>%
  filter(!is.na(parent))

parent_child_test = editParentChild(parent_child_test,
                                    parent_locs = c("USI", "CEY"),
                                    child_locs = c("SAWT", "YANKFK"),
                                    new_parent_locs = c('STL', 'YFK'))


parent_child_test = editParentChild(parent_child_test,
                                    child_locs = c("PEU", "ICM", "ICU", 'LNF', 'LEAV'),
                                    parent_locs = c("TUM", "TUM", "TUM", "TUM", "TUM"),
                                    new_parent_locs = c('PES', 'ICL', 'ICM', "ICL", "LNF"))

parent_child_test = editParentChild(parent_child_test,
                                    child_locs = c("GRANDW", 'CATHEW', 'LOOKGC'),
                                    parent_locs = c("UGR", "UGR", 'LOOH'),
                                    new_parent_locs = c("UGS", "CCW", "GRA"))

parent_child_test = editParentChild(parent_child_test,
                                    child_locs = c("MCN", "JD1", "ICH", "PRA"),
                                    parent_locs = c(NA, NA, "MCN", "MCN"),
                                    new_parent_locs = c("PRO", "PRO", "PRO", "PRO")) %>%
  anti_join(tibble(parent = c("MCN"),
                   child = c("PRA", "PRO", "ICH")))

# compare with PITcleanr parent-child table
parent_child_test
parent_child_df

anti_join(parent_child_test,
          parent_child_df)

anti_join(parent_child_df,
          parent_child_test)


parent_child_test %>%
  rename(ParentNode = parent,
         ChildNode = child) %>%
  getValidPaths(root_site = root_site$SiteID)

findDwnstrmSite("SAWT",
                flowlines,
                sites_NHDseg)


