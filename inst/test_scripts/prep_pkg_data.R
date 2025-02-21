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
library(usethis)
# library(PITcleanr)
devtools::load_all()

#-----------------------------------------------------------------
# download all the metadata for all sites from PTAGIS
org_config = buildConfig()

#-----------------------------------------------------------------
# designate a starting point
root_site_code = c("LGR",
                   "PRA",
                   "PRO",
                   "TUM")[3]

#-----------------------------------------------------------------
# update configuration file depending on which DABOM model is being run

#-----------------------------------------------------------------
# Lower Granite - LGR
#-----------------------------------------------------------------
if(root_site_code == "LGR") {
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
if(root_site_code == "PRA") {
  configuration = org_config %>%
    # manually add site for Colockum Creek (not in PTAGIS)
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
    # this puts CLK on Colockum Creek, but between PRA and RIA
    # latitude = 47.29722788926544,
    # longitude = -120.10577913008702)) %>%
    filter(!(site_code == 'WAN' & site_type == 'MRR'),
           !(site_code == 'TMF' & site_type == 'MRR'),
           !(site_code == 'PRO' & site_type == 'MRR')) %>%
    mutate(across(node,
                  ~ case_when(site_code %in% c('RIA', 'RRF', 'WEA', 'PRV', 'PRH') ~ site_code,
                              site_code == 'PRDLD1' ~ "PRA",
                              . == "LWE" ~ "LWE_D",
                              site_code == "LWB" ~ "LWE_D",
                              # any fish seen at Dryden dam should also be seen at LWE
                              site_code == 'DRY' ~ "LWE_U",
                              site_code %in% c('TUF', 'TUMFBY', 'TUM') ~ "TUM",
                              site_code == 'LNF' & antenna_id %in% c('01', '02') ~ "LNF_U",
                              site_code == 'LNF' & antenna_id %in% c('03', '04') ~ "LNF_D",
                              site_code == 'LEAV' ~ "LNF_U",
                              site_code == 'ICL' & config_id == 100 ~ "ICL_D",
                              site_code == 'CHIWAC' ~ "CHW_U",
                              site_code == 'CHIWAR' ~ "CHL_U",
                              site_code == "CWT" ~ "CHL_U",
                              site_code == 'WHITER' ~ "WTL_U",
                              site_code == 'LWENAT' ~ "LWN_U",
                              site_code == 'NASONC' ~ "NAL_U",
                              # any fish seen at Chiwawa acclimation pond gets moved to CHL
                              site_code == 'CHP' ~ 'CHL_U',
                              site_code == 'EBO' ~ "EBO_D",
                              site_code == 'RRJ' ~ 'RRF',
                              site_code == "MAD" & config_id == 110 & antenna_id == "01" ~ "MAD_U",
                              site_code == 'EHL' & config_id == 100 & antenna_id == '02' ~ 'EHL_D',
                              site_code == 'EHL' & config_id == 100 & antenna_id == '01' ~ 'EHL_U',
                              site_code == 'EHL' & config_id == 110 & antenna_id == '03' ~ 'EHL_D',
                              site_code == 'EHL' & config_id == 110 & antenna_id %in% c('01', '02') ~ 'EHL_U',
                              # combine a couple sites in the Entiat
                              site_code %in% c("ENS", "ENM") ~ "ENA_U",
                              site_code == "WEH" & antenna_id == "A2" ~ "WEH_D",
                              site_code == "WEH" & antenna_id != "A2" ~ "WEH_U",
                              site_code == "LMB" ~ "LMR_D",
                              site_code == 'MRC' ~ 'MRC_D',
                              site_code %in% c('SSC', '18N', 'MHB', 'M3R', 'MWF') ~ 'MRC_U',
                              site_code == 'LLC' & config_id == 100 & antenna_id %in% c("D1", "D2") ~ "LLC_U",
                              # ZSL has definitive up/down antennas in initial configurations, but it gets more complicated after that
                              site_code == "ZSL" &
                                str_detect(antenna_group,
                                           "(?i)Weir 3") &
                                config_id %in% c("100", "110") ~ "ZSL_D",
                              site_code == "ZSL" &
                                str_detect(antenna_group,
                                           "(?i)Weir 2") &
                                config_id %in% c("100", "110") ~ "ZSL_U",
                              site_code == "ZSL" &
                                !config_id %in% c("100", "110") ~ "ZSL_D",
                              site_code == 'BPC' &
                                config_id == 100 &
                                antenna_id %in% c("C1", "C2") ~ "BPC_U",
                              site_code == 'BPC' &
                                config_id == 100 &
                                antenna_id %in% c("C3") ~ "BPC_D",
                              site_code == 'PRO' & site_type == 'INT' ~ 'PRO_D',
                              # grab all sites upstream of Prosser dam, and assign them to PRO_U
                              site_code != "PRO" &
                                as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) == 539 &
                                as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,2]) >= 76 ~ "PRO_U",
                              site_code == 'ICH' ~ 'ICH_D',
                              str_detect(rkm, '522\\.') & rkm_total > 538 ~ 'ICH_U',
                              site_code == 'MDR' ~ 'MDR_D',
                              site_code %in% c('LWD', 'BGM', 'NBA', 'MCD') ~ 'MDR_U',
                              site_code == 'HST' ~ 'HST_D',
                              site_code %in% c('BBT', 'COP', 'PAT') ~ 'HST_U',
                              as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) == 351 &
                                site_code != "JD1" ~ "JD1_U",
                              site_code == 'JD1' ~ 'JD1_D',
                              site_code != 'JD1' &
                                as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) < 351 &
                                str_detect(site_code, "^COLR", negate = T) ~ 'JDA',
                              .default = node
                  ))) |>
    distinct() %>%
    # correct a couple rkm values
    mutate(across(rkm,
                  ~ case_when(site_code == 'SA1' ~ '858.041.003',
                              site_code == 'TON' ~ '858.133.001',
                              str_detect(node, "WEH") ~ '829.001',
                              site_code == "MSH" ~ '843.082',
                              site_code == "METH" ~ '843.083',
                              .default = .)),
           across(rkm_total,
                  ~ case_when(site_code == 'SA1' ~ 902,
                              site_code == 'TON' ~ 992,
                              str_detect(node, "WEH") ~ 830,
                              site_code == "MSH" ~ 925,
                              site_code == "METH" ~ 926,
                              .default = .)))

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
if(root_site_code == "PRO") {
  # customize some nodes based on DABOM framework
  configuration = org_config %>%
    mutate(
      across(
        node,
        ~ case_when(site_code %in% "PRO" ~ "PRO",
                    site_code %in% c("NFTEAN", "TEANAR", "TEANM", "TEANWF") ~ "LMT_U",
                    site_code == "ROZ" & !antenna_id %in% c('01', '02', '03') ~ NA_character_,
                    site_code == 'TAN' & config_id %in% c(120, 130) ~ "TAN_D",
                    site_code %in% c('MC1', 'MC2', 'MCJ', 'MCN') ~ 'MCN',
                    site_code == 'ICH' ~ 'ICH_D',
                    str_detect(rkm, '522\\.') & rkm_total > 538 ~ 'ICH_U',
                    as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) == 351 &
                      site_code != "JD1" ~ "JD1_U",
                    site_code == 'JD1' ~ 'JD1_D',
                    site_code != 'JD1' &
                      as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) < 351 &
                      str_detect(site_code, "^COLR", negate = T) ~ 'JDA',
                    site_code == 'PRA' ~ 'PRA_D',
                    site_code != 'PRA' & as.integer(stringr::str_split(rkm, '\\.', simplify = T)[,1]) >= 639 ~ 'PRA_U',
                    .default = .))) |>
    # add a missing lat/long
    mutate(across(latitude,
                  ~ case_when(site_code == "SWK" ~ 47.210348,
                                .default = .)),
           across(longitude,
                  ~ case_when(site_code == "SWK" ~ -120.699021,
                              .default = .)))

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
if(root_site_code == "TUM") {
  configuration = org_config %>%
    muate(
      across(
        node,
        ~ case_when(site_code %in% c('LNF', 'LEAV') ~ 'LNF',
                    site_code %in% c('TUF', 'TUMFBY', 'TUM') ~ 'TUM',
                    site_code == 'CHIWAC' ~ 'CHW_U',
                    site_code %in% c('CHIWAR', 'CHIW') ~ 'CHL_U',
                    site_code == 'CHIKAC' ~ 'CHU_U',
                    site_code == 'WHITER' ~ 'WTL_U',
                    site_code == 'LWENAT' ~ 'LWN_U',
                    site_code == 'NASONC' ~ 'NAL_U',
                    .default = .)
      )
    ) |>
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

# want to cut up the flowlines at an upstream point?
# upstream extent of study area (cut off areas further upstream)
upstrm_loc = case_when(root_site_code == "PRA" ~ "Chief Joseph Dam",
                       root_site_code == "LGR" ~ "Hells Canyon Dam",
                       .default = NA_character_)
if(!is.na(upstrm_loc)) {

  library(ggmap)

  upstrm_comid = ggmap::geocode(upstrm_loc, output = "latlon") %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = 4326) %>%
    nhdplusTools::discover_nhdplus_id()

} else {
  upstrm_comid = NULL
}

nhd_list = queryFlowlines(sites_sf = sites_sf,
                          root_site_code = root_site_code,
                          min_strm_order = 2,
                          max_upstream_comid = upstrm_comid)

# compile the upstream and downstream flowlines
flowlines = nhd_list$flowlines

#-----------------------------------------------------------------
# plot the flowlines and the sites
ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(streamorde),
              size = streamorde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        name = "Stream\nOrder",
                        end = 0.8) +
  scale_size_continuous(range = c(0.2, 1.2),
                        guide = 'none') +
  geom_sf(data = nhd_list$basin,
          fill = NA,
          lwd = 2) +
  geom_sf(data = sites_sf,
          size = 3,
          color = "black") +
  # geom_sf_label(data = sites_sf,
  #               size = 1.5,
  #               aes(label = site_code)) +
  ggrepel::geom_label_repel(
    data = sites_sf |>
      filter(site_code != root_site_code),
    aes(label = site_code,
        geometry = geometry),
    size = 1.5,
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = 100
  ) +
  geom_sf_label(data = sites_sf %>%
                  filter(site_code == root_site_code),
                aes(label = site_code),
                color = "red") +
  theme_bw() +
  theme(axis.title = element_blank())

#-----------------------------------------------------------------
# build parent child table
if(root_site_code == "PRO") {
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
} else if(root_site_code == "TUM") {
  parent_child = sites_sf %>%
    buildParentChild(flowlines,
                     rm_na_parent = F,
                     add_rkm = F) %>%
    editParentChild(fix_list = list(c(NA, "ICL", "TUM"),
                                    c(NA, "PES", "TUM"),
                                    c(NA, "LNF", "ICL")),
                    switch_parent_child = list(c("ICL", 'TUM')))
} else if(root_site_code == "PRA") {
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
} else if(root_site_code == "LGR") {
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
  write_csv(here("inst/extdata",
                 paste0(root_site_code, "_configuration.csv")))

parent_child |>
  write_csv(here("inst/extdata",
                 paste0(root_site_code, "_parent_child.csv")))

save(configuration,
     sites_sf,
     flowlines,
     parent_child,
     file = here("inst/extdata",
                 paste0(root_site_code, "_site_config.Rdata")))



#-----------------------------------------------------------------
# smolts tagged at Upper Lemhi RST
#-----------------------------------------------------------------
root_site_code = "LEMTRP"

# update configuration file
# combine some sites at and below Bonneville
configuration <-
  buildConfig(node_assign = "site") |>
  mutate(across(node,
                ~ case_when(as.numeric(str_sub(rkm, 1, 3)) <= 234 ~ "B2J",
                            site_code %in% c("GRJ", "GRS") ~ "GRJ",
                            .default = .))) |>
  filter(!is.na(node))

# read in PTAGIS detections
ptagis_file = here('inst/extdata',
                   "LEMTRP_chnk_cth_2021.csv")


ptagis_obs <- readCTH(ptagis_file) |>
  arrange(tag_code,
          event_date_time_value)

# extract the sites with detections
sites_sf <-
  ptagis_obs |>
  filter(!event_site_code_value == "ORPHAN") |>
  # filter(tag_code %in% pluck(qc_lst, "orphan_tags"),
  #        tag_code %in% pluck(qc_lst, "disown_tags"))
  extractSites(as_sf = T,
               configuration = configuration,
               max_date = "20220630") |>
  arrange(rkm)

# refine further
sites_sf <-
  sites_sf |>
  left_join(configuration |>
              select(site_code,
                     rkm_total) |>
              distinct()) |>
  filter(nchar(rkm) <= 7 |
           (str_detect(rkm, "522.303.416") &
              rkm_total <= rkm_total[site_code == "LEMTRP"] &
              nchar(rkm) == 15),
         site_code != "HAYDNC")


# query flowlines
nhd_list <-
  queryFlowlines(sites_sf,
                 # root_site_code = "LEMTRP",
                 root_site_code = "LLR",
                 min_strm_order = 2,
                 dwnstrm_sites = T,
                 dwn_min_stream_order_diff = 4,
                 buffer_dist = units::as_units(10, "km"))

# compile the upstream and downstream flowlines
flowlines = nhd_list$flowlines |>
  rbind(nhd_list$dwn_flowlines)

# construct parent-child table
parent_child = sites_sf |>
  buildParentChild(flowlines,
                   rm_na_parent = T,
                   add_rkm = T)

plotNodes(parent_child)

# flip direction of parent/child relationships
parent_child <-
  parent_child |>
  select(p = parent,
         c = child,
         p_rkm = parent_rkm,
         c_rkm = child_rkm) |>
  mutate(parent = c,
         child = p,
         parent_rkm = c_rkm,
         child_rkm = p_rkm) |>
  select(parent,
         child,
         parent_rkm,
         child_rkm)

#-----------------------------------------------------------------
# write configuration & parent-child table
configuration |>
  write_csv(here("inst/extdata/LEMTRP",
                 paste0(root_site_code, "_configuration.csv")))

parent_child |>
  write_csv(here("inst/extdata/LEMTRP",
                 paste0(root_site_code, "_parent_child.csv")))

flowlines |>
  st_write(dsn = here("inst/extdata/LEMTRP",
                      paste0(root_site_code, "_flowlines.gpkg")),
           delete_dsn = T)


save(configuration,
     sites_sf,
     flowlines,
     parent_child,
     file = here("inst/extdata/LEMTRP",
                 paste0(root_site_code, "_site_config.Rdata")))
