# Author: Kevin See
# Purpose: Test new functions for processing PTAGIS data for DABOM
# Created: 2/10/2021
# Last Modified: 9/11/2023
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(sf)
library(magrittr)
library(here)
# library(PITcleanr)
devtools::load_all()

#-----------------------------------------------------------------
# designate a starting point
root_site = c("LGR",
              "PRA",
              "PRO",
              "TUM")[2]

#-----------------------------------------------------------------
# load configuration and parent-child table
load(here("inst/extdata",
          paste0(root_site, "_site_config.Rdata")))

configuration <-
  read_csv(here("inst/extdata",
                paste0(root_site, "_configuration.csv")),
           show_col_types = F)

parent_child <-
  read_csv(here("inst/extdata",
                paste0(root_site, "_parent_child.csv")),
           show_col_types = F)


# add nodes to parent-child
parent_child_nodes <-
  addParentChildNodes(parent_child,
                      configuration = configuration)

#-----------------------------------------------------------------
# read in PTAGIS observations

# locate the file
ptagis_file <-
  list.files("inst/extdata")[str_detect(list.files("inst/extdata"), root_site) &
                                            str_detect(list.files("inst/extdata"), "cth")]

# read in detections
ptagis_cth <- readCTH(paste0("inst/extdata/",
                             ptagis_file),
                      file_type = "PTAGIS")

# QA/QC detections
qcTagHistory(ptagis_cth)

#-----------------------------------------------------------------
# set minimum and maximum detection dates
spwn_yr <-
  str_extract(ptagis_file, "[:digit:]+") |>
  as.numeric()

min_obs_date <-
  case_when(
    root_site == "LGR" ~ paste0(spwn_yr, "0301"),
    root_site == "PRA" ~ paste0(spwn_yr - 1, "0601"),
    root_site == "PRO" ~ paste0(spwn_yr - 1, "0601"),
    root_site == "TUM" ~ paste0(spwn_yr, "0301")
  )

max_obs_date <-
  case_when(
    root_site == "LGR" ~ paste0(spwn_yr, "1031"),
    root_site == "PRA" ~ paste0(spwn_yr, "0531"),
    root_site == "PRO" ~ paste0(spwn_yr, "0531"),
    root_site == "TUM" ~ paste0(spwn_yr, "0930")
  )

#-----------------------------------------------------------------
# compress detections
comp_obs <-
  compress(ptagis_cth,
           configuration = configuration)

identical(n_distinct(ptagis_cth$tag_code),
          n_distinct(comp_obs$tag_code))

setdiff(unique(ptagis_cth$tag_code),
        unique(comp_obs$tag_code))

# remove detections prior to min_obs_date, and prior to root_site
comp_obs <-
  comp_obs |>
  group_by(tag_code) |>
  mutate(start_date = min(min_det[node == root_site])) |>
  filter(min_det >= start_date) |>
  ungroup() |>
  filter(min_det >= ymd(min_obs_date))

# add direction
comp_dir <-
  addDirection(comp_obs,
               parent_child = parent_child_nodes)

# determine if detections should be kept
comp_filter <-
  filterDetections(comp_obs,
                   parent_child = parent_child_nodes,
                   max_obs_date = max_obs_date)

#-----------------------------------------------------------------
# extract detection sites
sites_obs <-
  extractSites(ptagis_cth,
               as_sf = F,
               min_date = min_obs_date,
               max_date = max_obs_date,
               configuration = configuration)

sites_sf |>
  anti_join(sites_obs |>
              select(site_code),
            by = join_by(site_code))

sites_obs |>
  anti_join(sites_sf |>
              sf::st_drop_geometry() |>
              select(site_code),
            by = join_by(site_code))


#-----------------------------------------------------------------
# prep for DABOM
prepped_ch <- prepWrapper(cth_file = ptagis_cth,
                          file_type = "PTAGIS",
                          configuration = configuration,
                          parent_child = parent_child_nodes,
                          start_node = root_site,
                          min_obs_date = min_obs_date,
                          max_obs_date = max_obs_date,
                          add_tag_detects = F,
                          save_file = F)


# look at examples of "weird" capture histories
prepped_ch %>%
  filter(is.na(user_keep_obs)) %>%
  filter(direction == 'unknown') %>%
  select(tag_code) %>%
  distinct() %>%
  slice_sample(n = 1) %>%
  # slice(119) %>%
  left_join(prepped_ch) %>%
  select(-(duration:start_date),
         -user_keep_obs,
         -max_det) #%>%
  # as.data.frame()



# filter to keep only the observations you want to keep
# for this example, use PITcleanr's suggestions
filter_obs = prepped_ch %>%
  mutate(across(user_keep_obs,
                ~ if_else(is.na(.),
                        auto_keep_obs,
                        .))) %>%
  filter(user_keep_obs)


# determine origin of each fish
fish_origin = ptagis_cth %>%
  filter(tag_code %in% unique(filter_obs$tag_code)) %>%
  select(tag_code, mark_rear_type_name) %>%
  distinct() |>
  mutate(origin = if_else(str_detect(mark_rear_type_name, "Hatchery"),
                          "H",
                          "W")) |>
  select(tag_code, origin)

identical(nrow(fish_origin),
          n_distinct(filter_obs$tag_code))

#-----------------------------------------------------------------
# library(DABOM)
devtools::load_all("../DABOM")

# file path to the default and initial model
basic_mod_file = tempfile("DABOM_init", fileext = ".txt")

writeDABOM(file_name = basic_mod_file,
           parent_child = parent_child,
           configuration = configuration)


#------------------------------------------------------------------------------
# Alter default model code for species and year of interest;
# sets prior for some detection node efficiencies at 0 or 100%
# based on actual tag detection data; 0% if no tags were seen
#------------------------------------------------------------------------------

# filepath for specific JAGS model code for species and year
mod_path = tempfile("DABOM_final", fileext = ".txt")

# writes species and year specific jags code
fixNoFishNodes(init_file = basic_mod_file,
               file_name = mod_path,
               filter_ch = filter_obs,
               parent_child = parent_child,
               configuration = configuration,
               fish_origin = fish_origin)

#------------------------------------------------------------------------------
# Creates a function to spit out initial values for MCMC chains
init_fnc = setInitialValues(filter_obs,
                            parent_child,
                            configuration)

# Create all the input data for the JAGS model
jags_data = createJAGSinputs(filter_ch = filter_obs,
                             parent_child = parent_child,
                             configuration = configuration,
                             fish_origin = fish_origin)

# Tell JAGS which parameters in the model that it should save.
jags_params = setSavedParams(model_file = mod_path,
                             time_varying = F)


# run the model
jags = rjags::jags.model(mod_path,
                         data = jags_data,
                         inits = init_fnc,
                         n.chains = 1,
                         n.adapt = 5)
