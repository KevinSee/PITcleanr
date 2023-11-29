# Author: Kevin See
# Purpose: Test processing PTAGIS data to develop a CJS model
# Created: 9/28/2023
# Last Modified: 9/28/2023
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(sf)
library(here)
library(janitor)
# library(PITcleanr)
devtools::load_all()

#-----------------------------------------------------------------
# read in mark-recapture observations from LEMTRP
trap_tags <-
  read_csv(here("inst/extdata/LEMTRP",
                "LEMTRP Mark_Recapture 2021-22.csv"),
           show_col_types = F) |>
  clean_names() |>
  mutate(across(contains("_date_mm"),
                mdy),
         across(contains("date_time"),
                mdy_hms)) |>
  mutate(spawn_year = year(event_date_mmddyyyy))


# pull out MRR data about all steelhead tags
bio_df <-
  trap_tags |>
  select(spawn_year,
         event_file_name) |>
  distinct() |>
  arrange(spawn_year,
          event_file_name) |>
  # slice(1:6) |>
  mutate(tag_file_query = map(event_file_name,
                              .f = function(x) {
                                out <-
                                  tryCatch(
                                    withCallingHandlers(
                                      {
                                        error_text = "No error"
                                        list(value = queryMRRDataFile(x),
                                             error_text = error_text)
                                      },
                                      warning =
                                        function(cond) {
                                          message(paste("Warning with file", x))
                                          # message("Warning message:")
                                          # message(cond)
                                          error_text <<- paste("Warning message:", cond)
                                          # return(NULL)
                                          invokeRestart("muffleWarning")
                                        }
                                    ),
                                    error =
                                      function(cond) {
                                        message(paste("Error with file", x))
                                        message("Error message:")
                                        message(cond)
                                        # return(NULL)
                                        return(list(value = NULL,
                                                    error_text = cond))
                                      },
                                    finally = {}
                                  )
                                return(out)
                              })) |>
  mutate(err_msg = map_chr(tag_file_query,
                           .f = "error_text"),
         tag_file = map(tag_file_query,
                        .f = "value"),
         across(tag_file,
                ~ map(., .f = function(out) {
                  if("spawn_year" %in% names(out)) {
                    out <- out |>
                      select(-spawn_year)
                  }
                  return(out)
                }))) |>
  select(-c(tag_file_query,
            err_msg)) |>
  unnest(tag_file) |>
  distinct() |>
  # pull out only spring Chinook
  filter(str_detect(species_run_rear_type, "^1"),
         # delete fish with no PIT tags
         str_detect(pit_tag, "\\.\\.\\.", negate = T),
         # filter out mortalities
         event_type != "Recovery") |>
  arrange(spawn_year,
          event_date)


# pull out tag codes to use in querying PTAGIS
tag_codes <-
  bio_df |>
  select(pit_tag) |>
  # remove a couple of orphan tags
  filter(!pit_tag %in% c('3DD.003D7FE25E',
                         '3DD.003DE66236')) |>
  write_delim(file = here('inst/extdata/LEMTRP',
                          "LEMTRP_tag_list.txt"),
              delim = '\n',
              col_names = F)

#-----------------------------------------------------------------
# build configuration file
org_config <- buildConfig(node_assign = "site")

# org_config <- system.file("extdata",
#                           "LGR_configuration.csv",
#                           package = "PITcleanr",
#                           mustWork = TRUE) |>
#   read_csv(show_col_types = F)

config <-
  org_config |>
  mutate(across(node,
                ~ if_else(as.numeric(str_sub(rkm, 1, 3)) <= 234,
                          "B2J",
                          .)),
         across(node,
                ~ if_else(site_code == "GRS",
                          "GRJ",
                          .))) |>
  filter(!is.na(node))

# read in PTAGIS detections
ptagis_file = here('inst/extdata/LEMTRP',
                   "LEMTRP_chnk_cth_2021.csv")


ptagis_obs <- readCTH(ptagis_file) |>
  arrange(tag_code,
          event_date_time_value)

qc_lst <-
  qcTagHistory(ptagis_obs)
qc_lst


sites_sf <-
  ptagis_obs |>
  filter(!event_site_code_value == "ORPHAN") |>
  # filter(tag_code %in% pluck(qc_lst, "orphan_tags"),
  #        tag_code %in% pluck(qc_lst, "disown_tags"))
  extractSites(as_sf = T,
               configuration = config,
               max_date = "20220630") |>
  arrange(rkm)


sites_sf |>
  # filter(nchar(rkm) <= 7 |
  #          str_detect(rkm, "522.303.416")) |>
  st_drop_geometry() |>
  select(site_code:rkm,
         node_site) |>
  as.data.frame()


nhd_list = sites_sf |>
  filter(nchar(rkm) <= 7 |
           str_sub(rkm, 1, 11) == "522.303.416") |>
  queryFlowlines(root_site_code = "ICH",
                 min_strm_order = 2,
                 dwnstrm_sites = T,
                 dwn_min_stream_order_diff = 4)

# compile the upstream and downstream flowlines
flowlines = nhd_list$flowlines |>
  rbind(nhd_list$dwn_flowlines)

# upstream extent of study area (cut off areas further upstream)
upstrm_loc = "Hells Canyon Dam"

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


bndry <- st_difference(nhd_list$basin,
                       sfheaders::sf_remove_holes(nhd_upstrm_lst$basin) |>
                         st_transform(st_crs(nhd_list$basin)) |>
                         st_buffer(dist = 100)) |>
  sfheaders::sf_remove_holes()


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
  geom_sf(data = bndry,
          fill = NA,
          lwd = 2) +
  geom_sf(data = sites_sf,
          size = 3,
          color = "black") +
  ggrepel::geom_label_repel(
    data = sites_sf,
    aes(label = site_code,
        geometry = geometry),
    size = 1.5,
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = 100) +
  theme_bw() +
  theme(axis.title = element_blank())

#-----------------------------------------------------------------
parent_child = sites_sf |>
  filter(!site_code %in% c("SALTRP",
                           "NFSTRP",
                           "NFS",
                           "HYC",
                           "HYDTRP",
                           "HAYDNC",
                           "LLS",
                           "LLSPRC",
                           "BTL",
                           "BTIMBC",
                           "BHC",
                           "KEN",
                           "LRW")) |>
  buildParentChild(flowlines,
                   rm_na_parent = T,
                   add_rkm = T)

plotNodes(parent_child)

# pc_nodes <-
#   addParentChildNodes(parent_child,
#                       config)

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

pc <-
  parent_child |>
  left_join(buildNodeOrder(parent_child) |>
              select(parent = node,
                     node_order)) |>
  arrange(node_order)

#-----------------------------------------------------------------
# compress detections
comp_obs <-
  compress(ptagis_obs,
           configuration = config,
           units = "days")


# determine if detections should be kept
# comp_filter <-
#   filterDetections(comp_obs,
#                    parent_child = parent_child)

prepped_ch <-
  prepWrapper(comp_obs,
              configuration = config,
              parent_child = parent_child,
              start_node = "LEMTRP",
              add_tag_detects = T,
              save_file = F)


tabyl(prepped_ch,
      direction)


prepped_ch |>
  filter(is.na(user_keep_obs)) |>
  select(tag_code) |>
  distinct()

prepped_ch |>
  filter(is.na(user_keep_obs)) |>
  select(tag_code) |>
  distinct() |>
  # sample_n(1) |>
  left_join(prepped_ch) |>
  select(tag_code,
         node,
         min_det,
         direction,
         ends_with("_obs")) |>
  group_by(tag_code) |>
  group_split()


#-----------------------------------------------------------------
# how long does it take fish to get to Lower Granite, by life stage?
ls_df <-
  prepped_ch |>
  filter(auto_keep_obs) |>
  mutate(life_stage = if_else(year(start_date) == 2021,
                              "parr",
                              "smolt"),
         across(life_stage,
                ~ factor(.,
                         levels = c("parr",
                                    "smolt"))))

# starting at LEMTRP
ls_df |>
  filter(node == "GRJ") |>
  mutate(grj_time = difftime(max_det, start_date, units = "days")) |>
  group_by(life_stage) |>
  summarize(n_tags = n_distinct(tag_code),
            across(grj_time,
                   list(mean = ~mean(.),
                        median = median,
                        sd = ~sd(.)),
                   .names = "grj_{.fn}"))


# start at LLR
ls_df |>
  filter(node == "LLR") |>
  select(tag_code) |>
  distinct() |>
  inner_join(ls_df |>
              filter(node == "GRJ") |>
              select(tag_code) |>
              distinct()) |>
  left_join(ls_df) |>
  group_by(tag_code,
           life_stage) |>
  summarize(grj_time = difftime(max_det[node == "GRJ"],
                                min_det[node == "LLR"],
                                units = "days"),
            .groups = "drop") |>
  group_by(life_stage) |>
  summarize(n_tags = n_distinct(tag_code),
            across(grj_time,
                   list(mean = ~mean(.),
                        median = median,
                        sd = ~sd(.)),
                   .names = "grj_{.fn}"),
            .groups = "drop")


# LEMTRP to LLR / LLRTP
ls_df |>
  filter(node %in% c("LLRTP", "LLR")) |>
  group_by(tag_code) |>
  filter(min_det == min(min_det)) |>
  ungroup() |>
  mutate(llr_time = difftime(max_det, start_date, units = "days")) |>
  group_by(life_stage) |>
  summarize(n_tags = n_distinct(tag_code),
            across(llr_time,
                   list(mean = ~mean(.),
                        median = median,
                        sd = ~sd(.)),
                   .names = "llr_{.fn}"))

ls_df |>
  filter(node %in% c("LLRTP", "LLR")) |>
  group_by(tag_code) |>
  filter(min_det == min(min_det)) |>
  ungroup() |>
  mutate(llr_time = difftime(max_det, start_date, units = "days")) |>
  ggplot(aes(x = llr_time,
             color = life_stage,
             fill = life_stage)) +
  geom_histogram(position = "dodge") +
  # geom_density(alpha = 0.3) +
  theme_bw() +
  labs(x = "Days between LEMTRP and LLR",
       y = "Number of Tags",
       fill = "Life Stage",
       color = "Life Stage")

#-----------------------------------------------------------------
# turn into capture histories
# set the levels of nodes based somewhat on the movement paths
node_levels <- buildNodeOrder(parent_child) |>
  arrange(path) |>
  pull(node)


cap_hist_df <-
  prepped_ch |>
  filter(auto_keep_obs) |>
  mutate(life_stage = if_else(year(start_date) == 2021,
                              "parr",
                              "smolt"),
         across(life_stage,
                ~ factor(.,
                         levels = c("parr",
                                    "smolt")))) |>
  select(tag_code, life_stage, node) |>
  distinct() |>
  mutate(across(node,
                ~ factor(.,
                         levels = node_levels))) |>
  mutate(detected = 1) |>
  pivot_wider(names_from = node,
              names_sort = TRUE,
              names_expand = TRUE,
              values_from = detected,
              values_fill = 0) |>
  unite(col = "ch",
        -c(tag_code,
           life_stage),
        sep = "",
        remove = F)

cap_hist_df |>
  tabyl(ch) |>
  adorn_pct_formatting() |>
  arrange(desc(n))

ch_df <-
  prepped_ch |>
  filter(auto_keep_obs) |>
  DABOM::createDABOMcapHist(parent_child = pc,
                            configuration = config) |>
  select(tag_code,
         start_date,
         all_of(node_levels)) |>
  mutate(life_stage = if_else(year(start_date) == 2021,
                              "parr",
                              "smolt"),
         across(life_stage,
                ~ factor(.,
                         levels = c("parr",
                                    "smolt")))) |>
  relocate(life_stage,
           .after = tag_code) |>
  unite(col = "ch",
        -c(tag_code,
           life_stage,
           start_date),
        sep = "",
        remove = F) |>
  select(-start_date)


#-----------------------------------------------------------------
# fit a CJS model
library(marked)

cjs_data <-
  cap_hist_df |>
  select(tag_code:ch)

#--------------------
# fit one model
cjs_proc <-
  cjs_data |>
  as.data.frame() |>
  process.data(model = "CJS")

cjs_ddl <-
  make.design.data(cjs_proc)

# set model construction
Phi.time <- list(formula = ~ time)
p.time <- list(formula = ~ time)



#--------------------
# fit a few options
cjs_proc <-
  cjs_data |>
  as.data.frame() |>
  process.data(model = "CJS",
               groups = c("life_stage"))

cjs_ddl <-
  make.design.data(cjs_proc)

# set model construction
Phi.time <- list(formula = ~ time)
p.time <- list(formula = ~ time)

Phi.time.grp <- list(formula = ~ time * life_stage)
p.time.grp <- list(formula = ~ time * life_stage)

cml <- create.model.list(c("Phi","p"))

all_mods <-
  crm.wrapper(cml,
              data = cjs_proc,
              ddl = cjs_ddl,
              external = F,
              accumulate = F,
              hessian = T)

all_mods

# pull out best model
best_mod <-
  pluck(all_mods,
        all_mods$model.table |>
          rownames() |>
          pluck(1) |>
          as.numeric()
  )

est_preds <-
  predict(best_mod) |>
  map(.f = as_tibble)

est_preds$Phi <-
  est_preds$Phi |>
  bind_cols(pc |>
              select(parent, child) |>
              unite(col = "surv",
                    parent,
                    child,
                    sep = "_"))


est_preds$p <-
  est_preds$p |>
  bind_cols(tibble(det_site = rep(pc$child, 2))) |>
  pivot_wider(names_from = life_stage,
              names_sort = T,
              values_from = c(estimate,
                              se,
                              lcl,
                              ucl),
              names_glue = "{life_stage}_{.value}",
              names_vary = "fastest")

est_preds |>
  map(.f = function(x) {
    x |>
      mutate(across(where(is.double),
                    ~ round(., 3)))
  })


# # fit CJS model
# cjs_mod <-
#   crm(cjs_proc,
#       cjs_ddl,
#       model.parameters = list(Phi = Phi.time,
#                               p = p.time))


# pull out predictions of survival and detection
est_preds <-
  predict(cjs_mod)
  # predict(cjs_mod2)

est_preds$Phi <-
  est_preds$Phi |>
  as_tibble() |>
  bind_cols(pc |>
              select(parent, child) |>
              unite(col = "surv",
                    parent,
                    child,
                    sep = "_"))
est_preds$p <-
  est_preds$p |>
  as_tibble() |>
  bind_cols(pc |>
              select(det = child))

est_preds

est_preds <-
  predict(cjs_mod2) |>
  # map(.f = as_tibble) |>
  map(.f = function(x) {
    x |>
      pivot_wider(names_from = life_stage,
                  names_sort = T,
                  values_from = estimate)
  })

# cumulative survival to Bonneville Dam
bon_surv <-
  est_preds$Phi |>
  pull(estimate) |>
  prod()

bon_surv

# survival to Lower Granite Dam
grj_surv <-
  est_preds$Phi |>
  filter(occ <= 6) |>
  pull(estimate) |>
  prod()

grj_surv
