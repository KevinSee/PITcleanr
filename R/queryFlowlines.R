#' @title Download NHDPlus flow lines
#'
#' @description Download a subset of the NHDPlus v2.1 flowlines that covers a list of detection sites.
#'
#' @author Kevin See
#'
#' @param sites_sf An `sf` class object containing points of all detection sites. Must contain a column named `site_code` containing the site code of each site.
#' @param root_site_code Site code for the starting detection site.
#' @param min_strm_order minimum stream order to query flowlines for. Default value is `0`.
#' @param max_upstream_comid if provided, this function will filter out any flowlines upstream of this unique identifier (COMID) in the NHDPlus stream network. Default value is `NULL`. For assistance identifying a COMID, please see the `hydrogeofetch` package, specifically the `discover_nhdplus_id()` function.
#' @param set_crs What CRS should be used? Default will read the CRS from the `sites_sf` object.
#'
#' @import dplyr sf hydrogeofetch hydroloom measurements tidyr purrr
#' @importFrom nngeo st_remove_holes
#' @export
#' @return list with at least two elements: `flowlines` is the `sf` object of flowlines downloaded from NHDPlus. `basin` is the polygon representing the catchment upstream of the root site.

queryFlowlines = function(sites_sf = NULL,
                          root_site_code = NULL,
                          min_strm_order = 0,
                          max_upstream_comid = NULL,
                          set_crs = NULL)
{

  requireNamespace("hydrogeofetch", quietly = TRUE)
  requireNamespace("hydroloom", quietly = TRUE)

  stopifnot(!is.null(sites_sf))

  # set crs
  if(is.null(set_crs)) {
    set_crs = sf::st_crs(sites_sf)
  }

  # if no root side code given, default to the first site code
  if(is.null(root_site_code)) {
    root_site_code = sites_sf$site_code[1]
  }

  # find most downstream site
  sites_comid <-
    sites_sf |>
    dplyr::rowwise() |>
    dplyr::mutate(comid = hydrogeofetch::discover_nhdplus_id(geometry)) |>
    dplyr::ungroup()

  sites_comid <-
    sites_comid |>
    left_join(
      hydrogeofetch::get_nhdplus(comid = sites_comid$comid,
                                 skip_geometry = T) |>
        select(comid,
               gnis_name,
               ftype,
               streamorde,
               totdasqkm,
               minelevraw,
               maxelevsmo,
               minelevsmo,
               uphydroseq),
      by = join_by(comid)
    )

  # find the starting point (most downstream point)
  # start_comid <-
  #   sites_comid |>
  #   dplyr::filter(site_code == root_site_code)

  start_comid <-
    sites_comid |>
    slice_max(totdasqkm)

  # query flowlines from NHDPlus layer
  message(paste("Querying streams upstream of", root_site_code, "\n"))

  # basin upstream of root site
  root_basin <-
    hydrogeofetch::get_nldi_basin(list(featureSource = "comid",
                                       featureID = sites_comid$comid[sites_comid$site_code == root_site_code])) |>
    nngeo::st_remove_holes() |>
    sf::st_transform(set_crs)

  # entire basin upstream of most downstream site
  full_basin <-
    hydrogeofetch::get_nldi_basin(list(featureSource = "comid",
                                       featureID = start_comid$comid)) |>
    nngeo::st_remove_holes() |>
    sf::st_transform(set_crs)

  # cutout basins upstream of max_upstream_comid
  if(!is.null(max_upstream_comid)) {

    message("Removing some upstream areas")

    upstrm_basin_list <- vector("list",
                                length = length(max_upstream_comid))


    for(i in seq_along(max_upstream_comid)) {
      upstrm_basin_list[[i]] <-
        hydrogeofetch::get_nldi_basin(list(featureSource = "comid",
                                           featureID = max_upstream_comid[i])) |>
        nngeo::st_remove_holes() |>
        sf::st_transform()
    }

    if(length(upstrm_basin_list) == 1) {

      upstrm_basin <- upstrm_basin_list[[1]]

    } else {

      upstrm_basin <-
        upstrm_basin_list |>
        dplyr::bind_rows() |>
        sf::st_union() |>
        nngeo::st_remove_holes() |>
        sf::st_transform(set_crs)
    }

    basin <-
      sf::st_difference(full_basin,
                        upstrm_basin |>
                          st_buffer(100))

    root_basin <-
      root_basin |>
      sf::st_difference(upstrm_basin |>
                          st_buffer(100))

  } else {
    basin <- full_basin
  }

  # flowlines upstream of root site
  up_flow <-
    hydrogeofetch::get_nhdplus(AOI = root_basin,
                               realization = "flowline",
                               streamorder = min_strm_order) |>
    dplyr::mutate(
      dplyr::across(
        c(lakefract,
          surfarea,
          rareahload,
          hwnodesqkm),
        ~ as.numeric(.)))# |>
  # hydroloom::hy() |>
  # hydroloom::add_toids() |>
  # st_set_geometry("geometry")

  # flowlines downstream of root site

  # what sites are downstream of root site?
  dwnstrm_sites_comid <-
    sites_comid |>
    dplyr::mutate(dwnstrm = purrr::map_lgl(geometry,
                                           .f = function(x) {
                                             !sf::st_covers(root_basin |>
                                                              sf::st_buffer(1000),
                                                            x,
                                                            sparse = F)
                                           })) |>
    dplyr::filter(dwnstrm |
                    site_code == root_site_code) |>
    dplyr::select(-dwnstrm)

  if(nrow(dwnstrm_sites_comid) > 1 ) {
    message(paste("Querying streams downstream of", root_site_code, "\n"))

    # maximum distance to query downstream of each point
    max_dist <-
      dwnstrm_sites_comid |>
      st_drop_geometry() |>
      mutate(rkm_total = map_dbl(rkm,
                                 .f = function(x) {
                                   str_split(x, "\\.") |>
                                     map(.f = as.numeric) |>
                                     map_dbl(.f = sum)
                                 })) |>
      summarize(across(rkm_total,
                       list(max = ~ max(., na.rm = T),
                            min = ~ min(., na.rm = T)))) |>
      mutate(across(starts_with("rkm1"),
                    as.numeric),
             dist_km = rkm_total_max - rkm_total_min) |>
      pull(dist_km)

    dwn_flow_comid <-
      dwnstrm_sites_comid |>
      st_drop_geometry() |>
      select(site_code,
             comid) |>
      mutate(down_path = map(comid,
                             .f = function(x) {
                               hydrogeofetch::navigate_nldi(list(featureSource = "comid",
                                                                 featureID = x),
                                                            mode = "DM",
                                                            distance_km = max_dist)
                             }),
             dplyr::across(down_path,
                           ~ map(., "DM_flowlines"))) |>
      tidyr::unnest(down_path) |>
      dplyr::pull(nhdplus_comid) |>
      unique()

    # drop flowlines downstream of lowest point
    drop_flow <-
      hydrogeofetch::navigate_nldi(list(featureSource = "comid",
                                        featureID = start_comid$comid),
                                   mode = "DM",
                                   distance_km = max_dist)$DM_flowlines |>
      filter_out(nhdplus_comid == start_comid$comid) |>
      st_drop_geometry()

    dwn_flow <-
      hydrogeofetch::get_nhdplus(comid = dwn_flow_comid,
                                 realization = "flowline") |>
      dplyr::mutate(
        dplyr::across(
          c(lakefract,
            surfarea,
            rareahload,
            hwnodesqkm),
          ~ as.numeric(.))) |>
      sf::st_transform(set_crs) |>
      # hydroloom::hy() |>
      # hydroloom::add_toids() |>
      filter_out(comid %in% drop_flow$nhdplus_comid) #|>
    # sf::st_set_geometry("geometry")

    flowlines <-
      up_flow |>
      filter_out(comid == sites_comid$comid[sites_comid$site_code == root_site_code]) |>
      # rbind(dwn_flow)
      bind_rows(dwn_flow) |>
      distinct()

  } else {

    flowlines <- up_flow

  }

  # a list to return
  # includes flowlines and polygon of basin
  return_list = list(flowlines = flowlines,
                     basin = root_basin)


  return(return_list)
}
