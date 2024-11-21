#' @title Download NHDPlus flow lines
#'
#' @description Download a subset of the NHDPlus v2.1 flowlines that covers a list of detection sites.
#'
#' @author Kevin See
#'
#' @param sites_sf An `sf` class object containing points of all detection sites. Must contain a column named `site_code` containing the site code of each site.
#' @param root_site_code Site code for the starting detection site.
#' @param min_strm_order minimum stream order to query flowlines for. Default value is `0`.
#' @param max_upstream_comid if provided, this function will filter out any flowlines upstream of this unique identifier (COMID) in the NHDPlus stream network. Default value is `NULL`. For assistance identifying a COMID, please see the `nhdplusTools` package, specifically the `discover_nhdplus_id()` function.
#' @param combine_up_down Should the flowlines upstream and downstream of the starting point (`root_site_code`) be combined into a single object, or be returned as separate elements? The default value is `TRUE`
#'
#' @import dplyr sf nhdplusTools measurements tidyr purrr
#' @importFrom nngeo st_remove_holes
#' @export
#' @return list with at least two elements: `flowlines` is the `sf` object of flowlines downloaded from NHDPlus. `basin` is the polygon representing the catchment upstream of the root site. If `combine_up_down` is `FALSE`, there is a third element in the list, called `dwn_flowlines`, containing an `sf` object of flowlines downstream of the root site.

queryFlowlines = function(sites_sf = NULL,
                          root_site_code = NULL,
                          min_strm_order = 0,
                          max_upstream_comid = NULL,
                          combine_up_down = TRUE)
{

  requireNamespace("nhdplusTools", quietly = TRUE)

  stopifnot(!is.null(sites_sf))

  # if no root side code given, default to the first site code
  if(is.null(root_site_code)) {
    root_site_code = sites_sf$site_code[1]
  }

  # find the starting point (most downstream point)
  start_comid = sites_sf %>%
    dplyr::filter(site_code == root_site_code) %>%
    nhdplusTools::discover_nhdplus_id()

  start_pt <-
    list(featureSource = "comid",
         featureID = start_comid)

  # query flowlines from NHDPlus layer
  message(paste("Querying streams upstream of", root_site_code, "\n"))

  basin <-
    nhdplusTools::get_nldi_basin(start_pt) |>
    nngeo::st_remove_holes() |>
    sf::st_transform(sf::st_crs(sites_sf))

  if(!is.null(max_upstream_comid)) {

    message("Removing some upstream areas")

    upstrm_basin <-
      nhdplusTools::get_nldi_basin(list(featureSource = "comid",
                                        featureID = upstrm_comid)) |>
      nngeo::st_remove_holes() |>
      sf::st_transform(sf::st_crs(sites_sf))

    new_basin <-
      sf::st_difference(basin,
                        upstrm_basin |>
                          st_buffer(1000))
    basin <-
      new_basin

    rm(new_basin)
  }

  flowlines <-
    nhdplusTools::get_nhdplus(AOI = basin,
                              realization = "flowline",
                              streamorder = min_strm_order) |>
    dplyr::mutate(
      dplyr::across(
        c(lakefract,
          surfarea,
          rareahload,
          hwnodesqkm),
        ~ as.numeric(.)))


  # a list to return
  # includes flowlines and polygon of basin
  return_list = list(flowlines = flowlines,
                     basin = basin)

  # pull out sites downstream of root site
  dwnstrm_sites_comid <-
    sites_sf |>
    dplyr::mutate(dwnstrm = purrr::map_lgl(geometry,
                                           .f = function(x) {
                                             !sf::st_covers(basin,
                                                            x,
                                                            sparse = F)
                                           })) |>
    dplyr::filter(dwnstrm |
                    site_code == root_site_code) |>
    dplyr::select(-dwnstrm)

  if(nrow(dwnstrm_sites_comid) > 1 ) {
    message("Calculating furthest mainstem point downstream \n")

    # find COMID for all downstream sites
    dwnstrm_sites_comid <-
      dwnstrm_sites_comid |>
      dplyr::rowwise() |>
      dplyr::mutate(comid = nhdplusTools::discover_nhdplus_id(geometry)) |>
      dplyr::ungroup()

    # how far away are downstream sites from the root site?
    dwnstrm_dist <-
      sites_sf |>
      dplyr::filter(site_code == root_site_code) |>
      sf::st_distance(dwnstrm_sites_comid)

    # maximum distance
    dwnstrm_max <-
      dwnstrm_dist[which.max(dwnstrm_dist)] |>
      measurements::conv_unit(from = "m", to = "km")

    message(paste("Querying streams downstream of", root_site_code, "\n"))

    # query downstream mainstem starting from each downstream site
    dwnstrm_main_fl <-
      dwnstrm_sites_comid |>
      tidyr::nest(data = -site_code) |>
      dplyr::mutate(network = purrr::map2(data,
                                          site_code,
                                          .f = function(x,y) {
                                            nhdplusTools::navigate_network(x,
                                                                           mode = "DM",
                                                                           output = "flowlines",
                                                                           distance_km = dwnstrm_max) |>
                                              suppressMessages() |>
                                              sf::st_transform(st_crs(sites_sf)) |>
                                              # dplyr::mutate(site_code = y) |>
                                              # dplyr::select(site_code,
                                              #               comid,
                                              #               streamorde,
                                              #               totdasqkm,
                                              #               contains("hydroseq"),
                                              #               geometry)
                                              dplyr::mutate(
                                                dplyr::across(
                                                  c(lakefract,
                                                    surfarea,
                                                    rareahload,
                                                    hwnodesqkm),
                                                  ~ as.numeric(.))) |>
                                              tibble::add_column(site_code = y,
                                                                 .before = 0)
                                          })) |>
      dplyr::pull(network) |>
      dplyr::bind_rows()

    # pull out the comid for the largest stream order and furthest downstream segment
    max_dwnstrm_comid <-
      dwnstrm_main_fl |>
      st_drop_geometry() |>
      group_by(site_code) |>
      filter(streamorde == max(streamorde)) |>
      filter(totdasqkm == min(totdasqkm)) |>
      ungroup() |>
      filter(totdasqkm == max(totdasqkm)) |>
      pull(comid) |>
      unique()

    rm_comids <-
      nhdplusTools::navigate_network(list(featureSource = "comid",
                                          featureID = max_dwnstrm_comid),
                                     mode = "DM",
                                     output = "flowlines",
                                     distance_km = dwnstrm_max) |>
      suppressMessages() |>
      st_drop_geometry() |>
      filter(comid != max_dwnstrm_comid) |>
      pull(comid)


    dwnstrm_main_fl <-
      dwnstrm_main_fl |>
      filter(!comid %in% rm_comids)

    connected <-
      if_else(dwnstrm_main_fl |>
                filter(!dnhydroseq %in% unique(hydroseq),
                       !comid %in% dwnstrm_sites_comid$comid) |>
                nrow() == 0,
              T, F)

    if(connected) {
      dwnstrm_flowline <-
        dwnstrm_main_fl |>
        select(-site_code) |>
        distinct()
    } else {

      mainstem_lngth <-
        dwnstrm_main_fl |>
        select(-site_code) |>
        distinct() |>
        pull(lengthkm) |>
        sum()

      main_flowline <-
        nhdplusTools::navigate_network(list(featureSource = "comid",
                                            featureID = max_dwnstrm_comid),
                                       mode = "UM",
                                       output = "flowlines",
                                       distance_km = mainstem_lngth) |>
        suppressMessages() |>
        st_transform(st_crs(sites_sf))

      min_dwnstrm_comid <-
        dwnstrm_main_fl |>
        st_drop_geometry() |>
        group_by(site_code) |>
        filter(streamorde == max(streamorde)) |>
        filter(totdasqkm == min(totdasqkm)) |>
        ungroup() |>
        filter(totdasqkm == min(totdasqkm)) |>
        pull(comid) |>
        unique()

      rm_upstrm_comids <-
        nhdplusTools::navigate_network(list(featureSource = "comid",
                                            featureID = min_dwnstrm_comid),
                                       mode = "UM",
                                       output = "flowlines",
                                       distance_km = mainstem_lngth) |>
        suppressMessages() |>
        st_drop_geometry() |>
        filter(comid != min_dwnstrm_comid) |>
        pull(comid) |>
        unique()

      main_flowline <-
        main_flowline |>
        filter(!comid %in% rm_upstrm_comids)


      dwnstrm_flowline <-
        dwnstrm_main_fl |>
        select(-site_code) |>
        distinct() |>
        bind_rows(main_flowline |>
                    dplyr::mutate(
                      dplyr::across(
                        c(lakefract,
                          surfarea,
                          rareahload,
                          hwnodesqkm),
                        ~ as.numeric(.)))) |>
        distinct()

    }

    # should the flowlines upstream and downstream of the root site be combined?
    if(combine_up_down) {
      return_list$flowlines <-
        return_list$flowlines |>
        dplyr::bind_rows(dwnstrm_flowline) |>
        distinct()
    } else {
      return_list$dwn_flowlines = dwnstrm_flowline
    }
  }

  return(return_list)
}
