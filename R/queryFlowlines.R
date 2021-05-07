#' @title Download NHDPlus flow lines
#'
#' @description Download a subset of the NHDPlus v2.1 flowlines that covers a list of detection sites.
#'
#' @author Kevin See
#'
#' @param sites_sf An `sf` class object containing points of all detection sites. Must contain a column named `site_code` containing the site code of each site.
#' @param root_site_code Site code for the starting detection site.
#' @param min_strm_order minimum stream order to query flowlines for. Default value is `0`.
#' @param dwnstream_sites Does `sites_sf` contain sites that are downstream of `root_site_code`? If `TRUE`, then all flowlines within a boundary box of `sites_df` will be downloaded
#' @param dwn_min_stream_order_diff Used to control the minimum stream order of downloaded downstream flowlines. This can be useful if the user wants to prevent downloading excessive smaller streams downstream of a tagging or release location. It corresponds to the difference between the stream order of the `root_site_code` site and the minimum stream order desired in the downstream flowlines. For example, if the `root_site_code` is located on a stream order 6 and `dwn_min_stream_order_diff = 2`, then only flowlines of at least 6-2=**4** will be downloaded. The default value is 0, meaning only streams of equal or greater order than the tagging or release site will be returned.
#'
#' @import dplyr sf nhdplusTools
#' @importFrom magrittr %<>%
#' @importFrom nngeo st_remove_holes
#' @export
#' @return sf

queryFlowlines = function(sites_sf = NULL,
                          root_site_code = NULL,
                          min_strm_order = 0,
                          dwnstrm_sites = FALSE,
                          dwn_min_stream_order_diff = NULL) {

  requireNamespace("nhdplusTools", quietly = TRUE)

  stopifnot(!is.null(sites_sf))

  # if no root side code given, default to the first site code
  if(is.null(root_site_code)) {
    root_site_code = sites_sf$site_code[1]
  }

  # set minimum stream order for downstream flowlines
  if(is.null(dwn_min_stream_order_diff)) dwn_min_stream_order_diff = 0

  # find the starting point (most downstream point)
  start_comid = sites_sf %>%
    dplyr::filter(site_code == root_site_code) %>%
    sf::st_transform(crs = 4326) %>%
    nhdplusTools::discover_nhdplus_id()

  # query flowlines from NHDPlus layer
  cat(paste("Querying streams upstream of", root_site_code, "\n"))
  nhd_lst = nhdplusTools::plot_nhdplus(outlets = list(start_comid),
                                       streamorder = min_strm_order,
                                       actually_plot = F)

  flowlines = nhd_lst$flowline %>%
    sf::st_zm() %>%
    sf::st_transform(crs = sf::st_crs(sites_sf))

  # a list to return
  # includes flowlines and polygon of basin
  return_list = list(flowlines = flowlines,
                         basin = nngeo::st_remove_holes(nhd_lst$basin) %>%
                       sf::st_zm() %>%
                       sf::st_transform(crs = sf::st_crs(sites_sf)))

  if(dwnstrm_sites) {
    cat("Calculating furthest mainstem point downstream \n")
    # get flowlines for downstream sites based on bounding box of all sites
      nhd_lst_tmp = sites_sf %>%
        sf::st_bbox() %>%
        nhdplusTools::plot_nhdplus(bbox = .,
                                   streamorder = max(min_strm_order, (max(flowlines$StreamOrde) - dwn_min_stream_order_diff)),
                                   actually_plot = F)

      # pull out the comid for the largest stream order and furthest downstream segment
      dwn_comid = nhd_lst_tmp$flowline %>%
        filter(StreamOrde == max(StreamOrde)) %>%
        filter(Hydroseq == min(Hydroseq)) %>%
        pull(COMID)

      cat(paste("Querying streams downstream of", root_site_code, "\n"))
      nhd_lst_dwn = nhdplusTools::plot_nhdplus(outlets = list(dwn_comid),
                                               # streamorder = max(flowlines$StreamOrde),
                                               streamorder = max(min_strm_order, (max(flowlines$StreamOrde) - dwn_min_stream_order_diff)),
                                               actually_plot = F)

    # dwn_flowlines = nhd_lst_dwn$flowline %>%
    #   dplyr::filter(!COMID %in% flowlines$COMID) %>%
    #   sf::st_zm() %>%
    #   sf::st_transform(crs = sf::st_crs(sites_sf))

      dwn_site_comids = nhd_lst_dwn$flowline %>%
        sf::st_zm() %>%
        sf::st_transform(crs = sf::st_crs(sites_sf)) %>%
        select(COMID, Hydroseq) %>%
        st_join(sites_sf,
                .,
                join = st_nearest_feature) %>%
        filter(! COMID %in% flowlines$COMID) %>%
        as_tibble() %>%
        pull(COMID)

      dwn_comids = c(start_comid,
                     dwn_site_comids) %>%
        as.list() %>%
        map_df(.f = function(x) {
          tibble(comid = nhdplusTools::get_DM(network = nhd_lst_dwn$flowline,
                                              comid = x))
        }) %>%
        distinct() %>%
        pull(comid)


#       dwn_comids = get_DM(nhd_lst_dwn$flowline,
#                           start_comid)

      dwn_flowlines = nhd_lst_dwn$flowline %>%
        dplyr::filter(COMID %in% dwn_comids) %>%
        sf::st_zm() %>%
        sf::st_transform(crs = sf::st_crs(sites_sf))

    return_list$dwn_flowlines = dwn_flowlines
  }

  # return(flowlines)
  return(return_list)
}
