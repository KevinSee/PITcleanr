#' @title Download NHDPlus flow lines
#'
#' @description Download a subset of the NHDPlus v2.1 flowlines that covers a list of detection sites.
#'
#' @author Kevin See
#'
#' @param sites_sf An `sf` class object containing points of all detection sites. Must contain a column named SiteID containing the site code of each site.
#' @param root_site_code Site code for the starting detection site.
#'
#' @import dplyr sf nhdplusTools
#' @importFrom magrittr %<>%
#' @export
#' @return sf

queryFlowlines = function(sites_sf = NULL,
                          root_site_code = NULL,
                          dwnstrm_sites = FALSE,
                          dwn_min_stream_order_diff = NULL) {

  stopifnot(!is.null(sites_sf))

  # if no root side code given, default to the first site code
  if(is.null(root_site_code)) {
    root_site_code = sites_sf$SiteID[1]
  }

  # find the starting point (most downstream point)
  start_comid = sites_sf %>%
    dplyr::filter(SiteID == root_site_code) %>%
    nhdplusTools::discover_nhdplus_id()

  # query flowlines from NHDPlus layer
  nhd_lst = nhdplusTools::plot_nhdplus(outlets = list(start_comid),
                                       actually_plot = F)

  flowlines = nhd_lst$flowline %>%
    sf::st_zm() %>%
    sf::st_transform(crs = sf::st_crs(sites_sf))

  if(dwnstrm_sites) {
    # get flowlines for downstream sites based on bounding box of all sites
    nhd_lst_dwn = sites_sf %>%
      sf::st_bbox() %>%
      nhdplusTools::plot_nhdplus(bbox = .,
                                 actually_plot = F)

    dwn_flowlines = nhd_lst_dwn$flowline %>%
      dplyr::filter(!COMID %in% flowlines$COMID) %>%
      sf::st_zm() %>%
      sf::st_transform(crs = sf::st_crs(sites_sf))

    if(!is.null(dwn_min_stream_order_diff)) {
      dwn_flowlines %<>%
        dplyr::filter(StreamOrde >= (max(flowlines$StreamOrde) - dwn_min_stream_order_diff))
    }

    flowlines %<>%
      rbind(dwn_flowlines)
  }

  return(flowlines)
}
