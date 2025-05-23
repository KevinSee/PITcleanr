#' @title Determine next downstream site
#'
#' @description Determine which detection site is the closest downstream site from a starting site, based on hydrographic flowlines
#'
#' @author Kevin See
#'
#' @param site_id Site code for the starting detection site.
#' @param sites_joined An `sf` class object containing points of all detection sites. Must contain a column named `site_code` containing the site code of each site, and a column called hydroseq containing the hydro sequence closest to each site.
#' @inheritParams findDwnstrmHydroseg
#'
#' @import dplyr
#' @export
#' @return character of the site code downstream

findDwnstrmSite = function(site_id = NULL,
                           flow_lines = NULL,
                           sites_joined = NULL,
                           quiet = TRUE) {

  stopifnot(!is.null(site_id),
            !is.null(flow_lines),
            !is.null(sites_joined))

  init_hydseq = sites_joined %>%
    dplyr::filter(site_code == site_id) %>%
    dplyr::pull(hydroseq)

  init_sites = sites_joined %>%
    dplyr::filter(hydroseq == init_hydseq) %>%
    dplyr::pull(site_code)

  if(length(init_sites) > 1) {
    warning(paste("Sites", paste(init_sites, collapse = " and "), "have the same hydro sequence.\n"))
  }


  dwn_hydseq = PITcleanr::findDwnstrmHydroseg(init_hydseq,
                                              flow_lines,
                                              sites_joined$hydroseq)

  if(is.na(dwn_hydseq)) {
    warning(paste("No downstream hydro sequence was found for site", site_id))

    return(NA_character_)

  } else {

    sites_joined %>%
      dplyr::filter(hydroseq == dwn_hydseq) %>%
      dplyr::pull(site_code) %>%
      return()

  }
}
