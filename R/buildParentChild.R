#' @title Build a Parent-Child table
#'
#' @description Build a parent-child table based on site locations and the flowlines from NHDPlusV2 layer.
#'
#' @author Kevin See
#'
#'
#' @param sites_sf an `sf` object containing the `site_code` and location of all detection sites.
#' @param flowlines output from `queryFlowlines()` function.
#' @param rm_na_parent should rows with NA as the parent be automatically removed? Default is `FALSE`.
#' @param add_rkm should the RKM of the parent and child be added, based on PTAGIS metadata?
#' Default is `FALSE`.
#'
#'
#' @import dplyr lubridate
#' @importFrom stringr str_replace
#' @importFrom magrittr %<>%
#' @export
#' @return NULL
#' @examples createParentChildDf()


buildParentChild = function(sites_sf = NULL,
                            flowlines = NULL,
                            rm_na_parent = F,
                            add_rkm = F) {

  if(!identical(st_crs(sites_sf),
                st_crs(flowlines))) {
    sites_sf %<>%
      st_transform(st_crs(flowlines))
  }

  # find the closest hydrosegment on the flowline to each site
  sites_NHDseg = st_join(sites_sf,
                         flowlines %>%
                           select(gnis_name, Hydroseq),
                         join = st_nearest_feature)

  parent_child = sites_NHDseg$site_code %>%
    as.list() %>%
    rlang::set_names() %>%
    map_df(.id = "site_code",
           .f = function(x) {
             dwn_site = try(findDwnstrmSite(x,
                                            flow_lines = flowlines,
                                            sites_joined = sites_NHDseg))
             if(class(dwn_site) == "try-error") {
               dwn_site = NA_character_
             }
             tibble(downSite = dwn_site) %>%
               return()
           }) %>%
    select(parent = downSite,
           child = site_code) %>%
    left_join(sites_NHDseg %>%
                st_drop_geometry() %>%
                select(parent = site_code,
                       parent_hydro = Hydroseq),
              by = "parent") %>%
    left_join(sites_NHDseg %>%
                st_drop_geometry() %>%
                select(child = site_code,
                       child_hydro = Hydroseq),
              by = "child") %>%
    arrange(parent_hydro,
            child_hydro)

  if(rm_na_parent & sum(is.na(parent_child$parent)) > 0) {
    cat(paste0("These child locations: ", paste(parent_child$child[is.na(parent_child$parent)], collapse = ", "),
               ",\n had no parent location and were removed from the table.\n"))
    parent_child %<>%
      filter(!is.na(parent))
  }

  if(add_rkm) {

    cat("\n")
    # query PTAGIS for RKMs
    all_meta = queryPtagisMeta()

    parent_child %<>%
      left_join(all_meta %>%
                  select(parent = site_code,
                         parent_rkm = rkm) %>%
                  distinct(),
                by = "parent") %>%
      left_join(all_meta %>%
                  select(child = site_code,
                         child_rkm = rkm) %>%
                  distinct(),
                by = "child")
  }

  if(sum(duplicated(parent_child$child)) > 0) {
    dup_childs = parent_child %>%
      filter(child %in% child[duplicated(child)]) %>%
      pull(child) %>%
      unique() %>%
      paste(collapse = ", ")

      cat(paste("The following site codes appear more than once in the child columm:\n", dup_childs, "\n"))
  }

  return(parent_child)

}
