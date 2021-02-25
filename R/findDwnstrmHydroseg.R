#' @title Find Hydro Segment of Downstream Site
#'
#' @description
#'
#' @author Kevin See
#'
#' @param hydro_id a character or numeric input containing the starting hydro sequence
#' @param flow_lines An `sf` object containing the flowlines (output of `queryFlowlines()`). Must have a column name called Hydroseq with unique identifiers, and a column name called DnHydroseq containing the hydro sequence of the next downstream segment
#' @param hydro_pause_ids a vector of hydro sequences containing possible end points
#'
#' @import dplyr sf
#' @export
#' @return character of the nearest downstream HY segment to starting segment that falls within a list of possible stopping segments.


findDwnstrmHydroseg = function(hydro_id,
                               flow_lines,
                               hydro_pause_ids) {

  if(!hydro_id %in% flow_lines$Hydroseq) {
    stop(paste("HydroSeq", hydro_id, "not found within flow lines.\n"))
  }

  hydro_dn = flow_lines %>%
    dplyr::filter(Hydroseq == hydro_id) %>%
    dplyr::pull(DnHydroseq) %>%
    unique()
  if(hydro_dn %in% hydro_pause_ids) {
    return(hydro_dn)
  } else {
    return(findDwnstrmHydroseg(hydro_dn, flow_lines, hydro_pause_ids))
  }
}
