#' @title Node Efficiency
#'
#' @description This function uses detections at a given node, and all detections upstream of that node to calculate detection efficiency at that node. It then uses the number of tags detected at that node and the detection efficiency to estimate the number of tags that have migrated upstream of that node.
#'
#' @param capHist_proc Dataframe returned by \code{processCapHist_*}, under the name \code{ProcCapHist}.
#'
#' @param node_order Dataframe returned by \code{processCapHist_*}, under the name \code{NodeOrder}.
#'
#' @param node character string of the node of interest.
#'
#' @author Kevin See
#' @import dplyr
#' @export
#' @return NULL
#' @examples estNodeEff()

estNodeEff = function(capHist_proc = NULL,
                      node_order = NULL,
                      node = NULL) {

  stopifnot(!is.null(capHist_proc),
            !is.null(node_order))

  if(is.null(node)) stop('Node must be supplied.')

  # get a vector of nodes upstream of
  node_vec = node_order %>%
    filter(grepl(node, Path)) %>%
    select(Node) %>%
    as.matrix() %>%
    as.vector()

  # calculate node efficiency and estimate tags above that node
  node_eff = proc_ch %>%
    filter(Node %in% node_vec) %>%
    summarise(n_tags_node = n_distinct(TagID[Node == node]),
              n_tags_tot = n_distinct(TagID),
              obsEff = n_tags_node / n_tags_tot,
              obsEff_se = sqrt((obsEff * (1 - obsEff)) / n_tags_tot)) %>%
    mutate(Node = node) %>%
    select(Node, everything()) %>%
    mutate(est_tags = n_tags_node / obsEff,
           est_tags = round(est_tags),
           est_tags_se = n_tags_node * obsEff_se / (obsEff^2))

  return(node_eff)
}
