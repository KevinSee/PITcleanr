#' @title Estimate Tags
#'
#' @description This function uses detections at a given node, and all detections upstream of that node to calculate detection efficiency at that node. It then uses the number of tags detected at that node and the detection efficiency to estimate the number of tags that have migrated upstream of that node.
#'
#' @param capHist_proc Dataframe returned by \code{processCapHist_*}, under the name \code{ProcCapHist}.
#'
#' @param node_order Dataframe returned by \code{processCapHist_*}, under the name \code{NodeOrder}.
#'
#' @param node character string of the node(s) of interest.
#'
#' @author Kevin See
#' @import dplyr purrr
#' @export
#' @return NULL
#' @examples # estNumTags()

estNumTags = function(capHist_proc = NULL,
                      node_order = NULL,
                      node = NULL) {

  stopifnot(!is.null(capHist_proc),
            !is.null(node_order))

  if(is.null(node)) {
    cat('If no node is supplied, calculated for all nodes')
    node = node_order %>%
      select(Node) %>%
      distinct() %>%
      as.matrix() %>%
      as.character()
  }

  node_list = as.list(node)
  names(node_list) = node

  tagEst = node_list %>%
    purrr::map_df(.id = 'Node',
                  .f = function(x) {

                    # get a vector of nodes upstream of node
                    node_vec = node_order %>%
                      filter(grepl(paste0(x, ' '), Path) | Node == x) %>%
                      select(Node) %>%
                      as.matrix() %>%
                      as.vector()

                    # if interested in an upstream array, use detections at downstream array as well to estimate efficiency
                    if(grepl('A0$', x)) {
                      node_vec = c(node_vec, gsub('A0$', 'B0', x))
                    }

                    # calculate node efficiency and estimate tags above that node
                    capHist_proc %>%
                      filter(Node %in% node_vec) %>%
                      summarise(n_tags_node = n_distinct(TagID[Node == x]),
                                n_tags_tot = n_distinct(TagID),
                                obsEff = n_tags_node / n_tags_tot,
                                obsEff_se = sqrt((obsEff * (1 - obsEff)) / n_tags_tot)) %>%
                      mutate(est_tags = n_tags_node / obsEff,
                             est_tags = round(est_tags),
                             est_tags_se = n_tags_node * obsEff_se / (obsEff^2))
                  })

  tagEst0 = tagEst %>%
    filter(n_tags_node == 0) %>%
    arrange(Node)

  tagEst = tagEst %>%
    anti_join(tagEst0,
              by = 'Node') %>%
    bind_rows(tagEst0)


  return(tagEst)
}
