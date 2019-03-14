#' @title Node Efficiency
#'
#' @description This function uses detections at a given node, and all detections upstream of that node to calculate detection efficiency at that node. It uses the Petersen estimator for the number of tags at that node (or the Chapman if no recaptures are available). The detection efficiency is then calcuated as the number of tags detected at that node, divided by the estimate of total tags at that node.
#'
#' @param capHist_proc Dataframe returned by \code{processCapHist_*}, under the name \code{ProcCapHist}.
#'
#' @param node_order Dataframe returned by \code{processCapHist_*}, under the name \code{NodeOrder}.
#'
#' @param node character string of the node(s) of interest.
#'
#' @param method one of Petersen, Chapman or Bailey. Determines which formula to use when estimating tags past an array. Default is \code{Chapman}. If another method is selected, and fails (e.g. due to no recaptures), the \code{Chapman} estimator will be used.
#'
#' @author Kevin See
#' @import dplyr tidyr
#' @export
#' @return NULL
#' @examples estNodeEff()

estNodeEff = function(capHist_proc = NULL,
                      node_order = NULL,
                      node = NULL,
                      method = c('Chapman', 'Petersen', 'Bailey')) {

  stopifnot(!is.null(capHist_proc),
            !is.null(node_order))

  if(is.null(node)) {
    cat('If no node is supplied, calculated for all nodes')
    node = node_order %>%
      pull(Node)
  }

  # get default method
  method = match.arg(method)


  node_list = as.list(node)
  names(node_list) = node

  node_eff = node_list %>%
    purrr::map_df(.id = 'Node',
                  .f = function(x) {

                    # get a vector of nodes upstream of node
                    node_vec = node_order %>%
                      filter(grepl(paste0(x, ' '), Path) | Node == x) %>%
                      pull(Node)

                    # # get a vector of nodes downstream of node
                    # node_vec = node_order %>%
                    #   filter(Node == x) %>%
                    #   pull(Path) %>%
                    #   str_split(' ') %>%
                    #   unlist()

                    # if interested in an upstream array, use detections at downstream array as well to estimate efficiency
                    if(grepl('A0$', x)) {
                      node_vec = c(node_vec, gsub('A0$', 'B0', x))
                    }

                    # calculate node efficiency and estimate tags above that node
                    capHistNode = capHist_proc %>%
                      filter(Node %in% node_vec) %>%
                      mutate(nodePos = if_else(Node == x, 'down', 'up'),
                             seen = 1) %>%
                      select(TagID, nodePos, seen) %>%
                      distinct() %>%
                      mutate(nodePos = factor(nodePos,
                                              levels = c('down', 'up'))) %>%
                      tidyr::spread(nodePos, seen,
                                    fill = 0,
                                    drop = F) %>%
                      mutate(ch = paste0(down, up)) %>%
                      group_by(ch) %>%
                      summarise(freq = n_distinct(TagID)) %>%
                      ungroup() %>%
                      summarise(M = sum(freq[ch == '10'], freq[ch == '11']),
                                C = sum(freq[ch == '01'], freq[ch == '11']),
                                R = sum(freq[ch == '11']))

                    # estimate number of tags using several estimators
                    tagEst = list('Petersen' = capHistNode %>%
                                    transmute(N = M * C / R,
                                              SE = sqrt(M^2 * C * (C - R) / R^3)),
                                  'Bailey' = capHistNode %>%
                                    transmute(N = M * (C + 1) / (R + 1),
                                              SE = sqrt(M^2 * (C + 1) * (C - R) / ((R + 1)^2 * (R + 2)))),
                                  'Chapman' = capHistNode %>%
                                    transmute(N = (M + 1) * (C + 1) / (R + 1) - 1,
                                              SE = sqrt((M + 1) * (C + 1) * (M - R) * (C - R) / ((R + 1)^2 * (R + 2))))) %>%
                      map_df(.id = 'estimator',
                             .f = function(x) x %>%
                               mutate_at(vars(N),
                                         list(round)) %>%
                               mutate_at(vars(SE),
                                         list(round),
                                         digits = 1))

                    Nhat = tagEst %>%
                      filter(estimator == method)

                    if(is.na(Nhat$N)) {
                      Nhat = tagEst %>%
                        filter(estimator == 'Chapman')
                    }

                    capHistNode %>%
                      bind_cols(Nhat) %>%
                      select(-estimator) %>%
                      mutate(detEff = M / N,
                             detEff_SE = M*SE / N^2) %>%
                      rename(tagsAtNode = M,
                             tagsAboveNode = C,
                             tagsResighted = R,
                             estTagsAtNode = N,
                             estTags_SE = SE)
                  })

  return(node_eff)
}
