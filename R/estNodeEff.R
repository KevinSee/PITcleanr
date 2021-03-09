#' @title Node Efficiency
#'
#' @description This function uses detections at a given node, and all detections upstream of that node to calculate detection efficiency at that node. It uses the Chapman, Petersen, or Bailey estimator for the number of tags at that node (the Chapman estimator is used if no recaptures are available). The detection efficiency is then calculated as the number of tags detected at that node, divided by the estimate of total tags at that node.
#'
#' @param cap_hist Dataframe returned by `compress()`; must have columns named `tag_code` and `node`. More appropriate would be the output of `filterDetections()` or `prepWrapper()`, which is then filterd by either `auto_keep_obs` or `user_keep_obs`.
#'
#' @param node_order Dataframe returned by `buildNodeOrder()`
#'
#' @param node character string of the node(s) of interest. If not supplied, efficiencies will be calculated for every node in `node_order`.
#'
#' @param method one of `Chapman`, `Petersen`, or `Bailey.` Determines which formula to use when estimating tags past an array. Default is `Chapman`. If another method is selected, and fails (e.g. due to no recaptures), the `Chapman` estimator will be used.
#'
#' @author Kevin See
#' @import dplyr tidyr purrr
#' @importFrom rlang set_names
#' @export
#' @return a tibble
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
      pull(node)
  }

  # get default method
  method = match.arg(method)


  node_list = as.list(node) %>%
    rlang::set_names()

  node_eff = node_list %>%
    purrr::map_df(.id = 'node',
                  .f = function(x) {

                    # get a vector of nodes upstream of node
                    node_vec = node_order %>%
                      filter(grepl(paste0(x, ' '), path) | node == x) %>%
                      pull(node)

                    # # get a vector of nodes downstream of node
                    # node_vec = node_order %>%
                    #   filter(Node == x) %>%
                    #   pull(Path) %>%
                    #   str_split(' ') %>%
                    #   unlist()

                    # if interested in an upstream array, use detections at downstream array as well to estimate efficiency
                    if(grepl('A0$', x)) {
                      node_vec = c(node_vec, stringr::str_replace(x, 'A0$', 'B0'))
                    }

                    # calculate node efficiency and estimate tags above that node
                    det_hist_summ = capHist_proc %>%
                      filter(node %in% node_vec) %>%
                      mutate(node_position = if_else(node == x, 'down', 'up'),
                             seen = 1) %>%
                      select(tag_code, node_position, seen) %>%
                      distinct() %>%
                      mutate(node_position = factor(node_position,
                                                    levels = c('down', 'up'))) %>%
                      tidyr::pivot_wider(names_from = "node_position",
                                         names_sort = T,
                                         values_from = "seen",
                                         values_fill = 0)

                    if(!("up" %in% names(det_hist_summ))) {
                      det_hist_summ = det_hist_summ %>%
                        tibble::add_column(up = 0)
                    }

                    cap_hist_node = det_hist_summ %>%
                      summarise(M = n_distinct(tag_code[down == 1]),
                                C = n_distinct(tag_code[up == 1]),
                                R = n_distinct(tag_code[down == 1 & up == 1]))
                      # tidyr::unite(ch, down, up, sep = "") %>%
                      # group_by(ch) %>%
                      # summarise(freq = n_distinct(tag_code)) %>%
                      # ungroup() %>%
                      # summarise(M = sum(freq[ch == '10'], freq[ch == '11']),
                      #           C = sum(freq[ch == '01'], freq[ch == '11']),
                      #           R = sum(freq[ch == '11']))

                    # estimate number of tags using several estimators
                    tag_est = list('Petersen' = cap_hist_node %>%
                                    transmute(N = M * C / R,
                                              SE = sqrt(M^2 * C * (C - R) / R^3)),
                                  'Bailey' = cap_hist_node %>%
                                    transmute(N = M * (C + 1) / (R + 1),
                                              SE = sqrt(M^2 * (C + 1) * (C - R) / ((R + 1)^2 * (R + 2)))),
                                  'Chapman' = cap_hist_node %>%
                                    transmute(N = (M + 1) * (C + 1) / (R + 1) - 1,
                                              SE = sqrt((M + 1) * (C + 1) * (M - R) * (C - R) / ((R + 1)^2 * (R + 2))))) %>%
                      purrr::map_df(.id = 'estimator',
                                    .f = function(x) {
                                      x %>%
                                      mutate(across(N,
                                                    round)) %>%
                                      mutate(across(SE,
                                                    round,
                                                    digits = 1))
                                      })

                    n_hat = tag_est %>%
                      filter(estimator == method)

                    if(is.na(n_hat$N)) {
                      n_hat = tag_est %>%
                        filter(estimator == 'Chapman')
                    }

                    cap_hist_node %>%
                      bind_cols(n_hat) %>%
                      select(-estimator) %>%
                      mutate(eff_est = M / N,
                             eff_se = M*SE / N^2) %>%
                      rename(tags_at_node = M,
                             tags_above_node = C,
                             tags_resighted = R,
                             est_tags_at_node = N,
                             est_tags_se = SE)
                  })

  return(node_eff)
}
