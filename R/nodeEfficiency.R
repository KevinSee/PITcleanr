#' @title nodeEfficiency: Estimates node detection efficiency.
#'
#' @description Estimates detection efficiences at each node in a data frame returned from
#' \code{nodeAssign} and \code{writeCapHistOutput}.
#'
#' @param capHist_proc a data frame containing observation histories processed by \code{nodeAssign} and \code{writeCapHistOutput}
#'
#' @param node_order a data frame containing the output of \code{createNodeOrder}
#'
#' @param node a character string of the desired nodes for efficiency estimates.  Unique tags at the specified nodes are then
#' searched for at all other nodes in the data frame in the designated upstream or downstream direction.
#'
#' @param direction
#'
#' @author Ryan Kinzer
#'
#' @examples nodeEfficiency()
#'
#' @import dplyr
#' @export
#' @return NULL
nodeEfficiency <- function(capHist_proc, node_order, direction = c('upstream', 'downstream')){

  stopifnot(!is.null(capHist_proc),
            !is.null(node_order))

  direction <- match.arg(direction)

  node = node_order %>%
    select(Node) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  node_list = as.list(node)
  names(node_list) = node

  node_eff = node_list %>%
    purrr::map_df(.id = 'Node',
                  .f = function(x) {

                    if(direction == 'upstream'){
                    # get a vector of nodes upstream of node
                    node_vec = node_order %>%
                      filter(grepl(paste0(x, ' '), Path) | Node == x) %>%
                      select(Node) %>%
                      as.matrix() %>%
                      as.vector()
                    }

                    if(direction == 'downstream'){
                      cat('Calcualting efficiencies for downstream moving fish is not currently available')
                    }

                    # if interested in an upstream array, use detections at downstream array as well to estimate efficiency
                    # if(grepl('A0$', x)) {
                    #   node_vec = c(node_vec, gsub('A0$', 'B0', x))
                    # }


                    marks <- capHist_proc %>%
                      filter(Node %in% node_vec) %>%
                      filter(Node != x) %>%
                      distinct(TagID)

                    obs <- capHist_proc %>%
                      filter(Node == x) %>%
                      distinct(TagID) %>%
                      inner_join(marks, by = "TagID") %>%
                      n_distinct()

                    # calculate node efficiency and estimate tags above that node
                    capHist_proc %>%
                      filter(Node %in% node_vec) %>%
                      summarise(Unique_tags = n_distinct(TagID[Node == x]),
                                Marks = n_distinct(TagID[Node != x]),
                                Recaps = obs)
                  })



  node_eff <- node_eff %>%
    filter(Unique_tags > 0 | Marks > 0) %>%
    filter(Node != 'GRA') %>%
    mutate(det_eff = Recaps/Marks,
           se_eff = (1/Marks^2)*(Marks*det_eff)*(1-det_eff),
           N_tags = Unique_tags/det_eff,
           se_N = sqrt(((Marks+1)*(Unique_tags+1)*(Marks - Recaps) * (Unique_tags - Recaps))/ ((Recaps+1)^2 * (Recaps+2))),
           lwr_N = N_tags - 1.96*se_N,
           upr_N = N_tags + 1.96*se_N)

  return(node_eff)
}
