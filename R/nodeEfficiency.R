#' @title nodeEfficiency: Estimates node detection efficiency.
#'
#' @description Estimates detection efficiences at each node in a data frame returned from
#' \code{nodeAssign}.
#'
#' @param data a data frame containing observation histories processed by \code{nodeAssign}
#'
#' @param node the desired node for a unique tag count.  Unique tags at the specified node are then
#' searched for at all other nodes in the data frame
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
nodeEfficiency <- function(data, direction = c('Upstream', 'Downstream')){

  direction <- match.arg(direction)

  nodes <- data %>%
    select(Node, NodeOrder) %>%
    distinct(Node, .keep_all = TRUE) %>%
    arrange(NodeOrder) %>%
    mutate(Unique_tags = as.integer(NA),
           Marks = as.integer(NA),
           Recaps = as.integer(NA))

  for(i in 1:nrow(nodes)){
    node_ <- nodes$Node[i] # change row to i
    nodeord_ <- nodes$NodeOrder[i] # change row to i

    unique <- data %>%
      filter(Node == node_) %>%
      select(TagID) %>%
      n_distinct()

    if(direction == 'Upstream'){

      tmp <- data %>%
        filter(NodeOrder > nodeord_) %>%
        distinct(TagID)
    }

    if(direction == 'Downstream'){
      tmp <- data %>%
        filter(NodeOrder < nodeord_) %>%
        distinct(TagID)
    }

    tmp_full <- data %>%
      filter(Node == node_) %>%
      inner_join(tmp) %>%
      distinct(TagID)

    nodes[i,3] <- unique
    nodes[i,4] <- n_distinct(tmp)
    nodes[i,5] <- n_distinct(tmp_full)

  } # end iloop

  df <- nodes
  # %>%
  #   mutate(d_hat = round(Recaps/Marks, digits = 2),
  #          N_tags = round(Unique_tags/d_hat, digits = 0)

  return(df)
}
