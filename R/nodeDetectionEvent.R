#' @title Node Detection Event Sequencing
#'
#' @description the function uses base R and run length encoding to identify the
#'   number of times a fish is detected on a single node during a single event and before being interupted
#'   with a detection on a different node.  The function returns a character string the same length as \code{x}.
#'
#' @author Ryan N. Kinzer
#'
#' @param x character string of Node names
#'
#' @return character string
#' @export
#'
#' @examples nodeDetectionEvent()
nodeDetectionEvent <- function(x){
  r = rle(x)
  r$values = seq_along(r$values)
  node_event = paste0(x, '.', inverse.rle(r))
  return(node_event)
}
