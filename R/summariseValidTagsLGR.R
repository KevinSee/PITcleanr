#' @title Summarise Valid Tags
#'
#' @description Filter the valid tag dataframe returned by \code{filterLGRtrapDB} to only include the tag code and the tagging date
#'
#' @author Kevin See
#'
#' @param valid_df dataframe of valid tags extracted from the trap database with the \code{filterLGRtrapDB} function
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples summariseValidTagsLGR(filterLGRtrapDB(spawnYear = 2015))

summariseValidTagsLGR = function(valid_df) {
  valid_df %>%
    dplyr::group_by(TagID = LGDNumPIT) %>%
    dplyr::summarise(TrapDate = min(CollectionDate, na.rm = T))
}
