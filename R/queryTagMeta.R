#' @title PTAGIS MRR sites
#'
#' @description Query and download mark data for one tag from PTAGIS
#'
#' @author Kevin See
#'
#' @param tagCode PTAGIS tag code
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr
#' @export
#' @return From PTAGIS: WARNING: this feature has been disabled. Contact PTAGIS for more info. Gets mark event information for a given tag code.
#' @examples queryTagMeta("3D9.1C2D929849")

queryTagMeta = function(tagCode = NULL) {

  # need a tag code
  stopifnot(!is.null(tagCode))

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = 'http://api.ptagis.org/tagdata'

  # send query to PTAGIS
  web_req = httr::GET(paste0(url_req, '/', tagCode),
                      ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')

  # parse the response
  parsed = httr::content(web_req,
                         'parsed') %>%
    stack() %>%
    tidyr::spread(ind, values) %>%
    tbl_df() %>%
    mutate_at(vars(matches('Date')),
              funs(lubridate::ymd_hms)) %>%
    mutate_at(vars(forkLength, broodYear),
              funs(as.numeric))

  return(parsed)

}
