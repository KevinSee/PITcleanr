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
#' @return NULL
#' @examples queryTagMeta()

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
    as.data.frame() %>%
    tbl_df()

    return(parsed)

}
