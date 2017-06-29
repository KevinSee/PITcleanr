#' @title PTAGIS Interagation sites
#'
#' @description Query and download metadata for PTAGIS Interrogation sites
#'
#' @author Kevin See
#'
#' @param site PTAGIS site code. Default is \code{NULL} which will query all sites
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr purrr
#' @return NULL
#' @examples queryInterrogationSites()

queryInterrogationSites = function(site = NULL) {

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = 'http://api.ptagis.org/interrogationsites'

  # send query to PTAGIS
  if(is.null(site)) web_req = httr::GET(url_req, ua)

  if(!is.null(site)) web_req = httr::GET(paste0(url_req, '/', site),
                                         ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')

  # parse the response
  parsed = httr::content(web_req,
                         'parsed')

  if(!is.null(site)) parsed = list(parsed)

  parsed = parsed %>%
    purrr::map(.f = function(x) {
      for(i in 1:length(x)) {
        if(is.null(x[[i]])) {
          if(names(x)[[i]] %in% c('isSiteActive')) {
            x[[i]] = vector('logical', 1)
            x[[i]] = NA
          }
          else x[[i]] = vector('character', 1)
        }
      }
      return(x)
    }) %>%
    purrr::map_df(`[`,
                  c('siteCode', 'siteName', 'siteType', 'siteDescription', 'isSiteActive', 'latitude', 'longitude', 'rkm', 'operationsOrganization', 'firstYearOperated', 'lastYearOperated')) %>%
    dplyr::mutate_at(vars(latitude:longitude),
                     funs(as.numeric))

  return(parsed)

}
