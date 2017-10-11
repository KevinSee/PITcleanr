#' @title PTAGIS Interagation sites configuration
#'
#' @description Query and download configuration metadata for PTAGIS Interrogation sites
#'
#' @author Kevin See
#'
#' @param site PTAGIS site code. Default is \code{NULL} which will query all sites
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr purrr
#' @export
#' @return NULL
#' @examples queryInterrogationConfig()

queryInterrogationConfig = function(site = NULL) {

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = 'http://api.ptagis.org/interrogationsites'

  if(!is.null(site)) url_req = paste(url_req, site, sep = '/')

  url_req = paste(url_req, 'configurations', sep = '/')

  # send query to PTAGIS
  web_req = httr::GET(url_req, ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')

  # parse the response
  parsed = httr::content(web_req,
                         'parsed')

  res = parsed %>%
    purrr::map(.f = function(x) {
      purrr::map(.x = x,
                 .f = function(y) {
                   ifelse(is.null(y),
                          NA,
                          y)
                 })
    }) %>%
    purrr::map_df(.f = identity) %>%
    dplyr::mutate_at(vars(startDate, endDate),
                     funs(lubridate::ymd_hms))

  return(res)

}
