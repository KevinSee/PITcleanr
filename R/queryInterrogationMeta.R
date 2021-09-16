#' @title PTAGIS Interagation sites metadata
#'
#' @description Query and download metadata for PTAGIS Interrogation sites
#'
#' @author Kevin See
#'
#' @param site_code PTAGIS site code. Default is \code{NULL} which will query all sites
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr purrr
#' @export
#' @return NULL
#' @examples queryInterrogationMeta(site_code = 'ZEN')

queryInterrogationMeta = function(site_code = NULL) {

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = "https://api.ptagis.org/sites/interrogation"

  if(!is.null(site_code)) url_req = paste(url_req, site_code, sep = '/')

  # send query to PTAGIS
  web_req = httr::GET(url_req, ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')

  # parse the response
  parsed = httr::content(web_req,
                         'parsed')

  if(!is.null(site_code)) parsed = list(parsed)

  res = parsed %>%
    map(.f = as_tibble) %>%
    map_df(.f = identity) %>%
    mutate(across(c(latitude:longitude,
                    firstYear,
                    lastYear),
                  as.numeric))

  return(res)

}
