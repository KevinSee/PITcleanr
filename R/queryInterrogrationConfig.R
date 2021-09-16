#' @title PTAGIS Interagation sites configuration
#'
#' @description Query and download configuration metadata for PTAGIS Interrogation sites
#'
#' @author Kevin See
#'
#' @inheritParams queryInterrogationMeta
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr purrr
#' @export
#' @return NULL
#' @examples queryInterrogationConfig(site_code = "ZEN")

queryInterrogationConfig = function(site_code = NULL) {

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = "https://api.ptagis.org/sites/interrogation/configuration"

  if(!is.null(site_code)) url_req = paste(url_req, "current", site_code, sep = '/')

  # send query to PTAGIS
  web_req = httr::GET(url_req, ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')

  # parse the response
  parsed = httr::content(web_req,
                         'parsed')

  if(!is.null(site_code)) {
    res = parsed %>%
      as_tibble() %>%
      mutate(details = map(details,
                           .f = as_tibble)) %>%
      tidyr::unnest(cols = details) %>%
      mutate(across(c(startDate),
                    lubridate::ymd_hms)) %>%
      tibble::add_column(endDate = NA,
                         .after = "startDate") %>%
      mutate(across(configurationSequence,
                    as.numeric))
  } else {

    res = parsed %>%
      map(.f = as_tibble) %>%
      map_df(.f = identity) %>%
      mutate(across(c(startDate,
                      endDate),
                    lubridate::ymd_hms)) %>%
      mutate(across(configurationSequence,
                    as.numeric))

  }

  return(res)

}
