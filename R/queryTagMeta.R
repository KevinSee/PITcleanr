#' @title PTAGIS Meta Data
#'
#' @description Query and download event metadata for one tag from PTAGIS. In the interest of not overwhelming PTAGIS with data requests, this function should not be used to query more than a thousand tags a day and there should be a couple of seconds between calls. If a user is looking for more than that, using the advanced reporting system in PTAGIS is the best option at this time.
#'
#' @author Kevin See
#'
#' @param tag_code PTAGIS tag code
#' @param api_key PTAGIS API key, given to individual users
#' @param type one of `mark`, `recapture`, `recovery`, or `observation`. If one is specified, only data about evernts of that type are returned. The defualt, `NA`, will return data about all events for that tag.
#'
#' @source \url{http://api.ptagis.org}
#'
#' @import dplyr httr
#' @return Gets data about all events for a given tag code.
#' @examples queryTagMeta("3D9.1C2D929849")

queryTagMeta = function(tag_code = NULL,
                        api_key = NULL,
                        type = c(NA,
                                 "mark",
                                 "recapture",
                                 "recovery",
                                 "observation")) {

  # need a tag code
  stopifnot(!is.null(tag_code))

  # need an API key
  stopifnot(!is.null(api_key))

  type = match.arg(type)

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = 'http://api.ptagis.org/data/events'

  # send query to PTAGIS
  if(!is.na(type)) {
    web_req = httr::GET(paste0(url_req,
                               "/", type,
                               "?apiKey=", api_key,
                               "&tagCode=", tag_code),
                        ua)
  } else {
    web_req = httr::GET(paste0(url_req,
                               "?apiKey=", api_key,
                               "&tagCode=", tag_code),
                        ua)
  }

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')

  # parse the response
  parsed = httr::content(web_req,
                         'parsed')

  if(!is.na(type)) {
    if(length(parsed[[1]]) == 1) {
      parsed <- parsed %>%
        stack() %>%
        tidyr::spread(ind, values) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        mutate(
          across(
            contains("date"),
            ~ lubridate::mdy(.)),
          across(
            contains("length") | contains("year"),
            ~ as.numeric(.)))
    } else {
      parsed <- parsed %>%
        map(.f = function(x) {
          x %>%
            stack() %>%
            tidyr::spread(ind, values) %>%
            dplyr::as_tibble() %>%
            janitor::clean_names() %>%
            mutate(
              across(
                contains("date"),
                ~ lubridate::mdy(.)),
              across(
                contains("length") | contains("year"),
                ~ as.numeric(.)))
        }) %>%
        map_df(.f = identity) %>%
        relocate(tag_code,
                 .before = 1)
    }

  } else {
    parsed <- parsed %>%
      map(.f = function(x) {
        x %>%
          stack() %>%
          tidyr::spread(ind, values) %>%
          dplyr::as_tibble() %>%
          janitor::clean_names() %>%
          mutate(
            across(
              contains("date"),
              ~ lubridate::ymd_hms(.)),
            across(
              contains("length") | contains("year"),
              ~ as.numeric(.)))
      }) %>%
      map_df(.f = identity) %>%
      relocate(tag_code,
               .before = 1)
  }


  return(parsed)

}
