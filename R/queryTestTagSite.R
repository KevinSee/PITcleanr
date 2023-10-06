#' @title PTAGIS Test Tag Detections
#'
#' @description Query and download complete capture history for one test tag from PTAGIS for an entire calendar year. May take awhile to run.
#'
#' @author Kevin See
#'
#' @param site_code PTAGIS site code
#' @param year Calendar year to be queried
#' @inheritParams queryTagMeta
#'
#' @source \url{http://api.ptagis.org}
#'
#' @import dplyr httr lubridate
#' @export
#' @return NULL
#' @examples #queryTestTagSite(site_code = "LWE", year = 2023)

queryTestTagSite = function(site_code = NULL,
                            year = NULL,
                            api_key = NULL) {

  # need a tag code
  stopifnot(!is.null(tag_code))

  if(is.null(year)) {
    year = lubridate::year(lubridate::today())
  }

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = paste0('https://api.ptagis.org/data/events/timertag/site/',
                   site_code,
                   "/year/",
                   year,
                   "?apiKey=", api_key)

  # send query to PTAGIS
  web_req = httr::GET(url_req,
                      ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')

  # parse the response
  parsed = httr::content(web_req,
                         'parsed')

  results <- parsed |>
    map(.f = function(x) {
      x %>%
        stack() %>%
        tidyr::spread(ind, values) %>%
        dplyr::as_tibble()
    }) |>
    map_df(.f = identity) |>
    clean_names() |>
    mutate(
      across(
        time_stamp,
        ~ lubridate::ymd_hms(.))) |>
    relocate(tag_code,
             .before = 1) |>
    arrange(site_code,
            transceiver_id,
            antenna_id,
            time_stamp)

  # results |>
  #   group_by(antenna_id) |>
  #   mutate(diff = difftime(lead(time_stamp),
  #                          time_stamp,
  #                          units = "hours"),
  #          diff_num = as.numeric(diff),
  #          across(diff_num,
  #                 ~ round(., digits = 4))) |>
  #   ungroup() |>
  #   ggplot(aes())
  #   # as.data.frame()
  #   # filter(diff_num > 1.001)
  #   filter(diff_num > 12)


    return(results)
  }

