#' @title PTAGIS Detection History
#'
#' @description Query and download complete capture history for one tag from PTAGIS (through DART). Due to
#' PTAGIS no longer offering queries for the mark event information for a given tag code, only
#' observations after the mark event are returned.
#'
#' @author Kevin See
#'
#' @param tag_code PTAGIS tag code
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr
#' @export
#' @return NULL
#' @examples queryCapHist(tag_code = '384.3B23A0A6EE')

queryCapHist = function(tag_code = NULL,
                        configuration = NULL) {

  # need a tag code
  stopifnot(!is.null(tag_code))

  if(is.null(configuration)) {
    message("No configuration file supplied; querying PTAGIS\n")

    configuration = suppressMessages(queryPtagisMeta()) %>%
      rename(config_id = configuration_sequence)
  }

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_one_tag?tag_id='

  # send query to PTAGIS
  web_req = httr::GET(paste0(url_req, tag_code),
                      ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')


  if(is.null(headers(web_req)$`content-length`)) {
    message('Error with tag ID format')
    return(NULL)
  } else {
    test = suppressMessages(httr::content(web_req,
                                          'parsed',
                                          encoding = 'UTF-8')) %>%
      names()
  }

  if(test[1] == '0') {
    # no observations found in DART
    message(paste('Tag', tag_code, 'found in DART, but no detections exist\n'))
    return(NULL)

    # # add the marking info (probably marked at LGR, never seen again)
    # tagMeta_org = suppressWarnings(queryTagMeta(tag_code))
    #
    # tagMeta_df = tagMeta_org %>%
    #   mutate(`Antenna ID` = as.character(NA),
    #          `Antenna Group Configuration Value` = 0,
    #          AntennaGroup = as.character(NA)) %>%
    #   select(`Tag Code` = tag,
    #          `Event Date Time Value` = markDate,
    #          `Event Site Code Value` = markSiteCode,
    #          `Antenna ID`,
    #          `Antenna Group Configuration Value`,
    #          AntennaGroup) %>%
    #   left_join(configuration %>%
    #               select(`Event Site Code Value` = SiteID,
    #                      SiteType) %>%
    #               distinct(),
    #             by = c("Event Site Code Value"))
    #
    # return(tagMeta_df)
  } else if(test[1] == '99') {
    message(paste('Tag', tag_code, 'not found in DART\n'))
    return(NULL)
  } else {

    # parse the response
    parsed = suppressMessages(httr::content(web_req,
                                            'parsed',
                                            encoding = 'UTF-8')) %>%
      dplyr::rename(tag_code = tag_id,
                    event_date_time_value = eventdatetime,
                    event_site_code_value = site_code,
                    site_type = obs_type) |>
      dplyr::mutate(event_type_name = dplyr::recode(site_type,
                                                    "INT" = "Observation",
                                                    "REC" = "Recapture",
                                                    "MRT" = "Recovery"))

    # get configuration ID
    config_df <- parsed %>%
      mutate(date = lubridate::floor_date(event_date_time_value,
                                          'day')) %>%
      select(site_code = event_site_code_value,
             antenna_id,
             date) %>%
      distinct() %>%
      left_join(configuration,
                by = c('site_code',
                       'antenna_id'),
                multiple = "all") %>%
      filter((start_date <= date | (is.na(start_date) & is.na(end_date))),
             (end_date > date | is.na(end_date))) %>%
      select(site_code, antenna_id,
             antenna_group_configuration_value = config_id) %>%
      distinct()

    # add configuration id to each observation
    parsed <- parsed %>%
      left_join(config_df,
                by = c('event_site_code_value' = 'site_code',
                       "antenna_id")) %>%
      select(tag_code,
             event_site_code_value,
             event_date_time_value,
             antenna_id,
             antenna_group_configuration_value,
             everything())


    # query mark information

    # compose url with query
    mrk_url_req = 'https://api.ptagis.org/data/events/'

    # send query to PTAGIS
    mrk_web_req = httr::GET(paste0(mrk_url_req, tag_code),
                        ua)

    mrk_parsed <- httr::content(mrk_web_req,
                  'parsed',
                  encoding = 'UTF-8') |>
      purrr::map_df(.f = as_tibble) |>
      janitor::clean_names() |>
      dplyr::select(tag_code,
                    event_site_code_value = site_code,
                    site_name,
                    event_type_name = event_type,
                    event_date_time_value = event_date,
                    cth_count = event_count) |>
      dplyr::mutate(
        dplyr::across(
          event_date_time_value,
          lubridate::ymd_hms
        )
      ) |>
      dplyr::filter(event_type_name == "Mark") |>
      dplyr::select(dplyr::any_of(names(parsed)))

    # add mark data to other detections
    if(nrow(mrk_parsed) > 0) {
      parsed <- parsed |>
        dplyr::bind_rows(mrk_parsed) |>
        dplyr::arrange(tag_code,
                       event_date_time_value)
    }


    return(parsed)
  }
}
