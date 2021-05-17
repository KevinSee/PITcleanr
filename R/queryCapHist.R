#' @title PTAGIS Detection History
#'
#' @description Query and download complete capture history for one tag from PTAGIS. Due to
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

  # stopifnot(!is.null(configuration))
  # if(is.null(configuration)) configuration = buildConfig() %>%
  #     mutate(node = site_code)
  if(is.null(configuration)) configuration = queryPtagisMeta() %>%
      rename(config_id = configuration_sequence)

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/BiomarkABS/PITcleanr')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_one_tag?tag_id='

  # send query to PTAGIS
  web_req = httr::GET(paste0(url_req, tag_code),
                      ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')


  if(is.null(headers(web_req)$`content-length`)) {
    print('Error with tag ID format')
    return(NULL)
  } else if(headers(web_req)$`content-length` > 3) {
    return(NULL)
  } else if(headers(web_req)$`content-length` <= 3) {
    test = suppressMessages(httr::content(web_req,
                                          'parsed',
                                          encoding = 'UTF-8')) %>%
      names()
  }
  if(test[1] == '0') {
    # no observations found in DART
    print(paste('Tag', tag_code, 'found in DART, but no detections exist'))

    # add the marking info (probably marked at LGR, never seen again)
    tagMeta_org = suppressWarnings(queryTagMeta(tag_code))

    tagMeta_df = tagMeta_org %>%
      mutate(`Antenna ID` = as.character(NA),
             `Antenna Group Configuration Value` = 0,
             AntennaGroup = as.character(NA)) %>%
      select(`Tag Code` = tag,
             `Event Date Time Value` = markDate,
             `Event Site Code Value` = markSiteCode,
             `Antenna ID`,
             `Antenna Group Configuration Value`,
             AntennaGroup) %>%
      left_join(configuration %>%
                  select(`Event Site Code Value` = SiteID,
                         SiteType) %>%
                  distinct(),
                by = c("Event Site Code Value"))

    return(tagMeta_df)
  } else if(test[1] == '99') {
    print(paste('Tag', tag_code, 'not found in DART'))
    return(NULL)
  } else {

    # parse the response
    parsed = suppressMessages(httr::content(web_req,
                                            'parsed',
                                            encoding = 'UTF-8')) %>%
      rename(`Tag Code` = tag_id,
             `Event Date Time Value` = eventdatetime,
             `Event Site Code Value` = site_code,
             `Antenna ID` = antenna_id,
             `Antenna Group` = antenna_group,
             SiteType = obs_type)

    # get configuration ID
    config_df = parsed %>%
      mutate(date = lubridate::floor_date(`Event Date Time Value`,
                                          'day')) %>%
      select(site_code = `Event Site Code Value`,
             antenna_id = `Antenna ID`,
             date) %>%
      distinct() %>%
      left_join(configuration,
                by = c('site_code',
                       'antenna_id')) %>%
      filter((start_date <= date | (is.na(start_date) & is.na(end_date))),
             (end_date > date | is.na(end_date))) %>%
      select(site_code, antenna_id, config_id) %>%
      distinct()

    parsed = parsed %>%
      left_join(config_df,
                by = c('Event Site Code Value' = 'site_code',
                       'Antenna ID' = "antenna_id")) %>%
      rename(`Antenna Group Configuration Value` = config_id) %>%
      select(`Tag Code`:`Antenna ID`,
             `Antenna Group Configuration Value`,
             everything())

    return(parsed)
  }
}
