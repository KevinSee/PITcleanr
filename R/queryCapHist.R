#' @title PTAGIS Detection History
#'
#' @description Query and download complete capture history for one tag from PTAGIS
#'
#' @author Kevin See
#'
#' @param tagCode PTAGIS tag code
#'
#' @source \url{http://www.ptagis.org}
#'
#' @import dplyr httr
#' @export
#' @return NULL
#' @examples queryCapHist(tagCode = '384.3B23A0A6EE')

queryCapHist = function(tagCode = NULL,
                        configuration = NULL) {

  # need a tag code
  stopifnot(!is.null(tagCode))

  stopifnot(!is.null(configuration))
  # if(is.null(configuration)) configuration = buildConfig()

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/PITcleanr')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_one_tag?tag_id='

  # send query to PTAGIS
  web_req = httr::GET(paste0(url_req, tagCode),
                      ua)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from PTAGIS')


  if(is.null(headers(web_req)$`content-length`)) {
    print('Error with tag ID format')
    return(NULL)
  }

  if(headers(web_req)$`content-length` <= 3) {
    test = suppressMessages(httr::content(web_req,
                                          'parsed',
                                          encoding = 'UTF-8')) %>%
      names()

    if(test[1] == '0') {
      # no observations found in DART
      print(paste('Tag', tagCode, 'found in DART, but no detections exist'))

      # add the marking info (probably marked at LGR, never seen again)
      tagMeta_org = suppressWarnings(queryTagMeta(tagCode))

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
    }

    if(test[1] == '99') {
      print(paste('Tag', tagCode, 'not found in DART'))
      return(NULL)
    }

  }

  # parse the response
  parsed = suppressMessages(httr::content(web_req,
                                          'parsed',
                                          encoding = 'UTF-8')) %>%
    rename(`Tag Code` = tag_id,
           `Event Date Time Value` = eventdatetime,
           `Event Site Code Value` = site_code,
           `Antenna ID` = antenna_id,
           AntennaGroup = antenna_group,
           SiteType = obs_type)

  # get configuration ID
  config_df = parsed %>%
    mutate(Date = floor_date(`Event Date Time Value`,
                             'day')) %>%
    select(SiteID = `Event Site Code Value`,
           AntennaID = `Antenna ID`,
           Date) %>%
    distinct() %>%
    left_join(configuration,
              by = c('SiteID', 'AntennaID')) %>%
    filter((StartDate <= Date | (is.na(StartDate) & is.na(EndDate))),
           (EndDate > Date | is.na(EndDate))) %>%
    select(SiteID, ConfigID) %>%
    distinct()

  parsed = parsed %>%
    left_join(config_df,
              by = c('Event Site Code Value' = 'SiteID')) %>%
    rename(`Antenna Group Configuration Value` = ConfigID) %>%
    select(`Tag Code`:`Antenna ID`,
           `Antenna Group Configuration Value`,
           everything())

  return(parsed)

}
