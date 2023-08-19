#' Compose a query string for an FCC Query Broadcast Station Search
#'
#' @param band c('FM', 'TV', 'AM', 'fm', 'tv', 'am')
#' @param dist number of kilometers radius in which to search
#' @param dlat2 degrees latitude
#' @param mlat2 minutes latitude
#' @param slat2 seconds latitude
#' @param NS c('N', 'S', 'n', 's')
#' @param dlon2 degrees longitude
#' @param mlon2 minutes longitude
#' @param slon2 seconds longitude
#' @param EW c('E', 'W', 'e', 'w')
#'
#' @return a character string in the format required by the FCC.
#' @export FCCqueryString
#'
#' @examples
#' FCCstr1 <- FCCqueryString()
#' FCCstr2 <- FCCqueryString(dlat2=38L,
#'   mlat2=54L, slat2=12+1e-9, NS='N',
#'   dlon2=77, mlon2=0, slon2=36+1e-9, EW='W')
#' # test integers for dlat and mlat2
#' # and with real numbers, not integers,
#' # for slat2 and slon2
FCCqueryString <- function(band=c('FM', 'TV', 'AM', 'fm', 'tv', 'am'),
                           dist=80, dlat2=38+((54+(12/60))/60),
                           mlat2=0, slat2=0, NS=c('N', 'S'),
                           dlon2=(-(77+(36/3600))),
                           mlon2=0, slon2=0, EW=c('E', 'W') ){

   # template (from Gordon):
  # https://transition.fcc.gov/fcc-bin/fmq?list=4&dist=50&dlat2=39&mlat2=5&slat2=3.519&NS=N&dlon2=94&mlon2=28&slon2=46.92&EW=W
  ##
  ## 1.  https://...list=1&dist=80
  ##
  Band <- tolower(match.arg(band))
  fcc1 <- paste0(
    'https://transition.fcc.gov/fcc-bin/',
    Band, 'q?list=4&dist=', dist)
  ##
  ## 2.  lat, lon
  ##
  lat <- (dlat2 + ((mlat2 + (slat2/60))/60))
  lon <- (dlon2 + ((mlon2 + (slon2/60))/60))
  if(lat<0){
    NS <- 'S'
    lat <- (-lat)
  } else NS <- NS[1]
  if(lon<0){
    EW <- 'W'
    lon - (-lon)
  } else EW <- EW[1]
  Dlat2 <- trunc(lat)
  Dlon2 <- trunc(lon)
  Mlat <- 60*(lat-Dlat2)
  Mlon <- 60*(lon-Dlon2)
  Mlat2 <- trunc(Mlat)
  Mlon2 <- trunc(Mlon)
  Slat2 <- 60*(Mlat-Mlat2)
  Slon2 <- 60*(Mlon-Mlon2)
  fcc2 <- paste0(fcc1, '&dlat2=', Dlat2,
                 '&mlat2=', Mlat2, '&slat2=', Slat2,
                 '&NS=', NS, '&dlon2=', Dlon2,
                 '&mlon2=', Mlon2, '&slon2=', Slon2,
                 '&EW=', EW)
  ##
  ## Return
  ##
  fcc2
}
