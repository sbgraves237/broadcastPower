#' Extract data from the FCC Query Broadcast Station Search
#' with dist of (lat, lon).
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
#' @return a data.frame of the table obtained from the FCC website.
#' @export FCCquery
#'
#' @examples
#' FCCdat1 <- FCCquery()
#' FCCdat2 <- FCCquery(dlat2=38L,
#'   mlat2=54L, slat2=12+1e-9, NS='N',
#'   dlon2=77, mlon2=0, slon2=36+1e-9, EW='W')
#' # test integers for dlat and mlat2
#' # and with real numbers, not integers,
#' # for slat2 and slon2

FCCquery <- function(band=c('FM', 'TV', 'AM'), 
                     dist=80, dlat2=38+((54+(12/60))/60), 
                     mlat2=0, slat2=0, NS=c('N', 'S'), 
                     dlon2=(-(77+(36/3600))), 
                     mlon2=0, slon2=0, EW=c('E', 'W') ){
  ##
  ## 1.  Query 
  ##  
  Band <- match.arg(band)
  query <- FCCqueryString(band=Band,
              dist=dist, dlat2=dlat2, mlat2=mlat2, slat2=slat2, NS=NS, 
              dlon2=dlon2, mlon2=mlon2, slon2=slon2, EW=EW)  
  Query <- xml2::read_html(query)
  ##
  ## 2.  Parse
  ##
  # fake it for now  
  queryDF <- data.frame()
  ##
  ## 3.  attributes
  ##
  attr(queryDF, 'query') <- query
  attr(queryDF, 'description') <- Query
  ##
  ## 4.  Return
  ##
  queryDF
}
