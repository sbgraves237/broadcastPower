#' Extract from FCC Query Broadcast Station Search
#' 
#' `FCCquery` returns a [data.frame()] of all stations 
#' broadcasting in `band` within `dist` km of (lat, lon).
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
#' @return an object of class `c(FCCquery, data.frame)` 
#' of the table obtained from the FCC website
#' with the following attributes:
#' \itemize{
#'   \item{`query`}{
#'      the character string returned by [FCCqueryString()] and 
#'      used to obtain the resulting [data.frame()].
#'   }
#'   \item{`query_time`}{
#'      an object of class [proc.time()] giving the time required
#'      to extract the information from `fcc.gov`. 
#'   } 
#'   \item{`entriesWFmtErrors`}{
#'      a [list()] of entries in the data returned from `fcc.gov`
#'      with other than the standard 441 fields after [strsplit()] 
#'      on '|'.  If `length(entriesWFmtErrors)` > 0, a warning 
#'      will be issued.  Each element in `entriesWFmtErrors` 
#'      corresponds to a row of the [data.frame()] returned that 
#'      is all `NA`s with the name of that element matching the 
#'      row of the returned [data.frame()].  
#'   }
#' }
#' 
#' The [data.frame()] returned has the following structure:  
#' \itemize{
#'   \item{`Call`}{ call letters}
#'   \item{`Channel`}{numeric vector}
#'   \item{`Class`}{
#'     character vector with values like `C1, D, A, C2, LP1, C3`, 
#'     etc.
#'   }
#'   \item{`Service`}{ 
#'     character vector with values like `FM, FS, FX, FL`, etc.
#'   }
#'   \item{`Frequency, FrequencyUnits`}{
#'     numeric and character vectors of the frequency 
#'     and units, respectively, with units being something 
#'     like `khz`. 
#'   }
#'   \item{`Status`}{
#'     character vector of codes like `LIC, CP, STA`, etc.  
#'   }
#'   \item{`City`}{
#'     character vector of the names of the city in 
#'     or near which the station claims as its home.  
#'   }
#'   \item{`State`}{
#'    a character vector of state codes, most of not 
#'    all of which will be 2-letters.
#'   }
#'   \item{`Country`}{
#'     a character vector of the name of the country, 
#'     e.g., `USA`.
#'   }
#'   \item{`FileNumber`}{
#'     character vector of the FCC file number for this 
#'     entry.  
#'   }
#'   \item{FacilityID}{
#'     character vector of the FCC `FacilityID` code.
#'   }
#'   \item{`ERP, ERPunits`}{
#'     `ERP` = a numeric vector of FCC approved 
#'     [effective radiated power](https://duckduckgo.com/?t=h_&q=effective+radiated+power+wikipedia&ia=web)
#'     for that station as a numeric vector and a character vector 
#'     of the units for that station.  
#'   }
#'   \item{`HAAT`}{
#'     Numeric vector of the 
#'     [height above average terrain](https://en.wikipedia.org/wiki/Height_above_average_terrain)
#'   }
#'   \item{`Dist_km, Dist_kmUnits, Dist_mi, Dist_miUnits`}{
#'     Numeric distances and character units from the reference 
#'     to the station for that entry.  `Dist_kmUnits` and 
#'     `Dist_miUnit` should all be `km` and `mi`, respectively.  
#'     If they are not, throw a [warning()].  (These are 
#'     provided in the query returned by `fcc.gov` and are 
#'     retained as a data quality check.)
#'   }
#'   \item{`Azimuth`}{
#'    Numeric vector of the 
#'    [horizontal angle from north of the direction of the 
#'    transmitter from the reference point](https://en.wikipedia.org/wiki/Azimuth).
#'   }
#'   \item{`Licensee`}{
#'     Character vector of the name of the Licensee / 
#'     Permittee for that entry. 
#'   }
#'  }  
#' 
#' @export FCCquery
#'
#' @examplesIf interactive()
#' # These examples have taken over a minute to complete
#' # because of long delays in getting a response from 
#' # fcc.gov:  
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
  st <- proc.time()
#  QuerTxt <- htm2txt::gettxt(query)
#  QuerLines <- readLines(query) #timed out; no timout parameter
  Query <- xml2::read_html(query)
#  QuerXML <- XML::readHTMLTable(query)
  et <- (proc.time()-st)
  # following 
# https://infatica.io/blog/web-scraping-with-r-and-rvest/
#  Quernodes <- rvest::html_elements(Query)
#  QuerTxt <- htm2txt::htm2txt(Query)
  QueTxt <- rvest::html_text(Query)
  createQDF <- function(n=0){
    queryDF <- data.frame(Call=character(n), Channel=numeric(n), 
                        Class=character(n), Service=character(n), 
                        Frequency=numeric(n), FrequencyUnits=character(n), 
                        Status=character(n), City=character(n), 
                        State=character(n), Country=character(n), 
                        FileNumber=character(n), FacilityID=character(n), 
                        ERP=numeric(n), ERPunits=character(n), 
                        HAAT=numeric(n), 
                        Dist_km=numeric(n), Dist_kmUnits=character(n), 
                        Dist_mi=numeric(n), Dist_miUnits=character(n), 
                        Azimuth=numeric(n), Licensee=character(n) )
    attr(queryDF, 'query') <- query
    attr(queryDF, 'query_time') <- et
    queryDF
  }
  queryDF <- createQDF()
## 
## 2. Response should be a character vector of length 1. Check.
##
  WarnList <- function(msg, QueRtn=QueTxt, variable='all', 
                   fieldNumInQuery='all', row='all'){
    list(message=msg, row=row, variable=variable, 
         fieldNumInQuery=fieldNumInQuery, 
         QuerySegment=QueRtn)
  }
  if((lenQT <- length(QueTxt))>1){
    msg <- paste0('PROBLEM: ', 
        'html_text(read_html(query)) is a list of length ', 
        lenQT, '. FCCquery assumes it is length 1. ', 
        'Returning an empty data.frame with the ', 
        'response as attribute "entriesWFmtErrors".')
    warning(msg)
    attr(queryDF, 'entriesWFmtErrors') <- 
          WarnList(msg)
    return(queryDF)
  } 
  if(lenQT<1){
    msg <- paste0('PROBLEM: ', 
                  'html_text(read_html(query)) is a list of length 0.', 
                  ' Returning an empty data.frame with the ', 
                  'response as attribute "entriesWFmtErrors".')
    warning(msg)
    attr(queryDF, 'entriesWFmtErrors') <- 
          WarnList(msg)
    return(queryDF)
  }
## 
## 3.  Lines shoud be  separated by \n and fields by |.
##  
  QueLines <- strsplit(QueTxt, '\n')[[1]]
  nQue <- length(QueLines)
  # Guess that fields are separated by "|"
  QueTbl <- strsplit(QueLines, '\\|')
  # are they all the same length? 
  QueTn <- sapply(QueTbl, length)
  QueT <- table(QueTn)
  if(length(QueT)>1){
    print(QueT)
    msg <- paste0('Not all rows have 39 fields. ', 
                  'Unparseable rows returned as ', 
                  'attribute "entriesWFmtErrors".')
    warning(msg)
  }
## 
## 4.  Fill queryDF where length(QueTbl[[i]])=41
##
  queryDF <- createQDF(nQue)
  nQi1 <- 0
  nQi16 <- 0
  nQi18 <- 0
  entriesWFmtErrors <- character(nQue)
  for(i in 1:nQue){
    iGood <- FALSE
    if(QueTn[i] == 39){
      iGood <- TRUE
      Qi <- trimws(QueTbl[[i]])
      if(Qi[1]!= ''){
        iGood <- FALSE
        msg <- paste0('Row ', i, ' should start with a blank.', 
                      ' Instead:', Qi[1])
        attr(queryDF, 'entriesWFmtErrors') <- 
          WarnList(msg, QueRtn=QueLines[[i]], variable='all', 
                   fieldNumInQuery='all', row=i)
        if(nQi1<1){
          warning(msg)
        }
        nQi1 <- nQi1+1
      }
      queryDF[i, 'Call'] <- Qi[2]
      freq <- strsplit(Qi[3], ' ')[[1]]
      queryDF[i, 'Frequency'] <- as.numeric(freq[1])
      queryDF[i, 'FrequencyUnits'] <- utils::tail(freq, 1)
      queryDF[i, 'Service'] <- Qi[4]
      queryDF[i, 'Channel'] <- as.numeric(Qi[5])
      queryDF[i, 'Class'] <- Qi[8]
      queryDF[i, 'Status'] <- Qi[10]
      queryDF[i, 'City'] <- Qi[11]
      queryDF[i, 'State'] <- Qi[12]
      queryDF[i, 'Country'] <- Qi[13]
      queryDF[i, 'FileNumber'] <- Qi[14]
      ERP. <- strsplit(Qi[15], ' ')[[1]]
      queryDF[i, 'ERP'] <- as.numeric(ERP.[1])
      queryDF[i, 'ERPunits'] <- utils::tail(ERP., 1)
      if(Qi[15] != Qi[16]){
        iGood <- FALSE
        if(nQi16<1){
          msg <- paste0('Fields 15 and 16 should both equal freq. ',
                      'Instead\n[15]=', Qi[15], ';\n', 
                      '[16]=', Qi[16], ' for row ', i)
          warning(msg)
          attr(queryDF, 'entriesWFmtErrors') <- 
            WarnList(msg, QueRtn=QueLines[[i]], 
                     variable='ERP, ERPunits', 
                     fieldNumInQuery=15:16, row=i)
        }
        nQi16 <- nQi16+1
      }
      queryDF[i, 'HAAT'] <- as.numeric(Qi[17])
      if(Qi[17] != Qi[18]){
        iGood <- FALSE
        if(nQi18<1){
          msg <- paste0('Fields 17 and 18 should both equal HAAT. ',
                        'Instead\n[17]=', Qi[17], ';\n', 
                        '[18]=', Qi[18], ' for row ', i)
          warning(msg)
          attr(queryDF, 'entriesWFmtErrors') <- 
            WarnList(msg, QueRtn=QueLines[[i]], 
                     variable='HAAT', 
                     fieldNumInQuery=17:18, row=i)
        }
        nQi18 <- nQi18+1
      }
      queryDF[i, 'FacilityID'] <- as.numeric(Qi[19])
      queryDF[i, 'NS'] <- Qi[20]
      queryDF[i, 'dlat2'] <- as.numeric(Qi[21])
      queryDF[i, 'mlat2'] <- as.numeric(Qi[22])
      queryDF[i, 'slat2'] <- as.numeric(Qi[23])
      queryDF[i, 'EW'] <- Qi[24]
      queryDF[i, 'dlon2'] <- as.numeric(Qi[25])
      queryDF[i, 'mlon2'] <- as.numeric(Qi[26])
      queryDF[i, 'slon2'] <- as.numeric(Qi[27])
      queryDF[i, 'Licensee'] <- Qi[28]
      distKM <- strsplit(Qi[29], ' ')[[1]]
      queryDF[i, 'Dist_km'] <- as.numeric(distKM[1])
      queryDF[i, 'Dist_kmUnits'] <- utils::tail(distKM, 1)
      distMI <- strsplit(Qi[30], ' ')[[1]]
      queryDF[i, 'Dist_mi'] <- as.numeric(distMI[1])
      queryDF[i, 'Dist_miUnits'] <- utils::tail(distMI, 1)
      Azmth <- strsplit(Qi[31], ' ')[[1]]
      queryDF[i, 'Azimuth'] <- as.numeric(Azmth[1])
    }
    if(!iGood)entriesWFmtErrors[i] <- QueLines[i]
  }
  NentriesWFmtE <- which(entriesWFmtErrors!='')
  if(length(NentriesWFmtE)>0){
    attr(queryDF, 'entriesWFmtErrors') <- entriesWFmtErrors
    warning('entries with formatting errors =', 
            paste(NentriesWFmtE, collapse=', '))
  } else {
    attr(queryDF, 'entriesWFmtErrors') <- 'No parsing problems detected.'
  }
  class(queryDF) <- c('FCCquery', 'data.frame')   
  ##
  ## 4.  Return
  ##
  queryDF
}
