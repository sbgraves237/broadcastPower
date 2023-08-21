#' Print method for FCCquery 
#'
#' Print method for objects of class FCCquery.
#' This creates a file in the working directory
#' in MS Excel `.xlsx` workbook format.  
#' The [data.frame()] is the first `data.frame` tab.
#' The second tab contains the `query` string.  
#' The third tab contains the `query_time` in 
#' the format of an object of class [proc.time()] 
#' giving the time required to extract the 
#' information from `fcc.gov`. See @details for
#' a discussion of the `entriesWFmtErrors` tab.  
#' 
#' @details
#' The information in the `entriesWFmtErrors` tab
#' depends on the extent to which the data obtained
#' from `fcc.gov` seemed to be in the standard 
#' format.  If the current algorithm failed to parse 
#' any of the information returned, all that 
#' information (if any) is stored in this 
#' `entriesWFmtErrors` tab.  If everything seemed
#' consistent with expectations, this tab will be 
#' blank.  Otherwise, it will contain a list of 
#' any and all rows that confused the algorithm 
#' with an explanation about what the algorithm did
#' not understand.  
#' 
#' @param x an object of class `FCCquery`
#' @param file = paste0(deparse(substitute(x)), '.xlsx')
#' @return `file`
#' 
#' @examplesIf interactive()
#' # These examples have taken over a minute to complete
#' # because of long delays in getting a response from 
#' # fcc.gov:  
#' # All FM stations within 80 km of FCC headquarters 
#' # in Washington, DC
#' FCCdat1 <- FCCquery()
#' print(FCCdat1)
#'
#' @export print.FCCquery
print.FCCquery <- function(x, 
      file = paste0(deparse(substitute(x)), '.xlsx')){
  XLConnect::writeWorksheetToFile(file, x, 
                    'data.frame')
  query <- attr(x, 'query')
  XLConnect::writeWorksheetToFile(file, query, 
                                 'query')
  et <- attr(x, 'query_time')
  XLConnect::writeWorksheetToFile(file, et, 
                      'query_time')
  eWFE <- attr(x, 'entriesWFmtErrors')
  XLConnect::writeWorksheetToFile(file, eWFE, 
          'entriesWFmtErrors')
  
  
  
}
