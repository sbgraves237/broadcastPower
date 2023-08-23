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
#' @param ... other arguments for consistency with 
#'        the generic `print` function.  Not used.   
#' @return `file`
#' 
#' @importFrom openxlsx2 wb_workbook wb_add_worksheet 
#' @importFrom openxlsx2 wb_add_data_table wb_save
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
#' @export 
print.FCCquery <- function(x, 
      file = paste0(deparse(substitute(x)), '.xlsx'), 
      ...){
##
## 1.  Write the data.frame
##  
  xNm <- deparse(substitute(x))
#  fileXLC <- paste0(xNm, 'XLConnect.xlsx')
#  XLConnect::writeWorksheetToFile(fileXLC, x, 'data.frame')
#  
#  require(openxlsx2)
#  fileOpen <- paste0(xNm, 'Openxlsx2.xlsx')
# write_xlsx accepts a named list as it first argument   
#  write_xlsx(x, fileOpen)
#  header row is shaded with XLConnect but not with openxlsx2
#  This ignores the attributes and puts the data in 'Sheet 1'
#  write_xlsx(x, fileOpen, sheet='data.frame')
#  Warning: unused arguments (sheet)   
#  X <- list(data.frame=x)
# openxlsx2 seems to provide more control starting with a "workbook"  
  xdf <- x
  wbO2 <- wb_workbook(title=xNm)
  class(xdf) <- 'data.frame'
  wbO2$add_worksheet(sheet='data.frame')
  wbO2$add_data_table(sheet='data.frame', x=xdf,
                      row_names = TRUE)
#  wb_save(wbO2, fileOpen) # works but premature
#
#  fileO2 <- paste0(xNm, 'Oxlsx2.xlsx')
##
## 2.  Write the query string
##  
  query <- attr(x, 'query')
#  XLConnect::writeWorksheetToFile(fileXLC, query, 'query')
#  
#  write_xlsx(list(query=query), fileOpen)
#  This overwrites the existing fileOpen, 
#  replacing it with an xlsx file with 
#  with a single sheet "query"
#  X['query'] <- query
  wbO2$add_worksheet(sheet='query')
  wbO2$add_data(sheet='query', x=query)
#  wb_save(wbO2, fileOpen) # works but premature
## 
## 3.  Write the query time
##  
  et <- attr(x, 'query_time')
#  XLConnect::writeWorksheetToFile(file, et, 'query_time')
#  X[['query_time']] <- as.data.frame(et)
#  write_xlsx(X, fileOpen)
  wbO2$add_worksheet(sheet='query_time')
  etDF <- as.data.frame(et)
  names(etDF) <- 'seconds'
  wbO2$add_data_table(sheet='query_time', 
                    x=etDF, row_names=TRUE)
#  wb_save(wbO2, fileOpen) # works but premature
##
## 4.  Write entriesWFmtErrors,
##     one tab for each non-null component
##  
#  4.1.  Get the list of entries with format errors  
  eWFE <- attr(x, 'entriesWFmtErrors')
  NeWFE <- length(eWFE)
#  4.2.  errorRow001, ... errorRow176 
  NmEwfe <- names(eWFE)
  for(i in 1:NeWFE){
#    XLConnect::writeWorksheetToFile(file, eWFE[[i]], NmEwfe[i])
    wbO2$add_worksheet(sheet=NmEwfe[i])
    UNeWFEi <- unlist(eWFE[[i]])
    errorRowi <- as.data.frame(UNeWFEi)
    wbO2$add_data_table(sheet=NmEwfe[i], 
                x=errorRowi, row_names=TRUE)
  }
##
## 5.  Done
##
  wb_save(wbO2, file)
  cat('Object ', xNm, 'written to the working directory = ', 
      getwd() )
  invisible(xNm)
}
