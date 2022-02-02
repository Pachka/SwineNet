#' Add time variables
#'
#' @description FIX ME
#' @param df FIX ME
#' @param formatDate FIX ME
#' @return FIX ME
#' @examples
#' data(listMovements)
#' timewindows(listMovements, formatDate = "%Y-%m-%d") %>% head
#' @importFrom data.table ":="
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' 
#' @export

timewindows <- function(df, formatDate = "%d/%m/%Y"){
  if(!"date" %in% colnames(df))stop(cat("df should contain a \"date\" column"))
  if(!"Date" %in% (df$date %>% class)) stop(cat("\"date\" column should be a Date"))
  turnBacktoDF <- FALSE
  
  if(!is.data.table(df)){
    turnBacktoDF <- TRUE
    setDT(df)
  } else {
    df[, `:=`(
      year = format(as.Date(date, format=formatDate),"%Y"),
      month = format(as.Date(date, format=formatDate),"%m-%Y"),
      semester = format(as.Date(date, format=formatDate),"%m") %>% as.character %>% as.numeric %>% replace(. < 7,1)  %>% replace(. > 6,2)
    )]
    
    df[, `:=`(
      semester = year %>% paste0(., "_") %>% paste0(., semester)
    )]
  }
  
  if(turnBacktoDF) df %<>% as.data.frame
  
  return(df)
}
