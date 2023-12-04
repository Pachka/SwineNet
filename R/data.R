# This goes in R/data.R

#' @title sites
#' @description Fictive premises dataset
#' @format A data frame with 200 rows and 10 variables:
#' \describe{
#'   \item{\code{siteID}}{double identification of premises}
#'   \item{\code{farmGroup}}{double identification of company}
#'   \item{\code{type}}{double type of premise}
#'   \item{\code{freerange}}{double does the premise include freerange reasing}
#'   \item{\code{BRS}}{double Batch rearing system of the premise} 
#'   \item{\code{lon}}{double Longitudinal coordinates} 
#'   \item{\code{nbRP}}{double Maximal number of reproductive sows in the farm} 
#'   \item{\code{nbPW}}{double Maximal number of growing pigs in the farm} 
#'   \item{\code{nbFat}}{double Maximal number of barrows in the farm} 
#'}
"sites"

#' @title movements
#' @description Fictive movements dataset 
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{\code{from}}{double origin premise identification}
#'   \item{\code{to}}{double destination premise identification}
#'   \item{\code{MType}}{double type of movement (loading: 'C' or unloading: 'D')}
#'   \item{\code{year}}{double year of the event}
#'   \item{\code{semester}}{double semester of the event} 
#'   \item{\code{semester}}{double month of the  event} 
#'}
"movements"

#  FIX ME create relevant movements according to premises type and animal type 
# from <- sample(sites$siteID, 2000,  replace = T)
# to   <- sample(sites$siteID, 2000,  replace = T)
# date <- sample(seq(as.Date('2019/01/01'), as.Date('2020/01/01'), by="day"), 2000,  replace = T)
# MType <- sample(c("PG","GP","RP","BR"), 2000,  replace = T) # piglets, growing pigs, reproducive sows, barrows
# movements <- data.frame(from=from,ro=to,date=date,Mtype=Mtype)
# save(movements, file = "SwineNet/data/movements.rda")
