# This goes in R/data.R

#' @title sites
#' @description Fictive sites dataset
#' @format A data frame with 200 rows and 10 variables:
#' \describe{
#'   \item{\code{siteID}}{double, identification of locations}
#'   \item{\code{company}}{double, identification of company}
#'   \item{\code{type}}{double, type of farm ("FA": farrow,  "FF": farrow-to-finish,  "FI": finish,
#'   "FPW": farrow-to-postwean,  "MU": multiplier,  "NU": nucleus,  "PW":postwean,  "PWF": postwean-to-finish)}
#'   \item{\code{outdoor}}{logical, does the site include outdour rearing system?}
#'   \item{\code{BRS}}{double, Batch rearing system of the site} 
#'   \item{\code{lon}}{double, Longitudinal coordinates} 
#'   \item{\code{lat}}{double, Latitude coordinates} 
#'   \item{\code{nbRP}}{double, Maximal number of reproductive sows in the farm} 
#'   \item{\code{nbPW}}{double, Maximal number of growing pigs in the farm} 
#'   \item{\code{nbFat}}{double, Maximal number of barrows in the farm} 
#'}
"sites"

#' @title movements
#' @description Fictive movements dataset 
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{\code{from}}{double origin site identification}
#'   \item{\code{to}}{double destination site identification}
#'   \item{\code{MType}}{double type of movement (loading: 'C' or unloading: 'D')}
#'   \item{\code{year}}{double year of the event}
#'   \item{\code{semester}}{double semester of the event} 
#'   \item{\code{semester}}{double month of the  event} 
#'}
"movements"

#  FIX ME Add doc for distance matrix
#  FIX ME create relevant movements according to sites type and animal type 
# from <- sample(sites$siteID, 2000,  replace = T)
# to   <- sample(sites$siteID, 2000,  replace = T)
# date <- sample(seq(as.Date('2019/01/01'), as.Date('2020/01/01'), by="day"), 2000,  replace = T)
# MType <- sample(c("PG","GP","RP","BR"), 2000,  replace = T) # piglets, growing pigs, reproducive sows, barrows
# listMovements <- data.frame(from=from,ro=to,date=date,Mtype=Mtype)
# save(listMovements, file = "SwineNet/data/listMovements.rda")