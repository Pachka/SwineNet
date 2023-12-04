<<<<<<< HEAD
# #' Add time variables FIX ME Imap is not available anymore
=======
# #' Add time variables FIX ME outdated package imap
>>>>>>> 42f1e3243b0b136dd72c76edb43e40e46dfee4bb
# #'
# #' @description FIX ME - by Peter Rosenmai on 30 Jan 2014 https://eurekastatistics.com/calculating-a-distance-matrix-for-geographic-points-using-r/
# #' @param df FIX ME
# #' @param formatDate FIX ME
# #' @return FIX ME
# #' @examples
# #' sites <- data.frame(siteID = letters[1:3], lat = c(5.1,2.2,3.4), lon = c(25.1,72.2,53.4))
# #' GeoDistanceInMetresMatrix(sites)
# #'
# #' @importFrom Imap gdist
# #'
# #' @export
# #'
#
# GeoDistanceInMetresMatrix <- function(df.geopoints, siteID = "siteID"){
#   # Returns a matrix (M) of distances between geographic points.
#   # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
#   # (df.geopoints$lat[j], df.geopoints$lon[j]).
#   # The row and column names are given by df.geopoints$name.
#
#
#   ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
#     # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
#     # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
#
#     if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
#     if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
#     else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
#     else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
#     m[tri] <- t(m)[tri]
#     return(m)
#   }
#
#   GeoDistanceInMetres <- function(g1, g2){
#     # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
#     # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
#     # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
#     # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
#     # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
#     DistM <- function(g1, g2){
#       require("Imap")
#       return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
#     }
#     return(mapply(DistM, g1, g2))
#   }
#
#   n.geopoints <- nrow(df.geopoints)
#
#   # The index column is used to ensure we only do calculations for the upper triangle of points
#   df.geopoints$index <- 1:n.geopoints
#
#   # Create a list of lists
#   list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
#
#   # Get a matrix of distances (in metres)
#   mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
#
#   # Set the row and column names
#   rownames(mat.distances) <- df.geopoints[,siteID]
#   colnames(mat.distances) <- df.geopoints[,siteID]
#
#   return(mat.distances)
# }
