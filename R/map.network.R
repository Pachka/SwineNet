#' Map network
#'
#' @description plot network on a map
#' @param network An igraph graph including at least longitude, latitude and type attributes. Expected names for those attributes are respectively lon, lat and type.
#' @param country Vector including country names and the ISO codes used by the function getData of raster package to download mapbase from 'GADM'.
#' @param path.rasters Path to save rasters of mapbase.
#' @param title Title of the map
#' @param topfeature String defining the top feature on map: path (edges) or point (nodes).
#' @param path.color String defining path color.
#' @param path.size String defining path size.
#' @param point.size.coef Numerical. Defining the upper scale for point size. Default is 6.
#' @return return a plot
#' @examples
#' map.network()
#'
#' @importFrom raster getData
#' @importFrom igraph simplify
#' @importFrom igraph vertex.attributes
#' @importFrom igraph get.edgelist
#' @importFrom igraph degree
#' @importFrom broom tidy
#' @importFrom intergraph asIgraph
#'
#' @export

map.network <- function(network, # at least include lon and lat
                        country,
                        premisescolours = NA,
                        path.rasters = '',
                        title = "network map",
                        topfeature = "point",
                        path.color = "grey",
                        path.size = 0.03,
                        point.size.coef = 6) {



  # network = intergraph::asIgraph(g)
  # country = c("FR")
  # path.rasters = "../data/raster/"
  # title = "French Swine network - 2nd semester 2019"
  # topfeature = "path"
  # path.color = "grey"
  # path.size = 0.03
  # point.size.coef = 1


  ### check inputs ###
  if(class(network) != "igraph") stop("network class must be igraph. Consider using intergraph to transform.")
  if(!topfeature %in% c("point","path")) stop("wrong topfeature")
  if(class(path.size) != "numeric") stop("path size must be numeric")
  if(class(point.size.coef) != "numeric") stop("path size must be numeric")

  ### Remove loops and duplicates ###
  g <- igraph::simplify(network, remove.multiple=T, remove.loops = TRUE)

  # Download background
  for(mapbase in country)
    assign(mapbase,raster::getData(name="GADM", country=mapbase, level=0, download = TRUE, path = path.rasters))

  newmap <- get(country[1])

  if(length(country) > 1)
    for(mapbase in country[2:length(country)])
      newmap <- rbind(newmap, get(mapbase), makeUniqueIDs = TRUE)

  newmap_df <- broom::tidy(newmap)

  # Extract premises and attributes from igraph
  listoffarms <- igraph::vertex.attributes(g)[grepl("name", igraph::vertex.attributes(g) %>% names)]
  longitudes<- igraph::vertex.attributes(g)$lon %>% as.character %>% as.numeric
  latitudes <- igraph::vertex.attributes(g)$lat %>% as.character %>% as.numeric
  farmsCoordinates <- data.frame(listoffarms,longitudes,latitudes)
  
  if(!is.na(premisescolours)){
    type <- igraph::vertex.attributes(g)[premisescolours]
    farmsCoordinates <- data.frame(listoffarms,longitudes,latitudes, type)}
  
  
  # Extract movements from igraph
  movements <- as.data.frame(igraph::as_edgelist(g, names = TRUE))
  colnames(movements)[1:2] <- c("from",'to')
  
  # FIX ME: the option should be related to the loaded packages (igraph and/or network)
  if(class(movements$from) == "character"){
    movements[,c("fromlon", "fromlat")] <- farmsCoordinates[match(movements$from,farmsCoordinates$name), c("longitudes", "latitudes")]
    movements[,c("tolon", "tolat")] <- farmsCoordinates[match(movements$to,farmsCoordinates$name), c("longitudes", "latitudes")]
  }else{
  movements[,c("fromlon", "fromlat")] <- farmsCoordinates[movements$from, c("longitudes", "latitudes")]
  movements[,c("tolon", "tolat")] <- farmsCoordinates[movements$to, c("longitudes", "latitudes")]
  }
  
  movements <- unique(movements)
  if(length(which(movements$fromlon == movements$tolon))!=0){
    message(paste(length(which(movements$fromlon == movements$tolon)), "movements with same coordinates for origin and destination where removed"))
    movements <- movements[-which(movements$fromlon == movements$tolon),]
      }

  farmsCoordinates$Degree <- igraph::degree(g)
  farmsCoordinates$Degree <- farmsCoordinates$Degree*point.size.coef/max(farmsCoordinates$Degree)
  
  if(!is.na(premisescolours)){
  if(topfeature == "path")
    p <- ggplot()  +
    geom_path(data = newmap_df, aes(x = long, y = lat, group =group)) +
    labs(title = title) +
    geom_point(data = farmsCoordinates,
               aes(x = longitudes, y = latitudes, fill = type, size = Degree),
               shape=21, color = "black") +
    scale_size(range = c(1,max(farmsCoordinates$Degree)),guide = "none") +
    geom_curve(data = movements,
             aes(x = fromlon, y = fromlat, xend = tolon, yend = tolat),
             colour = path.color,
             size = path.size,
             curvature = 0.05,
             alpha = 0.5)


  if(topfeature == "point")
    p <- ggplot()  +
    geom_path(data = newmap_df, aes(x = long, y = lat, group =group)) +
    labs(title = title) +
    geom_curve(data = movements,
               aes(x = fromlon, y = fromlat, xend = tolon, yend = tolat),
               colour = path.color,
               size = path.size,
               curvature = 0.05,
               alpha = 0.5) +
    geom_point(data = farmsCoordinates,
               aes(x = longitudes, y = latitudes, fill = type, size = Degree),
               shape=21, color = "black") +
    scale_size(range = c(1,max(farmsCoordinates$Degree)), guide = "none")
  } else {
    if(topfeature == "path")
      p <- ggplot()  +
        geom_path(data = newmap_df, aes(x = long, y = lat, group =group)) +
        labs(title = title) +
        geom_point(data = farmsCoordinates,
                   aes(x = longitudes, y = latitudes, size = Degree),
                   shape=21, color = "black") +
        scale_size(range = c(1,max(farmsCoordinates$Degree)),guide = "none") +
        geom_curve(data = movements,
                   aes(x = fromlon, y = fromlat, xend = tolon, yend = tolat),
                   colour = path.color,
                   size = path.size,
                   curvature = 0.05,
                   alpha = 0.5)
    
    
    if(topfeature == "point")
      p <- ggplot()  +
        geom_path(data = newmap_df, aes(x = long, y = lat, group =group)) +
        labs(title = title) +
        geom_curve(data = movements,
                   aes(x = fromlon, y = fromlat, xend = tolon, yend = tolat),
                   colour = path.color,
                   size = path.size,
                   curvature = 0.05,
                   alpha = 0.5) +
        geom_point(data = farmsCoordinates,
                   aes(x = longitudes, y = latitudes, size = Degree),
                   shape=21, color = "black") +
        scale_size(range = c(1,max(farmsCoordinates$Degree)), guide = "none")
  }

  p
}
