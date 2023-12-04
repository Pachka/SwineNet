#' Calculate network centralities
#'
#' @description  calculate basic network centralities
#' @param network An igraph graph.
#' @param distMatrix matrix. Default is false. If a distance matrix is provided the function calculates the median distance of movements.
#' @return return a vector listing network centralities
#' @examples
#' network_centralities()
#'
#' @importFrom igraph simplify
#' @importFrom igraph vertex.attributes
#' @importFrom igraph get.edgelist
#' @importFrom igraph delete.vertices
#' @importFrom igraph V
#' @importFrom igraph degree
#' @importFrom igraph similarity
#' @importFrom igraph vcount
#' @importFrom igraph ecount
#' @importFrom igraph clusters
#' @importFrom igraph betweenness
#' @importFrom igraph closeness
#' @importFrom igraph graph.density
#' @importFrom igraph diameter
#' @importFrom igraph assortativity_degree
#' @importFrom igraph reciprocity
#' @importFrom igraph transitivity
#' @importFrom igraph average.path.length
#' @importFrom igraph reciprocity
#' @importFrom igraph list.edge.attributes

#' @importFrom stats median
#'
#' @export

network_centralities <- function(network = g, include.distance = FALSE, distance.matrix = NA, distance.att = NA) {

  if((include.distance & identical(distance.matrix,NA) & identical(distance.att,NA) ) | 
     (include.distance & identical(distance.matrix,NA) & !(distance.att %in% list.edge.attributes(network)))| 
     (include.distance & identical(distance.att,NA) & !("matrix" %in% (distance.matrix %>% class))))
    stop("If distance is included, format must be a matrix or a string identifying an edge attribute of the network")
  
  if(!identical(distance.matrix,NA) & !identical(distance.att,NA))
    stop("If distance is included, it can be calculated from either a distance matrix or an edges attribute, but not both.")
  
  if(!identical(distance.matrix,NA) & !identical(colnames(distance.matrix), rownames(distance.matrix)))
    stop("The distance matrix must be symetric with identical colnames and rownames")
  
  if(!identical(distance.matrix,NA) & FALSE %in% (igraph::get.vertex.attribute(network, "name") %in% rownames(distance.matrix)))
    stop(sum(!(igraph::get.vertex.attribute(network, "name") %in% rownames(distance.matrix))), " sites are missing from the distance matrix:\n")
         
  
  
    
  # Remove loops and duplicates
  g <- simplify(network, remove.multiple=T, remove.loops = TRUE)

  # is.loop(g, eids=E(g))
  # count.multiple(g, eids=E(g))

  if (!is.null(dim(distMatrix))) {
    Mov <- get.edgelist(g)
    dist <- apply(Mov, 1, function(x) {
      if (x[1] %in% colnames(distMatrix) & x[2] %in% colnames(distMatrix))
        distMatrix[x[1], x[2]]
    })
  }

  #suppression boucles et isolates
  g <- delete.vertices(g, V(g)[degree(g)==0])
  # g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

  ##################################
  ### Analysis of static network ###
  ##################################

  # vertex_attr_names(g)
  # edge_attr_names(g)

  output <- NULL
  # number of vertices
  output["vcount"] <- vcount(g)
  output["count.isolates"] <- length(V(g)[degree(g)==0])
  # number of edges
  output["ecount"] <- ecount(g)

  output["nClusters"] <- clusters(g)$no
  output["maxClusterSize"] <- max(clusters(g)$csize)
  output["medClusterSize"] <- median(clusters(g)$csize)
  output["99thpercentileClusterSize"] <- clusters(g)$csize %>% quantile(., probs = 0.95)

  output["avg.degree"] <- mean(degree(g, mode = "all"))
  output["med.degree"] <- median(degree(g, mode = "all"))
  output["med.indegree"] <- median(degree(g, mode = "in"))
  output["med.outdegree"] <- median(degree(g, mode = "out"))
  output["avg.betweeness"] <- mean(betweenness(g))
  output["avg.closeness"] <- mean(closeness(g))
  output["graph.density"] <- graph.density(g)
  output["diameter"] <- diameter(g)
  output["assortativity"] <- assortativity_degree(g)
  output["reciprocity"] <- reciprocity(g, mode="ratio")
  output["transitivity"] <- transitivity(g)
  output["average.path.length"] <- average.path.length(g) # mean_distance(g)
  output["reciprocity"] <- reciprocity(g, ignore.loops=TRUE)

  jin <- similarity(g, mode = "all",loops = FALSE, method = "jaccard")
  output["JaccquarSimilCoefNull"] <- round(length(which(jin==0))/(vcount(g)^2)*100,2)

  if(include.distance == TRUE){
    # FIX ME: use GeoDistanceInMetres to directly calculate distances from lat, lon and vertex.names attributes.
    
    if("matrix" %in% (distance.matrix %>% class)){
      dist <- get.edgelist(g) %>% apply(.,1,function(x) distance.matrix[x[1], x[2]])
      output[c("distance.2.5%","distance.med","distance.97.5%")] <- quantile(dist, probs = c(0.025,0.5,0.975))
      }
    if(!identical(distance.att,NA)) output[c("distance.2.5%","distance.med","distance.97.5%")] <- get.edge.attribute(network)[[distance.att]] %>% as.character %>% as.numeric  %>% quantile(., probs = c(0.025,0.5,0.975))
      }

  output
}
