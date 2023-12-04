#' Plot a representation of network
#'
#' @description plot network on a map
#' @param network An igraph graph including at least longitude, latitude and type attributes. Expected names for those attributes are respectively lon, lat and type.
#' @param title Title of the map
#' @param edge.attr string. Default is NULL
#' @param movements.type.lab string. Default is "animType"
#' @param premises.type.lab string. Default is "type"
#' 
#' @return return a plot
#' @examples FIX ME
#'
#' @importFrom raster getData
#' @importFrom viridis viridis
#' @importFrom broom tidy
#' @importFrom intergraph asIgraph
#' @importFrom plyr ddply
#' @importFrom plyr "."
#' @importFrom RColorBrewer brewer.pal
#' @importFrom igraph as_edgelist
#' @importFrom igraph get.edge.attribute
#' @importFrom network network
#' @importFrom ggplotify as.ggplot
#'
#' @export

vis_swineNet <- function(networktoplot = g,
                        title = NA,
                        movements.type.lab = "animType",
                        premises.type.lab = "type") {

  ### check inputs ###
  if(class(networktoplot) != "igraph"){
    if(class(networktoplot) == "network")
      networktoplot <- intergraph::asIgraph(networktoplot) else
      stop("networktoplot class must be igraph. Consider using intergraph to transform.")}

    g <- networktoplot
    
  ### Remove loops and duplicates ###
    if(is.null(igraph::get.vertex.attribute(g)$name))
  V(g)$name <- igraph::get.vertex.attribute(g)$vertex.name
    
  fromto <- igraph::as_edgelist(g, names = T) %>% data.frame
  colnames(fromto)[1:2] <- c("from","to")

  fromto[,"movements.type"] <- igraph::get.edge.attribute(g, movements.type.lab)

  # as_data_frame(g, what = c("edges", "vertices", "both"))

  farms <- data.frame(site = igraph::get.vertex.attribute(g)$name,
                      type = igraph::get.vertex.attribute(g)[premises.type.lab])

  fromto$fromtype <- farms[match(fromto[,1],farms$site), "type"]
  fromto$totype <- farms[match(fromto[,2],farms$site), "type"]
  fromto <- fromto[!is.na(fromto[,"fromtype"]),]
  fromto <- fromto[!is.na(fromto[,"totype"]),]
  fromto <- fromto[,c("movements.type", "fromtype", "totype")]
  fromto <- plyr::ddply(fromto, plyr::`.`(fromtype,totype,movements.type), nrow)
  farmsnb <- plyr::ddply(farms,plyr::`.`(type),nrow)
  
  
    net <- network::network(fromto, matrix.type = "edgelist",loops=T, multiple = T, directed=TRUE)
    network::set.edge.attribute(net, "movements.type", fromto$movements.type)
    network::set.edge.attribute(net, "movements.nb", fromto$V1)
    network::set.vertex.attribute(net, "premise.nb", farmsnb[match(network::network.vertex.names(net),farmsnb$type), "V1"])
    
    pal <- RColorBrewer::brewer.pal(6,"Paired")
    
    do_work <- function(net){
      ggplotify::as.ggplot(function(){
        plot(net,
             vertex.col = viridis::viridis(length(network::get.vertex.attribute(net,"vertex.names"))),
             label = network::network.vertex.names(net),
             edge.lwd=20*network::get.edge.attribute(net,"movements.nb")/max(network::get.edge.attribute(net,"movements.nb")),
             edge.col=pal[factor(network::get.edge.attribute(net,"movements.type"))],
             vertex.cex=10*(network::get.vertex.attribute(net,"premise.nb")/max(network::get.vertex.attribute(net,"premise.nb"))),
             mode = "circle");
        legend("topleft",bty = "n", lty=1, cex=0.8,
               legend=levels(factor(network::get.edge.attribute(net,"movements.type"))),col=pal,border=NA)})
    }
    
    p <- do_work(net)  
      
      if(!is.na(title))
        p <-  p + ggtitle(title) + theme(
          plot.title = element_text(color="black", size=14, face="bold.italic", hjust = 0.5)
        )
    
    return(p)
}
