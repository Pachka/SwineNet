#' Generate network
#'
#' @description generate network and subnetworks
# using this function, movements can be splited by year, semester or month
# movements dataset must at least contain three columns: from, to, year
#' @param movements A dataframe listing the movements defined by at least an origin ('from' column) and a destination ('to' column).
#' @param premises A dataframe listing the origin and destination sites defined by at least the site identifier (1st column) and the nodal attributes.
#' @param splitByEdges FIX ME - A vector or characters including one or several time periods amoung year, semester and month. FALSE is leading to the production of the complete network.
#' @param splitByVertex FIX ME - A vector or characters including one or several time periods amoung year, semester and month. FALSE is leading to the production of the complete network.
#' @param save.network logical. Default is TRUE. Produce an '.Rda' file for each generated network and save it under the declared path and prefixe.
#' @param path.save String defining the location file to save the networks.
#' @param prefixe String defining the prefixe if output files.
#' @return return a list of networks
#' @examples
#' data(listPremises)
#' data(listMovements)
#' generate_network(movements = listMovements, premises = listPremises, save.network = FALSE)
#'
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph set_edge_attr
#' @importFrom igraph delete.vertices
#' @importFrom igraph V
#' @importFrom igraph degree
#' @importFrom igraph simplify
#'
#' @export

generate_network <- function(movements,
                             premises,
                             splitByEdges = NA,
                             splitByVertex = NA,
                             save.network = TRUE,
                             path.save = getwd(),
                             prefixe = "network",
                             remove.duplicated.edges = F,
                             remove.loop.edges = T) {

  # Check inputs
  if (!inherits(movements, "data.frame") | !identical(movements %>% colnames %>% .[1:2], c("from", "to")))
    stop("movements object should be a data.frame with 'from' and 'to' as first columns.")
  if (!inherits(premises, "data.frame") | FALSE %in% (c(movements$from, movements$to) %in% premises[,1]))
    stop("premises object should be a data.frame where the first column contains all ids in from and to columns of movement object.")
  
  if (!(splitByEdges %>% class %in% c("logical", "character")))
    stop("splitByEdges object should be vector of characters or logical values.")
  if (!(splitByVertex %>% class %in% c("logical", "character")))
    stop("splitByEdges object should be vector of characters or logical values.")
  
  if (FALSE %in% (splitByEdges[which(!is.na(splitByEdges))] %in% colnames(movements)))
    stop("splitByEdges object should be refering to a column name of the movements data.frame.")
  if (FALSE %in% (splitByVertex[which(!is.na(splitByVertex))] %in% colnames(premises)))
    stop("splitByVertex object should be refering to a column name of the premises data.frame.")

  # Split datasets 
   
   movInNetwork <- sapply(splitByEdges, function(x){
     if(is.na(x)){
       movements <- movements %>% list
       names(movements)  <- "all"}
     if(!is.na(x))
       movements <- split(movements, movements[,x]) 
   
   movements 
   }) 
   if(splitByEdges %>% length > 1)
     movInNetwork <- movInNetwork %>% unlist(.,recursive=FALSE)
   
   
   premisesInNetwork <- sapply(splitByVertex, function(x){
     if(is.na(x)){
       premises <- premises %>% list
     names(premises)  <- "all"}
     if(!is.na(x))
       premises <- split(premises, premises[,x]) 
     
     premises 
   })
   
   if(splitByVertex %>% length > 1)
     premisesInNetwork <- premisesInNetwork %>% unlist(.,recursive=FALSE)
   
   
   # clean names
   if(is.null(names(movInNetwork))) names(movInNetwork) <- splitByEdges %>% paste0(.,".") %>% paste0(., (movInNetwork %>% dimnames %>% .[[1]]))
   if(is.null(names(premisesInNetwork))) names(premisesInNetwork) <-  splitByVertex %>% paste0(.,".") %>% paste0(., (premisesInNetwork %>% dimnames %>% .[[1]]))
   
   names(movInNetwork)[names(movInNetwork) == "NA.all"] <- "all"
   names(premisesInNetwork)[names(premisesInNetwork) == "NA.all"] <- "all"
   
  # Generate network(s)
   
   experiementationPlan <- expand.grid(movInNetwork %>% names, premisesInNetwork%>% names) 
   experiementationPlan$name <- apply(experiementationPlan, 1 , function(experiment){
     experiment %>% paste0(., collapse = "Mov.for." ) %>%
     paste0(., "Premises")
   })
   
   
   G <- apply(experiementationPlan, 1, function(experiment){
     
     # Select movements based on selected premises
     vertex <- premisesInNetwork[[experiment["Var2"]]][,1]
     
     movInNetwork[[experiment["Var1"]]] <- movInNetwork[[experiment["Var1"]]] %>% 
       .[which(.[,1] %in% vertex & .[,2] %in% vertex),]
     
     g <- igraph::graph_from_data_frame(d = movInNetwork[[experiment["Var1"]]],
                                   directed = T,
                                   vertices = premisesInNetwork[[experiment["Var2"]]])
     
     # Remove inactive nodes
     g <- igraph::delete.vertices(g, igraph::V(g)[igraph::degree(g) == 0])
     
     # Remove loops and duplicates
     g <- igraph::simplify(g, remove.multiple = remove.duplicated.edges, remove.loops = remove.loop.edges)
     
     if(save.network == TRUE){
       save(g, file = paste0(path.save,"/", prefixe,"_",experiment["name"], ".Rda"))
       message(paste(
         "Network was saved as g object under",
         paste0(path.save,"/", prefixe,"_",experiment["name"], ".Rda")
       ))
     }
     
     g
     
   })
   
   # names networks based on experimentation plan
   names(G) <- apply(experiementationPlan,1, function(experiment) experiment["name"])
   
   G
}
  
   
   
  ######### PREVIOUS function splitting by period of time
#   
#     # If period is false, generate the network using all movements and premises
# 
#     if (subsetByEdgesAtt == FALSE) {
#       g <- igraph::graph_from_data_frame(movements[, c("from", "to")],
#                                  directed = T,
#                                  vertices = premises)
# 
#       if(!is.null(edge.att))
#         for(i in edge.att){
#           g <- g %>% set_edge_attr(i, value = movements[, i])
#         }
# 
#       # Remove inactive nodes
#       g <- igraph::delete.vertices(g, V(g)[degree(g) == 0])
# 
#       # Remove loops and duplicates
#       g <- igraph::simplify(g, remove.multiple = F, remove.loops = TRUE)
#       # g  <- lapply(g,simplify, remove.multiple=T, remove.loops = TRUE)
# 
#       if(save.network == TRUE){
#       save(g, file = paste0(path.save,"/", prefixe, ".Rda"))
#       message(paste(
#         "Network was saved as g object under",
#         paste0(path.save,"/", prefixe, ".Rda")
#       ))
#       }
# 
#       G[[length(G)+1]] <- g
#       names(G)[length(G)]<- "full"
#       }
#     else {
#       # Split movements per year
#       annual.movements <- split(movements, movements$year)
# 
#       if (period != "year") {
#         period.movements <-
#           lapply(annual.movements, function(x)
#             split(x, x[, period]))
# 
#         for (year in names(period.movements)) {
#           selected.year <- period.movements[[year]]
# 
#           for (select.period in names(period.movements[[year]])) {
#             selected.movements <- selected.year[[select.period]]
# 
#             g <-
#               graph_from_data_frame(selected.movements[, c("from", 'to')],
#                                     directed = T,
#                                     vertices = premises)
# 
#             if(!is.null(edge.att))
#               for(i in edge.att){
#                 g <- g %>% set_edge_attr(i, value = selected.movements[, i])
#               }
# 
#             # Remove inactive nodes
#             g <- delete.vertices(g, V(g)[degree(g) == 0])
# 
#             # Remove loops and duplicates
#             g <- igraph::simplify(g,
#                           remove.multiple = F,
#                           remove.loops = TRUE)
#             # g  <- lapply(g,simplify, remove.multiple=T, remove.loops = TRUE)
# 
#             gstatic.industry.epid.year <- g
# 
# 
#             if(save.network == TRUE){
#             save(g,
#                  file = paste0(
#                    path.save,"/",
#                    prefixe,
#                    ".",
#                    year,
#                    period,
#                    select.period,
#                    ".Rda"
#                  ))
#             message(paste(
#               "Network was saved as g object under",
#               paste0(
#                 path.save,"/",
#                 prefixe,
#                 ".",
#                 year,
#                 period,
#                 select.period,
#                 ".Rda"
#               )
#             ))}
# 
#             G[[length(G)+1]] <- g
#             names(G)[length(G)]<- paste0(year,
#                                         period,
#                                         select.period)
#           }
#         }
#       } else{
#         for (year in names(annual.movements)) {
#           selected.movements <- annual.movements[[year]]
# 
#           g <- graph_from_data_frame(selected.movements[, c("from", "to")],
#                                      directed = T,
#                                      vertices = premises)
# 
# 
#           if(!is.null(edge.att))
#             for(i in edge.att){
#               g <- g %>% set_edge_attr(i, value = selected.movements[, i])
#             }
# 
#           # Remove inactive nodes
#           g <- delete.vertices(g, V(g)[degree(g) == 0])
# 
#           # Remove loops and duplicates
#           g <- simplify(g,
#                         remove.multiple = F,
#                         remove.loops = TRUE)
#           # g  <- lapply(g,simplify, remove.multiple=T, remove.loops = TRUE)
# 
#           gstatic.industry.epid.year <- g
#           if(save.network == TRUE){
#           save(g, file = paste0(path.save,"/", prefixe, ".", year, ".Rda"))
#           message(paste(
#             "Network was saved as g object under",
#             paste0(path.save,"/", prefixe, ".", year, ".Rda")
#           ))}
# 
#         G[[length(G)+1]] <- g
#         names(G)[length(G)] <- year
#         }
#       }
#     }
#   }
# G
