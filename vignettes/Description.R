## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
list.of.packages <- c("SwineNet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

## -----------------------------------------------------------------------------
data(listPremises)
data(listMovements)

## -----------------------------------------------------------------------------
premises.attributs <- c("siteID", "farmGroup",  "type", "freerange", "BRS", "lon", "lat", "mainland")

## -----------------------------------------------------------------------------
listMovements <- timewindows(listMovements, formatDate = "%Y-%m-%d")

## -----------------------------------------------------------------------------
G <- get_network(movements = listMovements[,c("from","to")],
                      premises = listPremises[,premises.attributs],
                      save.network = FALSE,
                      remove.duplicated.edges = F,
                      remove.loop.edges = T)

## -----------------------------------------------------------------------------
listPremises <- listPremises[which(listPremises$mainland==TRUE),]
premises.attributs <- c("siteID", "farmGroup",  "type", "freerange", "BRS", "lon", "lat")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU","BS","SL")),premises.attributs]

related.movements <- listMovements[which(listMovements$from %in% selectedFARMS$siteID &
                                               listMovements$to %in% selectedFARMS$siteID ),]


G <- get_network(movements = related.movements[,c("from","to")],
                      premises = selectedFARMS[,premises.attributs],
                      save.network = FALSE)

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU")),
                              premises.attributs]

related.movements <- listMovements[which(listMovements$from %in% selectedFARMS$siteID &
                                           listMovements$to %in% selectedFARMS$siteID ),]

G <- get_network(movements = related.movements,
                      premises = selectedFARMS,
                      save.network = FALSE)

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU")),
                              premises.attributs]

related.movements <- listMovements[which(listMovements$from %in% selectedFARMS$siteID &
                                               listMovements$to %in% selectedFARMS$siteID ),]

G <- get_network(movements = related.movements,
                      premises = selectedFARMS,
                      splitByEdges = c("MType", "year"),
                      save.network = FALSE)

G %>% names

## -----------------------------------------------------------------------------

G <- get_network(movements = related.movements,
                      premises = selectedFARMS,
                      splitByVertex = "type",
                      save.network = FALSE)

G %>% names

G <- get_network(movements = related.movements,
                      premises = selectedFARMS,
                      splitByEdges = c(NA, "semester"),
                      splitByVertex = "freerange",
                      save.network = FALSE)

G %>% names

## ---- eval=F------------------------------------------------------------------
#  igraph::get.edge.attribute(G[[1]]) %>% names
#  igraph::get.vertex.attribute(G[[1]]) %>% names
#  
#  swineNetwork.representation(network = G[[1]],
#                          title = "network representation",
#                          movements.type.lab = "MType",
#                          premises.type.lab = "type")
#  

## ---- eval=F------------------------------------------------------------------
#  igraph::get.vertex.attribute(G[[1]]) %>% names
#  
#  map_network(
#    network = G[[1]],
#    country = c("FR"),
#    premisescolours = "type",
#    title = "network map",
#    topfeature = "point"
#  )
#  

## -----------------------------------------------------------------------------
# Save coordinates in the right format to feed The Geographic Distance Matrix Generator
write.table(selectedFARMS[,c("siteID","lon","lat")], file = "GPS.txt", sep="\t", row.names = F, quote = FALSE)

# final required format
data(distMatrix)

## -----------------------------------------------------------------------------
centralities_network(G[[1]], distMatrix = F)

## -----------------------------------------------------------------------------
g <- intergraph::asNetwork(G[[1]])

########### 
########### Nodal and edges attributes
########### 
library(network)

network::list.vertex.attributes(g)

farmGroup.levels <- network::get.vertex.attribute(g, "farmGroup") %>% unique %>% length
type.levels <- network::get.vertex.attribute(g, "type") %>% unique %>% length
BRS.levels <- network::get.vertex.attribute(g, "BRS") %>% unique %>% length
freerange.levels <- network::get.vertex.attribute(g, "freerange") %>% unique %>% length

elements <- c(
  "edgecov(distMatrix)",
  paste("nodefactor(\"farmGroup\", levels=c(",seq(farmGroup.levels),"))"),
  paste("nodeifactor(\"farmGroup\", levels=c(",seq(farmGroup.levels),"))"),
  paste("nodeofactor(\"farmGroup\", levels=c(",seq(farmGroup.levels),"))"),
  paste("nodefactor(\"type\", levels=c(",seq(type.levels),"))"),
  paste("nodeifactor(\"type\", levels=c(",seq(type.levels),"))"),
  paste("nodeofactor(\"type\", levels=c(",seq(type.levels),"))"),
  paste("nodefactor(\"BRS\", levels=c(",seq(BRS.levels),"))"),
  paste("nodeifactor(\"BRS\", levels=c(",seq(BRS.levels),"))"),
  paste("nodeofactor(\"BRS\", levels=c(",seq(BRS.levels),"))"),
  paste("nodefactor(\"freerange\", levels=c(",seq(freerange.levels),"))"),
  paste("nodeifactor(\"freerange\", levels=c(",seq(freerange.levels),"))"),
  paste("nodeofactor(\"freerange\", levels=c(",seq(freerange.levels),"))"),
  # interactions
  paste("nodematch(\"farmGroup\", diff=F)"),
  paste("nodematch(\"BRS\", diff=F)"),
  paste("nodematch(\"farmGroup\", diff=T, levels=c(",seq(farmGroup.levels),"))"),
  paste("nodemix(\"freerange\", levels2=c(",seq(freerange.levels^2),"))"),
  paste("nodemix(\"BRS\", levels2=c(",seq(BRS.levels)^2,"))"),
  paste("nodemix(\"type\", levels2=c(",seq(type.levels^2),"))")#,
  # paste("asymmetric(\"type\")"),
  # paste("asymmetric(\"type\", diff = T, levels=c(",seq(type.levels),"))")
  ) %>% as.list

elements %>% unlist %>% length

########### 
########### Structural characteristics
########### 

library(sna)
ostar.levels <- degree(g,gmode = "out") %>% unique
istar.levels <- degree(g,gmode = "in") %>% unique
triadcensus.levels <- c(0:15)[which(summary(g ~ triadcensus(0:15)) != 0)]
ITP.levels <- which(summary(g~ddsp(1:50, "ITP")) !=0)
OTP.levels <- which(summary(g~ddsp(1:50, "OTP")) !=0)
OSP.levels <- which(summary(g~ddsp(1:50, "OSP")) !=0)

structural.elements <- as.list(c(
  "edges","twopath", "idegree1.5", "odegree1.5", "transitive",
  paste("ostar(",seq(ostar.levels),")"),
  paste("istar(",seq(istar.levels),")"),
  paste("triadcensus(",triadcensus.levels,")"),
  paste("ddsp(",seq(OSP.levels),", type =\"OSP\")"),
  paste("ddsp(",seq(ITP.levels),", type =\"ITP\")"),
  paste("ddsp(",seq(OTP.levels),", type =\"OTP\")")
))

structural.elements %>% unlist %>% length

## ----eval=FALSE---------------------------------------------------------------
#  ###########
#  ########### Run the stepwise selection process
#  ###########
#  
#  base.formula <- "g ~ edges"
#  
#  ###########
#  ########### Forward stepwise procedure
#  ###########
#  
#  stepwise4ERGM(base.formula,
#                             nbworkers =  10, # integer
#                             elements = append(elements,structural.elements) %>% str, # list
#                             network.name = "mynetwork", # string
#                             results.path = getwd(),
#                             maxduration = 60*60*1, # integer in seconds
#                             g,
#                             mode = "forward", # string "forward" or "backward"
#                             # stepwise.summary = stepwise.summary,
#                             verbose =T
#  )
#  
#  ###########
#  ########### Backward stepwise procedure
#  ###########
#  
#  
#  load(file = getwd() %>%
#    list.files(., full.names = TRUE) %>%
#    .[grep(paste0("stepwise.summary_mynetwork"),.)]%>%
#    extract(which.max(file.mtime(.))))
#  
#  base.formula <- stepwise.summary[[stepwise.summary %>% length]]$formula %>%  substr(., gregexpr("\\g ~", .)[[1]][1] , nchar(.)-1)
#  
#  
#  stepwise4ERGM(base.formula,
#                             nbworkers = 10, #integer
#                             network.name = "mynetwork", # string
#                             results.path = getwd(),
#                             maxduration = 60*60*2, # integer in seconds
#                             g,
#                             distMatrix,
#                             mode = "backward", # string "forward" or "backward"
#                             stepwise.summary = stepwise.summary,
#                             verbose = T
#  )
#  

## ----eval = FALSE-------------------------------------------------------------
#  # read the most recent output
#  load(file = list.files(., full.names = TRUE) %>%
#    .[grep(paste0("stepwise.summary_mynetwork"),.)]%>%
#    extract(which.max(file.mtime(.))))
#  
#  selected.terms <- analyseStepwise(stepwise.summary)

## ----eval = FALSE-------------------------------------------------------------
#  selectedERGM(stepwise.summary,
#               g,
#               outputpath='',
#               distMatrix = NA,
#               nsimGof = 2000,
#               nSimNet = 5,
#               maxtimeERGM = 60*60*2,
#               country.plot = c("FR"),
#               nrepAIC = 3,
#               verbose = T)

