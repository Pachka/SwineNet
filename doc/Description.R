## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
devtools::load_all()
library(SwineNet)

## -----------------------------------------------------------------------------
data(listPremises)
data(listMovements)

## -----------------------------------------------------------------------------
premises.attributs <- c("siteID", "farmGroup",  "type", "freerange", "BRS", "lon", "lat", "mainland")

## -----------------------------------------------------------------------------
G <- generate_network(movements = listMovements[,c("from","to")],
                 premises = listPremises[,premises.attributs],
                 save.network = FALSE,
                 path.save ="networks", prefixe = "full.net")

## -----------------------------------------------------------------------------
listPremises <- listPremises[which(listPremises$mainland==TRUE),]
premises.attributs <- c("siteID", "farmGroup",  "type", "freerange", "BRS", "lon", "lat")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU","BS","SL")),
                              premises.attributs]

related.movements <- listMovements[which(listMovements$from %in% selectedFARMS$siteID &
                                               listMovements$to %in% selectedFARMS$siteID ),]


G <- generate_network(movements = related.movements,
                 premises = selectedFARMS,
                 save.network = FALSE, path.save ="networks", prefixe = "full.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU")),
                              premises.attributs]

related.movements <- listMovements[which(listMovements$from %in% selectedFARMS$siteID &
                                               listMovements$to %in% selectedFARMS$siteID ),]

G <- generate_network(movements = related.movements,
                 premises = selectedFARMS,
                 save.network = FALSE, path.save ="networks", prefixe = "epidPurposes.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU")),
                              premises.attributs]

related.movements <- listMovements[which(listMovements$from %in% selectedFARMS$siteID &
                                               listMovements$to %in% selectedFARMS$siteID ),]

G <- generate_network(movements = related.movements,
                 premises = selectedFARMS,
                 periods = c("year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "epidPurposes.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("MU", "NU","BS")),
                              premises.attributs]

related.movements <- listMovements[which(listMovements$from %in% selectedFARMS$siteID &
                                               listMovements$to %in% selectedFARMS$siteID ),]

G <- generate_network(movements = related.movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "breeding.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI")),
                              premises.attributs]

related.movements <- listMovements[which(listMovements$from %in% selectedFARMS$siteID &
                                               listMovements$to %in% selectedFARMS$siteID ),]


G <- generate_network(movements = related.movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "production.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "PW", "PWF", "MU", "NU")),
             premises.attributs]

movements <- listMovements[which(listMovements$MType == "piglets" &
                                       listMovements$from %in% selectedFARMS$siteID &
                                       listMovements$to %in% selectedFARMS$siteID ),]

G <- generate_network(movements = movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "piglets.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FPW", "FF", "PW", "PWF", "FI", "MU", "NU")),
                              premises.attributs]

movements <- listMovements[which(listMovements$MType == "growingPigs" &
                                       listMovements$from %in% selectedFARMS$siteID &
                                       listMovements$to %in% selectedFARMS$siteID ),]

G <- generate_network(movements = movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "growing.pigs.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- listPremises[which(listPremises$type %in% c("FA", "FPW", "FF", "MU", "NU")),
                              premises.attributs]

movements <- listMovements[which(listMovements$MType == "rpSows" &
                                       listMovements$from %in% selectedFARMS$siteID &
                                       listMovements$to %in% selectedFARMS$siteID ),]

G <- generate_network(movements = movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "reproductive.sows.industry.net")


## ---- eval=F------------------------------------------------------------------
#  map_network(
#    network = G[[1]],
#    country = c("FR"),
#    title = "network map",
#    topfeature = "point"
#  )

## -----------------------------------------------------------------------------
# Save coordinates in the right format to feed The Geographic Distance Matrix Generator
write.table(selectedFARMS[,c("siteID","lon","lat")], file = "GPS.txt", sep="\t", row.names = F, quote = FALSE)

# final required format
data(distMatrix)

## -----------------------------------------------------------------------------
network_centralities(G[[1]])

## -----------------------------------------------------------------------------
exp.design <- expand.grid(network = names(G[2:3]), nCov = 2:3)

## -----------------------------------------------------------------------------
summary(intergraph::asNetwork(G[[1]]) ~ edges + idegree1.5 + odegree1.5 + twopath)

coVar <- c("edges", "idegree1.5","odegree1.5","twopath","transitive",
           "nodematch(\"farmGroup\", diff = TRUE,levels = which(diag(mixingmatrix(G, \"farmGroup\")) > 2))",
           "edgecov(distMatrix)")

comb.Covar <- generate.coVar.comb(exp.design, coVar, splitin=FALSE)
# how many combination per batch/experiment?
sapply(comb.Covar, length)


## ----eval=FALSE---------------------------------------------------------------
#  # Set prefixe for output file
#  prefixe <- "output"
#  
#  
#  required.cores <- nrow(exp.design)
#  nc = ifelse((detectCores()-1) < required.cores, detectCores()-1, required.cores)
#  
#  if(.Platform$OS.type == "unix") {
#    registerDoMC(nc) } else {
#      cl = makeCluster(nc)
#      registerDoParallel(cl)
#    }
#  
#  foreach(i = seq(nrow(exp.design)),
#          .packages = c("ergm", "network","intergraph")) %dopar% {
#  
#            exp.design[,1] <- as.character(exp.design[,1])
#            network <- exp.design[i,1]
#            nCov <- as.numeric(exp.design[i,2])
#            combtorun <- comb.Covar[[paste(exp.design[i,], collapse = ".")]]
#  
#            filename = paste0(prefixe,"_",network,"_",nCov,".Rda")
#  
#            if(splitin!=FALSE){
#              batch <- as.character(subNet.nCov[i,3])
#              filename = paste0(prefixe,"_",network,"_",nCov,"_",batch,".Rda")
#              }
#  
#            launch.ergm(G = intergraph::asNetwork(G[[network]]),
#                        matDist = distMatrix,
#                        fileloc = "output/",
#                        filename = filename,
#                        combtotest=combtorun
#                        )
#  
#          } # end foreach
#  
#  if(.Platform$OS.type == "windows")
#  stopCluster(cl)

## ----eval=FALSE---------------------------------------------------------------
#  merge.ergm.output(list.networks = unique(exp.design$network),
#                        list.nCov = unique(exp.design$nCov),
#                        fileloc = "output/",
#                        prefixe = "output",
#                        duration.h = 24)

## ----eval=FALSE---------------------------------------------------------------
#  comb.Covar <- generate.coVar.comb(exp.design, coVar, splitin=FALSE,
#                                    screen.results = TRUE, result.file ="output/", prefixe="output")
#  # how many combination per batch/experiment?
#  sapply(comb.Covar, length)

## ----eval = FALSE-------------------------------------------------------------
#  list.output <- compare.ergm.output(list.networks = unique(exp.design$network),
#                     list.nCov = unique(exp.design$nCov),
#                     fileloc = "output/",
#                     prefixe = "output",
#                     marginAIC = 1000,
#                     display.info = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  analyse.ERGM(G, list.output, fileloc = "output/", prefixe = 'final')

