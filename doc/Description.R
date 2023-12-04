## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
devtools::load_all()
library(SwineNet)

## -----------------------------------------------------------------------------
data(sites)
data(movements)

## -----------------------------------------------------------------------------
premises.attributs <- c("siteID", "farmGroup",  "type", "freerange", "BRS", "lon", "lat", "mainland")

## -----------------------------------------------------------------------------
G <- get_network(movements = movements[,c("from","to")],
                 premises = sites[,premises.attributs],
                 save.network = FALSE,
                 path.save ="networks", prefixe = "full.net")

## -----------------------------------------------------------------------------
sites <- sites[which(sites$mainland==TRUE),]
premises.attributs <- c("siteID", "farmGroup",  "type", "freerange", "BRS", "lon", "lat")

## -----------------------------------------------------------------------------
selectedFARMS <- sites[which(sites$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU","BS","SL")),
                              premises.attributs]

related.movements <- movements[which(movements$from %in% selectedFARMS$siteID &
                                               movements$to %in% selectedFARMS$siteID ),]


G <- get_network(movements = related.movements,
                 premises = selectedFARMS,
                 save.network = FALSE, path.save ="networks", prefixe = "full.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- sites[which(sites$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU")),
                              premises.attributs]

related.movements <- movements[which(movements$from %in% selectedFARMS$siteID &
                                               movements$to %in% selectedFARMS$siteID ),]

G <- get_network(movements = related.movements,
                 premises = selectedFARMS,
                 save.network = FALSE, path.save ="networks", prefixe = "epidPurposes.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- sites[which(sites$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI", "MU", "NU")),
                              premises.attributs]

related.movements <- movements[which(movements$from %in% selectedFARMS$siteID &
                                               movements$to %in% selectedFARMS$siteID ),]

G <- get_network(movements = related.movements,
                 premises = selectedFARMS,
                 periods = c("year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "epidPurposes.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- sites[which(sites$type %in% c("MU", "NU","BS")),
                              premises.attributs]

related.movements <- movements[which(movements$from %in% selectedFARMS$siteID &
                                               movements$to %in% selectedFARMS$siteID ),]

G <- get_network(movements = related.movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "breeding.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- sites[which(sites$type %in% c("FA", "FPW", "FF", "PW", "PWF", "FI")),
                              premises.attributs]

related.movements <- movements[which(movements$from %in% selectedFARMS$siteID &
                                               movements$to %in% selectedFARMS$siteID ),]


G <- get_network(movements = related.movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "production.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- sites[which(sites$type %in% c("FA", "FPW", "FF", "PW", "PWF", "MU", "NU")),
             premises.attributs]

movements <- movements[which(movements$MType == "piglets" &
                                       movements$from %in% selectedFARMS$siteID &
                                       movements$to %in% selectedFARMS$siteID ),]

G <- get_network(movements = movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "piglets.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- sites[which(sites$type %in% c("FPW", "FF", "PW", "PWF", "FI", "MU", "NU")),
                              premises.attributs]

movements <- movements[which(movements$MType == "growingPigs" &
                                       movements$from %in% selectedFARMS$siteID &
                                       movements$to %in% selectedFARMS$siteID ),]

G <- get_network(movements = movements,
                 premises = selectedFARMS,
                 periods = c(FALSE, "year", "semester", "month"),
                 save.network = FALSE, path.save ="networks",
                 prefixe = "growing.pigs.industry.net")

## -----------------------------------------------------------------------------
selectedFARMS <- sites[which(sites$type %in% c("FA", "FPW", "FF", "MU", "NU")),
                              premises.attributs]

movements <- movements[which(movements$MType == "rpSows" &
                                       movements$from %in% selectedFARMS$siteID &
                                       movements$to %in% selectedFARMS$siteID ),]

G <- get_network(movements = movements,
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

centralities_network(G[[1]])

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

