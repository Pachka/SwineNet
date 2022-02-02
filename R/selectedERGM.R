#' Summarize stepwise output
#'
#' @description  FIX ME
#'
#' @param stepwise.summary FIX ME
#' @param g FIX ME
#' @param outputpath FIX ME
#' @param distMatrix  FIX ME
#' @param nsimGof  FIX ME
#' @param nSimNet FIX ME
#' @param maxtimeERGM FIX ME
#' @param country.plot FIX ME
#' @param nrepAIC FIX ME
#' 
#' @import ergm
#' @import coda
#' 
#' @return FIX ME
#' 
#' @export


selectedERGM <- function(stepwise.summary,
                         g,
                         outputpath='',
                         distMatrix = NA,
                         nsimGof = 2000,
                         nSimNet = 5,
                         maxtimeERGM = 60*60*2,
                         country.plot = c("FR"),
                         nrepAIC = 3,
                         verbose = T){

  selectedmodel <- stepwise.summary[stepwise.summary %>% length][[1]]$formula %>% substr(.,1, nchar(.) -1)
  
  capt.out <-
    R.utils::withTimeout({
      SwineNet::capture_log1(
        model <- paste0(substr(selectedmodel, gregexpr("\\:", selectedmodel)[[1]][1] + 2 , nchar(selectedmodel)),",control=ergm::control.ergm(",
                        "MCMLE.maxit = 35,", 
                        "MCMC.samplesize = 25000,",
                        "MCMC.burnin = 10000,",
                        "parallel = 3))") %>% parse(text = .) %>% eval
      )$logs %>% lapply(., function(x) x$message) %>% unlist
    }, timeout=maxtimeERGM)
  
  if(class(model)=="ergm"){
    listAIC <- summary(model)$aic
    best.model.output <- list(model, capt.out)
  }
  
  while(length(listAIC) < nrepAIC){
    capt.out <-
      R.utils::withTimeout({SwineNet::capture_log1(
        model <- paste0(substr(selectedmodel, gregexpr("\\:", selectedmodel)[[1]][1] + 2 , nchar(selectedmodel)),",control=ergm::control.ergm(",
                        "MCMLE.maxit = 35,", 
                        "MCMC.samplesize = 25000,",
                        "MCMC.burnin = 10000,",
                        "parallel = 3))") %>% parse(text = .) %>% eval
      )$logs %>% lapply(., function(x) x$message) %>% unlist}, timeout=maxtimeERGM)
    
    if(class(model) =="ergm"){
      if(is.null(listAIC)){
        best.model.output <- list(model, capt.out)
      } else if(summary(model)$aic < min(listAIC)){
        best.model.output <- list(model, capt.out)
      } else if(summary(model)$aic > max(listAIC)){
        worst.model.output <- list(model, capt.out)
      } 
      
      listAIC <- append(listAIC, summary(model)$aic)
    }
  }
  
  print(paste("model done with AIC:",min(listAIC),", go saving"))
  
  save(listAIC, file = paste0(outputpath,"ListAIC_.Rda"))
  if(verbose) message("AIC estimated over repeatition were saved in ",paste0(outputpath,"ListAIC_.Rda"))
  
  covariables <- best.model.output[[1]]$glm$coef
  
  save(covariables, file = paste0(outputpath,"covariables.Rda"))
  if(verbose) message("Estimates were saved in ",paste0(outputpath,"covariables.Rda"))
  save(best.model.output, file = paste0(outputpath,"Best_model_net.Rda"))
  if(verbose) message("Best model was saved in ",paste0(outputpath,"Best_model_net.Rda"))
  
  if(exists("worst.model.output")){
    save(worst.model.output, file = paste0(outputpath,"Worst_model_net.Rda"))
    if(verbose) message("Worst model was saved in ",paste0(outputpath,"Worst_model_net.Rda"))}
  
  model <- best.model.output[[1]]
  
  if(exists("worst.model.output"))
    rm(best.model.output, worst.model.output, listAIC, capt.out)else
      rm(best.model.output, listAIC, capt.out)
  
  if(!is.null(model$sample)){
    pdf(paste0(outputpath,"Mcmdiag.pdf"))
    par(mar = c(1, 1, 1, 1))
    mcmc.diagnostics(model)
    dev.off()
    
    pdf(paste0(outputpath,"Codaplot.pdf"))
    plot(model$sample, smooth = TRUE, ask = FALSE)
    dev.off()
    
    pdf(paste0(outputpath,"Gelmanplot.pdf"))
    coda::gelman.plot(model$sample)
    dev.off()
    if(verbose) message("MCMC diagnostics were saved in ",paste0(outputpath, c("Mcmdiag.pdf, ", "Codaplot.pdf and ", "Gelmanplot.pdf")))
    }
  
  modelgof <- gof(model, GOF =  formula(~ idegree + odegree + espartners + dspartners + distance + triadcensus + model),
                  control = control.gof.ergm(nsim = nsimGof))
  
  
  save(modelgof, file = paste0(outputpath,"modelgof.Rda"))
  if(verbose) message("Goodness-of-fit was saved in ",paste0(outputpath,"modelgof.Rda"))
  
  # PNG device
  png(file=paste0(outputpath,"Gof_logodds.png"),
      width = 9000,
      height = 9000,
      res = 500)
  
  # Code
  par(mfrow=c(3,3))
  # par(oma=c(0.5,2,1,0.5))
  par(oma=c(0.5,0.5,0.5,0.5))
  plot(modelgof, plotlogodds=TRUE)
  
  # Close device
  dev.off()
  
  # PNG device
  png(paste0(outputpath,"Gof.png"),
      width = 9000,
      height = 9000,
      res = 500)
  
  # Code
  par(mfrow=c(3,3))
  # par(oma=c(0.5,2,1,0.5))
  par(oma=c(0.5,0.5,0.5,0.5))
  plot(modelgof)
  
  # Close device
  dev.off()
  
  if(verbose) message("GOF plots were saved in ",paste0(outputpath, c("Gof_logodds.png and ", "Gof.png")))
  
  
  par(mfrow=c(1,1))
  sim.net <- simulate(model, nsim = nSimNet)
  print("sim model done go saving")
  save(sim.net, file = paste0(outputpath,"Sim.net_net.Rda"))
  if(verbose) message("Simulated networks were saved in ", paste0(outputpath, "Sim.net_net.Rda"))
  
  #### print simulated networks
  obsVSsim <- append(list(g), sim.net)
  
  obsVSsim <- lapply(obsVSsim, function(i){
    if(class(i)=="network")
      i <- intergraph::asIgraph(i)
    
    i
  })
  
  
  p <- lapply(as.list(seq(length(obsVSsim))), function(i){
    plot.title <- if(i == 1) "observed network" else paste("simulated network", i-1)
    pp <- SwineNet::map.network(
      network = obsVSsim[[i]],
      country = country.plot,
      path.rasters = "",
      title = plot.title,
      topfeature = "path",
      path.color = "black",
      path.size = 0.01,
      point.size.coef = 1
    )
    
    ggsave(
      paste0(outputpath,"map.",i,".png"),
      plot = pp,
      width = 6,
      height = 5,
      dpi = 300
    )
    
    pp
  })
  if(verbose) message("Simulated networks plots were saved in ", paste0(outputpath,"map.",seq(length(obsVSsim)),".png"))
  
  predictTiesOdds <- predict(model)
  print("predict done go saving")

  save(predictTiesOdds, file = paste0(outputpath,"PredictTiesOdds_net.Rda"))
  if(verbose) message("Contact probabilities per pair of nodes were saved in ", paste0(outputpath,"PredictTiesOdds_net.Rda"))
  
}