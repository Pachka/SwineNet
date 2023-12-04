#' Stepwise selection for ergm
#'
#' @description Process to forward, backward or bidirectional stepwise selection of ergm based on term
#'
#' @param base.formula string
#' @param nbworkers integer
#' @param elements list of terms
#' @param network.name string
#' @param results.path string
#' @param maxduration integer/ Duration in seconds
#' @param g network
#' @param distMatrix matrix
#' @param mode String. "forward" or "backward"
#' @param stepwise.summary list
#' @param baseline.aic integer
#' @param verbose logical
#'
#' @return Saved objects
#'
#' @import ergm
#'
#' @export

stepwise4ERGM <- function(base.formula = NULL,  #string  stepwise.summary[[stepwise.summary%>% length]]$formula %>% substr(., gregexpr("\\~", base.formula)[[1]][1] - 2, nchar(.)-1)
                          nbworkers = 10, #integer
                          elements = NA, # list
                          network.name, # string
                          results.path = "../data/ERGM_output/combinations/",
                          maxduration = 60*60*2, # integer in seconds
                          g,
                          distMatrix = NA,
                          mode = "forward", # string "forward" or "backward"
                          stepwise.summary = list(),
                          baseline.aic = NA,
                          predict.limit = TRUE,
                          verbose = F
){

  ############
  ## CHECKS ##
  ############

  # check the mode
  if(!(mode %in% c("forward", "backward", "bidirectional")))
    stop("mode should be \"forward\" or \"backward\" ")

  # check is the list of covariable to test has been provided
  if(mode %in% c("forward","bidirectional")){
    if(identical(NA, elements)) stop('a list of elements must be provided for the stepwise forward procedure')
  }

  ###############
  ## FUNCTIONS ##
  ###############

  # Extract elements included in the base formula
  incVar <- function(base.formula, bracket = FALSE){
    substr(base.formula, gregexpr("\\~", base.formula)[[1]][1] + 1 , nchar(base.formula) - bracket) %>%
      strsplit(., split = "\\+") %>%
      unlist %>% gsub(" ", "", .) %>% unique}

  # List elements non included in the base formula
  no.incVar <- function(elements, included.elements){elements[which(!(elements %>% gsub(" ", "", .) %>% unique) %in% (included.elements))] %>%
      gsub(" ", "", .) %>%
      unique}

  # Remove from the list of element to test the one that were already run or that crashed
  removePrevious <- function(tested_elements, crashing_elements, done_elements){
    if((crashing_elements %>% length) != 0){

      # update list of tested elements by removing covariable that crashed
      tested_elements <- tested_elements[-which((tested_elements %>% unlist) %in% crashing_elements)]

      # Creating a file with the crashed variables in case it crashes again
      file.create(paste0(results.path,network.name,"/",prefixe,"_stocked.txt"))
      cat(crashing_elements,file=paste0(results.path, network.name,"/", prefixe, "_stocked.txt"),append=TRUE)

      # Remove crashes record from previous run
      list.files(paste0(results.path,network.name)) %>%
        .[grep(paste0("^",prefixe, "_running"),.)] %>% as.list %>%
        lapply(., function(y){
          y <- paste0(results.path,network.name,"/",y)
          file.remove(y)})
    }


    if((done_elements %>% nrow) > 0)
      # update list of tested elements by removing covariable that crashed
      tested_elements <- tested_elements[-which((tested_elements %>% unlist) %in% (done_elements %>% .$V1 %>% unlist))]

    tested_elements
  }

  # Run the ergm and output the aic when possible
  runmodels <- function(model.asStringtorun, i){
    # Load required objects
    g
    if(model.asStringtorun %>% as.character %>% grepl("edgecov",.)) distMatrix[1,1]

    capt.out <-
      R.utils::withTimeout({capture_log1(
        model <- eval(parse(text = model.asStringtorun))
      )$logs %>% lapply(., function(x) x$message) %>% unlist}, timeout=maxduration)

    file.remove(paste0(results.path,network.name,"/",prefixe,"_running_",which(tested_elements %>% unlist == i),".txt"))

    if((grep("reached elapsed time limit",capt.out) %>% length +
        grep("la limite de temps est atteinte",capt.out) %>% length +
        grep("Their coefficients will be fixed at -Inf.",capt.out) %>% length +
        grep("fitted probabilities numerically 0 or 1 occurred",capt.out) %>% length +
        grep("Too many unique dyads.", capt.out) %>% length) > 0  | (
          grep("Starting Monte Carlo maximum likelihood estimation (MCMLE):",capt.out) %>% length == 1 &
          grep("Converged with 99% confidence", capt.out) %>% length == 0
        )){
      res <- NA
    }else{
      # print(capt.out)
      res <- summary(model)$aic %>% .[1]}

    file.create(paste0(results.path,network.name,"/",prefixe,"_done_",which(tested_elements %>% unlist == i),".txt"))
    cat(paste(i," ",res),file=paste0(results.path, network.name,"/", prefixe, "_done_", which(tested_elements %>% unlist == i),".txt"),append=TRUE)

    res
  }

  # Add previous outputs if any and remove files
  dealWithPreviousOutPuts <- function(aic.list, tested_elements, done_elements){

    # listdonefiles <- paste0(results.path,network.name,"/") %>%
    #   list.files %>%
    #   grep(paste(c(paste0(prefixe,"_done_"),
    #                paste0(prefixe,"_ended"),
    #                paste0(prefixe,"_stocked")),collapse="|"),., value=TRUE) %>%
    #   unique %>%
    #   paste0(results.path,network.name,"/",.)
    #
    # if(length(listdonefiles)>0) file.remove(listdonefiles)

    if((done_elements %>% nrow) > 0){
      aic.list <- c(aic.list,(done_elements$V2 %>% unlist))
      tested_elements <- c(tested_elements,(done_elements$V1 %>% unlist))
      done_elements <- data.frame()
    }

    list(aic.list,
         tested_elements,
         done_elements)
  }

  # reset the mode in case of bidirectional stepwise approach
  resetmode <- function(mode, multimode, stepwise.summary){
    if(multimode){
      if(mode %in% c("forward", "backward")){
        mode <- c("forward", "backward") %>% .[.!= mode]
      } else
        if(mode == "bidirectional"){
          mode <- "forward"
          if((stepwise.summary %>% length) > 0) mode <- c("forward", "backward") %>% .[.!= stepwise.summary[[(stepwise.summary %>% length)]]$mode]}
    }
    mode
  }

  #  Test predict function
  test.predict.function <- function(modelformula){
    capt.out <- capture_log1({
      model <- eval(parse(text = modelformula))
      predict(model)
    }
    )$logs %>% lapply(., function(x) x$type) %>% unlist

    if(TRUE %in% grepl("error", capt.out)) stop("================== \n The attempt to predict probabilities produced an error \n ================== ")
  }


  ####################
  ## INITIALIZATION ##
  ####################

  # except for bidirectional: set multimode as FALSE
  multimode <- FALSE

  # set prefixe for output files
  if(mode == "forward"){
    prefixe <- "frwd"
  } else
    if(mode == "backward"){
      prefixe <- "bckwd"
    } else
      if(mode == "bidirectional"){
        multimode <- TRUE
        prefixe <- "2dir"
      }

  # if necessary, create the output folder
  if(!file.exists(paste0(results.path,network.name))){
    dir.create(paste0(results.path,network.name))
    print(paste("The directory",paste0(results.path,network.name), "has been created"))
  }

  # Set baseline AIC
  if(is.null(base.formula) & is.na(baseline.aic)) baseline.aic <- Inf

  if(is.na(baseline.aic)){
    if(verbose) message("Start evaluation of baseline \n")

    capt.out <-
      R.utils::withTimeout({capture_log1(
        model <- paste0("ergm(",base.formula,")") %>% parse(text = .) %>% eval(.)
      )$logs %>% lapply(., function(x) x$message) %>% unlist}, timeout=maxduration)


    if((grep("Their coefficients will be fixed at -Inf.",capt.out) %>% length +
        grep("fitted probabilities numerically 0 or 1 occurred",capt.out) %>% length +
        grep("Too many unique dyads.", capt.out) %>% length) > 0  | (
          grep("Starting Monte Carlo maximum likelihood estimation (MCMLE):",capt.out) %>% length == 1 &
          grep("Converged with 99% confidence", capt.out) %>% length == 0
        ))
      baseline.aic <- Inf else baseline.aic <- model %>% summary%>%  .$aic %>% .[1]
  }

  if(!is.null(base.formula) & verbose)
    cat("Start from baseline formula: \n",base.formula, "\n With baseline AIC = ", baseline.aic , " at ", Sys.time() %>% as.character(), "\n")


  #  list already included elements if any
  if(!is.null(base.formula))
    included.elements <- incVar(base.formula)

  if(is.null(base.formula))
    included.elements <- NULL

  #  list non included elements
  if(!identical(elements, NA)){
    non.included.elements <- no.incVar(elements, included.elements)
  }

  #  list and save previously 'done' elements
  done_elements <- list.files(paste0(results.path,network.name)) %>%
    .[grep(paste0("^",prefixe),.)] %>% .[grep("done",.)] %>%
    sapply(.,function(x) suppressWarnings(read.table(file = paste0(results.path,network.name,"/",x))))  %>%
    as.data.frame %>% do.call(rbind,.) %>% data.frame

  if(file.exists(paste0(results.path,network.name,"/",prefixe,"_ended.Rda"))){
    load(file = paste0(results.path,network.name,"/",prefixe,"_ended.Rda"))
    done_elements <- rbind(ended,done_elements)}

  done_elements %<>% unique
  ended <- done_elements
  if((ended %>% nrow) > 0)
    save(ended, file = paste0(results.path,network.name,"/",prefixe,"_ended.Rda"))

  listdonefiles <- list.files(paste0(results.path,network.name,"/"))[grep(paste0(prefixe,"_done_"), list.files(paste0(results.path,network.name,"/")))]
  if(length(listdonefiles)>0) file.remove(paste0(results.path,network.name,"/",listdonefiles))

  #  list previously 'crashed' elements
  crashing_elements <- list.files(paste0(results.path,network.name)) %>%
    .[grep(paste0("^",prefixe),.)] %>% .[grep("running",.)] %>%
    sapply(.,function(x) suppressWarnings(read.table(file = paste0(results.path,network.name,"/",x)))) %>%
    unlist

  if(file.exists(paste0(results.path,network.name,"/",prefixe,"_stocked.txt")))
    crashing_elements <- c(crashing_elements, read.table(file = paste0(results.path,network.name,"/",prefixe,"_stocked.txt")) %>% suppressWarnings %>% unlist) %>% unique

  #################
  ## SIMULATIONS ##
  #################

  # set the parallele mode
  plan(multisession, workers = nbworkers)

  # Initialize improvement to TRUE
  improvement <- TRUE
  if(multimode){
    forwardImprovement <- TRUE
    backwardImprovement <- TRUE
  }

  ####### start the simulations
  while(improvement == TRUE){

    mode <- resetmode(mode, multimode, stepwise.summary)

    #################################
    ####### Forward procedure #######
    #################################

    if(mode == "forward"){

      tested_elements <- removePrevious(non.included.elements, crashing_elements, done_elements)
      if(verbose) message("\n Testing independant addition of ", tested_elements %>% length, ' variables')

      aic.list <- future.apply::future_lapply(tested_elements, function(i){

        file.create(paste0(results.path,network.name,"/",prefixe,"_running_",which(tested_elements %>% unlist == i),".txt"))
        cat(i,file=paste0(results.path, network.name,"/", prefixe, "_running_", which(tested_elements %>% unlist == i),".txt"),append=TRUE)

        model.asStringtorun  <- paste0("ergm::ergm(g ~",paste(included.elements %>% unlist, collapse ="+"),"+",i, ")")

        runmodels(model.asStringtorun, i)

      }, future.seed = TRUE) %>% unlist

      # Add previous outputs if any and remove files
      PrevOP <- dealWithPreviousOutPuts(aic.list, tested_elements, done_elements)
      aic.list <- PrevOP[[1]]
      tested_elements <- PrevOP[[2]]
      done_elements <- PrevOP[[3]]
      rm(PrevOP)


      if(multimode &
         ((sum(is.na(aic.list)) == (aic.list %>% length)) | (min(aic.list, na.rm = T) > baseline.aic))){
        message("no improvement in forward step")
        forwardImprovement <- FALSE
      }

      if((sum(is.na(aic.list)) == (aic.list %>% length)) & !multimode){
        improvement <- FALSE
        outputmess <- paste0(c("================== \n No model tested in the last step was analysable ; list of AIC :\n",aic.list, "\n ================== "))
      }

      if(min(aic.list, na.rm = T) > baseline.aic & !multimode){
        improvement <- FALSE
        outputmess <- paste0(c("\n================== \n Last step generated the following AIC list:\n",aic.list,", the model parcimony wasn't improved\n ================== "))
      }

      if(min(aic.list, na.rm = T) <= baseline.aic){
        baseline.aic <- min(aic.list, na.rm = T)
        final.model.asString  <- paste0("ergm::ergm(g ~",paste(included.elements %>% unlist, collapse ="+"),"+",tested_elements[[which.min(aic.list)]], ")")

        # Test the predict function
        if(predict.limit){
          test.predict.function(final.model.asString)
        }

        #  list included variables
        if(verbose) message("The model, initially including ", included.elements %>% length , " variables, ")

        included.elements <- incVar(final.model.asString, bracket = TRUE)

        if(verbose) message("is now including ", included.elements %>% length, "variables")

        #  list non included elements
        if(verbose) message("The model, initially excluding ", non.included.elements %>% length , " variables, ")
        non.included.elements <- no.incVar(elements, included.elements)
        if(verbose) message("is now excluding ", non.included.elements %>% length , "variables ")


        stepwise.summary[[(stepwise.summary %>% length) + 1 ]] <-
          list(formula = final.model.asString,
               min.aic = baseline.aic,
               list.AIC.step = aic.list,
               list.covariable.step = tested_elements %>% unlist,
               mode = mode,
               time = Sys.time())

        save(stepwise.summary,
             file = paste0(results.path,prefixe,"_testfunction_stepwise.summary_",network.name,".rda"))

        print(list(formula = final.model.asString,
                   min.aic = baseline.aic,
                   time = Sys.time(),
                   mode = mode))

        if(multimode) forwardImprovement <- TRUE
      }

    }

    mode <- resetmode(mode, multimode, stepwise.summary)

    if(mode == "backward"){

      tested_elements <- removePrevious(included.elements, crashing_elements, done_elements)
      if(verbose) message("\n Testing independant removal of ", tested_elements %>% length, ' variables')
      aic.list <- future.apply::future_lapply(tested_elements, function(i){

        file.create(paste0(results.path,network.name,"/",prefixe,"_running_",which(tested_elements %>% unlist == i),".txt"))
        cat(i,file=paste0(results.path, network.name,"/", prefixe, "_running_", which(tested_elements %>% unlist == i),".txt"),append=TRUE)

        model.asStringtorun  <- paste0("ergm::ergm(g ~",paste(included.elements[which(included.elements %>% unlist != i)], collapse ="+"),")")

        runmodels(model.asStringtorun, i)

      }, future.seed = TRUE) %>% unlist

      # Add previous outputs if any and remove files
      PrevOP <- dealWithPreviousOutPuts(aic.list, tested_elements, done_elements)
      aic.list <- PrevOP[[1]]
      tested_elements <- PrevOP[[2]]
      done_elements <- PrevOP[[3]]
      rm(PrevOP)

      if(multimode &
         ((sum(is.na(aic.list)) == (aic.list %>% length)) | (min(aic.list, na.rm = T) > baseline.aic))){
        message("no improvement in backward step")
        backwardImprovement <- FALSE
      }

      if((sum(is.na(aic.list)) == (aic.list %>% length)) & !multimode){
        improvement <- FALSE
        outputmess <- paste0(c("================== \n No model tested in the last step was analysable ; list of AIC :\n",aic.list, "\n ================== "))
      }

      if(min(aic.list, na.rm = T) > baseline.aic & !multimode){
        improvement <- FALSE
        outputmess <- paste0(c("\n================== \n Last step generated the following AIC list:\n",aic.list,", the model parcimony wasn't improved\n ================== "))
      }

      if(min(aic.list, na.rm = T) <= baseline.aic){
        baseline.aic <- min(aic.list, na.rm = T)
        final.model.asString  <- paste0("ergm::ergm(g ~",paste(included.elements[which(included.elements %>% unlist != tested_elements[[which.min(aic.list)]])], collapse ="+"),")")

        # Test the predict function
        if(predict.limit){
          test.predict.function(final.model.asString)
        }

        #  list included elements

        if(verbose) message("The model, initially including ", included.elements %>% length , " variables, ")

        included.elements <- incVar(final.model.asString, bracket = TRUE)

        if(verbose) message("is now including ", included.elements %>% length, "variables")

        stepwise.summary[[(stepwise.summary %>% length) + 1 ]] <-
          list(formula = final.model.asString,
               min.aic = baseline.aic,
               list.AIC.step = aic.list,
               list.covariable.step = tested_elements %>% unlist,
               mode = mode,
               time = Sys.time())

        save(stepwise.summary,
             file = paste0(results.path,prefixe,"_testfunction_stepwise.summary_",network.name,".rda"))

        print(list(formula = final.model.asString,
                   min.aic = baseline.aic,
                   time = Sys.time(),
                   mode = mode))

        if(multimode){
          backwardImprovement <- TRUE
          non.included.elements <- no.incVar(elements, included.elements)}
      }
    }

    if(multimode)
      if(sum(forwardImprovement,backwardImprovement) == 0){
        improvement <- FALSE
        outputmess <- paste0(c("\n================== \n Neither adding or removing any variable improved the model parcimony \n =================="))
      }

  }

  cat(outputmess)
}

