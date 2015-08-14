source("estimate2.R")
source("startmodel.R")

###Initialize deltas###

delta <- list()
initialDelta <- list()
tolerance <- 0.1
initialDelta$kmcost <- 0.04
delta$roadkmcost <- initialDelta$kmcost
delta$railkmcost <- initialDelta$kmcost
delta$iwkmcost <- initialDelta$kmcost
initialDelta$attraction <- 2
delta$attractionO <-rep(initialDelta$attraction, n)
delta$attractionD <-rep(initialDelta$attraction, n)
initialDelta$VoT <- 1
initialDelta$beta <- 1

delta$commodities <- list()
for (i in 1:10) {
  commodity <- list()
  commodity$id <- as.character(i-1)
  commodity$VoT <- initialDelta$VoT
  commodity$beta <- initialDelta$beta
  
  delta$commodities[[i]] <- commodity
}

Calibrate2<-function(model,realFlow){
  evaluate <- function(model) {
    GetModelQuality(model,realFlow)
  }

  Twiddle <-function(parameter, model, delta, i=1, j=1) {
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] + delta[[parameter]][[i]][[j]]
    error <- evaluate(model)
    
    if (error < model$bestError) {
      model$bestError<-error
      delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] *(1.1)
      print(paste("value increased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
      return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=error))
    }
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] - 2*delta[[parameter]][[i]][[j]]
    error <-  evaluate(model)
    
    if (error < model$bestError) {
      delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] *(1.1)
      print(paste("value decreased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], error))
      return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=error))
    }
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] + delta[[parameter]][[i]][[j]]
    delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] * (0.9)
    
    return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=model$bestError))
  } 
  model$bestError <- evaluate(model)
  result <- Twiddle( "roadkmcost", model, delta)
  model$roadkmcost <- result$p
  model$bestError <- result$e
  delta$roadkmcost<- result$d
  
  result <- Twiddle( "railkmcost", model, delta)
  model$railkmcost <- result$p
  model$bestError <- result$e
  delta$railkmcost<- result$d
  
  result <- Twiddle( "iwkmcost", model, delta)
  model$iwkmcost <- result$p
  model$bestError <- result$e
  delta$iwkmcost<- result$d
  
  return("iwkmcost")

}

  
  
  
  
  
  


  

