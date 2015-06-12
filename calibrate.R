source("estimate.R")
# init step values (deltas) taken from pascal code [(max-min)/steps]*10
delta <- list()


initialDelta <- list()
initialDelta$attraction <- 2
initialDelta$kmcost <- 0.04
initialDelta$VoT <- 0.1
initialDelta$beta <- 0.1
delta$attractionO <-rep(initialDelta$attraction, model$NoR)
delta$attractionD <-rep(initialDelta$attraction, model$NoR)
delta$roadkmcost <- initialDelta$kmcost
delta$railkmcost <- initialDelta$kmcost
delta$iwkmcost <- initialDelta$kmcost
#delta$reliabilitycost
tolerance <- 0.1


`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
`%*=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 * e2))



Twiddle <-function(parameter, delta, model, realFlow) {
  
  eval.parent(substitute(parameter %+=% delta))
  error <-  GetModelQuality(model,realFlow)
  
  if (error < model$bestError) {
    model$bestError<-error
    eval.parent(substitute(delta %*=% 1.1))
    next
  }
    
  eval.parent(substitute(parameter %+=% -2*delta))
  error <-  GetModelQuality(model,realFlow)
  if (error < model$bestError) {
    model$bestError<-error
    eval.parent(substitute(delta %*=% 1.1))
    next
  }
  
  eval.parent(substitute(parameter %+=% delta))
  eval.parent(substitute(delta %*=% 0.9))
  
}

#Normalize to get different resolutions for different parameters

DeltaSize<-function(delta){
  totalSum <- sum(delta$attractionO) / initialDelta$attraction +
              sum(delta$attractionD) / initialDelta$attraction +
              delta$roadkmcost / initialDelta$kmcost +
              delta$railkmcost / initialDelta$kmcost +
              delta$iwkmcost / initialDelta$kmcost
  
  for (commodity in delta$commodities) {
    totalSum <- totalSum +
      commodity$VoT / initialDelta$VoT +
      commodity$beta / initialDelta$beta
  }
  
  totalSum
}


Calibrate<-function(model,realFlow){
  
  #Initialization of parameters
  model$attractionO <-rep(0, model$NoR)
  model$attractionD <-rep(0, model$NoR)
  model$roadkmcost <- 0.1
  model$railkmcost <- 0.1
  model$iwkmcost <- 0.1
  #model$reliabilitycost
  
  model$commodities <- list()
  for (i in 1:10) {
    commodity <- list()
    commodity$VoT <- 0.5
    commodity$beta <- -0.2 
    commodity$id <- as.character(i-1)
    
    model$commodities[[i]] <- commodity
  }
  
  model$flowRoad <- list()
  for ( com in as.character(0:9) ) {
    model$flowRoad[[com]] <- matrix(0, 1000, 1000)
  }
  
  model$flowRail <- list()
  for ( com in as.character(0:9) ) {
    model$flowRail[[com]] <- matrix(0, 1000, 1000)
  }
  
  model$flowIw <- list()
  for ( com in as.character(0:9) ) {
    model$flowIw[[com]] <- matrix(0, 1000, 1000)
  }
  
  
 
  delta$commodities <- list()
  for (i in 1:10) {
    commodity <- list()
    commodity$id <- as.character(i-1)
    commodity$VoT <- initialDelta$VoT
    commodity$beta <- initialDelta$beta
    
    delta$commodities[[i]] <- commodity
  }
  
  model$bestError <- GetModelQuality(model,realFlow)
  for (i in 1:100){
    deltaSize <- DeltaSize(delta)
    print(paste("delta of",delta))
    
    if (deltaSize < tolerance) {
      break
    }
    
    Twiddle(model$roadkmcost, delta$roadkmcost)
    Twiddle(model$railkmcost, delta$railkmcost)
    Twiddle(model$iwkmcost, delta$iwkmcost)
    Twiddle(model$attractionO,delta$attractionO)
    Twiddle(model$attractionD,delta$attractionD)
    for (commodity in delta$commodities){
      Twiddle(commodity$VoT,delta$VoT)
      Twiddle(commodity$beta,delta$beta)
      #TODO finish this 
    }
  }
}