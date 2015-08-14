#source("estimate.R")
source("estimate.R")
source("startmodel.R")
# init step values (deltas) taken from pascal code [(max-min)/steps]*10
delta <- list()

initialDelta <- list()
initialDelta$attraction <- 2
initialDelta$kmcost <- 0.04
initialDelta$VoT <- 1
initialDelta$beta <- 1
delta$attractionO <-rep(initialDelta$attraction, n)
delta$attractionD <-rep(initialDelta$attraction, n)
delta$roadkmcost <- initialDelta$kmcost
delta$railkmcost <- initialDelta$kmcost
delta$iwkmcost <- initialDelta$kmcost

#delta$railReliability<-matrix(initialDelta$reliability,n,n)
#delta$roadReliability<-matrix(initialDelta$reliability,n,n)
#delta$iwwReliability<-matrix(initialDelta$reliability,n,n)
tolerance <- 0.1


#`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
#`%*=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 * e2))

#Normalize to get different resolutions for different parameters

DeltaSize<-function(delta) {
  totalSum <- c(
    #mean(delta$attractionO / initialDelta$attraction),
    #mean(delta$attractionD / initialDelta$attraction),
    #mean(delta$roadReliability / initialDelta$reliability),
    #mean(delta$railReliability / initialDelta$reliability),
    #mean(delta$iwwReliability / initialDelta$reliability),
    delta$roadkmcost / initialDelta$kmcost,
    delta$railkmcost / initialDelta$kmcost,
    delta$iwkmcost / initialDelta$kmcost)
  
  #for (commodity in delta$commodities) {
  #    totalSum <- c (totalSum,
  #    commodity$VoT / initialDelta$VoT,
  #    commodity$beta / initialDelta$beta)
  #}
  
  mean(totalSum)
}


Calibrate<-function(model,realFlow){
  
  evaluate <- function(model) {
    print("evaluate")
    GetModelQuality(model,realFlow)
  }
  
  Twiddle <-function(parameter, model, delta, i=1, j=1, maxRange=+Inf, minRange=-Inf) {
    print(paste("error:", model$bestError))
    print(paste(parameter,model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] + delta[[parameter]][[i]][[j]]
    if (model[[parameter]][[i]][[j]] < maxRange) {
    
      result <- evaluate(model)
      error <- result$q
      model <- result$m
      print(model$bestError)
      
      if (error < model$bestError) {
        model$bestError<-error
        delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] *(1.1)
        print(paste("value increased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
        
        return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=error, m=model))
      }
    }
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] - 2*delta[[parameter]][[i]][[j]]
    
    if (model[[parameter]][[i]][[j]] > minRange) {  
    
      result <- evaluate(model)
      error <- result$q
      model <- result$m
      
      
      if (error < model$bestError) {
        delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] *(1.1)
        print(paste("value decreased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], error))
        return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=error, m=model))
      }
    }
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] + delta[[parameter]][[i]][[j]]
    delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] * (0.9)
    
    print(paste("delta decreased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
    return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=model$bestError, m=model))
  }
  
  #Initialization of parameters
  #model$railAttractionO <-rep(0, n)
  #model$roadAttractionO <-rep(0, n)
  #model$iwwAttractionO <-rep(0, n)
  #model$railAttractionD <-rep(0, n)
  #model$roadAttractionD <-rep(0, n)
  #model$iwwAttractionD <-rep(0, n)
 # model$roadReliability<-matrix(1,n,n)
 # model$railReliability<-matrix(1,n,n)
 # model$iwwReliability<-matrix(1,n,n)
  #model$reliabilitycost
  
 
  ####Why????######
  
  model$flowRoad <- list()
  for ( com in as.character(0:9) ) {
    model$flowRoad[[com]] <- matrix(0, n, n)
  }
  
  model$flowRail <- list()
  for ( com in as.character(0:9) ) {
    model$flowRail[[com]] <- matrix(0, n, n)
  }
  
  model$flowIw <- list()
  for ( com in as.character(0:9) ) {
    model$flowIw[[com]] <- matrix(0, n, n)
  }
 ####Why????######
 
 
  delta$commodities <- list()
  for (i in 1:10) {
    commodity <- list()
    commodity$id <- as.character(i-1)
    commodity$VoT <- initialDelta$VoT
    commodity$beta <- initialDelta$beta
    
    delta$commodities[[i]] <- commodity
  }
  
  result <- evaluate(model)
  model <- result$m
  model$bestError <- result$q
  for (j in 1:100) {
    deltaSize <- DeltaSize(delta)
    print(paste("############# delta size",deltaSize, "[",j,"] #############"))
    
    if (deltaSize < tolerance) {
      break
    }
    
    #print(model$roadkmcost)
    #print(delta$roadkmcost)
    result <- Twiddle( "roadkmcost", model, delta, 1, 1, +Inf, 0)
    model <- result$m
    model$roadkmcost <- result$p
    model$bestError <- result$e
    delta$roadkmcost<- result$d
    
    result <- Twiddle( "railkmcost", model, delta, 1, 1, +Inf, 0)
    model <- result$m
    model$railkmcost <- result$p
    model$bestError <- result$e
    delta$railkmcost<- result$d
    
    result <- Twiddle( "iwkmcost", model, delta, 1, 1, +Inf, 0)
    model <- result$m
    model$iwkmcost <- result$p
    model$bestError <- result$e
    delta$iwkmcost<- result$d
    
    
    #for (i in 1:10) {
    #  # model$commodities[[i]]$VoT
    #  result <- Twiddle( "commodities", model, delta, i, "VoT", +Inf, 0)
    #  model$commodities[[i]]$VoT <- result$p
    #  model$bestError <- result$e
    #  delta$commodities[[i]]$VoT<- result$d
    #  
    #  result <- Twiddle( "commodities", model, delta, i, "beta")
    #  model$commodities[[i]]$beta <- result$p
    #  model$bestError <- result$e
    #  delta$commodities[[i]]$beta <- result$d  
    #}
    
    
   # for (r in c("roadReliability", "railReliability", "iwwReliability")) {
         # result <- Twiddle( r, model, delta, i, j)
         # model[[r]][i][j] <- result$p
         # model$bestError <- result$e
         # delta[[r]][i][j] <- result$d
        #}
      #}
    #}
    
    
    #if (j %% 10 != 0) next
    #for (i in 1:model$NoR) {
      # model$attractionO[i]
     # result <- Twiddle( "attractionO", model, delta, i)
     # model$attractionO[[i]] <- result$p
    #  model$bestError <- result$e
     # delta$attractionO[[i]]<- result$d
    #  
    #  result <- Twiddle( "attractionD", model, delta, i)
    #  model$attractionD[[i]] <- result$p
    #  model$bestError <- result$e
    #  delta$attractionD[[i]]<- result$d
   
  }
  return(model)
}