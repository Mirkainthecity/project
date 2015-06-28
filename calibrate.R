source("estimate.R")
# init step values (deltas) taken from pascal code [(max-min)/steps]*10
delta <- list()


initialDelta <- list()
initialDelta$attraction <- 2
initialDelta$kmcost <- 0.04
initialDelta$VoT <- 1
initialDelta$beta <- 1
initialDelta$reliability <- 0.1
delta$attractionO <-rep(initialDelta$attraction, model$NoR)
delta$attractionD <-rep(initialDelta$attraction, model$NoR)
delta$roadkmcost <- initialDelta$kmcost
delta$railkmcost <- initialDelta$kmcost
delta$iwkmcost <- initialDelta$kmcost
delta$railReliability<-matrix(initialDelta$reliability,model$NoR,model$NoR)
delta$roadReliability<-matrix(initialDelta$reliability,model$NoR,model$NoR)
delta$iwwReliability<-matrix(initialDelta$reliability,model$NoR,model$NoR)
tolerance <- 0.1


#`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
#`%*=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 * e2))

#Normalize to get different resolutions for different parameters

DeltaSize<-function(delta){
  totalSum <- c(
    #mean(delta$attractionO / initialDelta$attraction),
    #mean(delta$attractionD / initialDelta$attraction),
    #mean(delta$roadReliability / initialDelta$reliability),
    #mean(delta$railReliability / initialDelta$reliability),
    #mean(delta$iwwReliability / initialDelta$reliability),
    delta$roadkmcost / initialDelta$kmcost,
    delta$railkmcost / initialDelta$kmcost,
    delta$iwkmcost / initialDelta$kmcost)
  
  for (commodity in delta$commodities) {
    totalSum <- c (totalSum,
      commodity$VoT / initialDelta$VoT,
      commodity$beta / initialDelta$beta)
  }
  
  mean(totalSum)
}


Calibrate<-function(model,realFlow){
  
  evaluate <- function(model) {
    print("evaluate")
    #print(model$attractionO)
    GetModelQuality(model,realFlow)
  }
  
  Twiddle <-function(parameter, model, delta, i=1, j=1) {
    print(paste("error:                 ", model$bestError))
    print(paste(parameter,model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] + delta[[parameter]][[i]][[j]]
    
    error <- evaluate(model)
    print(paste("error (increased value):", error))
    
    if (error < model$bestError) {
      model$bestError<-error
      delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] *(1.1)
      print(paste("value increased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
      return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=error))
    }
    
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] - 2*delta[[parameter]][[i]][[j]]
    
    error <-  evaluate(model)
    print(paste("error (decresed value): ", error))
    if (error < model$bestError) {
      delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] *(1.1)
      print(paste("value decreased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], error))
      return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=error))
    }
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] + delta[[parameter]][[i]][[j]]
    delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] * (0.9)
    
    print(paste("delta decreased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
    return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=model$bestError))
  }
  
  #Initialization of parameters
  #model$railAttractionO <-rep(0, model$NoR)
  #model$roadAttractionO <-rep(0, model$NoR)
  #model$iwwAttractionO <-rep(0, model$NoR)
  #model$railAttractionD <-rep(0, model$NoR)
  #model$roadAttractionD <-rep(0, model$NoR)
  #model$iwwAttractionD <-rep(0, model$NoR)
 # model$roadReliability<-matrix(1,model$NoR,model$NoR)
 # model$railReliability<-matrix(1,model$NoR,model$NoR)
 # model$iwwReliability<-matrix(1,model$NoR,model$NoR)
  model$roadkmcost <- 0.1
  model$railkmcost <- 0.1
  model$iwkmcost <- 0.1
  #model$reliabilitycost
  
  #road attraction
  model$roadAttractionO<-d$value[d$OD=="Origin" & d$mode=="Road"]
  model$roadAttractionD<-d$value[d$OD=="Destination" & d$mode=="Road"]
  
  #rail attraction
  model$railAttractionO<-d$value[d$OD=="Origin" & d$mode=="Rail"]
  model$railAttractionD<-d$value[d$OD=="Destination" & d$mode=="Rail"]
  
  #iww attraction
  model$iwwAttractionO<-d$value[d$OD=="Origin" & d$mode=="IWW"]
  model$iwwAttractionD<-d$value[d$OD=="Destination" & d$mode=="IWW"]
  
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
    model$flowRoad[[com]] <- matrix(0, 314, 314)
  }
  
  model$flowRail <- list()
  for ( com in as.character(0:9) ) {
    model$flowRail[[com]] <- matrix(0, 314, 314)
  }
  
  model$flowIw <- list()
  for ( com in as.character(0:9) ) {
    model$flowIw[[com]] <- matrix(0, 314, 314)
  }
  
  delta$commodities <- list()
  for (i in 1:10) {
    commodity <- list()
    commodity$id <- as.character(i-1)
    commodity$VoT <- initialDelta$VoT
    commodity$beta <- initialDelta$beta
    
    delta$commodities[[i]] <- commodity
  }
  
  model$bestError <- evaluate(model)
  for (j in 1:10){
    deltaSize <- DeltaSize(delta)
    print(paste("delta size",deltaSize, "[",j,"]"))
    
    if (deltaSize < tolerance) {
      break
    }
    
    print(model$roadkmcost)
    print(delta$roadkmcost)
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
    
    for (i in 1:10){
      # model$commodities[[i]]$VoT
      result <- Twiddle( "commodities", model, delta, i, "VoT")
      model$commodities[[i]]$VoT <- result$p
      model$bestError <- result$e
      delta$commodities[[i]]$VoT<- result$d
      
      result <- Twiddle( "commodities", model, delta, i, "beta")
      model$commodities[[i]]$beta <- result$p
      model$bestError <- result$e
      delta$commodities[[i]]$beta <- result$d
      
    }
    
    
   # for (r in c("roadReliability", "railReliability", "iwwReliability")) {
     # for (i in 1:model$NoR){
      #  for (j in 1:model$NoR){
         # result <- Twiddle( r, model, delta, i, j)
         # model[[r]][i][j] <- result$p
         # model$bestError <- result$e
         # delta[[r]][i][j] <- result$d
        }
      }
    }
    
    
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
    #}
  #}
  return(model)
}
