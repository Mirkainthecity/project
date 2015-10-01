source("estimate.R")
source("themodel.R")


####Initialize deltas######

delta <- list()

initialDelta <- list()

#initialDelta$attraction <- 20
#initialDelta$kmcost <- 0.04
initialDelta$VoT <- 0.1
initialDelta$beta <- 0.1
initialDelta$VoR<-0.1
delta$railReliability<-initialDelta$VoR
delta$roadReliability<-initialDelta$VoR
delta$iwwReliability<-initialDelta$VoR


#delta$roadAttractionO <-rep(initialDelta$attraction, n)
#delta$railAttractionO <-rep(initialDelta$attraction, n)
#delta$iwwAttractionO <-rep(initialDelta$attraction, n)
#delta$roadAttractionD <-rep(initialDelta$attraction, n)
#delta$railAttractionD <-rep(initialDelta$attraction, n)
#delta$iwwAttractionD <-rep(initialDelta$attraction, n)
#delta$roadkmcost <- initialDelta$kmcost
#delta$railkmcost <- initialDelta$kmcost
#delta$iwkmcost <- initialDelta$kmcost

delta$commodities <- list()
for (i in 1:10) { #used to be for (i in 1:10)
  commodity <- list()
  commodity$id <- as.character(i-1)
  commodity$VoT <- initialDelta$VoT
  commodity$beta <- initialDelta$beta
  
  delta$commodities[[i]] <- commodity
}


tolerance <- 0.001


#Normalize to get different resolutions for different parameters

DeltaSize<-function(delta) {
  totalSum <- c(delta$railReliability/initialDelta$VoR,
                   delta$roadReliability/initialDelta$VoR,
                   delta$iwwReliability/initialDelta$VoR)
    #mean(delta$roadAttractionO / initialDelta$attraction),
    #mean(delta$railAttractionO / initialDelta$attraction),
    #mean(delta$iwwAttractionO / initialDelta$attraction),
    #mean(delta$roadAttractionD / initialDelta$attraction),
    #mean(delta$railAttractionD / initialDelta$attraction),
    #mean(delta$iwwAttractionD / initialDelta$attraction),
    #delta$roadkmcost / initialDelta$kmcost,
    #delta$railkmcost / initialDelta$kmcost,
    #delta$iwkmcost / initialDelta$kmcost)
  
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
    GetModelQuality(model,realFlow)
  }
  
  Twiddle <-function(parameter, model, delta, i=1, j=1, minRange=-Inf, maxRange=+Inf) {
    print(paste("error:", model$bestError))
    print(paste(parameter,model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] + delta[[parameter]][[i]][[j]]
    if (model[[parameter]][[i]][[j]] < maxRange) {
      
      result<-list()
      result <- evaluate(model)
      error <- result$q
      model <- result$m
      print(model$bestError)
      
      if (error < model$bestError) {
        model$bestError<-error
        delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] *(1.1)
        print(paste("parameter increased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
        
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
        print(paste("parameter decreased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], error))
        return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=error, m=model))
      }
    }
    
    model[[parameter]][[i]][[j]] <- model[[parameter]][[i]][[j]] + delta[[parameter]][[i]][[j]]
    delta[[parameter]][[i]][[j]] <- delta[[parameter]][[i]][[j]] * (0.9)
    
    print(paste("delta decreased",model[[parameter]][[i]][[j]], delta[[parameter]][[i]][[j]], model$bestError))
    return(list(p=model[[parameter]][[i]][[j]], d=delta[[parameter]][[i]][[j]], e=model$bestError, m=model))
  }
  

  #result is a list()
  result <- evaluate(model)
  model <- result$m
  model$bestError <- result$q


  
 for (j in 1:1000) { #Number of iterations
    deltaSize <- DeltaSize(delta)
    print(paste("############# delta size",deltaSize, "[",j,"] #############"))
    
    if (deltaSize < tolerance) {
      break
    }
    
    #print(model$roadkmcost)
    #print(delta$roadkmcost)
    #result <- Twiddle( "roadkmcost", model, delta, 1, 1, 0, 2)
    #model <- result$m
    #model$roadkmcost <- result$p
    #model$bestError <- result$e
    #delta$roadkmcost<- result$d
    
    #result <- Twiddle( "railkmcost", model, delta, 1, 1, 0, 2)
    #model <- result$m
    #model$railkmcost <- result$p
    #model$bestError <- result$e
    #delta$railkmcost<- result$d
    
    #result <- Twiddle( "iwkmcost", model, delta, 1, 1, 0, 2)
    #model <- result$m
    #model$iwkmcost <- result$p
    #model$bestError <- result$e
    #delta$iwkmcost<- result$d
    
    
    for (i in 10) {#used to be for (i in 1:10)
       
      #model$commodities[[i]]$VoT
      result <- Twiddle( "commodities", model, delta, i, "VoT") ##No Limit!!
      model <- result$m
      model$commodities[[i]]$VoT <- result$p
      model$bestError <- result$e
      delta$commodities[[i]]$VoT<- result$d
      
      result <- Twiddle( "commodities", model, delta, i, "beta")
      model <- result$m
      model$commodities[[i]]$beta <- result$p
      model$bestError <- result$e
      delta$commodities[[i]]$beta <- result$d  
      
      #result <- Twiddle( "commodities", model, delta, i, "VoR",-1, 1)
      #model <- result$m
      #model$commodities[[i]]$VoR <- result$p
      #model$bestError <- result$e
      #delta$commodities[[i]]$VoR<- result$d
    }
    
   #for (r in c("roadReliability", "railReliability", "iwwReliability")) {
         #result <- Twiddle( r, model, delta, 1, 1)
         #model[[r]] <- result$p
         #model$bestError <- result$e
         #delta[[r]] <- result$d
        #}
      
  ##  No Limits for Reliability  ###
   result <- Twiddle( "roadReliability", model, delta, 1, 1)
   model <- result$m
   model$roadReliability <- result$p
   model$bestError <- result$e
   delta$roadReliability<- result$d
   
   result <- Twiddle( "railReliability", model, delta, 1, 1)
   model <- result$m
   model$railReliability <- result$p
   model$bestError <- result$e
   delta$railReliability<- result$d
   
   result <- Twiddle( "iwwReliability", model, delta, 1, 1)
   model <- result$m
   model$iwwReliability <- result$p
   model$bestError <- result$e
   delta$iwwReliability<- result$d
   
    
    #if (j %% 10 != 0) next
    #for (i in 1:model$NoR) {
      
      # model$attractionO[i]
     # result <- Twiddle( "roadAttractionO", model, delta, i, 1, -500, 500)
      #model <- result$m
      #model$roadAttractionO[[i]] <- result$p
      #model$bestError <- result$e
      #delta$roadAttractionO[[i]]<- result$d
      
      #result <- Twiddle( "railAttractionO", model, delta, i, 1, -500, 500)
      #model <- result$m
      #model$railAttractionO[[i]] <- result$p
      #model$bestError <- result$e
      #delta$railAttractionO[[i]]<- result$d
      
      #result <- Twiddle( "iwwAttractionO", model, delta, i, 1, -500, 500)
      #model <- result$m
      #model$iwwAttractionO[[i]] <- result$p
      #model$bestError <- result$e
      #delta$iwwAttractionO[[i]]<- result$d
      
      #result <- Twiddle( "roadAttractionD", model, delta, i, 1, -500, 500)
      #model <- result$m
      #model$roadAttractionD[[i]] <- result$p
      #model$bestError <- result$e
      #delta$roadAttractionD[[i]]<- result$d
      
      #result <- Twiddle( "railAttractionD", model, delta, i, 1, -500, 500)
      #model <- result$m
      #model$railAttractionD[[i]] <- result$p
      #model$bestError <- result$e
      #delta$railAttractionD[[i]]<- result$d
      
      #result <- Twiddle( "iwwAttractionD", model, delta, i, 1, -500, 500)
      #model <- result$m
      #model$iwwAttractionD[[i]] <- result$p
      #model$bestError <- result$e
      #delta$iwwAttractionD[[i]]<- result$d
      
    #}
 }
  return(model)

}