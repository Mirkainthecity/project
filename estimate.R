Estimate<-function(distance, time, attractionO, attractionD, kmcost, VoT, beta) {
  if (beta == 0) return((0*distance) + 1)
  
  cost <- distance * kmcost +
          time * VoT * reliability+
          #reliability * reliabilityCost +
          (attractionO %*% t(rep(1,ncol(distance)))) +
          (rep(1,nrow(distance)) %*% t(attractionD))
  
  exp(beta * cost)
}
#blabalabla

MSE<-function(sim, obs){
  sum((sim-obs)^2,na.rm=T)
}

GetModelQualityOld<-function(model, realFlow) {
  print("evaluate")
  quality <- 0
  for (origin in seq(model$NoR)) {
    for (destination in seq(model$NoR)) {
      for (commodity in model$commodities) {
        RoP <- Estimate(
          model$distanceRoad[origin,destination],
          model$timeRoad[origin,destination],
          #model$reliability[origin,destination],
          model$attractionO[origin],
          model$attractionD[destination],
          model$roadkmcost,
          #model$reliabilitycost,
          commodity$VoT,
          commodity$beta
        )
        RaP <- Estimate(
          model$distanceRail[origin,destination],
          model$timeRail[origin,destination],
          #model$reliability[origin,destination],
          model$attractionO[origin],
          model$attractionD[destination],
          model$railkmcost,
          #model$reliabilitycost,
          commodity$VoT,
          commodity$beta
        )
        IwP <- Estimate(
          model$distanceIw[origin,destination],
          model$timeIw[origin,destination],
          #model$reliability[origin,destination],
          model$attractionO[origin],
          model$attractionD[destination],
          model$iwkmcost,
          #model$reliabilitycost,
          commodity$VoT,
          commodity$beta
        )
        
        PSum <- RoP + RaP + IwP
        if (PSum == 0) {
          RoP <- 0; RaP <- 0; IwP <- 0;
        } else {
          RoP <- RoP / PSum
          RaP <- RaP / PSum
          IwP <- IwP / PSum
        }
        
        totalFlow <- realFlow$road[[commodity$id]][origin, destination] +
          realFlow$iw[[commodity$id]][origin, destination]
        
        if (commodity$id == "9") {
          totalFlow = realFlow$rail[[commodity$id]][origin, destination] + totalFlow
        }
        
        model$flowRoad[[commodity$id]][origin,destination] <-
          totalFlow * RoP
        
        model$flowRail[[commodity$id]][origin,destination] <-
          totalFlow * RaP
        
        model$flowIw[[commodity$id]][origin,destination] <-
          totalFlow * IwP
        
      
        quality<- quality + MSE(
          c(model$flowRoad[[commodity$id]][origin,destination],
            model$flowRail[[commodity$id]][origin,destination],
            model$flowIw[[commodity$id]][origin,destination]),
          
          c(realFlow$road[[commodity$id]][origin, destination],
            realFlow$rail[[commodity$id]][origin, destination],
            realFlow$iw[[commodity$id]][origin, destination])
        )     
      }
    }
  }
  
  print("evaluate finished")
  sqrt(quality)
}

GetModelQuality<-function(model, realFlow) {
  #print("evaluate")
  quality <- 0
  for (commodity in model$commodities) {
    #print(paste("Commodity", commodity$id))
    RoP <- Estimate(
      model$distanceRoad,
      model$timeRoad,
      #model$reliability[origin,destination],
      model$roadAttractionO,
      model$roadAttractionD,
      model$roadkmcost,
      #model$reliabilitycost,
      commodity$VoT,
      commodity$beta
    )
    RaP <- Estimate(
      model$distanceRail,
      model$timeRail,
      #model$reliability[origin,destination],
      model$railAttractionO,
      model$railAttractionD,
      model$railkmcost,
      #model$reliabilitycost,
      commodity$VoT,
      commodity$beta
    )
    IwP <- Estimate(
      model$distanceIw,
      model$timeIw,
      #model$reliability[origin,destination],
      model$iwwAttractionO,
      model$iwwAttractionD,
      model$iwkmcost,
      #model$reliabilitycost,
      commodity$VoT,
      commodity$beta
    )
    
    PSum <- RoP + RaP + IwP
    
    if(sum(PSum == Inf) > 0) {
        print("#### discarding because of infinite value found ####")
        return (Inf) #discard infinite values
    }
 
    RoP <- RoP / PSum
    RaP <- RaP / PSum
    IwP <- IwP / PSum
    
    #RoP[is.nan(RoP)] <- 0
    #RaP[is.nan(RaP)] <- 0
    #IwP[is.nan(IwP)] <- 0
    rapply(RoP, f=function(RoP) ifelse(is.nan(RoP),0,RoP), how="replace" )#replace NaN's to 0
    rapply(RaP, f=function(RaP) ifelse(is.nan(RaP),0,RaP), how="replace" )
    rapply(IwP, f=function(IwP) ifelse(is.nan(IwP),0,IwP), how="replace" )
   
    

    
    totalFlow <- realFlow$road[[commodity$id]] +
      realFlow$rail[[commodity$id]] +
      realFlow$iw[[commodity$id]]
    
    model$flowRoad[[commodity$id]] <-
      totalFlow * RoP
    
    model$flowRail[[commodity$id]] <-
      totalFlow * RaP
    
    model$flowIw[[commodity$id]] <-
      totalFlow * IwP
    
    
    qualityRoad <- MSE(
      model$flowRoad[[commodity$id]],
      realFlow$road[[commodity$id]]
    )
    
    qualityRail <- MSE(
        model$flowRail[[commodity$id]],
        realFlow$rail[[commodity$id]]
    )

    qualityIw <- MSE(
      model$flowIw[[commodity$id]],
      realFlow$iw[[commodity$id]]
    )

    quality <- sum(quality, qualityRoad, qualityRail, qualityIw)
  }    # arrived here to change into matrix: [origin,destination] removed after every [[commodity$id]]
  
  #print("evaluate finished")
  #list(mean=mean(quality), errors=quality)#mean and per commodity
  sqrt(quality)
}
