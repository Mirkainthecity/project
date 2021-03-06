Estimate<-function(distance, time, attractionO, attractionD, kmcost, VoT, beta, reliability) {
  
  #m1<-matrix(runif(nrow(time)*ncol(time), nrow(time), ncol(time)))
  cost <- distance * kmcost +
          time * VoT + reliability
        + (attractionO %*% t(rep(1,ncol(distance)))) +
        (rep(1,nrow(distance)) %*% t(attractionD))
  
  beta*cost/10000
  #print(paste("beta*cost", beta*cost/10000))
}

MSE<-function(sim, obs){
  sqrt(sum((sim-obs)^2,na.rm=T))   #RMSE insert root!
}

GetModelQuality<-function(model, realFlow) {
  #print("evaluate")
  quality <- c()
  
  summode<-c()
  
  for (commodity in model$commodities) {
    if (commodity$id != "9") {
      next
    }
    RoC<-list()
    RaC<-list()
    IwC<-list()
    
    #print(paste("Commodity", commodity$id))
    RoC <- Estimate(
      model$distanceRoad,
      model$timeRoad,
      model$roadAttractionO,
      model$roadAttractionD,
      0.14, #model$roadkmcost,
      100,#commodity$VoT,
      -6,#commodity$beta,
      100#model$roadReliability #commodity$VoR 
    )
    RaC <- Estimate(
      model$distanceRail,
      model$timeRail,
      model$railAttractionO,
      model$railAttractionD,
      0.08, #model$railkmcost,
      40,#commodity$VoT,
      -6,#commodity$beta,
      20#model$railReliability #commodity$VoR 
    )
    IwC <- Estimate(
      model$distanceIw,
      model$timeIw,
      model$iwwAttractionO,
      model$iwwAttractionD,
      0.01, #model$iwkmcost,
      40,#commodity$VoT,
      -6,#commodity$beta,
      20#model$iwwReliability #commodity$VoR 
    )
    
    #Prevent Inf or zeros
  highnumber=700

  rapply(RoC, f=function(RoC) ifelse(abs(RoC) > highnumber,highnumber,RoC), how="replace" )
  rapply(RaC, f=function(RaC) ifelse(abs(RaC) > highnumber,highnumber,RaC), how="replace" )
  rapply(IwC, f=function(IwC) ifelse(abs(IwC) > highnumber,highnumber,IwC), how="replace" )
  
    RoP<-exp(RoC)
    RaP<-exp(RaC)
    IwP<-exp(IwC)
    
    
    PSum <- RoP + RaP + IwP
    
    if(sum(PSum == Inf, na.rm=T) > 0) {
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
   
    

    commodity$id<-"9" #Take only 9th into account
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

    quality <- c(quality, mean(c(qualityRoad, qualityRail, qualityIw)))
   
    #return(summode)
    return(list(m=model, q=mean(quality)))
  }    
  
  #print("evaluate finished")
  #list(mean=mean(quality), errors=quality)#mean and per commodity
  return(list(m=model, q=mean(quality)))
  #return(summode)
  
  #return(sqrt(quality))

 
}
