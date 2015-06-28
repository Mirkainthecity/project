#cOMPARE MODEL TO Igor's parameters



d<-read.table("ParametersF_v1.csv", header=F, nrows=1884)
colnames(d)<-c("node", "OD", "mode", "value")
e<-read.csv("VoT_resi.csv", header=F)
e<-e[,c(1,3)]
colnames(e)<-c("parameter", "value")
f<-read.csv("alpha_resi.csv", header=F)
colnames(f)<-c("parameter", "value")
#model <- createModel()
#realFlow<-loadRealFlow()

  model$roadkmcost <- 0.05
  model$railkmcost <- 0.03
  model$iwkmcost <- 0.01
  #model$reliabilitycost
  
  model$commodities <- list()
  for (i in 1:10) {
    commodity <- list()
    commodity$VoT <- e$value[i]
    commodity$beta <- f$value[i]
    commodity$id <- as.character(i-1)
    
    model$commodities[[i]] <- commodity
  }
  
  
 #road attraction
 model$roadAttractionO<-d$value[d$OD=="Origin" & d$mode=="Road"]
 model$roadAttractionD<-d$value[d$OD=="Destination" & d$mode=="Road"]

 #rail attraction
 model$railAttractionO<-d$value[d$OD=="Origin" & d$mode=="Rail"]
 model$railAttractionD<-d$value[d$OD=="Destination" & d$mode=="Rail"]
 
 #iww attraction
 model$iwwAttractionO<-d$value[d$OD=="Origin"][d$mode=="IWW"]
 model$iwwAttractionD<-d$value[d$OD=="Destination"][d$mode=="IWW"]
  
  
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



Estimate<-function(distance, time, attractionO, attractionD, kmcost, VoT, beta) {
  if (beta == 0) return(1)
  
  cost <- distance * kmcost +
    time * VoT +
    #reliability * reliabilityCost +
    (attractionO %*% t(rep(1,ncol(distance)))) +
    (rep(1,nrow(distance)) %*% t(attractionD))
  
  exp(beta * cost)
}


MSE<-function(sim, obs){
  sum((sim-obs)^2,na.rm=T)
}

GetModelFlow<-function(model, realFlow) {
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
  #sqrt(quality)

  list(model$flowRoad, model$flowRail, model$flowIw)
} 

