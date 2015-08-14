###Set-up given data into a model###

model <- list()
scale_distance <- 1000
n <- 10
model$NoR <- n 

model$distanceIw<-read.csv("IWWDistance_v2.csv", header=F,sep=";")[1:n, 1:n]
model$timeIw<-read.csv("IWWTime_v2.csv", header=F,sep=";")[1:n, 1:n]
model$distanceRail<-read.csv("RailDistance_v2.csv", header=F,sep=";")[1:n, 1:n]
model$timeRail<-read.csv("RailTime_v2.csv", header=F,sep=";")[1:n, 1:n]
model$distanceRoad<-read.csv("RoadDistance_v2.csv", header=F,sep=";")[1:n, 1:n]
model$timeRoad<-read.csv("RoadTime_v2.csv", header=F,sep=";")[1:n, 1:n]

MAX_VALUE <- 99999 
model$distanceIw[model$distanceIw>=MAX_VALUE] <- NA
model$distanceRail[model$distanceRail>=MAX_VALUE] <- NA
model$distanceRoad[model$distanceRoad>=MAX_VALUE] <- NA

model$timeIw[model$timeIw>=MAX_VALUE] <- NA
model$timeRail[model$timeRail>=MAX_VALUE] <- NA
model$timeRoad[model$timeRoad>=MAX_VALUE] <- NA

###VoT per commodity taken from Igor
vot<-read.csv("VoT_resi.csv", header=F)
vot<-vot[,c(1,3)]
colnames(vot)<-c("parameter", "value")

###BETA per commodity taken from Igor
beti<-read.csv("alpha_resi.csv", header=F)
colnames(beti)<-c("parameter", "value")


model$commodities <- list()
for (i in 1:10) {
  commodity <- list()
  commodity$VoT <- vot$value[i]
  commodity$beta <- beti$value[i]
  commodity$id <- as.character(i-1)#since commodities are numbered between 0-9
  
  model$commodities[[i]] <- commodity
}
###Attraction parameters taken from Igor's results###

attr<-read.table("ParametersF_v1.csv", header=F, nrows=1884)#attraction parameters taken from Igor
colnames(attr)<-c("node", "OD", "mode", "value")

#road attraction
model$roadAttractionO<-attr$value[attr$OD=="Origin" & attr$mode=="Road"][1:n]
model$roadAttractionD<-attr$value[attr$OD=="Destination" & attr$mode=="Road"][1:n]

#rail attraction
model$railAttractionO<-attr$value[attr$OD=="Origin" & attr$mode=="Rail"][1:n]
model$railAttractionD<-attr$value[attr$OD=="Destination" & attr$mode=="Rail"][1:n]

#iww attraction
model$iwwAttractionO<-attr$value[attr$OD=="Origin" & attr$mode=="IWW"][1:n]
model$iwwAttractionD<-attr$value[attr$OD=="Destination" & attr$mode=="IWW"][1:n]

###km-costs initialized
model$roadkmcost <- 0.1
model$railkmcost <- 0.1
model$iwkmcost <- 0.1

source("themodel.R")
#Real flow for O-D pairs per commodity type, per mode
#loadRealFlow <- function() {...



###Estimate costs###
source("estimate2.R")#call estimate function

###Get the squared error between predicted and real flows###
GetModelQuality<-function(model, realFlow) {
  #print("evaluate")
  quality <- 0
  for (commodity in model$commodities) {
    #print(paste("Commodity", commodity$id))
    RoC <- Estimate(
      model$distanceRoad,
      model$timeRoad,
      model$roadAttractionO,
      model$roadAttractionD,
      model$roadkmcost,
      commodity$VoT,
      commodity$beta,
      1#model$roadReliability
    )
    RaC <- Estimate(
      model$distanceRail,
      model$timeRail,
      model$railAttractionO,
      model$railAttractionD,
      model$railkmcost,
      commodity$VoT,
      commodity$beta,
      1#model$railReliability
    )
    IwC <- Estimate(
      model$distanceIw,
      model$timeIw,
      model$iwwAttractionO,
      model$iwwAttractionD,
      model$iwkmcost,
      commodity$VoT,
      commodity$beta,
      1#model$iwwReliability
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
    
    quality <- sum(quality, qualityRoad, qualityRail, qualityIw)/n  #taking the mean here.
  }    # arrived here to change into matrix: [origin,destination] removed after every [[commodity$id]]
  
  #print("evaluate finished")
  #list(mean=mean(quality), errors=quality)#mean and per commodity
  sqrt(quality)
  return(list(m=model, q=quality))
}
