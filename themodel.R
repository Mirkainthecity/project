#setwd("C:/Users/Mirka/Desktop/TNO_Thesis/stage/SM/Data")

d<-read.table("ParametersF_v1.csv", header=F, nrows=1884)
colnames(d)<-c("node", "OD", "mode", "value")
e<-read.csv("VoT_resi.csv", header=F)
e<-e[,c(1,3)]
colnames(e)<-c("parameter", "value")
f<-read.csv("alpha_resi.csv", header=F)
colnames(f)<-c("parameter", "value")

createModel <- function() {
  model <- list()
  
  scale_distance <- 1000
  
  n <- 314
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
  #zones<-read.table("Zones.csv", header=F,sep="\t")
  #colnames(zones) <- c("NUTS2","Node")
  
  model
}

createRealFlow <- function() {
  realFlow <- list()
  realFlow$road <- list()
  realFlow$rail <- list()
  realFlow$iw <- list()
  
  flow<-read.table("Flow_v1.csv", header=F, sep="\t")#for tab separated file\
  colnames(flow) <- c("Origin", "Destination","Commodity","Mode","Flow")
  flow<-flow[!(flow$Origin>314 | flow$Destination>314),] #delete rows for non european flows
  
  #flow$Commodity[flow$Commodity == 0] <- 10
  
  fillFlow <- function(container, mode_id) {
    commodities <- as.character(unique(flow$Commodity))#cast to string
    for (com in commodities) {
      print(paste(com))
      fflow <- flow[(flow$Commodity == com) & (flow$Mode == mode_id),c("Origin","Destination","Flow")]#select correct rows, fill up with these columns
      
      matrixflow <- matrix(0,314,314)
      
      if (nrow(fflow) > 0) {
        # scan line by line the fflow
        for (i in 1:nrow(fflow)) {
          line <- fflow[i,]
          if (line$Origin > 314 | line$Destination > 314) next;
          matrixflow[line$Origin, line$Destination] <- line$Flow
        }
      }
      
      write.table(matrixflow, paste("matrixFlow",mode_id,com,".csv"), row.names=T, col.names=T)#csv. or table not sure
      
      eval.parent(substitute(container[[com]] <- matrixflow))
    }
  }
   #apply function to fill up realflowa
  
  fillFlow(realFlow$road, 1)
  fillFlow(realFlow$rail, 2)
  fillFlow(realFlow$iw, 3)
  
  realFlow
}

loadRealFlow <- function() {
  realFlow <- list()
  realFlow$road <- list()
  realFlow$rail <- list()
  realFlow$iw <- list()
  
  fillFlow <- function(container, mode_id) {
    commodities <- as.character(0:9)#cast to string
    for (com in commodities) {
      print(paste(com))
      
      matrixflow <- read.table(paste("matrixFlow",mode_id,com,".csv"))#csv. or table not sure
      
      eval.parent(substitute(container[[com]] <- matrixflow))
    }
  }
  
  fillFlow(realFlow$road, 1)
  fillFlow(realFlow$rail, 2)
  fillFlow(realFlow$iw, 3)
  
  realFlow
}
