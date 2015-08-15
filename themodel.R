#setwd("C:/Users/Mirka/Desktop/TNO_Thesis/stage/SM/Data")

attr<-read.table("ParametersF_v1.csv", header=F, nrows=1884)#attraction parameters
colnames(attr)<-c("node", "OD", "mode", "value")
vot<-read.csv("VoT_resi.csv", header=F)
vot<-vot[,c(1,3)]
colnames(vot)<-c("parameter", "value")
beti<-read.csv("alpha_resi.csv", header=F)
colnames(beti)<-c("parameter", "value")

createModel <- function() {
  model <- list()
  
  scale_distance <- 1000
  
  n <- 10
  a<-57 ### These values are to select a range of OD nodes where all 3 modes are present
  z<-66
  model$NoR <- n 
  
  model$distanceIw<-read.csv("IWWDistance_v2.csv", header=F,sep=";")[a:z, a:z]
  model$timeIw<-read.csv("IWWTime_v2.csv", header=F,sep=";")[a:z, a:z]
  model$distanceRail<-read.csv("RailDistance_v2.csv", header=F,sep=";")[a:z, a:z]
  model$timeRail<-read.csv("RailTime_v2.csv", header=F,sep=";")[a:z, a:z]
  model$distanceRoad<-read.csv("RoadDistance_v2.csv", header=F,sep=";")[a:z, a:z]
  model$timeRoad<-read.csv("RoadTime_v2.csv", header=F,sep=";")[a:z, a:z]
 
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
  
 
  #flow$Commodity[flow$Commodity == 0] <- 10
  
  fillFlow <- function(container, mode_id) { flow<-read.table("Flow_v1.csv", header=F, sep="\t")#for tab separated file\
  colnames(flow) <- c("Origin", "Destination","Commodity","Mode","Flow")
  flow<-flow[!(flow$Origin>z |flow$Origin<a | flow$Destination>z | flow$Destination<a ),] #only use flows for necessary nodes
  
    commodities <- as.character(unique(flow$Commodity))#cast to string
    for (com in commodities) {
      print(paste(com))
      fflow <- flow[(flow$Commodity == com) & (flow$Mode == mode_id),c("Origin","Destination","Flow")]#select correct rows, fill up with these columns
      
      matrixflow <- matrix(0,n,n)
      
      if (nrow(fflow) > 0) {
        # scan line by line the fflow
        for (i in 1:nrow(fflow)) {
          line <- fflow[i,]
          if (line$Origin > z | line$Destination > z |line$Origin < a | line$Destination < a) next;
          matrixflow[line$Origin-a+1, line$Destination-a+1] <- line$Flow
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
      
      matrixflow <- as.matrix(read.table(paste("matrixFlow",mode_id,com,".csv")))#csv. or table not sure
      
      eval.parent(substitute(container[[com]] <- matrixflow))
    }
  }
  
  fillFlow(realFlow$road, 1)
  fillFlow(realFlow$rail, 2)
  fillFlow(realFlow$iw, 3)
  
  realFlow
}
