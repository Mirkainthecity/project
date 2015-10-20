#setwd("C:/Users/Mirka/Desktop/TNO_Thesis/stage/SM/Data")

#attr<-read.table("ParametersF_v1.csv", header=F, nrows=1884)#attraction parameters
#colnames(attr)<-c("node", "OD", "mode", "value")
#vot<-read.csv("VoT_resi.csv", header=F)
#vot<-vot[,c(1,3)]
#colnames(vot)<-c("parameter", "value")
#beti<-read.csv("alpha_resi.csv", header=F)
#colnames(beti)<-c("parameter", "value")

createModel <- function() {
  model <- list()
  
  scale_distance <- 1000
  
  n <- 314
  a<-1 ### These values are to select a range of OD nodes where all 3 modes are present
  z<-314
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
  
  initialPara<-list()
  initialPara$VoT <-0.2
  initialPara$beta <- -0.5
  #initialPara$VoR<-0.3
  model$commodities <- list()
  for (i in 1:10) {
    commodity <- list()
    commodity$VoT <- initialPara$VoT  
    commodity$beta <- initialPara$beta  
    #commodity$VoR <-initialPara$VoR
    commodity$id <- as.character(i-1)#since commodities are numbered between 0-9
    
    model$commodities[[i]] <- commodity
  }
  
  #Initialization of parameters
  model$roadkmcost <- 0.14
  model$railkmcost <- 0.08
  model$iwkmcost <- 0.01
  model$railAttractionO <-rep(0, n)
  model$roadAttractionO <-rep(0, n)
  model$iwwAttractionO <-rep(0, n)
  model$railAttractionD <-rep(0, n)
  model$roadAttractionD <-rep(0, n)
  model$iwwAttractionD <-rep(0, n)
  
  model$roadReliability <- 0.1
  model$railReliability <- 0.1
  model$iwwReliability <- 0.1
  
  ###Initialize estimated flows######
  
  model$flowRoad <- list()
  #for ( com in as.character(0:9) )
  com <- "9"
    model$flowRoad[[com]] <- matrix(0, n, n)
  #}
  
  model$flowRail <- list()
  #for ( com in as.character(0:9) ) 
    model$flowRail[[com]] <- matrix(0, n, n)
  #}
  
  model$flowIw <- list()
 # for ( com in as.character(0:9) ) {
    model$flowIw[[com]] <- matrix(0, n, n)
  #}
  
  model
}

createRealFlow <- function() {
  a<-1 ### These values are to select a range of OD nodes where all 3 modes are present
  z<-314
  n <- 314
  realFlow <- list()
  realFlow$road <- list()
  realFlow$rail <- list()
  realFlow$iw <- list()
  
 
  #flow$Commodity[flow$Commodity == 0] <- 10
  
  fillFlow <- function(container, mode_id) {
    flow<-read.table("Flow_v1.csv", header=F, sep="\t")#for tab separated file\
    colnames(flow) <- c("Origin", "Destination","Commodity","Mode","Flow")
    flow<-flow[!(flow$Origin>z |flow$Origin<a | flow$Destination>z | flow$Destination<a ),] #only use flows for necessary nodes
  
    commodities <- as.character(unique(flow$Commodity))#cast to string
    for (com in commodities) {
      if (com=="9") {
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
      } else {
        next;
      }
      
      if (com != "9" && mode_id == 2) {
        print(paste('rail', com))
        for (i in 1:nrow(matrixflow)) {
          for (j in 1:ncol(matrixflow)) {
            matrixflow[i,j]<-abs(rnorm(1,mean=30000,sd=1000))#generate only positive nr
          }
        }
      }
      
      
      write.table(matrixflow, paste("matrixFlow",mode_id,com,".csv"), row.names=T, col.names=T)#csv. or table not sure
      #### Create random flows for missing data ###
      #if (mode_id==2){
      
         # mat<-matrix(rnorm(10*10,mean=30000,sd=10000), 10, 10) 
          #for (com in commodities){ ##how to keep commodity 9?
          #  matrixflow[line$Origin-a+1, line$Destination-a+1] <-rnorm(1,mean=30000,sd=10000) 
            #
            #write.table(mat, paste("matrixFlow",mode_id,com,".csv"), row.names=T, col.names=T)
            
        #}
      #}   
      eval.parent(substitute(container[[com]] <- matrixflow))
    }
  }

   #apply function to fill up realflowa
  
  fillFlow(realFlow$road, 1)
  fillFlow(realFlow$rail, 2)
  fillFlow(realFlow$iw, 3)
  return(realFlow)
}

loadRealFlow <- function() {
  realFlow <- list()
  realFlow$road <- list()
  realFlow$rail <- list()
  realFlow$iw <- list()
  
  fillFlow <- function(container, mode_id) {
    commodities <- as.character(0:9)#cast to string
    for (com in commodities) {
       if (com=="9") {
      print(paste(com))
      
      matrixflow <- as.matrix(read.table(paste("matrixFlow",mode_id,com,".csv")))#csv. or table not sure
      
      eval.parent(substitute(container[[com]] <- matrixflow))
       }else {
         next;
       }
    }
  }
  
  fillFlow(realFlow$road, 1)
  fillFlow(realFlow$rail, 2)
  fillFlow(realFlow$iw, 3)
  
  return(realFlow)
}
