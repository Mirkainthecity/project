#setwd("C:/Users/Mirka/Desktop/TNO_Thesis/stage/SM/Data")

model <- list()

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
  print(class(container))
  for (com in commodities) {
    print(paste(com))
    fflow <- flow[(flow$Commodity == com) & (flow$Mode == mode_id),c("Origin","Destination","Flow")]#select correct rows, fill up with these columns
    
    if (nrow(fflow) <= 0) {
      break
      } else {
        matrixflow <- matrix(0,max(fflow$Origin,314),max(fflow$Destination,314)) 
      }
    
    
    # scan line by line the fflow
    for (i in 1:nrow(fflow)) {
      line <- fflow[i,]
      matrixflow[line$Origin, line$Destination] <- line$Flow
    }
    
    write.table(matrixflow, paste("matrixFlow",mode_id,com,".csv"), row.names=F, col.names=F)#csv. or table not sure
    
    eval.parent(substitute(container[[com]] <- matrixflow))
  }
}

fillFlow(realFlow$road, 1)
fillFlow(realFlow$rail, 2)
fillFlow(realFlow$iw, 3)


model$distanceIw<-read.csv("IWWDistance_v2.csv", header=F,sep=";")
model$timeIw<-read.csv("IWWTime_v2.csv", header=F,sep=";")

model$distanceRail<-read.csv("RailDistance_v2.csv", header=F,sep=";")
model$timeRail<-read.csv("RailTime_v2.csv", header=F,sep=";")
model$distanceRoad<-read.csv("RoadDistance_v2.csv", header=F,sep=";")
model$timeRoad<-read.csv("RoadTime_v2.csv", header=F,sep=";")

model$NoR<-447

#zones<-read.table("Zones.csv", header=F,sep="\t")
#colnames(zones) <- c("NUTS2","Node")
