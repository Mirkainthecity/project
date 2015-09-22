library(rCMA);
source("themodel.R")
source("estimate.R")

if (!exists("realFlow"))
  realFlow <- loadRealFlow()

model <- createModel()


loadParameters <- function(model, parameters) {
  
  #model$VoT <- parameters[1]
  #model$VoR <- parameters[2]
  
  
  for (i in 1:10) {
    model$commodities[[i]]$VoT <- parameters[i]
    model$commodities[[i]]$beta <- parameters[10+i]
  }
  
  model$roadReliability <- parameters[21]
  model$railReliability <- parameters[22]
  model$iwwReliability <- parameters[23]
  
  
  return (model)
}

fitFunc <- function(parameters) {
  print(parameters)  
  
  #model <- loadParameters(model, parameters)
  
  quality <- GetModelQuality(model,realFlow)
  
  print(paste("quality: ", quality))
  
  return(quality)
}

cma <- cmaNew();
## 
cmaInit(cma, seed=42, dimension=23, initialX=1.5, initialStandardDeviations=0.2);
res1 = cmaOptimDP(cma,fitFunc,iterPrint=30);

plot(res1$fitnessVec,type="l",log="y",col="blue"
     ,xlab="Iteration",ylab="Fitness");
str(res1);
