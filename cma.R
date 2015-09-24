library(rCMA);
source("themodel.R")
source("estimate.R")

if (!exists("realFlow"))
  realFlow <- loadRealFlow()

model <- createModel()


loadParameters <- function(model, parameters) {
  
  #model$VoT <- parameters[1]
  #model$VoR <- parameters[2]
  
  
  for (i in 10) { #for (i in 1:10)
    model$commodities[[i]]$VoT <- parameters[1]
    model$commodities[[i]]$beta <- parameters[2]
  }
  
  model$roadReliability <- parameters[3]
  model$railReliability <- parameters[4]
  model$iwwReliability <- parameters[5]
  
  
  return (model)
}

fitFunc <- function(parameters) {
  #print(parameters)  
  
  model <- loadParameters(model, parameters)
  
  quality <- GetModelQuality(model,realFlow)
  
  print(paste("quality: ", quality))
  
  return(quality)
}

cma <- cmaNew();
## 
cmaInit(cma, seed=42, dimension=5, initialX=1.5, initialStandardDeviations=0.2);


popR <- cmaSamplePopulation(cma);

fitness <- cmaCalcFitness(cma,popR,fitFunc);
cmaUpdateDistribution(cma,fitness);

res1 = cmaOptimDP(cma,fitFunc,iterPrint=30);
plot(res1$fitnessVec,type="l",log="y",col="blue"
     ,xlab="Iteration",ylab="Fitness");
str(res1);
