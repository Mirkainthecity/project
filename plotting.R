flatterflow<-function(flow){
  result<-c()
  for (mode in flow)
    for (commodity in mode)
      result<-c(result, as.vector(as.matrix(commodity)))
  
  result
}

#plot(flatterflow(realflow),flatterflow(guyFlow))
