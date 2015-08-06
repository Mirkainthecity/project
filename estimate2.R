Estimate<-function(distance, time, attractionO, attractionD, kmcost, VoT, beta, reliability) {
  if (beta == 0) return((0*distance) + 1)
  
  cost <- distance * kmcost +
    time * VoT * reliability+
    (attractionO %*% t(rep(1,ncol(distance)))) +
    (rep(1,nrow(distance)) %*% t(attractionD))
  
  -beta*cost/10000
  #print(paste("beta*cost", beta*cost/1000))
}
MSE<-function(sim, obs){
  sum((sim-obs)^2,na.rm=T)
}