
randFrstReg<-function(dbase,model){
  require(randomForest)
  set.seed(100)
  n<-dim(dbase)[1]
  m=sample(1:n,ceiling(n/2))
  n<-200

  for(i in 1:n) {
    rfRegModel<-randomForest(model,data=dbase[-m,],importance=T,proximity=T,ntree=i,type="regression")
  }
  
  rfRegModel<-randomForest(model,data=dbase,importance=T,proximity=T,ntree=50,type="regression")

  return(rfRegModel)

}


