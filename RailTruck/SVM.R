svmReg<-function(dbase,svmmodel,new_data){
#Support vector machine, SVM
#require(rminer)
#require(e1071)
require(kernlab)
n<-dim(dbase)[1]
m=sample(1:n,ceiling(n/2))
n<-100
#NMSE<-rep(0,n)
#NMSE0<-NMSE
set.seed(1445)
regModel<-ksvm(svmmodel,data=dbase,model="svm",type="eps-svr",cross=10)
#for(i in 1:n){ 
  #A<-svm(svmmodel,data=dbase[-m,],type="eps-regression",fitted=FALSE)  #??e1071??????
#  A<-ksvm(svmmodel,data=dbase[-m],model="svm",type="eps-svr",fit=FALSE)
#}

newDb<-rbind(dbase,new_data)

reg.fit<-predict(regModel,dbase)

#pre<-as.data.frame(predict(A,new_data))

prediction<-as.data.frame(reg.fit)
#names(pre)[1]<-"prediction"
names(prediction)[1]<-"prediction"
#prediction<-rbind(pre1,pre)
#return(prediction)
return(regModel)

}
