svmReg<-function(dbase,svmmodel){
#Support vector machine, SVM
#require(rminer)
#require(e1071)
require(kernlab)

regModel<-ksvm(svmmodel,data=dbase,model="svm",type="eps-svr",cross=10)

return(regModel)

}
