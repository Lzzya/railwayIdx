svmReg<-function(dbase,model){
#Support vector machine, SVM
#require(rminer)
require(e1071)
n<-dim(dbase)[1]
m=sample(1:n,ceiling(n/2))
n<-500
NMSE<-rep(0,n)
NMSE0<-NMSE
set.seed(1445)
for(i in 1:n){ 
 #  M<-fit(f~x1+x2+x3+x4,data=dbase,model="svm")
  M<-svm(model,data=dbase[-m,])  #??e1071??????
  y0<-predict(M,dbase[-m,]);
  y1<-predict(M,dbase[m,]);
  NMSE0[i]<-mean((dbase$PV[-m]-y0)^2)/mean((dbase$PV[-m]-mean(dbase$PV[-m]))^2);
  NMSE[i]<-mean((dbase$PV[m]-y1)^2)/mean((dbase$PV[m]-mean(dbase$PV[m]))^2)
}
print(M)
MNMSE0<-mean(NMSE0)
MNMSE<-mean(NMSE)

cat("SVM训练集NMSE0=",MNMSE0,"\n")
cat("测试集NMSE=",MNMSE,"\n")
par(mfrow=c(1,1))
plot(1:n,NMSE,type="l",ylim=c(min(NMSE,NMSE0),max(NMSE,NMSE0)),ylab="NMSE",main="Support Vector Machine:NMSE",lty=2)
lines(1:n,NMSE0)
legend("topright",c("Training Set","Testing Set"),lty=1:2)
n<-dim(dbase)[1]
f.fit<-rep(0,n)
for(i in 1:n){ 
  f.fit[i]<-predict(M,dbase[i,])
}
plot(1:n,f.fit,type="p",pch=1,
     ylim=c(min(f.fit,dbase[,1]),max(f.fit,dbase[,1])),
     ylab="Railway Freight",xlab="Date",main="支持向量机回归预测")
lines(1:n,dbase$f,pch=16)
results<-as.matrix(f.fit,ncol=1)
}
passanger<-read.csv("locomotive-PV.csv",head=T)
svmReg(passanger,locomotive~PV)
