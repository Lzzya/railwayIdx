passanger<-read.csv("locomotive-PV.csv",head=T)
randomForest(PV~locomotive,data=passanger,ntree=1,importance=TRUE,proximity=T)

names(passanger)
      
dim(passanger)
n=dim(passanger)[i]
m=sample(1:n,ceiling(n/2))
n<-100
NMSE<-rep(0,n)
NEMSE0<-NMSE
set.seed(100)
for(i in 1:n){A=randomForest(PV~locomotive,data=passanger[-m,],importance=T,proximity=T,ntree=i); 
+ y0=predict(A,passanger[-m,]);
+ y1=predict(A,passanger[m,]);
+ NMSE0[i]<-mean((passanger$PV[-m]-y0)^2)/mean((passanger$PV[-m]-mean(passanger$PV[-m]))^2);
+ NMSE[i]=mean((passanger$PV-y1)^2)/mean((passanger$PV-mean(passanger$PV[m]))^2)}
MNMSE0<-mean(NMSE0)
MNMSE<-MEAN(NMSE)
MNMSE0
