rate2<-function(x,y){
   ylen<-length(x)
   y<-y[1:ylen]
   for(i in 3:ylen){
     if((x[i]+x[i-1])!=0){
      y[i]<-200*(x[i]-x[i-1])/((x[i]+x[i-1]))}
else{ y[i]<-200*(x[i]-x[i-1])*((x[i]+x[i-1]))}
}
y[1]<-0
y[2]<-0
y
  }