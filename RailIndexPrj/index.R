
index<-function(x,y){
  ylen=length(x)
  y[1]=100
  y[2]=100
  y[3]=y[2]*(200+x[3])/(200-x[3])
  for(i in 4:ylen){
    y[i]=y[i-1]*(200+x[i])/(200-x[i])
    
  }
  y
}