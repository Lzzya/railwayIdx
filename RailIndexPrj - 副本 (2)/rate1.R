rate1<-function(x){
  xlen<-length(x)
  x[2:xlen]<-x[2:xlen]/x[1:(xlen-1)]-c(1)
  x[1]<-0
  x
}