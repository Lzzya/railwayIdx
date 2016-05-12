#自定义函数，输入为某量的时间序列数据，输出为1,0.5,0三个值
function1<-function(v){
  vud<-(v[-1]-v[-length(v)])/v[-length(v)]#变化率
  t1<-mean(vud)+qt(1-0.3,length(vud))*sd(vud)/sqrt(length(vud))#求之心上线
  t2<-mean(vud)+qt(1-0.45,length(vud))*sd(vud)/sqrt(length(vud))
  t3<-mean(vud)+qt(0.6,length(vud))*sd(vud)/sqrt(length(vud))
  return(ifelse(vud>t3,1,ifelse(vud>t2,0.5,0)))
}

df<-read.csv("xianxing.csv")#计算先行指数
tm<-df$time[-1]
c1<-function1(df$成品钢材产量)
c2<-function1(df$原煤产量)
c3<-function1(df$原油加工量产量)
c4<-function1(df$火力发电产量)
DIx<-0.428464369002374*c1+0.250325315484835*c2+0.107696606448519*c3+0.213513709064272*c4

tb<-read.csv("tongbu.csv",header=T)#计算同步指数
a1<-function1(tb$货运周转量)
a2<-function1(tb$货运量)
a3<-function1(tb$工业增加值)
DIt<-0.23397760173316*a1+0.267846632087827*a2+0.498175766179013*a3

DI<-data.frame(tm,DIx,DIt)#存储所有指数计算结果的数据框
DI$tm<-as.Date.POSIXct(DI$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转换时间格式
plot(DI$DIx~DI$tm,type="l",col="red")
lines(DI$DIt~DI$tm)

#需要加权重的输入项，权重要是可调的；要把历史数据保存起来（锁定哪几年的数据，即使以后权重变了，历史数据也不会变）



