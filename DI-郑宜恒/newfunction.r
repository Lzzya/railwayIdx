#�Զ��庯��������Ϊĳ����ʱ���������ݣ����Ϊ1,0.5,0����ֵ
function1<-function(v){
  vud<-(v[-1]-v[-length(v)])/v[-length(v)]#�仯��
  t1<-mean(vud)+qt(1-0.3,length(vud))*sd(vud)/sqrt(length(vud))#��֮������
  t2<-mean(vud)+qt(1-0.45,length(vud))*sd(vud)/sqrt(length(vud))
  t3<-mean(vud)+qt(0.6,length(vud))*sd(vud)/sqrt(length(vud))
  return(ifelse(vud>t3,1,ifelse(vud>t2,0.5,0)))
}

df<-read.csv("xianxing.csv")#��������ָ��
tm<-df$time[-1]
c1<-function1(df$��Ʒ�ֲĲ���)
c2<-function1(df$ԭú����)
c3<-function1(df$ԭ�ͼӹ�������)
c4<-function1(df$�����������)
DIx<-0.428464369002374*c1+0.250325315484835*c2+0.107696606448519*c3+0.213513709064272*c4

tb<-read.csv("tongbu.csv",header=T)#����ͬ��ָ��
a1<-function1(tb$������ת��)
a2<-function1(tb$������)
a3<-function1(tb$��ҵ����ֵ)
DIt<-0.23397760173316*a1+0.267846632087827*a2+0.498175766179013*a3

DI<-data.frame(tm,DIx,DIt)#�洢����ָ�������������ݿ�
DI$tm<-as.Date.POSIXct(DI$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #ת��ʱ���ʽ
plot(DI$DIx~DI$tm,type="l",col="red")
lines(DI$DIt~DI$tm)

#��Ҫ��Ȩ�ص������Ȩ��Ҫ�ǿɵ��ģ�Ҫ����ʷ���ݱ��������������ļ�������ݣ���ʹ�Ժ�Ȩ�ر��ˣ���ʷ����Ҳ����䣩



