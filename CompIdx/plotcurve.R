require(ggplot2)
df<-read.csv("compidx.csv",head=T)
len<-length(df$no)
plot(df$no,df$coor)
p<-ggplot(df,x=c(df$no[1],df$no[len]),aes(x=no,y=coor))+geom_line(aes(y=delay),color="blue")+
         geom_line(aes(y=adv),color="red")+geom_line()



