shinyServer(function(input, output) {
  require(ggplot2)
  require(DT)
  

  #自定义函数，输入为某量的时间序列数据，输出为1,0.5,0三个值
  function1<-function(v){
    vud<-(v[-1]-v[-length(v)])/v[-length(v)]#变化率
    t1<-mean(vud)+qt(1-0.3,length(vud))*sd(vud)/sqrt(length(vud))#求置信上线
    t2<-mean(vud)+qt(1-0.45,length(vud))*sd(vud)/sqrt(length(vud))
    t3<-mean(vud)+qt(0.6,length(vud))*sd(vud)/sqrt(length(vud))
    return(ifelse(vud>t3,1,ifelse(vud>t2,0.5,0)))
  }
  
  xx<-read.csv("xianxing.csv",head=T)#计算先行指数
  tm<-xx$time[-1]
  c1<-function1(xx$成品钢材产量)
  c2<-function1(xx$原煤产量)
  c3<-function1(xx$原油加工量产量)
  c4<-function1(xx$火力发电产量)
  DIx<-0.428464369002374*c1+0.250325315484835*c2+0.107696606448519*c3+0.213513709064272*c4
  
  tb<-read.csv("tongbu.csv",head=T)#计算同步指数
  a1<-function1(tb$货运周转量)
  a2<-function1(tb$货运量)
  a3<-function1(tb$工业增加值)
  DIt<-0.23397760173316*a1+0.267846632087827*a2+0.498175766179013*a3
  
  DI<-data.frame(tm,DIx,DIt)#存储所有指数计算结果的数据框
  DI$tm<-as.Date.POSIXct(DI$tm,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
  write.csv(DI,file="DI-Trans.csv",row.names = FALSE)
  #plot(DI$DIx~DI$tm,type="l",col="red")
  #lines(DI$DIt~DI$tm);
  
  

  output$main_plot <- renderPlot( {
    
   if(input$csvfile=="铁路运输扩散景气指数"){
      #df<-read.csv("DI-Trans.csv",head=T)
      df<-DI
      len<-length(df$tm)
   }

  if(input$year_start> input$year_end)  {
    p<-ggplot(df,x=c(df$tm[1],df$tm[len]))#如果选择错误则显示所有年份
  }
  else{
    dfsub<-subset(df,(substr(df$tm,1,4)>=input$year_start) )#subset取子集，显示一部分年份的数据，把其他的过滤掉
    dfsub<-subset(dfsub,(substr(dfsub$tm,1,4)<=input$year_end))
    p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[len]),aes(x=tm,y=0.5))
  }
  if(input$year_start< input$year_end&&input$csvfile=="铁路运输扩散景气指数" )  {
    p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[len]),aes(x=tm,y=0.5))
  }
    if(input$coord_Index){
      p<-p+geom_line(aes(x=tm,y=DIt),color="black",size=0.6)
    }
 
    if (input$advanced_Index) {
      p<-p+geom_line(aes(x=tm,y=DIx),color="red",size=0.6)
    }
   
    p+ylab(input$csvfile)+xlab("时间")+geom_line()
  })
  
  output$table<-DT::renderDataTable(
    DT::datatable(
      {
        if(input$csvfile=="铁路规模扩散景气指数"){
          df<-read.csv("DI-Trans.csv",head=T)
        }
        if(input$csvfile=="铁路设备扩散景气指数" ){
          df<-read.csv("DI-Trans.csv",head=T)
        }
        data<-df
      } , 
      colnames = c('时间', '先行指数','同步指数'),
      rownames = FALSE)
  )
})