shinyServer(function(input, output) {
  require(ggplot2)
  require(DT)
  

  df<-read.csv("DI-trans.csv",head=T)
  df$tm<-as.Date.POSIXct(df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  
  len<-length(df$tm)
  
  

  output$main_plot <- renderPlot( {
    
   if(input$csvfile=="铁路规模扩散景气指数"){
      df<-read.csv("DI-scale.csv",head=T)
      len<-length(df$tm)
   }
  if(input$csvfile=="铁路设备扩散景气指数" ){
      df<-read.csv("DI-equip.csv",head=T)
      len<-length(df$tm)
    }  
  if(input$year_start> input$year_end)  {
    p<-ggplot(df,x=c(df$tm[1],df$tm[len]))#如果选择错误则显示所有年份
  }
  else{
    dfsub<-subset(df,(substr(df$tm,1,4)>=input$year_start) )#subset取子集，显示一部分年份的数据，把其他的过滤掉
    dfsub<-subset(dfsub,(substr(dfsub$tm,1,4)<=input$year_end))
    p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[len]),aes(x=tm,y=100))
  }
  if(input$year_start< input$year_end&&input$csvfile=="铁路运输扩散景气指数" )  {
    p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[len]),aes(x=tm,y=1))
  }
    if(input$coord_Index){
      p<-p+geom_line(aes(x=tm,y=coor),color="black",size=0.6)
    }
  #????ggplot,?Ȼ???tm??coor??ʣ??��?е???????һ???????ϵ???
    if (input$advanced_Index) {
      p<-p+geom_line(aes(x=tm,y=adv),color="red",size=0.6)
    }
    
    if (input$delay_Index) {
      p<-p+geom_line(aes(x=tm,y=delay),color="blue",size=0.6)
    }
    p+ylab(input$csvfile)+xlab("时间")+geom_line()
  })
  
  output$table<-DT::renderDataTable(
    DT::datatable(
      {
        if(input$csvfile=="铁路规模扩散景气指数"){
          df<-read.csv("DI-scale.csv",head=T)
        }
        if(input$csvfile=="铁路设备扩散景气指数" ){
          df<-read.csv("DI-equip.csv",head=T)
        }
        data<-df
      } , 
      colnames = c('时间', '同步指数',  '滞后指数','先行指数'),
      rownames = TRUE)
  )
})