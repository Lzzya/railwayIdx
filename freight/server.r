shinyServer(function(input, output) {
  require(ggplot2)
  require(DT)

  df<-read.csv("freightReg.csv",head=T)
  df$tm<-as.Date.POSIXct(df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  df$predict<-(14520+0.5393*df$iron+0.2576*df$coal)
  len<-length(df$tm)

  output$main_plot <- renderPlot( {
    
    if(input$year_start> input$year_end)  {
      
      if (input$stat_data) {
        
         p<-ggplot(df,x=c(df$tm[1],df$tm[len]),aes(x=tm,y=df$freight),color="red")
      }
      else
      {
        p<-ggplot(df,x=c(df$tm[1],df$tm[len]),aes(x=tm,y=df$predict))
      }
    }
    else{
      dfsub<-subset(df,(substr(df$tm,1,4)>=input$year_start) )
      dfsub<-subset(dfsub,(substr(dfsub$tm,1,4)<=input$year_end))
      if (input$stat_data) {
        p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[len]),aes(x=tm,y=freight))
      }
      else
      {
        p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[len]),aes(x=tm,y=predict))
      }
    }
      
    if(input$predict_data){
      p<-p+geom_line(aes(x=tm,y=predict),color="blue",size=0.8)
        #+stat_smooth(color='black',level=0.95)
    }
    
    if (input$stat_data) {
      p<-p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    
    
    p+ylab("铁路货运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  output$freight_output<-renderText({
    if(as.numeric(input$iron_input)>0 && as.numeric(input$coal_input)>0)
      {   paste("多元回归预测货运量为：", 14520+0.5393*as.numeric(input$iron_input)+0.2576*as.numeric(input$coal_input))   
       
      }
     }
    )
    
  output$freight_FRR<-renderText({
       paste("随机森林多元回归预测货运量为：", 14520+0.5393*as.numeric(input$iron_input)+0.2576*as.numeric(input$coal_input))   
  }
  )
  
  output$table<-DT::renderDataTable(
    DT::datatable(
      {
        
        data<-df
      } , 
      colnames = c('时间', '铁路货运量（统计值）',  '成品钢材产量','原煤产量','原油加工量','火力发电量','铁路固定资产投资额','货运量预测值'),
      rownames = TRUE)
  )
 
}
)