shinyServer(function(input, output) {
  
  #------------------------------
  #货运量预测
  #------------------------------
  require(ggplot2)
  require(DT)
  require(e1071)
  require(randomForest)
 
  
  
  
  df<-read.csv("freight.csv",head=T)
  df$tm<-as.Date.POSIXct(df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
  
  #-------------olsRegModel为多元回归模型
  olsRegModel<-lm(freight~iron+coal,data=df)
  df$linearRegPred<-as.integer(predict(olsRegModel,newdata=df))
  
  
  
  #-------rfRegModel是随机森林得到的回归模型，后面用predict直接调用此模型即可,因数量少，不运行交叉验证
  rfRegModel<-randomForest(freight~iron+coal,data=df,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  df$frRegPred<-as.integer(predict(rfRegModel,df))    #<-----------随机森林的预测数据已经在这里计算得到
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  svmRegModel<-svm(freight~iron+coal,data=df,type="eps-regression",cross=dim(df)[1]/2)
  #svm 内含交叉验证，所以不需要再运行交叉验证
  df$svmRegPred<-as.integer(predict(svmRegModel,df))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  len<-length(df$tm)
  #plotCurve是画曲线的通过用函数，为了减少后面的代码量  
  plotCurve<-function(db,xdata,ydata)
  {
    len=dim(xdata)[1]
    plt<-ggplot(db,x=c(xdata[1],xdata[len]),aes(x=xdata,y=ydata),color="red")
    return(plt)
  }
  #---------------------------多元回归画线
  output$linearplot <- renderPlot( {
    
    if(input$year_start> input$year_end)  {
      
      if (input$stat_data) {
        p<-plotCurve(df,df$tm,df$freight)
      }
      else
      {
        p<-plotCurve(df,df$tm,df$linearRegPred)
      }
    }
    else{
      dfsub<-subset(df,substr(df$tm,1,4)>=input$year_start) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$year_end)
      if (input$stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$freight)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$linearRegPred)
      }
    }
    
    if(input$predict_data){
      
      p<-p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$stat_data) {
      p<-p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    p+ylab("货运量（万吨）")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------------------------------
  
  #----------------------------------------------------   
  #多元回归预测计算
  output$freight_output<-renderText({
    x1<-as.numeric(input$iron_input)
    x2<-as.numeric(input$coal_input)
    iron<-c(x1)
    coal<-c(x2)
    tm<-c(2016)
    freight<-c(0)
    inputdata<-data.frame(tm,freight,iron,coal)
    pred<-as.integer(predict(olsRegModel,inputdata))
    paste("多元回归预测：",pred ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$freight_FRR<-renderText({
    x1<-as.numeric(input$iron_input)
    x2<-as.numeric(input$coal_input)
    iron<-c(x1)
    coal<-c(x2)
    tm<-c(2016)
    freight<-c(0)
    inputdata<-data.frame(tm,freight,iron,coal)
    railCarriage<-predict(rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railCarriage[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$freight_zhi<-renderText({
    x1<-as.numeric(input$iron_input)
    x2<-as.numeric(input$coal_input)
    iron<-c(x1)
    coal<-c(x2)
    tm<-c(2016)
    freight<-c(0)
    inputdata<-data.frame(tm,freight,iron,coal)
    pred<-as.integer(predict(svmRegModel,inputdata))
    
    paste("支持向量机预测：",pred)
    
  }
  )
  #-------------------------------------
  
  
  #-----------随机森林Tabset画线  
  output$rfplot <- renderPlot( {
    
    if(input$year_start> input$year_end)  {
      
      if (input$stat_data) {
        p<-plotCurve(df,df$tm,df$freight)
      }
      else
      {
        p<-plotCurve(df,df$tm,df$frRegPred)
      }
    }
    else{
      dfsub<-subset(df,substr(df$tm,1,4)>=input$year_start) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$year_end)
      if (input$stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$freight)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$frRegPred)
      }
    }
    
    if(input$predict_data){
      p<-p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)#+stat_smooth(method=rfRegModel,color='black',level=0.95)
    }
    
    if (input$stat_data) {
      p<-p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    p+ylab("货运量(万吨)")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------支持向量机Tabset画线
  
  output$svmplot <- renderPlot( {
    
    if(input$year_start> input$year_end)  {
      
      if (input$stat_data) {
        p<-plotCurve(df,df$tm,df$carriage)
      }
      else
      {
        p<-plotCurve(df,df$tm,df$svmRegPred)
      }
    }
    else{
      dfsub<-subset(df,substr(df$tm,1,4)>=input$year_start) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$year_end)
      if (input$stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$freight)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$svmRegPred)
      }
    }
    
    if(input$predict_data){
      p<-p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
    }
    
    if (input$stat_data) {
      p<-p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    p+ylab("货运量(万吨)")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  output$table<-DT::renderDataTable(
    DT::datatable(
      data<-df, 
      colnames = c('日期', '货运量(万吨)','成品钢材产量(万吨)','原煤产量(万吨)','多元回归预测(万吨)','随机森林回归预测(万吨)','支持向量机回归预测(万吨)'),
      rownames = TRUE)
  )
  
     
}
)