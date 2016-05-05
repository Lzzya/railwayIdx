shinyServer(function(input, output) {
  require(ggplot2)
  require(DT)
  require(e1071)

  require(randomForest)
  #因数据量小，不做随机森林的交叉验证，所以不再调用randomForest.r
  #source("randomForest.r")


  
  df<-read.csv("客车车辆预测.csv",head=T)
  #-------------olsRegModel为多元回归模型
  olsRegModel<-lm(carriage~distance,data=df)
  #bound<-(predict(olsRegModel,newdata=df,interval = "prediction"))  #<-----------回归模型的预测数据已经计算得到
  #df$linearRegPred<-as.integer(bound[,1])
  df$linearRegPred<-as.integer(predict(olsRegModel,newdata=df))
 
 
  
  #-------rfRegModel是随机森林得到的回归模型，后面用predict直接调用此模型即可,因数量少，不运行交叉验证
  rfRegModel<-randomForest(carriage~distance,data=df,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  df$frRegPred<-as.integer(predict(rfRegModel,df))    #<-----------随机森林的预测数据已经在这里计算得到
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  svmRegModel<-svm(carriage~distance,data=df,type="eps-regression",cross=dim(df)[1]/2)
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
         p<-plotCurve(df,df$tm,df$carriage)
      }
      else
      {
        p<-plotCurve(df,df$tm,df$linearRegPred)
      }
    }
    else{
      dfsub<-subset(df,df$tm>=input$year_start) 
      dfsub<-subset(dfsub,dfsub$tm<=input$year_end)
      if (input$stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$carriage)
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
      p<-p+geom_point(aes(x=tm,y=carriage),color="red",size=3,shape=21)
    }
    p+ylab("客车车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
 
#----------------------------------------------------
  
#----------------------------------------------------   
  #多元回归预测计算
  output$carriage_output<-renderText({
    x1<-as.numeric(input$km_input)
    distance<-c(x1)
    tm<-c(2016)
    carriage<-c(0)
    inputdata<-data.frame(tm,carriage,distance)
    pred<-as.integer(predict(olsRegModel,inputdata))
    paste("多元回归预测：",pred ) 
    }
  )
#-------------------------------------------------
#随机森林回归预测计算
  output$carriage_FRR<-renderText({
    x1<-as.numeric(input$km_input)
    distance<-c(x1)
    tm<-c(2016)
    carriage<-c(0)
    inputdata<-data.frame(tm,carriage,distance)
    railCarriage<-predict(rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railCarriage[1])  ) 
  
  }
  )
#----------------------------------
  #支持向量机回归预测计算
  output$carriage_zhi<-renderText({
    x1<-as.numeric(input$km_input)
    distance<-c(x1)
    tm<-c(2016)
    carriage<-c(0)
    inputdata<-data.frame(tm,carriage,distance)
    pred<-as.integer(predict(svmRegModel,inputdata))
    
    paste("支持向量机预测：",pred)
    
  }
  )
#-------------------------------------
  
  
#-----------随机森林Tabset画线  
  output$rfplot <- renderPlot( {
    
    if(input$year_start> input$year_end)  {
      
      if (input$stat_data) {
        p<-plotCurve(df,df$tm,df$carriage)
      }
      else
      {
        p<-plotCurve(df,df$tm,df$frRegPred)
      }
    }
    else{
      dfsub<-subset(df,df$tm>=input$year_start) 
      dfsub<-subset(dfsub,dfsub$tm<=input$year_end)
      if (input$stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$carriage)
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
      p<-p+geom_point(aes(x=tm,y=carriage),color="red",size=3,shape=21)
    }
    p+ylab("客车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
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
      dfsub<-subset(df,df$tm>=input$year_start) 
      dfsub<-subset(dfsub,dfsub$tm<=input$year_end)
      if (input$stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$carriage)
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
      p<-p+geom_point(aes(x=tm,y=carriage),color="red",size=3,shape=21)
    }
    p+ylab("客车车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
#--------------------------------------
  
 #----------------------datatable显示数据
#-----------------在df中，又增加了3列数据，存放预测结果,
 
  output$table<-DT::renderDataTable(
    DT::datatable(
      data<-df, 
      colnames = c('序号', '年','客车辆数（辆）','营业里程（公里）','多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
      rownames = TRUE)
  )
 
#----------------------------------------------------
}
)