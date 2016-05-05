shinyServer(function(input, output) {
  require(ggplot2)
  require(DT)
  require(kernlab)
  source("randomForest.r")
#  source("SVM-1.R")

  df<-read.csv("客车车辆预测.csv",head=T)
  df$predict<-(-9604.696+0.682*df$distance)
  len<-length(df$tm)

  output$main_plot <- renderPlot( {
    
    if(input$year_start> input$year_end)  {
      
      if (input$stat_data) {
        
         p<-ggplot(df,x=c(df$tm[1],df$tm[len]),aes(x=tm,y=df$carriage),color="red")
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
        p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[len]),aes(x=tm,y=carriage))
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
      p<-p+geom_point(aes(x=tm,y=carriage),color="red",size=3,shape=21)
    }
    
    
    p+ylab("客车车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
    
  #if(input$method=="多元回归") {
  output$carriage_output<-renderText({
    #if(as.numeric(input$km_input)>0)
      {   paste("多元回归预测客车辆数为：", -9604.696+0.682*as.numeric(input$km_input)  ) 
       
      }
    }
  )

    #if(input$method=="随机森林回归"){
  output$carriage_FRR<-renderText({
    x1<-input$km_input
    #new_data<-data.frame(x1)
    tm<-c(2016)
    carriage<-c(0)
    distance<-c(x1)
    
    new_data<-data.frame(tm,carriage,distance,stringsAsFactors=FALSE)
    dfsub<-subset(df,select=c(tm,carriage,distance))
    #new_data需要修改为 data.Frame(tm,carrage,distance)
    total_data<-randFrstReg(dfsub,carriage~distance,new_data) 
    len<-dim(total_data)[1]
    paste("随机森林回归预测客车辆数为：",total_data[len,1]  ) 
  
  }
  )
 
    #支持向量机回归
  output$carriage_zhi<-renderText({
    x1<-as.numeric(input$km_input)
    tm<-c(2016)
    carriage<-c(100)
    distance<-c(x1)
    dfsub<-subset(df,select=c(tm,carriage,distance))
    new_data<-data.frame(tm,carriage,distance,stringsAsFactors=FALSE)
    #total_data<-svmReg(dfsub,carriage~distance,new_data)  
    svmRegModel<-ksvm(carriage~distance,data=dfsub,model="svm",type="eps-svr",cross=10)
    
    #svmRegModel<-svmReg(dfsub,carriage~distance)
    
    predict(svmRegModel,new_data)
    
   # len<-dim(total_data)[1]
    paste("支持向量机预测货运量为：",pred)
    
  }
  )
  

  output$table<-DT::renderDataTable(
    DT::datatable(
      {
        
        data<-df
      } , 
      colnames = c('序号', '时间','客车辆数','营业里程','预测值'),
      rownames = TRUE)
  )
 
}
)