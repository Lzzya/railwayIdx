shinyServer(function(input, output) {
  
  
  require(ggplot2)
  require(DT)
  require(e1071)
  require(randomForest)
  require(forecast)
  require(rJava)
  require(xlsx)
  
  df_monthly<-read.xlsx("rawdata_monthly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
  df_yearly<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
 #-------------------其它铁路原始数据----------------------
  #---------------------mashaomeng--------------------------
mengmeng_yearly<-read.xlsx("3-1 全国铁路线路、铁路复线、电气化、内燃牵引里程.xlsx",1,head=T,startRow=2,encoding = "UTF-8")#--------表3-1是3-1和3-6的合并
railway_mileage_yearly<-substr(mengmeng_yearly$tm,1,4)

mengmeng1_yearly<-read.xlsx("3-9 全国铁路机车拥有量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
Locomotive_ownership_yearly<-substr(mengmeng1_yearly$tm,1,4)

mengmeng2_yearly<-read.xlsx("3-2 全国铁路分地区营业里程.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
sub_regional_mileage_yearly<-substr(mengmeng2_yearly$tm,1,4)

mengmeng3_yearly<-read.xlsx("3-10 国家铁路分机型机车拥有量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
model_locomotive_ownership_yearly<-substr(mengmeng3_yearly$tm,1,4)

#---------------------mashaomeng--------------------------
#-------------聪聪-----------------------------
fcc_holding<-read.xlsx("3-13 全国铁路客、货车拥有量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")##聪聪3.13，全国客车货车拥有量情况
fcc_availability<-read.xlsx("3-12 国家铁路机、客、货车利用率.xlsx",1,head=T,startRow=2,encoding = "UTF-8")##聪聪3.12,合并进了3.17
fcc_kecheholding<-read.xlsx("3-14 国家铁路客车拥有量（分座次）.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
fcc_huocheholding<-read.xlsx("3-15 国家铁路分车型货车拥有量（分车辆种类）.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
fcc_sheshishebei<-read.xlsx("3-22 国家铁路车站主要客货运设施、设备.xlsx",1,head=T,startRow=2,encoding = "UTF-8")

#景钊，4-1，4-2，4-3，4-4合并
df_yslzzl_yearly<-read.xlsx("4-1 全国铁路运营量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
lxy1_yearly<-read.xlsx("4_16国家铁路省、市、自治区货运量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
lxy2_yearly<-read.xlsx("4_17国家铁路省、市、自治区货运周转量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
lxy3_yearly<-read.xlsx("4_18国家铁路省、市、自治区客运量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
lxy4_yearly<-read.xlsx("4_19国家铁路省、市、自治区客运周转量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")

##国家铁路指标
lxy5_yearly<-read.xlsx("5_1国家铁路机车运用指标.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
#-------李亚芳------------#
#-------读取铁路列车正点率原始数据--------
df_yearly_55<-read.xlsx("5-5 国家铁路列车正点率.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
#-------读取国家铁路机车能源消耗原始数据---------------
df_yearly_52<-read.xlsx("5-2 国家铁路机车能源消耗.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
#-------读取国家铁路机车工作量原始数据-----------------
df_yearly_53<-read.xlsx("5-3 国家铁路机车工作量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
#-------读取国家铁路货车运用指标原始数据---------------
df_yearly_54<-read.xlsx("5-4 国家铁路货车运用指标.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
#-------读取国家铁路固定资产投资原始数据---------------
df_yearly_61<-read.xlsx("6-1 全国铁路固定资产投资.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
#-------读取国家铁路基本建设投资原始数据---------------
df_yearly_62<-read.xlsx("6-2 全国铁路基本建设投资.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
table6.3 <- read.xlsx("6-3 铁道部基本建设投资的资金来源.xls",1,header = T,startRow=2,encoding = "UTF-8") 
table6.4 <- read.xlsx("6-4 全国铁路基本建设铺轨及投产里程.xls",1,header = T,startRow=2,encoding = "UTF-8")
table6.7 <- read.xlsx("6-7 国家铁路机车车辆购置.xls",1,header = T,startRow=2,encoding = "UTF-8")
    
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #铁路预警信号灯
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------
  #从x-12数据起计算预警指数
  
  index_x12data<-read.csv("预警 - from x-12.csv",head=T)
  
  n<-dim(index_x12data)[1]
  index_x12data2<-index_x12data[13:n,]
  index_x12data3<-index_x12data[1:12,]
  index_x12data_trs<-rbind(index_x12data2,index_x12data3)  #把第一年的数据放到最后形成新的数据集
  
  #-----------------------------------
  #时间列不能参与矩阵间的数字运算，把时间列单独取出来，需要的时候再合并到数据集
  tm<-index_x12data_trs[1:(n-12),1]
  tm<-as.data.frame(tm)
  names(tm)[1]<-"date"
  
  #去除时间列
  index_x12data_trs<-index_x12data_trs[,-1]
  index_x12data_trs<-as.data.frame(index_x12data_trs)
  
  #---------------------------------------------------------------
  #计算同比
  #
  index_x12data_tm<-index_x12data[,-1]
  index_tongbi<-(index_x12data_trs-index_x12data_tm)/index_x12data_tm
  m<-dim(index_tongbi)[2]
  index_tongbi<-index_tongbi[1:(n-12),1:m]
  #View(index_tongbi)
  
  #---------------------------------------------------------------
  #计算指标分位数
  index_mean<-apply(index_tongbi,2,mean)   #计算同比后各个指标的均值
  index_sd<-apply(index_tongbi,2,sd)       #计算同比后各个指标的标准差
  m<-dim(index_tongbi)[1]           #数据集的行数
  n<-dim(index_tongbi)[2]           #数据集的列数 
  
  index_standard<-(index_tongbi[1,]-index_mean)/index_sd
  for(i in 2:m){
    index_forstandard<-(index_tongbi[i,]-index_mean)/index_sd
    index_standard<-rbind(index_standard,index_forstandard)
  }
  
  delete_data<-c(-m:-1)
  index_tdata<-index_tongbi[delete_data,]  #建立一个空白数据集index_tdata,储存T分布累计概率密度
  
  for(i in 1:m){
    for(j in 1:n){
      index_tdata[i,j]<-pt(index_standard[i,j],m-1)
    }
  }
  
  index_weights_freight<-c(0.1572,0.1387,0.0563,0.1556,0.1922,0.1984,0.1016)  #货运各指标权重
  index_weights_passenger<-c(0.4668,0.5332)
  
  #sum(index_weights*index_tdata[1,])
  
  index_data_freight<-data.frame()
  index_data_passenger<-data.frame()
  for(i in 1:m){
    index_data_freight[i,1]<-sum(index_weights_freight*index_tdata[i,1:(n-2)])
    index_data_passenger[i,1]<-sum(index_weights_passenger*index_tdata[i,(n-1):n])
  }
  
  names(index_data_freight)<-"freight_index"
  names(index_data_passenger)<-"passenger_index"
  
  index_data<-cbind(tm,index_data_freight,index_data_passenger)
  index_data$date<-as.Date.POSIXct(index_data$date,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
  
  output$plot_index<-renderPlot({ 
    a<-length(index_data$date)
    p<-ggplot(data=index_data,aes(x=date,y=freight_index))
    p<-p+ylim(0,1)+xlim(index_data[1,1],index_data[a,1])
    p<-p+annotate("rect",xmin=index_data[1,1],xmax=index_data[a,1],ymin=0,ymax=0.15,fill="green",alpha=0.7)
    p<-p+annotate("rect",xmin=index_data[1,1],xmax=index_data[a,1],ymin=0.15,ymax=0.35,fill="green",alpha=0.7)
    p<-p+annotate("rect",xmin=index_data[1,1],xmax=index_data[a,1],ymin=0.35,ymax=0.65,fill="yellow",alpha=0.7)
    p<-p+annotate("rect",xmin=index_data[1,1],xmax=index_data[a,1],ymin=0.65,ymax=0.85,fill="red",alpha=0.7)
    p<-p+annotate("rect",xmin=index_data[1,1],xmax=index_data[a,1],ymin=0.85,ymax=1,fill="red",alpha=0.7)

    p<-p+geom_line(size=1)+geom_line(aes(x=date,y=passenger_index),color="blue",size=1)
    p+theme_bw()+theme(panel.border=element_blank())+xlab("日期")+ylab("指数")
  })
  
  output$index_table<-DT::renderDataTable(
    DT::datatable(
      data<-index_data, 
      colnames = c('日期','货运景气指数','客运景气指数'),
      rownames = TRUE))
  
  
  #————————————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————————————
  #铁路景气指数，包括合成指数和扩散指数
  #————————————————————————————————————————————————————————————————————————————————————————————————
  #-------------------------------------------------------
  #---------------合成指数--------------------------------
  
  #运输合成指数计算------------------------------
  dftrans<-read.xlsx("trans_index_x12.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
  dftrans$tm<-as.Date.POSIXct(dftrans$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  trans.len<-length(dftrans$tm)
  
  #-----运输----1. 权重计算函数--------------------
  percent.1<- function(x)
  { xlen<- length(x)
  x<- x/max(x)
  x<- -(1/log(xlen))*sum( (x/sum(x))*log((x/sum(x))) )
  x<- 1-x}
  
  #------运输---1.1 同步/一致指标的权重--------
  hyl.trans.percent<- percent.1(dftrans$hyl)/(percent.1(dftrans$hyl)+percent.1(dftrans$gyzjz)+percent.1(dftrans$hyzzl))
  gyzjz.trans.percent<- percent.1(dftrans$gyzjz)/(percent.1(dftrans$hyl)+percent.1(dftrans$gyzjz)+percent.1(dftrans$hyzzl))
  hyzzl.trans.percent<- percent.1(dftrans$hyzzl)/(percent.1(dftrans$hyl)+percent.1(dftrans$gyzjz)+percent.1(dftrans$hyzzl))
  
  #------运输----1.2 滞后指标的权重--------------------------------------------------------------- 
  kyl.trans.percent<- percent.1(dftrans$kyl)/(percent.1(dftrans$kyl)+percent.1(dftrans$kyzzl)+percent.1(dftrans$gdzctz))
  kyzzl.trans.percent<- percent.1(dftrans$kyzzl)/(percent.1(dftrans$kyl)+percent.1(dftrans$kyzzl)+percent.1(dftrans$gdzctz))
  gdzctz.trans.percent<- percent.1(dftrans$gdzctz)/(percent.1(dftrans$kyl)+percent.1(dftrans$kyzzl)+percent.1(dftrans$gdzctz))
  
  #----运输------1.3 先行指标的权重--------------------------------------------------------------- 
  gc.trans.percent<- percent.1(dftrans$gc)/(percent.1(dftrans$gc)+percent.1(dftrans$ym)+percent.1(dftrans$yy)+percent.1(dftrans$hlfdl))
  ym.trans.percent<- percent.1(dftrans$ym)/(percent.1(dftrans$gc)+percent.1(dftrans$ym)+percent.1(dftrans$yy)+percent.1(dftrans$hlfdl))
  yy.trans.percent<- percent.1(dftrans$yy)/(percent.1(dftrans$gc)+percent.1(dftrans$ym)+percent.1(dftrans$yy)+percent.1(dftrans$hlfdl))
  hlfdl.trans.percent<- percent.1(dftrans$hlfdl)/(percent.1(dftrans$gc)+percent.1(dftrans$ym)+percent.1(dftrans$yy)+percent.1(dftrans$hlfdl))
  
  #-----运输----2. 合成指数计算------------------------  
  
  #-----运输----2.1 标准化变化率计算--------
  dftrans2<-dftrans[dftrans$x12hyzzl!=0,]
  index.1<- function(z)
  { zlen<- length(z)
  z[3:zlen]<- 200*(z[3:zlen]-z[2:(zlen-1)])/(z[3:zlen]+z[2:(zlen-1)])
  z[2]<- 0
  z[1]<- 0
  z<- z/(sum(abs(z))/(zlen-2))}  #标准化变化率计算函数
  
  index.2<- function(z)
  {zlen<- length(z)
  z[2:zlen]<- 200*(z[2:zlen]-z[1:(zlen-1)])/(z[2:zlen]+z[1:(zlen-1)])
  z[1]<- 0
  z<- z/(sum(abs(z))/(zlen-1))}
  
  #-----运输----2.2 平均变化率R----------------------------------------------------------------------------
  coor.trans.test<- index.2(dftrans2$x12hyl)*hyl.trans.percent + index.2(dftrans2$x12hyzzl)*hyzzl.trans.percent+index.2(dftrans2$x12gyzjz)*gyzjz.trans.percent
  #coor.test一致合成指数平均变化率R2
  adv.trans.test<- index.2(dftrans2$x12gc)*(gc.trans.percent-0.2)+ index.2(dftrans2$x12ym)*ym.trans.percent+index.2(dftrans2$x12yy)*(yy.trans.percent+0.1)+index.2(dftrans2$x12hlfdl)*(hlfdl.trans.percent+0.1)
  #adv.trans.test先行合成指数平均变化率R1
  delay.trans.test<- index.2(dftrans2$x12kyl)*kyl.trans.percent + index.2(dftrans2$x12kyzzl)*kyzzl.trans.percent+index.2(dftrans2$x12gdzctz)*gdzctz.trans.percent
  #coor.trans.test滞后合成指数平均变化率R3
  
  #-----运输----2.3 标准化因子F----------------------------------------------
  biaozhunhua.F.coor<- 1  #同步的标准化因子是1
  biaozhunhua.trans.F.adv<- sum(abs(adv.trans.test))/sum(abs(coor.trans.test)) #先行的标准化因子
  biaozhunhua.trans.F.delay<- sum(abs(delay.trans.test))/sum(abs(coor.trans.test)) #滞后的标准化因子
  
  #-----运输----2.4 合成指数计算---------------
  hecheng.trans.index<- function(a,b)
  { alen<- length(a)
  a<- a/b
  a[1]<- 100
  a[2:alen]<- (200+a[2:alen])/(200-a[2:alen])#(200+当列)/(200-当列)
  a1<- a
  for(i in 2:alen){a1[i]<- a1[i-1]*a[i] }#a1[i]=a1[i-1]*a[i]初步合成指数
  index.jizhunnianfen<- mean(a1[37:48]) #抽取基准年份的平均值
  a1<- 100*a1/index.jizhunnianfen
  return(a1)
  }
  
  #默认权重计算得到的运输同步、先行、滞后指数
  trans.coor<- hecheng.trans.index(coor.trans.test,biaozhunhua.F.coor)
  trans.adv<- hecheng.trans.index(adv.trans.test,biaozhunhua.trans.F.adv)
  trans.delay<- hecheng.trans.index(delay.trans.test,biaozhunhua.trans.F.delay)
  
  dftrans2$coor<- trans.coor
  dftrans2$adv<- trans.adv
  dftrans2$delay<- trans.delay
  
  #-----------运输的算完了！！----3.运输画线和显示数据表--------
  percent.input<- function(a)
  {a<- as.numeric(a)/100}  #权重手动输入部分计算的函数们
  
  output$trans_index<- renderPlot( {
    #---权重手动输入的计算----------
    hyl.qz.input<- percent.input(input$trans_hyl_qz_input)
    gyzjz.qz.input<- percent.input(input$trans_gyzjz_qz_input)
    hyzzl.qz.input<- percent.input(input$trans_hyzzl_qz_input)
    gc.qz.input<- percent.input(input$trans_gc_qz_input)
    ym.qz.input<- percent.input(input$trans_ym_qz_input)
    yy.qz.input<- percent.input(input$trans_yy_qz_input)
    hlfdl.qz.input<- percent.input(input$trans_hlfdl_qz_input)
    kyl.qz.input<- percent.input(input$trans_kyl_qz_input)
    gdzctz.qz.input<- percent.input(input$trans_gdzctz_qz_input)
    kyzzl.qz.input<- percent.input(input$trans_kyzzl_qz_input)
    
    coor.test.input<- index.2(dftrans2$x12hyl)*hyl.qz.input+index.2(dftrans2$x12hyzzl)*hyzzl.qz.input+index.2(dftrans2$x12gyzjz)*gyzjz.qz.input
    adv.test.input<- index.2(dftrans2$x12gc)*gc.qz.input + index.2(dftrans2$x12ym)*ym.qz.input+index.2(dftrans2$x12yy)*yy.qz.input+index.2(dftrans2$x12hlfdl)*hlfdl.qz.input
    delay.test.input<- index.2(dftrans2$x12kyl)*kyl.qz.input + index.2(dftrans2$x12kyzzl)*kyzzl.qz.input+index.2(dftrans2$x12gdzctz)*gdzctz.qz.input
    
    biaozhunhua.F.adv.input<- sum(abs(adv.test.input))/sum(abs(coor.test.input)) 
    biaozhunhua.F.delay.input<- sum(abs(delay.test.input))/sum(abs(coor.test.input))
    
    trans.coor.input<- hecheng.trans.index(coor.test.input,biaozhunhua.F.coor)
    trans.adv.input<- hecheng.trans.index(adv.test.input,biaozhunhua.F.adv.input)
    trans.delay.input<- hecheng.trans.index(delay.test.input,biaozhunhua.F.delay.input)
    
    dftrans2$coor.input<- trans.coor.input   
    dftrans2$adv.input<- trans.adv.input
    dftrans2$delay.input<- trans.delay.input
    
    #-----运输----3.1 运输默认权重计算的画线------------  
    if(input$year_start_trans> input$year_end_trans)  {
      p<-ggplot(dftrans2,x=c(dftrans2$tm[1],dftrans2$tm[trans.len]),aes(x=tm,y=100))}
    else{
      dftranssub<-subset(dftrans2,(substr(dftrans2$tm,1,4)>=input$year_start_trans) )
      dftranssub<-subset(dftranssub,(substr(dftranssub$tm,1,4)<=input$year_end_trans))
      p<-ggplot(dftranssub,x=c(dftranssub$tm[1],dftranssub$tm[trans.len]),aes(x=tm,y=100))}
    
    if(input$trans_coor_Index){
      p<-p+geom_line(aes(x=tm,y=dftranssub$coor),color="black",size=1)}
    if (input$trans_advanced_Index) {
      p<-p+geom_line(aes(x=tm,y=dftranssub$adv),color="red",size=1) }
    if (input$trans_delay_Index) {
      p<-p+geom_line(aes(x=tm,y=dftranssub$delay),color="blue",size=1)}
    
    #----运输----3.2 输入修改权重后算出来的新先行指数------------------
    if(input$trans_qz_coor_input)#输入修改权重后算出来的新先行指数
    { p<-p+geom_line(aes(x=tm,y=dftranssub$coor.input),color="black",size=1.6,linetype=1)}
    if(input$trans_qz_adv_input)
    {p<-p+geom_line(aes(x=tm,y=dftranssub$adv.input),color="red",size=1.6,linetype=1)}
    if(input$trans_qz_delay_input)
    {p<-p+geom_line(aes(x=tm,y=dftranssub$delay.input),color="blue",size=1.6,linetype=1)}  
    
    p+ylab("运输合成指数")+xlab("时间")+geom_line()
  })
  
  #----运输-----4.数据表的显示------------------------------------ 
  output$table_trans_index<-DT::renderDataTable({
    
    hyl.qz.input<- percent.input(input$trans_hyl_qz_input)
    gyzjz.qz.input<- percent.input(input$trans_gyzjz_qz_input)
    hyzzl.qz.input<- percent.input(input$trans_hyzzl_qz_input)
    gc.qz.input<- percent.input(input$trans_gc_qz_input)
    ym.qz.input<- percent.input(input$trans_ym_qz_input)
    yy.qz.input<- percent.input(input$trans_yy_qz_input)
    hlfdl.qz.input<- percent.input(input$trans_hlfdl_qz_input)
    kyl.qz.input<- percent.input(input$trans_kyl_qz_input)
    gdzctz.qz.input<- percent.input(input$trans_gdzctz_qz_input)
    kyzzl.qz.input<- percent.input(input$trans_kyzzl_qz_input)
    
    coor.test.input<- index.2(dftrans2$x12hyl)*hyl.qz.input+index.2(dftrans2$x12hyzzl)*hyzzl.qz.input+index.2(dftrans2$x12gyzjz)*gyzjz.qz.input
    adv.test.input<- index.2(dftrans2$x12gc)*gc.qz.input + index.2(dftrans2$x12ym)*ym.qz.input+index.2(dftrans2$x12yy)*yy.qz.input+index.2(dftrans2$x12hlfdl)*hlfdl.qz.input
    delay.test.input<- index.2(dftrans2$x12kyl)*kyl.qz.input + index.2(dftrans2$x12kyzzl)*kyzzl.qz.input+index.2(dftrans2$x12gdzctz)*gdzctz.qz.input
    
    biaozhunhua.F.adv.input<- sum(abs(adv.test.input))/sum(abs(coor.test.input)) 
    biaozhunhua.F.delay.input<- sum(abs(delay.test.input))/sum(abs(coor.test.input))
    
    trans.coor.input<- hecheng.trans.index(coor.test.input,biaozhunhua.F.coor)
    trans.adv.input<- hecheng.trans.index(adv.test.input,biaozhunhua.F.adv.input)
    trans.delay.input<- hecheng.trans.index(delay.test.input,biaozhunhua.F.delay.input)
    
    dftrans2$coor.input<- trans.coor.input   
    dftrans2$adv.input<- trans.adv.input
    dftrans2$delay.input<- trans.delay.input
    
    #----运输----4.1 输入修改权重后算出来的三个指数------------------   
    if(input$trans_qz_coor_input|input$trans_qz_adv_input|input$trans_qz_delay_input){
      DT::datatable(
        { dftrans<- data.frame(dftrans2$tm,dftrans2$adv.input,dftrans2$coor.input,dftrans2$delay.input)
        data<-dftrans},
        colnames = c('时间', '先行指数',  '同步指数','滞后指数'),
        rownames = TRUE)}
    #----运输----4.2 默认权重计算下的三个指数------------------  
    else{ 
      DT::datatable(
        { dftrans<- data.frame(dftrans2$tm,dftrans2$adv,dftrans2$coor,dftrans2$delay)
        data<-dftrans},
        colnames = c('时间', '运输合成先行指数',  '运输合成同步指数','运输合成滞后指数'),
        rownames = TRUE)
    }
  })
  
  #-----设备合成指数计算-------------------------------
  dfequipall<-data.frame(df_yearly$tm,df_yearly$iron_output_yearly,df_yearly$coal_output_yearly,df_yearly$oil_processing_volume_yearly,df_yearly$coalfired_power_generation_yearly,df_yearly$locomotive_mileage_sum,df_yearly$dailycar_run,df_yearly$dailycar_now,df_yearly$locomotive_mileage_pcar,df_yearly$locomotive_mileage_fcar,df_yearly$passenger_car,df_yearly$freight_car,df_yearly$locomotive_number,df_yearly$practitioner_number)
  dfequip<-subset(dfequipall,(substr(dfequipall$df_yearly.tm,1,4)>="2001") )
  dfequip$df_yearly.tm<-as.Date.POSIXct(dfequip$df_yearly.tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  equip.len<-length(dfequip$df_yearly.tm)
  
  #---------设备 1. 权重计算------------------------
  #---函数公式同上，不用再写一遍
  
  #----------设备 1.1 同步/一致指标的权重---------------------------------------------------------------
  locomotive_mileage_sum.equip.qz<- percent.1(dfequip$df_yearly.locomotive_mileage_sum)/(percent.1(dfequip$df_yearly.locomotive_mileage_sum)+percent.1(dfequip$df_yearly.dailycar_run))
  dailycar_run.equip.qz<- percent.1(dfequip$df_yearly.dailycar_run)/(percent.1(dfequip$df_yearly.locomotive_mileage_sum)+percent.1(dfequip$df_yearly.dailycar_run))
  
  #----------设备 1.2 滞后指标的权重--------------------------------------------------------------- 
  dailycar_now.equip.qz<- percent.1(dfequip$df_yearly.dailycar_now)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  locomotive_mileage_pcar.equip.qz<- percent.1(dfequip$df_yearly.locomotive_mileage_pcar)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  locomotive_mileage_fcar.equip.qz<- percent.1(dfequip$df_yearly.locomotive_mileage_fcar)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  passenger_car.equip.qz<- percent.1(dfequip$df_yearly.passenger_car)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  freight_car.equip.qz<- percent.1(dfequip$df_yearly.freight_car)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  locomotive_number.equip.qz<- percent.1(dfequip$df_yearly.locomotive_number)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  
  #----------设备 1.3 先行指标的权重，计算出来同运输的不一样，所以变量标注2--------------------------------------------------------------- 
  iron_output_yearly.equip.qz<- percent.1(dfequip$df_yearly.iron_output_yearly)/(percent.1(dfequip$df_yearly.iron_output_yearly)+percent.1(dfequip$df_yearly.coal_output_yearly)+percent.1(dfequip$df_yearly.oil_processing_volume_yearly)+percent.1(dfequip$df_yearly.coalfired_power_generation_yearly))
  coal_output_yearly.equip.qz<- percent.1(dfequip$df_yearly.coal_output_yearly)/(percent.1(dfequip$df_yearly.iron_output_yearly)+percent.1(dfequip$df_yearly.coal_output_yearly)+percent.1(dfequip$df_yearly.oil_processing_volume_yearly)+percent.1(dfequip$df_yearly.coalfired_power_generation_yearly))
  oil_processing_volume_yearly.equip.qz<- percent.1(dfequip$df_yearly.oil_processing_volume_yearly)/(percent.1(dfequip$df_yearly.iron_output_yearly)+percent.1(dfequip$df_yearly.coal_output_yearly)+percent.1(dfequip$df_yearly.oil_processing_volume_yearly)+percent.1(dfequip$df_yearly.coalfired_power_generation_yearly))
  coalfired_power_generation_yearly.equip.qz<- percent.1(dfequip$df_yearly.coalfired_power_generation_yearly)/(percent.1(dfequip$df_yearly.iron_output_yearly)+percent.1(dfequip$df_yearly.coal_output_yearly)+percent.1(dfequip$df_yearly.oil_processing_volume_yearly)+percent.1(dfequip$df_yearly.coalfired_power_generation_yearly))
  
  #---------设备 2. 合成指数计算------------------------  
  
  #--------设备的合成指数不用去季节化，因为本来就是年度数据，直接计算增长率---------
  rate.1<- function(m)
  { m[2:length(m)]<- m[2:length(m)]/m[1:(length(m)-1)]-1
  m[1]<- 0
  return(m)}
  
  #---------设备 2.1 标准化变化率计算----同上运输标准化变化率，不用再重复----
  
  #---------设备 2.2 平均变化率R----------------------------------------------------------------------------
  coor.equip.test<- index.1(rate.1(dfequip$df_yearly.locomotive_mileage_sum))*locomotive_mileage_sum.equip.qz + index.1(rate.1(dfequip$df_yearly.dailycar_run))*dailycar_run.equip.qz
  #coor2.test一致合成指数平均变化率R2
  adv.equip.test<- index.1(rate.1(dfequip$df_yearly.iron_output_yearly))*iron_output_yearly.equip.qz + index.1(rate.1(dfequip$df_yearly.coal_output_yearly))*coal_output_yearly.equip.qz+index.1(rate.1(dfequip$df_yearly.oil_processing_volume_yearly))*oil_processing_volume_yearly.equip.qz+index.1(rate.1(dfequip$df_yearly.coalfired_power_generation_yearly))*coalfired_power_generation_yearly.equip.qz
  #coor2.test先行合成指数平均变化率R1
  delay.equip.test<- index.1(rate.1(dfequip$df_yearly.dailycar_now))*dailycar_now.equip.qz+index.1(rate.1(dfequip$df_yearly.locomotive_mileage_pcar))*locomotive_mileage_pcar.equip.qz+index.1(rate.1(dfequip$df_yearly.locomotive_mileage_fcar))*locomotive_mileage_fcar.equip.qz+index.1(rate.1(dfequip$df_yearly.passenger_car))*passenger_car.equip.qz+index.1(rate.1(dfequip$df_yearly.freight_car))*freight_car.equip.qz+index.1(rate.1(dfequip$df_yearly.locomotive_number))*locomotive_number.equip.qz
  #coor2.test滞后合成指数平均变化率R3
  
  #---------设备 2.3 标准化因子F----------------------------------------------
  biaozhunhua.equip.F.coor<- 1  #同步的标准化因子是1
  biaozhunhua.equip.F.adv<- sum(abs(adv.equip.test))/sum(abs(coor.equip.test)) #先行的标准化因子
  biaozhunhua.equip.F.delay<- sum(abs(delay.equip.test))/sum(abs(coor.equip.test)) #滞后的标准化因子
  
  #---------设备 2.4 合成指数计算--设备的合成基本年数的值设为100-------------
  hecheng.equip.index<- function(a,b)
  {
    alen<- length(a)
    a<- a/b
    a[1]<- 100
    a[2:alen]<- (200+a[2:alen])/(200-a[2:alen])#(200+当列)/(200-当列)
    a1<- a
    for(i in 2:alen){a1[i]<- a1[i-1]*a[i] }#a1[i]=a1[i-1]*a[i]初步合成指数
    index.jizhunnianfen<- a1[2] #抽取基准年份的平均值
    a1<- 100*a1/index.jizhunnianfen
    return(a1)
  }
  
  equip.coor<- hecheng.equip.index(coor.equip.test,biaozhunhua.equip.F.coor)
  equip.adv<- hecheng.equip.index(adv.equip.test,biaozhunhua.equip.F.adv)
  equip.delay<- hecheng.equip.index(delay.equip.test,biaozhunhua.equip.F.delay)
  
  dfequip$coor<- c(1:equip.len)
  dfequip$adv<- c(1:equip.len)
  dfequip$delay<- c(1:equip.len)
  dfequip$coor<- equip.coor
  dfequip$adv<- equip.adv
  dfequip$delay<- equip.delay
  
  #-----------设备的算完了！！----3.设备 画线和显示数据表--------
  output$equip_index<- renderPlot( {
    
    rjyyc.qz.input<- percent.input(input$equip_rjyyc_qz_input)
    jczxzlc.qz.input<- percent.input(input$equip_jczxzlc_qz_input)
    gc.qz.input<- percent.input(input$equip_gc_qz_input)
    ym.qz.input<- percent.input(input$equip_ym_qz_input)
    yy.qz.input<- percent.input(input$equip_yy_qz_input)
    hlfdl.qz.input<- percent.input(input$equip_hlfdl_qz_input)
    rjxzc.qz.input<- percent.input(input$equip_rjxzc_qz_input)
    kyjclc.qz.input<- percent.input(input$equip_kyjclc_qz_input)
    hyjclc.qz.input<- percent.input(input$equip_hyjclc_qz_input)
    kcls.qz.input<- percent.input(input$equip_kcls_qz_input)
    hcls.qz.input<- percent.input(input$equip_hcls_qz_input)
    jcts.qz.input<- percent.input(input$equip_jcts_qz_input)
    
    equip.coor.test.input<- index.1(rate.1(dfequip$df_yearly.locomotive_mileage_sum))*jczxzlc.qz.input + index.1(rate.1(dfequip$df_yearly.dailycar_run))*rjyyc.qz.input
    equip.adv.test.input<- index.1(rate.1(dfequip$df_yearly.iron_output_yearly))*gc.qz.input + index.1(rate.1(dfequip$df_yearly.coal_output_yearly))*ym.qz.input+index.1(rate.1(dfequip$df_yearly.oil_processing_volume_yearly))*yy.qz.input+index.1(rate.1(dfequip$df_yearly.coalfired_power_generation_yearly))*hlfdl.qz.input
    equip.delay.test.input<- index.1(rate.1(dfequip$df_yearly.dailycar_now))*rjxzc.qz.input+index.1(rate.1(dfequip$df_yearly.locomotive_mileage_pcar))*kyjclc.qz.input+index.1(rate.1(dfequip$df_yearly.locomotive_mileage_fcar))*hyjclc.qz.input+index.1(rate.1(dfequip$df_yearly.passenger_car))*kcls.qz.input+index.1(rate.1(dfequip$df_yearly.freight_car))*hcls.qz.input+index.1(rate.1(dfequip$df_yearly.locomotive_number))*jcts.qz.input
    
    equip.biaozhunhua.F.adv.input<- sum(abs(equip.adv.test.input))/sum(abs(equip.coor.test.input)) 
    equip.biaozhunhua.F.delay.input<- sum(abs(equip.delay.test.input))/sum(abs(equip.coor.test.input))
    
    equip.coor.input<- hecheng.equip.index(equip.coor.test.input,biaozhunhua.F.coor)
    equip.adv.input<- hecheng.equip.index(equip.adv.test.input,equip.biaozhunhua.F.adv.input)
    equip.delay.input<- hecheng.equip.index(equip.delay.test.input,equip.biaozhunhua.F.delay.input)
    
    dfequip$coor.input<- equip.coor.input   
    dfequip$adv.input<- equip.adv.input
    dfequip$delay.input<- equip.delay.input
    
    #-----设备----3.1 默认权重计算的画线------------     
    if(input$year_start_equip > input$year_end_equip)  {
      p<-ggplot(dfequip,x=c(dfequip$df_yearly.tm[1],dfequip$df_yearly.tm[equip.len]),aes(x=df_yearly.tm,y=100))  }
    else{
      dfequipsub<-subset(dfequip,(substr(dfequip$df_yearly.tm,1,4)>=input$year_start_equip) )
      dfequipsub<-subset(dfequipsub,(substr(dfequipsub$df_yearly.tm,1,4)<=input$year_end_equip))
      p<-ggplot(dfequipsub,x=c(dfequipsub$df_yearly.tm[1],dfequipsub$df_yearly.tm[equip.len]),aes(x=df_yearly.tm,y=100))    }
    
    if(input$equip_coor_Index){
      p<-p+geom_line(aes(x=df_yearly.tm,y=dfequipsub$coor),color="black",size=0.6)
      p<-p+geom_point(aes(x=df_yearly.tm,y=dfequipsub$coor),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if (input$equip_advanced_Index) {
      p<-p+geom_line(aes(x=df_yearly.tm,y=dfequipsub$adv),color="red",size=0.6)
      p<-p+geom_point(aes(x=df_yearly.tm,y=dfequipsub$adv),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if (input$equip_delay_Index) {
      p<-p+geom_line(aes(x=df_yearly.tm,y=dfequipsub$delay),color="blue",size=0.6)
      p<-p+geom_point(aes(x=df_yearly.tm,y=dfequipsub$delay),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    
    #-----设备----3.2 权重手动输入后的画线------------      
    if(input$equip_qz_coor_input)#输入修改权重后算出来的新先行指数
    { p<-p+geom_line(aes(x=df_yearly.tm,y=dfequipsub$coor.input),color="black",size=1,linetype=1)
    p<-p+geom_point(aes(x=df_yearly.tm,y=dfequipsub$coor.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if(input$equip_qz_adv_input)
    {p<-p+geom_line(aes(x=df_yearly.tm,y=dfequipsub$adv.input),color="red",size=1,linetype=1)
    p<-p+geom_point(aes(x=df_yearly.tm,y=dfequipsub$adv.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if(input$equip_qz_delay_input)
    {p<-p+geom_line(aes(x=df_yearly.tm,y=dfequipsub$delay.input),color="blue",size=1,linetype=1)
    p<-p+geom_point(aes(x=df_yearly.tm,y=dfequipsub$delay.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))} 
    
    
    p+ylab("设备合成指数")+xlab("时间")+geom_line()
  })
  
  output$table_equip_index<-DT::renderDataTable({
    
    rjyyc.qz.input<- percent.input(input$equip_rjyyc_qz_input)
    jczxzlc.qz.input<- percent.input(input$equip_jczxzlc_qz_input)
    gc.qz.input<- percent.input(input$equip_gc_qz_input)
    ym.qz.input<- percent.input(input$equip_ym_qz_input)
    yy.qz.input<- percent.input(input$equip_yy_qz_input)
    hlfdl.qz.input<- percent.input(input$equip_hlfdl_qz_input)
    rjxzc.qz.input<- percent.input(input$equip_rjxzc_qz_input)
    kyjclc.qz.input<- percent.input(input$equip_kyjclc_qz_input)
    hyjclc.qz.input<- percent.input(input$equip_hyjclc_qz_input)
    kcls.qz.input<- percent.input(input$equip_kcls_qz_input)
    hcls.qz.input<- percent.input(input$equip_hcls_qz_input)
    jcts.qz.input<- percent.input(input$equip_jcts_qz_input)
    
    equip.coor.test.input<- index.1(rate.1(dfequip$df_yearly.locomotive_mileage_sum))*jczxzlc.qz.input + index.1(rate.1(dfequip$df_yearly.dailycar_run))*rjyyc.qz.input
    equip.adv.test.input<- index.1(rate.1(dfequip$df_yearly.iron_output_yearly))*gc.qz.input + index.1(rate.1(dfequip$df_yearly.coal_output_yearly))*ym.qz.input+index.1(rate.1(dfequip$df_yearly.oil_processing_volume_yearly))*yy.qz.input+index.1(rate.1(dfequip$df_yearly.coalfired_power_generation_yearly))*hlfdl.qz.input
    equip.delay.test.input<- index.1(rate.1(dfequip$df_yearly.dailycar_now))*rjxzc.qz.input+index.1(rate.1(dfequip$df_yearly.locomotive_mileage_pcar))*kyjclc.qz.input+index.1(rate.1(dfequip$df_yearly.locomotive_mileage_fcar))*hyjclc.qz.input+index.1(rate.1(dfequip$df_yearly.passenger_car))*kcls.qz.input+index.1(rate.1(dfequip$df_yearly.freight_car))*hcls.qz.input+index.1(rate.1(dfequip$df_yearly.locomotive_number))*jcts.qz.input
    
    equip.biaozhunhua.F.adv.input<- sum(abs(equip.adv.test.input))/sum(abs(equip.coor.test.input)) 
    equip.biaozhunhua.F.delay.input<- sum(abs(equip.delay.test.input))/sum(abs(equip.coor.test.input))
    
    equip.coor.input<- hecheng.equip.index(equip.coor.test.input,biaozhunhua.F.coor)
    equip.adv.input<- hecheng.equip.index(equip.adv.test.input,equip.biaozhunhua.F.adv.input)
    equip.delay.input<- hecheng.equip.index(equip.delay.test.input,equip.biaozhunhua.F.delay.input)
    
    dfequip$coor.input<- equip.coor.input   
    dfequip$adv.input<- equip.adv.input
    dfequip$delay.input<- equip.delay.input
    
    if(input$equip_qz_coor_input|input$equip_qz_adv_input|input$equip_qz_delay_input){
      DT::datatable(
        { dfequip<- data.frame(dfequip$df_yearly.tm,dfequip$adv.input,dfequip$coor.input,dfequip$delay.input)
        data<-dfequip},
        colnames = c('时间', '先行指数',  '同步指数','滞后指数'),
        rownames = TRUE)}
    
    else{DT::datatable(
      {data<-data.frame(dfequip$df_yearly.tm,dfequip$adv,dfequip$coor,dfequip$delay)},
      colnames = c('时间', '设备合成先行指数',  '设备合成同步指数','设备合成滞后指数'),
      rownames = TRUE)}
  })
  
  #-----规模合成指数计算-------------------------------
  dfscaleall<-data.frame(df_yearly$tm,df_yearly$Industrial_Added_Value_Rate_yearly,df_yearly$freight_volume_28_yearly,df_yearly$freight_rotation_volume_yearly,df_yearly$iron_output_yearly,df_yearly$coal_output_yearly,df_yearly$oil_processing_volume_yearly,df_yearly$coalfired_power_generation_yearly,df_yearly$passenger_car,df_yearly$freight_car,df_yearly$mileage,df_yearly$practitioner_number,df_yearly$locomotive_number)
  dfscale<-subset(dfscaleall,(substr(dfscaleall$df_yearly.tm,1,4)>="2001") )
  dfscale$df_yearly.tm<-as.Date.POSIXct(dfscale$df_yearly.tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  scale.len<-length(dfscale$df_yearly.tm)
  
  #---------规模 1. 权重计算------------------------
  #---函数公式同上，不用再写一遍
  
  #---------规模 1.1 同步/一致指标的权重---------------------------------------------------------------
  gyzjz.scale.qz<- percent.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)/(percent.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)+percent.1(dfscale$df_yearly.freight_volume_28_yearly)+percent.1(dfscale$df_yearly.freight_rotation_volume_yearly))
  hyl.scale.qz<- percent.1(dfscale$df_yearly.freight_volume_28_yearly)/(percent.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)+percent.1(dfscale$df_yearly.freight_volume_28_yearly)+percent.1(dfscale$df_yearly.freight_rotation_volume_yearly))
  hyzzl.scale.qz<- percent.1(dfscale$df_yearly.freight_rotation_volume_yearly)/(percent.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)+percent.1(dfscale$df_yearly.freight_volume_28_yearly)+percent.1(dfscale$df_yearly.freight_rotation_volume_yearly))
  #----------规模 1.2 滞后指标的权重--------------------------------------------------------------- 
  kcls.scale.qz<- percent.1(dfscale$df_yearly.passenger_car)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  hcls.scale.qz<- percent.1(dfscale$df_yearly.freight_car)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  yylc.scale.qz<- percent.1(dfscale$df_yearly.mileage)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  cyrysl.scale.qz<- percent.1(dfscale$df_yearly.practitioner_number)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  jcts.scale.qz<- percent.1(dfscale$df_yearly.locomotive_number)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  
  #----------规模 1.3 先行指标的权重，计算出来同规模一样，直接用设备的即可--------------------------------------------------------------- 
  
  #---------规模 2. 合成指数计算----规模的合成指数不用去季节化，因为本来就是年度数据，直接计算增长率-----------------------------  
  #---------规模 2.1 标准化变化率计算----同上设备标准化变化率，不用再重复----
  
  #---------规模 2.2 平均变化率R----------------------------------------------------------------------------
  coor.scale.test<- index.1(rate.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly))*gyzjz.scale.qz + index.1(rate.1(dfscale$df_yearly.freight_volume_28_yearly))*hyl.scale.qz + index.1(rate.1(dfscale$df_yearly.freight_rotation_volume_yearly))*hyzzl.scale.qz
  #coor.scale.test一致合成指数平均变化率R2
  #adv.equip.test<- index.1(rate.1(dfscale$df_yearly.iron_output_yearly))*gc2.qz + index.1(rate.1(dfscale$df_yearly.coal_output_yearly))*ym2.qz+index.1(rate.1(dfscale$df_yearly.oil_processing_volume_yearly))*yy2.qz+index.1(rate.1(dfscale$df_yearly.coalfired_power_generation_yearly))*hlfdl2.qz
  #adv.scale.test先行合成指数平均变化率R1 同coor.equip.test
  
  delay.scale.test<- index.1(rate.1(dfscale$df_yearly.passenger_car))*kcls.scale.qz+index.1(rate.1(dfscale$df_yearly.freight_car))*hcls.scale.qz+index.1(rate.1(dfscale$df_yearly.mileage))*yylc.scale.qz+index.1(rate.1(dfscale$df_yearly.practitioner_number))*cyrysl.scale.qz+index.1(rate.1(dfscale$df_yearly.locomotive_number))*jcts.scale.qz  #delay3.test滞后合成指数平均变化率R3
  
  #---------规模 2.3 标准化因子F----------------------------------------------
  biaozhunhua.scale.F.coor<- 1  #同步的标准化因子是1
  biaozhunhua.scale.F.adv<- sum(abs(adv.equip.test))/sum(abs(coor.scale.test)) #先行的标准化因子
  biaozhunhua.scale.F.delay<- sum(abs(delay.scale.test))/sum(abs(coor.scale.test)) #滞后的标准化因子
  
  #---------规模 2.4 合成指数计算--规模的合成基本年数的值同设备的，都设为100，也不用写了-------------
  scale.coor<- hecheng.equip.index(coor.scale.test,biaozhunhua.scale.F.coor)
  scale.adv<- hecheng.equip.index(adv.equip.test,biaozhunhua.scale.F.adv)
  scale.delay<- hecheng.equip.index(delay.scale.test,biaozhunhua.scale.F.delay)
  
  dfscale$coor<- scale.coor
  dfscale$adv<- scale.adv
  dfscale$delay<- scale.delay
  
  #-----------规模的也算完了！！----底下是画线和显示数据表--------
  
  output$scale_index<- renderPlot( {
    
    hyl.qz.input<- percent.input(input$scale_hyl_qz_input)
    gyzjz.qz.input<- percent.input(input$scale_gyzjz_qz_input)
    hyzzl.qz.input<- percent.input(input$scale_hyzzl_qz_input)
    gc.qz.input<- percent.input(input$scale_gc_qz_input)
    ym.qz.input<- percent.input(input$scale_ym_qz_input)
    yy.qz.input<- percent.input(input$scale_yy_qz_input)
    hlfdl.qz.input<- percent.input(input$scale_hlfdl_qz_input)
    kcls.qz.input<- percent.input(input$scale_kcls_qz_input)
    hcls.qz.input<- percent.input(input$scale_hcls_qz_input)
    yylc.qz.input<- percent.input(input$scale_yylc_qz_input)
    cyrysl.qz.input<- percent.input(input$scale_cyrysl_qz_input)
    jcts.qz.input<- percent.input(input$scale_jcts_qz_input)
    
    coor.test.input<- index.1(rate.1(dfscale$df_yearly.freight_volume_28_yearly))*hyl.qz.input+index.1(rate.1(dfscale$df_yearly.freight_rotation_volume_yearly))*hyzzl.qz.input+index.1(rate.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly))*gyzjz.qz.input
    adv.test.input<- index.1(rate.1(dfscale$df_yearly.iron_output_yearly))*gc.qz.input + index.1(rate.1(dfscale$df_yearly.coal_output_yearly))*ym.qz.input+index.1(rate.1(dfscale$df_yearly.oil_processing_volume_yearly))*yy.qz.input+index.1(rate.1(dfscale$df_yearly.coalfired_power_generation_yearly))*hlfdl.qz.input
    delay.test.input<- index.1(rate.1(dfscale$df_yearly.passenger_car))*kcls.qz.input + index.1(rate.1(dfscale$df_yearly.freight_car))*hcls.qz.input+index.1(rate.1(dfscale$df_yearly.mileage))*yylc.qz.input+index.1(rate.1(dfscale$df_yearly.practitioner_number))*cyrysl.qz.input+index.1(rate.1(dfscale$df_yearly.locomotive_number))*jcts.qz.input
    
    biaozhunhua.F.adv.input<- sum(abs(adv.test.input))/sum(abs(coor.test.input)) 
    biaozhunhua.F.delay.input<- sum(abs(delay.test.input))/sum(abs(coor.test.input))
    
    scale.coor.input<- hecheng.equip.index(coor.test.input,biaozhunhua.F.coor)
    scale.adv.input<- hecheng.equip.index(adv.test.input,biaozhunhua.F.adv.input)
    scale.delay.input<- hecheng.equip.index(delay.test.input,biaozhunhua.F.delay.input)
    
    dfscale$coor.input<- scale.coor.input   
    dfscale$adv.input<- scale.adv.input
    dfscale$delay.input<- scale.delay.input
    
    #-----规模----3.1 默认权重计算的画线------------          
    if(input$year_start_scale> input$year_end_scale)  {
      p<-ggplot(dfscale,x=c(dfscale$df_yearly.tm[1],dfscale$df_yearly.tm[scale.len]),aes(x=df_yearly.tm,y=100))
    }
    else{
      dfscalesub<-subset(dfscale,(substr(dfscale$df_yearly.tm,1,4)>=input$year_start_scale) )
      dfscalesub<-subset(dfscalesub,(substr(dfscalesub$df_yearly.tm,1,4)<=input$year_end_scale))
      p<-ggplot(dfscalesub,x=c(dfscalesub$df_yearly.tm[1],dfscalesub$df_yearly.tm[scale.len]),aes(x=df_yearly.tm,y=100))
    }
    
    if(input$scale_coor_Index){
      p<-p+geom_line(aes(x=df_yearly.tm,y=dfscalesub$coor),color="black",size=0.6)
      p<-p+geom_point(aes(x=df_yearly.tm,y=dfscalesub$coor),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if (input$scale_advanced_Index) {
      p<-p+geom_line(aes(x=df_yearly.tm,y=dfscalesub$adv),color="red",size=0.6) 
      p<-p+geom_point(aes(x=df_yearly.tm,y=dfscalesub$adv),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if (input$scale_delay_Index) {
      p<-p+geom_line(aes(x=df_yearly.tm,y=dfscalesub$delay),color="blue",size=0.6)
      p<-p+geom_point(aes(x=df_yearly.tm,y=dfscalesub$delay),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    
    #-----equip----3.2 权重手动输入后的画线------------      
    if(input$scale_qz_coor_input)#输入修改权重后算出来的新先行指数
    { p<-p+geom_line(aes(x=df_yearly.tm,y=dfscalesub$coor.input),color="black",size=1,linetype=1)
    p<-p+geom_point(aes(x=df_yearly.tm,y=dfscalesub$coor.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if(input$scale_qz_adv_input)
    {p<-p+geom_line(aes(x=df_yearly.tm,y=dfscalesub$adv.input),color="red",size=1,linetype=1)
    p<-p+geom_point(aes(x=df_yearly.tm,y=dfscalesub$adv.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if(input$scale_qz_delay_input)
    {p<-p+geom_line(aes(x=df_yearly.tm,y=dfscalesub$delay.input),color="blue",size=1,linetype=1)
    p<-p+geom_point(aes(x=df_yearly.tm,y=dfscalesub$delay.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}    
    
    p+ylab("规模合成指数")+xlab("时间")+geom_line()
  })
  
  
  output$table_scale_index<-DT::renderDataTable(
    {
      hyl.qz.input<- percent.input(input$scale_hyl_qz_input)
      gyzjz.qz.input<- percent.input(input$scale_gyzjz_qz_input)
      hyzzl.qz.input<- percent.input(input$scale_hyzzl_qz_input)
      gc.qz.input<- percent.input(input$scale_gc_qz_input)
      ym.qz.input<- percent.input(input$scale_ym_qz_input)
      yy.qz.input<- percent.input(input$scale_yy_qz_input)
      hlfdl.qz.input<- percent.input(input$scale_hlfdl_qz_input)
      kcls.qz.input<- percent.input(input$scale_kcls_qz_input)
      hcls.qz.input<- percent.input(input$scale_hcls_qz_input)
      yylc.qz.input<- percent.input(input$scale_yylc_qz_input)
      cyrysl.qz.input<- percent.input(input$scale_cyrysl_qz_input)
      jcts.qz.input<- percent.input(input$scale_jcts_qz_input)
      
      coor.test.input<- index.1(rate.1(dfscale$df_yearly.freight_volume_28_yearly))*hyl.qz.input+index.1(rate.1(dfscale$df_yearly.freight_rotation_volume_yearly))*hyzzl.qz.input+index.1(rate.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly))*gyzjz.qz.input
      adv.test.input<- index.1(rate.1(dfscale$df_yearly.iron_output_yearly))*gc.qz.input + index.1(rate.1(dfscale$df_yearly.coal_output_yearly))*ym.qz.input+index.1(rate.1(dfscale$df_yearly.oil_processing_volume_yearly))*yy.qz.input+index.1(rate.1(dfscale$df_yearly.coalfired_power_generation_yearly))*hlfdl.qz.input
      delay.test.input<- index.1(rate.1(dfscale$df_yearly.passenger_car))*kcls.qz.input + index.1(rate.1(dfscale$df_yearly.freight_car))*hcls.qz.input+index.1(rate.1(dfscale$df_yearly.mileage))*yylc.qz.input+index.1(rate.1(dfscale$df_yearly.practitioner_number))*cyrysl.qz.input+index.1(rate.1(dfscale$df_yearly.locomotive_number))*jcts.qz.input
      
      biaozhunhua.F.adv.input<- sum(abs(adv.test.input))/sum(abs(coor.test.input)) 
      biaozhunhua.F.delay.input<- sum(abs(delay.test.input))/sum(abs(coor.test.input))
      
      scale.coor.input<- hecheng.equip.index(coor.test.input,biaozhunhua.F.coor)
      scale.adv.input<- hecheng.equip.index(adv.test.input,biaozhunhua.F.adv.input)
      scale.delay.input<- hecheng.equip.index(delay.test.input,biaozhunhua.F.delay.input)
      
      dfscale$coor.input<- scale.coor.input   
      dfscale$adv.input<- scale.adv.input
      dfscale$delay.input<- scale.delay.input
      
      if(input$scale_qz_coor_input|input$scale_qz_adv_input|input$scale_qz_delay_input){
        DT::datatable(
          { dfscale<- data.frame(dfscale$df_yearly.tm,dfscale$adv.input,dfscale$coor.input,dfscale$delay.input)
          data<-dfscale},
          colnames = c('时间', '先行指数',  '同步指数','滞后指数'),
          rownames = TRUE)}
      
      else{ 
        DT::datatable(
          { dfscale<-data.frame(dfscale$df_yearly.tm,dfscale$adv,dfscale$coor,dfscale$delay)
          data<-dfscale},
          colnames = c('时间', '规模合成先行指数',  '规模合成同步指数','规模合成滞后指数'),
          rownames = TRUE)}
    }  
  )
  
  #-------------------------------------------------------------------------
  #----------------------------扩散指数------------------------------------
  
  #dftrans<-read.csv("Trans_DI.csv",head=T)
  
  #——————————————1.计算过程———————————————
  #-------------1.1 权重计算--------------
  
  #------------(1) 运输先行指标的权重-----
  #gc.trans.percent<- percent.1(dftrans$gc)/(percent.1(dftrans$gc)+percent.1(dftrans$ym)+percent.1(dftrans$yy)+percent.1(dftrans$hlfdl))
  #ym.trans.percent<- percent.1(dftrans$ym)/(percent.1(dftrans$gc)+percent.1(dftrans$ym)+percent.1(dftrans$yy)+percent.1(dftrans$hlfdl))
  #yy.trans.percent<- percent.1(dftrans$yy)/(percent.1(dftrans$gc)+percent.1(dftrans$ym)+percent.1(dftrans$yy)+percent.1(dftrans$hlfdl))
  #hlfdl.trans.percent<- percent.1(dftrans$hlfdl)/(percent.1(dftrans$gc)+percent.1(dftrans$ym)+percent.1(dftrans$yy)+percent.1(dftrans$hlfdl))
  
  #------------(2) 运输同步指标的权重---
  #hyl.trans.percent<- percent.1(dftrans$hyl)/(percent.1(dftrans$hyl)+percent.1(dftrans$gyzjz)+percent.1(dftrans$hyzzl))
  #gyzjz.trans.percent<- percent.1(dftrans$gyzjz)/(percent.1(dftrans$hyl)+percent.1(dftrans$gyzjz)+percent.1(dftrans$hyzzl))
  #hyzzl.trans.percent<- percent.1(dftrans$hyzzl)/(percent.1(dftrans$hyl)+percent.1(dftrans$gyzjz)+percent.1(dftrans$hyzzl))
  
  #------------(3) 运输滞后指标的权重---
  kyl.trans.percent<- percent.1(dftrans$kyl)/(percent.1(dftrans$kyl)+percent.1(dftrans$kyzzl)+percent.1(dftrans$gdzctz))
  kyzzl.trans.percent<- percent.1(dftrans$kyzzl)/(percent.1(dftrans$kyl)+percent.1(dftrans$kyzzl)+percent.1(dftrans$gdzctz))
  gdzctz.trans.percent<- percent.1(dftrans$gdzctz)/(percent.1(dftrans$kyl)+percent.1(dftrans$kyzzl)+percent.1(dftrans$gdzctz))
  yylc.trans.percent<- percent.1(dftrans$yylc)/(percent.1(dftrans$kyl)+percent.1(dftrans$kyzzl)+percent.1(dftrans$gdzctz))
  
  #------------(4) 设备先行指标的权----
  #iron_output_yearly.equip.qz<- percent.1(dfequip$df_yearly.iron_output_yearly)/(percent.1(dfequip$df_yearly.iron_output_yearly)+percent.1(dfequip$df_yearly.coal_output_yearly)+percent.1(dfequip$df_yearly.oil_processing_volume_yearly)+percent.1(dfequip$df_yearly.coalfired_power_generation_yearly))
  #coal_output_yearly.equip.qz<- percent.1(dfequip$df_yearly.coal_output_yearly)/(percent.1(dfequip$df_yearly.iron_output_yearly)+percent.1(dfequip$df_yearly.coal_output_yearly)+percent.1(dfequip$df_yearly.oil_processing_volume_yearly)+percent.1(dfequip$df_yearly.coalfired_power_generation_yearly))
  #oil_processing_volume_yearly.equip.qz<- percent.1(dfequip$df_yearly.oil_processing_volume_yearly)/(percent.1(dfequip$df_yearly.iron_output_yearly)+percent.1(dfequip$df_yearly.coal_output_yearly)+percent.1(dfequip$df_yearly.oil_processing_volume_yearly)+percent.1(dfequip$df_yearly.coalfired_power_generation_yearly))
  #coalfired_power_generation_yearly.equip.qz<- percent.1(dfequip$df_yearly.coalfired_power_generation_yearly)/(percent.1(dfequip$df_yearly.iron_output_yearly)+percent.1(dfequip$df_yearly.coal_output_yearly)+percent.1(dfequip$df_yearly.oil_processing_volume_yearly)+percent.1(dfequip$df_yearly.coalfired_power_generation_yearly))
  
  #------------(5) 设备同步指标的权重------------------------------------
  #locomotive_mileage_sum.equip.qz<- percent.1(dfequip$df_yearly.locomotive_mileage_sum)/(percent.1(dfequip$df_yearly.locomotive_mileage_sum)+percent.1(dfequip$df_yearly.dailycar_run))
  #dailycar_run.equip.qz<- percent.1(dfequip$df_yearly.dailycar_run)/(percent.1(dfequip$df_yearly.locomotive_mileage_sum)+percent.1(dfequip$df_yearly.dailycar_run))
  
  #------------(6) 设备滞后指标的权重------------------------------------ 
  #dailycar_now.equip.qz<- percent.1(dfequip$df_yearly.dailycar_now)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  #locomotive_mileage_pcar.equip.qz<- percent.1(dfequip$df_yearly.locomotive_mileage_pcar)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  #locomotive_mileage_fcar.equip.qz<- percent.1(dfequip$df_yearly.locomotive_mileage_fcar)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  #passenger_car.equip.qz<- percent.1(dfequip$df_yearly.passenger_car)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  #freight_car.equip.qz<- percent.1(dfequip$df_yearly.freight_car)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  #locomotive_number.equip.qz<- percent.1(dfequip$df_yearly.locomotive_number)/(percent.1(dfequip$df_yearly.dailycar_now)+percent.1(dfequip$df_yearly.locomotive_mileage_pcar)+percent.1(dfequip$df_yearly.locomotive_mileage_fcar)+percent.1(dfequip$df_yearly.passenger_car)+percent.1(dfequip$df_yearly.freight_car)+percent.1(dfequip$df_yearly.locomotive_number))
  
  #------------(7) 规模先行指标的权重------------------------------------ 
  gc.scale.percent<- iron_output_yearly.equip.qz #percent.1(dfscale$df_yearly.iron_output_yearly)/(percent.1(dfscale$df_yearly.iron_output_yearly)+percent.1(dfscale$df_yearly.coal_output_yearly)+percent.1(dfscale$df_yearly.oil_processing_volume_yearly)+percent.1(dfscale$df_yearly.coalfired_power_generation_yearly))
  ym.scale.percent<- coal_output_yearly.equip.qz #percent.1(dfscale$df_yearly.coal_output_yearly)/(percent.1(dfscale$df_yearly.iron_output_yearly)+percent.1(dfscale$df_yearly.coal_output_yearly)+percent.1(dfscale$df_yearly.oil_processing_volume_yearly)+percent.1(dfscale$df_yearly.coalfired_power_generation_yearly))
  yy.scale.percent<- oil_processing_volume_yearly.equip.qz #percent.1(dfscale$df_yearly.oil_processing_volume_yearly)/(percent.1(dfscale$df_yearly.iron_output_yearly)+percent.1(dfscale$df_yearly.coal_output_yearly)+percent.1(dfscale$df_yearly.oil_processing_volume_yearly)+percent.1(dfscale$df_yearly.coalfired_power_generation_yearly))
  hlfdl.scale.percent<- coalfired_power_generation_yearly.equip.qz #percent.1(dfscale$df_yearly.coalfired_power_generation_yearly)/(percent.1(dfscale$df_yearly.iron_output_yearly)+percent.1(dfscale$df_yearly.coal_output_yearly)+percent.1(dfscale$df_yearly.oil_processing_volume_yearly)+percent.1(dfscale$df_yearly.coalfired_power_generation_yearly))
  
  #------------(8) 规模同步指标的权重------------------------------------
  #hyl.scale.qz<- percent.1(dfscale$df_yearly.freight_volume_28_yearly)/(percent.1(dfscale$df_yearly.freight_volume_28_yearly)+percent.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)+percent.1(dfscale$df_yearly.freight_rotation_volume_yearly))
  #gyzjz.scale.qz<- percent.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)/(percent.1(dfscale$df_yearly.freight_volume_28_yearly)+percent.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)+percent.1(dfscale$df_yearly.freight_rotation_volume_yearly))
  #hyzzl.scale.qz<- percent.1(dfscale$df_yearly.freight_rotation_volume_yearly)/(percent.1(dfscale$df_yearly.freight_volume_28_yearly)+percent.1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)+percent.1(dfscale$df_yearly.freight_rotation_volume_yearly))
  
  #------------(9) 规模滞后指标的权重------------------------------------
  #kcls.scale.qz<- percent.1(dfscale$df_yearly.passenger_car)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  #hcls.scale.qz<- percent.1(dfscale$df_yearly.freight_car)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  #yylc.scale.qz<- percent.1(dfscale$df_yearly.mileage)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  #cyrysl.scale.qz<- percent.1(dfscale$df_yearly.practitioner_number)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  #jcts.scale.qz<- percent.1(dfscale$df_yearly.locomotive_number)/(percent.1(dfscale$df_yearly.passenger_car)+percent.1(dfscale$df_yearly.freight_car)+percent.1(dfscale$df_yearly.mileage)+percent.1(dfscale$df_yearly.practitioner_number)+percent.1(dfscale$df_yearly.locomotive_number))
  
  #-------------1.2 扩散指数计算---------------------------------------------------------------------
  #自定义函数，输入为某量的时间序列数据，输出为1,0.5,0三个值
  function1<-function(v){
    vud<-(v[-1]-v[-length(v)])/v[-length(v)]#变化率
    t1<-mean(vud)+qt(1-0.3,length(vud))*sd(vud)/sqrt(length(vud))#求置信上线
    t2<-mean(vud)+qt(1-0.45,length(vud))*sd(vud)/sqrt(length(vud))
    t3<-mean(vud)+qt(0.6,length(vud))*sd(vud)/sqrt(length(vud))
    return(ifelse(vud>t3,1,ifelse(vud>t2,0.5,0)))
  }
  
  #权重手动输入部分计算的函数们
  
  #--------------(1) 运输扩散指数计算---------------------------
  #计算运输扩散指数
  tm1<-dftrans$tm[2:length(dftrans$tm)]
  c1<-function1(dftrans$gc)
  c2<-function1(dftrans$ym)
  c3<-function1(dftrans$yy)
  c4<-function1(dftrans$hlfdl)
  c5<-function1(dftrans$hyzzl)
  c6<-function1(dftrans$hyl)
  c7<-function1(dftrans$gyzjz)
  c8<-function1(dftrans$kyl)
  c9<-function1(dftrans$kyzzl)
  c10<-function1(dftrans$gdzctz)
  c11<-function1(dftrans$yylc)
  
  DIx_trans<-gc.trans.percent*c1+ym.trans.percent*c2+yy.trans.percent*c3+hlfdl.trans.percent*c4#运输扩散先行指数
  DIt_trans<-hyzzl.trans.percent*c5+hyl.trans.percent*c6+gyzjz.trans.percent*c7#运输扩散同步指数
  DIz_trans<-kyl.trans.percent*c8+kyzzl.trans.percent*c9+gdzctz.trans.percent*c10+yylc.trans.percent*c11#运输扩散滞后指数
  
  DI_trans<-data.frame(tm1,DIx_trans,DIt_trans,DIz_trans)#存储所有指数计算结果的数据框
  #DI_trans$tm1<-as.Date.POSIXct(DI_trans$tm1,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
  #write.csv(DI_trans,file="DI_Trans.csv",row.names = FALSE)
  
  #-------------(2) 设备扩散指数计算-----------------------------
  tm2<-dfequip$df_yearly.tm[2:equip.len]#这里是从第二行开始计算和显示的，所以可以把第一行的数据设置为0
  c12<-function1(dfequip$df_yearly.locomotive_mileage_sum)
  c13<-function1(dfequip$df_yearly.dailycar_run)
  c14<-function1(dfequip$df_yearly.dailycar_now)
  c15<-function1(dfequip$df_yearly.locomotive_mileage_pcar)
  c16<-function1(dfequip$df_yearly.locomotive_mileage_fcar)
  c17<-function1(dfequip$df_yearly.passenger_car)
  c18<-function1(dfequip$df_yearly.freight_car)
  c19<-function1(dfequip$df_yearly.locomotive_number)
  c22<-function1(dfequip$df_yearly.iron_output_yearly)
  c23<-function1(dfequip$df_yearly.coal_output_yearly)
  c24<-function1(dfequip$df_yearly.oil_processing_volume_yearly)
  c25<-function1(dfequip$df_yearly.coalfired_power_generation_yearly)
  DIx_equip<-iron_output_yearly.equip.qz*c22+coal_output_yearly.equip.qz*c23+oil_processing_volume_yearly.equip.qz*c24+coalfired_power_generation_yearly.equip.qz*c25 #设备扩散先行指数
  DIt_equip<-locomotive_mileage_sum.equip.qz*c12+dailycar_run.equip.qz*c13#设备扩散同步指数
  DIz_equip<-dailycar_now.equip.qz*c14+locomotive_mileage_pcar.equip.qz*c15+locomotive_mileage_fcar.equip.qz*c16+passenger_car.equip.qz*c17+freight_car.equip.qz*c18+locomotive_number.equip.qz*c19#设备扩散滞后指数
  
  DI_equip<-data.frame(tm2,DIx_equip,DIt_equip,DIz_equip)#存储所有指数计算结果的数据框，不需要再写c成csv表格
  
  #-------------(3) 规模扩散指数计算-----------------------------
  tm3<-dfscale$df_yearly.tm[2:scale.len]
  c20<-function1(dfscale$df_yearly.practitioner_number)
  c21<-function1(dfscale$df_yearly.mileage)
  #c22<-function1(dfscale$df_yearly.iron_output_yearly) 
  #c23<-function1(dfscale$df_yearly.coal_output_yearly)
  #c24<-function1(dfscale$df_yearly.oil_processing_volume_yearly)
  #c25<-function1(dfscale$df_yearly.coalfired_power_generation_yearly)
  c30<-function1(dfscale$df_yearly.freight_rotation_volume_yearly)
  c31<-function1(dfscale$df_yearly.freight_volume_28_yearly)
  c32<-function1(dfscale$df_yearly.Industrial_Added_Value_Rate_yearly)
  DIx_scale<-gc.scale.percent*c22+ym.scale.percent*c23+yy.scale.percent*c24+hlfdl.scale.percent*c25 #规模扩散先行指数
  DIt_scale<-hyzzl.scale.qz*c30+hyl.scale.qz*c31+gyzjz.scale.qz*c32#规模扩散同步指数
  DIz_scale<-kcls.scale.qz*c17+hcls.scale.qz*c18+yylc.scale.qz*c21+cyrysl.scale.qz*c20+jcts.scale.qz*c19#规模扩散滞后指数
  
  DI_scale<-data.frame(tm3,DIx_scale,DIt_scale,DIz_scale)#存储所有指数计算结果的数据框
  
  #——————————————2.画图过程——————————————————————————————
  #------------2.1运输扩散指数画图------------------------
  #----------(1)运输扩散指数计算--权重手动输入------------
  output$trans_DI_index<- renderPlot( {
    
    hyl.input<- percent.input(input$trans_hyl_percent_input)
    gyzjz.input<- percent.input(input$trans_gyzjz_percent_input)
    hyzzl.input<- percent.input(input$trans_hyzzl_percent_input)
    gc.input<- percent.input(input$trans_gc_percent_input)
    ym.input<- percent.input(input$trans_ym_percent_input)
    yy.input<- percent.input(input$trans_yy_percent_input)
    hlfdl.input<- percent.input(input$trans_hlfdl_percent_input)
    kyl.input<- percent.input(input$trans_kyl_percent_input)
    gdzctz.input<- percent.input(input$trans_gdzctz_percent_input)
    kyzzl.input<- percent.input(input$trans_kyzzl_percent_input)
    
    yylc.input<- percent.input(input$trans_yylc_percent_input)
    jczxzlc.input<- percent.input(input$trans_jczxzlc_percent_input)
    rjyyc.input<- percent.input(input$trans_rjyyc_percent_input)
    rjxzc.input<- percent.input(input$equip_rjxzc_percent_input)
    
    DIx_trans_input<- gc.input*c1 + ym.input*c2+yy.input*c3+hlfdl.input*c4
    DIt_trans_input<- hyzzl.input*c5+hyl.input*c6+gyzjz.input*c7
    DIz_trans_input<- kyl.input*c8 + kyzzl.input*c9+gdzctz.input*c10+yylc.input*c11
    
    DI_trans_input<-data.frame(tm1,DIx_trans_input, DIt_trans_input,DIz_trans_input)#存储所有指数计算结果的数据框
    DI_trans_input$tm1<-as.Date.POSIXct(DI_trans_input$tm1,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    #write.csv(DI_trans_input,file="DI_Trans_Input.csv",row.names = FALSE)    
    
    #----------(2)运输扩散指数--默认权重计算的画图--------------------------------- 
    DI_trans.len<-length(DI_trans_input$tm1)
    
    if(input$year_start_trans_ID> input$year_end_trans_ID)  {
      p<-ggplot(DI_trans,x=c(DI_trans$tm1[1],DI_trans$tm1[DI_trans.len]),aes(x=tm1,y=0.5))} else
      {
        dftranssub<-subset(DI_trans,(substr(DI_trans$tm1,1,4)>=input$year_start_trans_ID) )
        dftranssub<-subset(dftranssub,(substr(dftranssub$tm1,1,4)<=input$year_end_trans_ID))
        p<-ggplot(dftranssub,x=c(dftranssub$tm1[1],dftranssub$tm1[DI_trans.len]),aes(x=tm1,y=0.5))}
    
    if(input$trans_DIt_Index){
      p<-p+geom_line(aes(x=tm1,y=dftranssub$DIt_trans),color="black",size=0.6)}
    if (input$trans_DIx_Index) {
      p<-p+geom_line(aes(x=tm1,y=dftranssub$DIx_trans),color="red",size=0.6) }
    if (input$trans_DIz_Index) {
      p<-p+geom_line(aes(x=tm1,y=dftranssub$DIz_trans),color="blue",size=0.6)}
    
    #----------(3)运输扩散指数--权重手动输入计算的画图---------------------------------
    dftransinputsub<-subset(DI_trans_input,(substr(DI_trans_input$tm1,1,4)>=input$year_start_trans_ID))
    dftransinputsub<-subset(dftransinputsub,(substr(dftransinputsub$tm1,1,4)<=input$year_end_trans_ID))
    if(input$trans_percent_coor_input)#输入修改权重后算出来的新先行指数
    { 
      p<-p+geom_line(aes(x=tm1,y=dftransinputsub$DIt_trans_input),color="black",size=1,linetype=1)}
    if(input$trans_percent_adv_input)
    {p<-p+geom_line(aes(x=tm1,y=dftransinputsub$DIx_trans_input),color="red",size=1,linetype=1)}
    if(input$trans_percent_delay_input)
    {p<-p+geom_line(aes(x=tm1,y=dftransinputsub$DIz_trans_input),color="blue",size=1,linetype=1)} 
    
    p+ylab("运输扩散指数")+xlab("时间")+geom_line()
  })
  
  
  #------------2.2设备扩散指数画图-----------------------------------------
  #----------(1)设备扩散指数计算--权重手动输入---------------------
  output$equip_DI_index<- renderPlot( {
    
    rjyyc.input<- percent.input(input$equip_rjyyc_percent_input)
    jczxzlc.input<- percent.input(input$equip_jczxzlc_percent_input)
    gc.input<- percent.input(input$equip_gc_percent_input)
    ym.input<- percent.input(input$equip_ym_percent_input)
    yy.input<- percent.input(input$equip_yy_percent_input)
    hlfdl.input<- percent.input(input$equip_hlfdl_percent_input)
    rjxzc.input<- percent.input(input$equip_rjxzc_percent_input)
    kyjclc.input<- percent.input(input$equip_kyjclc_percent_input)
    hyjclc.input<- percent.input(input$equip_hyjclc_percent_input)
    kcls.input<- percent.input(input$equip_kcls_percent_input)
    hcls.input<- percent.input(input$equip_hcls_percent_input)
    jcts.input<- percent.input(input$equip_jcts_percent_input)
    
    DIx_equip_input<- gc.input*c22 + ym.input*c23+yy.input*c24+hlfdl.input*c25
    DIt_equip_input<- jczxzlc.input*c12+rjyyc.input*c13
    DIz_equip_input<- rjxzc.input*c14+kyjclc.input*c15+hyjclc.input*c16+kcls.input*c17+hcls.input*c18+jcts.input*c19
    
    DI_equip_input<-data.frame(tm2,DIx_equip_input, DIt_equip_input,DIz_equip_input)#存储所有指数计算结果的数据框
    #DI_equip_input$tm2<-as.Date.POSIXct(DI_equip_input$tm2,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    #write.csv(DI_equip_input,file="DI_Equip_Input.csv",row.names = FALSE) 
    
    #----------(2)设备扩散指数--默认权重计算的画图---------------------
    DI_equip.len<-length(DI_equip_input$tm2)
    
    if(input$year_start_equip_ID > input$year_end_equip_ID)  {
      p<-ggplot(DI_equip,x=c(DI_equip$tm2[1],DI_equip$tm2[DI_equip.len]),aes(x=tm2,y=0.5))}
    else{
      dfequipsub<-subset(DI_equip,(substr(DI_equip$tm2,1,4)>=input$year_start_equip_ID) )
      dfequipsub<-subset(dfequipsub,(substr(dfequipsub$tm2,1,4)<=input$year_end_equip_ID))
      p<-ggplot(dfequipsub,x=c(dfequipsub$tm2[1],dfequipsub$tm2[DI_equip.len]),aes(x=tm2,y=0.5))}
    
    if(input$equip_DIt_Index){
      p<-p+geom_line(aes(x=tm2,y=dfequipsub$DIt_equip),color="black",size=0.6)+geom_point(aes(x=tm2,y=dfequipsub$DIt_equip),size=3,shape=18,colour="black",fill="black",position=position_dodge(width=0.2))}
    if (input$equip_DIx_Index) {
      p<-p+geom_line(aes(x=tm2,y=dfequipsub$DIx_equip),color="red",size=0.6)+geom_point(aes(x=tm2,y=dfequipsub$DIx_equip),size=3,shape=18,colour="red",fill="red",position=position_dodge(width=0.2))}
    if (input$equip_DIz_Index) {
      p<-p+geom_line(aes(x=tm2,y=dfequipsub$DIz_equip),color="blue",size=0.6)+geom_point(aes(x=tm2,y=dfequipsub$DIz_equip),size=3,shape=18,colour="blue",fill="blue",position=position_dodge(width=0.2))}
    
    #----------(3)设备扩散指数--权重手动输入计算的画图---------------------
    dfequipinputsub<-subset(DI_equip_input,(substr(DI_equip_input$tm2,1,4)>=input$year_start_equip_ID))
    dfequipinputsub<-subset(dfequipinputsub,(substr(dfequipinputsub$tm2,1,4)<=input$year_end_equip_ID))
    if(input$equip_percent_coor_input)#输入修改权重后算出来的新先行指数
    { p<-p+geom_line(aes(x=tm2,y=dfequipinputsub$DIt_equip_input),color="black",size=1,linetype=1)+geom_point(aes(x=tm2,y=dfequipinputsub$DIt_equip_input),size=3,shape=18,colour="black",fill="black",position=position_dodge(width=0.2))}
    if(input$equip_percent_adv_input)
    {p<-p+geom_line(aes(x=tm2,y=dfequipinputsub$DIx_equip_input),color="red",size=1,linetype=1)+geom_point(aes(x=tm2,y=dfequipinputsub$DIx_equip_input),size=3,shape=18,colour="red",fill="red",position=position_dodge(width=0.2))}
    if(input$equip_percent_delay_input)
    {p<-p+geom_line(aes(x=tm2,y=dfequipinputsub$DIz_equip_input),color="blue",size=1,linetype=1)+geom_point(aes(x=tm2,y=dfequipinputsub$DIz_equip_input),size=3,shape=18,colour="blue",fill="blue",position=position_dodge(width=0.2))} 
    
    p+ylab("设备扩散指数")+xlab("时间")+geom_line()
  })  
  
  
  #------------2.3规模扩散指数画图-----------------------------------------
  #----------(1)规模扩散指数计算--权重手动输入---------------------
  output$scale_DI_index<- renderPlot( {
    
    hyl.input<- percent.input(input$scale_hyl_percent_input)
    gyzjz.input<- percent.input(input$scale_gyzjz_percent_input)
    hyzzl.input<- percent.input(input$scale_hyzzl_percent_input)
    gc.input<- percent.input(input$scale_gc_percent_input)
    ym.input<- percent.input(input$scale_ym_percent_input)
    yy.input<- percent.input(input$scale_yy_percent_input)
    hlfdl.input<- percent.input(input$scale_hlfdl_percent_input)
    kcls.input<- percent.input(input$scale_kcls_percent_input)
    hcls.input<- percent.input(input$scale_hcls_percent_input)
    yylc.input<- percent.input(input$scale_yylc_percent_input)
    cyrysl.input<- percent.input(input$scale_cyrysl_percent_input)
    jcts.input<- percent.input(input$scale_jcts_percent_input)
    
    DIx_scale_input<- gc.input*c22 + ym.input*c23+yy.input*c24+hlfdl.input*c25
    DIt_scale_input<- hyzzl.input*c30+hyl.input*c31+gyzjz.input*c32
    DIz_scale_input<- kcls.input*c17+hcls.input*c18+yylc.input*c21+cyrysl.input*c20+jcts.input*c19
    
    DI_scale_input<-data.frame(tm3,DIx_scale_input, DIt_scale_input,DIz_scale_input)#存储所有指数计算结果的数据框
    #DI_scale_input$tm3<-as.Date.POSIXct(DI_scale_input$tm3,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    #write.csv(DI_scale_input,file="DI_Scale_Input.csv",row.names = FALSE)    
    
    #----------(2)规模扩散指数--默认权重计算的画图---------------------
    DI_scale.len<-length(DI_scale_input$tm3)
    
    if(input$year_start_scale_ID> input$year_end_scale_ID)  {
      p<-ggplot(DI_scale,x=c(DI_scale$tm3[1],DI_scale$tm3[DI_scale.len]),aes(x=tm3,y=0.5))}
    else{
      dfscalesub<-subset(DI_scale,(substr(DI_scale$tm3,1,4)>=input$year_start_scale_ID) )
      dfscalesub<-subset(dfscalesub,(substr(dfscalesub$tm3,1,4)<=input$year_end_scale_ID))
      p<-ggplot(dfscalesub,x=c(dfscalesub$tm3[1],dfscalesub$tm3[DI_scale.len]),aes(x=tm3,y=0.5))}
    
    if(input$scale_DIt_Index){
      p<-p+geom_line(aes(x=tm3,y=dfscalesub$DIt_scale),color="black",size=0.6)+geom_point(aes(x=tm3,y=dfscalesub$DIt_scale),size=3,shape=18,colour="black",fill="black",position=position_dodge(width=0.2))}
    if (input$scale_DIx_Index) {
      p<-p+geom_line(aes(x=tm3,y=dfscalesub$DIx_scale),color="red",size=0.6)+geom_point(aes(x=tm3,y=dfscalesub$DIx_scale),size=3,shape=18,colour="red",fill="red",position=position_dodge(width=0.2))}
    if (input$scale_DIz_Index) {
      p<-p+geom_line(aes(x=tm3,y=dfscalesub$DIz_scale),color="blue",size=0.6)+geom_point(aes(x=tm3,y=dfscalesub$DIz_scale),size=3,shape=18,colour="blue",fill="blue",position=position_dodge(width=0.2))}
    
    #----------(3)规模扩散指数--权重手动输入计算的画图---------------------
    dfscaleinputsub<-subset(DI_scale_input,(substr(DI_scale_input$tm3,1,4)>=input$year_start_scale_ID))
    dfscaleinputsub<-subset(dfscaleinputsub,(substr(dfscaleinputsub$tm3,1,4)<=input$year_end_scale_ID))
    if(input$scale_percent_coor_input)#输入修改权重后算出来的新先行指数
    { p<-p+geom_line(aes(x=tm3,y=dfscaleinputsub$DIt_scale_input),color="black",size=1,linetype=1)+geom_point(aes(x=tm3,y=dfscaleinputsub$DIt_scale_input),size=3,shape=18,colour="black",fill="black",position=position_dodge(width=0.2))}
    if(input$scale_percent_adv_input)
    {p<-p+geom_line(aes(x=tm3,y=dfscaleinputsub$DIx_scale_input),color="red",size=1,linetype=1)+geom_point(aes(x=tm3,y=dfscaleinputsub$DIx_scale_input),size=3,shape=18,colour="red",fill="red",position=position_dodge(width=0.2))}
    if(input$scale_percent_delay_input)
    {p<-p+geom_line(aes(x=tm3,y=dfscaleinputsub$DIz_scale_input),color="blue",size=1,linetype=1)+geom_point(aes(x=tm3,y=dfscaleinputsub$DIz_scale_input),size=3,shape=18,colour="blue",fill="blue",position=position_dodge(width=0.2))} 
    
    p+ylab("规模扩散指数")+xlab("时间")+geom_line()
  }) 
  
  #——————————————3.数据表的显示—————————————————————————————
  
  #------------3.1运输扩散指数数据表显示-----------------
  output$table_trans_DI_index<-DT::renderDataTable({
    hyl.input<- percent.input(input$trans_hyl_percent_input)
    gyzjz.input<- percent.input(input$trans_gyzjz_percent_input)
    hyzzl.input<- percent.input(input$trans_hyzzl_percent_input)
    gc.input<- percent.input(input$trans_gc_percent_input)
    ym.input<- percent.input(input$trans_ym_percent_input)
    yy.input<- percent.input(input$trans_yy_percent_input)
    hlfdl.input<- percent.input(input$trans_hlfdl_percent_input)
    kyl.input<- percent.input(input$trans_kyl_percent_input)
    gdzctz.input<- percent.input(input$trans_gdzctz_percent_input)
    kyzzl.input<- percent.input(input$trans_kyzzl_percent_input)
    
    yylc.input<- percent.input(input$trans_yylc_percent_input)
    jczxzlc.input<- percent.input(input$trans_jczxzlc_percent_input)
    rjyyc.input<- percent.input(input$trans_rjyyc_percent_input)
    rjxzc.input<- percent.input(input$equip_rjxzc_percent_input)
    
    DIx_trans_input<- gc.input*c1 + ym.input*c2+yy.input*c3+hlfdl.input*c4
    DIt_trans_input<- hyzzl.input*c5+hyl.input*c6+gyzjz.input*c7
    DIz_trans_input<- kyl.input*c8 + kyzzl.input*c9+gdzctz.input*c10+yylc.input*c11
    
    DI_trans_input<-data.frame(tm1,DIx_trans_input, DIt_trans_input,DIz_trans_input)#存储所有指数计算结果的数据框
    #DI_trans_input$tm1<-as.Date.POSIXct(DI_trans_input$tm1,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    #write.csv(DI_trans_input,file="DI_Trans_Input.csv",row.names = FALSE)
    
    #----运输----输入修改权重后算出来的三个指数------------------   
    if(input$trans_percent_coor_input|input$trans_percent_adv_input|input$trans_percent_delay_input){
      DT::datatable(
        { data<-DI_trans_input},
        colnames = c('时间', '运输扩散先行指数','运输扩散同步指数','运输扩散滞后指数'),
        rownames = TRUE)}
    #----运输-----默认权重计算下的三个指数------------------  
    else{ 
      DT::datatable(
        {data<-DI_trans},
        colnames = c('时间', '运输扩散先行指数',  '运输扩散同步指数','运输扩散滞后指数'),
        rownames = TRUE)
    }
  })      
  
  #------------3.2设备扩散指数数据表显示----------
  
  output$table_equip_DI_index<-DT::renderDataTable({
    rjyyc.input<- percent.input(input$equip_rjyyc_percent_input)
    jczxzlc.input<- percent.input(input$equip_jczxzlc_percent_input)
    gc.input<- percent.input(input$equip_gc_percent_input)
    ym.input<- percent.input(input$equip_ym_percent_input)
    yy.input<- percent.input(input$equip_yy_percent_input)
    hlfdl.input<- percent.input(input$equip_hlfdl_percent_input)
    rjxzc.input<- percent.input(input$equip_rjxzc_percent_input)
    kyjclc.input<- percent.input(input$equip_kyjclc_percent_input)
    hyjclc.input<- percent.input(input$equip_hyjclc_percent_input)
    kcls.input<- percent.input(input$equip_kcls_percent_input)
    hcls.input<- percent.input(input$equip_hcls_percent_input)
    jcts.input<- percent.input(input$equip_jcts_percent_input)
    
    DIx_equip_input<- gc.input*c22 + ym.input*c23+yy.input*c24+hlfdl.input*c25
    DIt_equip_input<- jczxzlc.input*c12+rjyyc.input*c13
    DIz_equip_input<- rjxzc.input*c14+kyjclc.input*c15+hyjclc.input*c16+kcls.input*c17+hcls.input*c18+jcts.input*c19
    
    DI_equip_input<-data.frame(tm2,DIx_equip_input, DIt_equip_input,DIz_equip_input)#存储所有指数计算结果的数据框
    #DI_equip_input$tm2<-as.Date.POSIXct(DI_equip_input$tm2,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    #write.csv(DI_equip_input,file="DI_Equip_Input.csv",row.names = FALSE)
    
    #----设备----输入修改权重后算出来的三个指数------------------   
    if(input$equip_percent_coor_input|input$equip_percent_adv_input|input$equip_percent_delay_input){
      DT::datatable(
        {data<-DI_equip_input},
        colnames = c('时间', '设备扩散先行指数','设备扩散同步指数','设备扩散滞后指数'),
        rownames = TRUE)}
    #----设备----默认权重计算下的三个指数------------------  
    else{DT::datatable(
      {data<-DI_equip},
      colnames = c('时间', '设备扩散先行指数','设备扩散同步指数','设备扩散滞后指数'),
      rownames = TRUE)}
  })
  
  #------------3.3规模扩散指数数据表显示-------------------
  output$table_scale_DI_index<-DT::renderDataTable({
    hyl.input<- percent.input(input$scale_hyl_percent_input)
    gyzjz.input<- percent.input(input$scale_gyzjz_percent_input)
    hyzzl.input<- percent.input(input$scale_hyzzl_percent_input)
    gc.input<- percent.input(input$scale_gc_percent_input)
    ym.input<- percent.input(input$scale_ym_percent_input)
    yy.input<- percent.input(input$scale_yy_percent_input)
    hlfdl.input<- percent.input(input$scale_hlfdl_percent_input)
    kcls.input<- percent.input(input$scale_kcls_percent_input)
    hcls.input<- percent.input(input$scale_hcls_percent_input)
    yylc.input<- percent.input(input$scale_yylc_percent_input)
    cyrysl.input<- percent.input(input$scale_cyrysl_percent_input)
    jcts.input<- percent.input(input$scale_jcts_percent_input)
    
    DIx_scale_input<- gc.input*c22 + ym.input*c23+yy.input*c24+hlfdl.input*c25
    DIt_scale_input<- hyzzl.input*c30+hyl.input*c31+gyzjz.input*c32
    DIz_scale_input<- kcls.input*c17+hcls.input*c18+yylc.input*c21+cyrysl.input*c20+jcts.input*c19
    
    DI_scale_input<-data.frame(tm3,DIx_scale_input, DIt_scale_input,DIz_scale_input)#存储所有指数计算结果的数据框
    #DI_scale_input$tm3<-as.Date.POSIXct(DI_scale_input$tm3,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    #write.csv(DI_scale_input,file="DI_Scale_Input.csv",row.names = FALSE)  
    
    #----规模---- 输入修改权重后算出来的三个指数------------------  
    if(input$scale_percent_coor_input|input$scale_percent_adv_input|input$scale_percent_delay_input){
      DT::datatable(
        {data<-DI_scale_input},
        colnames = c('时间', '规模扩散先行指数','规模扩散同步指数','规模扩散滞后指数'),
        rownames = TRUE)}
    #----规模----默认权重计算下的三个指数------------------  
    else{DT::datatable(
      {data<-DI_scale},
      colnames = c('时间', '规模扩散先行指数','规模扩散同步指数','规模扩散滞后指数'),
      rownames = TRUE)}
  })
  
  
  
  
  #——————————————————————————————————————————————————————————————————————————————————————————————
  #——————————————————————————————————————————————————————————————————————————————————————————————
  #黑货指数白货指数
  #——————————————————————————————————————————————————————————————————————————————————————————————
  #——————————————————————————————————————————————————————————————————————————————————————————————
  source("rate1.R")
  source("rate2.R")
  source("rate3.R")
  source("index.R")
  
  #-----黑货指数计算--------------------------------------------------------------------------------------------
  # liaozili<-read.csv("index-black.csv",head=T)# 读取黑货原始数据到变量liaozili中
  # liaozili$tm<-as.Date.POSIXct(liaozili$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  # liaozili_len<-length(liaozili$tm)
  wb<-read.xlsx("rawdata_monthly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
  wb_len<-length(wb$coal)
  metal<-wb$metal[85:wb_len]
  mine<-wb$mine[85:wb_len]
  iron<-wb$iron[85:wb_len]
  oil<-wb$oil[85:wb_len]
  coal<-wb$coal[85:wb_len]
  machinery<-wb$machinery[85:wb_len]
  electronic<-wb$electronic[85:wb_len]
  agricultural<-wb$agricultural[85:wb_len]
  food_tobacco<-wb$food_tobacco[85:wb_len]
  education<-wb$education[85:wb_len]
  ltl<-wb$ltl[85:wb_len]
  container<-wb$container[85:wb_len]
  wb_index<-data.frame(metal,mine,iron,oil,coal,machinery,electronic,agricultural,food_tobacco,education,ltl,container)#建立黑货和白货的总表
  wb_index$tm<-as.Date.POSIXct(wb$tm[85:wb_len],"%Y-%m-%d",tz=Sys.timezone(location = TRUE))    #转化为日期型数据
  balck_index_len<-length(wb_index$tm)
  #-----增长率--------------
  
  coal1<-rate1(wb_index$coal)
  oil1<- rate1(wb_index$oil)
  metal1<-rate1(wb_index$metal)
  iron1<-rate1(wb_index$iron)
  mine1<-rate1(wb_index$mine)
  
  #-----对称变化率--------------
  
  coal2<- rate2(coal1,0)
  oil2<-rate2(oil1,0)
  metal2<-rate2(metal1,0)
  iron2<-rate2(iron1,0)
  mine2<-rate2(mine1,0)
  
  
  #-----标准化对称变换率--------
  
  coal3<-rate3(coal1,coal2)
  oil3<-rate3(oil1,oil2)
  metal3<-rate3(metal1,metal2)
  iron3<-rate3(iron1,iron2)
  mine3<-rate3(mine1,mine2)
  
  
  # -----黑货指数：图像显示--------
  
  output$heihuo_index<- renderPlot( {
    lx1<-input$weightcoal_input/100
    lx2<-input$weightoil_input/100
    lx3<-input$weightmetal_input/100
    lx4<-input$weightiron_input/100
    lx5<-input$weightmine_input/100
    
    averagerate<-coal3*lx1 +oil3*lx2 + metal3*lx3 + iron3*lx4+ mine3*lx5
    wb_index$heihuo_index<-index(averagerate,0)
    
    if(input$balck_index_year_start> input$balck_index_year_end)  {
      p<-ggplot(wb_index,x=c(wb_index$tm[1],wb_index$tm[balck_index_len]),aes(x=tm,y=0))
    }
    else{
      dfsub<-subset(wb_index,(substr(wb_index$tm,1,4)>=input$balck_index_year_start) )
      dfsub<-subset(dfsub,(substr(dfsub$tm,1,4)<=input$balck_index_year_end))
      p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[balck_index_len]),aes(x=tm,y=80))
    }
    
    p<-p+geom_line(aes(x=tm,y=dfsub$heihuo_index),color="blue",size=1)
    p+ylab("黑货指数")+xlab("时间")+geom_line()+ylim(60,105)
  })
  
  # -----黑货指数：数据显示--------
  output$heihuotable<-DT::renderDataTable({

    
    lx1<-input$weightcoal_input/100
    lx2<-input$weightoil_input/100
    lx3<-input$weightmetal_input/100
    lx4<-input$weightiron_input/100
    lx5<-input$weightmine_input/100
    
    averagerate<-coal3*lx1 +oil3*lx2 + metal3*lx3 + iron3*lx4+ mine3*lx5
    wb_index$heihuo_index<-index(averagerate,0)
    heihuo_index<-round(wb_index$heihuo_index,3)
    total<-metal+mine+iron+oil+coal
    balck_index<-data.frame(wb_index$tm,total,metal,mine,iron,oil,coal,heihuo_index)
    DT::datatable(
      {data<-balck_index},
      colnames = c('时间','总量','金属矿石','矿建','钢材','石油','煤','黑货指数'),
      rownames = TRUE,
      style="bootstrap")
  } )
  
  
  #-----白货指数计算--------------------------------------------------------------------------------------------
  
  white_index_len<-length(wb_index$tm)
  
  #-----增长率--------------调用上面公式
  machinery1<-rate1(wb_index$machinery)
  electronic1<-rate1(wb_index$electronic)
  agricultural1<-rate1(wb_index$agricultural)
  food1<-rate1(wb_index$food_tobacco)
  education1<-rate1(wb_index$education)
  ltl1<-rate1(wb_index$ltl)
  container1<-rate1(wb_index$container)
  
  
  #-----对称变化率--------------调用上面公式
  machinery2<- rate2( machinery1,0)
  electronic2<-rate2(electronic1,0)
  agricultural2<-rate2(agricultural1,0)
  food2<-rate2(food1,0)
  education2<-rate2(education1,0)
  ltl2<-rate2(ltl1,0)
  container2<-rate2(container1,0)
  
  #-----标准化对称变换率--------调用上面公式
  machinery3<- rate3(machinery1, machinery2)
  electronic3<- rate3( electronic1, electronic2)
  agricultural3<- rate3( agricultural1, agricultural2)
  food3<- rate3( food1, food2)
  education3<- rate3( education1, education2)
  ltl3<- rate3( ltl1,ltl2)
  container3<- rate3( container1, container2)
  
  # -----平均变换率--------调用上面公式
  
  output$baihuo_index<- renderPlot( {
    
    lz_x1<-input$weightmachinery_input/100
    lz_x2<-input$weightelectronic_input/100
    lz_x3<-input$weightagricultural_input/100
    lz_x4<-input$weightfood_input/100
    lz_x5<-input$weighteducation_input/100
    lz_x6<-input$weightltl_input/100
    lz_x7<-input$weightcontainer_input/100
    
    dfweight1<- data.frame( lz_x1, lz_x2, lz_x3, lz_x4, lz_x5, lz_x6, lz_x7)
    dfinitial1<- data.frame(0.181,0.188,0.111,0.1719,0.1777,0.0429,0.1275) 
    
    
    
    averagerate1<-machinery3* lz_x1 +electronic3* lz_x2 + agricultural3* lz_x3 + food3* lz_x4+ education3*lz_x5+ltl3*lz_x6+container3* lz_x7
    wb_index$baihuo_index<-index(averagerate1,0)
    
    if(input$white_index_year2_start> input$white_index_year2_end)  {
      p<-ggplot(wb_index,x=c(wb_index$tm[1],wb_index$tm[white_index_len]),aes(x=tm,y=100))
    }
    else{
      dfsub<-subset(wb_index,(substr(wb_index$tm,1,4)>=input$white_index_year2_start) )
      dfsub<-subset(dfsub,(substr(dfsub$tm,1,4)<=input$white_index_year2_end))
      p<-ggplot(dfsub,x=c(dfsub$tm,dfsub$tm),aes(x=tm,y=90))
    }
    
    p<-p+geom_line(aes(x=tm,y=dfsub$baihuo_index),color="red",size=1)
    p+ylab("白货指数")+xlab("时间")+geom_line()+ylim(60,105)
  })
  
  # -----白货指数：数据显示-------- 
  output$baihuotable<-DT::renderDataTable({
    
    lz_x1<-input$weightmachinery_input/100
    lz_x2<-input$weightelectronic_input/100
    lz_x3<-input$weightagricultural_input/100
    lz_x4<-input$weightfood_input/100
    lz_x5<-input$weighteducation_input/100
    lz_x6<-input$weightltl_input/100
    lz_x7<-input$weightcontainer_input/100
    
    averagerate1<-machinery3* lz_x1 +electronic3* lz_x2 + agricultural3* lz_x3 + food3*lz_x4+ education3*lz_x5+ltl3*lz_x6+container3*lz_x7
    wb_index$baihuo_index<-index(averagerate1,0)
    baihuo_index<-round(wb_index$baihuo_index,3)
    total2<-machinery+electronic+agricultural+food_tobacco+education+ltl+container
    white_index<-data.frame(wb_index$tm,total2,machinery,electronic,agricultural,food_tobacco,education,ltl,container,baihuo_index)
    DT::datatable(
      {data<-white_index},
      colnames = c('时间','总量','工业机械','电子电器','农副产品','饮食烟草','文教用品','零担','集装箱','白货指数'),
      rownames = TRUE,
      style="bootstrap")
  })
  
  
  #————————————————————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————————————————————
  #适配性研究，包含固定资产与营业里程、固定资产与铺轨里程、固定资产与动车组、客运量与客车车辆数、机车辆数与营业里程、
  #货车车辆与营业里程、客车车辆与营业里程、货运量与营业里程八对指标之间的适配性研究
  #————————————————————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————————————————————
  
  #---------------------------------------------------------------------
  #---------------基本建设投资-营业里程---------------------------------
  #mileage_delta-----新增营业里程（陈雯）construct_investment----基本建设投资
  #df_yearly<-read.csv("营业里程.csv",head=T)
  a<-length(df_yearly$construct_investment)
  tm4<-df_yearly$tm[2:a]
  construct_investment1<-df_yearly$construct_investment[2:a]
  mileage_delta<-round(df_yearly$mileage_delta[2:a],2)
  df_yearly1<-data.frame(tm4,construct_investment1,mileage_delta)
  
  mileage_olsRegModel<-lm(construct_investment1~mileage_delta,data=df_yearly1)
  df_yearly1$linearRegPred<-as.integer(predict(mileage_olsRegModel,newdata=df_yearly1))
  mileage_rfRegModel<-randomForest(construct_investment1~mileage_delta,data=df_yearly1,importance=T, ntree=100,type="regression")
  df_yearly1$frRegPred<-as.integer(predict(mileage_rfRegModel,df_yearly1))
  mileage_svmRegModel<-svm(construct_investment1~mileage_delta,data=df_yearly1,type="eps-regression",cross=dim(df_yearly1)[1]/2)
  df_yearly1$svmRegPred<-as.integer(predict(mileage_svmRegModel,df_yearly1))
  mileage_len<-length(df_yearly1$tm4)
  
  
  plotCurve<-function(db,xdata,ydata)
  {
    mileage_len=dim(xdata)[1]
    mileage_plt<-ggplot(db,x=c(xdata[1],xdata[mileage_len]),aes(x=xdata,y=ydata),color="red")
    return(mileage_plt)
  }
  output$mileage_linearplot <- renderPlot( {
    
    if(input$mileage_year_start> input$mileage_year_end)  {
      
      if (input$mileage_stat_data) {
        mileage_p<-plotCurve(df_yearly1,df_yearly1$tm4,df_yearly1$construct_investment1)
      }
      else
      {
        mileage_p<-plotCurve(df_yearly1,df_yearly1$tm4,df_yearly1$linearRegPred)
      }
    }
    else{
      df_yearly1sub<-subset(df_yearly1,substr(df_yearly1$tm4,1,4)>=input$mileage_year_start) 
      df_yearly1sub<-subset(df_yearly1sub,substr(df_yearly1sub$tm4,1,4)<=input$mileage_year_end)
      if (input$mileage_stat_data) {
        mileage_p<-plotCurve(df_yearly1sub,df_yearly1sub$tm4,df_yearly1sub$construct_investment1)
      }
      else
      {
        mileage_p<-plotCurve(df_yearly1sub,df_yearly1sub$tm4,df_yearly1sub$linearRegPred)
      }
    }
    if(input$mileage_predict_data){
      
      mileage_p<-mileage_p+geom_line(aes(x=tm4,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=tm4,y=linearRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$mileage_stat_data) {
      mileage_p<-mileage_p+geom_point(aes(x=tm4,y=construct_investment1),color="red",size=3,shape=21)
    }
    mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  output$mileage_fixed_assets_investment_output<-renderText({
    mileage_x<-as.numeric(input$mileage_input)
    mileage_delta<-c(mileage_x)
    tm4<-c(2016)
    construct_investment1<-c(0)
    inputdata<-data.frame(tm4,construct_investment1,mileage_delta)
    mileage_pred<-as.integer(predict(mileage_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",mileage_pred[1],"预测区间95%：(",mileage_pred[2],",",mileage_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$mileage_fixed_assets_investment_FRR<-renderText({
    mileage_x<-as.numeric(input$mileage_input)
    mileage_delta<-c(mileage_x)
    tm4<-c(2016)
    construct_investment1<-c(0)
    inputdata<-data.frame(tm4,construct_investment1,mileage_delta)
    railfixed_assets_investment<-predict(mileage_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railfixed_assets_investment[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$mileage_fixed_assets_investment_zhi<-renderText({
    mileage_x<-as.numeric(input$mileage_input)
    mileage_delta<-c(mileage_x)
    tm4<-c(2016)
    construct_investment1<-c(0)
    inputdata<-data.frame(tm4,construct_investment1,mileage_delta)
    mileage_pred<-as.integer(predict(mileage_svmRegModel,inputdata))
    
    paste("支持向量机预测：",mileage_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$mileage_rfplot <- renderPlot( {
    
    if(input$mileage_year_start> input$mileage_year_end)  {
      
      if (input$mileage_stat_data) {
        mileage_p<-plotCurve(df_yearly1,df_yearly1$tm4,df_yearly1$construct_investment1)
      }
      else
      {
        mileage_p<-plotCurve(df_yearly1,df_yearly1$tm4,df_yearly1$frRegPred)
      }
    }
    else{
      df_yearly1sub<-subset(df_yearly1,substr(df_yearly1$tm4,1,4)>=input$mileage_year_start) 
      df_yearly1sub<-subset(df_yearly1sub,substr(df_yearly1$tm4,1,4)<=input$mileage_year_end)
      if (input$mileage_stat_data) {
        mileage_p<-plotCurve(df_yearly1sub,df_yearly1sub$tm4,df_yearly1sub$construct_investment1)
      }
      else
      {
        mileage_p<-plotCurve(df_yearly1sub,df_yearly1sub$tm4,df_yearly1sub$frRegPred)
      }
    }
    
    if(input$mileage_predict_data){
      mileage_p<-mileage_p+geom_line(aes(x=tm4,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm4,y=frRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$mileage_stat_data) {
      mileage_p<-mileage_p+geom_point(aes(x=tm4,y=construct_investment1),color="red",size=3,shape=21)
    }
    mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$mileage_svmplot <- renderPlot( {
    
    if(input$mileage_year_start> input$mileage_year_end)  {
      
      if (input$mileage_stat_data) {
        mileage_p<-plotCurve(df_yearly1,df_yearly1$tm4,df_yearly1$construct_investment1)
      }
      else
      {
        
        mileage_p<-plotCurve(df_yearly1,df_yearly1$tm4,df_yearly1$svmRegPred)
        
      }
    }
    else{
      df_yearly1sub<-subset(df_yearly1,substr(df_yearly1$tm4,1,4)>=input$mileage_year_start) 
      df_yearly1sub<-subset(df_yearly1sub,substr(df_yearly1sub$tm4,1,4)<=input$mileage_year_end)
      if (input$mileage_stat_data) {
        mileage_p<-plotCurve(df_yearly1sub,df_yearly1sub$tm4,df_yearly1sub$construct_investment1)
      }
      else
      {
        mileage_p<-plotCurve(df_yearly1sub,df_yearly1sub$tm4,df_yearly1sub$svmRegPred)
      }
    }
    if(input$mileage_predict_data){
      mileage_p<-mileage_p+geom_line(aes(x=tm4,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm4,y=svmRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
      
    }
    
    if (input$mileage_stat_data) {
      mileage_p<-mileage_p+geom_point(aes(x=tm4,y=construct_investment1),color="red",size=3,shape=21)
    }
    mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  construct_investment1<-df_yearly1$construct_investment1
  mileage_delta<-df_yearly1$mileage_delta
  linearRegPred<-df_yearly1$linearRegPred
  frRegPred<-df_yearly1$frRegPred
  svmRegPred<-df_yearly1$svmRegPred
  tm4<-unique(substr(df_yearly1$tm4,1,4))
  mileage1_data<-data.frame(tm4,construct_investment1,mileage_delta,linearRegPred,frRegPred,svmRegPred)
  
  output$mileage_table<-DT::renderDataTable(
    DT::datatable(
{
  mileage_data<-mileage1_data
} , 
colnames = c('序号', '时间', '基本建设投资','新增营业里程','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
rownames = TRUE)
  ) 



#--------------------适配性研究-----------------------------
#----------------基本建设投资-新线铺轨里程（陈雯）--------------------------
#tl_mileage-------铺轨里程 newline_tracklaying_mileage------新线铺轨里程  oldline_tracklaying_mileage--------旧线铺轨里程
#df_yearly<-read.csv("rawdata_property.csv",head=T)
df_yearly2<-df_yearly
tracklaying_mileage_olsRegModel<-lm(construct_investment~newline_tracklaying_mileage+0,data=df_yearly2)
df_yearly2$linearRegPred<-as.integer(predict(tracklaying_mileage_olsRegModel,newdata=df_yearly2))
tracklaying_mileage_rfRegModel<-randomForest(construct_investment~newline_tracklaying_mileage,data=df_yearly2,importance=T, ntree=100,type="regression")
df_yearly2$frRegPred<-as.integer(predict(tracklaying_mileage_rfRegModel,df_yearly2))
tracklaying_mileage_svmRegModel<-svm(construct_investment~newline_tracklaying_mileage,data=df_yearly2,type="eps-regression",cross=dim(df_yearly2)[1]/2)
df_yearly2$svmRegPred<-as.integer(predict(tracklaying_mileage_svmRegModel,df_yearly2))
tracklaying_mileage_len<-length(df_yearly2$tm)

plotCurve<-function(db,xdata,ydata)
{
  tracklaying_mileage_len=dim(xdata)[1]
  tracklaying_mileage_plt<-ggplot(db,x=c(xdata[1],xdata[tracklaying_mileage_len]),aes(x=xdata,y=ydata,group=1),color="red")
  return(tracklaying_mileage_plt)
}
output$tracklaying_mileage_linearplot <- renderPlot( {
  
  if(input$tracklaying_mileage_year_start> input$tracklaying_mileage_year_end)  {
    
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-plotCurve(df_yearly2,df_yearly2$tm,df_yearly2$construct_investment)
    }
    else
    {
      tracklaying_mileage_p<-plotCurve(df_yearly2,df_yearly2$tm,df_yearly2$linearRegPred)
    }
  }
  else{
    df_yearly2sub<-subset(df_yearly2,substr(df_yearly2$tm,1,4)>=input$tracklaying_mileage_year_start) 
    df_yearly2sub<-subset(df_yearly2sub,substr(df_yearly2sub$tm,1,4)<=input$tracklaying_mileage_year_end)
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-plotCurve(df_yearly2sub,df_yearly2sub$tm,df_yearly2sub$construct_investment)
    }
    else
    {
      tracklaying_mileage_p<-plotCurve(df_yearly2sub,df_yearly2sub$tm,df_yearly2sub$linearRegPred)
    }
  }
  if(input$tracklaying_mileage_predict_data){
    
    tracklaying_mileage_p<-tracklaying_mileage_p+geom_line(aes(x=tm,y=linearRegPred,group=1),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred,group=1),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    #+stat_smooth(method=lm,color='black',level=0.95)
  }
  
  if (input$tracklaying_mileage_stat_data) {
    tracklaying_mileage_p<-tracklaying_mileage_p+geom_point(aes(x=tm,y=construct_investment,group=1),color="red",size=3,shape=21)
  }
  tracklaying_mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
})
output$tracklaying_mileage_output<-renderText({
  tracklaying_mileage_x1<-as.numeric(input$newline_tracklaying_mileage_input)
  #tracklaying_mileage_x2<-as.numeric(input$oldline_tracklaying_mileage_input)
  newline_tracklaying_mileage<-c(tracklaying_mileage_x1)
  #oldline_tracklaying_mileage<-c(tracklaying_mileage_x2)
  tm<-c(2016)
  construct_investment<-c(0)
  inputdata<-data.frame(tm,construct_investment,newline_tracklaying_mileage)
  tracklaying_mileage_pred<-as.integer(predict(tracklaying_mileage_olsRegModel,inputdata,interval="prediction",level=0.95))
  paste("多元回归预测：",tracklaying_mileage_pred[1],"预测区间95%：(",tracklaying_mileage_pred[2],",",tracklaying_mileage_pred[3],")" ) 
}
)
#-------------------------------------------------
#随机森林回归预测计算
output$tracklaying_mileage_FRR<-renderText({
  tracklaying_mileage_x1<-as.numeric(input$newline_tracklaying_mileage_input)
  #tracklaying_mileage_x2<-as.numeric(input$oldline_tracklaying_mileage_input)
  newline_tracklaying_mileage<-c(tracklaying_mileage_x1)
  #oldline_tracklaying_mileage<-c(tracklaying_mileage_x2)
  tm<-c(2016)
  construct_investment<-c(0)
  inputdata<-data.frame(tm,construct_investment,newline_tracklaying_mileage)
  tracklaying_mileage_pred<-predict(tracklaying_mileage_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
  paste("随机森林回归预测：",as.integer(tracklaying_mileage_pred[1])  ) 
  
}
)
#----------------------------------
#支持向量机回归预测计算
output$tracklaying_mileage_zhi<-renderText({
  tracklaying_mileage_x1<-as.numeric(input$newline_tracklaying_mileage_input)
  #tracklaying_mileage_x2<-as.numeric(input$oldline_tracklaying_mileage_input)
  newline_tracklaying_mileage<-c(tracklaying_mileage_x1)
  #oldline_tracklaying_mileage<-c(tracklaying_mileage_x2)
  tm<-c(2016)
  construct_investment<-c(0)
  inputdata<-data.frame(tm,construct_investment,newline_tracklaying_mileage)
  tracklaying_mileage_pred<-as.integer(predict(tracklaying_mileage_svmRegModel,inputdata))
  
  paste("支持向量机预测：",tracklaying_mileage_pred)
  
}
)
#-----------随机森林Tabset画线  
output$tracklaying_mileage_rfplot <- renderPlot( {
  
  if(input$tracklaying_mileage_year_start> input$tracklaying_mileage_year_end)  {
    
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-plotCurve(df_yearly2,df_yearly2$tm,df_yearly2$construct_investment)
    }
    else
    {
      tracklaying_mileage_p<-plotCurve(df_yearly2,df_yearly2$tm,df_yearly2$frRegPred)
    }
  }
  else{
    df_yearly2sub<-subset(df_yearly2,substr(df_yearly2$tm,1,4)>=input$tracklaying_mileage_year_start) 
    df_yearly2sub<-subset(df_yearly2sub,substr(df_yearly2sub$tm,1,4)<=input$tracklaying_mileage_year_end)
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-plotCurve(df_yearly2sub,df_yearly2sub$tm,df_yearly2sub$construct_investment)
    }
    else
    {
      tracklaying_mileage_p<-plotCurve(df_yearly2sub,df_yearly2sub$tm,df_yearly2sub$frRegPred)
    }
  }
  
  if(input$tracklaying_mileage_predict_data){
    tracklaying_mileage_p<-tracklaying_mileage_p+geom_line(aes(x=tm,y=frRegPred,group=1),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred,group=1),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
  }
  
  if (input$tracklaying_mileage_stat_data) {
    tracklaying_mileage_p<-tracklaying_mileage_p+geom_point(aes(x=tm,y=construct_investment,group=1),color="red",size=3,shape=21)
  }
  tracklaying_mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
})
#----------------------------支持向量机Tabset画线

output$tracklaying_mileage_svmplot <- renderPlot( {
  
  if(input$tracklaying_mileage_year_start> input$tracklaying_mileage_year_end)  {
    
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-plotCurve(df_yearly2,df_yearly2$tm,df_yearly2$construct_investment)
    }
    else
    {
      tracklaying_mileage_p<-plotCurve(df_yearly2,df_yearly2$tm,df_yearly2$svmRegPred)
    }
  }
  else{
    df_yearly2sub<-subset(df_yearly2,substr(df_yearly2$tm,1,4)>=input$tracklaying_mileage_year_start) 
    df_yearly2sub<-subset(df_yearly2sub,substr(df_yearly2sub$tm,1,4)<=input$tracklaying_mileage_year_end)
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-plotCurve(df_yearly2sub,df_yearly2sub$tm,df_yearly2sub$construct_investment)
    }
    else
    {
      tracklaying_mileage_p<-plotCurve(df_yearly2sub,df_yearly2sub$tm,df_yearly2sub$svmRegPred)
    }
  }
  if(input$tracklaying_mileage_predict_data){
    tracklaying_mileage_p<-tracklaying_mileage_p+geom_line(aes(x=tm,y=svmRegPred,group=1),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred,group=1),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
  }
  
  if (input$tracklaying_mileage_stat_data) {
    tracklaying_mileage_p<-tracklaying_mileage_p+geom_point(aes(x=tm,y=construct_investment,group=1),color="red",size=3,shape=21)
  }
  tracklaying_mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
})

construct_investment<-df_yearly2$construct_investment
newline_tracklaying_mileage<-df_yearly2$newline_tracklaying_mileage
#oldline_tracklaying_mileage<-df_yearly2$oldline_tracklaying_mileage
linearRegPred<-df_yearly2$linearRegPred
frRegPred<-df_yearly2$frRegPred
svmRegPred<-df_yearly2$svmRegPred
tm<-unique(substr(df_yearly2$tm,1,4))
tracklaying_mileage2_data<-data.frame(tm,construct_investment,newline_tracklaying_mileage,linearRegPred,frRegPred,svmRegPred)


output$tracklaying_mileage_table<-DT::renderDataTable(
  DT::datatable(
{  tracklaying_mileage_data<-tracklaying_mileage2_data} , 
colnames = c('序号', '时间', '基本建设投资','新线铺轨里程','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
rownames = TRUE)
) 

  #--------------------------------------------------------------------------
  #----------------固定资产适配性研究----------------------------------------
  investment_fre<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
  a<-length(investment_fre$passenger_car_delta)
  tm_delta<-investment_fre$tm[11:a]
  fixed_assets_investment_delta<-investment_fre$fixed_assets_investment_delta[11:a]
  passenger_car_delta<-investment_fre$passenger_car_delta[11:a]
  bullettrain_number_delta<-investment_fre$bullettrain_number_delta[11:a]
  investment_data<-data.frame(tm_delta,fixed_assets_investment_delta,passenger_car_delta,bullettrain_number_delta)
  investment_data$tm_delta<-as.Date.POSIXct(investment_data$tm_delta,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))
  investment_y<-unique(substr(investment_data$tm_delta,1,4))
  
  #-------------olsRegModel为多元回归模型
  ptrainolsRegModel<-lm(fixed_assets_investment_delta~bullettrain_number_delta+passenger_car_delta+0,data=investment_data)
  #bound<-(predict(olsRegModel,newdata=investment_data,interval = "prediction"))  #<-----------回归模型的预测数据已经计算得到
  #investment_data$linearRegPred<-as.integer(bound[,1])
  investment_data$linearRegPred<-as.integer(predict(ptrainolsRegModel,newdata=investment_data))
  
  ptrainrfRegModel<-randomForest(fixed_assets_investment_delta~bullettrain_number_delta+passenger_car_delta,data=investment_data,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  investment_data$frRegPred<-as.integer(predict(ptrainrfRegModel,investment_data))  
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  ptrainsvmRegModel<-svm(fixed_assets_investment_delta~bullettrain_number_delta+passenger_car_delta,data=investment_data,type="eps-regression",cross=dim(investment_data)[1]/2)
  #svm 内含交叉验证，所以不需要再运行交叉验证.eps-regression   huigui
  investment_data$svmRegPred<-as.integer(predict(ptrainsvmRegModel,investment_data))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  investment_len<-length(investment_data$tm_delta)
  #plotCurve是画曲线的通过用函数，为了减少后面的代码量  
  plotCurve<-function(db,xdata,ydata)
  {
    investment_len=dim(xdata)[1]
    plt<-ggplot(db,x=c(xdata[1],xdata[investment_len]),aes(x=xdata,y=ydata),color="red")
    return(plt)
  }
  #---------------------------多元回归画线
  output$investmentlinearplot <- renderPlot( {
    
    if(input$investment_year_start> input$investment_year_end)  {
      
      if (input$investment_stat_data) {
        p<-plotCurve(investment_data,investment_data$tm_delta,investment_data$fixed_assets_investment_delta)
      }
      else
      {
        p<-plotCurve(investment_data,investment_data$tm_delta,investment_data$linearRegPred)
      }
    }
    else{
      dfsub<-subset(investment_data,substr(investment_data$tm_delta,1,4)>=input$investment_year_start) 
      dfsub<-subset(dfsub,substr(dfsub$tm_delta,1,4)<=input$investment_year_end)
      if (input$investment_stat_data) {
        p<-plotCurve(dfsub,dfsub$tm_delta,dfsub$fixed_assets_investment_delta)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm_delta,dfsub$linearRegPred)
      }
    }
    
    if(input$investment_predict_data){
      
      p<-p+geom_line(aes(x=tm_delta,y=linearRegPred),color="blue",size=0.8)#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$investment_stat_data) {
      p<-p+geom_point(aes(x=tm_delta,y=fixed_assets_investment_delta),color="red",size=3,shape=21)
    }
    p+ylab("固定资产投资额")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------------------------------
  
  #----------------------------------------------------   
  #多元回归预测计算
  output$investment_output<-renderText({
    ptrain_x1<-as.numeric(input$ptrain_input)
    passenger_car_delta<-c(ptrain_x1)
    htrain_x1<-as.numeric(input$htrain_input)
    bullettrain_number_delta<-c(htrain_x1)   
    tm<-c(2014)
    fixed_assets_investment_delta<-c(0)
    inputdata<-data.frame(tm,fixed_assets_investment_delta,passenger_car_delta,bullettrain_number_delta)#  其中的数不能省略
    pred<-as.integer(predict(ptrainolsRegModel,inputdata))
    paste("多元回归预测：",pred ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$investment_FRR<-renderText({
    ptrain_x1<-as.numeric(input$ptrain_input)
    passenger_car_delta<-c(ptrain_x1)
    htrain_x1<-as.numeric(input$htrain_input)
    bullettrain_number_delta<-c(htrain_x1)
    tm<-c(2014)
    fixed_assets_investment_delta<-c(0)
    inputdata<-data.frame(tm,fixed_assets_investment_delta,passenger_car_delta,bullettrain_number_delta)
    railinvestment<-predict(ptrainrfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railinvestment[1])  )
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$investment_zhi<-renderText({
    ptrain_x1<-as.numeric(input$ptrain_input)
    passenger_car_delta<-c(ptrain_x1)
    htrain_x1<-as.numeric(input$htrain_input)
    bullettrain_number_delta<-c(htrain_x1)   
    tm<-c(2014)
    fixed_assets_investment_delta<-c(0)
    inputdata<-data.frame(tm,fixed_assets_investment_delta,passenger_car_delta,bullettrain_number_delta)
    pred<-as.integer(predict(ptrainsvmRegModel,inputdata))
    paste("支持向量机预测：",pred)
    
  }
  )
  #-------------------------------------
  # -----------随机森林Tabset画线
  output$investmentrfplot <- renderPlot( {
    
    if(input$investment_year_start> input$investment_year_end)  {
      
      if (input$investment_stat_data) {
        p<-plotCurve(investment_data,investment_data$tm_delta,investment_data$fixed_assets_investment_delta)
      }
      else
      {
        p<-plotCurve(investment_data,investment_data$tm_delta,investment_data$frRegPred)
      }
    }
    else{
      dfsub<-subset(investment_data,substr(investment_data$tm_delta,1,4)>=input$investment_year_start)
      dfsub<-subset(dfsub,substr(dfsub$tm_delta,1,4)<=input$investment_year_end)
      if (input$investment_stat_data) {
        p<-plotCurve(dfsub,dfsub$tm_delta,dfsub$fixed_assets_investment_delta)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm_delta,dfsub$frRegPred)
      }
    }
    
    if(input$investment_predict_data){
      p<-p+geom_line(aes(x=tm_delta,y=frRegPred),color="blue",size=0.8,show.legend = T)#+stat_smooth(method=rfRegModel,color='black',level=0.95)
    }
    
    if (input$investment_stat_data) {
      p<-p+geom_point(aes(x=tm_delta,y=fixed_assets_investment_delta),color="red",size=3,shape=21)
    }
    p+ylab("固定资产投资额")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$investmentsvmplot <- renderPlot( {
    
    if(input$investment_year_start> input$investment_year_end)  {
      
      if (input$investment_stat_data) {
        p<-plotCurve(investment_data,investment_data$tm_delta,investment_data$fixed_assets_investment_delta)
      }
      else
      {
        p<-plotCurve(investment_data,investment_data$tm_delta,investment_data$svmRegPred)
      }
    }
    else{
      dfsub<-subset(investment_data,substr(investment_data$tm_delta,1,4)>=input$investment_year_start) 
      dfsub<-subset(dfsub,substr(dfsub$tm_delta,1,4)<=input$investment_year_end)
      if (input$investment_stat_data) {
        p<-plotCurve(dfsub,dfsub$tm_delta,dfsub$fixed_assets_investment_delta)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm_delta,dfsub$svmRegPred)
      }
    }
    
    if(input$investment_predict_data){
      p<-p+geom_line(aes(x=tm_delta,y=svmRegPred),color="blue",size=0.8)#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
    }
    
    if (input$investment_stat_data) {
      p<-p+geom_point(aes(x=tm_delta,y=fixed_assets_investment_delta),color="red",size=3,shape=21)
    }
    p+ylab("固定资产额")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  output$investmenttable<-DT::renderDataTable(
    DT::datatable(
      data<-investment_data, 
      colnames = c('序号', '年','固定资产投资增加额（万元）','客车车辆数（辆）','动车组数量(组)','多元回归预测（万元）','支持向量机回归预测（万元）'),
      rownames = TRUE)
  )
  
  
  
  
#------------------------------------------------------------------------------------------
#------------------客运量-客车车辆数适配性研究--------------------------------------------
#PV-------客运量（passenger_volume）简写
#passenger_volume-------客运量
#bullettrain_number-------动车组数
#locomotive_mileage_pcar-------客车机车日行公里数
PVdf<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
PVolsRegModel<-lm(passenger_volume~bullettrain_number+locomotive_mileage_pcar,data=PVdf)
PVdf$linearRegPred<-as.integer(predict(PVolsRegModel,newdata=PVdf))
PVrfRegModel<-randomForest(passenger_volume~bullettrain_number+locomotive_mileage_pcar,data=PVdf,importance=T, ntree=100,type="regression")
PVdf$frRegPred<-as.integer(predict(PVrfRegModel,PVdf))
PVsvmRegModel<-svm(passenger_volume~bullettrain_number+locomotive_mileage_pcar,data=PVdf,type="eps-regression",cross=dim(PVdf)[1]/2)
PVdf$svmRegPred<-as.integer(predict(PVsvmRegModel,PVdf))
PVlen<-length(PVdf$tm)

plotCurve<-function(db,xdata,ydata)
{
  PVlen=dim(xdata)[1]
  PVplt<-ggplot(db,x=c(xdata[1],xdata[PVlen]),aes(x=xdata,y=ydata),color="red")
  return(PVplt)
}
output$passenger_volume_linearplot <- renderPlot( {
  
  if(input$passenger_volume_year_start> input$passenger_volume_year_end)  {
    
    if (input$passenger_volume_stat_data) {
      PVp<-plotCurve(PVdf,PVdf$tm,PVdf$passenger_volume)
    }
    else
    {
      PVp<-plotCurve(PVdf,PVdf$tm,PVdf$linearRegPred)
    }
  }
  else{
    PVdfsub<-subset(PVdf,substr(PVdf$tm,1,4)>=input$passenger_volume_year_start) 
    PVdfsub<-subset(PVdfsub,substr(PVdfsub$tm,1,4)<=input$passenger_volume_year_end)
    if (input$passenger_volume_stat_data) {
      PVp<-plotCurve(PVdfsub,PVdfsub$tm,PVdfsub$passenger_volume)
    }
    else
    {
      PVp<-plotCurve(PVdfsub,PVdfsub$tm,PVdfsub$linearRegPred)
    }
  }
  if(input$passenger_volume_predict_data){
    
    PVp<-PVp+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=1)+geom_point(aes(x=tm,y=linearRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
    
  }
  
  if (input$passenger_volume_stat_data) {
    PVp<-PVp+geom_point(aes(x=tm,y=passenger_volume),color="red",size=3,shape=21)
  }
  PVp+ylab("客运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
})
output$passenger_volume_output<-renderText({
  PVx1<-as.numeric(input$bullettrain_number_input)
  PVx2<-as.numeric(input$locomotive_mileage_pcar_input)
  bullettrain_number<-c(PVx1)
  locomotive_mileage_pcar<-c(PVx2)
  tm<-c(2016)
  passenger_volume<-c(0)
  inputdata<-data.frame(tm,passenger_volume,bullettrain_number,locomotive_mileage_pcar)
  PVpred<-as.integer(predict(PVolsRegModel,inputdata,interval="prediction",level=0.95))
  paste("多元回归预测：",PVpred[1],"预测区间95%：(",PVpred[2],",",PVpred[3],")" ) 
}
)
#-------------------------------------------------
#随机森林回归预测计算
output$passenger_volume_FRR<-renderText({
  PVx1<-as.numeric(input$bullettrain_number_input)
  PVx2<-as.numeric(input$locomotive_mileage_pcar_input)
  bullettrain_number<-c(PVx1)
  locomotive_mileage_pcar<-c(PVx2)
  tm<-c(2016)
  passenger_volume<-c(0)
  inputdata<-data.frame(tm,passenger_volume,bullettrain_number,locomotive_mileage_pcar)
  railpassenger_volume<-predict(PVrfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
  paste("随机森林回归预测：",as.integer(railpassenger_volume[1])  ) 
  
}
)
#----------------------------------
#支持向量机回归预测计算
output$passenger_volume_zhi<-renderText({
  PVx1<-as.numeric(input$bullettrain_number_input)
  PVx2<-as.numeric(input$locomotive_mileage_pcar_input)
  bullettrain_number<-c(PVx1)
  locomotive_mileage_pcar<-c(PVx2)
  tm<-c(2016)
  passenger_volume<-c(0)
  inputdata<-data.frame(tm,passenger_volume,bullettrain_number,locomotive_mileage_pcar)
  PVpred<-as.integer(predict(PVsvmRegModel,inputdata))
  
  paste("支持向量机预测：",PVpred)
  
}
)
#-----------随机森林Tabset画线  
output$passenger_volume_rfplot <- renderPlot( {
  
  if(input$passenger_volume_year_start> input$passenger_volume_year_end)  {
    
    if (input$passenger_volume_stat_data) {
      PVp<-plotCurve(PVdf,PVdf$tm,PVdf$passenger_volume)
    }
    else
    {
      PVp<-plotCurve(PVdf,PVdf$tm,PVdf$frRegPred)
    }
  }
  else{
    PVdfsub<-subset(PVdf,substr(PVdf$tm,1,4)>=input$passenger_volume_year_start) 
    PVdfsub<-subset(PVdfsub,substr(PVdfsub$tm,1,4)<=input$passenger_volume_year_end)
    if (input$passenger_volume_stat_data) {
      PVp<-plotCurve(PVdfsub,PVdfsub$tm,PVdfsub$passenger_volume)
    }
    else
    {
      PVp<-plotCurve(PVdfsub,PVdfsub$tm,PVdfsub$frRegPred)
    }
    PVp+ylab("客运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  }
 
  if(input$passenger_volume_predict_data){
    PVp<-PVp+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+stat_smooth(method=rfRegModel,color='black',level=0.95)
  }
  
  if (input$passenger_volume_stat_data) {
    PVp<-PVp+geom_point(aes(x=tm,y=passenger_volume),color="red",size=3,shape=21)
  }
  
  PVp+ylab("客运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  }
)
#----------------------------支持向量机Tabset画线

output$passenger_volume_svmplot <- renderPlot( {
  
  if(input$passenger_volume_year_start> input$passenger_volume_year_end)  {
    
    if (input$passenger_volume_stat_data) {
      PVp<-plotCurve(PVdf,PVdf$tm,PVdf$passenger_volume)
    }
    else
    {
      PVp<-plotCurve(PVdf,PVdf$tm,PVdf$svmRegPred)
    }
  }
  else{
    PVdfsub<-subset(PVdf,substr(PVdf$tm,1,4)>=input$passenger_volume_year_start) 
    PVdfsub<-subset(PVdfsub,substr(PVdfsub$tm,1,4)<=input$passenger_volume_year_end)
    if (input$passenger_volume_stat_data) {
      PVp<-plotCurve(PVdfsub,PVdfsub$tm,PVdfsub$passenger_volume)
    }
    else
    {
      PVp<-plotCurve(PVdfsub,PVdfsub$tm,PVdfsub$svmRegPred)
    }
  }
  if(input$passenger_volume_predict_data){
    PVp<-PVp+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
  }
  
  if (input$passenger_volume_stat_data) {
    PVp<-PVp+geom_point(aes(x=tm,y=passenger_volume),color="red",size=3,shape=21)
  }
  PVp+ylab("客运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
})

#--------------------------------------

#----------------------datatable显示数据
#-----------------在df中，又增加了3列数据，存放预测结果,
passenger_volume<-PVdf$passenger_volume
bullettrain_number<-PVdf$bullettrain_number
locomotive_mileage_pcar<-PVdf$locomotive_mileage_pcar
linearRegPred<-PVdf$linearRegPred
frRegPred<-PVdf$frRegPred
svmRegPred<-PVdf$svmRegPred
tm<-unique(substr(PVdf$tm,1,4))
passenger_volume_data<-data.frame(tm,passenger_volume,bullettrain_number,locomotive_mileage_pcar,linearRegPred,frRegPred,svmRegPred)


output$passenger_volume_table<-DT::renderDataTable(
  DT::datatable(
{
  
  PVdata<-passenger_volume_data
} , 
colnames = c('序号', '时间', '客运量（万人）','动车组数（组）','客车机车日行公里（公里）','多元回归预测（亿万）','随机森林回归预测（亿万）','随机森林回归预测（万元）','支持向量机回归预测（亿万）'),
rownames = TRUE)
)
  #--------------------------------------------------------------------
  #----------------------营业里程适配性研究---------------------------
  distance_fre<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
  
  #-------------olsRegModel为多元回归模型
  distanceolsRegModel<-lm(mileage~locomotive_number+bullettrain_number+0,data=distance_fre)
  
  distance_fre$dislinearRegPred<-as.integer(predict(distanceolsRegModel,newdata=distance_fre))
  
  
  
  #-------rfRegModel是随机森林得到的回归模型，后面用predict直接调用此模型即可,因数量少，不运行交叉验证
  distancerfRegModel<-randomForest(mileage~locomotive_number+bullettrain_number,data=distance_fre,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  distance_fre$disRegPred<-as.integer(predict(distancerfRegModel,distance_fre))    #<-----------随机森林的预测数据已经在这里计算得到
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  distancesvmRegModel<-svm(mileage~locomotive_number+bullettrain_number,data=distance_fre,type="eps-regression",cross=dim(distance_fre)[1]/2)
  #svm 内含交叉验证，所以不需要再运行交叉验证.eps-regression   huigui
  distance_fre$dissvmRegPred<-as.integer(predict(distancesvmRegModel,distance_fre))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  distance_len<-length(distance_fre$tm)
  #plotCurve是画曲线的通过用函数，为了减少后面的代码量  
  plotCurve<-function(db,xdata,ydata)
  {
    distance_len=dim(xdata)[1]
    plt<-ggplot(db,x=c(xdata[1],xdata[distance_len]),aes(x=xdata,y=ydata),color="red")
    return(plt)
  }
  #---------------------------多元回归画线-------------------------------------------
  output$distancelinearplot <- renderPlot( {
    
    if(input$distance_year_start1>input$distance_year_end1)  {
      
      if (input$distance_stat_data1) {
        p<-plotCurve(distance_fre,distance_fre$tm,distance_fre$mileage)
      }
      else
      {
        p<-plotCurve(distance_fre,distance_fre$tm,distance_fre$dislinearRegPred)
      }
    }
    else{
      dfsub<-subset(distance_fre,substr(distance_fre$tm,1,4)>=input$distance_year_start1) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$distance_year_end1)
      if (input$distance_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$mileage)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$dislinearRegPred)
      }
    }
    
    if(input$distance_predict_data1){
      
      p<-p+geom_line(aes(x=tm,y=dislinearRegPred),color="blue",size=0.8)#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$distance_stat_data1) {
      p<-p+geom_point(aes(x=tm,y=mileage),color="red",size=3,shape=21)

    }
    p+ylab("营业里程")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------------------------------
  
  #----------------------------------------------------   
  #多元回归预测计算
  output$distance_output<-renderText({
    Locomotive_x2<-as.numeric(input$locomotive_input)
    locomotive_number<-c(Locomotive_x2)
    bullettrain_x3<-as.numeric(input$bullettrain_input)
    bullettrain_number<-bullettrain_x3
    tm<-c(2016)
    mileage<-c(0)
    inputdata<-data.frame(tm,mileage,locomotive_number,bullettrain_number)#  其中的数不能省略
    distancepred<-as.integer(predict(distanceolsRegModel,inputdata))
    paste("多元回归预测：",distancepred) 
  }

  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$distance_FRR<-renderText({
    Locomotive_x2<-as.numeric(input$locomotive_input)
    locomotive_number<-c(Locomotive_x2)
    bullettrain_x3<-as.numeric(input$bullettrain_input)
    bullettrain_number<-bullettrain_x3
    tm<-c(2016)
    mileage<-c(0)
    inputdata<-data.frame(tm,mileage,locomotive_number,bullettrain_number)
    distancepred<-predict(distancerfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(distancepred)  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$distance_zhi<-renderText({
    Locomotive_x2<-as.numeric(input$locomotive_input)
    locomotive_number<-c(Locomotive_x2)
    bullettrain_x3<-as.numeric(input$bullettrain_input)
    bullettrain_number<-bullettrain_x3
    tm<-c(2016)
    mileage<-c(0)
    inputdata<-data.frame(tm,mileage,locomotive_number,bullettrain_number)
    distancepred<-as.integer(predict(distancesvmRegModel,inputdata))
    paste("支持向量机预测：",distancepred)
    
  }
  )
  #-------------------------------------
  
  
  #-----------随机森林Tabset画线  
  output$distancerfplot<- renderPlot( {
    
    if(input$distance_year_start1> input$distance_year_end1)  {
      
      if (input$distance_stat_data1) {
        p<-plotCurve(distance_fre,distance_fre$tm,distance_fre$mileage)
      }
      else
      {
        p<-plotCurve(distance_fre,distance_fre$tm,distance_fre$disRegPred)
      }
    }
    else{
      dfsub<-subset(distance_fre,substr(distance_fre$tm,1,4)>=input$distance_year_start1) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$distance_year_end1)
      if (input$distance_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$mileage)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$disRegPred)
      }
    }
    
    if(input$distance_predict_data1){
      p<-p+geom_line(aes(x=tm,y=disRegPred),color="blue",size=0.8,show.legend = T)#+stat_smooth(method=rfRegModel,color='black',level=0.95)
    }
    
    if (input$distance_stat_data1) {
      p<-p+geom_point(aes(x=tm,y=mileage),color="red",size=3,shape=21)
    }
    p+ylab("营业里程")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------支持向量机Tabset画线
  
  output$distancesvmplot<- renderPlot( {
    
    if(input$distance_year_start1> input$distance_year_end1)  {
      
      if (input$distance_stat_data1) {
        p<-plotCurve(distance_fre,distance_fre$tm,distance_fre$mileage)
      }
      else
      {
        p<-plotCurve(distance_fre,distance_fre$tm,distance_fre$dissvmRegPred)
      }
    }
    else{
      dfsub<-subset(distance_fre,substr(distance_fre$tm,1,4)>=input$distance_year_start1) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$distance_year_end1)
      if (input$distance_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$mileage)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$dissvmRegPred)
      }
    }
    
    if(input$distance_predict_data1){
      p<-p+geom_line(aes(x=tm,y=dissvmRegPred),color="blue",size=0.8)#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
    }
    
    if (input$distance_stat_data1) {
      p<-p+geom_point(aes(x=tm,y=mileage),color="red",size=3,shape=21)
    }
    p+ylab("营业里程")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  mileage<-distance_fre$mileage
  bullettrain_number<-distance_fre$bullettrain_number
  locomotive_number<-distance_fre$locomotive_number
  dislinearRegPred<-distance_fre$dislinearRegPred
  disRegPred<-distance_fre$disRegPred
  dissvmRegPred<-distance_fre$dissvmRegPred
  tm<-unique(substr(distance_fre$tm,1,4))
  distance_data<-data.frame(tm,mileage,locomotive_number,bullettrain_number,dislinearRegPred,disRegPred,dissvmRegPred)
  output$distancetable<-DT::renderDataTable(
    DT::datatable(
      data<-distance_data, 
      colnames = c('序号', '年','营业里程（公里）','机车数量（辆）',"动车组数（组)",'多元回归预测（公里）','随机森林回归预测（公里）','支持向量机回归预测（公里）'),
      rownames = TRUE)
  )
 
  
  #---------------------------------------------------------------------------------------------------------
  #-----------------------------------------机车数量适配性研究----------------------------------------------
  
  Locomotive_fre<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
  Locomotive_fre$tm<-as.Date.POSIXct(Locomotive_fre$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))
  
  #-------------olsRegModel为多元回归模型
  freightolsRegModel<-lm(locomotive_number~freight_volume_yearly+ptpassenger_volume,data=Locomotive_fre)
  
  Locomotive_fre$linearRegPred<-as.integer(predict(freightolsRegModel,newdata=Locomotive_fre))
  
  
  
  #-------rfRegModel是随机森林得到的回归模型，后面用predict直接调用此模型即可,因数量少，不运行交叉验证
  freightrfRegModel<-randomForest(locomotive_number~freight_volume_yearly+ptpassenger_volume,data=Locomotive_fre,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  Locomotive_fre$frRegPred<-as.integer(predict(freightrfRegModel,Locomotive_fre))    #<-----------随机森林的预测数据已经在这里计算得到
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  freightsvmRegModel<-svm(locomotive_number~freight_volume_yearly+ptpassenger_volume,data=Locomotive_fre,type="eps-regression",cross=dim(Locomotive_fre)[1]/2)
  #svm 内含交叉验证，所以不需要再运行交叉验证.eps-regression   huigui
  Locomotive_fre$svmRegPred<-as.integer(predict(freightsvmRegModel,Locomotive_fre))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  locomotive_len<-length(Locomotive_fre$tm)
  #plotCurve是画曲线的通过用函数，为了减少后面的代码量  
  plotCurve<-function(db,xdata,ydata)
  {
    locomotive_len=dim(xdata)[1]
    plt<-ggplot(db,x=c(xdata[1],xdata[locomotive_len]),aes(x=xdata,y=ydata),color="red")
    return(plt)
  }
  #---------------------------多元回归画线-------------------------------------------
  output$freightlinearplot <- renderPlot( {
    
    if(input$Locomotive_year_start1> input$Locomotive_year_end1)  {
      
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$locomotive_number)
      }
      else
      {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$linearRegPred)
      }
    }
    else{
      dfsub<-subset(Locomotive_fre,substr(Locomotive_fre$tm,1,4)>=input$Locomotive_year_start1) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$Locomotive_year_end1)
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$locomotive_number)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$linearRegPred)
      }
    }
    
    if(input$Locomotive_predict_data1){
      
      p<-p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$Locomotive_stat_data1) {
      p<-p+geom_point(aes(x=tm,y=locomotive_number),color="red",size=3,shape=21)
    }
    p+ylab("机车数量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------------------------------
  
  #----------------------------------------------------   
  #多元回归预测计算
  output$locomotive_output1<-renderText({
    Locomotive_x2<-as.numeric(input$ton_input)
    freight_volume_yearly<-c(Locomotive_x2)
    Locomotive_x3<-as.numeric(input$ptpassenger_input)
    ptpassenger_volume<-Locomotive_x3
    tm<-c(2016)
    locomotive_number<-c(0)
    inputdata<-data.frame(tm,locomotive_number, freight_volume_yearly,ptpassenger_volume)#  其中的数不能省略
    freightpred<-as.integer(predict(freightolsRegModel,inputdata))
    paste("多元回归预测：",freightpred ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$locomotive_FRR1<-renderText({
    Locomotive_x2<-as.numeric(input$ton_input)
    freight_volume_yearly<-c(Locomotive_x2)
    Locomotive_x3<-as.numeric(input$ptpassenger_input)
    ptpassenger_volume<-Locomotive_x3
    tm<-c(2016)
    locomotive_number<-c(0)
    inputdata<-data.frame(tm,locomotive_number, freight_volume_yearly,ptpassenger_volume)
    raillocomotive<-predict(freightrfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(raillocomotive[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$locomotive_zhi1<-renderText({
    Locomotive_x2<-as.numeric(input$ton_input)
    freight_volume_yearly<-c(Locomotive_x2)
    Locomotive_x3<-as.numeric(input$ptpassenger_input)
    ptpassenger_volume<-Locomotive_x3
    tm<-c(2016)
    locomotive_number<-c(0)
    inputdata<-data.frame(tm,locomotive_number, freight_volume_yearly,ptpassenger_volume)
    freightpred<-as.integer(predict(freightsvmRegModel,inputdata))
    
    paste("支持向量机预测：",freightpred)
    
  }
  )
  #-------------------------------------
  
  
  #-----------随机森林Tabset画线  
  output$freightrfplot<- renderPlot( {
    
    if(input$Locomotive_year_start1> input$Locomotive_year_end1)  {
      
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$locomotive_number)
      }
      else
      {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$frRegPred)
      }
    }
    else{
      dfsub<-subset(Locomotive_fre,substr(Locomotive_fre$tm,1,4)>=input$Locomotive_year_start1) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$Locomotive_year_end1)
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$locomotive_number)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$frRegPred)
      }
    }
    
    if(input$Locomotive_predict_data1){
      p<-p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)#+stat_smooth(method=rfRegModel,color='black',level=0.95)
    }
    
    if (input$Locomotive_stat_data1) {
      p<-p+geom_point(aes(x=tm,y=locomotive_number),color="red",size=3,shape=21)
    }
    p+ylab("机车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------支持向量机Tabset画线
  
  output$freightsvmplot<- renderPlot( {
    
    if(input$Locomotive_year_start1> input$Locomotive_year_end1)  {
      
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$locomotive_number)
      }
      else
      {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$svmRegPred)
      }
    }
    else{
      dfsub<-subset(Locomotive_fre,substr(Locomotive_fre$tm,1,4)>=input$Locomotive_year_start1) 
      dfsub<-subset(dfsub,substr(dfsub$tm,1,4)<=input$Locomotive_year_end1)
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$locomotive_number)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$svmRegPred)
      }
    }
    
    if(input$Locomotive_predict_data1){
      p<-p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
    }
    
    if (input$Locomotive_stat_data1) {
      p<-p+geom_point(aes(x=tm,y=locomotive_number),color="red",size=3,shape=21)
    }
    p+ylab("机车数量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  ptpassenger<-Locomotive_fre$ptpassenger_volume
  freight<-Locomotive_fre$freight_volume_yearly
  locomotive<-Locomotive_fre$locomotive_number
  linearRegPred<-Locomotive_fre$linearRegPred
  frRegPred<-Locomotive_fre$frRegPred
  svmRegPred<-Locomotive_fre$svmRegPred
  tm<-unique(substr(Locomotive_fre$tm,1,4))
  locomotive_data<-data.frame(tm,locomotive,ptpassenger,freight,linearRegPred,frRegPred,svmRegPred)
  output$freighttable<-DT::renderDataTable(
    DT::datatable(
      data<-locomotive_data, 
      colnames = c('序号', '年','机车数量（辆）','货运量（万吨）',"普铁客运量（万人)",'多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
      rownames = TRUE)
  )
  
  

  #----------------------------------------------------------
  #------------货运量-营业里程适配性研究--------------------
  #该部分对应报告中“货运量-营业里程适配性研究”一节，最终的回归模型中不仅包含货运量和营业里程，还包含货车车辆数
  #freight_olm_car_df表示本适配性研究用到的数据集，包含货运量、营业里程、货车车辆数三个变量
  
  freight_olm_car_df<-read.csv("货运量-营业里程.csv",head=T)  
  freight_olm_car_df$tm<-as.Date.POSIXct(freight_olm_car_df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
  
  f_car_olsRegModel<-lm(freight~freightcar+olm,data=freight_olm_car_df)
  freight_olm_car_df$linearRegPred<-as.integer(predict(f_car_olsRegModel,newdata=freight_olm_car_df))
  f_car_rfRegModel<-randomForest(freight~freightcar+olm,data=freight_olm_car_df,importance=T, ntree=100,type="regression")
  freight_olm_car_df$frRegPred<-as.integer(predict(f_car_rfRegModel,freight_olm_car_df))
  f_car_svmRegModel<-svm(freight~freightcar+olm,data=freight_olm_car_df,type="eps-regression",cross=dim(freight_olm_car_df)[1]/2)
  freight_olm_car_df$svmRegPred<-as.integer(predict(f_car_svmRegModel,freight_olm_car_df))
  pg_cw_len<-length(freight_olm_car_df$tm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    pg_cw_len=dim(xdata)[1]
    cw_plt<-ggplot(db,x=c(xdata[1],xdata[pg_cw_len]),aes(x=xdata,y=ydata),color="red")
    return(cw_plt)
  }
  output$f_car_linearplot <- renderPlot( {
    
    if(input$freight_mileage_year_start> input$freight_mileage_year_end)  {
      
      if (input$freight_mileage_stat_data) {
        cw_p<-plotCurve(freight_olm_car_df,freight_olm_car_df$tm,freight_olm_car_df$freight)
      }
      else
      {
        cw_p<-plotCurve(freight_olm_car_df,freight_olm_car_df$tm,freight_olm_car_df$linearRegPred)
      }
    }
    else{
      freight_olm_car_dfsub<-subset(freight_olm_car_df,substr(freight_olm_car_df$tm,1,4)>=input$freight_mileage_year_start) 
      freight_olm_car_dfsub<-subset(freight_olm_car_dfsub,substr(freight_olm_car_df$tm,1,4)<=input$freight_mileage_year_end)
      if (input$freight_mileage_stat_data) {
        cw_p<-plotCurve(freight_olm_car_dfsub,freight_olm_car_dfsub$tm,freight_olm_car_dfsub$freight)
      }
      else
      {
        cw_p<-plotCurve(freight_olm_car_dfsub,freight_olm_car_dfsub$tm,freight_olm_car_dfsub$linearRegPred)
      }
    }
    if(input$freight_mileage_predict_data){
      
      cw_p<-cw_p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred),size=4,shape=18,colour="blue",position=position_dodge(width=0.2))
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$freight_mileage_stat_data) {
      cw_p<-cw_p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    cw_p+ylab("货运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  output$f_car_output<-renderText({
    cw_x1<-as.numeric(input$freightcar_input)
    cw_x2<-as.numeric(input$freightolm_input)
    freightcar<-c(cw_x1)
    olm<-c(cw_x2)
    tm<-c(2016)
    freight<-c(0)
    inputdata<-data.frame(tm,freight,freightcar,olm)
    cw_pred<-as.integer(predict(f_car_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",cw_pred[1],"预测区间95%：(",cw_pred[2],",",cw_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算

  output$f_car_FRR<-renderText({
    cw_x1<-as.numeric(input$freightcar_input)
    cw_x2<-as.numeric(input$freightolm_input)
    freightcar<-c(cw_x1)
    olm<-c(cw_x2)
    tm<-c(2016)
    freight<-c(0)
    inputdata<-data.frame(tm,freight,freightcar,olm)
    railfreight<-predict(f_car_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railfreight[1])  ) 

  }
  )
  #----------------------------------
  #支持向量机回归预测计算

  output$f_car_zhi<-renderText({
    cw_x1<-as.numeric(input$freightcar_input)
    cw_x2<-as.numeric(input$freightolm_input)
    freightcar<-c(cw_x1)
    olm<-c(cw_x2)
    tm<-c(2016)
    freight<-c(0)
    inputdata<-data.frame(tm,freight,freightcar,olm)
    cw_pred<-as.integer(predict(f_car_svmRegModel,inputdata))
    
    paste("支持向量机预测：",cw_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$f_car_rfplot <- renderPlot( {
    
    if(input$freight_mileage_year_start> input$freight_mileage_year_end)  {
      
      if (input$freight_mileage_stat_data) {
        cw_p<-plotCurve(freight_olm_car_df,freight_olm_car_df$tm,freight_olm_car_df$freight)
      }
      else
      {
        cw_p<-plotCurve(freight_olm_car_df,freight_olm_car_df$tm,freight_olm_car_df$frRegPred)
      }
    }
    else{
      freight_olm_car_dfsub<-subset(freight_olm_car_df,substr(freight_olm_car_df$tm,1,4)>=input$freight_mileage_year_start) 
      freight_olm_car_dfsub<-subset(freight_olm_car_dfsub,substr(freight_olm_car_df$tm,1,4)<=input$freight_mileage_year_end)
      if (input$freight_mileage_stat_data) {
        cw_p<-plotCurve(freight_olm_car_dfsub,freight_olm_car_dfsub$tm,freight_olm_car_dfsub$freight)
      }
      else
      {
        cw_p<-plotCurve(freight_olm_car_dfsub,freight_olm_car_dfsub$tm,freight_olm_car_dfsub$frRegPred)
      }
    }
    
    if(input$freight_mileage_predict_data){
      cw_p<-cw_p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred),size=4,shape=18,colour="blue",position=position_dodge(width=0.2))
    }
    
    if (input$freight_mileage_stat_data) {
      cw_p<-cw_p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    cw_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$f_car_svmplot <- renderPlot( {
    
    if(input$freight_mileage_year_start> input$freight_mileage_year_end)  {
      
      if (input$freight_mileage_stat_data) {
        cw_p<-plotCurve(freight_olm_car_df,freight_olm_car_df$tm,freight_olm_car_df$freight)
      }
      else
      {
        cw_p<-plotCurve(freight_olm_car_df,freight_olm_car_df$tm,freight_olm_car_df$svmRegPred)
      }
    }
    else{
      freight_olm_car_dfsub<-subset(freight_olm_car_df,substr(freight_olm_car_df$tm,1,4)>=input$freight_mileage_year_start) 
      freight_olm_car_dfsub<-subset(freight_olm_car_dfsub,substr(freight_olm_car_df$tm,1,4)<=input$freight_mileage_year_end)
      if (input$freight_mileage_stat_data) {
        cw_p<-plotCurve(freight_olm_car_dfsub,freight_olm_car_dfsub$tm,freight_olm_car_dfsub$freight)
      }
      else
      {
        cw_p<-plotCurve(freight_olm_car_dfsub,freight_olm_car_dfsub$tm,freight_olm_car_dfsub$svmRegPred)
      }
    }
    if(input$freight_mileage_predict_data){
      cw_p<-cw_p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred),size=4,shape=18,colour="blue",position=position_dodge(width=0.2))
    }
    
    if (input$freight_mileage_stat_data) {
      cw_p<-cw_p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    cw_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
 
  output$f_car_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        pg_cw_data<-freight_olm_car_df
      } , 
      colnames = c('序号', '时间', '货运量','货车车辆数','营业里程','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
      rownames = TRUE)
  )
  
  
  #————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————
  #货运量预测
  #————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————
  
a<-c(1,2,3,8)
df<-df_monthly[1:180,a]
#变量重命名，tm-时间，iron—成品钢材产量，coal—原煤产量，freight-货运量
names(df)<-c("tm","iron","coal","freight") #iron表示成品钢材产量，coal表示原煤产量

  olsRegModel<-lm(freight~iron+coal,data=df)    
  df$linearRegPred<-as.integer(predict(olsRegModel,newdata=df))
  
  rfRegModel<-randomForest(freight~iron+coal,data=df,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  df$frRegPred<-as.integer(predict(rfRegModel,df))     #<-----------随机森林的预测数据已经在这里计算得到
  
  svmRegModel<-svm(freight~iron+coal,data=df,type="eps-regression",cross=dim(df)[1]/2)
  df$svmRegPred<-as.integer(predict(svmRegModel,df))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  len<-length(df$tm)
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
      
      p<-p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)#+geom_point(aes(x=tm,y=linearRegPred),size=4,shape=18,colour="blue",position=position_dodge(width=0.2))
    }
    
    if (input$stat_data) {
      p<-p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    p+ylab("货运量(万吨)")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
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
      p<-p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)#+geom_point(aes(x=tm,y=linearRegPred),size=4,shape=18,colour="blue",position=position_dodge(width=0.2))
    }
    
    if (input$stat_data) {
      p<-p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    p+ylab("货运量(万吨)")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  
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
      p<-p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)
    }
  
    if (input$stat_data) {
      p<-p+geom_point(aes(x=tm,y=freight),color="red",size=3,shape=21)
    }
    p+ylab("货运量(万吨)")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  output$table<-DT::renderDataTable(
    DT::datatable(
      data<-df, 
      colnames = c('日期', '成品钢材产量(万吨)','原煤产量(万吨)','货运量(万吨)','多元回归预测(万吨)','随机森林回归预测(万吨)','支持向量机回归预测(万吨)'),
      rownames = TRUE)
  )
  
  
  
  
  #————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————
  #客运量预测
  #————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————
  
passenger_dataindex<-c(1,2,5,27,29,30,31,32,33)
passagerpre_df<-df_yearly[16:26,passenger_dataindex]
names(passagerpre_df)<-c("Year","railcar","EMU","passager","population","GDP","income","aviation","third_industry")


passagerpre_df$linearRegPred<-0.04*passagerpre_df$GDP+2.76*passagerpre_df$population+
  0.87*passagerpre_df$income+2569.27*passagerpre_df$third_industry+
  0.65*passagerpre_df$aviation+11.27*passagerpre_df$EMU+
  0.78*passagerpre_df$railcar-409634.8
  
  passagerpre_rfRegModel<-randomForest(passager~GDP+population+income+third_industry+aviation+EMU+railcar,
                                       data=passagerpre_df,importance=T, ntree=100,type="regression")   #ranpassagerpre_dfrstReg函数在randomForest.r文件中
  
  passagerpre_df$frRegPred<-as.integer(predict(passagerpre_rfRegModel,passagerpre_df))     #<-----------随机森林的预测数据已经在这里计算得到
  
  passagerpre_svmRegModel<-svm(passager~GDP+population+income+third_industry+aviation+EMU+railcar,
                               data=passagerpre_df,type="eps-regression",cross=dim(passagerpre_df)[1]/2)
  passagerpre_df$svmRegPred<-as.integer(predict(passagerpre_svmRegModel,passagerpre_df))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  len<-length(passagerpre_df$Year)
  plotCurve<-function(db,xdata,ydata)
  {
    len=dim(xdata)[1]
    plt<-ggplot(db,x=c(xdata[1],xdata[len]),aes(x=xdata,y=ydata),color="red")
    return(plt)
  }
  #---------------------------多元回归画线
  output$passagerpre_linearplot <- renderPlot( {
    
    if(input$passagerpre_year_start> input$passagerpre_year_end)  {
      
      if (input$passagerpre_stat_data) {
        p<-plotCurve(passagerpre_df,passagerpre_df$Year,passagerpre_df$passager)
      }
      else
      {
        p<-plotCurve(passagerpre_df,passagerpre_df$Year,passagerpre_df$linearRegPred)
      }
    }
    else{
      passagerpre_dfsub<-subset(passagerpre_df,substr(passagerpre_df$Year,1,4)>=input$passagerpre_year_start) 
      passagerpre_dfsub<-subset(passagerpre_dfsub,substr(passagerpre_dfsub$Year,1,4)<=input$passagerpre_year_end)
      if (input$passagerpre_stat_data) {
        p<-plotCurve(passagerpre_dfsub,passagerpre_dfsub$Year,passagerpre_dfsub$passager)
      }
      else
      {
        p<-plotCurve(passagerpre_dfsub,passagerpre_dfsub$Year,passagerpre_dfsub$linearRegPred)
      }
    }
    
    if(input$passagerpre_predict_data){
      
      p<-p+geom_line(aes(x=Year,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=Year,y=linearRegPred),size=4,shape=18,colour="blue",position=position_dodge(width=0.2))
    }
    
    if (input$passagerpre_stat_data) {
      p<-p+geom_point(aes(x=Year,y=passager),color="red",size=3,shape=21)
    }
    p+ylab("客运量（万人）")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #多元回归预测计算
  output$passagerpre_output<-renderText({
    x1<-as.numeric(input$passagerpre_GDP_input)
    x2<-as.numeric(input$passagerpre_population_input)
    x3<-as.numeric(input$passsagerpre_income_input)
    x4<-as.numeric(input$passagerpre_thirdindustry_input)
    x5<-as.numeric(input$passagerpre_aviation_input)
    x6<-as.numeric(input$passagerpre_EMU_input)
    x7<-as.numeric(input$passagepre_railcar_input)
    
    pred<-0.04*x1+2.76*x2+0.87*x3+2569.27*x4+0.65*x5+11.27*x6+0.78*x7-409634.8
    paste("多元回归预测：",pred ) 
  }
  )
  
  #随机森林回归预测计算
  output$passagerpre_FRR<-renderText({
    x1<-as.numeric(input$passagerpre_GDP_input)
    x2<-as.numeric(input$passagerpre_population_input)
    x3<-as.numeric(input$passsagerpre_income_input)
    x4<-as.numeric(input$passagerpre_thirdindustry_input)
    x5<-as.numeric(input$passagerpre_aviation_input)
    x6<-as.numeric(input$passagerpre_EMU_input)
    x7<-as.numeric(input$passagepre_railcar_input)
    
    GDP<-c(x1)
    population<-c(x2)
    income<-c(x3)
    third_industry<-c(x4)
    aviation<-c(x5)
    EMU<-c(x6)
    railcar<-c(x7)
    
    Year<-c(2015)
    passager<-c(0)
    inputdata<-data.frame(Year,passager,GDP,population,income,third_industry,aviation,EMU,railcar)
    #passagerpre_pred<-as.integer(predict(passagerpre_svmRegModel,inputdata))
    passagerpre_FRR<-predict(passagerpre_rfRegModel,inputdata)   #passagerpre_rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(passagerpre_FRR[1])  ) 
    
  }
  )
  
  #支持向量机回归预测计算
  output$passagerpre_zhi<-renderText({
    x1<-as.numeric(input$passagerpre_GDP_input)
    x2<-as.numeric(input$passagerpre_population_input)
    x3<-as.numeric(input$passsagerpre_income_input)
    x4<-as.numeric(input$passagerpre_thirdindustry_input)
    x5<-as.numeric(input$passagerpre_aviation_input)
    x6<-as.numeric(input$passagerpre_EMU_input)
    x7<-as.numeric(input$passagepre_railcar_input)
    
    GDP<-c(x1)
    population<-c(x2)
    income<-c(x3)
    third_industry<-c(x4)
    aviation<-c(x5)
    EMU<-c(x6)
    railcar<-c(x7)
    
    Year<-c(2015)
    passager<-c(0)
    inputdata<-data.frame(Year,passager,GDP,population,income,third_industry,aviation,EMU,railcar)
    passagerpre_pred<-as.integer(predict(passagerpre_svmRegModel,inputdata))
    
    paste("支持向量机预测：",passagerpre_pred)
    
  }
  )
  
  output$passagerpre_rfplot <- renderPlot( {
    if(input$passagerpre_year_start> input$passagerpre_year_end)  {
     
      if (input$passagerpre_stat_data) {
        p<-plotCurve(passagerpre_df,passagerpre_df$Year,passagerpre_df$passager)
      }
      else
      {

        p<-plotCurve(passagerpre_df,passagerpre_df$Year,passagerpre_df$frRegPred)

      }
    }
    else{
      passagerpre_dfsub<-subset(passagerpre_df,substr(passagerpre_df$Year,1,4)>=input$passagerpre_year_start) 
      passagerpre_dfsub<-subset(passagerpre_dfsub,substr(passagerpre_dfsub$Year,1,4)<=input$passagerpre_year_end)
      if (input$passagerpre_stat_data) {
        p<-plotCurve(passagerpre_dfsub,passagerpre_dfsub$Year,passagerpre_dfsub$passager)
      }
      else
      {
        p<-plotCurve(passagerpre_dfsub,passagerpre_dfsub$Year,passagerpre_dfsub$frRegPred)
      }
    }
    
    if(input$passagerpre_predict_data){
      p<-p+geom_line(aes(x=Year,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=Year,y=frRegPred),size=4,shape=18,colour="blue",position=position_dodge(width=0.2))
    }
    
    if (input$passagerpre_stat_data) {
      p<-p+geom_point(aes(x=Year,y=passager),color="red",size=3,shape=21)
    }
    p+ylab("客运量（万人）")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  
  output$passagerpre_svmplot <- renderPlot( {
    
    if(input$passagerpre_year_start> input$passagerpre_year_end)  {
      
      if (input$passagerpre_stat_data) {
        p<-plotCurve(passagerpre_df,passagerpre_df$Year,passagerpre_df$carriage)
      }
      else
      {
        p<-plotCurve(passagerpre_df,passagerpre_df$Year,passagerpre_df$svmRegPred)
      }
    }
    else{
      passagerpre_dfsub<-subset(passagerpre_df,substr(passagerpre_df$Year,1,4)>=input$passagerpre_year_start) 
      passagerpre_dfsub<-subset(passagerpre_dfsub,substr(passagerpre_dfsub$Year,1,4)<=input$passagerpre_year_end)
      if (input$passagerpre_stat_data) {
        p<-plotCurve(passagerpre_dfsub,passagerpre_dfsub$Year,passagerpre_dfsub$passager)
      }
      else
      {
        p<-plotCurve(passagerpre_dfsub,passagerpre_dfsub$Year,passagerpre_dfsub$svmRegPred)
      }
    }
    
    if(input$passagerpre_predict_data){
      p<-p+geom_line(aes(x=Year,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=Year,y=svmRegPred),size=4,shape=18,colour="blue",position=position_dodge(width=0.2))
    }
    
    if (input$passagerpre_stat_data) {
      p<-p+geom_point(aes(x=Year,y=passager),color="red",size=3,shape=21)
    }
    p+ylab("客运量（万人）")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  
  output$passagerpre_table<-DT::renderDataTable(
    DT::datatable(
      data<-passagerpre_df, 
      colnames = c('日期', '铁路客运量(万人)','国内生产总值(亿元)','年末总人口(万人)','城镇居民家庭人均可支配收入(元)',
                   '第三产业增加值%','民用航空客运量(万人)','动车组数量','客车辆数（辆）',
                   '多元回归预测(万吨)','随机森林回归预测(万吨)','支持向量机回归预测(万吨)'),
      rownames = TRUE)
  )
  #——————————————————————————————————————————————————————————————————————————————
  #——————————————————————————————————————————————————————————————————————————————
  #时间序列预测，包括货运量、工业增加值增长量、铁路固定资产投资、货车车辆数、
  #原煤产量、成品钢材产量、原油加工量常量七个指标的时间序列预测
  #——————————————————————————————————————————————————————————————————————————————
  #——————————————————————————————————————————————————————————————————————————————
  
  #--------------------------------------------
  #-----------货运量时间序列预测---------------
  #freight-----------货运量
  freight_ind<-df_monthly$freight_volume
  freight_indus<-ts(freight_ind,start=c(2001,1),freq=12)
  freight_rn<-auto.arima(freight_indus,ic="bic")
  freight_rn<-arima(freight_indus,order=c(2,1,3),seasonal=c(0,1,2))
  freight_rn2<-forecast(freight_rn,h=12)
  freight_rn3<- data.frame(freight_rn2)
  freight_rn3$forecast<- data.frame(freight_rn2)[1]
  freight_rn3$low80<- data.frame(freight_rn2)[2]
  freight_rn3$upper80<- data.frame(freight_rn2)[3]
  freight_rn3$low90<- data.frame(freight_rn2)[4]
  freight_rn3$upper90<- data.frame(freight_rn2)[5]
  freight_rn4<- data.frame(freight_rn3$forecast,freight_rn3$low80,freight_rn3$upper80,freight_rn3$low90,freight_rn3$upper90)
  
  output$freight_forecast <- renderPlot( {
    freight_p<- plot(freight_rn2,main="货运量（预测未来一年）",ylab="货运量",xlab="年")})
  output$freight_forecast_table<-DT::renderDataTable(
    DT::datatable(
      {freight_data<-freight_rn4},
      colnames = c('货运量',  '80%概率区间下限','80%概率区间上限','95%概率区间下限','95%概率区间上限')
    )
  )
#--------------------------------------------
#-----------客运量时间序列预测---------------
#passenger_volume-----------客运量
passenger_volume_ind<-df_monthly$passenger_volume
passenger_volume_indus<-ts(passenger_volume_ind,start=c(2001,1),freq=12)
passenger_volume_rn<-auto.arima(passenger_volume_indus,ic="bic")
passenger_volume_rn<-arima(passenger_volume_indus,order=c(0,1,1),seasonal=c(0,1,1))
passenger_volume_rn2<-forecast(passenger_volume_rn,h=12)
passenger_volume_rn3<- data.frame(passenger_volume_rn2)
passenger_volume_rn3$forecast<- data.frame(passenger_volume_rn2)[1]
passenger_volume_rn3$low80<- data.frame(passenger_volume_rn2)[2]
passenger_volume_rn3$upper80<- data.frame(passenger_volume_rn2)[3]
passenger_volume_rn3$low90<- data.frame(passenger_volume_rn2)[4]
passenger_volume_rn3$upper90<- data.frame(passenger_volume_rn2)[5]
passenger_volume_rn4<- data.frame(passenger_volume_rn3$forecast,passenger_volume_rn3$low80,passenger_volume_rn3$upper80,passenger_volume_rn3$low90,passenger_volume_rn3$upper90)

output$passenger_volume_forecast <- renderPlot( {
  passenger_volume_p<- plot(passenger_volume_rn2,main="客运量（预测未来一年）",ylab="客运量",xlab="年")})
output$passenger_volume_forecast_table<-DT::renderDataTable(
  DT::datatable(
{passenger_volume_data<-passenger_volume_rn4},
colnames = c('客运量',  '80%概率区间下限','80%概率区间上限','95%概率区间下限','95%概率区间上限')
  )
)

  #-------------------------------------------
  #--------成品钢材产量时间序列预测-----------
  #SteelTime-----------成品钢材产量时间序列预测
  SteelTimeind<-df_monthly$iron_output
  SteelTimeindus<-ts(SteelTimeind,start=c(2001,1),freq=12)
  SteelTimern<-auto.arima(SteelTimeindus,ic="bic")
  SteelTimern<-arima(SteelTimeindus,order=c(2,1,3),seasonal=c(0,1,2))
  SteelTimern2<-forecast(SteelTimern,h=12)
  SteelTimern3<- data.frame(SteelTimern2)
  SteelTimern3$forecast<- data.frame(SteelTimern2)[1]
  SteelTimern3$low80<- data.frame(SteelTimern2)[2]
  SteelTimern3$upper80<- data.frame(SteelTimern2)[3]
  SteelTimern3$low90<- data.frame(SteelTimern2)[4]
  SteelTimern3$upper90<- data.frame(SteelTimern2)[5]
  SteelTimern4<- data.frame(SteelTimern3$forecast,SteelTimern3$low80,SteelTimern3$upper80,SteelTimern3$low90,SteelTimern3$upper90)
  
  output$SteelTime_forecast <- renderPlot( {
    
    SteelTimep<- plot(SteelTimern2,main="成品钢材产量（预测未来一年）",ylab="成品钢材产量",xlab="年")})
  
  
  
  output$SteelTime_forecast_table<-DT::renderDataTable(
    DT::datatable(
      {data<-SteelTimern4},
      colnames = c('成品钢材产量',  '80%概率区间下限','80%概率区间上限','95%概率区间下限','95%概率区间上限')
    )
  )
  
  
  #----------------------------------
  #----货车车辆数时间序列预测--------
  #TRUCKTime-----------货车车辆数时间序列预测
  TruckTimeind<-df_yearly$freight_car
  TruckTimeindus<-ts(TruckTimeind,start=c(1990),freq=1)
  TruckTimern<-auto.arima(TruckTimeindus,ic="bic")
  TruckTimern<-arima(TruckTimeindus,order=c(2,1,3),seasonal=c(0,1,2))
  TruckTimern2<-forecast(TruckTimern,h=1)
  TruckTimern3<- data.frame(TruckTimern2)
  TruckTimern3$forecast<- data.frame(TruckTimern2)[1]
  TruckTimern3$low80<- data.frame(TruckTimern2)[2]
  TruckTimern3$upper80<- data.frame(TruckTimern2)[3]
  TruckTimern3$low90<- data.frame(TruckTimern2)[4]
  TruckTimern3$upper90<- data.frame(TruckTimern2)[5]
  TruckTimern4<- data.frame(TruckTimern3$forecast,TruckTimern3$low80,TruckTimern3$upper80,TruckTimern3$low90,TruckTimern3$upper90)
  
  output$TruckTime_forecast <- renderPlot( {
    
    TruckTimep<- plot(TruckTimern2,main="货车辆数（预测未来一年）",ylab="货车辆数",xlab="年")})
  
  
  
  output$TruckTime_forecast_table<-DT::renderDataTable(
    DT::datatable(
      {data<-TruckTimern4},
      colnames = c('货车辆数',  '80%概率区间下限','80%概率区间上限','95%概率区间下限','95%概率区间上限')
    )
  )
  
  #-----------------------------------
  #-----原煤产量时间序列预测----------
  #CoalTime-----------原煤产量时间序列预测
  CoalTimeind<-df_monthly$coal_output
  CoalTimeindus<-ts(CoalTimeind,start=c(2001,1),freq=12)
  CoalTimern<-auto.arima(CoalTimeindus,ic="bic")
  CoalTimern<-arima(CoalTimeindus,order=c(2,1,3),seasonal=c(0,1,2))
  CoalTimern2<-forecast(CoalTimern,h=12)
  CoalTimern3<- data.frame(CoalTimern2)
  CoalTimern3$forecast<- data.frame(CoalTimern2)[1]
  CoalTimern3$low80<- data.frame(CoalTimern2)[2]
  CoalTimern3$upper80<- data.frame(CoalTimern2)[3]
  CoalTimern3$low90<- data.frame(CoalTimern2)[4]
  CoalTimern3$upper90<- data.frame(CoalTimern2)[5]
  CoalTimern4<- data.frame(CoalTimern3$forecast,CoalTimern3$low80,CoalTimern3$upper80,CoalTimern3$low90,CoalTimern3$upper90)
  
  output$CoalTime_forecast <- renderPlot( {
    
    CoalTimep<- plot(CoalTimern2,main="原煤产量（预测未来一年）",ylab="原煤产量",xlab="年")})
  
  
  
  output$CoalTime_forecast_table<-DT::renderDataTable(
    DT::datatable(
      {data<-CoalTimern4},
      colnames = c('原煤产量',  '80%概率区间下限','80%概率区间上限','95%概率区间下限','95%概率区间上限')
    )
  )
  
  
  #----------------------------------------------
  #-----------原油加工量时间序列预测-------------
  #OilTime-----------原油加工量时间序列预测
  OilTimeind<-df_monthly$oil_processing_volume
  OilTimeindus<-ts(OilTimeind,start=c(2001,1),freq=12)
  OilTimern<-auto.arima(OilTimeindus,ic="bic")
  OilTimern<-arima(OilTimeindus,order=c(2,1,3),seasonal=c(0,1,2))
  OilTimern2<-forecast(OilTimern,h=12)
  OilTimern3<- data.frame(OilTimern2)
  OilTimern3$forecast<- data.frame(OilTimern2)[1]
  OilTimern3$low80<- data.frame(OilTimern2)[2]
  OilTimern3$upper80<- data.frame(OilTimern2)[3]
  OilTimern3$low90<- data.frame(OilTimern2)[4]
  OilTimern3$upper90<- data.frame(OilTimern2)[5]
  OilTimern4<- data.frame(OilTimern3$forecast,OilTimern3$low80,OilTimern3$upper80,OilTimern3$low90,OilTimern3$upper90)
  
  output$OilTime_forecast <- renderPlot( {
    
    p<- plot(OilTimern2,main="原油加工量（预测未来一年）",ylab="原油加工量",xlab="年")})
  
  
  
  output$OilTime_forecast_table<-DT::renderDataTable(
    DT::datatable(
      {data<-OilTimern4},
      colnames = c('原油加工量',  '80%概率区间下限','80%概率区间上限','95%概率区间下限','95%概率区间上限')
    )
  )
  
  
  #-------------------------------------------
  #--------工业增加值增长量时间序列预测---------
  
  dfIndustrial_Added_Value_Rate<-df_monthly$Industrial_Added_Value_Rate
  dfIndustrial_Added_Value_Rate1<-ts(dfIndustrial_Added_Value_Rate,start=c(2001,1),freq=12)
  dfIndustrial_Added_Value_Rate0<-arima(dfIndustrial_Added_Value_Rate1,order=c(1,1,1),seasonal=c(2,0,2))
  dfIndustrial_Added_Value_Rate2<-forecast(dfIndustrial_Added_Value_Rate0,h=12)
  dfIndustrial_Added_Value_Rate3<- data.frame(dfIndustrial_Added_Value_Rate2)
  dfIndustrial_Added_Value_Rate3$forecast<- data.frame(dfIndustrial_Added_Value_Rate2)[1]
  dfIndustrial_Added_Value_Rate3$low80<- data.frame(dfIndustrial_Added_Value_Rate2)[2]
  dfIndustrial_Added_Value_Rate3$upper80<- data.frame(dfIndustrial_Added_Value_Rate2)[3]
  dfIndustrial_Added_Value_Rate3$low95<- data.frame(dfIndustrial_Added_Value_Rate2)[4]
  dfIndustrial_Added_Value_Rate3$upper95<- data.frame(dfIndustrial_Added_Value_Rate2)[5]
  dfIndustrial_Added_Value_Rate4<- data.frame(dfIndustrial_Added_Value_Rate3$forecast,dfIndustrial_Added_Value_Rate3$low80,dfIndustrial_Added_Value_Rate3$upper80,dfIndustrial_Added_Value_Rate3$low95,dfIndustrial_Added_Value_Rate3$upper95)
  
  output$Industrial_Added_Value_Rate_forecast_timesery <- renderPlot( {
    p<- plot(dfIndustrial_Added_Value_Rate2,main="工业增加值增长率（预测未来一年）",ylab="增长率",xlab="年")})
  
  output$Industrial_Added_Value_Rate_forecast_timesery_table<-DT::renderDataTable(
    DT::datatable(
      {data<-dfIndustrial_Added_Value_Rate4},
      colnames = c('工业增加值增长率',  '80%置信区间下限','80%置信区间上限','95%置信区间下限','95%置信区间上限')
    )
  )
  
  
  #---------------------------------------------
  #----------固定资产投资时间序列预测-----------
  dfInvestment_in_Fixed_Assets0<-df_monthly$Investment_in_Fixed_Assets
  dfInvestment_in_Fixed_Assets00<-ts(dfInvestment_in_Fixed_Assets0,start=c(2001,1),freq=12)
  dfInvestment_in_Fixed_Assets<-arima(dfInvestment_in_Fixed_Assets00,order=c(4,1,2),seasonal=c(0,0,1))
  dfInvestment_in_Fixed_Assets2<-forecast(dfInvestment_in_Fixed_Assets,h=12)
  dfInvestment_in_Fixed_Assets3<- data.frame(dfInvestment_in_Fixed_Assets2)
  dfInvestment_in_Fixed_Assets3$forecast<- data.frame(dfInvestment_in_Fixed_Assets2)[1]
  dfInvestment_in_Fixed_Assets3$low80<- data.frame(dfInvestment_in_Fixed_Assets2)[2]
  dfInvestment_in_Fixed_Assets3$upper80<- data.frame(dfInvestment_in_Fixed_Assets2)[3]
  dfInvestment_in_Fixed_Assets3$low95<- data.frame(dfInvestment_in_Fixed_Assets2)[4]
  dfInvestment_in_Fixed_Assets3$upper95<- data.frame(dfInvestment_in_Fixed_Assets2)[5]
  dfInvestment_in_Fixed_Assets4<- data.frame(dfInvestment_in_Fixed_Assets3$forecast,dfInvestment_in_Fixed_Assets3$low80,dfInvestment_in_Fixed_Assets3$upper80,dfInvestment_in_Fixed_Assets3$low95,dfInvestment_in_Fixed_Assets3$upper95)
  
  output$Investment_in_Fixed_Assets_forecast_timesery <- renderPlot( {
    p<- plot(dfInvestment_in_Fixed_Assets2,main="固定资产投资（预测未来一年）",ylab="增长率",xlab="年")})
  
  output$Investment_in_Fixed_Assets_forecast_table_timesery<-DT::renderDataTable(
    DT::datatable(
      {data<-dfInvestment_in_Fixed_Assets4},
      colnames = c('固定资产投资',  '80%置信区间下限','80%置信区间上限','95%置信区间下限','95%置信区间上限')
    )
  )
  
 
  #————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————
  #原始数据显示，查询显示本程序用到的所有原始数据
  #————————————————————————————————————————————————————————————————————————————————————————
  
  #——相关行业——————————————————————————————————————————————————————————————————————————————————————
  #——相关行业——————————————————————————————————————————————————————————————————————————————————————
  output$rawdata_relevant_industry_plot <- renderPlot( {
    dfrawdata<-df_monthly
    dfrawdata$tm<-as.Date.POSIXct(df_monthly$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
    len<-length(dfrawdata$tm)
    
    if(input$year_start_relevant_industry> input$year_end_relevant_industry)  {
      p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
    }
    else{
      dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_relevant_industry) )
      dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_relevant_industry))
      p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
    }
    
    #iron_output_rawdata---------------成品钢材产量(亿吨)
    if(input$relevant_industry_rawdata=="iron_output_rawdata"){
      p<-p+geom_line(aes(x=tm,y=iron_output),color="black",size=0.7)+geom_point(aes(x=tm,y=iron_output),size=2,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #oil_processing_volume_rawdata --------------原油加工量(亿吨) 
    if (input$relevant_industry_rawdata=="oil_processing_volume_rawdata") {
      p<-p+geom_line(aes(x=tm,y=oil_processing_volume),color="red",size=0.6)+geom_point(aes(x=tm,y=oil_processing_volume),size=2,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
      
    }
    #coal_output_rawdata-----------------原煤产量(亿吨)
    if (input$relevant_industry_rawdata=="coal_output_rawdata") {
      p<-p+geom_line(aes(x=tm,y=coal_output),color="blue",size=0.6)+ylim(6000,35000)+geom_point(aes(x=tm,y=coal_output),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #coalfired_power_generation_rawdata----------------火力发电量(亿千瓦时)
    if (input$relevant_industry_rawdata=="coalfired_power_generation_rawdata") {
      p<-p+geom_line(aes(x=tm,y=coalfired_power_generation),color="orange",size=0.6)+ylim(500,4500)+geom_point(aes(x=tm,y=coalfired_power_generation),size=2,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #industrial_added_value_rawdata-----------工业增加值
    if (input$relevant_industry_rawdata=="industrial_added_value_rawdata") {
      p<-p+geom_line(aes(x=tm,y=Industrial_Added_Value_Rate),color="purple",size=0.6)+ylim(3,25)
      p<-p+geom_point(aes(x=tm,y=Industrial_Added_Value_Rate),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    
    p+ylab("相关行业数据")+xlab("时间")+geom_line()
    
  })
  
  #-运量相关原始数据
  output$rawdata_transport_plot <- renderPlot( {
    
    dfrawdata<-df_monthly
    dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
    len<-length(dfrawdata$tm)
    
    
    if(input$year_start_rawdata_transport> input$year_end_rawdata_transport)  {
      p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
    }
    else{
      dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_rawdata_transport) )
      dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_rawdata_transport))
      p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
    }
    
    #freight_rotation_volume_rawdata -----------货运周转量(亿吨公里)
    if(input$transport_rawdata=="freight_rotation_volume_rawdata"){
      p<-p+geom_line(aes(x=tm,y=freight_rotation_volume),color="black",size=0.6)+ylim(800,2300)+geom_point(aes(x=tm,y=freight_rotation_volume),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #freight_volume_rawdata  ------------货运量(万吨)
    if (input$transport_rawdata=="freight_volume_rawdata") {
      p<-p+geom_line(aes(x=tm,y=freight_volume),color="red",size=0.6)+ylim(10000,29000)
      p<-p+geom_point(aes(x=tm,y=freight_volume),size=2,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    }
    
    #passenger_volume_rawdata----------客运量(亿人)
    if (input$transport_rawdata=="passenger_volume_rawdata") {
      p<-p+geom_line(aes(x=tm,y=passenger_volume),color="blue",size=0.6)+ylim(0.5,3)
      p<-p+geom_point(aes(x=tm,y=passenger_volume),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    
    # passenger_person_km_rawdata---------------客运周转量(亿人公里)
    if (input$transport_rawdata=="passenger_person_km_rawdata") {
      p<-p+geom_line(aes(x=tm,y=passenger_person_km),color="purple",size=0.6)
      p<-p+geom_point(aes(x=tm,y=passenger_person_km),size=2,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    
    p+ylab("运量相关")+xlab("时间")+geom_line()
  })
  
  # 运营相关
  output$rawdata_operation_plot <- renderPlot( {
    
    dfrawdata<-df_yearly
    dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
    len<-length(dfrawdata$tm)
    
    
    if(input$year_start_operation> input$year_end_operation)  {
      p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
    }
    else{
      dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_operation) )
      dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_operation))
      p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
    }
    #mileage-----------运营里程
    if(input$operation_rawdata=="mileage_rawdata"){
      p<-p+geom_line(aes(x=tm,y=mileage),color="black",size=0.6)+ylim(60000,120000)
      p<-p+geom_point(aes(x=tm,y=mileage),size=4,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #dailycar_run---------日均运用车
    if (input$operation_rawdata=="dailycar_run_rawdata") {
      p<-p+geom_line(aes(x=tm,y=dailycar_run),color="red",size=0.6)+ylim(300000,680000)
      p<-p+geom_point(aes(x=tm,y=dailycar_run),size=4,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    }
    #dailycar_now--------日均现在车
    if (input$operation_rawdata=="dailycar_now_rawdata") {
      p<-p+geom_line(aes(x=tm,y=dailycar_now),color="purple",size=0.6)+ylim(380000,820000)
      p<-p+geom_point(aes(x=tm,y=dailycar_now),size=4,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #locomotive_mileage_pcar-----------客运机车日车公里
    if (input$operation_rawdata=="locomotive_mileage_pcar_rawdata") {
      p<-p+geom_line(aes(x=tm,y=locomotive_mileage_pcar),color="blue",size=0.6)+ylim(500,1000)
      p<-p+geom_point(aes(x=tm,y=locomotive_mileage_pcar),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #locomotive_mileage_fcar-----------货运机车日车公里
    if (input$operation_rawdata=="locomotive_mileage_fcar_rawdata") {
      p<-p+geom_line(aes(x=tm,y=locomotive_mileage_fcar),color="blue",size=0.6)+ylim(400,600)
      p<-p+geom_point(aes(x=tm,y=locomotive_mileage_fcar),size=4,shape=21,colour="darkgreen",fill="cornsilk",position=position_dodge(width=0.2))
    }
    
    #locomotive_mileage_sum-------------机车总行走里程
    if (input$operation_rawdata=="locomotive_mileage_sum_rawdata") {
      p<-p+geom_line(aes(x=tm,y=locomotive_mileage_sum),color="orange",size=0.6)+ylim(1300,3000)
      p<-p+geom_point(aes(x=tm,y=locomotive_mileage_sum),size=4,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    
    p+ylab("运营相关")+xlab("时间")+geom_line()
  })
  
  #资产相关
  output$rawdata_property_plot <- renderPlot( {
    
    dfrawdata<-df_yearly
    dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
    len<-length(dfrawdata$tm)
    
    
    if(input$year_start_operation> input$year_end_property)  {
      p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
    }
    else{
      dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_property) )
      dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_property))
      p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
    }
    #passenger_car------------客车辆数
    if(input$property_rawdata=="passenger_car_rawdata"){
      p<-p+geom_line(aes(x=tm,y=passenger_car),color="black",size=0.6)+ylim(30000,70000)
      p<-p+geom_point(aes(x=tm,y=passenger_car),size=4,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #freight_car-----------货车辆数
    if (input$property_rawdata=="freight_car_rawdata") {
      p<-p+geom_line(aes(x=tm,y=freight_car),color="red",size=0.6)+ylim(360000,800000)
      p<-p+geom_point(aes(x=tm,y=freight_car),size=4,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    }
    #locomotive_number---------- 机车台数
    if (input$property_rawdata=="locomotive_number_rawdata") {
      p<-p+geom_line(aes(x=tm,y=locomotive_number),color="blue",size=0.6)+ylim(13000,25000)
      p<-p+geom_point(aes(x=tm,y=locomotive_number),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #bullettrain_number------------动车台数
    if(input$property_rawdata=="bullettrain_number_rawdata"){
      p<-p+geom_line(aes(x=tm,y=bullettrain_number),color="purple",size=0.6)+ylim(0,2300)
      p<-p+geom_point(aes(x=tm,y=bullettrain_number),size=4,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #practitioner_number------------从业人员数量
    if (input$property_rawdata=="practitioner_number_rawdata") {
      p<-p+geom_line(aes(x=tm,y=practitioner_number),color="orange",size=0.6)+ylim(180,320)
      p<-p+geom_point(aes(x=tm,y=practitioner_number),size=4,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #fixed_assets_investment------------铁路固定资产投资
    if (input$property_rawdata=="fixed_assets_investment_rawdata") {
      p<-p+geom_line(aes(x=tm,y=fixed_assets_investment),color="darkgreen",size=0.6)+ylim(150,10000)
      p<-p+geom_point(aes(x=tm,y=fixed_assets_investment),size=4,shape=21,colour="darkgreen",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #newline_tracklaying_mileage-----------新线铺轨里程
    if (input$property_rawdata=="newline_tracklaying_mileage_rawdata") {
      p<-p+geom_line(aes(x=tm,y=newline_tracklaying_mileage),color="red",size=0.6)
      p<-p+geom_point(aes(x=tm,y=newline_tracklaying_mileage),size=4,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    }
    #oldline_tracklaying_mileage------------复线铺轨里程
    if (input$property_rawdata=="oldline_tracklaying_mileage_rawdata") {
      p<-p+geom_line(aes(x=tm,y=oldline_tracklaying_mileage),color="brown",size=0.6)
      p<-p+geom_point(aes(x=tm,y=oldline_tracklaying_mileage),size=4,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    }
    p+ylab("规模相关")+xlab("时间")+geom_line()
  })  
  
  #----黑货运量原始数据---------------
  output$rawdata_black_plot <- renderPlot( {
    
    dfrawdata<-subset(df_monthly,(substr(df_monthly$tm,1,4)>=2008))
    dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
    len<-length(dfrawdata$tm)
    
    if(input$year_start_black_rawdata>input$year_end_black_rawdata)  { 
      p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0)) }
    
    else{
      dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_black_rawdata) )
      dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_black_rawdata))
      p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))   }
    
    #metal------------金属矿石
    if (input$black_rawdata=="metal_rawdata") {
      p<-p+geom_line(aes(x=tm,y=metal),color="red",size=0.6)+ylim(2000,4000)
      p<-p+geom_point(aes(x=tm,y=metal),size=4,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    }
    #mine---------矿建
    if (input$black_rawdata=="mine_rawdata") {
      p<-p+geom_line(aes(x=tm,y=mine),color="red",size=0.6)+ylim(300,1600)
      p<-p+geom_point(aes(x=tm,y=mine),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    
    #iron------钢材的运量月度
    if (input$black_rawdata=="iron_rawdata") {
      p<-p+geom_line(aes(x=tm,y=iron),color="blue",size=0.6)+ylim(1200,2200)
      p<-p+geom_point(aes(x=tm,y=iron),size=4,shape=21,colour="darkblue",fill="blue",position=position_dodge(width=0.2))}
    
    #oil----------石油的运量月度
    if (input$black_rawdata=="oil_rawdata") {
      p<-p+geom_line(aes(x=tm,y=oil),color="brown",size=0.6)+ylim(900,1200)
      p<-p+geom_point(aes(x=tm,y=oil),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    
    #coal--------煤的运量月度
    if (input$black_rawdata=="coal_rawdata") {
      p<-p+geom_line(aes(x=tm,y=coal),color="blue",size=0.6)+ylim(9000,16000)
      p<-p+geom_point(aes(x=tm,y=coal),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    
    p+ylab("黑货运量")+xlab("时间")+geom_line()
  })  
  
  #----白货运量原始数据---------------
  output$rawdata_white_plot <- renderPlot( {
    
    dfrawdata<-subset(df_monthly,(substr(df_monthly$tm,1,4)>=2008))
    dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
    len<-length(dfrawdata$tm)
    
    if(input$year_start_white_rawdata> input$year_end_white_rawdata)  { 
      p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0)) }
    
    else{
      dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_white_rawdata) )
      dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_white_rawdata))
      p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))   }
    
    #machinery--------------工业机械
    if(input$white_rawdata=="machinery_rawdata"){
      p<-p+geom_line(aes(x=tm,y=machinery),color="black",size=0.6)+ylim(20,55)
      p<-p+geom_point(aes(x=tm,y=machinery),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #electronic--------------电子电器
    if (input$white_rawdata=="electronic_rawdata") {
      p<-p+geom_line(aes(x=tm,y=electronic),color="red",size=0.6)+geom_point(aes(x=tm,y=electronic),size=4,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
      
    }
    #agricultural-------------农副产品
    if (input$white_rawdata=="agricultural_rawdata") {
      p<-p+geom_line(aes(x=tm,y=agricultural),color="blue",size=0.6)
      p<-p+geom_point(aes(x=tm,y=agricultural),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #food_tobacco------------饮食烟草
    if (input$white_rawdata=="food_tobacco_rawdata") {
      p<-p+geom_line(aes(x=tm,y=food_tobacco),color="orange",size=0.6)+ylim(70,230)
      p<-p+geom_point(aes(x=tm,y=food_tobacco),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #education------------文教用品
    if (input$white_rawdata=="education_rawdata") {
      p<-p+geom_line(aes(x=tm,y=education),color="darkgreen",size=0.6)+ylim(25,65)
      p<-p+geom_point(aes(x=tm,y=education),size=4,shape=21,colour="darkblue",fill="lightgreen",position=position_dodge(width=0.2))
    }
    #ltl-------------零担
    if (input$white_rawdata=="ltl_rawdata") {
      p<-p+geom_line(aes(x=tm,y=ltl),color="red",size=0.6)
      p<-p+geom_point(aes(x=tm,y=ltl),size=4,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    }
    #container--------------集装箱
    if (input$white_rawdata=="container_rawdata") {
      p<-p+geom_line(aes(x=tm,y=container),color="brown",size=0.6)+ylim(300,1000)
      p<-p+geom_point(aes(x=tm,y=container),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    
    p+ylab("白货运量")+xlab("时间")+geom_line()
  })  

  
  #其他行业
  output$rawdata_relevant_industry_table<-DT::renderDataTable(
    DT::datatable(
      {
        dfrawdata<-df_monthly
        dfrawdata<-data.frame(dfrawdata[1:6])
        data<-dfrawdata},
      colnames = c('时间','成品钢材产量（万吨）','原煤产量（万吨）','原油加工量（万吨）','火力发电量（亿千瓦时）','工业增加值（增长率）'),
      rownames = TRUE))
  
  #rawdata_transport-----------原始数据/运量相关
  output$rawdata_transport_table<-DT::renderDataTable(
    DT::datatable(
      {  
        dfrawdata<-df_monthly
        dfrawdata<-data.frame(dfrawdata$tm,dfrawdata[8:11])
        data<-dfrawdata},
      colnames = c('时间','货运量（万吨）','货运周转量（亿吨公里）','客运量（亿人）','客运周转量（亿人公里）'),
      rownames = TRUE))
  
  output$rawdata_operation_table<-DT::renderDataTable(
    DT::datatable(
      {  
        dfrawdata<-df_yearly
        dfrawdata<-data.frame(dfrawdata$tm,dfrawdata[13:18])
        data<-dfrawdata},
      colnames = c('时间','营业里程（km）','日均运用车（辆）','日均现在车（辆）','客运机车日车公里（km）','货运机车日车公里（km）','机车总行走里程（百万km）'),
      rownames = TRUE))
  
  #rawdata_property-------原始数据/资产相关
  output$rawdata_property_table<-DT::renderDataTable(
    DT::datatable(
      {  
        dfrawdata<-df_yearly
        dfrawdata<-data.frame(dfrawdata[1:9])
        data<-dfrawdata},
      colnames = c('时间','客车辆数(辆)','机车台数(辆)','货车辆数(辆)','动车组数(辆)', '铁路固定资产投资(亿元)','从业人员数量(万人)','新线铺轨里程(km)','复线铺轨里程(km))'),
      rownames = TRUE))
  
  output$rawdata_black_table<-DT::renderDataTable(
    DT::datatable(
      {  
        dfrawdata<-subset(df_monthly,(substr(df_monthly$tm,1,4)>=2008))
        dfrawdata<-data.frame(dfrawdata$tm,dfrawdata[19:23])
        data<-dfrawdata},
      colnames = c('时间','金属矿石(万吨)','矿建(万吨)','钢材(万吨)', '石油(万吨)','煤(万吨)'),
      rownames = TRUE))
  
  output$rawdata_white_table<-DT::renderDataTable(
    DT::datatable(
      {  
        dfrawdata<-subset(df_monthly,(substr(df_monthly$tm,1,4)>=2008))
        dfrawdata<-data.frame(dfrawdata$tm,dfrawdata[12:18])
        data<-dfrawdata},
      colnames = c('时间','工业机械(万吨)','电子电气(万吨)','农副产品(万吨)', '饮食烟草(万吨)','文教用品(万吨)','零担(吨)','集装箱(万吨)'),
      rownames = TRUE))

output$yssj.xghy.table<-DT::renderDataTable(
  DT::datatable(
{
  dfyssj<-read.csv("compidx-qitahangye.csv",head=T)
  data<-dfyssj},
colnames = c('时间','成品钢材产量（亿吨）','原油加工量（亿吨）','原煤产量（亿吨）','火力发电量（亿千瓦时）','工业增加值（增长率）'),
rownames = TRUE))


output$yssj.ylxg.table<-DT::renderDataTable(
  DT::datatable(
{  
  dfyssj<-read.csv("compidx-yunliang.csv",head=T)
  data<-dfyssj},
colnames = c('时间','货运量（亿吨）','货运周转量（亿吨）','客运量（亿人）','客运周转量（亿人）'),
rownames = TRUE))

output$yssj.yyxg.table<-DT::renderDataTable(
  DT::datatable(
{  
  dfyssj<-read.csv("compidx-yunying.csv",head=T)
  data<-dfyssj},
colnames = c('时间','营业里程（km）','日均运用车（万辆）','日均现在车（万辆）','客运机车日车公里（km）','货运机车日车公里（km）','机车总行走里程（1000km）'),
rownames = TRUE))

#yssj.zcxg-------原始数据/资产相关
output$yssj.zcxg.table<-DT::renderDataTable(
  DT::datatable(
{  
  dfyssj<-read.csv("compidx-zichan.csv",head=T)
  data<-dfyssj},
colnames = c('时间','客车辆数(辆)','货车辆数(万辆)','机车台数(辆)','动车台数(辆)', '铁路固定资产投资(亿元)','从业人员数量(万人)','新线铺轨里程(km)','复线铺轨里程(km))'),
rownames = TRUE))

output$yssj.heihuo.table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfyssj<-read.csv("compidx-heihuobaihuo.csv",head=T)
      dfyssj<-data.frame(dfyssj[1],dfyssj[9:13])
      data<-dfyssj},
    colnames = c('时间','金属矿石(万吨)','矿建(万吨)','钢材(万吨)', '石油(万吨)','煤(万吨)'),
    rownames = TRUE))

output$yssj.baihuo.table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfyssj<-read.csv("compidx-heihuobaihuo.csv",head=T)
      dfyssj<-data.frame(dfyssj[1:8])
      data<-dfyssj},
    colnames = c('时间','工业机械(万吨)','电子电气(万吨)','农副产品(万吨)', '饮食烟草(万吨)','文教用品(万吨)','零担(吨)','集装箱(万吨)'),
    rownames = TRUE))
#-------------------mashaomeng---START-----------------------------------------------------------------
#——--------------------里程相关——————————————————————————————————————————————————————————————————————————————————————
output$rawdata_relevant_mileage_plot <- renderPlot( {
  
  dfrawdata<-mengmeng_yearly
  dfrawdata$tm<-as.Date.POSIXct(mengmeng_yearly$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_mileage> input$year_end_mileage)  {
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_mileage) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_mileage))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  
  
  if(input$total_railway_mileage){
    p<-p+geom_line(aes(x=tm,y=total_railway_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=total_railway_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$national_railway1){
    p<-p+geom_line(aes(x=tm,y=national_railway1),color="red",size=0.7)+geom_point(aes(x=tm,y=national_railway1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$joint_investment_railway1){
    p<-p+geom_line(aes(x=tm,y=joint_investment_railway1),color="orange",size=0.7)+geom_point(aes(x=tm,y=joint_investment_railway1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$local_railway1){
    p<-p+geom_line(aes(x=tm,y=local_railway1),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$total_railway_extended_mileage){
    p<-p+geom_line(aes(x=tm,y=total_railway_extended_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=total_railway_extended_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$national_railway2){
    p<-p+geom_line(aes(x=tm,y=national_railway2),color="red",size=0.7)+geom_point(aes(x=tm,y=national_railway2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$joint_investment_railway2){
    p<-p+geom_line(aes(x=tm,y=joint_investment_railway2),color="orange",size=0.7)+geom_point(aes(x=tm,y=joint_investment_railway2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$local_railway2){
    p<-p+geom_line(aes(x=tm,y=local_railway2),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$total_railway_main_track_extended_mileage){
    p<-p+geom_line(aes(x=tm,y=total_railway_main_track_extended_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=total_railway_main_track_extended_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$national_railway3){
    p<-p+geom_line(aes(x=tm,y=national_railway3),color="red",size=0.7)+geom_point(aes(x=tm,y=national_railway3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$joint_investment_railway3){
    p<-p+geom_line(aes(x=tm,y=joint_investment_railway3),color="orange",size=0.7)+geom_point(aes(x=tm,y=joint_investment_railway3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$local_railway3){
    p<-p+geom_line(aes(x=tm,y=local_railway3),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$total_railway_double_track_mileage){
    p<-p+geom_line(aes(x=tm,y=total_railway_double_track_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=total_railway_double_track_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$national_railway4){
    p<-p+geom_line(aes(x=tm,y=national_railway4),color="red",size=0.7)+geom_point(aes(x=tm,y=national_railway4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$joint_investment_railway4){
    p<-p+geom_line(aes(x=tm,y=joint_investment_railway4),color="orange",size=0.7)+geom_point(aes(x=tm,y=joint_investment_railway4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$local_railway4){
    p<-p+geom_line(aes(x=tm,y=local_railway4),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$total_railway_electric_mileage){
    p<-p+geom_line(aes(x=tm,y=total_railway_electric_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=total_railway_electric_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$national_railway5){
    p<-p+geom_line(aes(x=tm,y=national_railway5),color="red",size=0.7)+geom_point(aes(x=tm,y=national_railway5),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$double_track5){
    p<-p+geom_line(aes(x=tm,y=double_track5),color="orange",size=0.7)+geom_point(aes(x=tm,y=double_track5),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$joint_investment_railway5){
    p<-p+geom_line(aes(x=tm,y=joint_investment_railway5),color="purple",size=0.7)+geom_point(aes(x=tm,y=joint_investment_railway5),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$local_railway5){
    p<-p+geom_line(aes(x=tm,y=local_railway5),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway5),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$national_railway_diesel_mileage){
    p<-p+geom_line(aes(x=tm,y=national_railway_diesel_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=national_railway_diesel_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  p+ylab("铁路营业里程")+xlab("时间")+geom_line()
  
})
output$rawdata_relevant_mileage_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-mengmeng_yearly
      dfrawdata<-data.frame(dfrawdata[1:23])
      data<-dfrawdata},
    colnames = c('时间','全国铁路营业里程（公里）','国家铁路','合资铁路','地方铁路',
                 '全国铁路延展里程（公里）', '国家铁路','合资铁路','地方铁路',
                 '全国铁路正线延展里程（公里）', '国家铁路','合资铁路','地方铁路',
                 '全国铁路复线里程（公里）','国家铁路','合资铁路','地方铁路',
                 '全国铁路电气化里程（公里）','国家铁路','复线','合资铁路','地方铁路',
                 '国家铁路内燃化里程（公里）'),
    rownames = TRUE))
#——--------------------分地区营业里程——————————————————————————————————————————————————————————————————————————————————————
output$sub_regional_mileage_yearly_plot <- renderPlot( {
  
  dfrawdata<-mengmeng2_yearly
  dfrawdata$tm<-as.Date.POSIXct(mengmeng2_yearly$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_regional_mileage> input$year_end_regional_mileage)  {
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_regional_mileage) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_regional_mileage))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  
  
  if(input$total){
    p<-p+geom_line(aes(x=tm,y=total),color="black",size=0.7)+geom_point(aes(x=tm,y=total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$beijing){
    p<-p+geom_line(aes(x=tm,y=beijing),color="red",size=0.7)+geom_point(aes(x=tm,y=beijing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$tianjin){
    p<-p+geom_line(aes(x=tm,y=tianjin),color="orange",size=0.7)+geom_point(aes(x=tm,y=tianjin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$hebei){
    p<-p+geom_line(aes(x=tm,y=hebei),color="green",size=0.7)+geom_point(aes(x=tm,y=hebei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$shanxi1){
    p<-p+geom_line(aes(x=tm,y=shanxi1),color="blue",size=0.7)+geom_point(aes(x=tm,y=shanxi1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$neimenggu){
    p<-p+geom_line(aes(x=tm,y=neimenggu),color="black",size=0.7)+geom_point(aes(x=tm,y=neimenggu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$liaoning){
    p<-p+geom_line(aes(x=tm,y=liaoning),color="red",size=0.7)+geom_point(aes(x=tm,y=liaoning),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$jilin){
    p<-p+geom_line(aes(x=tm,y=jilin),color="black",size=0.7)+geom_point(aes(x=tm,y=jilin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$heilongjiang){
    p<-p+geom_line(aes(x=tm,y=heilongjiang),color="blue",size=0.7)+geom_point(aes(x=tm,y=heilongjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$shanghai){
    p<-p+geom_line(aes(x=tm,y=shanghai),color="red",size=0.7)+geom_point(aes(x=tm,y=shanghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$jiangsu){
    p<-p+geom_line(aes(x=tm,y=jiangsu),color="orange",size=0.7)+geom_point(aes(x=tm,y=jiangsu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$zhejiang){
    p<-p+geom_line(aes(x=tm,y=zhejiang),color="green",size=0.7)+geom_point(aes(x=tm,y=zhejiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$anhui){
    p<-p+geom_line(aes(x=tm,y=anhui),color="black",size=0.7)+geom_point(aes(x=tm,y=anhui),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$fujian){
    p<-p+geom_line(aes(x=tm,y=fujian),color="gray",size=0.7)+geom_point(aes(x=tm,y=fujian),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$jiangxi){
    p<-p+geom_line(aes(x=tm,y=jiangxi),color="blue",size=0.7)+geom_point(aes(x=tm,y=jiangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$shandong){
    p<-p+geom_line(aes(x=tm,y=shandong),color="purple",size=0.7)+geom_point(aes(x=tm,y=shandong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$henan){
    p<-p+geom_line(aes(x=tm,y=henan),color="black",size=0.7)+geom_point(aes(x=tm,y=henan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$hunan){
    p<-p+geom_line(aes(x=tm,y=hunan),color="red",size=0.7)+geom_point(aes(x=tm,y=hunan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$hubei){
    p<-p+geom_line(aes(x=tm,y=hubei),color="blue",size=0.7)+geom_point(aes(x=tm,y=hubei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$guangdong){
    p<-p+geom_line(aes(x=tm,y=guangdong),color="black",size=0.7)+geom_point(aes(x=tm,y=guangdong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$guangxi){
    p<-p+geom_line(aes(x=tm,y=guangxi),color="red",size=0.7)+geom_point(aes(x=tm,y=guangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$hainan){
    p<-p+geom_line(aes(x=tm,y=hainan),color="blue",size=0.7)+geom_point(aes(x=tm,y=hainan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$sichuan){
    p<-p+geom_line(aes(x=tm,y=sichuan),color="black",size=0.7)+geom_point(aes(x=tm,y=sichuan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$guizhou){
    p<-p+geom_line(aes(x=tm,y=guizhou),color="red",size=0.7)+geom_point(aes(x=tm,y=guizhou),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$yunnan){
    p<-p+geom_line(aes(x=tm,y=yunnan),color="blue",size=0.7)+geom_point(aes(x=tm,y=yunnan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$guangdong){
    p<-p+geom_line(aes(x=tm,y=guangdong),color="purple",size=0.7)+geom_point(aes(x=tm,y=guangdong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$xizang){
    p<-p+geom_line(aes(x=tm,y=xizang),color="black",size=0.7)+geom_point(aes(x=tm,y=xizang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$shanxi2){
    p<-p+geom_line(aes(x=tm,y=shanxi2),color="black",size=0.7)+geom_point(aes(x=tm,y=shanxi2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$gansu){
    p<-p+geom_line(aes(x=tm,y=gansu),color="red",size=0.7)+geom_point(aes(x=tm,y=gansu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$qinghai){
    p<-p+geom_line(aes(x=tm,y=qinghai),color="blue",size=0.7)+geom_point(aes(x=tm,y=qinghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$ningxia){
    p<-p+geom_line(aes(x=tm,y=ningxia),color="green",size=0.7)+geom_point(aes(x=tm,y=ningxia),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$xinjiang){
    p<-p+geom_line(aes(x=tm,y=xinjiang),color="purple",size=0.7)+geom_point(aes(x=tm,y=xinjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  p+ylab("全国铁路分地区营业里程")+xlab("时间")+geom_line()
  
})
output$sub_regional_mileage_yearly_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-mengmeng2_yearly
      dfrawdata<-data.frame(dfrawdata[1:33])
      data<-dfrawdata},
    colnames = c('时间','北京','天津','河北','山西',
                 '内蒙古', '辽宁','吉林','黑龙江',
                 '上海', '江苏','浙江','安徽',
                 '福建','江西','山东','河南',
                 '湖北','湖南','广东','广西','海南',
                 '重庆','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆'),
    rownames = TRUE)
  
)   
#——-------------------机车拥有量——————————————————————————————————————————————————————————————————————————————————————
output$rawdata_Locomotive_ownership_plot <- renderPlot( {
  
  dfrawdata<- mengmeng1_yearly
  dfrawdata$tm<-as.Date.POSIXct( mengmeng1_yearly$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_locomotive> input$year_end_locomotive)  {
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_locomotive) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_locomotive))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  
  
  if(input$total_railway_locomotive){
    p<-p+geom_line(aes(x=tm,y=total_railway_locomotive),color="black",size=0.7)+geom_point(aes(x=tm,y=total_railway_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$diesel_locomotive1){
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive1),color="red",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$electic_locomotive1){
    p<-p+geom_line(aes(x=tm,y=electic_locomotive1),color="blue",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$steam_locomotive1){
    p<-p+geom_line(aes(x=tm,y=steam_locomotive1),color="purple",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$national_railway_locomotive){
    p<-p+geom_line(aes(x=tm,y=national_railway_locomotive),color="black",size=0.7)+geom_point(aes(x=tm,y=national_railway_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$diesel_locomotive2){
    p<-p+geom_line(aes(x=tm,y=nrjc2),color="red",size=0.7)+geom_point(aes(x=tm,y=nrjc2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$electic_locomotive2){
    p<-p+geom_line(aes(x=tm,y=dljc2),color="blue",size=0.7)+geom_point(aes(x=tm,y=dljc2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$steam_locomotive2){
    p<-p+geom_line(aes(x=tm,y=zqjc2),color="purple",size=0.7)+geom_point(aes(x=tm,y=zqjc2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$joint_investment_locomotive){
    p<-p+geom_line(aes(x=tm,y=joint_investment_locomotive),color="black",size=0.7)+geom_point(aes(x=tm,y=joint_investment_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$diesel_locomotive3){
    p<-p+geom_line(aes(x=tm,y=nrjc3),color="red",size=0.7)+geom_point(aes(x=tm,y=nrjc3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$electic_locomotive3){
    p<-p+geom_line(aes(x=tm,y=dljc3),color="blue",size=0.7)+geom_point(aes(x=tm,y=dljc3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$steam_locomotive3){
    p<-p+geom_line(aes(x=tm,y=zqjc3),color="purple",size=0.7)+geom_point(aes(x=tm,y=zqjc3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$local_railway_locomotive){
    p<-p+geom_line(aes(x=tm,y=local_railway_locomotive),color="black",size=0.7)+geom_point(aes(x=tm,y=local_railway_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$diesel_locomotive4){
    p<-p+geom_line(aes(x=tm,y=nrjc4),color="red",size=0.7)+geom_point(aes(x=tm,y=nrjc4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$electic_locomotive4){
    p<-p+geom_line(aes(x=tm,y=dljc4),color="blue",size=0.7)+geom_point(aes(x=tm,y=dljc4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$steam_locomotive4){
    p<-p+geom_line(aes(x=tm,y=zqjc4),color="purple",size=0.7)+geom_point(aes(x=tm,y=zqjc4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  
  p+ylab("机车拥有量")+xlab("时间")+geom_line()
  
})
output$rawdata_Locomotive_ownership_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-mengmeng1_yearly
      dfrawdata<-data.frame(dfrawdata[1:17])
      data<-dfrawdata},
    colnames = c('时间','全国铁路机车（台）','内燃机','电力机','蒸汽机',
                 '国家铁路机车（台）','内燃机车','电力机车','蒸汽机车','合资铁路机车（台）',
                 '内燃机','电力机','蒸汽机','地方铁路机车（台）','内燃机','电力机', '蒸汽机'),
    rownames = TRUE)
)


#——--------------------分型号机车拥有量——————————————————————————————————————————————————————————————————————————————————————

output$rawdata_model_Locomotive_ownership_plot <- renderPlot( {
  dfrawdata<-mengmeng3_yearly
  dfrawdata$tm<-as.Date.POSIXct(mengmeng3_yearly$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_model_locomotive> input$year_end_model_locomotive)  {
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_model_locomotive) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_model_locomotive))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  
  if(input$total){
    p<-p+geom_line(aes(x=tm,y=total),color="red",size=0.7)+geom_point(aes(x=tm,y=total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$diesel_locomotive){
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive),color="black",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$dongfeng4){
    p<-p+geom_line(aes(x=tm,y=dongfeng4),color="black",size=0.7)+geom_point(aes(x=tm,y=dongfeng4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$dongfeng6){
    p<-p+geom_line(aes(x=tm,y=dongfeng6),color="black",size=0.7)+geom_point(aes(x=tm,y=dongfeng6),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$dongfeng7){
    p<-p+geom_line(aes(x=tm,y=dongfeng7),color="black",size=0.7)+geom_point(aes(x=tm,y=dongfeng7),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$dongfeng8){
    p<-p+geom_line(aes(x=tm,y=dongfeng8),color="black",size=0.7)+geom_point(aes(x=tm,y=dongfeng8),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$dongfeng10){
    p<-p+geom_line(aes(x=tm,y=dongfeng10),color="black",size=0.7)+geom_point(aes(x=tm,y=dongfeng10),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$dongfeng11){
    p<-p+geom_line(aes(x=tm,y=dongfeng11),color="black",size=0.7)+geom_point(aes(x=tm,y=dongfeng11),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$dongfeng12){
    p<-p+geom_line(aes(x=tm,y=dongfeng12),color="black",size=0.7)+geom_point(aes(x=tm,y=dongfeng12),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$beijing){
    p<-p+geom_line(aes(x=tm,y=beijing),color="black",size=0.7)+geom_point(aes(x=tm,y=beijing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$nd5){
    p<-p+geom_line(aes(x=tm,y=nd5),color="black",size=0.7)+geom_point(aes(x=tm,y=nd5),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$ny6){
    p<-p+geom_line(aes(x=tm,y=ny6),color="black",size=0.7)+geom_point(aes(x=tm,y=ny6),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$ny7){
    p<-p+geom_line(aes(x=tm,y=ny7),color="black",size=0.7)+geom_point(aes(x=tm,y=ny7),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$qt1){
    p<-p+geom_line(aes(x=tm,y=qt1),color="black",size=0.7)+geom_point(aes(x=tm,y=qt1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$electric_locomotive){
    p<-p+geom_line(aes(x=tm,y=electric_locomotive),color="blue",size=0.7)+geom_point(aes(x=tm,y=electric_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$shaoshan1){
    p<-p+geom_line(aes(x=tm,y=shaoshan1),color="black",size=0.7)+geom_point(aes(x=tm,y=shaoshan1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$shaoshan3){
    p<-p+geom_line(aes(x=tm,y=shaoshan3),color="black",size=0.7)+geom_point(aes(x=tm,y=shaoshan3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$shaoshan4){
    p<-p+geom_line(aes(x=tm,y=shaoshan4),color="black",size=0.7)+geom_point(aes(x=tm,y=shaoshan4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$shaoshan6){
    p<-p+geom_line(aes(x=tm,y=shaoshan6),color="black",size=0.7)+geom_point(aes(x=tm,y=shaoshan6),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$shaoshan7){
    p<-p+geom_line(aes(x=tm,y=shaoshan7),color="black",size=0.7)+geom_point(aes(x=tm,y=shaoshan7),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$shaoshan8){
    p<-p+geom_line(aes(x=tm,y=shaoshan8),color="black",size=0.7)+geom_point(aes(x=tm,y=shaoshan8),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$shaoshan9){
    p<-p+geom_line(aes(x=tm,y=shaoshan9),color="black",size=0.7)+geom_point(aes(x=tm,y=shaoshan9),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$g6){
    p<-p+geom_line(aes(x=tm,y=g6),color="black",size=0.7)+geom_point(aes(x=tm,y=g6),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$g8){
    p<-p+geom_line(aes(x=tm,y=g8),color="black",size=0.7)+geom_point(aes(x=tm,y=g8),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$k6){
    p<-p+geom_line(aes(x=tm,y=k6),color="black",size=0.7)+geom_point(aes(x=tm,y=k6),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$k8){
    p<-p+geom_line(aes(x=tm,y=k8),color="black",size=0.7)+geom_point(aes(x=tm,y=k8),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$hxd1){
    p<-p+geom_line(aes(x=tm,y=hxd1),color="black",size=0.7)+geom_point(aes(x=tm,y=hxd1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$hxd2){
    p<-p+geom_line(aes(x=tm,y=hxd2),color="black",size=0.7)+geom_point(aes(x=tm,y=hxd2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$hxd3){
    p<-p+geom_line(aes(x=tm,y=hxd3),color="black",size=0.7)+geom_point(aes(x=tm,y=hxd3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$qt2){
    p<-p+geom_line(aes(x=tm,y=qt2),color="black",size=0.7)+geom_point(aes(x=tm,y=qt2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$steam_locomotive){
    p<-p+geom_line(aes(x=tm,y=steam_locomotive),color="orange",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$renmin){
    p<-p+geom_line(aes(x=tm,y=renmin),color="black",size=0.7)+geom_point(aes(x=tm,y=renmin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$shengli){
    p<-p+geom_line(aes(x=tm,y=shengli),color="black",size=0.7)+geom_point(aes(x=tm,y=shengli),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$jianshe){
    p<-p+geom_line(aes(x=tm,y=jianshe),color="black",size=0.7)+geom_point(aes(x=tm,y=jianshe),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$qianjin){
    p<-p+geom_line(aes(x=tm,y=qianjin),color="black",size=0.7)+geom_point(aes(x=tm,y=qianjin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$jiefang){
    p<-p+geom_line(aes(x=tm,y=jiefang),color="black",size=0.7)+geom_point(aes(x=tm,y=jiefang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$kd7){
    p<-p+geom_line(aes(x=tm,y=kd7),color="black",size=0.7)+geom_point(aes(x=tm,y=kd7),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  if(input$qt3){
    p<-p+geom_line(aes(x=tm,y=qt3),color="black",size=0.7)+geom_point(aes(x=tm,y=qt3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  p+ylab("分机型机车量")+xlab("时间")+geom_line()
})
output$rawdata_model_Locomotive_ownership_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-mengmeng3_yearly
      dfrawdata<-data.frame(dfrawdata[1:39])
      data<-dfrawdata},
    colnames = c('时间','总计','内燃机车','东风4','东风6','东风7','东风8','东风10','东风11','东风12',
                 '北京','ND5','NY6','NY7','其他','电力机车', '韶山1', '韶山3', '韶山4', '韶山6', 
                 '韶山7', '韶山8', '韶山9','6G','8G','6K','8K','HXD1','HXD2','HXD3','其他',
                 '蒸汽机车', '人民','胜利','建设','前进','解放','KD7','其他'),
    rownames = TRUE)
)
#--------fancongcong--start------
#全国客车货车拥有量
output$holding_plot <- renderPlot( {
  
  dfrawdata<-fcc_holding
  dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_holding> input$year_end_holding)  { 
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0)) }
  
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_holding) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_holding))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))   }
  
 #electronic--------------国家铁路客车拥有量

if (input$national_railway_passenger) {
  p<-p+geom_line(aes(x=tm,y=national_railway_passenger),color="red",size=0.7)+geom_point(aes(x=tm,y=national_railway_passenger),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  
}
#electronic--------------合资铁路客车拥有量
if (input$joint_investment_railway_passenger) {
  p<-p+geom_line(aes(x=tm,y=joint_investment_railway_passenger),color="blue",size=0.7)+geom_point(aes(x=tm,y=joint_investment_railway_passenger),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  
}
#electronic--------------地方铁路客车拥有量
if (input$local_railway_passenger) {
  p<-p+geom_line(aes(x=tm,y=local_railway_passenger),color="orange",size=0.7)+geom_point(aes(x=tm,y=local_railway_passenger),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  
}

#electronic--------------客车合计拥有量
if (input$passenger_car_total) {
  p<-p+geom_line(aes(x=tm,y=passenger_car_total),color="black",size=0.7)+geom_point(aes(x=tm,y=passenger_car_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  
}
#electronic--------------国家铁路货车拥有量
if (input$national_railway_feight) {
  p<-p+geom_line(aes(x=tm,y=national_railway_feight),color="red",size=0.7)+geom_point(aes(x=tm,y=national_railway_feight),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  
}
#electronic--------------合资铁路货车拥有量
if (input$joint_investment_railway_feight) {
  p<-p+geom_line(aes(x=tm,y=joint_investment_railway_feight),color="blue",size=0.7)+geom_point(aes(x=tm,y=joint_investment_railway_feight),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  
}
#electronic--------------地方铁路货车拥有量  
if (input$local_railway_feight) {
  p<-p+geom_line(aes(x=tm,y=local_railway_feight),color="orange",size=0.7)+geom_point(aes(x=tm,y=local_railway_feight),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  
}
#electronic--------------货车合计拥有量
if (input$freight_car_total) {
  p<-p+geom_line(aes(x=tm,y=freight_car_total),color="black",size=0.7)+geom_point(aes(x=tm,y=freight_car_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  
}


p+ylab("国家铁路客车拥有量")+xlab("时间")+geom_line()

})

output$holding_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-fcc_holding
      dfrawdata<-data.frame(dfrawdata[1:9])
      data<-dfrawdata},
    colnames = c('时间','全国客车总拥有量(辆)','国家铁路客车拥有量(辆)','合资铁路客车拥有量(辆)','地方铁路客车拥有量(辆)', 
                 '全国货车总拥有量(辆)','国家铁路货车拥有量(辆)','合资铁路货车拥有量(辆)','地方铁路货车拥有量(辆)'),
    rownames = TRUE))
#机车客车货车利用率
output$availability_plot <- renderPlot( {
  
  dfrawdata<-fcc_availability
  dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_availablity> input$year_end_availablity)  { 
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0)) }
  
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_availablity) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_availablity))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))   }
  
  
  #electronic--------------平均每台机车换算周转量
  if (input$availablity=="locomotive_rotation_average") {
    p<-p+geom_line(aes(x=tm,y=locomotive_rotation_average),color="red",size=0.7)+geom_point(aes(x=tm,y=locomotive_rotation_average),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------平均每台机车总重吨公里
  if (input$availablity=="locomotive_weight_average") {
    p<-p+geom_line(aes(x=tm,y=locomotive_weight_average),color="red",size=0.7)+geom_point(aes(x=tm,y=locomotive_weight_average),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------每百营业公里拥有机车台数
  if (input$availablity=="locomotive_ownership_volume") {
    p<-p+geom_line(aes(x=tm,y=locomotive_ownership_volume),color="blue",size=0.7)+geom_point(aes(x=tm,y=locomotive_ownership_volume),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------平均每辆客车客运量
  
  if (input$availablity=="passenger_car_volume_average") {
    p<-p+geom_line(aes(x=tm,y=passenger_car_volume_average),color="red",size=0.7)+geom_point(aes(x=tm,y=passenger_car_volume_average),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------平均每辆客车客运周转量
  if (input$availablity=="passenger_rotation_average") {
    p<-p+geom_line(aes(x=tm,y=passenger_rotation_average),color="red",size=0.7)+geom_point(aes(x=tm,y=passenger_rotation_average),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------每百营业公里拥有客车辆数
  if (input$availablity=="passenger_car_ownership_number") {
    p<-p+geom_line(aes(x=tm,y=passenger_car_ownership_number),color="blue",size=0.7)+geom_point(aes(x=tm,y=passenger_car_ownership_number),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------平均每辆货车完成货运量
  if (input$availablity=="freight_car_volume_average") {
    p<-p+geom_line(aes(x=tm,y=freight_car_volume_average),color="red",size=0.7)+geom_point(aes(x=tm,y=freight_car_volume_average),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------平均每辆货车完成货运周转量
  if (input$availablity=="feight_rotation_average") {
    p<-p+geom_line(aes(x=tm,y=feight_rotation_average),color="red",size=0.7)+geom_point(aes(x=tm,y=feight_rotation_average),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------每百营业公里拥有货车数
  if (input$availablity=="freight_car_ownership_number") {
    p<-p+geom_line(aes(x=tm,y=mbyyglyyhcs),color="purple",size=0.7)+geom_point(aes(x=tm,y=freight_car_ownership_number),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  p+ylab("机车客车货车利用率")+xlab("时间")+geom_line()
  
})  

output$availability_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-fcc_availability
      dfrawdata<-data.frame(dfrawdata[1:10])
      data<-dfrawdata},
    colnames = c('时间','平均每台机车换算周转量(万换算吨公里/台)','平均每台机车总重吨公里(万吨公里/台)','每百营业公里拥有机车台数(台/百公里)', 
                 '平均每辆客车客运量(万人/辆)','平均每辆客车客运周转量(万人公里/辆)','每百营业公里拥有客车辆数(辆/百公里)',
                 '平均每辆货车完成货运量(吨/辆)','平均每辆货车完成货运周转量(万吨公里/辆)','每百营业公里拥有货车数(辆/百公里)'),
    rownames = TRUE))


#国家铁路客车拥有量（分座次）
output$kecheholding_plot <- renderPlot( {
  
  dfrawdata<-fcc_kecheholding
  dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_kecheholding> input$year_end_kecheholding)  { 
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0)) }
  
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_kecheholding) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_kecheholding))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))   }
  
  
  #electronic--------------软卧
  if (input$soft_sleeper) {
    p<-p+geom_line(aes(x=tm,y=soft_sleeper),color="red",size=0.7)+geom_point(aes(x=tm,y=soft_sleeper),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------硬卧
  if (input$hard_sleeper) {
    p<-p+geom_line(aes(x=tm,y=hard_sleeper),color="black",size=0.7)+geom_point(aes(x=tm,y=hard_sleeper),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------软硬卧
  if (input$sleeper) {
    p<-p+geom_line(aes(x=tm,y=sleeper),color="blue",size=0.7)+geom_point(aes(x=tm,y=sleeper),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  
  #electronic--------------软座
  if (input$soft_seat) {
    p<-p+geom_line(aes(x=tm,y=soft_seat),color="orange",size=0.7)+geom_point(aes(x=tm,y=soft_seat),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------硬座
  if (input$hard_seat) {
    p<-p+geom_line(aes(x=tm,y=hard_seat),color="purple",size=0.7)+geom_point(aes(x=tm,y=hard_seat),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------软硬座
  if (input$seat) {
    p<-p+geom_line(aes(x=tm,y=seat),color="red",size=0.7)+geom_point(aes(x=tm,y=seat),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------餐车 
  if (input$restaurant_car) {
    p<-p+geom_line(aes(x=tm,y=restaurant_car),color="black",size=0.7)+geom_point(aes(x=tm,y=restaurant_car),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------行李
  if (input$package) {
    p<-p+geom_line(aes(x=tm,y=package),color="blue",size=0.7)+geom_point(aes(x=tm,y=package),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------公务
  if (input$business) {
    p<-p+geom_line(aes(x=tm,y=business),color="orange",size=0.7)+geom_point(aes(x=tm,y=business),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------其他
  if (input$other_car) {
    p<-p+geom_line(aes(x=tm,y=other_car),color="purple",size=0.7)+geom_point(aes(x=tm,y=other_car),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------空调车
  if (input$air_conditiona_car) {
    p<-p+geom_line(aes(x=tm,y=air_conditiona_car),color="red",size=0.7)+geom_point(aes(x=tm,y=air_conditiona_car),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  
  p+ylab("国家铁路客车拥有量")+xlab("时间")+geom_line()
  
})

output$kecheholding_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-fcc_kecheholding
      dfrawdata<-data.frame(dfrawdata[1:15])
      data<-dfrawdata},
    colnames = c('时间','客车辆数合计(辆)','软卧车厢(节)','硬卧车厢(节)', '软硬卧车厢(节)','软座车厢(节)','硬座车厢(节)',
                 '软硬座车厢(节)','餐车车厢(节)','行李车厢(节)','公务车厢(节)','其他车厢(节)',
                 '空调车(辆)','座位（万个）','卧铺（万个）'),
    rownames = TRUE))

#国家铁路分车型货车拥有量
output$huocheholding_plot <- renderPlot( {
  
  dfrawdata<-fcc_huocheholding
  dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_huocheholding> input$year_end_huocheholding)  { 
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0)) }
  
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_huocheholding) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_huocheholding))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))   }
  
  #electronic--------------棚车
  if (input$box_car) {
    p<-p+geom_line(aes(x=tm,y=box_car),color="red",size=0.7)+geom_point(aes(x=tm,y=box_car),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------敞车
  if (input$open_car) {
    p<-p+geom_line(aes(x=tm,y=open_car),color="purple",size=0.7)+geom_point(aes(x=tm,y=open_car),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------平车
  if (input$flat_car) {
    p<-p+geom_line(aes(x=tm,y=flat_car),color="blue",size=0.7)+geom_point(aes(x=tm,y=flat_car),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------毒品车
  if (input$poison_car) {
    p<-p+geom_line(aes(x=tm,y=poison_car),color="orange",size=0.7)+geom_point(aes(x=tm,y=poison_car),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------罐车合计
  if (input$tank_car) {
    p<-p+geom_line(aes(x=tm,y=tank_car),color="black",size=0.7)+geom_point(aes(x=tm,y=tank_car),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------轻油罐车
  if (input$light_oil) {
    p<-p+geom_line(aes(x=tm,y=light_oil),color="red",size=0.7)+geom_point(aes(x=tm,y=light_oil),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------粘油罐车
  if (input$viscous_oil) {
    p<-p+geom_line(aes(x=tm,y=viscous_oil),color="blue",size=0.7)+geom_point(aes(x=tm,y=viscous_oil),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------其他罐车
  if (input$other_oil_car) {
    p<-p+geom_line(aes(x=tm,y=other_oil_car),color="purple",size=0.7)+geom_point(aes(x=tm,y=other_oil_car),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------冷藏车
  if (input$refeigerator_car) {
    p<-p+geom_line(aes(x=tm,y=refeigerator_car),color="orange",size=0.7)+geom_point(aes(x=tm,y=refeigerator_car),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------其他货车
  if (input$other_car) {
    p<-p+geom_line(aes(x=tm,y=other_car),color="red",size=0.7)+geom_point(aes(x=tm,y=other_car),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------货车合计
  if (input$freight_car_total) {
    p<-p+geom_line(aes(x=tm,y=freight_car_total),color="black",size=0.7)+geom_point(aes(x=tm,y=freight_car_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  
  
  
  p+ylab("国家铁路货车拥有量")+xlab("时间")+geom_line()
  
})  

output$huocheholding_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-fcc_huocheholding
      dfrawdata<-data.frame(dfrawdata[1:12])
      data<-dfrawdata},
    colnames = c('时间','货车辆数合计(辆)','棚车(辆)','敞车(辆)', '平车(辆)','毒品车(辆)','罐车总数(辆)',
                 '清油罐车(辆)','粘油罐车(辆)','其他罐车(辆)','冷藏车(辆)','其他(辆)'),
    rownames =TRUE))

#铁路车站主要客货运设施、设备
output$sheshishebei_plot <- renderPlot( {
  
  dfrawdata<-fcc_sheshishebei
  dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_sheshishebei> input$year_end_sheshishebei)  { 
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0)) }
  
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_sheshishebei) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_sheshishebei))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))   }
  
  #electronic--------------办理客运业务车站
  if (input$sheshishebei=="passenger_service_station") {
    p<-p+geom_line(aes(x=tm,y=passenger_service_station),color="red",size=0.7)+geom_point(aes(x=tm,y=passenger_service_station),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------候车室数量
  if (input$sheshishebei=="waiting_room") {
    p<-p+geom_line(aes(x=tm,y=waiting_room),color="blue",size=0.7)+geom_point(aes(x=tm,y=waiting_room),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------有行包房车站 
  if (input$sheshishebei=="luggage_parcel_station") {
    p<-p+geom_line(aes(x=tm,y=luggage_parcel_station),color="orange",size=0.7)+geom_point(aes(x=tm,y=luggage_parcel_station),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------有雨棚站台
  if (input$sheshishebei=="rainshed_platform") {
    p<-p+geom_line(aes(x=tm,y=rainshed_platform),color="purple",size=0.7)+geom_point(aes(x=tm,y=rainshed_platform),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------办理客运乘降的处所
  if (input$sheshishebei=="passenger_take_down_position") {
    p<-p+geom_line(aes(x=tm,y=passenger_take_down_position),color="red",size=0.7)+geom_point(aes(x=tm,y=passenger_take_down_position),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electronic--------------货物仓库数量
  if (input$sheshishebei=="freight_warehouse_number") {
    p<-p+geom_line(aes(x=tm,y=freight_warehouse_number),color="blue",size=0.7)+geom_point(aes(x=tm,y=freight_warehouse_number),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------货物仓库建筑面积
  if (input$sheshishebei=="freight_warehouse_area") {
    p<-p+geom_line(aes(x=tm,y=freight_warehouse_area),color="orange",size=0.7)+geom_point(aes(x=tm,y=freight_warehouse_area),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------货物雨棚数量
  if (input$sheshishebei=="freight_rainshed_number") {
    p<-p+geom_line(aes(x=tm,y=freight_rainshed_number),color="blue",size=0.7)+geom_point(aes(x=tm,y=freight_rainshed_number),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------货物雨棚建筑面积
  if (input$sheshishebei=="freight_rainshed_area") {
    p<-p+geom_line(aes(x=tm,y=freight_rainshed_area),color="orange",size=0.7)+geom_point(aes(x=tm,y=freight_rainshed_area),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------货物站台数量
  if (input$sheshishebei=="freight_platform_number") {
    p<-p+geom_line(aes(x=tm,y=freight_platform_number),color="blue",size=0.7)+geom_point(aes(x=tm,y=freight_platform_number),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------货物站台建筑面积
  if (input$sheshishebei=="freight_platform_area") {
    p<-p+geom_line(aes(x=tm,y=freight_platform_area),color="orange",size=0.7)+geom_point(aes(x=tm,y=freight_platform_area),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------装卸线数量
  if (input$sheshishebei=="load_unload_line_number") {
    p<-p+geom_line(aes(x=tm,y=load_unload_line_number),color="blue",size=0.7)+geom_point(aes(x=tm,y=load_unload_line_number),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------装卸线有效长
  if (input$sheshishebei=="load_unload_line_effective_lenghth") {
    p<-p+geom_line(aes(x=tm,y=load_unload_line_effective_lenghth),color="orange",size=0.7)+geom_point(aes(x=tm,y=load_unload_line_effective_lenghth),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    
  }
  #electronic--------------装卸线全长
  if (input$sheshishebei=="load_unload_line_lenghth") {
    p<-p+geom_line(aes(x=tm,y=load_unload_line_lenghth),color="red",size=0.7)+geom_point(aes(x=tm,y=load_unload_line_lenghth),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  } 
  
  p+ylab("铁路车站主要客货运设施设备")+xlab("时间")+geom_line()
  
})   

output$sheshishebei_table<-DT::renderDataTable(
  DT::datatable(
    {  
      dfrawdata<-fcc_sheshishebei
      dfrawdata<-data.frame(dfrawdata[1:15])
      data<-dfrawdata},
    colnames = c('时间','办理客运业务车站(个)','候车室数量(个)','有行包房车站(个)', '有雨棚站台(个)','办理客运乘降的处所(个)','货物仓库数量(座)',
                 '货物仓库建筑面积(平米)','货物雨棚数量(座)','货物雨棚建筑面积(平米)','货物站台数量(座)','货物站台建筑面积(平米)',
                 '装卸线数量(条)','装卸线全长(米)','装卸线装卸线有效长(米)'),
    rownames =TRUE))


    
    #-------------------景钊
    output$rawdata_yslzzl_plot<-renderPlot({
  dfrawdata<-df_yslzzl_yearly
  dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata$tm)
  
  if(input$year_start_yslzzl> input$year_end_yslzzl)  {
    p<-ggplot(dfrawdata,x=c(dfrawdata$tm[1],dfrawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub<-subset(dfrawdata,(substr(dfrawdata$tm,1,4)>=input$year_start_yslzzl) )
    dfrawdatasub<-subset(dfrawdatasub,(substr(dfrawdatasub$tm,1,4)<=input$year_end_yslzzl))
    p<-ggplot(dfrawdatasub,x=c(dfrawdatasub$tm[1],dfrawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  #national_passenger_volume_rawdata---------------全国铁路客运量(万人)
  if(input$yslzzl_rawdata=="national_passenger_volume_rawdata"){
    p<-p+geom_line(aes(x=tm,y=national_passenger_volume),color="black",size=0.7)+geom_point(aes(x=tm,y=national_passenger_volume),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #national_passenger_turnover_volume_rawdata --------------全国铁路客运周转量(亿人公里) 
  if(input$yslzzl_rawdata=="national_passenger_turnover_volume_rawdata") {
    p<-p+geom_line(aes(x=tm,y=national_passenger_turnover_volume),color="red",size=0.7)+geom_point(aes(x=tm,y=national_passenger_turnover_volume),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  }
  #national_freight_volume_rawdata-----------------全国铁路货运量(万吨)
  if(input$yslzzl_rawdata=="national_freight_volume_rawdata") {
    p<-p+geom_line(aes(x=tm,y=national_freight_volume),color="blue",size=0.7)+geom_point(aes(x=tm,y=national_freight_volume),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #national_freight_turnover_volume_rawdata----------------全国铁路货运周转量(亿吨公里)
  if(input$yslzzl_rawdata=="national_freight_turnover_volume_rawdata") {
    p<-p+geom_line(aes(x=tm,y=national_freight_turnover_volume),color="orange",size=0.7)+geom_point(aes(x=tm,y=national_freight_turnover_volume),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #national_parcel_volume_rawdata-----------全国铁路行包运量(万吨)
  if(input$yslzzl_rawdata=="national_parcel_volume_rawdata") {
    p<-p+geom_line(aes(x=tm,y=national_parcel_volume),color="purple",size=0.7)+geom_point(aes(x=tm,y=national_parcel_volume),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #national_parcel_turnover_volume_rawdata-----------全国铁路行包周转量(亿吨公里)
  if(input$yslzzl_rawdata=="national_parcel_turnover_volume_rawdata") {
    p<-p+geom_line(aes(x=tm,y=national_parcel_turnover_volume),color="red",size=0.7)+geom_point(aes(x=tm,y=national_parcel_turnover_volume),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #passenger_average_mileage------------全国旅客平均里程(公里)
  if(input$yslzzl_rawdata=="national_passenger_average_mileage") {
    p<-p+geom_line(aes(x=tm,y=national_passenger_average_mileage),color="blue",size=0.7)+geom_point(aes(x=tm,y=national_passenger_average_mileage),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  p+ylab("运输量-周转量数据")+xlab("时间")
})
#-------------------------景钊--------------------------
#rawdata_yslzzl-----------原始数据/运输量-周转量-jingzhao
output$rawdata_yslzzl_table<-DT::renderDataTable(
  DT::datatable(
    dfrawdata<-df_yslzzl_yearly,
    colnames = c('时间','全国铁路客运量(万人)','全国铁路客运周转量(亿人公里)','全国铁路货运量(万吨)','全国铁路货运周转量(亿吨公里)','全国铁路行包运量(万吨)','全国铁路行包周转量(亿吨公里)','全国旅客平均里程(公里)')
    ))
#—————————————————————————————运输量—周转量 景钊  END———————————————————————————————

#------李雪妍4_16国家铁路省、市、自治区货物运量
output$hyl_plot <- renderPlot( {
  lxy1rawdata<-lxy1_yearly
  lxy1rawdata$tm<-as.Date.POSIXct(lxy1rawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(lxy1rawdata$tm)
  
  if(input$year_start_hyl> input$year_end_hyl)  {
    p<-ggplot(lxy1rawdata,x=c(lxy1rawdata$tm[1],lxy1rawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    lxy1rawdatasub<-subset(lxy1rawdata,(substr(lxy1rawdata$tm,1,4)>=input$year_start_hyl) )
    lxy1rawdatasub<-subset(lxy1rawdatasub,(substr(lxy1rawdatasub$tm,1,4)<=input$year_end_hyl))
    p<-ggplot(lxy1rawdatasub,x=c(lxy1rawdatasub$tm[1],lxy1rawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  
  #zj---------------总计
  if(input$zj1){
    p<-p+geom_line(aes(x=tm,y=mileage_total),color="black",size=0.7)+geom_point(aes(x=tm,y=mileage_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #beijing---------------北京
  if(input$beijing1){
    p<-p+geom_line(aes(x=tm,y=beijing),color="red",size=0.7)+geom_point(aes(x=tm,y=beijing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #tianjin---------------天津
  if(input$tianjin1){
    p<-p+geom_line(aes(x=tm,y=tianjin),color="blue",size=0.7)+geom_point(aes(x=tm,y=tianjin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hebei---------------河北
  if(input$hebei1){
    p<-p+geom_line(aes(x=tm,y=hebei),color="orange",size=0.7)+geom_point(aes(x=tm,y=hebei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanxi1---------------山西
  if(input$shanxi11){
    p<-p+geom_line(aes(x=tm,y=shanxi1),color="purple",size=0.7)+geom_point(aes(x=tm,y=shanxi1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #neimenggu---------------内蒙古
  if(input$neimenggu1){
    p<-p+geom_line(aes(x=tm,y=neimenggu),color="brown",size=0.7)+geom_point(aes(x=tm,y=neimenggu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #liaoning---------------辽宁
  if(input$liaoning1){
    p<-p+geom_line(aes(x=tm,y=liaoning),color="red",size=0.7)+geom_point(aes(x=tm,y=liaoning),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jilin---------------吉林
  if(input$jilin1){
    p<-p+geom_line(aes(x=tm,y=jilin),color="blue",size=0.7)+geom_point(aes(x=tm,y=jilin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #heilongjiang---------------黑龙江
  if(input$heilongjiang1){
    p<-p+geom_line(aes(x=tm,y=heilongjiang),color="purple",size=0.7)+geom_point(aes(x=tm,y=heilongjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanghai---------------上海
  if(input$shanghai1){
    p<-p+geom_line(aes(x=tm,y=shanghai),color="red",size=0.7)+geom_point(aes(x=tm,y=shanghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jiangsu---------------江苏
  if(input$jiangsu1){
    p<-p+geom_line(aes(x=tm,y=jiangsu),color="blue",size=0.7)+geom_point(aes(x=tm,y=jiangsu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #zhejiang---------------浙江
  if(input$zhejiang1){
    p<-p+geom_line(aes(x=tm,y=zhejiang),color="purple",size=0.7)+geom_point(aes(x=tm,y=zhejiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #anhui---------------安徽
  if(input$anhui1){
    p<-p+geom_line(aes(x=tm,y=anhui),color="orange",size=0.7)+geom_point(aes(x=tm,y=anhui),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #fujian---------------福建
  if(input$fujian1){
    p<-p+geom_line(aes(x=tm,y=fujian),color="brown",size=0.7)+geom_point(aes(x=tm,y=fujian),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jiangxi---------------江西
  if(input$jiangxi1){
    p<-p+geom_line(aes(x=tm,y=jiangxi),color="black",size=0.7)+geom_point(aes(x=tm,y=jiangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shandong---------------山东
  if(input$shandong1){
    p<-p+geom_line(aes(x=tm,y=shandong),color="green",size=0.7)+geom_point(aes(x=tm,y=shandong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #henan---------------河南
  if(input$henan1){
    p<-p+geom_line(aes(x=tm,y=henan),color="blue",size=0.7)+geom_point(aes(x=tm,y=henan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hubei---------------湖北
  if(input$hubei1){
    p<-p+geom_line(aes(x=tm,y=hubei),color="brown",size=0.7)+geom_point(aes(x=tm,y=hubei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hunan---------------湖南
  if(input$hunan1){
    p<-p+geom_line(aes(x=tm,y=hunan),color="purple",size=0.7)+geom_point(aes(x=tm,y=hunan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guangdong---------------广东
  if(input$guangdong1){
    p<-p+geom_line(aes(x=tm,y=guangdong),color="red",size=0.7)+geom_point(aes(x=tm,y=guangdong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guangxi---------------广西
  if(input$guangxi1){
    p<-p+geom_line(aes(x=tm,y=guangxi),color="black",size=0.7)+geom_point(aes(x=tm,y=guangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hainan---------------海南
  if(input$hainan1){
    p<-p+geom_line(aes(x=tm,y=hainan),color="blue",size=0.7)+geom_point(aes(x=tm,y=hainan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #chongqing---------------重庆
  if(input$chongqing1){
    p<-p+geom_line(aes(x=tm,y=chongqing),color="black",size=0.7)+geom_point(aes(x=tm,y=chongqing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #sichuan---------------四川
  if(input$sichuan1){
    p<-p+geom_line(aes(x=tm,y=sichuan),color="red",size=0.7)+geom_point(aes(x=tm,y=sichuan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guizhou---------------贵州
  if(input$guizhou1){
    p<-p+geom_line(aes(x=tm,y=guizhou),color="blue",size=0.7)+geom_point(aes(x=tm,y=guizhou),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #yunnan---------------云南
  if(input$yunnan1){
    p<-p+geom_line(aes(x=tm,y=yunnan),color="purple",size=0.7)+geom_point(aes(x=tm,y=yunnan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #xizang---------------西藏
  if(input$xizang1){
    p<-p+geom_line(aes(x=tm,y=xizang),color="brown",size=0.7)+geom_point(aes(x=tm,y=xizang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanxi2---------------陕西
  if(input$shanxi21){
    p<-p+geom_line(aes(x=tm,y=shanxi2),color="orange",size=0.7)+geom_point(aes(x=tm,y=shanxi2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #gansu---------------甘肃
  if(input$gansu1){
    p<-p+geom_line(aes(x=tm,y=gansu),color="black",size=0.7)+geom_point(aes(x=tm,y=gansu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #qinghai---------------青海
  if(input$qinghai1){
    p<-p+geom_line(aes(x=tm,y=qinghai),color="red",size=0.7)+geom_point(aes(x=tm,y=qinghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #ningxia---------------宁夏
  if(input$ningxia1){
    p<-p+geom_line(aes(x=tm,y=ningxia),color="blue",size=0.7)+geom_point(aes(x=tm,y=ningxia),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #xinjiang---------------新疆
  if(input$xinjiang1){
    p<-p+geom_line(aes(x=tm,y=xinjiang),color="purple",size=0.7)+geom_point(aes(x=tm,y=xinjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  p+ylab("国家铁路省、市、自治区货物运量")+xlab("时间")+geom_line()
  
})
#4_16hyl-------国家铁路省、市、自治区货运量表输出lixueyan
output$hyl_table<-DT::renderDataTable(
  DT::datatable(
    {  
      lxy1rawdata<-lxy1_yearly
      lxy1rawdata<-data.frame(lxy1rawdata[1:33])
      data<-lxy1rawdata},
    colnames = c('年度','合计','北京','天津','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海','江苏','浙江','安徽','福建','江西','山东','河南','湖北','湖南','广东','广西','海南','重庆','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆'),
    rownames = TRUE))
#------李雪妍4_17国家铁路省、市、自治区货运周转量
output$hyzzl_plot <- renderPlot( {
  lxy2rawdata<-lxy2_yearly
  lxy2rawdata$tm<-as.Date.POSIXct(lxy2rawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(lxy2rawdata$tm)
  
  if(input$year_start_hyzzl> input$year_end_hyzzl)  {
    p<-ggplot(lxy2rawdata,x=c(lxy2rawdata$tm[1],lxy2rawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    lxy2rawdatasub<-subset(lxy2rawdata,(substr(lxy2rawdata$tm,1,4)>=input$year_start_hyzzl) )
    lxy2rawdatasub<-subset(lxy2rawdatasub,(substr(lxy2rawdatasub$tm,1,4)<=input$year_end_hyzzl))
    p<-ggplot(lxy2rawdatasub,x=c(lxy2rawdatasub$tm[1],lxy2rawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  
  #zj---------------总计
  if(input$zj2){
    p<-p+geom_line(aes(x=tm,y=mileage_total),color="black",size=0.7)+geom_point(aes(x=tm,y=mileage_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #beijing---------------北京
  if(input$beijing2){
    p<-p+geom_line(aes(x=tm,y=beijing),color="red",size=0.7)+geom_point(aes(x=tm,y=beijing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #tianjin---------------天津
  if(input$tianjin2){
    p<-p+geom_line(aes(x=tm,y=tianjin),color="blue",size=0.7)+geom_point(aes(x=tm,y=tianjin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hebei---------------河北
  if(input$hebei2){
    p<-p+geom_line(aes(x=tm,y=hebei),color="brown",size=0.7)+geom_point(aes(x=tm,y=hebei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanxi1---------------山西
  if(input$shanxi12){
    p<-p+geom_line(aes(x=tm,y=shanxi1),color="purple",size=0.7)+geom_point(aes(x=tm,y=shanxi1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #neimenggu---------------内蒙古
  if(input$neimenggu2){
    p<-p+geom_line(aes(x=tm,y=neimenggu),color="black",size=0.7)+geom_point(aes(x=tm,y=neimenggu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #liaoning---------------辽宁
  if(input$liaoning2){
    p<-p+geom_line(aes(x=tm,y=liaoning),color="red",size=0.7)+geom_point(aes(x=tm,y=liaoning),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jilin---------------吉林
  if(input$jilin2){
    p<-p+geom_line(aes(x=tm,y=jilin),color="blue",size=0.7)+geom_point(aes(x=tm,y=jilin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #heilongjiang---------------黑龙江
  if(input$heilongjiang2){
    p<-p+geom_line(aes(x=tm,y=heilongjiang),color="black",size=0.7)+geom_point(aes(x=tm,y=heilongjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanghai---------------上海
  if(input$shanghai2){
    p<-p+geom_line(aes(x=tm,y=shanghai),color="red",size=0.7)+geom_point(aes(x=tm,y=shanghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jiangsu---------------江苏
  if(input$jiangsu2){
    p<-p+geom_line(aes(x=tm,y=jiangsu),color="blue",size=0.7)+geom_point(aes(x=tm,y=jiangsu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #zhejiang---------------浙江
  if(input$zhejiang2){
    p<-p+geom_line(aes(x=tm,y=zhejiang),color="black",size=0.7)+geom_point(aes(x=tm,y=zhejiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #anhui---------------安徽
  if(input$anhui2){
    p<-p+geom_line(aes(x=tm,y=anhui),color="brown",size=0.7)+geom_point(aes(x=tm,y=anhui),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #fujian---------------福建
  if(input$fujian2){
    p<-p+geom_line(aes(x=tm,y=fujian),color="purple",size=0.7)+geom_point(aes(x=tm,y=fujian),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jiangxi---------------江西
  if(input$jiangxi2){
    p<-p+geom_line(aes(x=tm,y=jiangxi),color="green",size=0.7)+geom_point(aes(x=tm,y=jiangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shandong---------------山东
  if(input$shandong2){
    p<-p+geom_line(aes(x=tm,y=shandong),color="red",size=0.7)+geom_point(aes(x=tm,y=shandong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #henan---------------河南
  if(input$henan2){
    p<-p+geom_line(aes(x=tm,y=henan),color="blue",size=0.7)+geom_point(aes(x=tm,y=henan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hubei---------------湖北
  if(input$hubei2){
    p<-p+geom_line(aes(x=tm,y=hubei),color="purple",size=0.7)+geom_point(aes(x=tm,y=hubei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hunan---------------湖南
  if(input$hunan2){
    p<-p+geom_line(aes(x=tm,y=hunan),color="orange",size=0.7)+geom_point(aes(x=tm,y=hunan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guangdong---------------广东
  if(input$guangdong2){
    p<-p+geom_line(aes(x=tm,y=guangdong),color="black",size=0.7)+geom_point(aes(x=tm,y=guangdong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guangxi---------------广西
  if(input$guangxi2){
    p<-p+geom_line(aes(x=tm,y=guangxi),color="red",size=0.7)+geom_point(aes(x=tm,y=guangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hainan---------------海南
  if(input$hainan2){
    p<-p+geom_line(aes(x=tm,y=hainan),color="blue",size=0.7)+geom_point(aes(x=tm,y=hainan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #chongqing---------------重庆
  if(input$chongqing2){
    p<-p+geom_line(aes(x=tm,y=chongqing),color="purple",size=0.7)+geom_point(aes(x=tm,y=chongqing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #sichuan---------------四川
  if(input$sichuan2){
    p<-p+geom_line(aes(x=tm,y=sichuan),color="red",size=0.7)+geom_point(aes(x=tm,y=sichuan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guizhou---------------贵州
  if(input$guizhou2){
    p<-p+geom_line(aes(x=tm,y=guizhou),color="blue",size=0.7)+geom_point(aes(x=tm,y=guizhou),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #yunnan---------------云南
  if(input$yunnan2){
    p<-p+geom_line(aes(x=tm,y=yunnan),color="black",size=0.7)+geom_point(aes(x=tm,y=yunnan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #xizang---------------西藏
  if(input$xizang2){
    p<-p+geom_line(aes(x=tm,y=xizang),color="brown",size=0.7)+geom_point(aes(x=tm,y=xizang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanxi2---------------陕西
  if(input$shanxi22){
    p<-p+geom_line(aes(x=tm,y=shanxi2),color="black",size=0.7)+geom_point(aes(x=tm,y=shanxi2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #gansu---------------甘肃
  if(input$gansu2){
    p<-p+geom_line(aes(x=tm,y=gansu),color="red",size=0.7)+geom_point(aes(x=tm,y=gansu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #qinghai---------------青海
  if(input$qinghai2){
    p<-p+geom_line(aes(x=tm,y=qinghai),color="blue",size=0.7)+geom_point(aes(x=tm,y=qinghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #ningxia---------------宁夏
  if(input$ningxia2){
    p<-p+geom_line(aes(x=tm,y=ningxia),color="purple",size=0.7)+geom_point(aes(x=tm,y=ningxia),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #xinjiang---------------新疆
  if(input$xinjiang2){
    p<-p+geom_line(aes(x=tm,y=xinjiang),color="brown",size=0.7)+geom_point(aes(x=tm,y=xinjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  p+ylab("国家铁路省、市、自治区货运周转量")+xlab("时间")+geom_line()
  
})
#4_17hyzzl-------国家铁路省、市、自治区货运周转量表输出lixueyan
output$hyzzl_table<-DT::renderDataTable(
  DT::datatable(
    {  
      lxy2rawdata<-lxy2_yearly
      lxy2rawdata<-data.frame(lxy2rawdata[1:33])
      data<-lxy2rawdata},
    colnames = c('年度','合计','北京','天津','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海','江苏','浙江','安徽','福建','江西','山东','河南','湖北','湖南','广东','广西','海南','重庆','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆'),
    rownames = TRUE))

#------李雪妍4_18国家铁路省、市、自治区客运量
output$kyl_plot <- renderPlot( {
  lxy3rawdata<-lxy3_yearly
  lxy3rawdata$tm<-as.Date.POSIXct(lxy3rawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(lxy3rawdata$tm)
  
  if(input$year_start_kyl> input$year_end_kyl)  {
    p<-ggplot(lxy3rawdata,x=c(lxy3rawdata$tm[1],lxy3rawdata$tm[len]))
  }
  else{
    lxy3rawdatasub<-subset(lxy3rawdata,(substr(lxy3rawdata$tm,1,4)>=input$year_start_kyl) )
    lxy3rawdatasub<-subset(lxy3rawdatasub,(substr(lxy3rawdatasub$tm,1,4)<=input$year_end_kyl))
    p<-ggplot(lxy3rawdatasub,x=c(lxy3rawdatasub$tm[1],lxy3rawdatasub$tm[len]))
  }
  
  #mileage_total---------------总计
  if(input$zj3){
    p<-p+geom_line(aes(x=tm,y=mileage_total),color="black",size=0.7)+geom_point(aes(x=tm,y=mileage_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #beijing---------------北京
  if(input$beijing3){
    p<-p+geom_line(aes(x=tm,y=beijing),color="red",size=0.7)+geom_point(aes(x=tm,y=beijing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #tianjin---------------天津
  if(input$tianjin3){
    p<-p+geom_line(aes(x=tm,y=tianjin),color="blue",size=0.7)+geom_point(aes(x=tm,y=tianjin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hebei---------------河北
  if(input$hebei3){
    p<-p+geom_line(aes(x=tm,y=hebei),color="purple",size=0.7)+geom_point(aes(x=tm,y=hebei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanxi1---------------山西
  if(input$shanxi13){
    p<-p+geom_line(aes(x=tm,y=shanxi1),color="brown",size=0.7)+geom_point(aes(x=tm,y=shanxi1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  #neimenggu---------------内蒙古
  if(input$neimenggu3){
    p<-p+geom_line(aes(x=tm,y=neimenggu),color="orange",size=0.7)+geom_point(aes(x=tm,y=neimenggu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #liaoning---------------辽宁
  if(input$liaoning3){
    p<-p+geom_line(aes(x=tm,y=liaoning),color="black",size=0.7)+geom_point(aes(x=tm,y=liaoning),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jilin---------------吉林
  if(input$jilin3){
    p<-p+geom_line(aes(x=tm,y=jilin),color="red",size=0.7)+geom_point(aes(x=tm,y=jilin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #heilongjiang---------------黑龙江
  if(input$heilongjiang3){
    p<-p+geom_line(aes(x=tm,y=heilongjiang),color="blue",size=0.7)+geom_point(aes(x=tm,y=heilongjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanghai---------------上海
  if(input$shanghai3){
    p<-p+geom_line(aes(x=tm,y=shanghai),color="red",size=0.7)+geom_point(aes(x=tm,y=shanghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jiangsu---------------江苏
  if(input$jiangsu3){
    p<-p+geom_line(aes(x=tm,y=jiangsu),color="black",size=0.7)+geom_point(aes(x=tm,y=jiangsu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #zhejiang---------------浙江
  if(input$zhejiang3){
    p<-p+geom_line(aes(x=tm,y=zhejiang),color="blue",size=0.7)+geom_point(aes(x=tm,y=zhejiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #anhui---------------安徽
  if(input$anhui3){
    p<-p+geom_line(aes(x=tm,y=anhui),color="brown",size=0.7)+geom_point(aes(x=tm,y=anhui),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #fujian---------------福建
  if(input$fujian3){
    p<-p+geom_line(aes(x=tm,y=fujian),color="purple",size=0.7)+geom_point(aes(x=tm,y=fujian),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jiangxi---------------江西
  if(input$jiangxi3){
    p<-p+geom_line(aes(x=tm,y=jiangxi),color="orange",size=0.7)+geom_point(aes(x=tm,y=jiangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shandong---------------山东
  if(input$shandong3){
    p<-p+geom_line(aes(x=tm,y=shandong),color="green",size=0.7)+geom_point(aes(x=tm,y=shandong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #henan---------------河南
  if(input$henan3){
    p<-p+geom_line(aes(x=tm,y=henan),color="blue",size=0.7)+geom_point(aes(x=tm,y=henan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hubei---------------湖北
  if(input$hubei3){
    p<-p+geom_line(aes(x=tm,y=hubei),color="purple",size=0.7)+geom_point(aes(x=tm,y=hubei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hunan---------------湖南
  if(input$hunan3){
    p<-p+geom_line(aes(x=tm,y=hunan),color="red",size=0.7)+geom_point(aes(x=tm,y=hunan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guangdong---------------广东
  if(input$guangdong3){
    p<-p+geom_line(aes(x=tm,y=guangdong),color="black",size=0.7)+geom_point(aes(x=tm,y=guangdong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guangxi---------------广西
  if(input$guangxi3){
    p<-p+geom_line(aes(x=tm,y=guangxi),color="red",size=0.7)+geom_point(aes(x=tm,y=guangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hainan---------------海南
  if(input$hainan3){
    p<-p+geom_line(aes(x=tm,y=hainan),color="blue",size=0.7)+geom_point(aes(x=tm,y=hainan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #chongqing---------------重庆
  if(input$chongqing3){
    p<-p+geom_line(aes(x=tm,y=chongqing),color="orange",size=0.7)+geom_point(aes(x=tm,y=chongqing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #sichuan---------------四川
  if(input$sichuan3){
    p<-p+geom_line(aes(x=tm,y=sichuan),color="black",size=0.7)+geom_point(aes(x=tm,y=sichuan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guizhou---------------贵州
  if(input$guizhou3){
    p<-p+geom_line(aes(x=tm,y=guizhou),color="red",size=0.7)+geom_point(aes(x=tm,y=guizhou),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #yunnan---------------云南
  if(input$yunnan3){
    p<-p+geom_line(aes(x=tm,y=yunnan),color="blue",size=0.7)+geom_point(aes(x=tm,y=yunnan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #xizang---------------西藏
  if(input$xizang3){
    p<-p+geom_line(aes(x=tm,y=xizang),color="brown",size=0.7)+geom_point(aes(x=tm,y=xizang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanxi2---------------陕西
  if(input$shanxi23){
    p<-p+geom_line(aes(x=tm,y=shanxi2),color="black",size=0.7)+geom_point(aes(x=tm,y=shanxi2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #gansu---------------甘肃
  if(input$gansu3){
    p<-p+geom_line(aes(x=tm,y=gansu),color="red",size=0.7)+geom_point(aes(x=tm,y=gansu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #qinghai---------------青海
  if(input$qinghai3){
    p<-p+geom_line(aes(x=tm,y=qinghai),color="blue",size=0.7)+geom_point(aes(x=tm,y=qinghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #ningxia---------------宁夏
  if(input$ningxia3){
    p<-p+geom_line(aes(x=tm,y=ningxia),color="brown",size=0.7)+geom_point(aes(x=tm,y=ningxia),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #xinjiang---------------新疆
  if(input$xinjiang3){
    p<-p+geom_line(aes(x=tm,y=xinjiang),color="purple",size=0.7)+geom_point(aes(x=tm,y=xinjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  
  p+ylab("国家铁路省、市、自治区客运量")+xlab("时间")
  
})
#4_18kyl-------国家铁路省、市、自治区客运量表输出lixueyan
output$kyl_table<-DT::renderDataTable(
  DT::datatable(
    {  
      lxy3rawdata<-lxy3_yearly
      lxy3rawdata<-data.frame(lxy3rawdata[1:33])
      data<-lxy3rawdata},
    colnames = c('年度','合计','北京','天津','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海','江苏','浙江','安徽','福建','江西','山东','河南','湖北','湖南','广东','广西','海南','重庆','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆'),
    rownames = TRUE))
#------李雪妍4_19国家铁路省、市、自治区客运周转量
output$kyzzl_plot <- renderPlot( {
  lxy4rawdata<-lxy4_yearly
  lxy4rawdata$tm<-as.Date.POSIXct(lxy4rawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(lxy4rawdata$tm)
  
  if(input$year_start_kyzzl> input$year_end_kyzzl)  {
    p<-ggplot(lxy4rawdata,x=c(lxy4rawdata$tm[1],lxy4rawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    lxy4rawdatasub<-subset(lxy4rawdata,(substr(lxy4rawdata$tm,1,4)>=input$year_start_kyzzl) )
    lxy4rawdatasub<-subset(lxy4rawdatasub,(substr(lxy4rawdatasub$tm,1,4)<=input$year_end_kyzzl))
    p<-ggplot(lxy4rawdatasub,x=c(lxy4rawdatasub$tm[1],lxy4rawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  
  #zj---------------总计
  if(input$zj4){
    p<-p+geom_line(aes(x=tm,y=mileage_total),color="black",size=0.7)+geom_point(aes(x=tm,y=mileage_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #beijing---------------北京
  if(input$beijing4){
    p<-p+geom_line(aes(x=tm,y=beijing),color="red",size=0.7)+geom_point(aes(x=tm,y=beijing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #tianjin---------------天津
  if(input$tianjin4){
    p<-p+geom_line(aes(x=tm,y=tianjin),color="blue",size=0.7)+geom_point(aes(x=tm,y=tianjin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hebei---------------河北
  if(input$hebei4){
    p<-p+geom_line(aes(x=tm,y=hebei),color="purple",size=0.7)+geom_point(aes(x=tm,y=hebei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanxi1---------------山西
  if(input$shanxi14){
    p<-p+geom_line(aes(x=tm,y=shanxi1),color="orange",size=0.7)+geom_point(aes(x=tm,y=shanxi1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #neimenggu---------------内蒙古
  if(input$neimenggu4){
    p<-p+geom_line(aes(x=tm,y=neimenggu),color="brown",size=0.7)+geom_point(aes(x=tm,y=neimenggu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #liaoning---------------辽宁
  if(input$liaoning4){
    p<-p+geom_line(aes(x=tm,y=liaoning),color="black",size=0.7)+geom_point(aes(x=tm,y=liaoning),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jilin---------------吉林
  if(input$jilin4){
    p<-p+geom_line(aes(x=tm,y=jilin),color="red",size=0.7)+geom_point(aes(x=tm,y=jilin),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #heilongjiang---------------黑龙江
  if(input$heilongjiang4){
    p<-p+geom_line(aes(x=tm,y=heilongjiang),color="blue",size=0.7)+geom_point(aes(x=tm,y=heilongjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanghai---------------上海
  if(input$shanghai4){
    p<-p+geom_line(aes(x=tm,y=shanghai),color="black",size=0.7)+geom_point(aes(x=tm,y=shanghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jiangsu---------------江苏
  if(input$jiangsu4){
    p<-p+geom_line(aes(x=tm,y=jiangsu),color="red",size=0.7)+geom_point(aes(x=tm,y=jiangsu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #zhejiang---------------浙江
  if(input$zhejiang4){
    p<-p+geom_line(aes(x=tm,y=zhejiang),color="blue",size=0.7)+geom_point(aes(x=tm,y=zhejiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #anhui---------------安徽
  if(input$anhui4){
    p<-p+geom_line(aes(x=tm,y=anhui),color="orange",size=0.7)+geom_point(aes(x=tm,y=anhui),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #fujian---------------福建
  if(input$fujian4){
    p<-p+geom_line(aes(x=tm,y=fujian),color="brown",size=0.7)+geom_point(aes(x=tm,y=fujian),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #jiangxi---------------江西
  if(input$jiangxi4){
    p<-p+geom_line(aes(x=tm,y=jiangxi),color="green",size=0.7)+geom_point(aes(x=tm,y=jiangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shandong---------------山东
  if(input$shandong4){
    p<-p+geom_line(aes(x=tm,y=shandong),color="purple",size=0.7)+geom_point(aes(x=tm,y=shandong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #henan---------------河南
  if(input$henan4){
    p<-p+geom_line(aes(x=tm,y=henan),color="blue",size=0.7)+geom_point(aes(x=tm,y=henan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hubei---------------湖北
  if(input$hubei4){
    p<-p+geom_line(aes(x=tm,y=hubei),color="purple",size=0.7)+geom_point(aes(x=tm,y=hubei),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hunan---------------湖南
  if(input$hunan4){
    p<-p+geom_line(aes(x=tm,y=hunan),color="orange",size=0.7)+geom_point(aes(x=tm,y=hunan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guangdong---------------广东
  if(input$guangdong4){
    p<-p+geom_line(aes(x=tm,y=guangdong),color="black",size=0.7)+geom_point(aes(x=tm,y=guangdong),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guangxi---------------广西
  if(input$guangxi4){
    p<-p+geom_line(aes(x=tm,y=guangxi),color="red",size=0.7)+geom_point(aes(x=tm,y=guangxi),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #hainan---------------海南
  if(input$hainan4){
    p<-p+geom_line(aes(x=tm,y=hainan),color="blue",size=0.7)+geom_point(aes(x=tm,y=hainan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  #chongqing---------------重庆
  if(input$chongqing4){
    p<-p+geom_line(aes(x=tm,y=chongqing),color="black",size=0.7)+geom_point(aes(x=tm,y=chongqing),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #sichuan---------------四川
  if(input$sichuan4){
    p<-p+geom_line(aes(x=tm,y=sichuan),color="red",size=0.7)+geom_point(aes(x=tm,y=sichuan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #guizhou---------------贵州
  if(input$guizhou4){
    p<-p+geom_line(aes(x=tm,y=guizhou),color="blue",size=0.7)+geom_point(aes(x=tm,y=guizhou),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #yunnan---------------云南
  if(input$yunnan4){
    p<-p+geom_line(aes(x=tm,y=yunnan),color="brown",size=0.7)+geom_point(aes(x=tm,y=yunnan),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #xizang---------------西藏
  if(input$xizang4){
    p<-p+geom_line(aes(x=tm,y=xizang),color="purple",size=0.7)+geom_point(aes(x=tm,y=xizang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #shanxi2---------------陕西
  if(input$shanxi24){
    p<-p+geom_line(aes(x=tm,y=shanxi2),color="black",size=0.7)+geom_point(aes(x=tm,y=shanxi2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #gansu---------------甘肃
  if(input$gansu4){
    p<-p+geom_line(aes(x=tm,y=gansu),color="red",size=0.7)+geom_point(aes(x=tm,y=gansu),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #qinghai---------------青海
  if(input$qinghai4){
    p<-p+geom_line(aes(x=tm,y=qinghai),color="blue",size=0.7)+geom_point(aes(x=tm,y=qinghai),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #ningxia---------------宁夏
  if(input$ningxia4){
    p<-p+geom_line(aes(x=tm,y=ningxia4),color="purple",size=0.7)+geom_point(aes(x=tm,y=ningxia4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #xinjiang---------------新疆
  if(input$xinjiang4){
    p<-p+geom_line(aes(x=tm,y=xinjiang),color="brown",size=0.7)+geom_point(aes(x=tm,y=xinjiang),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  p+ylab("国家铁路省、市、自治区客运周转量")+xlab("时间")+geom_line()
  
})
#4_19kyzzl-------国家铁路省、市、自治区客运周转量表输出lixueyan
output$kyzzl_table<-DT::renderDataTable(
  DT::datatable(
    {  
      lxy4rawdata<-lxy4_yearly
      lxy4rawdata<-data.frame(lxy4rawdata[1:33])
      data<-lxy4rawdata},
    colnames = c('年度','合计','北京','天津','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海','江苏','浙江','安徽','福建','江西','山东','河南','湖北','湖南','广东','广西','海南','重庆','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆'),
    rownames = TRUE))
#------李雪妍5_1国家铁路机车运用指标
output$jcyyzb_plot <- renderPlot( {
  lxy5rawdata<-lxy5_yearly
  lxy5rawdata$tm<-as.Date.POSIXct(lxy5rawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(lxy5rawdata$tm)
  
  if(input$year_start_jcyyzb> input$year_end_jcyyzb)  {
    p<-ggplot(lxy5rawdata,x=c(lxy5rawdata$tm[1],lxy5rawdata$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    lxy5rawdatasub<-subset(lxy5rawdata,(substr(lxy5rawdata$tm,1,4)>=input$year_start_jcyyzb) )
    lxy5rawdatasub<-subset(lxy5rawdatasub,(substr(lxy5rawdatasub$tm,1,4)<=input$year_end_jcyyzb))
    p<-ggplot(lxy5rawdatasub,x=c(lxy5rawdatasub$tm[1],lxy5rawdatasub$tm[len]),aes(x=tm[1],y=0))
  }
  
  #daily_possessed_locomotive_number---------------平均一日支配机车台数
  if(input$daily_possessed_locomotive_number){
    p<-p+geom_line(aes(x=tm,y=daily_possessed_locomotive_number),color="black",size=0.7)+geom_point(aes(x=tm,y=daily_possessed_locomotive_number),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #used_locomotive_number---------------运用机车
  if(input$used_locomotive_number){
    p<-p+geom_line(aes(x=tm,y=used_locomotive_number),color="red",size=0.7)+geom_point(aes(x=tm,y=used_locomotive_number),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #locomotive_opration_rate---------------机车运用率（%）
  if(input$locomotive_opration_rate){
    p<-p+geom_line(aes(x=tm,y=locomotive_opration_rate),color="blue",size=0.7)+geom_point(aes(x=tm,y=locomotive_opration_rate),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #locomotive_repair_rate---------------机车检修率（%）
  if(input$locomotive_repair_rate){
    p<-p+geom_line(aes(x=tm,y=locomotive_repair_rate),color="brown",size=0.7)+geom_point(aes(x=tm,y=locomotive_repair_rate),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #daily_passenger_locomotive_mileage---------------客运机车日车公里(公里)
  if(input$daily_passenger_locomotive_mileage){
    p<-p+geom_line(aes(x=tm,y=daily_passenger_locomotive_mileage),color="red",size=0.7)+geom_point(aes(x=tm,y=daily_passenger_locomotive_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #diesel_locomotive1---------------内燃
  if(input$diesel_locomotive1){
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive1),color="blue",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #electic_locomotive1---------------电力
  if(input$electic_locomotive1){
    p<-p+geom_line(aes(x=tm,y=electic_locomotive1),color="black",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #steam_locomotive1---------------蒸汽
  if(input$steam_locomotive1){
    p<-p+geom_line(aes(x=tm,y=steam_locomotive1),color="orange",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #passenger_locomotive_speed---------------客运机车旅行速度(公里/小时)
  if(input$passenger_locomotive_speed){
    p<-p+geom_line(aes(x=tm,y=passenger_locomotive_speed),color="brown",size=0.7)+geom_point(aes(x=tm,y=passenger_locomotive_speed),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #passenger_locomotive_tech_speed---------------客运机车技术速度(公里/小时)
  if(input$passenger_locomotive_tech_speed){
    p<-p+geom_line(aes(x=tm,y=passenger_locomotive_tech_speed),color="purple",size=0.7)+geom_point(aes(x=tm,y=passenger_locomotive_tech_speed),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #daily_freight_locomotive_mileage---------------货运机车日车公里(公里)
  if(input$daily_freight_locomotive_mileage){
    p<-p+geom_line(aes(x=tm,y=daily_freight_locomotive_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=daily_freight_locomotive_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #diesel_locomotive2---------------内燃
  if(input$diesel_locomotive2){
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive2),color="red",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #electic_locomotive2---------------电力
  if(input$electic_locomotive2){
    p<-p+geom_line(aes(x=tm,y=electic_locomotive2),color="blue",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #steam_locomotive2---------------蒸汽
  if(input$steam_locomotive2){
    p<-p+geom_line(aes(x=tm,y=steam_locomotive2),color="brown",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #freight_locomotive_speed---------------货运机车旅行速度(公里/小时)
  if(input$freight_locomotive_speed){
    p<-p+geom_line(aes(x=tm,y=freight_locomotive_speed),color="purple",size=0.7)+geom_point(aes(x=tm,y=freight_locomotive_speed),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #freight_locomotive_tech_speed---------------货运机车技术速度(公里/小时)
  if(input$freight_locomotive_tech_speed){
    p<-p+geom_line(aes(x=tm,y=freight_locomotive_tech_speed),color="orange",size=0.7)+geom_point(aes(x=tm,y=freight_locomotive_tech_speed),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #daily_freight_locomotive_volume---------------货运机车日产量
  if(input$daily_freight_locomotive_volume){
    p<-p+geom_line(aes(x=tm,y=daily_freight_locomotive_volume),color="black",size=0.7)+geom_point(aes(x=tm,y=daily_freight_locomotive_volume),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #diesel_locomotive3---------------内燃
  if(input$diesel_locomotive3){
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive3),color="red",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #electic_locomotive3---------------电力
  if(input$electic_locomotive3){
    p<-p+geom_line(aes(x=tm,y=electic_locomotive3),color="blue",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #steam_locomotive3---------------蒸汽
  if(input$steam_locomotive3){
    p<-p+geom_line(aes(x=tm,y=steam_locomotive3),color="brown",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #daily_freight_locomotive_height---------------货运列车平均总重(吨)
  if(input$daily_freight_locomotive_height){
    p<-p+geom_line(aes(x=tm,y=daily_freight_locomotive_height),color="black",size=0.7)+geom_point(aes(x=tm,y=daily_freight_locomotive_height),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #diesel_locomotive4---------------内燃
  if(input$diesel_locomotive4){
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive4),color="red",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #electic_locomotive4---------------电力
  if(input$electic_locomotive4){
    p<-p+geom_line(aes(x=tm,y=electic_locomotive4),color="blue",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #steam_locomotive4---------------蒸汽
  if(input$steam_locomotive4){
    p<-p+geom_line(aes(x=tm,y=steam_locomotive4),color="purple",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  p+ylab("国家铁路机车运用指标")+xlab("时间")+geom_line()
  
})
#5_1jcyyzb-------国家铁路机车运用指标表输出lixueyan
output$jcyyzb_table<-DT::renderDataTable(
  DT::datatable(
    {  
      lxy5rawdata<-lxy5_yearly
      lxy5rawdata<-data.frame(lxy5rawdata[1:25])
      data<-lxy5rawdata},
    colnames = c('年度','平均一日支配机车台数','运用机车','机车运用率（%）','机车检修率（%）','客运机车日车公里(公里)','内燃','电力','蒸汽','客运机车旅行速度(公里/小时)','客运机车技术速度(公里/小时)','货运机车日车公里(公里)','内燃','电力','蒸汽','货运机车旅行速度(公里/小时)','货运机车技术速度(公里/小时)','货运机车日产量','内燃','电力','蒸汽','货运列车平均总重(吨)','内燃','电力','蒸汽'),
    rownames = TRUE))
#——国家铁路机车能源消耗——————————————————————————————————————————————————————————————————————————————————————
#——国家铁路机车能源消耗——————————————————————————————————————————————————————————————————————————————————————
output$railwaytrain_energy_plot <- renderPlot( {
  dfrawdata_52<-df_yearly_52
  dfrawdata_52$tm<-as.Date.POSIXct(df_yearly_52$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata_52$tm)
  
  if(input$year_start_railwaytrain_energy> input$year_end_railwaytrain_energy)  {
    p<-ggplot(dfrawdata_52,x=c(dfrawdata_52$tm[1],dfrawdata_52$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub_52<-subset(dfrawdata_52,(substr(dfrawdata_52$tm,1,4)>=input$year_start_railwaytrain_energy) )
    dfrawdatasub_52<-subset(dfrawdatasub_52,(substr(dfrawdatasub_52$tm,1,4)<=input$year_end_railwaytrain_energy))
    p<-ggplot(dfrawdatasub_52,x=c(dfrawdatasub_52$tm[1],dfrawdatasub_52$tm[len]),aes(x=tm[1],y=0))
  }
  
  #oil_consume_volume---------------内燃机车油消耗总量(万吨)
  if(input$railwaytrain_consumption=="oil_consume_volume"){
    p<-p+geom_line(aes(x=tm,y=oil_consume_volume),color="black",size=0.7)+ylim(200,530)+geom_point(aes(x=tm,y=oil_consume_volume),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #oil_consume_volume_kilometer --------------内燃机车每万吨公里耗油量(公斤) 
  if (input$railwaytrain_consumption=="oil_consume_volume_kilometer") {
    p<-p+geom_line(aes(x=tm,y=oil_consume_volume_kilometer),color="red",size=0.7)+ylim(20,30)+geom_point(aes(x=tm,y=oil_consume_volume_kilometer),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #electric_consume_volume-----------------电力机车电消耗总量(亿千瓦小时)
  if (input$railwaytrain_consumption=="electric_consume_volume") {
    p<-p+geom_line(aes(x=tm,y=electric_consume_volume),color="blue",size=0.7)+geom_point(aes(x=tm,y=electric_consume_volume),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #electric_consume_volume_kilometer----------------电力机车每万吨公里耗电量(千瓦小时)
  if (input$railwaytrain_consumption=="electric_consume_volume_kilometer") {
    p<-p+geom_line(aes(x=tm,y=electric_consume_volume_kilometer),color="orange",size=0.7)+ylim(100,120)+geom_point(aes(x=tm,y=electric_consume_volume_kilometer),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #coal_consume_volume-----------------蒸汽机车煤消耗总量(万吨)
  if (input$railwaytrain_consumption=="coal_consume_volume") {
    p<-p+geom_line(aes(x=tm,y=coal_consume_volume),color="blue",size=0.7)+geom_point(aes(x=tm,y=coal_consume_volume),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #coal_consume_volume_kilometer----------------蒸汽机车每万吨公里耗煤量(公斤)
  if (input$railwaytrain_consumption=="coal_consume_volume_kilometer") {
    p<-p+geom_line(aes(x=tm,y=coal_consume_volume_kilometer),color="orange",size=0.7)+geom_point(aes(x=tm,y=coal_consume_volume_kilometer),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  p+ylab("铁路机车能源消耗")+xlab("时间")+geom_line()
  
})
#机车能源消耗
output$railwaytrain_energy_table<-DT::renderDataTable(
  DT::datatable(
    {
      dfrawdata_52<-df_yearly_52
      dfrawdata_52<-data.frame(dfrawdata_52)
      data<-dfrawdata_52},
    colnames = c('时间','内燃机车油消耗总量(万吨)','内燃机车每万吨公里耗油量(公斤)','电力机车电消耗总量(亿千瓦小时)','电力机车每万吨公里耗电量(千瓦小时)','蒸汽机车煤消耗总量(万吨)','蒸汽机车每万吨公里耗煤量(公斤)'),
    rownames = TRUE))

#——国家铁路机车工作量——————————————————————————————————————————————————————————————————————————————————————
#——国家铁路机车工作量——————————————————————————————————————————————————————————————————————————————————————
output$railwaytrain_work_plot <- renderPlot( {
  dfrawdata_53<-df_yearly_53
  dfrawdata_53$tm<-as.Date.POSIXct(df_yearly_53$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata_53$tm)
  
  if(input$year_start_railwaytrain_work> input$year_end_railwaytrain_work)  {
    p<-ggplot(dfrawdata_53,x=c(dfrawdata_53$tm[1],dfrawdata_53$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub_53<-subset(dfrawdata_53,(substr(dfrawdata_53$tm,1,4)>=input$year_start_railwaytrain_work) )
    dfrawdatasub_53<-subset(dfrawdatasub_53,(substr(dfrawdatasub_53$tm,1,4)<=input$year_end_railwaytrain_work))
    p<-ggplot(dfrawdatasub_53,x=c(dfrawdatasub_53$tm[1],dfrawdatasub_53$tm[len]),aes(x=tm[1],y=0))
  }
  
  if(input$locomotive_running_mileage){
    p<-p+geom_line(aes(x=tm,y=locomotive_running_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=locomotive_running_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$diesel_locomotive1) {
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive1),color="red",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive1),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  if (input$electic_locomotive1) {
    p<-p+geom_line(aes(x=tm,y=electic_locomotive1),color="blue",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive1),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$steam_locomotive1) {
    p<-p+geom_line(aes(x=tm,y=steam_locomotive1),color="orange",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive1),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$service_running_mileage){
    p<-p+geom_line(aes(x=tm,y=service_running_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=service_running_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$diesel_locomotive2) {
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive2),color="red",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive2),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  if (input$electic_locomotive2) {
    p<-p+geom_line(aes(x=tm,y=electic_locomotive2),color="blue",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive2),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$steam_locomotive2) {
    p<-p+geom_line(aes(x=tm,y=steam_locomotive2),color="orange",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive2),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$traction_running_mileage){
    p<-p+geom_line(aes(x=tm,y=traction_running_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=traction_running_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$diesel_locomotive3) {
    p<-p+geom_line(aes(x=tm,y=diesel_locomotive3),color="red",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive3),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  }
  if (input$electic_locomotive3) {
    p<-p+geom_line(aes(x=tm,y=electic_locomotive3),color="blue",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive3),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$steam_locomotive3) {
    p<-p+geom_line(aes(x=tm,y=steam_locomotive3),color="orange",size=0.7)+geom_point(aes(x=tm,y=steam_locomotive3),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  p+ylab("机车走行公里数据")+xlab("时间")+geom_line()
  
})
#机车工作量
output$railwaytrain_work_table<-DT::renderDataTable(
  DT::datatable(
    {
      dfrawdata_53<-df_yearly_53
      dfrawdata_53<-data.frame(dfrawdata_53)
      data<-dfrawdata_53},
    colnames = c('时间','机车走行公里合计(百万)','内燃机车走行公里(百万)','电力机车走行公里(百万)','蒸汽机车走行公里(百万)','本务机走行公里合计(百万)','本务内燃机走行公里(百万)','本务电力机走行公里(百万)','本务蒸汽机走行公里(百万)','牵引重吨公里合计(亿)','内燃牵引重吨公里(亿)','电力牵引重吨公里(亿)','蒸汽牵引重吨公里(亿)'),
    rownames = TRUE))
#——国家铁路货车运用指标——————————————————————————————————————————————————————————————————————————————————————
#——国家铁路货车运用指标——————————————————————————————————————————————————————————————————————————————————————
output$railway_freightusing_plot <- renderPlot( {
  dfrawdata_54<-df_yearly_54
  dfrawdata_54$tm<-as.Date.POSIXct(df_yearly_54$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata_54$tm)
  
  if(input$year_start_railway_freightusing> input$year_end_railway_freightusing)  {
    p<-ggplot(dfrawdata_54,x=c(dfrawdata_54$tm[1],dfrawdata_54$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub_54<-subset(dfrawdata_54,(substr(dfrawdata_54$tm,1,4)>=input$year_start_railway_freightusing) )
    dfrawdatasub_54<-subset(dfrawdatasub_54,(substr(dfrawdatasub_54$tm,1,4)<=input$year_end_railway_freightusing))
    p<-ggplot(dfrawdatasub_54,x=c(dfrawdatasub_54$tm[1],dfrawdatasub_54$tm[len]),aes(x=tm[1],y=0))
  }
  
  if(input$daily_load){
    p<-p+geom_line(aes(x=tm,y=daily_load),color="black",size=0.7)+geom_point(aes(x=tm,y=daily_load),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$daily_load_coal) {
    p<-p+geom_line(aes(x=tm,y=daily_load_coal),color="red",size=0.7)+geom_point(aes(x=tm,y=daily_load_coal),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  }
  if (input$daily_unload) {
    p<-p+geom_line(aes(x=tm,y=daily_unload),color="blue",size=0.7)+geom_point(aes(x=tm,y=daily_unload),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$freight_car_static_weight) {
    p<-p+geom_line(aes(x=tm,y=freight_car_static_weight),color="orange",size=0.7)+ylim(50,70)+geom_point(aes(x=tm,y=freight_car_static_weight),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$daily_ownership_car){
    p<-p+geom_line(aes(x=tm,y=daily_ownership_car),color="black",size=0.7)+geom_point(aes(x=tm,y=daily_ownership_car),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$daily_using_car) {
    p<-p+geom_line(aes(x=tm,y=daily_using_car),color="red",size=0.7)+geom_point(aes(x=tm,y=daily_using_car),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  if (input$feight_car_using_percent) {
    p<-p+geom_line(aes(x=tm,y=feight_car_using_percent),color="blue",size=0.7)+geom_point(aes(x=tm,y=feight_car_using_percent),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$empty_car_running_percent) {
    p<-p+geom_line(aes(x=tm,y=empty_car_running_percent),color="orange",size=0.7)+geom_point(aes(x=tm,y=empty_car_running_percent),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$feight_car_turnover){
    p<-p+geom_line(aes(x=tm,y=feight_car_turnover),color="black",size=0.7)+ylim(4,6)+geom_point(aes(x=tm,y=feight_car_turnover),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$once_detention_time) {
    p<-p+geom_line(aes(x=tm,y=once_detention_time),color="red",size=0.7)+geom_point(aes(x=tm,y=once_detention_time),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  if (input$transit_detention_time) {
    p<-p+geom_line(aes(x=tm,y=transit_detention_time),color="blue",size=0.7)+geom_point(aes(x=tm,y=transit_detention_time),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$complete_round) {
    p<-p+geom_line(aes(x=tm,y=complete_round),color="orange",size=0.7)+geom_point(aes(x=tm,y=complete_round),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$heavy_round) {
    p<-p+geom_line(aes(x=tm,y=heavy_round),color="blue",size=0.7)+geom_point(aes(x=tm,y=heavy_round),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$transit_round) {
    p<-p+geom_line(aes(x=tm,y=transit_round),color="red",size=0.7)+geom_point(aes(x=tm,y=transit_round),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  }
  p+ylab("货车运用指标数据")+xlab("时间")+geom_line()
  
})
#货车运用指标
output$railway_freightusing_table<-DT::renderDataTable(
  DT::datatable(
    {
      dfrawdata_54<-df_yearly_54
      dfrawdata_54<-data.frame(dfrawdata_54)
      data<-dfrawdata_54},
    colnames = c('时间','日均装车(车)','日均装车_煤(车)','日均卸车(车)','货车静载重(吨)','日均现在车(辆)','日均运用车(辆)','货车运用率(%)','空车走行率(%)','货车周转时间(天)','一次作业停留时间(小时)','中转停留时间(小时)','全周距 (公里)','重周距(公里)','中转距离(公里)'),
    rownames = TRUE))
#-----李亚芳---------------
#——国家铁路列车正点率——————————————————————————————————————————————————————————————————————————————————————
#——国家铁路列车正点率——————————————————————————————————————————————————————————————————————————————————————
output$railwaytrain_correct_point_plot <- renderPlot( {
  dfrawdata_55<-df_yearly_55
  dfrawdata_55$tm<-as.Date.POSIXct(dfrawdata_55$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata_55$tm)
  
  if(input$year_start_railwaytrain_correct_point> input$year_end_railwaytrain_correct_point)  {
    p<-ggplot(dfrawdata_55,x=c(dfrawdata_55$tm[1],dfrawdata_55$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub_55<-subset(dfrawdata_55,(substr(dfrawdata_55$tm,1,4)>=input$year_start_railwaytrain_correct_point) )
    dfrawdatasub_55<-subset(dfrawdatasub_55,(substr(dfrawdatasub_55$tm,1,4)<=input$year_end_railwaytrain_correct_point))
    p<-ggplot(dfrawdatasub_55,x=c(dfrawdatasub_55$tm[1],dfrawdatasub_55$tm[len]),aes(x=tm[1],y=0))
  }
  
  #passenger_depart_on_shedule_rate---------------旅客列车出发正点率(%)
  if(input$passenger_depart_on_shedule_rate){
    p<-p+geom_line(aes(x=tm,y=passenger_depart_on_shedule_rate),color="black",size=0.7)+ylim(89,100)+geom_point(aes(x=tm,y=passenger_depart_on_shedule_rate),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #passenger_running_on_shedule_rate --------------旅客列车运行正点率(%) 
  if (input$passenger_running_on_shedule_rate) {
    p<-p+geom_line(aes(x=tm,y=passenger_running_on_shedule_rate),color="red",size=0.7)+ylim(89,100)+geom_point(aes(x=tm,y=passenger_running_on_shedule_rate),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    
  }
  #freight_depart_on_shedule_rate-----------------货物列车出发正点率(%)
  if (input$freight_depart_on_shedule_rate) {
    p<-p+geom_line(aes(x=tm,y=freight_depart_on_shedule_rate),color="blue",size=0.7)+ylim(89,100)+geom_point(aes(x=tm,y=freight_depart_on_shedule_rate),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  #freight_running_on_shedule_rate----------------货物列车运行正点率(%)
  if (input$freight_running_on_shedule_rate) {
    p<-p+geom_line(aes(x=tm,y=freight_running_on_shedule_rate),color="orange",size=0.7)+ylim(89,100)+geom_point(aes(x=tm,y=freight_running_on_shedule_rate),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  
  p+ylab("列车正点率数据")+xlab("时间")+geom_line()
  
})
#列车正点率
output$railwaytrain_correct_point_table<-DT::renderDataTable(
  DT::datatable(
    {
      dfrawdata_55<-df_yearly_55
      dfrawdata_55<-data.frame(dfrawdata_55)
      data<-dfrawdata_55},
    colnames = c('时间','旅客列车出发正点率(%)','旅客列车运行正点率(%)','货物列车出发正点率(%)','货物列车运行正点率(%)'),
    rownames = TRUE))
#——全国铁路固定资产投资——————————————————————————————————————————————————————————————————————————————————————
#——全国铁路固定资产投资——————————————————————————————————————————————————————————————————————————————————————
output$railway_gdzctz_plot <- renderPlot( {
  dfrawdata_61<-df_yearly_61
  dfrawdata_61$tm<-as.Date.POSIXct(df_yearly_61$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata_61$tm)
  
  if(input$year_start_railway_gdzctz> input$year_end_railway_gdzctz)  {
    p<-ggplot(dfrawdata_61,x=c(dfrawdata_61$tm[1],dfrawdata_61$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub_61<-subset(dfrawdata_61,(substr(dfrawdata_61$tm,1,4)>=input$year_start_railway_gdzctz) )
    dfrawdatasub_61<-subset(dfrawdatasub_61,(substr(dfrawdatasub_61$tm,1,4)<=input$year_end_railway_gdzctz))
    p<-ggplot(dfrawdatasub_61,x=c(dfrawdatasub_61$tm[1],dfrawdatasub_61$tm[len]),aes(x=tm[1],y=0))
  }
  
  if(input$fixed_assets_investment){
    p<-p+geom_line(aes(x=tm,y=fixed_assets_investment),color="black",size=0.7)+geom_point(aes(x=tm,y=fixed_assets_investment),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$infrastructure_investment) {
    p<-p+geom_line(aes(x=tm,y=infrastructure_investment),color="red",size=0.7)+geom_point(aes(x=tm,y=infrastructure_investment),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  }
  if (input$renovation_investment) {
    p<-p+geom_line(aes(x=tm,y=renovation_investment),color="blue",size=0.7)+geom_point(aes(x=tm,y=renovation_investment),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$locomotive_purchase_investment) {
    p<-p+geom_line(aes(x=tm,y=locomotive_purchase_investment),color="orange",size=0.7)+geom_point(aes(x=tm,y=locomotive_purchase_investment),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  p+ylab("固定资产投资数据")+xlab("时间")+geom_line()
  
})
output$railway_gdzctz_table<-DT::renderDataTable(
  DT::datatable(
    {
      dfrawdata_61<-df_yearly_61
      dfrawdata_61<-data.frame(dfrawdata_61)
      data<-dfrawdata_61},
    colnames = c('时间','总固定资产(亿元)','基本建设(亿元)','更新改造(亿元)','机车车辆购置(亿元)'),
    rownames = TRUE))
#——全国铁路基本建设投资——————————————————————————————————————————————————————————————————————————————————————
#——全国铁路基本建设投资——————————————————————————————————————————————————————————————————————————————————————
output$railway_jbjstz_plot <- renderPlot( {
  dfrawdata_62<-df_yearly_62
  dfrawdata_62$tm<-as.Date.POSIXct(df_yearly_62$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  len<-length(dfrawdata_62$tm)
  
  if(input$year_start_railway_jbjstz> input$year_end_railway_jbjstz)  {
    p<-ggplot(dfrawdata_62,x=c(dfrawdata_62$tm[1],dfrawdata_62$tm[len]),aes(x=tm[1],y=0))
  }
  else{
    dfrawdatasub_62<-subset(dfrawdata_62,(substr(dfrawdata_62$tm,1,4)>=input$year_start_railway_jbjstz) )
    dfrawdatasub_62<-subset(dfrawdatasub_62,(substr(dfrawdatasub_62$tm,1,4)<=input$year_end_railway_jbjstz))
    p<-ggplot(dfrawdatasub_62,x=c(dfrawdatasub_62$tm[1],dfrawdatasub_62$tm[len]),aes(x=tm[1],y=0))
  }
  
  if(input$infrastructure_investment_sum){
    p<-p+geom_line(aes(x=tm,y=infrastructure_investment_sum),color="black",size=0.7)+geom_point(aes(x=tm,y=infrastructure_investment_sum),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$national_joint_railway) {
    p<-p+geom_line(aes(x=tm,y=national_joint_railway),color="red",size=0.7)+geom_point(aes(x=tm,y=national_joint_railway),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  }
  if (input$local_railway) {
    p<-p+geom_line(aes(x=tm,y=local_railway),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$MOR_investment) {
    p<-p+geom_line(aes(x=tm,y=MOR_investment),color="orange",size=0.7)+geom_point(aes(x=tm,y=MOR_investment),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if(input$MOR_national_joint_railway){
    p<-p+geom_line(aes(x=tm,y=MOR_national_joint_railway),color="black",size=0.7)+geom_point(aes(x=tm,y=MOR_national_joint_railway),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$MOR_local_railway) {
    p<-p+geom_line(aes(x=tm,y=MOR_local_railway),color="red",size=0.7)+geom_point(aes(x=tm,y=MOR_local_railway),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  }
  if (input$local_government_enterprise_investment) {
    p<-p+geom_line(aes(x=tm,y=local_government_enterprise_investment),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_government_enterprise_investment),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$local_railway_local) {
    p<-p+geom_line(aes(x=tm,y=local_railway_local),color="orange",size=0.7)+geom_point(aes(x=tm,y=local_railway_local),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$old_line) {
    p<-p+geom_line(aes(x=tm,y=old_line),color="red",size=0.7)+geom_point(aes(x=tm,y=old_line),size=3,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
  }
  if (input$new_line) {
    p<-p+geom_line(aes(x=tm,y=new_line),color="blue",size=0.7)+geom_point(aes(x=tm,y=new_line),size=3,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
  }
  if (input$other_line) {
    p<-p+geom_line(aes(x=tm,y=other_line),color="orange",size=0.7)+geom_point(aes(x=tm,y=other_line),size=3,shape=21,colour="darkred",fill="cornsilk",position=position_dodge(width=0.2))
  }
  p+ylab("基本建设投资数据")+xlab("时间")+geom_line()
  
})
#--------李亚芳---------  
#基本建设投资
output$railway_jbjstz_table<-DT::renderDataTable(
  DT::datatable(
    {
      dfrawdata_62<-df_yearly_62
      dfrawdata_62<-data.frame(dfrawdata_62)
      data<-dfrawdata_62},
    colnames = c('时间','基本建设投资合计(亿元)','国家和合资铁路(亿元)','地方铁路(亿元)','铁道部投资合计(亿元)','铁道部国家和合资铁路(亿元)','铁道部地方铁路(亿元)','地方政府与企业投资合计(亿元)','地方政府与企业投资地方铁路(亿元)','既有线(亿元)','新线(亿元)','其他(亿元)'),
    rownames = TRUE))
#--------李亚芳---------  
#--------刘治----------
    output$plot6.3 <- renderPlot({
if(input$tm_start6.3 > input$tm_end6.3){
        p<-ggplot(table6.3)
        }
        else{
        sub6.3<-subset(table6.3,tm>=input$tm_start6.3)
        sub6.3<-subset(sub6.3,tm<=input$tm_end6.3)
        p<-ggplot(sub6.3,x=c(input$tm_start6.3,input$tm_start6.3))
        }
        if(input$df6.3_capital_toal){
            p<-p+geom_line(aes(x=tm,y=infrustrate_capital_total),color="black",size=0.7)+geom_point(aes(x=tm,y=infrustrate_capital_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_national_budget){
            p<-p+geom_line(aes(x=tm,y=national_budget),color="red",size=0.7)+geom_point(aes(x=tm,y=national_budget),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_internal_loan){
            p<-p+geom_line(aes(x=tm,y=domestic_loans),color="blue",size=0.7)+geom_point(aes(x=tm,y=domestic_loans),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_foreign_capital){
            p<-p+geom_line(aes(x=tm,y=foreign_capital),color="orange",size=0.7)+geom_point(aes(x=tm,y=foreign_capital),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_special_fund){
            p<-p+geom_line(aes(x=tm,y=special_fund),color="green",size=0.7)+geom_point(aes(x=tm,y=special_fund),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_bond){
            p<-p+geom_line(aes(x=tm,y=bond),color="purple",size=0.7)+geom_point(aes(x=tm,y=bond),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_coal_oil){
            p<-p+geom_line(aes(x=tm,y=coal_replace_oil),color="black",size=0.7)+geom_point(aes(x=tm,y=coal_replace_oil),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_MOR_self){
            p<-p+geom_line(aes(x=tm,y=MOR_self),color="red",size=0.7)+geom_point(aes(x=tm,y=MOR_self),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_enterprise_self){
            p<-p+geom_line(aes(x=tm,y=public_institution_self),color="blue",size=0.7)+geom_point(aes(x=tm,y=public_institution_self),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_others){
            p<-p+geom_line(aes(x=tm,y=other_capital),color="orange",size=0.7)+geom_point(aes(x=tm,y=other_capital),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.3_vehicle_tax){
            p<-p+geom_line(aes(x=tm,y=vehicle_purchase_tax),color="green",size=0.7)+geom_point(aes(x=tm,y=vehicle_purchase_tax),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        p+ylab("铁道部基本建设投资的资金来源")+xlab("时间")
    })
    
    output$table6.3 <- DT::renderDataTable({
        names(table6.3)<- c("年度","合计","国家预算内",'国内贷款','利用外资','专项基金','债券','煤代油','铁道部自筹','企事业自筹','其他资金','车辆购置税')
        table6.3})

    output$plot6.4 <- renderPlot({
        if(input$tm_start6.4 > input$tm_end6.4){
        p<-ggplot(table6.4)
        }
        else{
        sub6.4<-subset(table6.4,tm>=input$tm_start6.4)
        sub6.4<-subset(sub6.4,tm<=input$tm_end6.4)
        p<-ggplot(sub6.4,x=c(input$tm_start6.4,input$tm_start6.4))
        }
        if(input$df6.4_new_lay_total){
            p<-p+geom_line(aes(x=tm,y=newline_tracklaying_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=newline_tracklaying_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_new_lay_national){
            p<-p+geom_line(aes(x=tm,y=national_joint_railway1),color="red",size=0.7)+geom_point(aes(x=tm,y=national_joint_railway1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_new_lay_regional){
            p<-p+geom_line(aes(x=tm,y=local_railway1),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway1),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_multi_lay_total){
            p<-p+geom_line(aes(x=tm,y=nultiline_tracklaying_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=nultiline_tracklaying_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_multi_lay_national){
            p<-p+geom_line(aes(x=tm,y=national_joint_railway2),color="red",size=0.7)+geom_point(aes(x=tm,y=national_joint_railway2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_multi_lay_regional){
            p<-p+geom_line(aes(x=tm,y=local_railway2),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway2),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_new_product_total){
            p<-p+geom_line(aes(x=tm,y=newline_operation_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=newline_operation_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_new_product_national){
            p<-p+geom_line(aes(x=tm,y=national_joint_railway3),color="red",size=0.7)+geom_point(aes(x=tm,y=national_joint_railway3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_new_product_regional){
            p<-p+geom_line(aes(x=tm,y=local_railway3),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway3),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_multi_product_total){
            p<-p+geom_line(aes(x=tm,y=nultiline_operation_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=nultiline_operation_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_multi_product_national){
            p<-p+geom_line(aes(x=tm,y=national_joint_railway4),color="red",size=0.7)+geom_point(aes(x=tm,y=national_joint_railway4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_multi_product_regional){
            p<-p+geom_line(aes(x=tm,y=local_railway4),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway4),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_electr_product_total){
            p<-p+geom_line(aes(x=tm,y=electric_railway_opration_mileage),color="black",size=0.7)+geom_point(aes(x=tm,y=electric_railway_opration_mileage),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_electr_product_national){
            p<-p+geom_line(aes(x=tm,y=national_joint_railway5),color="red",size=0.7)+geom_point(aes(x=tm,y=national_joint_railway5),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.4_electr_product_regional){
            p<-p+geom_line(aes(x=tm,y=local_railway5),color="blue",size=0.7)+geom_point(aes(x=tm,y=local_railway5),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        p+ylab("铁路基本建设铺轨及投产里程")+xlab("时间")
    })
    
    output$table6.4 <- DT::renderDataTable({
        names(table6.4) <- c('年度','新线铺轨里程','国家和合资铁路','地方铁路','复线铺轨里程','国家和合资铁路','地方铁路','新线投产里程','国家和合资铁路','地方铁路','复线投产里程','国家和合资铁路','地方铁路','电气化铁路投产里程','国家和合资铁路','地方铁路')
        table6.4})
    
    output$plot6.7 <- renderPlot({
        if(input$tm_start6.7 > input$tm_end6.7){
        p<-ggplot(table6.7)
        }
        else{
        sub6.7<-subset(table6.7,tm>=input$tm_start6.7)
        sub6.7<-subset(sub6.7,tm<=input$tm_end6.7)
        p<-ggplot(sub6.7,x=c(input$tm_start6.7,input$tm_end6.7))
        }
        if(input$df6.7_invest_toal){
            p<-p+geom_line(aes(x=tm,y=completed_investment),color="black",size=0.7)+geom_point(aes(x=tm,y=completed_investment),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_invest_infruatructure){
            p<-p+geom_line(aes(x=tm,y=infrastructure_capital),color="red",size=0.7)+geom_point(aes(x=tm,y=infrastructure_capital),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_invest_change){
            p<-p+geom_line(aes(x=tm,y=change_capital),color="blue",size=0.7)+geom_point(aes(x=tm,y=change_capital),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_invest_others){
            p<-p+geom_line(aes(x=tm,y=other_capital),color="orange",size=0.7)+geom_point(aes(x=tm,y=other_capital),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_motor_total){
            p<-p+geom_line(aes(x=tm,y=locomotive_total),color="black",size=0.7)+geom_point(aes(x=tm,y=locomotive_total),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_motor_fuel){
            p<-p+geom_line(aes(x=tm,y=diesel_locomotive),color="red",size=0.7)+geom_point(aes(x=tm,y=diesel_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_motor_electric){
            p<-p+geom_line(aes(x=tm,y=electic_locomotive),color="blue",size=0.7)+geom_point(aes(x=tm,y=electic_locomotive),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_bus7){
            p<-p+geom_line(aes(x=tm,y=passenger_car),color="red",size=0.7)+geom_point(aes(x=tm,y=passenger_car),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_truck){
            p<-p+geom_line(aes(x=tm,y=freight_car),color="blue",size=0.7)+geom_point(aes(x=tm,y=freight_car),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        if(input$df6.7_bullet){
            p<-p+geom_line(aes(x=tm,y=bullettrain_group),color="black",size=0.7)+geom_point(aes(x=tm,y=bullettrain_group),size=3,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))}
        p+ylab("国家铁路机车车辆购置")+xlab('时间')
    })
    
    output$table6.7 <- DT::renderDataTable({
        names(table6.7)<-c('年度','投资完成（亿元）','基建资金','更改资金','其他资金','机车（台）','内燃','电力',"客车（辆）",'货车（辆）',"动车组(组)")
        table6.7})

}
)