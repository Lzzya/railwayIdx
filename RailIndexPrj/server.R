shinyServer(function(input, output) {
  
  
  require(ggplot2)
  require(DT)
  require(e1071)
  require(randomForest)
  require(forecast)
  require(rJava)
  require(xlsx)
  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  #铁路预警信号灯
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  df_index<-read.csv("预警.csv",header=T)
  df_index$tm<-as.Date.POSIXct(df_index$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
  df_monthly<-read.xlsx("rawdata_monthly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
  df_yearly<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
  output$plot_index<-renderPlot({ 
    p<-ggplot(data=df_index,aes(x=tm,y=index))
    p<-p+ylim(0,1)+xlim(df_index[1,1],df_index[156,1])
    p<-p+annotate("rect",xmin=df_index[1,1],xmax=df_index[156,1],ymin=0,ymax=0.15,fill="red",alpha=0.3)
    p<-p+annotate("rect",xmin=df_index[1,1],xmax=df_index[156,1],ymin=0.15,ymax=0.35,fill="yellow",alpha=0.3)
    p<-p+annotate("rect",xmin=df_index[1,1],xmax=df_index[156,1],ymin=0.35,ymax=0.65,fill="green",alpha=0.3)
    p<-p+annotate("rect",xmin=df_index[1,1],xmax=df_index[156,1],ymin=0.65,ymax=0.85,fill="#56B4E9",alpha=0.3)
    p<-p+annotate("rect",xmin=df_index[1,1],xmax=df_index[156,1],ymin=0.85,ymax=1,fill="blue",alpha=0.3)
    p<-p+geom_line(size=1)
    p+theme_bw()+theme(panel.border=element_blank())+xlab("日期")+ylab("指数")
  })
  
  output$index_table<-DT::renderDataTable(
    DT::datatable(
      data<-df_index, 
      colnames = c('日期','景气指数'),
      rownames = TRUE))
  
  
  #————————————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————————————
  #铁路景气指数，包括合成指数和扩散指数
  #————————————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————————————
  
  #-------------------------------------------------------
  #---------------合成指数------add a comments to make a change-------------
  
  #运输合成指数计算------------------------------
  dftrans<-read.csv("trans-coor.csv",head=T)
  dftrans$tm<-as.Date.POSIXct(dftrans$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  trans.len<-length(dftrans$tm)
  
  #-----运输----1. 权重计算函数--------------------
  quanzhong.1<- function(x)
  { xlen<- length(x)
  x<- x/max(x)
  x<- -(1/log(xlen))*sum( (x/sum(x))*log((x/sum(x))) )
  x<- 1-x}
  
  #------运输---1.1 同步/一致指标的权重--------
  hyl.trans.qz<- quanzhong.1(dftrans$hyl)/(quanzhong.1(dftrans$hyl)+quanzhong.1(dftrans$gyzjz)+quanzhong.1(dftrans$hyzzl))
  gyzjz.trans.qz<- quanzhong.1(dftrans$gyzjz)/(quanzhong.1(dftrans$hyl)+quanzhong.1(dftrans$gyzjz)+quanzhong.1(dftrans$hyzzl))
  hyzzl.trans.qz<- quanzhong.1(dftrans$hyzzl)/(quanzhong.1(dftrans$hyl)+quanzhong.1(dftrans$gyzjz)+quanzhong.1(dftrans$hyzzl))
  
  #------运输----1.2 滞后指标的权重--------------------------------------------------------------- 
  kyl.trans.qz<- quanzhong.1(dftrans$kyl)/(quanzhong.1(dftrans$kyl)+quanzhong.1(dftrans$kyzzl)+quanzhong.1(dftrans$gdzctz))
  kyzzl.trans.qz<- quanzhong.1(dftrans$kyzzl)/(quanzhong.1(dftrans$kyl)+quanzhong.1(dftrans$kyzzl)+quanzhong.1(dftrans$gdzctz))
  gdzctz.trans.qz<- quanzhong.1(dftrans$gdzctz)/(quanzhong.1(dftrans$kyl)+quanzhong.1(dftrans$kyzzl)+quanzhong.1(dftrans$gdzctz))
  
  #----运输------1.3 先行指标的权重--------------------------------------------------------------- 
  gc.trans.qz<- quanzhong.1(dftrans$gc)/(quanzhong.1(dftrans$gc)+quanzhong.1(dftrans$ym)+quanzhong.1(dftrans$yy)+quanzhong.1(dftrans$hlfdl))
  ym.trans.qz<- quanzhong.1(dftrans$ym)/(quanzhong.1(dftrans$gc)+quanzhong.1(dftrans$ym)+quanzhong.1(dftrans$yy)+quanzhong.1(dftrans$hlfdl))
  yy.trans.qz<- quanzhong.1(dftrans$yy)/(quanzhong.1(dftrans$gc)+quanzhong.1(dftrans$ym)+quanzhong.1(dftrans$yy)+quanzhong.1(dftrans$hlfdl))
  hlfdl.trans.qz<- quanzhong.1(dftrans$hlfdl)/(quanzhong.1(dftrans$gc)+quanzhong.1(dftrans$ym)+quanzhong.1(dftrans$yy)+quanzhong.1(dftrans$hlfdl))
  
  #-----运输----2. 合成指数计算------------------------  
  
  #-----运输----2.1 标准化变化率计算--------
  index.1<- function(z)
  { zlen<- length(z)
  z[3:zlen]<- 200*(z[3:zlen]-z[2:(zlen-1)])/(z[3:zlen]+z[2:(zlen-1)])
  z[2]<- 0
  z[1]<- 0
  z<- z/(sum(abs(z))/(zlen-2))}  #标准化变化率计算函数
  
  #-----运输----2.2 平均变化率R----------------------------------------------------------------------------
  coor.trans.test<- index.1(dftrans$x12hyl)*hyl.trans.qz + index.1(dftrans$x12hyzzl)*hyzzl.trans.qz+index.1(dftrans$x12gyzjz)*gyzjz.trans.qz
  #coor.test一致合成指数平均变化率R2
  
  adv.trans.test<- index.1(dftrans$x12gc)*(gc.trans.qz-0.2)+ index.1(dftrans$x12ym)*ym.trans.qz+index.1(dftrans$x12yy)*(yy.trans.qz+0.1)+index.1(dftrans$x12hlfdl)*(hlfdl.trans.qz+0.1)
  #adv.trans.test先行合成指数平均变化率R1
  
  delay.trans.test<- index.1(dftrans$x12kyl)*kyl.trans.qz + index.1(dftrans$x12kyzzl)*kyzzl.trans.qz+index.1(dftrans$x12gdzctz)*gdzctz.trans.qz
  #coor.trans.test滞后合成指数平均变化率R3
  
  #-----运输----2.3 标准化因子F----------------------------------------------
  biaozhunhua.F.coor<- 1  #同步的标准化因子是1
  biaozhunhua.trans.F.adv<- sum(abs(adv.trans.test))/sum(abs(coor.trans.test)) #先行的标准化因子
  biaozhunhua.trans.F.delay<- sum(abs(delay.trans.test))/sum(abs(coor.trans.test)) #滞后的标准化因子
  
  #-----运输----2.4 合成指数计算---------------
  hecheng.trans.index<- function(a,b)
  {
    alen<- length(a)
    a<- a/b
    a[1]<- 100
    a[2:alen]<- (200+a[2:alen])/(200-a[2:alen])#(200+当列)/(200-当列)
    a1<- a
    for(i in 2:alen){a1[i]<- a1[i-1]*a[i] }#a1[i]=a1[i-1]*a[i]初步合成指数
    index.jizhunnianfen<- mean(a1[49:60]) #抽取基准年份的平均值
    a1<- 100*a1/index.jizhunnianfen
    return(a1)
  }
  
  #默认权重计算得到的运输同步、先行、滞后指数
  trans.coor<- hecheng.trans.index(coor.trans.test,biaozhunhua.F.coor)
  trans.adv<- hecheng.trans.index(adv.trans.test,biaozhunhua.trans.F.adv)
  trans.delay<- hecheng.trans.index(delay.trans.test,biaozhunhua.trans.F.delay)
  
  dftrans$coor<- trans.coor
  dftrans$adv<- trans.adv
  dftrans$delay<- trans.delay
  
  #-----------运输的算完了！！----3.运输画线和显示数据表--------
  
  qz.input<- function(a)
  {a<- as.numeric(a)/100}  #权重手动输入部分计算的函数们
  
  output$trans_index<- renderPlot( {
    
    #---权重手动输入的计算----------
    hyl.qz.input<- qz.input(input$trans_hyl_qz_input)
    gyzjz.qz.input<- qz.input(input$trans_gyzjz_qz_input)
    hyzzl.qz.input<- qz.input(input$trans_hyzzl_qz_input)
    gc.qz.input<- qz.input(input$trans_gc_qz_input)
    ym.qz.input<- qz.input(input$trans_ym_qz_input)
    yy.qz.input<- qz.input(input$trans_yy_qz_input)
    hlfdl.qz.input<- qz.input(input$trans_hlfdl_qz_input)
    kyl.qz.input<- qz.input(input$trans_kyl_qz_input)
    gdzctz.qz.input<- qz.input(input$trans_gdzctz_qz_input)
    kyzzl.qz.input<- qz.input(input$trans_kyzzl_qz_input)
    
    coor.test.input<- index.1(dftrans$x12hyl)*hyl.qz.input+index.1(dftrans$x12hyzzl)*hyzzl.qz.input+index.1(dftrans$x12gyzjz)*gyzjz.qz.input
    adv.test.input<- index.1(dftrans$x12gc)*gc.qz.input + index.1(dftrans$x12ym)*ym.qz.input+index.1(dftrans$x12yy)*yy.qz.input+index.1(dftrans$x12hlfdl)*hlfdl.qz.input
    delay.test.input<- index.1(dftrans$x12kyl)*kyl.qz.input + index.1(dftrans$x12kyzzl)*kyzzl.qz.input+index.1(dftrans$x12gdzctz)*gdzctz.qz.input
    
    biaozhunhua.F.adv.input<- sum(abs(adv.test.input))/sum(abs(coor.test.input)) 
    biaozhunhua.F.delay.input<- sum(abs(delay.test.input))/sum(abs(coor.test.input))
    
    trans.coor.input<- hecheng.trans.index(coor.test.input,biaozhunhua.F.coor)
    trans.adv.input<- hecheng.trans.index(adv.test.input,biaozhunhua.F.adv.input)
    trans.delay.input<- hecheng.trans.index(delay.test.input,biaozhunhua.F.delay.input)
    
    dftrans$coor.input<- trans.coor.input   
    dftrans$adv.input<- trans.adv.input
    dftrans$delay.input<- trans.delay.input
    
    #-----运输----3.1 运输默认权重计算的画线------------  
    if(input$year_start_trans> input$year_end_trans)  {
      p<-ggplot(dftrans,x=c(dftrans$tm[1],dftrans$tm[trans.len]),aes(x=tm,y=100))}
    else{
      dftranssub<-subset(dftrans,(substr(dftrans$tm,1,4)>=input$year_start_trans) )
      dftranssub<-subset(dftranssub,(substr(dftranssub$tm,1,4)<=input$year_end_trans))
      p<-ggplot(dftranssub,x=c(dftranssub$tm[1],dftranssub$tm[trans.len]),aes(x=tm,y=100))}
    
    
    if(input$trans_coor_Index){
      p<-p+geom_line(aes(x=tm,y=dftranssub$coor),color="black",size=0.6)}
    if (input$trans_advanced_Index) {
      p<-p+geom_line(aes(x=tm,y=dftranssub$adv),color="red",size=0.6) }
    if (input$trans_delay_Index) {
      p<-p+geom_line(aes(x=tm,y=dftranssub$delay),color="blue",size=0.6)}
    
    #----运输----3.2 输入修改权重后算出来的新先行指数------------------
    if(input$trans_qz_coor_input)#输入修改权重后算出来的新先行指数
    { p<-p+geom_line(aes(x=tm,y=dftranssub$coor.input),color="black",size=1,linetype=1)}
    if(input$trans_qz_adv_input)
    {p<-p+geom_line(aes(x=tm,y=dftranssub$adv.input),color="red",size=1,linetype=1)}
    if(input$trans_qz_delay_input)
    {p<-p+geom_line(aes(x=tm,y=dftranssub$delay.input),color="blue",size=1,linetype=1)}  
    
    p+ylab("运输合成指数")+xlab("时间")+geom_line()
  })
  
  #----运输-----4.数据表的显示------------------------------------ 
  output$table_trans_index<-DT::renderDataTable({
    
    hyl.qz.input<- qz.input(input$trans_hyl_qz_input)
    gyzjz.qz.input<- qz.input(input$trans_gyzjz_qz_input)
    hyzzl.qz.input<- qz.input(input$trans_hyzzl_qz_input)
    gc.qz.input<- qz.input(input$trans_gc_qz_input)
    ym.qz.input<- qz.input(input$trans_ym_qz_input)
    yy.qz.input<- qz.input(input$trans_yy_qz_input)
    hlfdl.qz.input<- qz.input(input$trans_hlfdl_qz_input)
    kyl.qz.input<- qz.input(input$trans_kyl_qz_input)
    gdzctz.qz.input<- qz.input(input$trans_gdzctz_qz_input)
    kyzzl.qz.input<- qz.input(input$trans_kyzzl_qz_input)
    
    coor.test.input<- index.1(dftrans$x12hyl)*hyl.qz.input+index.1(dftrans$x12hyzzl)*hyzzl.qz.input+index.1(dftrans$x12gyzjz)*gyzjz.qz.input
    adv.test.input<- index.1(dftrans$x12gc)*gc.qz.input + index.1(dftrans$x12ym)*ym.qz.input+index.1(dftrans$x12yy)*yy.qz.input+index.1(dftrans$x12hlfdl)*hlfdl.qz.input
    delay.test.input<- index.1(dftrans$x12kyl)*kyl.qz.input + index.1(dftrans$x12kyzzl)*kyzzl.qz.input+index.1(dftrans$x12gdzctz)*gdzctz.qz.input
    
    biaozhunhua.F.adv.input<- sum(abs(adv.test.input))/sum(abs(coor.test.input)) 
    biaozhunhua.F.delay.input<- sum(abs(delay.test.input))/sum(abs(coor.test.input))
    
    trans.coor.input<- hecheng.trans.index(coor.test.input,biaozhunhua.F.coor)
    trans.adv.input<- hecheng.trans.index(adv.test.input,biaozhunhua.F.adv.input)
    trans.delay.input<- hecheng.trans.index(delay.test.input,biaozhunhua.F.delay.input)
    
    dftrans$coor.input<- trans.coor.input   
    dftrans$adv.input<- trans.adv.input
    dftrans$delay.input<- trans.delay.input
    
    #----运输----4.1 输入修改权重后算出来的三个指数------------------   
    if(input$trans_qz_coor_input|input$trans_qz_adv_input|input$trans_qz_delay_input){
      DT::datatable(
        { dftrans<- data.frame(dftrans[1],dftrans[5],dftrans[6],dftrans[7])
        data<-dftrans},
        colnames = c('时间', '先行指数',  '同步指数','滞后指数'),
        rownames = TRUE)}
    #----运输----4.2 默认权重计算下的三个指数------------------  
    else{ 
      DT::datatable(
        {data<-dftrans[1:4]},
        colnames = c('时间', '运输合成先行指数',  '运输合成同步指数','运输合成滞后指数'),
        rownames = TRUE)
    }
  })
  
  
  #-----设备合成指数计算-------------------------------
  dfequip<-read.csv("equip-coor.csv",head=T)
  dfequip$tm<-as.Date.POSIXct(dfequip$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  equip.len<-length(dfequip$tm)
  
  #---------设备 1. 权重计算------------------------
  #---函数公式同上，不用再写一遍
  
  #---------设备 1.1 同步/一致指标的权重---------------------------------------------------------------
  jczxzlc.equip.qz<- quanzhong.1(dfequip$jczxzlc)/(quanzhong.1(dfequip$jczxzlc)+quanzhong.1(dfequip$rjyyc))
  rjyyc.equip.qz<- quanzhong.1(dfequip$rjyyc)/(quanzhong.1(dfequip$jczxzlc)+quanzhong.1(dfequip$rjyyc))
  
  #----------设备 1.2 滞后指标的权重--------------------------------------------------------------- 
  rjxzc.equip.qz<- quanzhong.1(dfequip$rjxzc)/(quanzhong.1(dfequip$rjxzc)+quanzhong.1(dfequip$kyjclc)+quanzhong.1(dfequip$hyjclc)+quanzhong.1(dfequip$kcls)+quanzhong.1(dfequip$hcls)+quanzhong.1(dfequip$jcts))
  kyjclc.equip.qz<- quanzhong.1(dfequip$kyjclc)/(quanzhong.1(dfequip$rjxzc)+quanzhong.1(dfequip$kyjclc)+quanzhong.1(dfequip$hyjclc)+quanzhong.1(dfequip$kcls)+quanzhong.1(dfequip$hcls)+quanzhong.1(dfequip$jcts))
  hyjclc.equip.qz<- quanzhong.1(dfequip$hyjclc)/(quanzhong.1(dfequip$rjxzc)+quanzhong.1(dfequip$kyjclc)+quanzhong.1(dfequip$hyjclc)+quanzhong.1(dfequip$kcls)+quanzhong.1(dfequip$hcls)+quanzhong.1(dfequip$jcts))
  kcls.equip.qz<- quanzhong.1(dfequip$kcls)/(quanzhong.1(dfequip$rjxzc)+quanzhong.1(dfequip$kyjclc)+quanzhong.1(dfequip$hyjclc)+quanzhong.1(dfequip$kcls)+quanzhong.1(dfequip$hcls)+quanzhong.1(dfequip$jcts))
  hcls.equip.qz<- quanzhong.1(dfequip$hcls)/(quanzhong.1(dfequip$rjxzc)+quanzhong.1(dfequip$kyjclc)+quanzhong.1(dfequip$hyjclc)+quanzhong.1(dfequip$kcls)+quanzhong.1(dfequip$hcls)+quanzhong.1(dfequip$jcts))
  jcts.equip.qz<- quanzhong.1(dfequip$jcts)/(quanzhong.1(dfequip$rjxzc)+quanzhong.1(dfequip$kyjclc)+quanzhong.1(dfequip$hyjclc)+quanzhong.1(dfequip$kcls)+quanzhong.1(dfequip$hcls)+quanzhong.1(dfequip$jcts))
  
  #----------设备 1.3 先行指标的权重，计算出来同运输的不一样，所以变量标注2--------------------------------------------------------------- 
  gc.equip.qz<- quanzhong.1(dfequip$gc)/(quanzhong.1(dfequip$gc)+quanzhong.1(dfequip$ym)+quanzhong.1(dfequip$yy)+quanzhong.1(dfequip$hlfdl))
  ym.equip.qz<- quanzhong.1(dfequip$ym)/(quanzhong.1(dfequip$gc)+quanzhong.1(dfequip$ym)+quanzhong.1(dfequip$yy)+quanzhong.1(dfequip$hlfdl))
  yy.equip.qz<- quanzhong.1(dfequip$yy)/(quanzhong.1(dfequip$gc)+quanzhong.1(dfequip$ym)+quanzhong.1(dfequip$yy)+quanzhong.1(dfequip$hlfdl))
  hlfdl.equip.qz<- quanzhong.1(dfequip$hlfdl)/(quanzhong.1(dfequip$gc)+quanzhong.1(dfequip$ym)+quanzhong.1(dfequip$yy)+quanzhong.1(dfequip$hlfdl))
  
  #---------设备 2. 合成指数计算------------------------  
  
  #--------设备的合成指数不用去季节化，因为本来就是年度数据，直接计算增长率---------
  rate.1<- function(m)
  { m[2:length(m)]<- m[2:length(m)]/m[1:(length(m)-1)]-1
  m[1]<- 0
  return(m)}
  
  #---------设备 2.1 标准化变化率计算----同上运输标准化变化率，不用再重复----
  
  #---------设备 2.2 平均变化率R----------------------------------------------------------------------------
  coor.equip.test<- index.1(rate.1(dfequip$jczxzlc))*jczxzlc.equip.qz + index.1(rate.1(dfequip$rjyyc))*rjyyc.equip.qz
  #coor2.test一致合成指数平均变化率R2
  
  adv.equip.test<- index.1(rate.1(dfequip$gc))*gc.equip.qz + index.1(rate.1(dfequip$ym))*ym.equip.qz+index.1(rate.1(dfequip$yy))*yy.equip.qz+index.1(rate.1(dfequip$hlfdl))*hlfdl.equip.qz
  #coor2.test先行合成指数平均变化率R1
  
  delay.equip.test<- index.1(rate.1(dfequip$rjxzc))*rjxzc.equip.qz+index.1(rate.1(dfequip$kyjclc))*kyjclc.equip.qz+index.1(rate.1(dfequip$hyjclc))*hyjclc.equip.qz+index.1(rate.1(dfequip$kcls))*kcls.equip.qz+index.1(rate.1(dfequip$hcls))*hcls.equip.qz+index.1(rate.1(dfequip$jcts))*jcts.equip.qz
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
  
  dfequip$coor<- equip.coor
  dfequip$adv<- equip.adv
  dfequip$delay<- equip.delay
  
  #-----------设备的算完了！！----3.设备 画线和显示数据表--------
  
  output$equip_index<- renderPlot( {
    
    rjyyc.qz.input<- qz.input(input$equip_rjyyc_qz_input)
    jczxzlc.qz.input<- qz.input(input$equip_jczxzlc_qz_input)
    gc.qz.input<- qz.input(input$equip_gc_qz_input)
    ym.qz.input<- qz.input(input$equip_ym_qz_input)
    yy.qz.input<- qz.input(input$equip_yy_qz_input)
    hlfdl.qz.input<- qz.input(input$equip_hlfdl_qz_input)
    rjxzc.qz.input<- qz.input(input$equip_rjxzc_qz_input)
    kyjclc.qz.input<- qz.input(input$equip_kyjclc_qz_input)
    hyjclc.qz.input<- qz.input(input$equip_hyjclc_qz_input)
    kcls.qz.input<- qz.input(input$equip_kcls_qz_input)
    hcls.qz.input<- qz.input(input$equip_hcls_qz_input)
    jcts.qz.input<- qz.input(input$equip_jcts_qz_input)
    
    equip.coor.test.input<- index.1(rate.1(dfequip$jczxzlc))*jczxzlc.qz.input + index.1(rate.1(dfequip$rjyyc))*rjyyc.qz.input
    equip.adv.test.input<- index.1(rate.1(dfequip$gc))*gc.qz.input + index.1(rate.1(dfequip$ym))*ym.qz.input+index.1(rate.1(dfequip$yy))*yy.qz.input+index.1(rate.1(dfequip$hlfdl))*hlfdl.qz.input
    equip.delay.test.input<- index.1(rate.1(dfequip$rjxzc))*rjxzc.qz.input+index.1(rate.1(dfequip$kyjclc))*kyjclc.qz.input+index.1(rate.1(dfequip$hyjclc))*hyjclc.qz.input+index.1(rate.1(dfequip$kcls))*kcls.qz.input+index.1(rate.1(dfequip$hcls))*hcls.qz.input+index.1(rate.1(dfequip$jcts))*jcts.qz.input
    
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
      p<-ggplot(dfequip,x=c(dfequip$tm[1],dfequip$tm[equip.len]),aes(x=tm,y=100))  }
    else{
      dfequipsub<-subset(dfequip,(substr(dfequip$tm,1,4)>=input$year_start_equip) )
      dfequipsub<-subset(dfequipsub,(substr(dfequipsub$tm,1,4)<=input$year_end_equip))
      p<-ggplot(dfequipsub,x=c(dfequipsub$tm[1],dfequipsub$tm[equip.len]),aes(x=tm,y=100))    }
    
    if(input$equip_coor_Index){
      p<-p+geom_line(aes(x=tm,y=dfequipsub$coor),color="black",size=0.6)
      p<-p+geom_point(aes(x=tm,y=dfequipsub$coor),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if (input$equip_advanced_Index) {
      p<-p+geom_line(aes(x=tm,y=dfequipsub$adv),color="red",size=0.6)
      p<-p+geom_point(aes(x=tm,y=dfequipsub$adv),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if (input$equip_delay_Index) {
      p<-p+geom_line(aes(x=tm,y=dfequipsub$delay),color="blue",size=0.6)
      p<-p+geom_point(aes(x=tm,y=dfequipsub$delay),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    
    #-----设备----3.2 权重手动输入后的画线------------      
    if(input$equip_qz_coor_input)#输入修改权重后算出来的新先行指数
    { p<-p+geom_line(aes(x=tm,y=dfequipsub$coor.input),color="black",size=1,linetype=1)
    p<-p+geom_point(aes(x=tm,y=dfequipsub$coor.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if(input$equip_qz_adv_input)
    {p<-p+geom_line(aes(x=tm,y=dfequipsub$adv.input),color="red",size=1,linetype=1)
    p<-p+geom_point(aes(x=tm,y=dfequipsub$adv.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if(input$equip_qz_delay_input)
    {p<-p+geom_line(aes(x=tm,y=dfequipsub$delay.input),color="blue",size=1,linetype=1)
    p<-p+geom_point(aes(x=tm,y=dfequipsub$delay.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))} 
    
    
    p+ylab("设备合成指数")+xlab("时间")+geom_line()
  })
  
  output$table_equip_index<-DT::renderDataTable({
    
    rjyyc.qz.input<- qz.input(input$equip_rjyyc_qz_input)
    jczxzlc.qz.input<- qz.input(input$equip_jczxzlc_qz_input)
    gc.qz.input<- qz.input(input$equip_gc_qz_input)
    ym.qz.input<- qz.input(input$equip_ym_qz_input)
    yy.qz.input<- qz.input(input$equip_yy_qz_input)
    hlfdl.qz.input<- qz.input(input$equip_hlfdl_qz_input)
    rjxzc.qz.input<- qz.input(input$equip_rjxzc_qz_input)
    kyjclc.qz.input<- qz.input(input$equip_kyjclc_qz_input)
    hyjclc.qz.input<- qz.input(input$equip_hyjclc_qz_input)
    kcls.qz.input<- qz.input(input$equip_kcls_qz_input)
    hcls.qz.input<- qz.input(input$equip_hcls_qz_input)
    jcts.qz.input<- qz.input(input$equip_jcts_qz_input)
    
    equip.coor.test.input<- index.1(rate.1(dfequip$jczxzlc))*jczxzlc.qz.input + index.1(rate.1(dfequip$rjyyc))*rjyyc.qz.input
    equip.adv.test.input<- index.1(rate.1(dfequip$gc))*gc.qz.input + index.1(rate.1(dfequip$ym))*ym.qz.input+index.1(rate.1(dfequip$yy))*yy.qz.input+index.1(rate.1(dfequip$hlfdl))*hlfdl.qz.input
    equip.delay.test.input<- index.1(rate.1(dfequip$rjxzc))*rjxzc.qz.input+index.1(rate.1(dfequip$kyjclc))*kyjclc.qz.input+index.1(rate.1(dfequip$hyjclc))*hyjclc.qz.input+index.1(rate.1(dfequip$kcls))*kcls.qz.input+index.1(rate.1(dfequip$hcls))*hcls.qz.input+index.1(rate.1(dfequip$jcts))*jcts.qz.input
    
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
        { dftrans<- data.frame(dfequip[1],dfequip[5],dfequip[6],dfequip[7])
        data<-dftrans},
        colnames = c('时间', '先行指数',  '同步指数','滞后指数'),
        rownames = TRUE)}
    
    else{DT::datatable(
      {data<-dfequip[1:4]},
      colnames = c('时间', '设备合成先行指数',  '设备合成同步指数','设备合成滞后指数'),
      rownames = TRUE)}
  })
  #-----规模合成指数计算-------------------------------
  dfscale<-read.csv("scale-coor.csv",head=T)
  dfscale$tm<-as.Date.POSIXct(dfscale$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  scale.len<-length(dfscale$tm)
  
  #---------规模 1. 权重计算------------------------
  #---函数公式同上，不用再写一遍
  
  #---------规模 1.1 同步/一致指标的权重---------------------------------------------------------------
  gyzjz.scale.qz<- quanzhong.1(dfscale$gyzjz)/(quanzhong.1(dfscale$gyzjz)+quanzhong.1(dfscale$hyl)+quanzhong.1(dfscale$hyzzl))
  hyl.scale.qz<- quanzhong.1(dfscale$hyl)/(quanzhong.1(dfscale$gyzjz)+quanzhong.1(dfscale$hyl)+quanzhong.1(dfscale$hyzzl))
  hyzzl.scale.qz<- quanzhong.1(dfscale$hyzzl)/(quanzhong.1(dfscale$gyzjz)+quanzhong.1(dfscale$hyl)+quanzhong.1(dfscale$hyzzl))
  #----------规模 1.2 滞后指标的权重--------------------------------------------------------------- 
  kcls.scale.qz<- quanzhong.1(dfscale$kcls)/(quanzhong.1(dfscale$kcls)+quanzhong.1(dfscale$hcls)+quanzhong.1(dfscale$yylc)+quanzhong.1(dfscale$cyrysl)+quanzhong.1(dfscale$jcts))
  hcls.scale.qz<- quanzhong.1(dfscale$hcls)/(quanzhong.1(dfscale$kcls)+quanzhong.1(dfscale$hcls)+quanzhong.1(dfscale$yylc)+quanzhong.1(dfscale$cyrysl)+quanzhong.1(dfscale$jcts))
  yylc.scale.qz<- quanzhong.1(dfscale$yylc)/(quanzhong.1(dfscale$kcls)+quanzhong.1(dfscale$hcls)+quanzhong.1(dfscale$yylc)+quanzhong.1(dfscale$cyrysl)+quanzhong.1(dfscale$jcts))
  cyrysl.scale.qz<- quanzhong.1(dfscale$cyrysl)/(quanzhong.1(dfscale$kcls)+quanzhong.1(dfscale$hcls)+quanzhong.1(dfscale$yylc)+quanzhong.1(dfscale$cyrysl)+quanzhong.1(dfscale$jcts))
  jcts.scale.qz<- quanzhong.1(dfscale$jcts)/(quanzhong.1(dfscale$kcls)+quanzhong.1(dfscale$hcls)+quanzhong.1(dfscale$yylc)+quanzhong.1(dfscale$cyrysl)+quanzhong.1(dfscale$jcts))
  
  #----------规模 1.3 先行指标的权重，计算出来同规模一样，直接用规模的即可--------------------------------------------------------------- 
  
  #---------规模 2. 合成指数计算------------------------  
  
  #--------规模的合成指数不用去季节化，因为本来就是年度数据，直接计算增长率---------
  rate.1<- function(m)
  { m[2:length(m)]<- m[2:length(m)]/m[1:(length(m)-1)]-1
  m[1]<- 0
  return(m)}
  
  #---------规模 2.1 标准化变化率计算----同上设备标准化变化率，不用再重复----
  
  #---------规模 2.2 平均变化率R----------------------------------------------------------------------------
  coor.scale.test<- index.1(rate.1(dfscale$gyzjz))*gyzjz.scale.qz + index.1(rate.1(dfscale$hyl))*hyl.scale.qz + index.1(rate.1(dfscale$hyzzl))*hyzzl.scale.qz
  #coor.scale.test一致合成指数平均变化率R2
  
  #adv.equip.test<- index.1(rate.1(dfscale$gc))*gc2.qz + index.1(rate.1(dfscale$ym))*ym2.qz+index.1(rate.1(dfscale$yy))*yy2.qz+index.1(rate.1(dfscale$hlfdl))*hlfdl2.qz
  #adv.scale.test先行合成指数平均变化率R1 同coor.equip.test
  
  delay.scale.test<- index.1(rate.1(dfscale$kcls))*kcls.scale.qz+index.1(rate.1(dfscale$hcls))*hcls.scale.qz+index.1(rate.1(dfscale$yylc))*yylc.scale.qz+index.1(rate.1(dfscale$cyrysl))*cyrysl.scale.qz+index.1(rate.1(dfscale$jcts))*jcts.scale.qz  #delay3.test滞后合成指数平均变化率R3
  
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
    
    hyl.qz.input<- qz.input(input$scale_hyl_qz_input)
    gyzjz.qz.input<- qz.input(input$scale_gyzjz_qz_input)
    hyzzl.qz.input<- qz.input(input$scale_hyzzl_qz_input)
    gc.qz.input<- qz.input(input$scale_gc_qz_input)
    ym.qz.input<- qz.input(input$scale_ym_qz_input)
    yy.qz.input<- qz.input(input$scale_yy_qz_input)
    hlfdl.qz.input<- qz.input(input$scale_hlfdl_qz_input)
    kcls.qz.input<- qz.input(input$scale_kcls_qz_input)
    hcls.qz.input<- qz.input(input$scale_hcls_qz_input)
    yylc.qz.input<- qz.input(input$scale_yylc_qz_input)
    cyrysl.qz.input<- qz.input(input$scale_cyrysl_qz_input)
    jcts.qz.input<- qz.input(input$scale_jcts_qz_input)
    
    coor.test.input<- index.1(rate.1(dfscale$hyl))*hyl.qz.input+index.1(rate.1(dfscale$hyzzl))*hyzzl.qz.input+index.1(rate.1(dfscale$gyzjz))*gyzjz.qz.input
    adv.test.input<- index.1(rate.1(dfscale$gc))*gc.qz.input + index.1(rate.1(dfscale$ym))*ym.qz.input+index.1(rate.1(dfscale$yy))*yy.qz.input+index.1(rate.1(dfscale$hlfdl))*hlfdl.qz.input
    delay.test.input<- index.1(rate.1(dfscale$kcls))*kcls.qz.input + index.1(rate.1(dfscale$hcls))*hcls.qz.input+index.1(rate.1(dfscale$yylc))*yylc.qz.input+index.1(rate.1(dfscale$cyrysl))*cyrysl.qz.input+index.1(rate.1(dfscale$jcts))*jcts.qz.input
    
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
      p<-ggplot(dfscale,x=c(dfscale$tm[1],dfscale$tm[scale.len]),aes(x=tm,y=100))
    }
    else{
      dfscalesub<-subset(dfscale,(substr(dfscale$tm,1,4)>=input$year_start_scale) )
      dfscalesub<-subset(dfscalesub,(substr(dfscalesub$tm,1,4)<=input$year_end_scale))
      p<-ggplot(dfscalesub,x=c(dfscalesub$tm[1],dfscalesub$tm[scale.len]),aes(x=tm,y=100))
    }
    
    if(input$scale_coor_Index){
      p<-p+geom_line(aes(x=tm,y=dfscalesub$coor),color="black",size=0.6)
      p<-p+geom_point(aes(x=tm,y=dfscalesub$coor),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if (input$scale_advanced_Index) {
      p<-p+geom_line(aes(x=tm,y=dfscalesub$adv),color="red",size=0.6) 
      p<-p+geom_point(aes(x=tm,y=dfscalesub$adv),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if (input$scale_delay_Index) {
      p<-p+geom_line(aes(x=tm,y=dfscalesub$delay),color="blue",size=0.6)
      p<-p+geom_point(aes(x=tm,y=dfscalesub$delay),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    
    #-----设备----3.2 权重手动输入后的画线------------      
    if(input$scale_qz_coor_input)#输入修改权重后算出来的新先行指数
    { p<-p+geom_line(aes(x=tm,y=dfscalesub$coor.input),color="black",size=1,linetype=1)
    p<-p+geom_point(aes(x=tm,y=dfscalesub$coor.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if(input$scale_qz_adv_input)
    {p<-p+geom_line(aes(x=tm,y=dfscalesub$adv.input),color="red",size=1,linetype=1)
    p<-p+geom_point(aes(x=tm,y=dfscalesub$adv.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}
    if(input$scale_qz_delay_input)
    {p<-p+geom_line(aes(x=tm,y=dfscalesub$delay.input),color="blue",size=1,linetype=1)
    p<-p+geom_point(aes(x=tm,y=dfscalesub$delay.input),size=2,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))}    
    
    p+ylab("规模合成指数")+xlab("时间")+geom_line()
  })
  
  
  output$table_scale_index<-DT::renderDataTable(
    {
      hyl.qz.input<- qz.input(input$scale_hyl_qz_input)
      gyzjz.qz.input<- qz.input(input$scale_gyzjz_qz_input)
      hyzzl.qz.input<- qz.input(input$scale_hyzzl_qz_input)
      gc.qz.input<- qz.input(input$scale_gc_qz_input)
      ym.qz.input<- qz.input(input$scale_ym_qz_input)
      yy.qz.input<- qz.input(input$scale_yy_qz_input)
      hlfdl.qz.input<- qz.input(input$scale_hlfdl_qz_input)
      kcls.qz.input<- qz.input(input$scale_kcls_qz_input)
      hcls.qz.input<- qz.input(input$scale_hcls_qz_input)
      yylc.qz.input<- qz.input(input$scale_yylc_qz_input)
      cyrysl.qz.input<- qz.input(input$scale_cyrysl_qz_input)
      jcts.qz.input<- qz.input(input$scale_jcts_qz_input)
      
      coor.test.input<- index.1(rate.1(dfscale$hyl))*hyl.qz.input+index.1(rate.1(dfscale$hyzzl))*hyzzl.qz.input+index.1(rate.1(dfscale$gyzjz))*gyzjz.qz.input
      adv.test.input<- index.1(rate.1(dfscale$gc))*gc.qz.input + index.1(rate.1(dfscale$ym))*ym.qz.input+index.1(rate.1(dfscale$yy))*yy.qz.input+index.1(rate.1(dfscale$hlfdl))*hlfdl.qz.input
      delay.test.input<- index.1(rate.1(dfscale$kcls))*kcls.qz.input + index.1(rate.1(dfscale$hcls))*hcls.qz.input+index.1(rate.1(dfscale$yylc))*yylc.qz.input+index.1(rate.1(dfscale$cyrysl))*cyrysl.qz.input+index.1(rate.1(dfscale$jcts))*jcts.qz.input
      
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
          { dfscale<- data.frame(dfscale[1],dfscale[5],dfscale[6],dfscale[7])
          data<-dfscale},
          colnames = c('时间', '先行指数',  '同步指数','滞后指数'),
          rownames = TRUE)}
      
      else{ 
        DT::datatable(
          {data<-dfscale[1:4]},
          colnames = c('时间', '规模合成先行指数',  '规模合成同步指数','规模合成滞后指数'),
          rownames = TRUE)}
    }  
  )
  
  
  #-------------------------------------------------------------------------
  #----------------------------扩散指数------------------------------------
  
  dftrans_DI<-read.csv("Trans_DI.csv",head=T)
  dftrans_DI$tm<-as.Date.POSIXct(dftrans_DI$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  trans_DI.len<-length(dftrans_DI$tm)
  
  dfequip_DI<-read.csv("Equip_DI.csv",head=T)
  dfequip_DI$tm<-as.Date.POSIXct(dfequip_DI$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  equip_DI.len<-length(dfequip_DI$tm)
  
  dfscale_DI<-read.csv("Scale_DI.csv",head=T)
  dfscale_DI$tm<-as.Date.POSIXct(dfscale_DI$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  scale_DI.len<-length(dfscale_DI$tm)
  
  #——————————————1.计算过程———————————————
  
  #-------------1.1 权重计算--------------
  percent.1<- function(x)
  { xlen<- length(x)
  x<- x/max(x)
  x<- -(1/log(xlen))*sum( (x/sum(x))*log((x/sum(x))) )
  x<- 1-x}
  
  #------------(1) 运输先行指标的权重-----
  gc.trans.percent<- percent.1(dftrans_DI$gc)/(percent.1(dftrans_DI$gc)+percent.1(dftrans_DI$ym)+percent.1(dftrans_DI$yy)+percent.1(dftrans_DI$hlfdl))
  ym.trans.percent<- percent.1(dftrans_DI$ym)/(percent.1(dftrans_DI$gc)+percent.1(dftrans_DI$ym)+percent.1(dftrans_DI$yy)+percent.1(dftrans_DI$hlfdl))
  yy.trans.percent<- percent.1(dftrans_DI$yy)/(percent.1(dftrans_DI$gc)+percent.1(dftrans_DI$ym)+percent.1(dftrans_DI$yy)+percent.1(dftrans_DI$hlfdl))
  hlfdl.trans.percent<- percent.1(dftrans_DI$hlfdl)/(percent.1(dftrans_DI$gc)+percent.1(dftrans_DI$ym)+percent.1(dftrans_DI$yy)+percent.1(dftrans_DI$hlfdl))
  
  #------------(2) 运输同步指标的权重---
  hyl.trans.percent<- percent.1(dftrans_DI$hyl)/(percent.1(dftrans_DI$hyl)+percent.1(dftrans_DI$gyzjz)+percent.1(dftrans_DI$hyzzl))
  gyzjz.trans.percent<- percent.1(dftrans_DI$gyzjz)/(percent.1(dftrans_DI$hyl)+percent.1(dftrans_DI$gyzjz)+percent.1(dftrans_DI$hyzzl))
  hyzzl.trans.percent<- percent.1(dftrans_DI$hyzzl)/(percent.1(dftrans_DI$hyl)+percent.1(dftrans_DI$gyzjz)+percent.1(dftrans_DI$hyzzl))
  
  #------------(3) 运输滞后指标的权重---
  kyl.trans.percent<- percent.1(dftrans_DI$kyl)/(percent.1(dftrans_DI$kyl)+percent.1(dftrans_DI$kyzzl)+percent.1(dftrans_DI$gdzctz))
  kyzzl.trans.percent<- percent.1(dftrans_DI$kyzzl)/(percent.1(dftrans_DI$kyl)+percent.1(dftrans_DI$kyzzl)+percent.1(dftrans_DI$gdzctz))
  gdzctz.trans.percent<- percent.1(dftrans_DI$gdzctz)/(percent.1(dftrans_DI$kyl)+percent.1(dftrans_DI$kyzzl)+percent.1(dftrans_DI$gdzctz))
  yylc.trans.percent<- percent.1(dftrans_DI$yylc)/(percent.1(dftrans_DI$kyl)+percent.1(dftrans_DI$kyzzl)+percent.1(dftrans_DI$gdzctz))
  
  #------------(4) 设备先行指标的权----
  gc.equip.percent<- percent.1(dfequip_DI$gc)/(percent.1(dfequip_DI$gc)+percent.1(dfequip_DI$ym)+percent.1(dfequip_DI$yy)+percent.1(dfequip_DI$hlfdl))
  ym.equip.percent<- percent.1(dfequip_DI$ym)/(percent.1(dfequip_DI$gc)+percent.1(dfequip_DI$ym)+percent.1(dfequip_DI$yy)+percent.1(dfequip_DI$hlfdl))
  yy.equip.percent<- percent.1(dfequip_DI$yy)/(percent.1(dfequip_DI$gc)+percent.1(dfequip_DI$ym)+percent.1(dfequip_DI$yy)+percent.1(dfequip_DI$hlfdl))
  hlfdl.equip.percent<- percent.1(dfequip_DI$hlfdl)/(percent.1(dfequip_DI$gc)+percent.1(dfequip_DI$ym)+percent.1(dfequip_DI$yy)+percent.1(dfequip_DI$hlfdl))
  
  #------------(5) 设备同步指标的权重------------------------------------
  jczxzlc.equip.percent<- percent.1(dfequip_DI$jczxzlc)/(percent.1(dfequip_DI$jczxzlc)+percent.1(dfequip_DI$rjyyc))
  rjyyc.equip.percent<- percent.1(dfequip_DI$rjyyc)/(percent.1(dfequip_DI$jczxzlc)+percent.1(dfequip_DI$rjyyc))
  
  #------------(6) 设备滞后指标的权重------------------------------------ 
  rjxzc.equip.percent<- percent.1(dfequip_DI$rjxzc)/(percent.1(dfequip_DI$rjxzc)+percent.1(dfequip_DI$kyjclc)+percent.1(dfequip_DI$hyjclc)+percent.1(dfequip_DI$kcls)+percent.1(dfequip_DI$hcls)+percent.1(dfequip_DI$jcts))
  kyjclc.equip.percent<- percent.1(dfequip_DI$kyjclc)/(percent.1(dfequip_DI$rjxzc)+percent.1(dfequip_DI$kyjclc)+percent.1(dfequip_DI$hyjclc)+percent.1(dfequip_DI$kcls)+percent.1(dfequip_DI$hcls)+percent.1(dfequip_DI$jcts))
  hyjclc.equip.percent<- percent.1(dfequip_DI$hyjclc)/(percent.1(dfequip_DI$rjxzc)+percent.1(dfequip_DI$kyjclc)+percent.1(dfequip_DI$hyjclc)+percent.1(dfequip_DI$kcls)+percent.1(dfequip_DI$hcls)+percent.1(dfequip_DI$jcts))
  kcls.equip.percent<- percent.1(dfequip_DI$kcls)/(percent.1(dfequip_DI$rjxzc)+percent.1(dfequip_DI$kyjclc)+percent.1(dfequip_DI$hyjclc)+percent.1(dfequip_DI$kcls)+percent.1(dfequip_DI$hcls)+percent.1(dfequip_DI$jcts))
  hcls.equip.percent<- percent.1(dfequip_DI$hcls)/(percent.1(dfequip_DI$rjxzc)+percent.1(dfequip_DI$kyjclc)+percent.1(dfequip_DI$hyjclc)+percent.1(dfequip_DI$kcls)+percent.1(dfequip_DI$hcls)+percent.1(dfequip_DI$jcts))
  jcts.equip.percent<- percent.1(dfequip_DI$jcts)/(percent.1(dfequip_DI$rjxzc)+percent.1(dfequip_DI$kyjclc)+percent.1(dfequip_DI$hyjclc)+percent.1(dfequip_DI$kcls)+percent.1(dfequip_DI$hcls)+percent.1(dfequip_DI$jcts))
  
  #------------(7) 规模先行指标的权重------------------------------------ 
  gc.scale.percent<- percent.1(dfscale_DI$gc)/(percent.1(dfscale_DI$gc)+percent.1(dfscale_DI$ym)+percent.1(dfscale_DI$yy)+percent.1(dfscale_DI$hlfdl))
  ym.scale.percent<- percent.1(dfscale_DI$ym)/(percent.1(dfscale_DI$gc)+percent.1(dfscale_DI$ym)+percent.1(dfscale_DI$yy)+percent.1(dfscale_DI$hlfdl))
  yy.scale.percent<- percent.1(dfscale_DI$yy)/(percent.1(dfscale_DI$gc)+percent.1(dfscale_DI$ym)+percent.1(dfscale_DI$yy)+percent.1(dfscale_DI$hlfdl))
  hlfdl.scale.percent<- percent.1(dfscale_DI$hlfdl)/(percent.1(dfscale_DI$gc)+percent.1(dfscale_DI$ym)+percent.1(dfscale_DI$yy)+percent.1(dfscale_DI$hlfdl))
  
  #------------(8) 规模同步指标的权重------------------------------------
  hyl.scale.percent<- percent.1(dfscale_DI$hyl)/(percent.1(dfscale_DI$hyl)+percent.1(dfscale_DI$gyzjz)+percent.1(dfscale_DI$hyzzl))
  gyzjz.scale.percent<- percent.1(dfscale_DI$gyzjz)/(percent.1(dfscale_DI$hyl)+percent.1(dfscale_DI$gyzjz)+percent.1(dfscale_DI$hyzzl))
  hyzzl.scale.percent<- percent.1(dfscale_DI$hyzzl)/(percent.1(dfscale_DI$hyl)+percent.1(dfscale_DI$gyzjz)+percent.1(dfscale_DI$hyzzl))
  
  #------------(9) 规模滞后指标的权重------------------------------------
  kcls.scale.percent<- percent.1(dfscale_DI$kcls)/(percent.1(dfscale_DI$kcls)+percent.1(dfscale_DI$hcls)+percent.1(dfscale_DI$yylc)+percent.1(dfscale_DI$cyrysl)+percent.1(dfscale_DI$jcts))
  hcls.scale.percent<- percent.1(dfscale_DI$hcls)/(percent.1(dfscale_DI$kcls)+percent.1(dfscale_DI$hcls)+percent.1(dfscale_DI$yylc)+percent.1(dfscale_DI$cyrysl)+percent.1(dfscale_DI$jcts))
  yylc.scale.percent<- percent.1(dfscale_DI$yylc)/(percent.1(dfscale_DI$kcls)+percent.1(dfscale_DI$hcls)+percent.1(dfscale_DI$yylc)+percent.1(dfscale_DI$cyrysl)+percent.1(dfscale_DI$jcts))
  cyrysl.scale.percent<- percent.1(dfscale_DI$cyrysl)/(percent.1(dfscale_DI$kcls)+percent.1(dfscale_DI$hcls)+percent.1(dfscale_DI$yylc)+percent.1(dfscale_DI$cyrysl)+percent.1(dfscale_DI$jcts))
  jcts.scale.percent<- percent.1(dfscale_DI$jcts)/(percent.1(dfscale_DI$kcls)+percent.1(dfscale_DI$hcls)+percent.1(dfscale_DI$yylc)+percent.1(dfscale_DI$cyrysl)+percent.1(dfscale_DI$jcts))
  
  
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
  percent.input<- function(a)
  {a<- as.numeric(a)/100} 
  
  #--------------(1) 运输扩散指数计算---------------------------
  trans<-read.csv("Trans_DI.csv",head=T)#计算运输扩散指数
  tm1<-trans$tm[2:length(trans$tm)]
  c1<-function1(trans$gc)
  c2<-function1(trans$ym)
  c3<-function1(trans$yy)
  c4<-function1(trans$hlfdl)
  c5<-function1(trans$hyzzl)
  c6<-function1(trans$hyl)
  c7<-function1(trans$gyzjz)
  c8<-function1(trans$kyl)
  c9<-function1(trans$kyzzl)
  c10<-function1(trans$gdzctz)
  c11<-function1(trans$yylc)
  
  DIx_trans<-gc.trans.percent*c1+ym.trans.percent*c2+yy.trans.percent*c3+hlfdl.trans.percent*c4#运输扩散先行指数
  DIt_trans<-hyzzl.trans.percent*c5+hyl.trans.percent*c6+gyzjz.trans.percent*c7#运输扩散同步指数
  DIz_trans<-kyl.trans.percent*c8+kyzzl.trans.percent*c9+gdzctz.trans.percent*c10+yylc.trans.percent*c11#运输扩散滞后指数
  
  DI_trans<-data.frame(tm1,DIx_trans,DIt_trans,DIz_trans)#存储所有指数计算结果的数据框
  DI_trans$tm1<-as.Date.POSIXct(DI_trans$tm1,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
  write.csv(DI_trans,file="DI_Trans.csv",row.names = FALSE)
  
  #-------------(2) 设备扩散指数计算-----------------------------
  equip<-read.csv("Equip_DI.csv",head=T)#计算设备扩散指数
  tm2<-equip$tm[2:length(equip$tm)]
  c12<-function1(equip$jczxzlc)
  c13<-function1(equip$rjyyc)
  c14<-function1(equip$rjxzc)
  c15<-function1(equip$kyjclc)
  c16<-function1(equip$hyjclc)
  c17<-function1(equip$kcls)
  c18<-function1(equip$hcls)
  c19<-function1(equip$jcts)
  c22<-function1(equip$gc)
  c23<-function1(equip$ym)
  c24<-function1(equip$yy)
  c25<-function1(equip$hlfdl)
  DIx_equip<-gc.equip.percent*c22+ym.equip.percent*c23+yy.equip.percent*c24+hlfdl.equip.percent*c25 #设备扩散先行指数
  DIt_equip<-jczxzlc.equip.percent*c12+rjyyc.equip.percent*c13#设备扩散同步指数
  DIz_equip<-rjxzc.equip.percent*c14+kyjclc.equip.percent*c15+hyjclc.equip.percent*c16+kcls.equip.percent*c17+hcls.equip.percent*c18+jcts.equip.percent*c19#设备扩散滞后指数
  
  DI_equip<-data.frame(tm2,DIx_equip,DIt_equip,DIz_equip)#存储所有指数计算结果的数据框
  DI_equip$tm2<-as.Date.POSIXct(DI_equip$tm2,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
  write.csv(DI_equip,file="DI_Equip.csv",row.names = FALSE)
  
  #-------------(3) 规模扩散指数计算-----------------------------
  scale<-read.csv("Scale_DI.csv",head=T)#计算规模扩散指数
  tm3<-scale$tm[2:length(scale$tm)]
  c20<-function1(scale$cyrysl)
  c21<-function1(scale$yylc)
  c26<-function1(scale$gc)
  c27<-function1(scale$ym)
  c28<-function1(scale$yy)
  c29<-function1(scale$hlfdl)
  c30<-function1(scale$hyzzl)
  c31<-function1(scale$hyl)
  c32<-function1(scale$gyzjz)
  DIx_scale<-gc.scale.percent*c26+ym.scale.percent*c27+yy.scale.percent*c28+hlfdl.scale.percent*c29 #规模扩散先行指数
  DIt_scale<-hyzzl.scale.percent*c30+hyl.scale.percent*c31+gyzjz.scale.percent*c32#规模扩散同步指数
  DIz_scale<-kcls.scale.percent*c17+hcls.scale.percent*c18+yylc.scale.percent*c21+cyrysl.scale.percent*c20+jcts.scale.percent*c19#规模扩散滞后指数
  
  DI_scale<-data.frame(tm3,DIx_scale,DIt_scale,DIz_scale)#存储所有指数计算结果的数据框
  DI_scale$tm3<-as.Date.POSIXct(DI_scale$tm3,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
  write.csv(DI_scale,file="DI_Scale.csv",row.names = FALSE)
  
  
  
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
    write.csv(DI_trans_input,file="DI_Trans_Input.csv",row.names = FALSE)    
    
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
    DI_equip_input$tm2<-as.Date.POSIXct(DI_equip_input$tm2,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    write.csv(DI_equip_input,file="DI_Equip_Input.csv",row.names = FALSE) 
    
    #----------(2)设备扩散指数--默认权重计算的画图---------------------
    DI_equip.len<-length(DI_equip_input$tm2)
    
    if(input$year_start_equip_ID> input$year_end_equip_ID)  {
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
    
    DIx_scale_input<- gc.input*c26 + ym.input*c27+yy.input*c28+hlfdl.input*c29
    DIt_scale_input<- hyzzl.input*c30+hyl.input*c31+gyzjz.input*c32
    DIz_scale_input<- kcls.input*c17+hcls.input*c18+yylc.input*c21+cyrysl.input*c20+jcts.input*c19
    
    DI_scale_input<-data.frame(tm3,DIx_scale_input, DIt_scale_input,DIz_scale_input)#存储所有指数计算结果的数据框
    DI_scale_input$tm3<-as.Date.POSIXct(DI_scale_input$tm3,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    write.csv(DI_scale_input,file="DI_Scale_Input.csv",row.names = FALSE)    
    
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
    DI_trans_input$tm1<-as.Date.POSIXct(DI_trans_input$tm1,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    write.csv(DI_trans_input,file="DI_Trans_Input.csv",row.names = FALSE)
    
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
    DI_equip_input$tm2<-as.Date.POSIXct(DI_equip_input$tm2,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    write.csv(DI_equip_input,file="DI_Equip_Input.csv",row.names = FALSE)
    
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
    
    DIx_scale_input<- gc.input*c26 + ym.input*c27+yy.input*c28+hlfdl.input*c29
    DIt_scale_input<- hyzzl.input*c30+hyl.input*c31+gyzjz.input*c32
    DIz_scale_input<- kcls.input*c17+hcls.input*c18+yylc.input*c21+cyrysl.input*c20+jcts.input*c19
    
    DI_scale_input<-data.frame(tm3,DIx_scale_input, DIt_scale_input,DIz_scale_input)#存储所有指数计算结果的数据框
    DI_scale_input$tm3<-as.Date.POSIXct(DI_scale_input$tm3,"%Y%m%d",tz=Sys.timezone(location = TRUE))#转换时间格式  
    write.csv(DI_scale_input,file="DI_Scale_Input.csv",row.names = FALSE)  
    
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
  liaozili<-read.csv("index-black.csv",head=T)# 读取黑货原始数据到变量liaozili中
  liaozili$tm<-as.Date.POSIXct(liaozili$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  liaozili_len<-length(liaozili$tm)
  #-----增长率--------------
  
  coal1<-rate1(liaozili$coal) #原煤增长率
  oil1<- rate1(liaozili$oil)#原油增长率
  metal1<-rate1(liaozili$metal)#金属矿石增长率
  iron1<-rate1(liaozili$iron)#钢铁增长率
  mine1<-rate1(liaozili$mine)#矿建增长率
  
  #-----对称变化率--------------
  
  coal2<- rate2(coal1,0)#原煤对称变化率
  oil2<-rate2(oil1,0)#原油对称变化率
  metal2<-rate2(metal1,0)#金属矿石对称变化率
  iron2<-rate2(iron1,0)#钢铁对称变化率
  mine2<-rate2(mine1,0)#矿建对称变化率
  
  
  #-----标准化对称变换率--------
  
  coal3<-rate3(coal1,coal2)#原煤标准化对称变换率
  oil3<-rate3(oil1,oil2)#原油标准化对称变换率
  metal3<-rate3(metal1,metal2)#金属矿石标准化对称变换率
  iron3<-rate3(iron1,iron2)#钢铁标准化对称变换率
  mine3<-rate3(mine1,mine2)#矿建标准化对称变换率
  
  
  # -----黑货指数：图像显示--------
  
  output$heihuo_index<- renderPlot( {
    lx1<-input$weightcoal_input/100
    lx2<-input$weightoil_input/100
    lx3<-input$weightmetal_input/100
    lx4<-input$weightiron_input/100
    lx5<-input$weightmine_input/100
    # 原煤、原油、金属矿石、钢铁、矿建权重输入
    
    
    dfweight<- data.frame(lx1,lx2,lx3,lx4,lx5)#将输入权重值写入数据框
    dfinitial <- data.frame(0.6693,0.0522,0.1497,0.0802,0.0485)# 权重值初始化
    
    averagerate<-coal3*lx1 +oil3*lx2 + metal3*lx3 + iron3*lx4+ mine3*lx5  #平均变换率
    liaozili$heihuo_index<-index(averagerate,0)#计算黑货指数并写入表格中
    
    if(input$liaozili_year_start> input$liaozili_year_end)  {
      p<-ggplot(liaozili,x=c(liaozili$tm[1],liaozili$tm[liaozili_len]),aes(x=tm,y=0))
    }
    else{
      dfsub<-subset(liaozili,(substr(liaozili$tm,1,4)>=input$liaozili_year_start) )
      dfsub<-subset(dfsub,(substr(dfsub$tm,1,4)<=input$liaozili_year_end))
      p<-ggplot(dfsub,x=c(dfsub$tm[1],dfsub$tm[liaozili_len]),aes(x=tm,y=80))
    }
    
    
    p<-p+geom_line(aes(x=tm,y=dfsub$heihuo_index),color="blue",size=0.6)#+geom_point(aes(x=tm,y=dfsub$heihuo_index),size=3,shape=22,colour="darkred",fill="pink",position="dodge")
    p+ylab("黑货指数")+xlab("时间")+geom_line()+ylim(70,105)
    
    p<-p+geom_line(aes(x=tm,y=dfsub$heihuo_index),color="blue",size=0.6)#+geom_point(aes(x=tm,y=dfsub$heihuo_index),size=3,shape=22,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    p+ylab("黑货指数")+xlab("时间")+geom_line()+ylim(70,110)
    
  })
  
  # -----黑货指数：数据显示--------
  output$heihuotable<-DT::renderDataTable({
    
    lx1<-input$weightcoal_input/100
    lx2<-input$weightoil_input/100
    lx3<-input$weightmetal_input/100
    lx4<-input$weightiron_input/100
    lx5<-input$weightmine_input/100
    # 原煤、原油、金属矿石、钢铁、矿建权重输入
    
    averagerate<-coal3*lx1 +oil3*lx2 + metal3*lx3 + iron3*lx4+ mine3*lx5
    liaozili$heihuo_index<-round(index(averagerate,0),2)
    
    DT::datatable(
      {data<-liaozili},
      colnames = c('时间','总量','煤','石油','金属矿石','钢铁','矿建','黑货指数'),
      rownames = TRUE,
      style="bootstrap")
  } )
  
  
  #-----白货指数计算--------------------------------------------------------------------------------------------
  liaozili2<-read.csv("index-white.csv",head=T)#读取白货原始数据到变量liaozili2中
  liaozili2$tm<-as.Date.POSIXct(liaozili2$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
  liaozili_len1<-length(liaozili2$tm)
  
  
  #-----增长率--------------调用上面公式
  machinery1<-rate1(liaozili2$machinery)#工业机械增长率
  electronic1<-rate1(liaozili2$electronic)#电子电气增长率
  agricultural1<-rate1(liaozili2$agricultural)#农副产品增长率
  food1<-rate1(liaozili2$food)#饮食烟草增长率
  education1<-rate1(liaozili2$education)#文教用增长率
  ltl1<-rate1(liaozili2$ltl)#零担增长率
  container1<-rate1(liaozili2$container)#集装箱增长率
  
  
  
  #-----对称变化率--------------调用上面公式
  machinery2<- rate2( machinery1,0)#工业机械对称变化率
  electronic2<-rate2(electronic1,0)#电子电气对称变化率
  agricultural2<-rate2(agricultural1,0)#农副产品对称变化率
  food2<-rate2(food1,0)#饮食烟草对称变化率
  education2<-rate2(education1,0)#文教用对称变化率
  ltl2<-rate2(ltl1,0)#零担对称变化率
  container2<-rate2(container1,0)#集装箱对称变化率
  
  #-----标准化对称变换率--------调用上面公式
  machinery3<- rate3(machinery1, machinery2)#工业机械标准对称变化率
  electronic3<- rate3( electronic1, electronic2)#电子电气标准对称变化率
  agricultural3<- rate3( agricultural1, agricultural2)#农副产品标准对称变化率
  food3<- rate3( food1, food2)#饮食烟草标准对称变化率
  education3<- rate3( education1, education2)#文教用品标准对称变化率
  ltl3<- rate3( ltl1,ltl2)#零担标准对称变化率
  container3<- rate3( container1, container2)#集装箱标准对称变化率
  
  
  
  # -----白货指数：图像显示--------
  
  output$baihuo_index<- renderPlot( {
    
    lz_x1<-input$weightmachinery_input/100#工业机械输入权重
    lz_x2<-input$weightelectronic_input/100#电子电气输入权重
    lz_x3<-input$weightagricultural_input/100#农副产品输入权重
    lz_x4<-input$weightfood_input/100#饮食烟草输入权重
    lz_x5<-input$weighteducation_input/100#文教用品输入权重
    lz_x6<-input$weightltl_input/100#零担标准输入权重
    lz_x7<-input$weightcontainer_input/100#集装箱输入权重
    
    dfweight1<- data.frame( lz_x1, lz_x2, lz_x3, lz_x4, lz_x5, lz_x6, lz_x7)#权重读入数据框
    dfinitial1<- data.frame(0.181,0.188,0.111,0.1719,0.1777,0.0429,0.1275) #权重初始化
    
    
    averagerate1<-machinery3* lz_x1 +electronic3* lz_x2 + agricultural3* lz_x3 + food3* lz_x4+ education3*lz_x5+ltl3*lz_x6+container3* lz_x7#白货平均变化率
    liaozili2$baihuo_index<-index(averagerate1,0)#白货指数
    
    if(input$liaozili_year2_start> input$liaozili_year2_end)  {
      p<-ggplot(liaozili2,x=c(liaozili2$tm[1],liaozili2$tm[liaozili_len1]),aes(x=tm,y=100))
    }
    else{
      dfsub<-subset(liaozili2,(substr(liaozili2$tm,1,4)>=input$liaozili_year2_start) )
      dfsub<-subset(dfsub,(substr(dfsub$tm,1,4)<=input$liaozili_year2_end))
      p<-ggplot(dfsub,x=c(dfsub$tm,dfsub$tm),aes(x=tm,y=90))
    }
    
    p<-p+geom_line(aes(x=tm,y=dfsub$baihuo_index),color="red",size=0.6)
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
    # 工业机械、电子电气、农副产品、饮食烟草、文教用品，零担，集装箱权重输入
    
    averagerate1<-machinery3* lz_x1 +electronic3* lz_x2 + agricultural3* lz_x3 + food3*lz_x4+ education3*lz_x5+ltl3*lz_x6+container3*lz_x7
    liaozili2$baihuo_index<-round(index(averagerate1,0),2)
    
    DT::datatable(
      {data<-liaozili2},
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
  #---------------固定资产投资-营业里程---------------------------------
  #operatingmileage-----营业里程
  operatingmileage_df<-read.csv("营业里程.csv",head=T)
  
  operatingmileage_olsRegModel<-lm(asset~operatingmileage,data=operatingmileage_df)
  
  operatingmileage_df$linearRegPred<-as.integer(predict(operatingmileage_olsRegModel,newdata=operatingmileage_df))
  operatingmileage_rfRegModel<-randomForest(asset~operatingmileage,data=operatingmileage_df,importance=T, ntree=100,type="regression")
  operatingmileage_df$frRegPred<-as.integer(predict(operatingmileage_rfRegModel,operatingmileage_df))
  operatingmileage_svmRegModel<-svm(asset~operatingmileage,data=operatingmileage_df,type="eps-regression",cross=dim(operatingmileage_df)[1]/2)
  operatingmileage_df$svmRegPred<-as.integer(predict(operatingmileage_svmRegModel,operatingmileage_df))
  operatingmileage_len<-length(operatingmileage_df$tm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    operatingmileage_len=dim(xdata)[1]
    operatingmileage_plt<-ggplot(db,x=c(xdata[1],xdata[operatingmileage_len]),aes(x=xdata,y=ydata),color="red")
    return(operatingmileage_plt)
  }
  output$operatingmileage_linearplot <- renderPlot( {
    
    if(input$operatingmileage_year_start> input$operatingmileage_year_end)  {
      
      if (input$operatingmileage_stat_data) {
        operatingmileage_p<-plotCurve(operatingmileage_df,operatingmileage_df$tm,operatingmileage_df$asset)
      }
      else
      {
        operatingmileage_p<-plotCurve(operatingmileage_df,operatingmileage_df$tm,operatingmileage_df$linearRegPred)
      }
    }
    else{
      operatingmileage_dfsub<-subset(operatingmileage_df,substr(operatingmileage_df$tm,1,4)>=input$operatingmileage_year_start) 
      operatingmileage_dfsub<-subset(operatingmileage_dfsub,substr(operatingmileage_dfsub$tm,1,4)<=input$operatingmileage_year_end)
      if (input$operatingmileage_stat_data) {
        operatingmileage_p<-plotCurve(operatingmileage_dfsub,operatingmileage_dfsub$tm,operatingmileage_dfsub$asset)
      }
      else
      {
        operatingmileage_p<-plotCurve(operatingmileage_dfsub,operatingmileage_dfsub$tm,operatingmileage_dfsub$linearRegPred)
      }
    }
    if(input$operatingmileage_predict_data){
      
      operatingmileage_p<-operatingmileage_p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$operatingmileage_stat_data) {
      operatingmileage_p<-operatingmileage_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    operatingmileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  output$operatingmileage_asset_output<-renderText({
    operatingmileage_x<-as.numeric(input$operatingmileage_input)
    operatingmileage<-c(operatingmileage_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,operatingmileage)
    operatingmileage_pred<-as.integer(predict(operatingmileage_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",operatingmileage_pred[1],"预测区间95%：(",operatingmileage_pred[2],",",operatingmileage_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$operatingmileage_asset_FRR<-renderText({
    operatingmileage_x<-as.numeric(input$operatingmileage_input)
    operatingmileage<-c(operatingmileage_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,operatingmileage)
    railasset<-predict(operatingmileage_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railasset[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$operatingmileage_asset_zhi<-renderText({
    operatingmileage_x<-as.numeric(input$operatingmileage_input)
    operatingmileage<-c(operatingmileage_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,operatingmileage)
    operatingmileage_pred<-as.integer(predict(operatingmileage_svmRegModel,inputdata))
    
    paste("支持向量机预测：",operatingmileage_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$operatingmileage_rfplot <- renderPlot( {
    
    if(input$operatingmileage_year_start> input$operatingmileage_year_end)  {
      
      if (input$operatingmileage_stat_data) {
        operatingmileage_p<-plotCurve(operatingmileage_df,operatingmileage_df$tm,operatingmileage_df$asset)
      }
      else
      {
        operatingmileage_p<-plotCurve(operatingmileage_df,operatingmileage_df$tm,operatingmileage_df$frRegPred)
      }
    }
    else{
      operatingmileage_dfsub<-subset(operatingmileage_df,substr(operatingmileage_df$tm,1,4)>=input$operatingmileage_year_start) 
      operatingmileage_dfsub<-subset(operatingmileage_dfsub,substr(operatingmileage_df$tm,1,4)<=input$operatingmileage_year_end)
      if (input$operatingmileage_stat_data) {
        operatingmileage_p<-plotCurve(operatingmileage_dfsub,operatingmileage_dfsub$tm,operatingmileage_dfsub$asset)
      }
      else
      {
        operatingmileage_p<-plotCurve(operatingmileage_dfsub,operatingmileage_dfsub$tm,operatingmileage_dfsub$frRegPred)
      }
    }
    
    if(input$operatingmileage_predict_data){
      operatingmileage_p<-operatingmileage_p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$operatingmileage_stat_data) {
      operatingmileage_p<-operatingmileage_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    operatingmileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$operatingmileage_svmplot <- renderPlot( {
    
    if(input$operatingmileage_year_start> input$operatingmileage_year_end)  {
      
      if (input$operatingmileage_stat_data) {
        operatingmileage_p<-plotCurve(operatingmileage_df,operatingmileage_df$tm,operatingmileage_df$asset)
      }
      else
      {
        operatingmileage_p<-plotCurve(operatingmileage_df,operatingmileage_df$tm,operatingmileage_df$svmRegPred)
      }
    }
    else{
      operatingmileage_dfsub<-subset(operatingmileage_df,substr(operatingmileage_df$tm,1,4)>=input$operatingmileage_year_start) 
      operatingmileage_dfsub<-subset(operatingmileage_dfsub,substr(operatingmileage_dfsub$tm,1,4)<=input$operatingmileage_year_end)
      if (input$operatingmileage_stat_data) {
        operatingmileage_p<-plotCurve(operatingmileage_dfsub,operatingmileage_dfsub$tm,operatingmileage_dfsub$asset)
      }
      else
      {
        operatingmileage_p<-plotCurve(operatingmileage_dfsub,operatingmileage_dfsub$tm,operatingmileage_dfsub$svmRegPred)
      }
    }
    if(input$operatingmileage_predict_data){
      operatingmileage_p<-operatingmileage_p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$operatingmileage_stat_data) {
      operatingmileage_p<-operatingmileage_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    operatingmileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  
  output$operatingmileage_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        operatingmileage_data<-operatingmileage_df
      } , 
      colnames = c('序号', '时间', '固定资产投资（亿元）', '营业里程','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
      rownames = TRUE)
  )
  
  
  #--------------------适配性研究-----------------------------
  #----------------固定资产-铺轨里程--------------------------
  #tl_mileage-------铺轨里程 newline_tracklaying_mileage------新线铺轨里程  oldline_tracklaying_mileage--------旧线铺轨里程
  #df_yearly<-read.csv("rawdata_property.csv",head=T)
  tracklaying_mileage_olsRegModel<-lm(fixed_assets_investment~I(newline_tracklaying_mileage+oldline_tracklaying_mileage)+0,data=df_yearly)
  df_yearly$linearRegPred<-as.integer(predict(tracklaying_mileage_olsRegModel,newdata=df_yearly))
  tracklaying_mileage_rfRegModel<-randomForest(fixed_assets_investment~newline_tracklaying_mileage+oldline_tracklaying_mileage,data=df_yearly,importance=T, ntree=100,type="regression")
  df_yearly$frRegPred<-as.integer(predict(tracklaying_mileage_rfRegModel,df_yearly))
  tracklaying_mileage_svmRegModel<-svm(fixed_assets_investment~newline_tracklaying_mileage+oldline_tracklaying_mileage,data=df_yearly,type="eps-regression",cross=dim(df_yearly)[1]/2)
  df_yearly$svmRegPred<-as.integer(predict(tracklaying_mileage_svmRegModel,df_yearly))
  tracklaying_mileage_len<-length(df_yearly$tm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    tracklaying_mileage_len=dim(xdata)[1]
    tracklaying_mileage_plt<-ggplot(db,x=c(xdata[1],xdata[tracklaying_mileage_len]),aes(x=xdata,y=ydata,group=1),color="red")
    return(tracklaying_mileage_plt)
  }
  output$tracklaying_mileage_linearplot <- renderPlot( {
    
    if(input$tracklaying_mileage_year_start> input$tracklaying_mileage_year_end)  {
      
      if (input$tracklaying_mileage_stat_data) {
        tracklaying_mileage_p<-plotCurve(df_yearly,df_yearly$tm,df_yearly$fixed_assets_investment)
      }
      else
      {
        tracklaying_mileage_p<-plotCurve(df_yearly,df_yearly$tm,df_yearly$linearRegPred)
      }
    }
    else{
      df_yearlysub<-subset(df_yearly,substr(df_yearly$tm,1,4)>=input$tracklaying_mileage_year_start) 
      df_yearlysub<-subset(df_yearlysub,substr(df_yearlysub$tm,1,4)<=input$tracklaying_mileage_year_end)
      if (input$tracklaying_mileage_stat_data) {
        tracklaying_mileage_p<-plotCurve(df_yearlysub,df_yearlysub$tm,df_yearlysub$fixed_assets_investment)
      }
      else
      {
        tracklaying_mileage_p<-plotCurve(df_yearlysub,df_yearlysub$tm,df_yearlysub$linearRegPred)
      }
    }
    if(input$tracklaying_mileage_predict_data){
      
      tracklaying_mileage_p<-tracklaying_mileage_p+geom_line(aes(x=tm,y=linearRegPred,group=1),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred,group=1),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-tracklaying_mileage_p+geom_point(aes(x=tm,y=fixed_assets_investment,group=1),color="red",size=3,shape=21)
    }
    tracklaying_mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  output$tracklaying_mileage_output<-renderText({
    tracklaying_mileage_x1<-as.numeric(input$newline_tracklaying_mileage_input)
    tracklaying_mileage_x2<-as.numeric(input$oldline_tracklaying_mileage_input)
    newline_tracklaying_mileage<-c(tracklaying_mileage_x1)
    oldline_tracklaying_mileage<-c(tracklaying_mileage_x2)
    tm<-c(2016)
    fixed_assets_investment<-c(0)
    inputdata<-data.frame(tm,fixed_assets_investment,newline_tracklaying_mileage,oldline_tracklaying_mileage)
    tracklaying_mileage_pred<-as.integer(predict(tracklaying_mileage_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",tracklaying_mileage_pred[1],"预测区间95%：(",tracklaying_mileage_pred[2],",",tracklaying_mileage_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$tracklaying_mileage_FRR<-renderText({
    tracklaying_mileage_x1<-as.numeric(input$newline_tracklaying_mileage_input)
    tracklaying_mileage_x2<-as.numeric(input$oldline_tracklaying_mileage_input)
    newline_tracklaying_mileage<-c(tracklaying_mileage_x1)
    oldline_tracklaying_mileage<-c(tracklaying_mileage_x2)
    tm<-c(2016)
    fixed_assets_investment<-c(0)
    inputdata<-data.frame(tm,fixed_assets_investment,newline_tracklaying_mileage,oldline_tracklaying_mileage)
    tracklaying_mileage_pred<-predict(tracklaying_mileage_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(tracklaying_mileage_pred[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$tracklaying_mileage_zhi<-renderText({
    tracklaying_mileage_x1<-as.numeric(input$newline_tracklaying_mileage_input)
    tracklaying_mileage_x2<-as.numeric(input$oldline_tracklaying_mileage_input)
    newline_tracklaying_mileage<-c(tracklaying_mileage_x1)
    oldline_tracklaying_mileage<-c(tracklaying_mileage_x2)
    tm<-c(2016)
    fixed_assets_investment<-c(0)
    inputdata<-data.frame(tm,fixed_assets_investment,newline_tracklaying_mileage,oldline_tracklaying_mileage)
    tracklaying_mileage_pred<-as.integer(predict(tracklaying_mileage_svmRegModel,inputdata))
    
    paste("支持向量机预测：",tracklaying_mileage_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$tracklaying_mileage_rfplot <- renderPlot( {
    
    if(input$tracklaying_mileage_year_start> input$tracklaying_mileage_year_end)  {
      
      if (input$tracklaying_mileage_stat_data) {
        tracklaying_mileage_p<-plotCurve(df_yearly,df_yearly$tm,df_yearly$fixed_assets_investment)
      }
      else
      {
        tracklaying_mileage_p<-plotCurve(df_yearly,df_yearly$tm,df_yearly$frRegPred)
      }
    }
    else{
      df_yearlysub<-subset(df_yearly,substr(df_yearly$tm,1,4)>=input$tracklaying_mileage_year_start) 
      df_yearlysub<-subset(df_yearlysub,substr(df_yearlysub$tm,1,4)<=input$tracklaying_mileage_year_end)
      if (input$tracklaying_mileage_stat_data) {
        tracklaying_mileage_p<-plotCurve(df_yearlysub,df_yearlysub$tm,df_yearlysub$fixed_assets_investment)
      }
      else
      {
        tracklaying_mileage_p<-plotCurve(df_yearlysub,df_yearlysub$tm,df_yearlysub$frRegPred)
      }
    }
    
    if(input$tracklaying_mileage_predict_data){
      tracklaying_mileage_p<-tracklaying_mileage_p+geom_line(aes(x=tm,y=frRegPred,group=1),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred,group=1),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-tracklaying_mileage_p+geom_point(aes(x=tm,y=fixed_assets_investment,group=1),color="red",size=3,shape=21)
    }
    tracklaying_mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$tracklaying_mileage_svmplot <- renderPlot( {
    
    if(input$tracklaying_mileage_year_start> input$tracklaying_mileage_year_end)  {
      
      if (input$tracklaying_mileage_stat_data) {
        tracklaying_mileage_p<-plotCurve(df_yearly,df_yearly$tm,df_yearly$fixed_assets_investment)
      }
      else
      {
        tracklaying_mileage_p<-plotCurve(df_yearly,df_yearly$tm,df_yearly$svmRegPred)
      }
    }
    else{
      df_yearlysub<-subset(df_yearly,substr(df_yearly$tm,1,4)>=input$tracklaying_mileage_year_start) 
      df_yearlysub<-subset(df_yearlysub,substr(df_yearlysub$tm,1,4)<=input$tracklaying_mileage_year_end)
      if (input$tracklaying_mileage_stat_data) {
        tracklaying_mileage_p<-plotCurve(df_yearlysub,df_yearlysub$tm,df_yearlysub$fixed_assets_investment)
      }
      else
      {
        tracklaying_mileage_p<-plotCurve(df_yearlysub,df_yearlysub$tm,df_yearlysub$svmRegPred)
      }
    }
    if(input$tracklaying_mileage_predict_data){
      tracklaying_mileage_p<-tracklaying_mileage_p+geom_line(aes(x=tm,y=svmRegPred,group=1),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred,group=1),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$tracklaying_mileage_stat_data) {
      tracklaying_mileage_p<-tracklaying_mileage_p+geom_point(aes(x=tm,y=fixed_assets_investment,group=1),color="red",size=3,shape=21)
    }
    tracklaying_mileage_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  
  
  
  output$tracklaying_mileage_table<-DT::renderDataTable(
    DT::datatable(
{
  
  tracklaying_mileage_data<-df_yearly
} , 
colnames = c('序号', '时间', '客车辆数', '机车台数','货车车辆','动车组数','固定资产投资','从业人员数量','新线铺轨里程','复线铺轨里程','客车车辆增加量','动车组增加量','固定资产投资增量','营业里程','日均运用车（辆）','日均现在车（辆）','客运机车日车里程（km）','货运机车日车里程（km）','机车总行走里程（百万km）','成品钢材产量','原煤产量','原油加工量','火力发电量','工业增加值','货运量_28个品类相加的','货运量（万吨）','货运周转量','客运量','铁路客运量(万人)','年末总人口(万人)','国内生产总值(亿元)','城镇居民家庭人均可支配收入(元)','民用航空客运量(万人)','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
rownames = TRUE)
  ) 

  
  #--------------------------------------------------------------------------
  #----------------固定资产-动车组数量适配性研究----------------------------
  #emu-------动车组增加数量
  emu_df<-read.csv("动车增加数量.csv",head=T)
  emu_df$tm<-as.Date.POSIXct(emu_df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
  emu_olsRegModel<-lm(asset~emu,data=emu_df)
  #bound<-(predict(olsRegModel,newdata=df,interval = "prediction"))  
  #df$linearRegPred<-as.integer(bound[,1])
  emu_df$linearRegPred<-as.integer(predict(emu_olsRegModel,newdata=emu_df))
  emu_rfRegModel<-randomForest(asset~emu,data=emu_df,importance=T, ntree=100,type="regression")
  emu_df$frRegPred<-as.integer(predict(emu_rfRegModel,emu_df))
  emu_svmRegModel<-svm(asset~emu,data=emu_df,type="eps-regression",cross=dim(emu_df)[1]/2)
  emu_df$svmRegPred<-as.integer(predict(emu_svmRegModel,emu_df))
  emu_len<-length(emu_df$tm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    emu_len=dim(xdata)[1]
    emu_plt<-ggplot(db,x=c(xdata[1],xdata[emu_len]),aes(x=xdata,y=ydata),color="red")
    return(emu_plt)
  }
  output$emu_asset_linearplot <- renderPlot( {
    
    if(input$emu_year_start> input$emu_year_end)  {
      
      if (input$emu_stat_data) {
        emu_p<-plotCurve(emu_df,emu_df$tm,emu_df$asset)
      }
      else
      {
        emu_p<-plotCurve(emu_df,emu_df$tm,emu_df$linearRegPred)
      }
    }
    else{
      emu_dfsub<-subset(emu_df,substr(emu_df$tm,1,4)>=input$emu_year_start) 
      emu_dfsub<-subset(emu_dfsub,substr(emu_dfsub$tm,1,4)<=input$emu_year_end)
      if (input$emu_stat_data) {
        emu_p<-plotCurve(emu_dfsub,emu_dfsub$tm,emu_dfsub$asset)
      }
      else
      {
        emu_p<-plotCurve(emu_dfsub,emu_dfsub$tm,emu_dfsub$linearRegPred)
      }
    }
    if(input$emu_predict_data){
      
      emu_p<-emu_p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$emu_stat_data) {
      emu_p<-emu_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    emu_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  output$emu_asset_output<-renderText({
    emu_x<-as.numeric(input$emu_input)
    emu<-c(emu_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,emu)
    emu_pred<-as.integer(predict(emu_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",emu_pred[1],"预测区间95%：(",emu_pred[2],",",emu_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$emu_asset_FRR<-renderText({
    emu_x<-as.numeric(input$emu_input)
    emu<-c(emu_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,emu)
    railasset<-predict(emu_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railasset[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$emu_asset_zhi<-renderText({
    emu_x<-as.numeric(input$emu_input)
    emu<-c(emu_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,emu)
    emu_pred<-as.integer(predict(emu_svmRegModel,inputdata))
    
    paste("支持向量机预测：",emu_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$emu_asset_rfplot <- renderPlot( {
    
    if(input$emu_year_start> input$emu_year_end)  {
      
      if (input$emu_stat_data) {
        emu_p<-plotCurve(emu_df,emu_df$tm,emu_df$asset)
      }
      else
      {
        emu_p<-plotCurve(emu_df,emu_df$tm,emu_df$frRegPred)
      }
    }
    else{
      emu_dfsub<-subset(emu_df,substr(emu_df$tm,1,4)>=input$emu_year_start) 
      emu_dfsub<-subset(emu_dfsub,substr(emu_df$tm,1,4)<=input$emu_year_end)
      if (input$emu_stat_data) {
        emu_p<-plotCurve(emu_dfsub,emu_dfsub$tm,emu_dfsub$asset)
      }
      else
      {
        emu_p<-plotCurve(emu_dfsub,emu_dfsub$tm,emu_dfsub$frRegPred)
      }
    }
    
    if(input$emu_predict_data){
      emu_p<-emu_p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$emu_stat_data) {
      emu_p<-emu_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    emu_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$emu_asset_svmplot <- renderPlot( {
    
    if(input$emu_year_start> input$emu_year_end)  {
      
      if (input$emu_stat_data) {
        emu_p<-plotCurve(emu_df,emu_df$tm,emu_df$asset)
      }
      else
      {
        emu_p<-plotCurve(emu_df,emu_df$tm,emu_df$svmRegPred)
      }
    }
    else{
      emu_dfsub<-subset(emu_df,substr(emu_df$tm,1,4)>=input$emu_year_start) 
      emu_dfsub<-subset(emu_dfsub,substr(emu_dfsub$tm,1,4)<=input$emu_year_end)
      if (input$emu_stat_data) {
        emu_p<-plotCurve(emu_dfsub,emu_dfsub$tm,emu_dfsub$asset)
      }
      else
      {
        emu_p<-plotCurve(emu_dfsub,emu_dfsub$tm,emu_dfsub$svmRegPred)
      }
    }
    if(input$emu_predict_data){
      emu_p<-emu_p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$emu_stat_data) {
      emu_p<-emu_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    emu_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  
  
  output$emu_asset_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        emu_data<-emu_df
      } , 
      colnames = c('序号', '时间', '固定资产投资（亿元）', '动车新增数量','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
      rownames = TRUE)
  )
  #-----------------------------------------
  #————————————固定资产-客车数量适配性分析————————————
  investment_df<-read.csv("investment-passenger.csv",head=T)
  #-------------olsRegModel为多元回归模型
  ptrainolsRegModel<-lm(investment~ptrain,data=investment_df)
  #bound<-(predict(olsRegModel,newdata=investment_df,interval = "prediction"))  #<-----------回归模型的预测数据已经计算得到
  #investment_df$linearRegPred<-as.integer(bound[,1])
  investment_df$linearRegPred<-as.integer(predict(ptrainolsRegModel,newdata=investment_df))
  
  
  
  #-------rfRegModel是随机森林得到的回归模型，后面用predict直接调用此模型即可,因数量少，不运行交叉验证
  ptrainrfRegModel<-randomForest(investment~ptrain,data=investment_df,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  investment_df$frRegPred<-as.integer(predict(ptrainrfRegModel,investment_df))    #<-----------随机森林的预测数据已经在这里计算得到
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  ptrainsvmRegModel<-svm(investment~ptrain,data=investment_df,type="eps-regression",cross=dim(investment_df)[1]/2)
  #svm 内含交叉验证，所以不需要再运行交叉验证.eps-regression   huigui
  investment_df$svmRegPred<-as.integer(predict(ptrainsvmRegModel,investment_df))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  investment_len<-length(investment_df$tm)
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
        p<-plotCurve(investment_df,investment_df$tm,investment_df$investment)
      }
      else
      {
        p<-plotCurve(investment_df,investment_df$tm,investment_df$linearRegPred)
      }
    }
    else{
      dfsub<-subset(investment_df,investment_df$tm>=input$investment_year_start) 
      dfsub<-subset(dfsub,dfsub$tm<=input$investment_year_end)
      if (input$investment_stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$investment)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$linearRegPred)
      }
    }
    
    if(input$investment_predict_data){
      
      p<-p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$investment_stat_data) {
      p<-p+geom_point(aes(x=tm,y=investment),color="red",size=3,shape=21)
    }
    p+ylab("固定资产投资额")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------------------------------
  
  #----------------------------------------------------   
  #多元回归预测计算
  output$investment_output<-renderText({
    ptrain_x1<-as.numeric(input$ptrain_input)
    ptrain<-c(ptrain_x1)
    tm<-c(2016)
    investment<-c(0)
    inputdata<-data.frame(tm,investment,ptrain)#  其中的数不能省略
    pred<-as.integer(predict(ptrainolsRegModel,inputdata))
    paste("多元回归预测：",pred ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$investment_FRR<-renderText({
    ptrain_x1<-as.numeric(input$ptrain_input)
    ptrain<-c(ptrain_x1)
    tm<-c(2016)
    investment<-c(0)
    inputdata<-data.frame(tm,investment,ptrain)
    railinvestment<-predict(ptrainrfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railinvestment[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$investment_zhi<-renderText({
    ptrain_x1<-as.numeric(input$ptrain_input)
    ptrain<-c(ptrain_x1)
    tm<-c(2016)
    investment<-c(0)
    inputdata<-data.frame(tm,investment,ptrain)
    pred<-as.integer(predict(ptrainsvmRegModel,inputdata))
    
    paste("支持向量机预测：",pred)
    
  }
  )
  #-------------------------------------
  
  
  #-----------随机森林Tabset画线  
  output$investmentrfplot <- renderPlot( {
    
    if(input$investment_year_start> input$investment_year_end)  {
      
      if (input$investment_stat_data) {
        p<-plotCurve(investment_df,investment_df$tm,investment_df$investment)
      }
      else
      {
        p<-plotCurve(investment_df,investment_df$tm,investment_df$frRegPred)
      }
    }
    else{
      dfsub<-subset(investment_df,investment_df$tm>=input$investment_year_start) 
      dfsub<-subset(dfsub,dfsub$tm<=input$investment_year_end)
      if (input$investment_stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$investment)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$frRegPred)
      }
    }
    
    if(input$investment_predict_data){
      p<-p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)#+stat_smooth(method=rfRegModel,color='black',level=0.95)
    }
    
    if (input$investment_stat_data) {
      p<-p+geom_point(aes(x=tm,y=investment),color="red",size=3,shape=21)
    }
    p+ylab("固定资产投资额")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------支持向量机Tabset画线
  
  output$investmentsvmplot <- renderPlot( {
    
    if(input$investment_year_start> input$investment_year_end)  {
      
      if (input$investment_stat_data) {
        p<-plotCurve(investment_df,investment_df$tm,investment_df$investment)
      }
      else
      {
        p<-plotCurve(investment_df,investment_df$tm,investment_df$svmRegPred)
      }
    }
    else{
      dfsub<-subset(investment_df,investment_df$tm>=input$investment_year_start) 
      dfsub<-subset(dfsub,dfsub$tm<=input$investment_year_end)
      if (input$investment_stat_data) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$investment)
      }
      else
      {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$svmRegPred)
      }
    }
    
    if(input$investment_predict_data){
      p<-p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
    }
    
    if (input$investment_stat_data) {
      p<-p+geom_point(aes(x=tm,y=investment),color="red",size=3,shape=21)
    }
    p+ylab("固定资产额")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  output$investmenttable<-DT::renderDataTable(
    DT::datatable(
      data<-investment_df, 
      colnames = c('序号', '年','固定资产投资额（万元）','客车车辆数（辆）','多元回归预测（万元）','随机森林回归预测（万元）','支持向量机回归预测（万元）'),
      rownames = TRUE)
  )
  #---------------固定资产投资-货车车辆---------------------------------
  #cw_truck-----货车车辆
  cw_truck_df<-read.csv("truck-asset.csv",head=T)
  cw_truck_olsRegModel<-lm(asset~cw_truck,data=cw_truck_df)
  cw_truck_df$linearRegPred<-as.integer(predict(cw_truck_olsRegModel,newdata=cw_truck_df))
  cw_truck_rfRegModel<-randomForest(asset~cw_truck,data=cw_truck_df,importance=T, ntree=100,type="regression")
  cw_truck_df$frRegPred<-as.integer(predict(cw_truck_rfRegModel,cw_truck_df))
  cw_truck_svmRegModel<-svm(asset~cw_truck,data=cw_truck_df,type="eps-regression",cross=dim(cw_truck_df)[1]/2)
  cw_truck_df$svmRegPred<-as.integer(predict(cw_truck_svmRegModel,cw_truck_df))
  cw_truck_len<-length(cw_truck_df$tm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    cw_truck_len=dim(xdata)[1]
    cw_truck_plt<-ggplot(db,x=c(xdata[1],xdata[cw_truck_len]),aes(x=xdata,y=ydata),color="red")
    return(cw_truck_plt)
  }
  output$cw_truck_linearplot <- renderPlot( {
    
    if(input$cw_truck_year_start> input$cw_truck_year_end)  {
      
      if (input$cw_truck_stat_data) {
        cw_truck_p<-plotCurve(cw_truck_df,cw_truck_df$tm,cw_truck_df$asset)
      }
      else
      {
        cw_truck_p<-plotCurve(cw_truck_df,cw_truck_df$tm,cw_truck_df$linearRegPred)
      }
    }
    else{
      cw_truck_dfsub<-subset(cw_truck_df,substr(cw_truck_df$tm,1,4)>=input$cw_truck_year_start) 
      cw_truck_dfsub<-subset(cw_truck_dfsub,substr(cw_truck_dfsub$tm,1,4)<=input$cw_truck_year_end)
      if (input$cw_truck_stat_data) {
        cw_truck_p<-plotCurve(cw_truck_dfsub,cw_truck_dfsub$tm,cw_truck_dfsub$asset)
      }
      else
      {
        cw_truck_p<-plotCurve(cw_truck_dfsub,cw_truck_dfsub$tm,cw_truck_dfsub$linearRegPred)
      }
    }
    if(input$cw_truck_predict_data){
      
      cw_truck_p<-cw_truck_p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$cw_truck_stat_data) {
      cw_truck_p<-cw_truck_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    cw_truck_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  output$cw_truck_asset_output<-renderText({
    cw_truck_x<-as.numeric(input$cw_truck_input)
    cw_truck<-c(cw_truck_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,cw_truck)
    cw_truck_pred<-as.integer(predict(cw_truck_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",cw_truck_pred[1],"预测区间95%：(",cw_truck_pred[2],",",cw_truck_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$cw_truck_asset_FRR<-renderText({
    cw_truck_x<-as.numeric(input$cw_truck_input)
    cw_truck<-c(cw_truck_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,cw_truck)
    truckasset<-predict(cw_truck_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(truckasset[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$cw_truck_asset_zhi<-renderText({
    cw_truck_x<-as.numeric(input$cw_truck_input)
    cw_truck<-c(cw_truck_x)
    tm<-c(2016)
    asset<-c(0)
    inputdata<-data.frame(tm,asset,cw_truck)
    cw_truck_pred<-as.integer(predict(cw_truck_svmRegModel,inputdata))
    
    paste("支持向量机预测：",cw_truck_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$cw_truck_rfplot <- renderPlot( {
    
    if(input$cw_truck_year_start> input$cw_truck_year_end)  {
      
      if (input$cw_truck_stat_data) {
        cw_truck_p<-plotCurve(cw_truck_df,cw_truck_df$tm,cw_truck_df$asset)
      }
      else
      {
        cw_truck_p<-plotCurve(cw_truck_df,cw_truck_df$tm,cw_truck_df$frRegPred)
      }
    }
    else{
      cw_truck_dfsub<-subset(cw_truck_df,substr(cw_truck_df$tm,1,4)>=input$cw_truck_year_start) 
      cw_truck_dfsub<-subset(cw_truck_dfsub,substr(cw_truck_df$tm,1,4)<=input$cw_truck_year_end)
      if (input$cw_truck_stat_data) {
        cw_truck_p<-plotCurve(cw_truck_dfsub,cw_truck_dfsub$tm,cw_truck_dfsub$asset)
      }
      else
      {
        cw_truck_p<-plotCurve(cw_truck_dfsub,cw_truck_dfsub$tm,cw_truck_dfsub$frRegPred)
      }
    }
    
    if(input$cw_truck_predict_data){
      cw_truck_p<-cw_truck_p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$cw_truck_stat_data) {
      cw_truck_p<-cw_truck_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    cw_truck_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$cw_truck_svmplot <- renderPlot( {
    
    if(input$cw_truck_year_start> input$cw_truck_year_end)  {
      
      if (input$cw_truck_stat_data) {
        cw_truck_p<-plotCurve(cw_truck_df,cw_truck_df$tm,cw_truck_df$asset)
      }
      else
      {
        cw_truck_p<-plotCurve(cw_truck_df,cw_truck_df$tm,cw_truck_df$svmRegPred)
      }
    }
    else{
      cw_truck_dfsub<-subset(cw_truck_df,substr(cw_truck_df$tm,1,4)>=input$cw_truck_year_start) 
      cw_truck_dfsub<-subset(cw_truck_dfsub,substr(cw_truck_dfsub$tm,1,4)<=input$cw_truck_year_end)
      if (input$cw_truck_stat_data) {
        cw_truck_p<-plotCurve(cw_truck_dfsub,cw_truck_dfsub$tm,cw_truck_dfsub$asset)
      }
      else
      {
        cw_truck_p<-plotCurve(cw_truck_dfsub,cw_truck_dfsub$tm,cw_truck_dfsub$svmRegPred)
      }
    }
    if(input$cw_truck_predict_data){
      cw_truck_p<-cw_truck_p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$cw_truck_stat_data) {
      cw_truck_p<-cw_truck_p+geom_point(aes(x=tm,y=asset),color="red",size=3,shape=21)
    }
    cw_truck_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  
  output$cw_truck_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        cw_truck_data<-cw_truck_df
      } , 
      colnames = c('序号', '时间', '固定资产投资（亿元）', '货车车辆','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
      rownames = TRUE)
  )
  
  
  #---------------固定资产投资-机车台数---------------------------------
  #JCNum-----机车台数
  #GDMoney-------固定资产投资
  #tm----------时间
  JCNum_df<-read.csv("固定资产-机车台数.csv",head=T)
  JCNum_olsRegModel<-lm(GDMoney~JCNum,data=JCNum_df)
  JCNum_df$linearRegPred<-as.integer(predict(JCNum_olsRegModel,newdata=JCNum_df))
  JCNum_rfRegModel<-randomForest(GDMoney~JCNum,data=JCNum_df,importance=T, ntree=100,type="regression")
  JCNum_df$frRegPred<-as.integer(predict(JCNum_rfRegModel,JCNum_df))
  JCNum_svmRegModel<-svm(GDMoney~JCNum,data=JCNum_df,type="eps-regression",cross=dim(JCNum_df)[1]/2)
  JCNum_df$svmRegPred<-as.integer(predict(JCNum_svmRegModel,JCNum_df))
  JCNum_len<-length(JCNum_df$tm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    JCNum_len=dim(xdata)[1]
    JCNum_plt<-ggplot(db,x=c(xdata[1],xdata[JCNum_len]),aes(x=xdata,y=ydata),color="red")
    return(JCNum_plt)
  }
  output$JCNum_linearplot <- renderPlot( {
    
    if(input$JCNum_year_start> input$JCNum_year_end)  {
      
      if (input$JCNum_stat_data) {
        JCNum_p<-plotCurve(JCNum_df,JCNum_df$tm,JCNum_df$GDMoney)
      }
      else
      {
        JCNum_p<-plotCurve(JCNum_df,JCNum_df$tm,JCNum_df$linearRegPred)
      }
    }
    else{
      JCNum_dfsub<-subset(JCNum_df,substr(JCNum_df$tm,1,4)>=input$JCNum_year_start) 
      JCNum_dfsub<-subset(JCNum_dfsub,substr(JCNum_dfsub$tm,1,4)<=input$JCNum_year_end)
      if (input$JCNum_stat_data) {
        JCNum_p<-plotCurve(JCNum_dfsub,JCNum_dfsub$tm,JCNum_dfsub$GDMoney)
      }
      else
      {
        JCNum_p<-plotCurve(JCNum_dfsub,JCNum_dfsub$tm,JCNum_dfsub$linearRegPred)
      }
    }
    if(input$JCNum_predict_data){
      
      JCNum_p<-JCNum_p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$JCNum_stat_data) {
      JCNum_p<-JCNum_p+geom_point(aes(x=tm,y=GDMoney),color="red",size=3,shape=21)
    }
    JCNum_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  output$JCNum_GDMoney_output<-renderText({
    JCNum_x<-as.numeric(input$JCNum_input)
    JCNum<-c(JCNum_x)
    tm<-c(2016)
    GDMoney<-c(0)
    inputdata<-data.frame(tm,GDMoney,JCNum)
    JCNum_pred<-as.integer(predict(JCNum_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",JCNum_pred[1],"预测区间95%：(",JCNum_pred[2],",",JCNum_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$JCNum_GDMoney_FRR<-renderText({
    JCNum_x<-as.numeric(input$JCNum_input)
    JCNum<-c(JCNum_x)
    tm<-c(2016)
    GDMoney<-c(0)
    inputdata<-data.frame(tm,GDMoney,JCNum)
    GDMoney<-predict(JCNum_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(GDMoney[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$JCNum_GDMoney_zhi<-renderText({
    JCNum_x<-as.numeric(input$JCNum_input)
    JCNum<-c(JCNum_x)
    tm<-c(2016)
    GDMoney<-c(0)
    inputdata<-data.frame(tm,GDMoney,JCNum)
    JCNum_pred<-as.integer(predict(JCNum_svmRegModel,inputdata))
    
    paste("支持向量机预测：",JCNum_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$JCNum_rfplot <- renderPlot( {
    
    if(input$JCNum_year_start> input$JCNum_year_end)  {
      
      if (input$JCNum_stat_data) {
        JCNum_p<-plotCurve(JCNum_df,JCNum_df$tm,JCNum_df$GDMoney)
      }
      else
      {
        JCNum_p<-plotCurve(JCNum_df,JCNum_df$tm,JCNum_df$frRegPred)
      }
    }
    else{
      JCNum_dfsub<-subset(JCNum_df,substr(JCNum_df$tm,1,4)>=input$JCNum_year_start) 
      JCNum_dfsub<-subset(JCNum_dfsub,substr(JCNum_df$tm,1,4)<=input$JCNum_year_end)
      if (input$JCNum_stat_data) {
        JCNum_p<-plotCurve(JCNum_dfsub,JCNum_dfsub$tm,JCNum_dfsub$GDMoney)
      }
      else
      {
        JCNum_p<-plotCurve(JCNum_dfsub,JCNum_dfsub$tm,JCNum_dfsub$frRegPred)
      }
    }
    
    if(input$JCNum_predict_data){
      JCNum_p<-JCNum_p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$JCNum_stat_data) {
      JCNum_p<-JCNum_p+geom_point(aes(x=tm,y=GDMoney),color="red",size=3,shape=21)
    }
    JCNum_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$JCNum_svmplot <- renderPlot( {
    
    if(input$JCNum_year_start> input$JCNum_year_end)  {
      
      if (input$JCNum_stat_data) {
        JCNum_p<-plotCurve(JCNum_df,JCNum_df$tm,JCNum_df$GDMoney)
      }
      else
      {
        JCNum_p<-plotCurve(JCNum_df,JCNum_df$tm,JCNum_df$svmRegPred)
      }
    }
    else{
      JCNum_dfsub<-subset(JCNum_df,substr(JCNum_df$tm,1,4)>=input$JCNum_year_start) 
      JCNum_dfsub<-subset(JCNum_dfsub,substr(JCNum_dfsub$tm,1,4)<=input$JCNum_year_end)
      if (input$JCNum_stat_data) {
        JCNum_p<-plotCurve(JCNum_dfsub,JCNum_dfsub$tm,JCNum_dfsub$GDMoney)
      }
      else
      {
        JCNum_p<-plotCurve(JCNum_dfsub,JCNum_dfsub$tm,JCNum_dfsub$svmRegPred)
      }
    }
    if(input$JCNum_predict_data){
      JCNum_p<-JCNum_p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$JCNum_stat_data) {
      JCNum_p<-JCNum_p+geom_point(aes(x=tm,y=GDMoney),color="red",size=3,shape=21)
    }
    JCNum_p+ylab("固定资产值")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  
  output$JCNum_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        JCNum_data<-JCNum_df
      } , 
      colnames = c('序号', '时间', '固定资产投资（亿元）', '货车车辆','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
      rownames = TRUE)
  )
  
  #---------------------------------------------------------------------
  #------------------客运量-客车车辆数适配性研究
  #PV-------客运量（PassengeVolume）简写
  #PassengeVolume-------客运量
  #CarriageNum-------客车数量
  #CarKm-------客车机车日行公里数
  PVdf<-read.csv("客运量.csv",head=T)
  PVolsRegModel<-lm(PassengeVolume~CarriageNum+CarKm,data=PVdf)
  PVdf$linearRegPred<-as.integer(predict(PVolsRegModel,newdata=PVdf))
  PVrfRegModel<-randomForest(PassengeVolume~CarriageNum+CarKm,data=PVdf,importance=T, ntree=100,type="regression")
  PVdf$frRegPred<-as.integer(predict(PVrfRegModel,PVdf))
  PVsvmRegModel<-svm(PassengeVolume~CarriageNum+CarKm,data=PVdf,type="eps-regression",cross=dim(PVdf)[1]/2)
  PVdf$svmRegPred<-as.integer(predict(PVsvmRegModel,PVdf))
  PVlen<-length(PVdf$PVtm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    PVlen=dim(xdata)[1]
    PVplt<-ggplot(db,x=c(xdata[1],xdata[PVlen]),aes(x=xdata,y=ydata),color="red")
    return(PVplt)
  }
  output$car_passenger_linearplot <- renderPlot( {
    
    if(input$mileage_year_start> input$mileage_year_end)  {
      
      if (input$mileage_stat_data) {
        PVp<-plotCurve(PVdf,PVdf$PVtm,PVdf$PassengeVolume)
      }
      else
      {
        PVp<-plotCurve(PVdf,PVdf$PVtm,PVdf$linearRegPred)
      }
    }
    else{
      PVdfsub<-subset(PVdf,PVdf$PVtm>=input$mileage_year_start) 
      PVdfsub<-subset(PVdfsub,PVdfsub$PVtm<=input$mileage_year_end)
      if (input$mileage_stat_data) {
        PVp<-plotCurve(PVdfsub,PVdfsub$PVtm,PVdfsub$PassengeVolume)
      }
      else
      {
        PVp<-plotCurve(PVdfsub,PVdfsub$PVtm,PVdfsub$linearRegPred)
      }
    }
    if(input$mileage_predict_data){
      
      PVp<-PVp+geom_line(aes(x=PVtm,y=linearRegPred),color="blue",size=1)+geom_point(aes(x=PVtm,y=linearRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      
    }
    
    if (input$mileage_stat_data) {
      PVp<-PVp+geom_point(aes(x=PVtm,y=PassengeVolume),color="red",size=3,shape=21)
    }
    PVp+ylab("客运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  output$PassengeVolume_output<-renderText({
    PVx1<-as.numeric(input$CarriageNum_input)
    PVx2<-as.numeric(input$CarKm_input)
    CarriageNum<-c(PVx1)
    CarKm<-c(PVx2)
    PVtm<-c(2016)
    PassengeVolume<-c(0)
    inputdata<-data.frame(PVtm,PassengeVolume,CarriageNum,CarKm)
    PVpred<-as.integer(predict(PVolsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",PVpred[1],"预测区间95%：(",PVpred[2],",",PVpred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$PassengeVolume_FRR<-renderText({
    PVx1<-as.numeric(input$CarriageNum_input)
    PVx2<-as.numeric(input$CarKm_input)
    CarriageNum<-c(PVx1)
    CarKm<-c(PVx2)
    PVtm<-c(2016)
    PassengeVolume<-c(0)
    inputdata<-data.frame(PVtm,PassengeVolume,CarriageNum,CarKm)
    railPassengeVolume<-predict(PVrfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railPassengeVolume[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$PassengeVolume_zhi<-renderText({
    PVx1<-as.numeric(input$CarriageNum_input)
    PVx2<-as.numeric(input$CarKm_input)
    CarriageNum<-c(PVx1)
    CarKm<-c(PVx2)
    PVtm<-c(2016)
    PassengeVolume<-c(0)
    inputdata<-data.frame(PVtm,PassengeVolume,CarriageNum,CarKm)
    PVpred<-as.integer(predict(PVsvmRegModel,inputdata))
    
    paste("支持向量机预测：",PVpred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$car_passenger_rfplot <- renderPlot( {
    
    if(input$mileage_year_start> input$mileage_year_end)  {
      
      if (input$mileage_stat_data) {
        PVp<-plotCurve(PVdf,PVdf$PVtm,PVdf$PassengeVolume)
      }
      else
      {
        PVp<-plotCurve(PVdf,PVdf$PVtm,PVdf$frRegPred)
      }
    }
    else{
      PVdfsub<-subset(PVdf,PVdf$PVtm>=input$mileage_year_start) 
      PVdfsub<-subset(PVdfsub,PVdfsub$PVtm<=input$mileage_year_end)
      if (input$mileage_stat_data) {
        PVp<-plotCurve(PVdfsub,PVdfsub$PVtm,PVdfsub$PassengeVolume)
      }
      else
      {
        PVp<-plotCurve(PVdfsub,PVdfsub$PVtm,PVdfsub$frRegPred)
      }
    }
    
    if(input$mileage_predict_data){
      PVp<-PVp+geom_line(aes(x=PVtm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=PVtm,y=frRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+stat_smooth(method=rfRegModel,color='black',level=0.95)
    }
    
    if (input$mileage_stat_data) {
      PVp<-PVp+geom_point(aes(x=PVtm,y=PassengeVolume),color="red",size=3,shape=21)
    }
    PVp+ylab("客运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$car_passenger_svmplot <- renderPlot( {
    
    if(input$mileage_year_start> input$mileage_year_end)  {
      
      if (input$mileage_stat_data) {
        PVp<-plotCurve(PVdf,PVdf$PVtm,PVdf$PassengeVolume)
      }
      else
      {
        PVp<-plotCurve(PVdf,PVdf$PVtm,PVdf$svmRegPred)
      }
    }
    else{
      PVdfsub<-subset(PVdf,PVdf$PVtm>=input$mileage_year_start) 
      PVdfsub<-subset(PVdfsub,PVdfsub$PVtm<=input$mileage_year_end)
      if (input$mileage_stat_data) {
        PVp<-plotCurve(PVdfsub,PVdfsub$PVtm,PVdfsub$PassengeVolume)
      }
      else
      {
        PVp<-plotCurve(PVdfsub,PVdfsub$PVtm,PVdfsub$svmRegPred)
      }
    }
    if(input$mileage_predict_data){
      PVp<-PVp+geom_line(aes(x=PVtm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=PVtm,y=svmRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
    }
    
    if (input$mileage_stat_data) {
      PVp<-PVp+geom_point(aes(x=PVtm,y=PassengeVolume),color="red",size=3,shape=21)
    }
    PVp+ylab("客运量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  
  output$car_passenger_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        PVdata<-PVdf
      } , 
      colnames = c('序号', '时间', '客运量（万人）','客车车辆数（辆）','客车机车日行公里（公里）','多元回归预测（亿元）','随机森林回归预测（亿元）','支持向量机回归预测（亿元）'),
      rownames = TRUE)
  )
  
  #-----------------------适配性研究---------------------------------
  #----------------------机车-营业里程---------------------------
  
  df_1<-read.csv("Locomotive-dis.csv",head=T)#读取机车与营业里程原始表
  olsRegModel_1<-lm(locomotive~distance,data=df_1)# 多元回归计算
  
  df_1$linearRegPred<-as.integer(predict(olsRegModel_1,newdata=df_1))# 多元回归计算并写入表
  
  
  rfRegModel_1<-randomForest(locomotive~distance,data=df_1,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  df_1$frRegPred<-as.integer(predict(rfRegModel_1,df_1))    #<-----------随机森林的预测数据已经在这里计算得到
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  svmRegModel_1<-svm(locomotive~distance,data=df_1,type="eps-regression",cross=dim(df_1)[1]/2)
  #svm 内含交叉验证，所以不需要再运行交叉验证.eps-regression   huigui
  df_1$svmRegPred<-as.integer(predict(svmRegModel_1,df_1))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  len<-length(df_1$tm)
  #plotCurve是画曲线的通过用函数，为了减少后面的代码量  
  plotCurve<-function(db,xdata,ydata)
  {
    len=dim(xdata)[1]
    plt<-ggplot(db,x=c(xdata[1],xdata[len]),aes(x=xdata,y=ydata),color="red")
    return(plt)
  }
  #---------------------------多元回归画线
  output$linearplot_1 <- renderPlot( {
    
    if(input$year_start_1> input$year_end_1)  {
      
      if (input$stat_data_1) {
        p<-plotCurve(df_1,df_1$tm,df_1$locomotive)
      }
      else
      {
        p<-plotCurve(df_1,df_1$tm,df_1$linearRegPred)
      }
    }
    else{
      dfsub_1<-subset(df_1,df_1$tm>=input$year_start_1) 
      dfsub_1<-subset(dfsub_1,dfsub_1$tm<=input$year_end_1)
      if (input$stat_data_1) {
        p<-plotCurve(dfsub_1,dfsub_1$tm,dfsub_1$locomotive)
      }
      else
      {
        p<-plotCurve(dfsub_1,dfsub_1$tm,dfsub_1$linearRegPred)
      }
    }
    
    if(input$predict_data_1){
      
      p<-p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$stat_data_1) {
      p<-p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    p+ylab("机车台数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #多元回归预测计算
  output$locomotive_output_1<-renderText({
    x1<-as.numeric(input$km_input_1)
    distance<-c(x1)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,distance)#  其中的数不能省略
    pred<-as.integer(predict(olsRegModel_1,inputdata))
    paste("多元回归预测：",round(pred,0) ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$locomotive_FRR_1<-renderText({
    x1<-as.numeric(input$km_input_1)
    distance<-c(x1)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,distance)
    raillocomotive<-predict(rfRegModel_1,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(raillocomotive[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$locomotive_zhi_1<-renderText({
    x1<-as.numeric(input$km_input_1)
    distance<-c(x1)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,distance)
    pred<-as.integer(predict(svmRegModel_1,inputdata))
    
    paste("支持向量机预测：",pred)
    
  }
  )
  #-------------------------------------
  
  #-----------随机森林Tabset画线  
  output$rfplot_1<- renderPlot( {
    
    if(input$year_start_1> input$year_end_1)  {
      
      if (input$stat_data_1) {
        p<-plotCurve(df_1,df_1$tm,df_1$locomotive)
      }
      else
      {
        p<-plotCurve(df_1,df$tm,df_1$frRegPred)
      }
    }
    else{
      dfsub_1<-subset(df_1,df_1$tm>=input$year_start_1) 
      dfsub_1<-subset(dfsub_1,dfsub_1$tm<=input$year_end_1)
      if (input$stat_data_1) {
        p<-plotCurve(dfsub_1,dfsub_1$tm,dfsub_1$locomotive)
      }
      else
      {
        p<-plotCurve(dfsub_1,dfsub_1$tm,dfsub_1$frRegPred)
      }
    }
    
    if(input$predict_data_1){
      p<-p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)#+stat_smooth(method=rfRegModel,color='black',level=0.95)
    }
    
    if (input$stat_data_1) {
      p<-p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    p+ylab("机车台数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------支持向量机Tabset画线
  
  output$svmplot_1 <- renderPlot( {
    
    if(input$year_start_1> input$year_end_1)  {
      
      if (input$stat_data_1) {
        p<-plotCurve(df_1,df_1$tm,df_1$locomotive)
      }
      else
      {
        p<-plotCurve(df_1,df_1$tm,df_1$svmRegPred)
      }
    }
    else{
      dfsub_1<-subset(df_1,df_1$tm>=input$year_start_1) 
      dfsub_1<-subset(dfsub_1,dfsub_1$tm<=input$year_end_1)
      if (input$stat_data_1) {
        p<-plotCurve(dfsub_1,dfsub_1$tm,dfsub_1$locomotive)
      }
      else
      {
        p<-plotCurve(dfsub_1,dfsub_1$tm,dfsub_1$svmRegPred)
      }
    }
    
    if(input$predict_data_1){
      p<-p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
    }
    
    if (input$stat_data_1) {
      p<-p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    p+ylab("机车台数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  output$table_1<-DT::renderDataTable(
    DT::datatable(
      data<-df_1, 
      colnames = c('序号', '年','机车辆数（辆）','营业里程（公里）','多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
      rownames = TRUE)
  )
  
  #-----------------------------------------------------------
  #--------------机车车辆-货运量适配性研究------------------  
  Locomotive_fre<-read.csv("Locomotive-freight.csv",head=T)
  #-------------olsRegModel为多元回归模型
  freightolsRegModel<-lm(locomotive1~freight,data=Locomotive_fre)
  #bound<-(predict(olsRegModel,newdata=Locomotive_fre,interval = "prediction"))  #<-----------回归模型的预测数据已经计算得到
  #Locomotive_fre$linearRegPred<-as.integer(bound[,1])
  Locomotive_fre$linearRegPred<-as.integer(predict(freightolsRegModel,newdata=Locomotive_fre))
  
  
  
  #-------rfRegModel是随机森林得到的回归模型，后面用predict直接调用此模型即可,因数量少，不运行交叉验证
  freightrfRegModel<-randomForest(locomotive1~freight,data=Locomotive_fre,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  Locomotive_fre$frRegPred<-as.integer(predict(freightrfRegModel,Locomotive_fre))    #<-----------随机森林的预测数据已经在这里计算得到
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  freightsvmRegModel<-svm(locomotive1~freight,data=Locomotive_fre,type="eps-regression",cross=dim(Locomotive_fre)[1]/2)
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
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$locomotive1)
      }
      else
      {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$linearRegPred)
      }
    }
    else{
      dfsub<-subset(Locomotive_fre,Locomotive_fre$tm>=input$Locomotive_year_start1) 
      dfsub<-subset(dfsub,dfsub$tm<=input$Locomotive_year_end1)
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$locomotive1)
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
      p<-p+geom_point(aes(x=tm,y=locomotive1),color="red",size=3,shape=21)
    }
    p+ylab("机车数量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------------------------------
  
  #----------------------------------------------------   
  #多元回归预测计算
  output$locomotive_output1<-renderText({
    Locomotive_x2<-as.numeric(input$ton_input)
    freight<-c(Locomotive_x2)
    tm<-c(2016)
    locomotive1<-c(0)
    inputdata<-data.frame(tm,locomotive1,freight)#  其中的数不能省略
    freightpred<-as.integer(predict(freightolsRegModel,inputdata))
    paste("多元回归预测：",freightpred ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$locomotive_FRR1<-renderText({
    Locomotive_x2<-as.numeric(input$ton_input)
    freight<-c(Locomotive_x2)
    tm<-c(2016)
    locomotive1<-c(0)
    inputdata<-data.frame(tm,locomotive1,freight)
    raillocomotive<-predict(freightrfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(raillocomotive[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$locomotive_zhi1<-renderText({
    Locomotive_x2<-as.numeric(input$ton_input)
    freight<-c(Locomotive_x2)
    tm<-c(2016)
    locomotive1<-c(0)
    inputdata<-data.frame(tm,locomotive1,freight)
    freightpred<-as.integer(predict(freightsvmRegModel,inputdata))
    
    paste("支持向量机预测：",freightpred)
    
  }
  )
  #-------------------------------------
  
  
  #-----------随机森林Tabset画线  
  output$freightrfplot<- renderPlot( {
    
    if(input$Locomotive_year_start1> input$Locomotive_year_end1)  {
      
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$locomotive1)
      }
      else
      {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$frRegPred)
      }
    }
    else{
      dfsub<-subset(Locomotive_fre,Locomotive_fre$tm>=input$Locomotive_year_start1) 
      dfsub<-subset(dfsub,dfsub$tm<=input$Locomotive_year_end1)
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$locomotive1)
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
      p<-p+geom_point(aes(x=tm,y=locomotive1),color="red",size=3,shape=21)
    }
    p+ylab("机车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------支持向量机Tabset画线
  
  output$freightsvmplot<- renderPlot( {
    
    if(input$Locomotive_year_start1> input$Locomotive_year_end1)  {
      
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$locomotive1)
      }
      else
      {
        p<-plotCurve(Locomotive_fre,Locomotive_fre$tm,Locomotive_fre$svmRegPred)
      }
    }
    else{
      dfsub<-subset(Locomotive_fre,Locomotive_fre$tm>=input$Locomotive_year_start1) 
      dfsub<-subset(dfsub,dfsub$tm<=input$Locomotive_year_end1)
      if (input$Locomotive_stat_data1) {
        p<-plotCurve(dfsub,dfsub$tm,dfsub$locomotive1)
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
      p<-p+geom_point(aes(x=tm,y=locomotive1),color="red",size=3,shape=21)
    }
    p+ylab("机车数量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  output$freighttable<-DT::renderDataTable(
    DT::datatable(
      data<-Locomotive_fre, 
      colnames = c('序号', '年','机车数量（辆）','货运量（万吨）','多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
      rownames = TRUE)
  )
  
  
  #-----------------------------------------------------------
  #--------------货车车辆-营业里程适配性研究------------------
  #truck----------------------货车辆数
  #distance-------------------营业里程
  #21----------------------——与前面程序的变量做区别
  #tm----------------------年份
  df_21<-read.csv("货车车辆预测.csv",head=T)
  #-------------olsRegModel为多元回归模型
  olsRegModel_21<-lm(truck~distance,data=df_21)
  
  df_21$linearRegPred<-as.integer(predict(olsRegModel_21,newdata=df_21))
  
  
  
  #-------rfRegModel是随机森林得到的回归模型，后面用predict直接调用此模型即可,因数量少，不运行交叉验证
  rfRegModel_21<-randomForest(truck~distance,data=df_21,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  df_21$frRegPred<-as.integer(predict(rfRegModel_21,df_21))    #<-----------随机森林的预测数据已经在这里计算得到
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  svmRegModel_21<-svm(truck~distance,data=df_21,type="eps-regression",cross=dim(df_21)[1]/2)
  #svm 内含交叉验证，所以不需要再运行交叉验证
  df_21$svmRegPred<-as.integer(predict(svmRegModel_21,df_21))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  len<-length(df_21$tm)
  #plotCurve是画曲线的通过用函数，为了减少后面的代码量  
  plotCurve<-function(db,xdata,ydata)
  {
    len=dim(xdata)[1]
    plt<-ggplot(db,x=c(xdata[1],xdata[len]),aes(x=xdata,y=ydata),color="red")
    return(plt)
  }
  #---------------------------多元回归画线
  output$linearplot_21 <- renderPlot( {
    
    if(input$year_start_21> input$year_end_21)  {
      
      if (input$stat_data_21) {
        p<-plotCurve(df_21,df_21$tm,df_21$truck)
      }
      else
      {
        p<-plotCurve(df_21,df_21$tm,df_21$linearRegPred)
      }
    }
    else{
      dfsub_21<-subset(df_21,df_21$tm>=input$year_start_21) 
      dfsub_21<-subset(dfsub_21,dfsub_21$tm<=input$year_end_21)
      if (input$stat_data_21) {
        p<-plotCurve(dfsub_21,dfsub_21$tm,dfsub_21$truck)
      }
      else
      {
        p<-plotCurve(dfsub_21,dfsub_21$tm,dfsub_21$linearRegPred)
      }
    }
    
    if(input$predict_data_21){
      
      p<-p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=1)+geom_point(aes(x=tm,y=linearRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$stat_data_21) {
      p<-p+geom_point(aes(x=tm,y=truck),color="red",size=3,shape=21)
    }
    p+ylab("货车车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------------------------------
  
  #----------------------------------------------------   
  #多元回归预测计算
  output$truck_output_21<-renderText({
    x1<-as.numeric(input$km_input_21)
    distance<-c(x1)
    tm<-c(2016)
    truck<-c(0)
    inputdata<-data.frame(tm,truck,distance)
    pred<-as.integer(predict(olsRegModel_21,inputdata))
    paste("多元回归预测：",round(pred,0) ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$truck_FRR_21<-renderText({
    x1<-as.numeric(input$km_input_21)
    distance<-c(x1)
    tm<-c(2016)
    truck<-c(0)
    inputdata<-data.frame(tm,truck,distance)
    railTruck<-predict(rfRegModel_21,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railTruck[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$truck_zhi_21<-renderText({
    x1<-as.numeric(input$km_input_21)
    distance<-c(x1)
    tm<-c(2016)
    truck<-c(0)
    inputdata<-data.frame(tm,truck,distance)
    pred<-as.integer(predict(svmRegModel_21,inputdata))
    
    paste("支持向量机预测：",pred)
    
  }
  )
  #-------------------------------------
  
  
  #-----------随机森林Tabset画线  
  output$rfplot_21 <- renderPlot( {
    
    if(input$year_start_21> input$year_end_21)  {
      
      if (input$stat_data_21) {
        p<-plotCurve(df_21,df_21$tm,df_21$truck)
      }
      else
      {
        p<-plotCurve(df_21,df_21$tm,df_21$frRegPred)
      }
    }
    else{
      dfsub_21<-subset(df_21,df_21$tm>=input$year_start_21) 
      dfsub_21<-subset(dfsub_21,dfsub_21$tm<=input$year_end_21)
      if (input$stat_data_21) {
        p<-plotCurve(dfsub_21,dfsub_21$tm,dfsub_21$truck)
      }
      else
      {
        p<-plotCurve(dfsub_21,dfsub_21$tm,dfsub_21$frRegPred)
      }
    }
    
    if(input$predict_data_21){
      
      p<-p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=frRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$stat_data_21) {
      p<-p+geom_point(aes(x=tm,y=truck),color="red",size=3,shape=21)
    }
    p+ylab("货车车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------支持向量机Tabset画线
  
  output$svmplot_21 <- renderPlot( {
    
    if(input$year_start_21> input$year_end_21)  {
      
      if (input$stat_data_21) {
        p<-plotCurve(df_21,df_21$tm,df_21$truck)
      }
      else
      {
        p<-plotCurve(df_21,df_21$tm,df_21$svmRegPred)
      }
    }
    else{
      dfsub_21<-subset(df_21,df_21$tm>=input$year_start_21) 
      dfsub_21<-subset(dfsub_21,dfsub_21$tm<=input$year_end_21)
      if (input$stat_data_21) {
        p<-plotCurve(dfsub_21,dfsub_21$tm,dfsub_21$truck)
      }
      else
      {
        p<-plotCurve(dfsub_21,dfsub_21$tm,dfsub_21$svmRegPred)
      }
    }
    
    if(input$predict_data_21){
      
      p<-p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$stat_data_21) {
      p<-p+geom_point(aes(x=tm,y=truck),color="red",size=3,shape=21)
    }
    p+ylab("货车车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  output$table_21<-DT::renderDataTable(
    DT::datatable(
      data<-df_21, 
      colnames = c('序号', '年','机车辆数（辆）','营业里程（公里）','多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
      #<<<<<<< HEAD
      #=======
      rownames = TRUE)
  )
  
  #---------------------------------------------------------------------
  #---------------机车数量-客运量---文静添加------------------------------
  #Locomotive_PV-----机车数量-客运量passenger vollum
  locomotivePV_df<-read.csv("locomotive-PV.csv",head=T)
  
  locomotivePV_olsRegModel<-lm(locomotive~PV,data=locomotivePV_df)
  
  locomotivePV_df$linearRegPred<-as.integer(predict(locomotivePV_olsRegModel,newdata=locomotivePV_df))
  locomotivePV_rfRegModel<-randomForest(locomotive~PV,data=locomotivePV_df,importance=T,ntree=100,type="regression")
  locomotivePV_df$frRegPred<-as.integer(predict(locomotivePV_rfRegModel,locomotivePV_df))
  locomotivePV_svmRegModel<-svm(locomotive~PV,data=locomotivePV_df,type="eps-regression",cross=dim(locomotivePV_df)[1]/2)
  locomotivePV_df$svmRegPred<-as.integer(predict(locomotivePV_svmRegModel,locomotivePV_df))
  locomotivePV_len<-length(locomotivePV_df$tm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    locomotivePV_len=dim(xdata)[1]
    locomotivePV_plt<-ggplot(db,x=c(xdata[1],xdata[locomotivePV_len]),aes(x=xdata,y=ydata),color="red")
    return(locomotivePV_plt)
  }
  
  #---------------------------多元回归画线
  output$locomotivePV_linearplot <- renderPlot( {
    
    if(input$locomotivePV_year_start> input$locomotivePV_year_end)  {
      
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$linearRegPred)
      }
    }
    else{
      locomotivePV_dfsub<-subset(locomotivePV_df,substr(locomotivePV_df$tm,1,4)>=input$locomotivePV_year_start) 
      locomotivePV_dfsub<-subset(locomotivePV_dfsub,substr(locomotivePV_dfsub$tm,1,4)<=input$locomotivePV_year_end)
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$linearRegPred)
      }
    }
    if(input$locomotivePV_predict_data){
      locomotivePV_p<-locomotivePV_p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$locomotivePV_stat_data) {
      locomotivePV_p<-locomotivePV_p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    locomotivePV_p+ylab("机车台数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  output$locomotivePV_locomotive_output<-renderText({
    locomotivePV_x<-as.numeric(input$locomotivePV_PV_input)
    PV<-c(locomotivePV_x)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,PV)
    locomotivePV_pred<-as.integer(predict(locomotivePV_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",locomotivePV_pred[1],"预测区间95%：(",locomotivePV_pred[2],",",locomotivePV_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$locomotivePV_locomotive_FRR<-renderText({
    locomotivePV_x<-as.numeric(input$locomotivePV_PV_input)
    PV<-c(locomotivePV_x)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,PV)
    locomotive<-predict(locomotivePV_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(locomotive[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$locomotivePV_locomotive_zhi<-renderText({
    locomotivePV_x<-as.numeric(input$locomotivePV_PV_input)
    PV<-c(locomotivePV_x)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,PV)
    locomotivePV_pred<-as.integer(predict(locomotivePV_svmRegModel,inputdata))
    
    paste("支持向量机预测：",locomotivePV_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$locomotivePV_rfplot <- renderPlot( {
    
    if(input$locomotivePV_year_start> input$locomotivePV_year_end)  {
      
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$frRegPred)
      }
    }
    else{
      locomotivePV_dfsub<-subset(locomotivePV_df,substr(locomotivePV_df$tm,1,4)>=input$locomotivePV_year_start) 
      locomotivePV_dfsub<-subset(locomotivePV_dfsub,substr(locomotivePV_df$tm,1,4)<=input$locomotivePV_year_end)
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$frRegPred)
      }
    }
    
    if(input$locomotivePV_predict_data){
      locomotivePV_p<-locomotivePV_p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes
                                                                                                                       (x=tm,y=frRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$locomotivePV_stat_data) {
      locomotivePV_p<-locomotivePV_p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    locomotivePV_p+ylab("机车数量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$locomotivePV_svmplot <- renderPlot( {
    
    if(input$locomotivePV_year_start> input$locomotivePV_year_end)  {
      
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$svmRegPred)
      }
    }
    else{
      locomotivePV_dfsub<-subset(locomotivePV_df,substr(locomotivePV_df$tm,1,4)>=input$locomotivePV_year_start) 
      locomotivePV_dfsub<-subset(locomotivePV_dfsub,substr(locomotivePV_df$tm,1,4)<=input$locomotivePV_year_end)
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$svmRegPred)
      }
    }
    if(input$locomotivePV_predict_data){
      locomotivePV_p<-locomotivePV_p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes
                                                                                                        (x=tm,y=svmRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$locomotivePV_stat_data) {
      locomotivePV_p<-locomotivePV_p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    locomotivePV_p+ylab("机车数量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  
  output$locomotivePV_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        locomotivePV_data<-locomotivePV_df
      } , 
      colnames = c('序号', '时间', '机车台数（辆）', '客运量（亿人）','多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
      #>>>>>>> refs/remotes/origin/develop
      rownames = TRUE)
  )
  
  #---------------------------------------------------------------------
  #---------------机车数量-客运量---文静添加------------------------------
  #Locomotive_PV-----机车数量-客运量passenger vollum
  locomotivePV_df<-read.csv("locomotive-PV.csv",head=T)
  
  locomotivePV_olsRegModel<-lm(locomotive~PV,data=locomotivePV_df)
  
  locomotivePV_df$linearRegPred<-as.integer(predict(locomotivePV_olsRegModel,newdata=locomotivePV_df))
  locomotivePV_rfRegModel<-randomForest(locomotive~PV,data=locomotivePV_df,importance=T,ntree=100,type="regression")
  locomotivePV_df$frRegPred<-as.integer(predict(locomotivePV_rfRegModel,locomotivePV_df))
  locomotivePV_svmRegModel<-svm(locomotive~PV,data=locomotivePV_df,type="eps-regression",cross=dim(locomotivePV_df)[1]/2)
  locomotivePV_df$svmRegPred<-as.integer(predict(locomotivePV_svmRegModel,locomotivePV_df))
  locomotivePV_len<-length(locomotivePV_df$tm)
  
  plotCurve<-function(db,xdata,ydata)
  {
    locomotivePV_len=dim(xdata)[1]
    locomotivePV_plt<-ggplot(db,x=c(xdata[1],xdata[locomotivePV_len]),aes(x=xdata,y=ydata),color="red")
    return(locomotivePV_plt)
  }
  
  #---------------------------多元回归画线
  output$locomotivePV_linearplot <- renderPlot( {
    
    if(input$locomotivePV_year_start> input$locomotivePV_year_end)  {
      
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$linearRegPred)
      }
    }
    else{
      locomotivePV_dfsub<-subset(locomotivePV_df,substr(locomotivePV_df$tm,1,4)>=input$locomotivePV_year_start) 
      locomotivePV_dfsub<-subset(locomotivePV_dfsub,substr(locomotivePV_dfsub$tm,1,4)<=input$locomotivePV_year_end)
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$linearRegPred)
      }
    }
    if(input$locomotivePV_predict_data){
      locomotivePV_p<-locomotivePV_p+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=linearRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$locomotivePV_stat_data) {
      locomotivePV_p<-locomotivePV_p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    locomotivePV_p+ylab("机车台数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  output$locomotivePV_locomotive_output<-renderText({
    locomotivePV_x<-as.numeric(input$locomotivePV_PV_input)
    PV<-c(locomotivePV_x)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,PV)
    locomotivePV_pred<-as.integer(predict(locomotivePV_olsRegModel,inputdata,interval="prediction",level=0.95))
    paste("多元回归预测：",locomotivePV_pred[1],"预测区间95%：(",locomotivePV_pred[2],",",locomotivePV_pred[3],")" ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$locomotivePV_locomotive_FRR<-renderText({
    locomotivePV_x<-as.numeric(input$locomotivePV_PV_input)
    PV<-c(locomotivePV_x)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,PV)
    locomotive<-predict(locomotivePV_rfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(locomotive[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$locomotivePV_locomotive_zhi<-renderText({
    locomotivePV_x<-as.numeric(input$locomotivePV_PV_input)
    PV<-c(locomotivePV_x)
    tm<-c(2016)
    locomotive<-c(0)
    inputdata<-data.frame(tm,locomotive,PV)
    locomotivePV_pred<-as.integer(predict(locomotivePV_svmRegModel,inputdata))
    
    paste("支持向量机预测：",locomotivePV_pred)
    
  }
  )
  #-----------随机森林Tabset画线  
  output$locomotivePV_rfplot <- renderPlot( {
    
    if(input$locomotivePV_year_start> input$locomotivePV_year_end)  {
      
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$frRegPred)
      }
    }
    else{
      locomotivePV_dfsub<-subset(locomotivePV_df,substr(locomotivePV_df$tm,1,4)>=input$locomotivePV_year_start) 
      locomotivePV_dfsub<-subset(locomotivePV_dfsub,substr(locomotivePV_df$tm,1,4)<=input$locomotivePV_year_end)
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$frRegPred)
      }
    }
    
    if(input$locomotivePV_predict_data){
      locomotivePV_p<-locomotivePV_p+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes
                                                                                                                       (x=tm,y=frRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$locomotivePV_stat_data) {
      locomotivePV_p<-locomotivePV_p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    locomotivePV_p+ylab("机车数量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  #----------------------------支持向量机Tabset画线
  
  output$locomotivePV_svmplot <- renderPlot( {
    
    if(input$locomotivePV_year_start> input$locomotivePV_year_end)  {
      
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_df,locomotivePV_df$tm,locomotivePV_df$svmRegPred)
      }
    }
    else{
      locomotivePV_dfsub<-subset(locomotivePV_df,substr(locomotivePV_df$tm,1,4)>=input$locomotivePV_year_start) 
      locomotivePV_dfsub<-subset(locomotivePV_dfsub,substr(locomotivePV_df$tm,1,4)<=input$locomotivePV_year_end)
      if (input$locomotivePV_stat_data) {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$locomotive)
      }
      else
      {
        locomotivePV_p<-plotCurve(locomotivePV_dfsub,locomotivePV_dfsub$tm,locomotivePV_dfsub$svmRegPred)
      }
    }
    if(input$locomotivePV_predict_data){
      locomotivePV_p<-locomotivePV_p+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes
                                                                                                        (x=tm,y=svmRegPred),fill='cornsilk',size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))
    }
    
    if (input$locomotivePV_stat_data) {
      locomotivePV_p<-locomotivePV_p+geom_point(aes(x=tm,y=locomotive),color="red",size=3,shape=21)
    }
    locomotivePV_p+ylab("机车数量")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  
  
  output$locomotivePV_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        locomotivePV_data<-locomotivePV_df
      } , 
      colnames = c('序号', '时间', '机车台数（辆）', '客运量（亿人）','多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
      rownames = TRUE)
  )
  
  
  
  
  
  
  
  
  output$locomotivePV_table<-DT::renderDataTable(
    DT::datatable(
      {
        
        locomotivePV_data<-locomotivePV_df
      } , 
      colnames = c('序号', '时间', '机车台数（辆）', '客运量（亿人）','多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
      rownames = TRUE)
  )
  
  
  
  #------------------------------------------------------------
  #---------客车车辆-营业里程适配性研究------------------------
  #Carriage----------------------客车辆数
  #distance----------------------营业里程
  Carriagedf<-read.csv("客车车辆预测.csv",head=T)
  
  CarriageolsRegModel<-lm(carriage~distance,data=Carriagedf)
  Carriagedf$linearRegPred<-as.integer(predict(CarriageolsRegModel,newdata=Carriagedf))#车辆整数，取整
  
  CarriagerfRegModel<-randomForest(carriage~distance,data=Carriagedf,importance=T, ntree=100,type="regression")   #randFrstReg函数在randomForest.r文件中
  
  Carriagedf$frRegPred<-as.integer(predict(CarriagerfRegModel,Carriagedf))    #<-----------随机森林的预测数据已经在这里计算得到，df是增加一列数据
  
  #-------svmRegModel是支持向量机得到的回归模型，后面也可以直接调用
  CarriagesvmRegModel<-svm(carriage~distance,data=Carriagedf,type="eps-regression",cross=dim(Carriagedf)[1]/2)
  #svm 内含交叉验证，所以不需要再运行交叉验证，支持向量机和随机森林可以按分类，也可以做回归，type选择类型，regression为回归。
  Carriagedf$svmRegPred<-as.integer(predict(CarriagesvmRegModel,Carriagedf))   #<-----------支持向量机的预测数据已经在这里计算得到
  
  Carriagelen<-length(Carriagedf$tm)
  #plotCurve是画曲线的通过用函数，为了减少后面的代码量，替换了ggplot中的东西 
  plotCurve<-function(db,xdata,ydata)
  {
    Carriagelen=dim(xdata)[1]
    Carriageplt<-ggplot(db,x=c(xdata[1],xdata[Carriagelen]),aes(x=xdata,y=ydata),color="red")
    return(Carriageplt)
  }
  #---------------------------多元回归画线
  output$ky_linearplot <- renderPlot( {
    
    if(input$year_start_ky> input$year_end_ky)  {
      
      if (input$stat_data_ky) {
        Carriagep<-plotCurve(Carriagedf,Carriagedf$tm,Carriagedf$carriage)
      }
      else
      {
        Carriagep<-plotCurve(Carriagedf,Carriagedf$tm,Carriagedf$linearRegPred)
      }
    }
    else{
      Carriagedfsub<-subset(Carriagedf,Carriagedf$tm>=input$year_start_ky) 
      Carriagedfsub<-subset(Carriagedfsub,Carriagedfsub$tm<=input$year_end_ky)
      if (input$stat_data_ky) {
        Carriagep<-plotCurve(Carriagedfsub,Carriagedfsub$tm,Carriagedfsub$carriage)
      }
      else
      {
        Carriagep<-plotCurve(Carriagedfsub,Carriagedfsub$tm,Carriagedfsub$linearRegPred)
      }
    }
    
    if(input$predict_data_ky){
      
      Carriagep<-Carriagep+geom_line(aes(x=tm,y=linearRegPred),color="blue",size=1)+geom_point(aes(x=tm,y=linearRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+geom_ribbon(aes(ymin=bound[,2],ymax=bound[,3]),alpha=0.2)
      #+stat_smooth(method=lm,color='black',level=0.95)
    }
    
    if (input$stat_data_ky) {
      Carriagep<-Carriagep+geom_point(aes(x=tm,y=carriage),color="red",size=3,shape=21)
    }
    Carriagep+ylab("客车车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------------------------------
  
  #----------------------------------------------------   
  #多元回归预测计算
  output$ky_carriage_output<-renderText({
    Carriagex1<-as.numeric(input$km_input_ky)#将input中的数据强制转换numeric
    distance<-c(Carriagex1)
    tm<-c(2016)
    carriage<-c(0)
    inputdata<-data.frame(tm,carriage,distance)#data.frame,要有表头，没有任何用的话也要输入一个值，必须把表中空的数据填进去。
    Carriagepred<-as.integer(predict(CarriageolsRegModel,inputdata))
    paste("多元回归预测：",Carriagepred ) 
  }
  )
  #-------------------------------------------------
  #随机森林回归预测计算
  output$ky_carriage_FRR<-renderText({
    Carriagex1<-as.numeric(input$km_input_ky)
    distance<-c(Carriagex1)
    tm<-c(2016)
    carriage<-c(0)
    inputdata<-data.frame(tm,carriage,distance)
    railCarriage<-predict(CarriagerfRegModel,inputdata)   #rfRegModel随机森林在最初已经计算得到
    paste("随机森林回归预测：",as.integer(railCarriage[1])  ) 
    
  }
  )
  #----------------------------------
  #支持向量机回归预测计算
  output$ky_carriage_zhi<-renderText({
    Carriagex1<-as.numeric(input$km_input_ky)
    distance<-c(Carriagex1)
    tm<-c(2016)
    carriage<-c(0)
    inputdata<-data.frame(tm,carriage,distance)
    Carriagepred<-as.integer(predict(CarriagesvmRegModel,inputdata))
    
    paste("支持向量机预测：",Carriagepred)
    
  }
  )
  #-------------------------------------
  
  
  #-----------随机森林Tabset画线  
  output$ky_rfplot <- renderPlot( {
    
    if(input$year_start_ky> input$year_end_ky)  {
      
      if (input$stat_data_ky) {
        Carriagep<-plotCurve(Carriagedf,Carriagedf$tm,Carriagedf$carriage)
      }
      else
      {
        Carriagep<-plotCurve(Carriagedf,Carriagedf$tm,Carriagedf$frRegPred)
      }
    }
    else{
      Carriagedfsub<-subset(Carriagedf,Carriagedf$tm>=input$year_start_ky) 
      Carriagedfsub<-subset(Carriagedfsub,Carriagedfsub$tm<=input$year_end_ky)
      if (input$stat_data_ky) {
        Carriagep<-plotCurve(Carriagedfsub,Carriagedfsub$tm,Carriagedfsub$carriage)
      }
      else
      {
        Carriagep<-plotCurve(Carriagedfsub,Carriagedfsub$tm,Carriagedfsub$frRegPred)
      }
    }
    
    if(input$predict_data_ky){
      Carriagep<-Carriagep+geom_line(aes(x=tm,y=frRegPred),color="blue",size=0.8,show.legend = T)+geom_point(aes(x=tm,y=frRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+stat_smooth(method=rfRegModel,color='black',level=0.95)
    }
    
    if (input$stat_data_ky) {
      Carriagep<-Carriagep+geom_point(aes(x=tm,y=carriage),color="red",size=3,shape=21)
    }
    Carriagep+ylab("客车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #----------------------------支持向量机Tabset画线
  
  output$ky_svmplot <- renderPlot( {
    
    if(input$year_start_ky> input$year_end_ky)  {
      
      if (input$stat_data_ky) {
        Carriagep<-plotCurve(Carriagedf,Carriagedf$tm,Carriagedf$carriage)
      }
      else
      {
        Carriagep<-plotCurve(Carriagedf,Carriagedf$tm,Carriagedf$svmRegPred)
      }
    }
    else{
      Carriagedfsub<-subset(Carriagedf,Carriagedf$tm>=input$year_start_ky) 
      Carriagedfsub<-subset(Carriagedfsub,Carriagedfsub$tm<=input$year_end_ky)
      if (input$stat_data_ky) {
        Carriagep<-plotCurve(Carriagedfsub,Carriagedfsub$tm,Carriagedfsub$carriage)
      }
      else
      {
        Carriagep<-plotCurve(Carriagedfsub,Carriagedfsub$tm,Carriagedfsub$svmRegPred)
      }
    }
    
    if(input$predict_data_ky){
      Carriagep<-Carriagep+geom_line(aes(x=tm,y=svmRegPred),color="blue",size=0.8)+geom_point(aes(x=tm,y=svmRegPred),size=4,shape=21,colour="darkblue",position=position_dodge(width=0.2))#+stat_smooth(method=svmRegModel ,color='black',level=0.95)
    }
    
    if (input$stat_data_ky) {
      Carriagep<-Carriagep+geom_point(aes(x=tm,y=carriage),color="red",size=3,shape=21)
    }
    Carriagep+ylab("客车车辆数")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #--------------------------------------
  
  #----------------------datatable显示数据
  #-----------------在df中，又增加了3列数据，存放预测结果,
  
  output$ky_table<-DT::renderDataTable(
    DT::datatable(
      data<-Carriagedf, 
      colnames = c('序号', '年','客车辆数（辆）','营业里程（公里）','多元回归预测（辆）','随机森林回归预测（辆）','支持向量机回归预测（辆）'),
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
  
  df<-read.csv("freight.csv",head=T)      #freight为货运量数据集，包包含货运量（18个主要货运品类相加）、成品钢材和原煤产量
  df$tm<-as.Date.POSIXct(df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
  
  olsRegModel<-lm(freight~iron+coal,data=df)     #iron表示成品钢材产量，coal表示原煤产量
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
      colnames = c('日期', '货运量(万吨)','成品钢材产量(万吨)','原煤产量(万吨)','多元回归预测(万吨)','随机森林回归预测(万吨)','支持向量机回归预测(万吨)'),
      rownames = TRUE)
  )
  
  
  
  
  #————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————
  #客运量预测
  #————————————————————————————————————————————————————————————————————————————————————————
  #————————————————————————————————————————————————————————————————————————————————————————
  
  passagerpre_df<-read.csv("铁路客运量预测.csv",head=T)     
  passagerpre_df$Year<-as.Date.POSIXct(passagerpre_df$Year,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
  
  #olsRegModel<-lm(passager~iron+coal,data=passagerpre_df)     #iron表示成品钢材产量，coal表示原煤产量
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
    p+ylab("货运量(万吨)")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
  })
  
  #多元回归预测计算
  output$passager_output<-renderText({
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
    p+ylab("货运量(万吨)")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
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
    p+ylab("货运量(万吨)")+xlab("时间")+geom_point(shape=21,color='red',fill='cornsilk',size=3)
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
  freight_ind<-read.csv("货运量.csv",head=T)
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
  
  #-------------------------------------------
  #--------成品钢材产量时间序列预测-----------
  #SteelTime-----------成品钢材产量时间序列预测
  SteelTimeind<-read.csv("成品钢材产量.csv",head=T)
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
  TruckTimeind<-read.csv("货车辆数.csv",head=T)
  TruckTimeindus<-ts(TruckTimeind,start=c(1993),freq=1)
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
  CoalTimeind<-read.csv("原煤产量.csv",head=T)
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
  OilTimeind<-read.csv("原油加工量.csv",head=T)
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
    #freight_volume_rawdata  ------------货运量(亿吨)
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
      p<-p+geom_line(aes(x=tm,y=freight_car),color="red",size=0.6)+ylim(13000,22000)
      p<-p+geom_point(aes(x=tm,y=freight_car),size=4,shape=21,colour="darkred",fill="pink",position=position_dodge(width=0.2))
    }
    #locomotive_number---------- 机车台数
    if (input$property_rawdata=="locomotive_number_rawdata") {
      p<-p+geom_line(aes(x=tm,y=locomotive_number),color="blue",size=0.6)+ylim(13000,25000)
      p<-p+geom_point(aes(x=tm,y=locomotive_number),size=4,shape=21,colour="darkblue",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #bullettrain_number------------动车台数
    if(input$property_rawdata=="bullettrain_number_rawdata"){
      p<-p+geom_line(aes(x=tm,y=bullettrain_number),color="purple",size=0.6)+ylim(500,1500)
      p<-p+geom_point(aes(x=tm,y=bullettrain_number),size=4,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #practitioner_number------------从业人员数量
    if (input$property_rawdata=="practitioner_number_rawdata") {
      p<-p+geom_line(aes(x=tm,y=practitioner_number),color="orange",size=0.6)+ylim(180,320)
      p<-p+geom_point(aes(x=tm,y=practitioner_number),size=4,shape=21,colour="black",fill="cornsilk",position=position_dodge(width=0.2))
    }
    #fixed_assets_investment------------铁路固定资产投资
    if (input$property_rawdata=="fixed_assets_investment_rawdata") {
      p<-p+geom_line(aes(x=tm,y=fixed_assets_investment),color="darkgreen",size=0.6)
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
      colnames = c('时间','成品钢材产量（亿吨）','原油加工量（亿吨）','原煤产量（亿吨）','火力发电量（亿千瓦时）','工业增加值（增长率）'),
      rownames = TRUE))
  
  #rawdata_transport-----------原始数据/运量相关
  output$rawdata_transport_table<-DT::renderDataTable(
    DT::datatable(
      {  
        dfrawdata<-df_monthly
        dfrawdata<-data.frame(dfrawdata$tm,dfrawdata[7:10])
        data<-dfrawdata},
      colnames = c('时间','货运量（亿吨）','货运周转量（亿吨公里）','客运量（亿人）','客运周转量（亿人公里）'),
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
      colnames = c('时间','客车辆数(辆)','机车台数(辆)','货车辆数(万辆)','动车组数(辆)', '铁路固定资产投资(亿元)','从业人员数量(万人)','新线铺轨里程(km)','复线铺轨里程(km))'),
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
        dfrawdata<-data.frame(dfrawdata$tm,dfrawdata[13:18])
        data<-dfrawdata},
      colnames = c('时间','工业机械(万吨)','电子电气(万吨)','农副产品(万吨)', '饮食烟草(万吨)','文教用品(万吨)','零担(吨)','集装箱(万吨)'),
      rownames = TRUE))
  
}
)
