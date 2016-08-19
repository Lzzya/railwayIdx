require(shiny)
require(ggplot2)
require(DT)
require(markdown)



#-----------------------------------界面所需数据集------------------------------
#-----------------------------------------
#-------预警信号系统----------------------
df_index<-read.csv("预警.csv",header=T)
df_index$tm<-as.Date.POSIXct(df_index$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据

#-----------------------------------------
#-------铁路景气指数----------------------
dftrans<-read.csv("trans-coor.csv",head=T)
dftrans$tm<-as.Date.POSIXct(dftrans$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
y_wenjing<-unique(substr(dftrans$tm,1,4))

dftrans_DI<-read.csv("DI_Trans.csv",head=T)
dftrans_DI$tm<-as.Date.POSIXct(dftrans_DI$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
y_yiheng<-unique(substr(dftrans_DI$tm,1,4))

#-----------------------------------------
#-------黑货白货指数----------------------
liaozili<-read.csv("index-black.csv",head=T)
liaozili$tm<-as.Date.POSIXct(liaozili$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
liaozili_y<-unique(substr(liaozili$tm,1,4))

#-----------------------------------------
#-------适配性研究----------------------
locomotive_dis<-read.csv("Locomotive-dis.csv",head=T)
locomotive_dis_y<-unique(substr(locomotive_dis$tm,1,4))


locomotive_PV<-read.csv("Locomotive-PV.csv",head=T)
locomotive_PV_y<-unique(substr(locomotive_PV$tm,1,4))


locomotive_PV<-read.csv("Locomotive-PV.csv",head=T)
locomotive_PV_y<-unique(substr(locomotive_PV$tm,1,4))
#=======
Locomotive_fre<-read.csv("Locomotive-freight.csv",head=T)#机车-货运量适配性研：读表
Locomotive_tm<-unique(substr(Locomotive_fre$tm,1,4))
#>>>>>>> liaozili


freightcar_dis<-read.csv("货车车辆预测.csv",head=T)
freightcar_dis_y<-unique(substr(freightcar_dis$tm,1,4))

cw_df<-read.csv("动车增加数量.csv",head=T)
cw_y<-unique(substr(cw_df$tm,1,4))

investment_df<-read.csv("investment-passenger.csv",head=T)#固定资产-客车数量适配性研：读表
investment_y<-unique(substr(investment_df$tm,1,4))

cw_truck_df<-read.csv("truck-asset.csv",head=T)
cw_truck_y<-unique(substr(cw_truck_df$tm,1,4))

JCNum_df<-read.csv("固定资产-机车台数.csv",head=T)#固定资产-机车台数适配性研究
JCNum_y<-unique(substr(JCNum_df$tm,1,4))

pg_cw_df<-read.csv("固定资产指标.csv",head=T)  #固定资产和铺轨里程（新线铺轨历程，旧线铺轨里程）
pg_cw_y<-unique(substr(pg_cw_df$tm,1,4))

Carriagedf<-read.csv("客车车辆预测.csv",head=T)
Carriagey<-unique(substr(Carriagedf$tm,1,4))

operatingmileage_df<-read.csv("营业里程.csv",head=T)
operatingmileage_y<-unique(substr(operatingmileage_df$tm,1,4))

PVdf<-read.csv("客运量.csv",head=T)
PVy<-unique(substr(PVdf$PVtm,1,4))

freight_car_df<-read.csv("货运量-营业里程.csv",head=T)  
freight_car_df$tm<-as.Date.POSIXct(freight_car_df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
freight_car_y<-unique(substr(freight_car_df$tm,1,4))

#-----------------------------------------
#-------货运量预测------------------------
freight_forecast_df<-read.csv("freight.csv",head=T)
freight_forecast_df$tm<-as.Date.POSIXct(freight_forecast_df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) 
y<-as.numeric(unique(substr(freight_forecast_df$tm,1,4)))

#-----------------------------------------
#-------客运量预测------------------------
passagerpre_df<-read.csv("铁路客运量预测.csv",head=T)
passagerpre_y<-unique(substr(passagerpre_df$Year,1,4)) #提取年份

#-----------------------------------------
#---------原始数据------------------------
dfyssj<-read.csv("compidx-qitahangye.csv",head=T)
dfyssj$tm<-as.Date.POSIXct(dfyssj$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  
y.wenjing.yssj<-unique(substr(dfyssj$tm,1,4))

dfyssj.hhbh<-read.csv("compidx-heihuobaihuo.csv",head=T)
dfyssj.hhbh$tm<-as.Date.POSIXct(dfyssj.hhbh$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  
y.wenjing.yssj.hhbh<-unique(substr(dfyssj.hhbh$tm,1,4))

#———————————————————————————————————————————————————————————————————————————————
#-----------------------------------------主界面--------------------------------

shinyUI(navbarPage(p(strong("铁路景气指数"),responsive=T,fluid=T),

                   
#-------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#预警信号系统界面

                   tabPanel("预警信号系统",
                            titlePanel("铁路预警信号灯"),
                            hr(),                          
                            plotOutput(outputId = "plot_index", height = "400px"),
                            hr(),
                            wellPanel(
                              h4("铁路运输景气预警信号系统，是借助于相关计量经济分析方法，将多个指标进行数据处理，合并为一个综合性"),
                              h4("的指标，对这组指标和综合指标所代表的铁路运输波动状况发出预警信号，通过观察信号的变化情况，来判断"), 
                              h4("未来铁路运输增长的趋势。在本APP中，五种颜色信号的含义如下："),
                              h4("绿灯---铁路运输发展很稳定"),
                              h4("蓝灯&浅蓝灯---运输市场景气偏热"),
                              h4("黄灯---铁路运输短期内有转稳和萎缩的可能"),
                              h4("红灯---铁路运输市场景气偏冷")
                            ),
                            fluidRow(  DT::dataTableOutput("index_table")   )
                   ),





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#铁路景气指数界面，包括合成指数和扩散指数

navbarMenu("铁路景气指数",
              tabPanel("合成指数",
                        titlePanel("铁路景气合成指数"),
                           fluidRow(
                               column(12, tabsetPanel(type="tabs",
                                                                
                                                                tabPanel( "运输合成指数", 
                                                                          fluidRow(
                                                                            sidebarLayout(
                                                                              sidebarPanel(
                                                                                h4(strong("1.各要素权重默认"),style="color:black"),
                                                                                checkboxInput(inputId="trans_coor_Index", #trans_coor_Index默认的运输合成同步指数复选框
                                                                                              label=("同步指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId="trans_advanced_Index",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                                              label=("先行指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId = "trans_delay_Index",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                                              label = ("滞后指数"),
                                                                                              value = TRUE),
                                                                                
                                                                                h4(strong("2.各要素权重手动调整"),style="color:black"),
                                                                                checkboxInput(inputId="trans_qz_coor_input",#trans_qz_coor_input权重手动输入的运输合成同步指数复选框
                                                                                              label = strong("2.1 同步指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="trans_hyl_qz_input",#trans_hyl_qz_input 货运量的权重输入框
                                                                                          label=h5("货运量%"),
                                                                                          value="38.66"),
                                                                                textInput(inputId="trans_gyzjz_qz_input",#trans_gyzjz_qz_input工业增加值的权重输入框
                                                                                          label=h5("工业增加值%"),
                                                                                          value="29.74"),
                                                                                textInput(inputId="trans_hyzzl_qz_input",#trans_hyzzl_qz_input货运的权重输入框
                                                                                          label=h5("货运周转量%"),
                                                                                          value="31.60"),
                                                                                
                                                                                checkboxInput(inputId="trans_qz_adv_input",#trans_qz_adv_input权重手动输入的运输合成先行指数复选框
                                                                                              label = strong("2.2 先行指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="trans_gc_qz_input",#trans_gc_qz_input运输合成指数中的钢材的权重输入框
                                                                                          label=h5("成品钢材%"),
                                                                                          value="42.60"),
                                                                                textInput(inputId="trans_ym_qz_input",#trans_ym_qz_input运输合成指数中的原煤的权重输入框
                                                                                          label=h5("原煤%"),
                                                                                          value="25.80"),
                                                                                textInput(inputId="trans_yy_qz_input",#trans_yy_qz_input运输合成指数中的原油的权重输入框
                                                                                          label=h5("原油%"),
                                                                                          value="10.31"),
                                                                                textInput(inputId="trans_hlfdl_qz_input",#trans_hlfdl_qz_input运输合成指数中的火力发电量的权重输入框
                                                                                          label=h5("火力发电量%"),
                                                                                          value="21.29"),
                                                                                
                                                                                checkboxInput(inputId="trans_qz_delay_input",#trans_qz_adv_input权重手动输入的运输合成滞后指数复选框
                                                                                              label = strong("2.3 滞后指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="trans_kyl_qz_input",#trans_kyl_qz_input客运量的权重输入框
                                                                                          label=h5("客运量%"),
                                                                                          value="9.46"),
                                                                                textInput(inputId="trans_kyzzl_qz_input",#trans_kyzzl_qz_input客运周转量的权重输入框
                                                                                          label=h5("客运周转量%"),
                                                                                          value="10.06"),
                                                                                textInput(inputId="trans_gdzctz_qz_input",#trans_gdzctz_qz_input固定资产投资的权重输入框
                                                                                          label=h5("固定资产投资%"),
                                                                                          value="80.48"),
                                                                                width=3
                                                                              ),#侧边框
                                                                              
                                                                              
                                                                              mainPanel(
                                                                                fluidRow(
                                                                                  column(3,  selectInput(inputId = "year_start_trans",#year_start_trans运输合成指数中的起始年下拉框
                                                                                                         label = "自:", 
                                                                                                         choices = y_wenjing,
                                                                                                         selected = min(y_wenjing) )),
                                                                                  column(3, selectInput(inputId="year_end_trans",#year_end_trans运输合成指数中的终止年下拉框
                                                                                                        label="至:",
                                                                                                        choice=y_wenjing,
                                                                                                        selected=max(y_wenjing)))
                                                                                ),
                                                                                plotOutput(outputId="trans_index", height = "400px"), #trans_index 运输合成指数的画图
                                                                                fluidRow(
                                                                                  column(12,DT::dataTableOutput("table_trans_index"))  ),#table_trans_index 运输合成指数的数据表输出
                                                                                width=9
                                                                              )#主显示区
                                                                              
                                                                            ))), #运输指数的页签
                                                                
                                                                tabPanel( "设备合成指数", 
                                                                          fluidRow(
                                                                            sidebarLayout(
                                                                              sidebarPanel(
                                                                                h4(strong("1.各要素权重默认"),style="color:black"),
                                                                                checkboxInput(inputId="equip_coor_Index",#equip_coor_Index默认的设备合成同步指数复选框
                                                                                              label=("同步指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId = "equip_delay_Index",#equip_delay_Index默认的设备合成滞后指数复选框
                                                                                              label = ("滞后指数"),
                                                                                              value = TRUE),
                                                                                checkboxInput(inputId="equip_advanced_Index",#equip_advanced_Index默认的设备合成先行指数复选框
                                                                                              label=("先行指数"),
                                                                                              value=TRUE),
                                                                                
                                                                                h4(strong("2.各要素权重手动调整"),style="color:black"),
                                                                                checkboxInput(inputId="equip_qz_coor_input",#equip_qz_adv_input权重手动输入的设备合成同步指数复选框
                                                                                              label = strong("2.1 同步指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_jczxzlc_qz_input",#equip_jczxzlc_qz_input机车总行走里程的权重输入框
                                                                                          label=h5("机车总行走里程%"),
                                                                                          value="81.28"),
                                                                                textInput(inputId="equip_rjyyc_qz_input",#equip_rjyyc_qz_input日均运用车的权重输入框
                                                                                          label=h5("日均运用车%"),
                                                                                          value="18.72"),
                                                                                
                                                                                checkboxInput(inputId="equip_qz_adv_input",#equip_qz_adv_input权重手动输入的设备合成先行指数复选框
                                                                                              label = strong("2.2 先行指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_gc_qz_input",#equip_gc_qz_input设备合成指数中的成品钢材的权重输入框
                                                                                          label=h5("成品钢材%"),
                                                                                          value="43.39"),
                                                                                textInput(inputId="equip_ym_qz_input",#equip_ym_qz_input设备合成指数中的原煤的权重输入框
                                                                                          label=h5("原煤%"),
                                                                                          value="26.53"),
                                                                                textInput(inputId="equip_yy_qz_input",#equip_yy_qz_input设备合成指数中的原油的权重输入框
                                                                                          label=h5("原油%"),
                                                                                          value="10.56"),
                                                                                textInput(inputId="equip_hlfdl_qz_input",#equip_hlfdl_qz_input设备合成指数中的火力发电量的权重输入框
                                                                                          label=h5("火力发电量%"),
                                                                                          value="19.51"),
                                                                                
                                                                                checkboxInput(inputId="equip_qz_delay_input",#equip_qz_delay_input权重手动输入的设备合成滞后指数复选框
                                                                                              label = strong("2.3 滞后指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_rjxzc_qz_input",#equip_rjxzc_qz_input设备合成指数中的日均现在车的权重输入框
                                                                                          label=h5("日均现在车%"),
                                                                                          value="15.44"),
                                                                                textInput(inputId="equip_kyjclc_qz_input",#equip_kyjclc_qz_input设备合成指数中的客运机车里程的权重输入框
                                                                                          label=h5("客运机车里程%"),
                                                                                          value="35.92"),
                                                                                textInput(inputId="equip_hyjclc_qz_input",#equip_hyjclc_qz_input设备合成指数中的货运机车里程的权重输入框
                                                                                          label=h5("货运机车里程%"),
                                                                                          value="1.80"),
                                                                                textInput(inputId="equip_kcls_qz_input",#equip_kcls_qz_input设备合成指数中的客车辆数的权重输入框
                                                                                          label=h5("客车辆数%"),
                                                                                          value="21.42"),
                                                                                textInput(inputId="equip_hcls_qz_input",#equip_hcls_qz_input设备合成指数中的货车辆数的权重输入框
                                                                                          label=h5("货车辆数%"),
                                                                                          value="16.34"),
                                                                                textInput(inputId="equip_jcts_qz_input",#equip_jcts_qz_input设备合成指数中的机车台数的权重输入框
                                                                                          label=h5("机车台数%"),
                                                                                          value="9.08"),
                                                                                
                                                                                width=3
                                                                              ),
                                                                              
                                                                              mainPanel(
                                                                                fluidRow(
                                                                                  column(3,  selectInput(inputId = "year_start_equip",#year_start_trans设备合成指数中的起始年下拉框
                                                                                                         label = "自:", 
                                                                                                         choices = y_wenjing,
                                                                                                         selected = min(y_wenjing) )),
                                                                                  column(3, selectInput(inputId="year_end_equip",#year_start_trans设备合成指数中的终止年下拉框
                                                                                                        label="至:",
                                                                                                        choice=y_wenjing,
                                                                                                        selected=max(y_wenjing)))
                                                                                ),
                                                                                plotOutput(outputId="equip_index", height = "400px"),#equip_index 设备合成指数的画图
                                                                                fluidRow(
                                                                                  column(12,DT::dataTableOutput("table_equip_index"))  ),#table_equip_index 设备合成指数的数据表输出
                                                                                width=9
                                                                              )
                                                                              
                                                                            ))), #设备指数的页签
                                                                
                                                                tabPanel( "规模合成指数", 
                                                                          fluidRow(
                                                                            sidebarLayout(
                                                                              sidebarPanel(
                                                                                h4(strong("1.各要素权重默认"),style="color:black"),
                                                                                checkboxInput(inputId="scale_coor_Index",#scale_coor_Index默认的规模合成同步指数复选框
                                                                                              label=strong("同步指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId="scale_advanced_Index",#scale_advanced_Index默认的规模合成先行指数复选框
                                                                                              label=strong("先行指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId = "scale_delay_Index",#scale_delay_Index默认的规模合成滞后指数复选框
                                                                                              label = strong("滞后指数"),
                                                                                              value = TRUE),
                                                                                
                                                                                h4(strong("2.各要素权重手动调整"),style="color:black"),
                                                                                checkboxInput(inputId="scale_qz_coor_input",#sacale_qz_coor_input权重手动输入的规模合成同步指数复选框
                                                                                              label = strong("2.1 同步指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_hyl_qz_input",#scale_hyl_qz_input设备合成指数中的货运量的权重输入框
                                                                                          label=h5("货运量%"),
                                                                                          value="17.87"),
                                                                                textInput(inputId="scale_gyzjz_qz_input",#scale_gyzjz_qz_input设备合成指数中的工业增加值的权重输入框
                                                                                          label=h5("工业增加值%"),
                                                                                          value="67.71"),
                                                                                textInput(inputId="scale_hyzzl_qz_input",#scale_hyzzl_qz_input设备合成指数中的货运周转量的权重输入框
                                                                                          label=h5("货运周转量%"),
                                                                                          value="14.42"),
                                                                                
                                                                                checkboxInput(inputId="scale_qz_adv_input",#sacale_qz_adv_input权重手动输入的规模合成先行指数复选框
                                                                                              label = strong("2.2 先行指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_gc_qz_input",#scale_gc_qz_input设备合成指数中的成品钢材产量的权重输入框
                                                                                          label=h5("成品钢材%"),
                                                                                          value="43.39"),
                                                                                textInput(inputId="scale_ym_qz_input",#scale_ym_qz_input设备合成指数中的原煤产量的权重输入框
                                                                                          label=h5("原煤%"),
                                                                                          value="26.53"),
                                                                                textInput(inputId="scale_yy_qz_input",#scale_yy_qz_input设备合成指数中的原油产量的权重输入框
                                                                                          label=h5("原油%"),
                                                                                          value="10.56"),
                                                                                textInput(inputId="scale_hlfdl_qz_input",#scale_hlfdl_qz_input设备合成指数中的火力发电量的权重输入框
                                                                                          label=h5("火力发电量%"),
                                                                                          value="19.51"),
                                                                                
                                                                                checkboxInput(inputId="scale_qz_delay_input",#sacale_qz_delay_input权重手动输入的规模合成滞后指数复选框
                                                                                              label = strong("2.3 滞后指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_yylc_qz_input",#scale_yylc_qz_input设备合成指数中的营业里程的权重输入框
                                                                                          label=h5("营业里程%"),
                                                                                          value="23.62"),
                                                                                textInput(inputId="scale_cyrysl_qz_input",#scale_cyrysl_qz_input设备合成指数中的从业人员数量的权重输入框
                                                                                          label=h5("从业人员数量%"),
                                                                                          value="6.62"),
                                                                                textInput(inputId="scale_kcls_qz_input",#scale_kcls_qz_input设备合成指数中的客车辆数的权重输入框
                                                                                          label=h5("客车辆数%"),
                                                                                          value="31.90"),
                                                                                textInput(inputId="scale_hcls_qz_input",#scale_hcls_qz_input设备合成指数中的货车辆数的权重输入框
                                                                                          label=h5("货车辆数%"),
                                                                                          value="24.33"),
                                                                                textInput(inputId="scale_jcts_qz_input",#scale_jcts_qz_input设备合成指数中的机车台数的权重输入框
                                                                                          label=h5("机车台数%"),
                                                                                          value="13.53"),
                                                                                
                                                                                width=3
                                                                                
                                                                              ),
                                                                              
                                                                              mainPanel(
                                                                                fluidRow(
                                                                                  column(3,  selectInput(inputId = "year_start_scale",#year_start_scale规模合成指数中的起始年下拉框
                                                                                                         label = "自:", 
                                                                                                         choices = y_wenjing,
                                                                                                         selected = min(y_wenjing) )),
                                                                                  column(3, selectInput(inputId="year_end_scale",#year_start_scale规模合成指数中的终止年下拉框
                                                                                                        label="至:",
                                                                                                        choice=y_wenjing,
                                                                                                        selected=max(y_wenjing)))
                                                                                ),
                                                                                plotOutput(outputId="scale_index", height = "400px"),#scale_index 规模合成指数的画图
                                                                                fluidRow(
                                                                                  column(12,DT::dataTableOutput("table_scale_index")) ),#table_scale_index 规模合成指数的数据表输出
                                                                                width=9
                                                                              )
                                                                              
                                                                            ))) #规模指数的页签
                                                                
                                                                
                                         )))
                                       
                                       ),
                              tabPanel("扩散指数",
                                       titlePanel("铁路扩散景气指数"),
                                       
                                       fluidRow(
                                         column(12, tabsetPanel(type="tabs",
                                                                
                                                                tabPanel( "运输扩散指数", 
                                                                          sidebarLayout(
                                                                            sidebarPanel(
                                                                              h4(strong("1.各要素权重默认"),style="color:black"),
                                                                              checkboxInput(inputId="trans_DIt_Index",
                                                                                            label=("同步指数"),
                                                                                            value=TRUE),
                                                                              checkboxInput(inputId="trans_DIx_Index",
                                                                                            label=("先行指数"),
                                                                                            value=TRUE),
                                                                              checkboxInput(inputId = "trans_DIz_Index",
                                                                                            label = ("滞后指数"),
                                                                                            value = TRUE),
                                                                              
                                                                              h4(strong("2.各要素权重手动调整"),style="color:black"),
                                                                              checkboxInput(inputId="trans_percent_coor_input",
                                                                                            label = strong("2.1 同步指数要素权重"),
                                                                                            value = FALSE),
                                                                              textInput(inputId="trans_hyl_percent_input",
                                                                                        label=h5("货运量%"),
                                                                                        value="38.66"),
                                                                              textInput(inputId="trans_gyzjz_percent_input",
                                                                                        label=h5("工业增加值%"),
                                                                                        value="29.74"),
                                                                              textInput(inputId="trans_hyzzl_percent_input",
                                                                                        label=h5("货运周转量%"),
                                                                                        value="31.60"),
                                                                              
                                                                              checkboxInput(inputId="trans_percent_adv_input",
                                                                                            label = strong("2.2 先行指数要素权重"),
                                                                                            value = FALSE),
                                                                              textInput(inputId="trans_gc_percent_input",
                                                                                        label=h5("成品钢材%"),
                                                                                        value="42.60"),
                                                                              textInput(inputId="trans_ym_percent_input",
                                                                                        label=h5("原煤%"),
                                                                                        value="25.80"),
                                                                              textInput(inputId="trans_yy_percent_input",
                                                                                        label=h5("原油%"),
                                                                                        value="10.31"),
                                                                              textInput(inputId="trans_hlfdl_percent_input",
                                                                                        label=h5("火力发电量%"),
                                                                                        value="21.29"),
                                                                              
                                                                              checkboxInput(inputId="trans_percent_delay_input",
                                                                                            label = strong("2.3 滞后指数要素权重"),
                                                                                            value = FALSE),
                                                                              textInput(inputId="trans_kyl_percent_input",
                                                                                        label=h5("客运量%"),
                                                                                        value="9.30"),
                                                                              textInput(inputId="trans_kyzzl_percent_input",
                                                                                        label=h5("客运周转量%"),
                                                                                        value="9.90"),
                                                                              textInput(inputId="trans_gdzctz_percent_input",
                                                                                        label=h5("固定资产投资%"),
                                                                                        value="78.80"),
                                                                              textInput(inputId="trans_yylc_percent_input",
                                                                                        label=h5("营业里程%"),
                                                                                        value="2.00"),
                                                                              width=3
                                                                            ),
                                                                            
                                                                            
                                                                            mainPanel(
                                                                              fluidRow(
                                                                                column(3,  selectInput(inputId = "year_start_trans_ID",
                                                                                                       label = "自:", 
                                                                                                       choices = y_yiheng,
                                                                                                       selected = min(y_yiheng) )),
                                                                                column(3, selectInput(inputId="year_end_trans_ID",
                                                                                                      label="至:",
                                                                                                      choice=y_yiheng,
                                                                                                      selected=max(y_yiheng)))
                                                                              ),
                                                                              plotOutput(outputId = "trans_DI_index", height = "400px"),
                                                                              fluidRow(
                                                                                column(12,DT::dataTableOutput("table_trans_DI_index")) ),
                                                                              width=9
                                                                              )#主显示区
                                                                          )), #运输指数的页签
                                                                
                                                                #------------------------------------------------------------  
                                                                tabPanel( "设备扩散指数", 
                                                                          fluidRow(
                                                                            sidebarLayout(
                                                                              sidebarPanel(
                                                                                h4(strong("1.各要素权重默认"),style="color:black"),
                                                                                checkboxInput(inputId="equip_DIt_Index",
                                                                                              label=("同步指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId = "equip_DIz_Index",
                                                                                              label = ("滞后指数"),
                                                                                              value = TRUE),
                                                                                checkboxInput(inputId="equip_DIx_Index",
                                                                                              label=("先行指数"),
                                                                                              value=TRUE),
                                                                                
                                                                                h4(strong("2.各要素权重手动调整"),style="color:black"),
                                                                                checkboxInput(inputId="equip_percent_coor_input",
                                                                                              label = strong("2.1 同步指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_jczxzlc_percent_input",
                                                                                          label=h5("机车总行走里程%"),
                                                                                          value="81.28"),
                                                                                textInput(inputId="equip_rjyyc_percent_input",
                                                                                          label=h5("日均运用车%"),
                                                                                          value="18.72"),
                                                                                
                                                                                checkboxInput(inputId="equip_percent_adv_input",
                                                                                              label = strong("2.2 先行指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_gc_percent_input",
                                                                                          label=h5("成品钢材%"),
                                                                                          value="43.39"),
                                                                                textInput(inputId="equip_ym_percent_input",
                                                                                          label=h5("原煤%"),
                                                                                          value="26.53"),
                                                                                textInput(inputId="equip_yy_percent_input",
                                                                                          label=h5("原油%"),
                                                                                          value="10.56"),
                                                                                textInput(inputId="equip_hlfdl_percent_input",
                                                                                          label=h5("火力发电量%"),
                                                                                          value="19.51"),
                                                                                
                                                                                checkboxInput(inputId="equip_percent_delay_input",
                                                                                              label = strong("2.3 滞后指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_rjxzc_percent_input",
                                                                                          label=h5("日均现在车%"),
                                                                                          value="15.44"),
                                                                                textInput(inputId="equip_kyjclc_percent_input",
                                                                                          label=h5("客运机车里程%"),
                                                                                          value="35.92"),
                                                                                textInput(inputId="equip_hyjclc_percent_input",
                                                                                          label=h5("货运机车里程%"),
                                                                                          value="1.80"),
                                                                                textInput(inputId="equip_kcls_percent_input",
                                                                                          label=h5("客车辆数%"),
                                                                                          value="21.42"),
                                                                                textInput(inputId="equip_hcls_percent_input",
                                                                                          label=h5("货车辆数%"),
                                                                                          value="16.34"),
                                                                                textInput(inputId="equip_jcts_percent_input",
                                                                                          label=h5("机车台数%"),
                                                                                          value="9.08"),
                                                                                width=3
                                                                              ),
                                                                              
                                                                              mainPanel(
                                                                                fluidRow(
                                                                                  column(3,  selectInput(inputId = "year_start_equip_ID",
                                                                                                         label = "自:", 
                                                                                                         choices = y_yiheng,
                                                                                                         selected = min(y_yiheng) )),
                                                                                  column(3, selectInput(inputId="year_end_equip_ID",
                                                                                                        label="至:",
                                                                                                        choice=y_yiheng,
                                                                                                        selected=max(y_yiheng)))
                                                                                ),
                                                                                plotOutput(outputId="equip_DI_index", height = "400px"),
                                                                                fluidRow(
                                                                                  column(12,DT::dataTableOutput("table_equip_DI_index"))  ),
                                                                                width=9
                                                                              )
                                                                              
                                                                            ))), #设备指数的页签
                                                                
                                                                tabPanel( "规模扩散指数", 
                                                                          fluidRow(
                                                                            sidebarLayout(
                                                                              sidebarPanel(
                                                                                h4(strong("1.各要素权重默认"),style="color:black"),
                                                                                checkboxInput(inputId="scale_DIt_Index",
                                                                                              label=("同步指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId="scale_DIx_Index",
                                                                                              label=("先行指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId = "scale_DIz_Index",
                                                                                              label = ("滞后指数"),
                                                                                              value = TRUE),
                                                                                
                                                                                h4(strong("2.各要素权重手动调整"),style="color:black"),
                                                                                checkboxInput(inputId="scale_percent_coor_input",
                                                                                              label = strong("2.1 同步指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_hyl_percent_input",
                                                                                          label=h5("工业增加值%"),
                                                                                          value="17.87"),
                                                                                textInput(inputId="scale_gyzjz_percent_input",
                                                                                          label=h5("货运量%"),
                                                                                          value="67.71"),
                                                                                textInput(inputId="scale_hyzzl_percent_input",
                                                                                          label=h5("货运周转量%"),
                                                                                          value="14.42"),
                                                                                
                                                                                checkboxInput(inputId="scale_percent_adv_input",
                                                                                              label = strong("2.2 先行指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_gc_percent_input",
                                                                                          label=h5("成品钢材%"),
                                                                                          value="43.39"),
                                                                                textInput(inputId="scale_ym_percent_input",
                                                                                          label=h5("原煤%"),
                                                                                          value="26.53"),
                                                                                textInput(inputId="scale_yy_percent_input",
                                                                                          label=h5("原油%"),
                                                                                          value="10.56"),
                                                                                textInput(inputId="scale_hlfdl_percent_input",
                                                                                          label=h5("火力发电量%"),
                                                                                          value="19.51"),
                                                                                
                                                                                checkboxInput(inputId="scale_percent_delay_input",
                                                                                              label = strong("2.3 滞后指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_yylc_percent_input",
                                                                                          label=h5("营业里程%"),
                                                                                          value="23.62"),
                                                                                textInput(inputId="scale_cyrysl_percent_input",
                                                                                          label=h5("从业人员数量%"),
                                                                                          value="6.62"),
                                                                                textInput(inputId="scale_kcls_percent_input",
                                                                                          label=h5("客车辆数%"),
                                                                                          value="31.90"),
                                                                                textInput(inputId="scale_hcls_percent_input",
                                                                                          label=h5("货车辆数%"),
                                                                                          value="24.33"),
                                                                                textInput(inputId="scale_jcts_percent_input",
                                                                                          label=h5("机车台数%"),
                                                                                          value="13.53"),
                                                                                
                                                                                width=3
                                                                                
                                                                              ),
                                                                              
                                                                              mainPanel(
                                                                                fluidRow(
                                                                                  column(3,  selectInput(inputId = "year_start_scale_ID",
                                                                                                         label = "自:", 
                                                                                                         choices = y_yiheng,
                                                                                                         selected = min(y_yiheng) )),
                                                                                  column(3, selectInput(inputId="year_end_scale_ID",
                                                                                                        label="至:",
                                                                                                        choice=y_yiheng,
                                                                                                        selected=max(y_yiheng)))
                                                                                ),
                                                                                plotOutput(outputId="scale_DI_index", height = "400px"),
                                                                                fluidRow(
                                                                                  column(12,DT::dataTableOutput("table_scale_DI_index")) ),
                                                                                width=9
                                                                              )
                                                                              
                                                                            ))) #规模指数的页签
                                                                
                                                                
                                         )))
                                       )
                   ),



#-----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#黑白货指数界面

tabPanel("黑货白货指数",
         titlePanel("黑货白货指数"),
         fluidRow(
           column(12, tabsetPanel(type="tabs",
                                  
                                  
#黑货指数数的页签-----------------------------------------------
                                  
                                  tabPanel( "黑货指数", 
                                            fluidRow(
                                              sidebarLayout(
                                                sidebarPanel(
                                                  selectInput(inputId = "liaozili_year_start",# 时间选项框
                                                              label = "自:", 
                                                              choices = liaozili_y,
                                                              selected = min(liaozili_y),
                                                              width =('100%')),
                                                  selectInput(inputId="liaozili_year_end",
                                                              label="至:",
                                                              choice=liaozili_y,
                                                              selected=max(liaozili_y),
                                                              width =('100%')),
                                                  
                                                  numericInput(inputId="weightcoal_input",#原煤权重输入
                                                               label=h6("原煤权重(%)"),
                                                               value='66.93',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightoil_input",#石油权重输入
                                                               label=h6("石油权重(%)"),
                                                               value='5.22',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightmetal_input",#金属矿石权重
                                                               label=h6("金属矿石权重(%)"),
                                                               value='14.97',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightiron_input",#钢铁权重输入
                                                               label=h6("钢铁权重(%)"),
                                                               value='8.02',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightmine_input",#矿建权重输入
                                                               label=h6("矿建权重(%)"),
                                                               value= "4.84",
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  width =3
                                                ),
                                                mainPanel(plotOutput(outputId="heihuo_index", height = "400px"),#heihuo_index 表示黑货指数
                                                          fluidRow( DT::dataTableOutput("heihuotable",width = "100%", height = "auto")),#heihuotable 黑货指数数据显示
                                                          width =8)
                                                
                                                # plotOutput(outputId="heihuo_index", height = "400px")
                                                
                                              )
                                            )
                                            
                                  ),
 #白货指数数的页签-----------------------------------------------
                                  tabPanel( "白货指数", 
                                            fluidRow(
                                              sidebarLayout(
                                                sidebarPanel(
                                                  selectInput(inputId = "liaozili_year2_start",#时间框
                                                              label = "自:", 
                                                              choices = liaozili_y,
                                                              selected = min(liaozili_y),
                                                              width =('100%')),
                                                  selectInput(inputId="liaozili_year2_end",
                                                              label="至:",
                                                              choice=liaozili_y,
                                                              selected=max(liaozili_y),
                                                              width =('100%')),
                                                  numericInput(inputId="weightmachinery_input",#工程机械权重输入
                                                               label=h6("工程机械权重(%)"),
                                                               value='18.10',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightelectronic_input",#电子电器权重输入
                                                               label=h6("电子电器权重(%)"),
                                                               value='18.80',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightagricultural_input",#农副产品权重输入
                                                               label=h6("农副产品权重(%)"),
                                                               value='11.10',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightfood_input",#饮食烟草权重输入
                                                               label=h6("饮食烟草权重(%)"),
                                                               value='17.19',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weighteducation_input",#科教用品权重输入
                                                               label=h6("科教用品权重(%)"),
                                                               value='17.77',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightltl_input",#零担权重输入
                                                               label=h6("零担权重(%)"),
                                                               value='4.29',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  numericInput(inputId="weightcontainer_input",#集装箱权重输入
                                                               label=h6("集装箱权重(%)"),
                                                               value='12.75',
                                                               min=0,
                                                               max=100,
                                                               step=0.1),
                                                  width = 3 
                                                ),
                                                mainPanel(
                                                  plotOutput(outputId="baihuo_index", height = "400px"),#白货指数
                                                  fluidRow(DT::dataTableOutput("baihuotable")),#白货数据显示
                                                  width =8)
                                                
                                                #plotOutput(outputId="baihuo_index", height = "400px")
                                                
                                              )
                                            )
                                            
                                  )
                                  
           )
           )
         )
         ),



#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#适配性研究界面


                   navbarMenu("适配性研究",
                     tabPanel("固定资产-营业里程",
                              titlePanel("固定资产-营业里程"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="operatingmileage_stat_data",
                                                label=strong("历史统计值"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "operatingmileage_predict_data",
                                                label = strong("回归预测值"),
                                                value = TRUE),
                                  selectInput(inputId = "operatingmileage_year_start",
                                              label = "自:", 
                                              choices = operatingmileage_y,
                                              selected = min(operatingmileage_y) ),
                                  selectInput(inputId="operatingmileage_year_end",
                                              label="至:",
                                              choice=operatingmileage_y,
                                              selected=max(operatingmileage_y) ),
                                  textInput(inputId="operatingmileage_input",
                                            label=strong("营业里程"),
                                            value=round(mean(operatingmileage_df$operatingmileage),2)),
                                  hr("预测结果—固定资产值（亿元）"),
                                  hr(),
                                  textOutput("operatingmileage_asset_output") ,
                                  hr(),
                                  textOutput("operatingmileage_asset_FRR"),
                                  hr(),
                                  textOutput("operatingmileage_asset_zhi")
                                  
                                  # actionButton("predictFre","预测新货运量") 
                                ),                                                       #sidebarPanel
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回归", plotOutput("operatingmileage_linearplot")), 
                                    tabPanel("随机森林回归", plotOutput("operatingmileage_rfplot")), 
                                    tabPanel("支持向量机回归", plotOutput("operatingmileage_svmplot"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("operatingmileage_table")   )
                                )
                              )
                              ),
                     
                     tabPanel("固定资产-铺轨里程",
                              titlePanel("固定资产投资-铺轨里程"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="mileage_stat_data",
                                                label=strong("历史统计值"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "mileage_predict_data",
                                                label = strong("回归预测值"),
                                                value = TRUE),
                                  selectInput(inputId = "mileage_year_start",
                                              label = "自:", 
                                              choices = pg_cw_y,
                                              selected = min(pg_cw_y) ),
                                  selectInput(inputId="mileage_year_end",
                                              label="至:",
                                              choice=pg_cw_y,
                                              selected=max(pg_cw_y) ),
                                  textInput(inputId="nlm_input",
                                            label=strong("新线铺轨里程（公里）"),
                                            value=round(mean(pg_cw_df$nlm),2)),
                                  textInput(inputId="olm_input",
                                            label=strong("复线铺轨里程（公里）"),
                                            value=round(mean(pg_cw_df$olm),2)),
                                  hr("预测结果——固定资产值（亿元）"),
                                  hr(),
                                  textOutput("pg_asset_output") ,
                                  hr(),
                                  textOutput("pg_asset_FRR"),
                                  hr(),
                                  textOutput("pg_asset_zhi")
                                  
                                ),                                                    
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回归", plotOutput("pg_asset_linearplot")), 
                                    tabPanel("随机森林回归", plotOutput("pg_asset_rfplot")), 
                                    tabPanel("支持向量机回归", plotOutput("pg_asset_svmplot"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("pg_asset_table")   )
                                )
                              )
                              ),
                     
                     tabPanel("固定资产-动车组",
                              titlePanel("固定资产投资-动车组"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="emu_stat_data",
                                                label=strong("历史统计值"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "emu_predict_data",
                                                label = strong("回归预测值"),
                                                value = TRUE),
                                  selectInput(inputId = "emu_year_start",
                                              label = "自:", 
                                              choices = cw_y,
                                              selected = min(cw_y) ),
                                  selectInput(inputId="emu_year_end",
                                              label="至:",
                                              choice=cw_y,
                                              selected=max(cw_y) ),
                                  textInput(inputId="emu_input",
                                            label=strong("动车新增数量"),
                                            value=round(mean(cw_df$emu),0)),
                                  hr("预测结果——固定资产值（亿元）"),
                                  hr(),
                                  textOutput("emu_asset_output") ,
                                  hr(),
                                  textOutput("emu_asset_FRR"),
                                  hr(),
                                  textOutput("emu_asset_zhi")
                                  
                                  # actionButton("predictFre","预测新货运量") 
                                ),                                                       #sidebarPanel
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回归", plotOutput("emu_asset_linearplot")), 
                                    tabPanel("随机森林回归", plotOutput("emu_asset_rfplot")), 
                                    tabPanel("支持向量机回归", plotOutput("emu_asset_svmplot"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("emu_asset_table")   )
                                )
                              )
                              ),
                     
#——————————————————————————————————————————————————————————————————————————
#————————————固定资产-客车数量适配性分析————————————
tabPanel("固定资产-客车数量",
titlePanel("固定资产-客车数量"),
sidebarLayout(
  sidebarPanel(
    checkboxInput(inputId="investment_stat_data",
                  label=strong("历史统计值"),
                  value=TRUE),
    
    checkboxInput(inputId = "investment_predict_data",
                  label = strong("回归预测值"),
                  value = TRUE),
    selectInput(inputId = "investment_year_start",
                label = "自:", 
                choices = investment_y,
                selected = min(investment_y) ),
    selectInput(inputId="investment_year_end",
                label="至:",
                choice=investment_y,
                selected=max(investment_y) ),
    textInput(inputId="ptrain_input",
              label=strong("预测输入值——客车增加数量（辆）"),
              value=mean(investment_df$ptrain)),
    hr("预测结果——固定资产投资额（万元）"),
    hr(),
    textOutput("investment_output") ,
    hr(),
    textOutput("investment_FRR"),
    hr(),
    textOutput("investment_zhi")
    # actionButton("predictCAR","预测新客车量") 
    
  ), 
  
  mainPanel(
    tabsetPanel(
      tabPanel("多元线性回归", plotOutput("investmentlinearplot")), 
      tabPanel("随机森林回归", plotOutput("investmentrfplot")), 
      tabPanel("支持向量机回归", plotOutput("investmentsvmplot"))
    ),
    
    fluidRow(  DT::dataTableOutput("investmenttable")   )
  )
)
),
tabPanel("固定资产-货车车辆",
         titlePanel("固定资产-货车车辆"),
         
         sidebarLayout(
           sidebarPanel(
             checkboxInput(inputId="cw_truck_stat_data",
                           label=strong("历史统计值"),
                           value=TRUE),
             
             checkboxInput(inputId = "cw_truck_predict_data",
                           label = strong("回归预测值"),
                           value = TRUE),
             selectInput(inputId = "cw_truck_year_start",
                         label = "自:", 
                         choices = cw_truck_y,
                         selected = min(cw_truck_y) ),
             selectInput(inputId="cw_truck_year_end",
                         label="至:",
                         choice=cw_truck_y,
                         selected=max(cw_truck_y) ),
             textInput(inputId="cw_truck_input",
                       label=strong("货车车辆"),
                       value=round(mean(cw_truck_df$cw_truck),2)),
             hr("预测结果—固定资产值（亿元）"),
             hr(),
             textOutput("cw_truck_asset_output") ,
             hr(),
             textOutput("cw_truck_asset_FRR"),
             hr(),
             textOutput("cw_truck_asset_zhi")
             
             # actionButton("predictFre","预测新货运量") 
           ),                                                       #sidebarPanel
           
           mainPanel(
             tabsetPanel(
               tabPanel("多元线性回归", plotOutput("cw_truck_linearplot")), 
               tabPanel("随机森林回归", plotOutput("cw_truck_rfplot")), 
               tabPanel("支持向量机回归", plotOutput("cw_truck_svmplot"))
             ),
             
             fluidRow(  DT::dataTableOutput("cw_truck_table")   )
           )
         )
),
tabPanel("固定资产-机车台数",
         titlePanel("固定资产-机车台数"),
         
         sidebarLayout(
           sidebarPanel(
             checkboxInput(inputId="JCNum_stat_data",
                           label=strong("历史统计值"),
                           value=TRUE),
             
             checkboxInput(inputId = "JCNum_predict_data",
                           label = strong("回归预测值"),
                           value = TRUE),
             selectInput(inputId = "JCNum_year_start",
                         label = "自:", 
                         choices = JCNum_y,
                         selected = min(JCNum_y) ),
             selectInput(inputId="JCNum_year_end",
                         label="至:",
                         choice=JCNum_y,
                         selected=max(JCNum_y) ),
             textInput(inputId="JCNum_input",
                       label=strong("货车车辆"),
                       value=round(mean(JCNum_df$JCNum),2)),
             hr("预测结果—固定资产值（亿元）"),
             hr(),
             textOutput("JCNum_GDMoney_output") ,
             hr(),
             textOutput("JCNum_GDMoney_FRR"),
             hr(),
             textOutput("JCNum_GDMoney_zhi")
             
             # actionButton("predictFre","预测新货运量") 
           ),                                                       #sidebarPanel
           
           mainPanel(
             tabsetPanel(
               tabPanel("多元线性回归", plotOutput("JCNum_linearplot")), 
               tabPanel("随机森林回归", plotOutput("JCNum_rfplot")), 
               tabPanel("支持向量机回归", plotOutput("JCNum_svmplot"))
             ),
             
             fluidRow(  DT::dataTableOutput("JCNum_table")   )
           )
         )
),


                     tabPanel("客运量-客车车辆数",
                              titlePanel("客运量-客车车辆数"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="mileage_stat_data",
                                                label=strong("历史统计值"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "mileage_predict_data",
                                                label = strong("回归预测值"),
                                                value = TRUE),
                                  selectInput(inputId = "mileage_year_start",
                                              label = "自:", 
                                              choices = PVy,
                                              selected = min(PVy) ),
                                  selectInput(inputId="mileage_year_end",
                                              label="至:",
                                              choice=PVy,
                                              selected=max(PVy) ),
                                  textInput(inputId="CarriageNum_input",
                                            label=strong("客车车辆数（辆）"),
                                            value=round(mean(PVdf$CarriageNum),0)),
                                  textInput(inputId="CarKm_input",
                                            label=strong("客车机车日车公里（公里）"),
                                            value=round(mean(PVdf$CarKm),2)),
                                  hr("预测结果——客运量（万人）"),
                                  hr(),
                                  textOutput("PassengeVolume_output") ,
                                  hr(),
                                  textOutput("PassengeVolume_FRR"),
                                  hr(),
                                  textOutput("PassengeVolume_zhi")
                                  
                                  
                                ),                                                     
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回归", plotOutput("car_passenger_linearplot")), 
                                    tabPanel("随机森林回归", plotOutput("car_passenger_rfplot")), 
                                    tabPanel("支持向量机回归", plotOutput("car_passenger_svmplot"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("car_passenger_table")   )
                                )
                              )
                              ),
                     tabPanel("机车台数-营业里程",
                 
                              titlePanel("机车台数-营业里程"),
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="stat_data_1",   #时间选择框
                                                label=strong("历史统计值"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "predict_data_1", #计算的回归预测值
                                                label = strong("回归预测值"),
                                                value = TRUE),
                                  selectInput(inputId = "year_start_1",
                                              label = "自:", 
                                              choices = locomotive_dis_y,
                                              selected = min(locomotive_dis_y) ),
                                  selectInput(inputId="year_end_1",
                                              label="至:",
                                              choice=locomotive_dis_y,
                                              selected=max(locomotive_dis_y) ),
                                  textInput(inputId="km_input_1",#输入营业里程
                                            label=strong("预测输入值——营业里程（公里）"),
                                            value=mean(locomotive_dis$distance)),
                                  hr("预测结果—机车台数（台）"),
                                  hr(),
                                  textOutput("locomotive_output_1") ,#多元回归预测值
                                  hr(),
                                  textOutput("locomotive_FRR_1"),# 随机森林预测值
                                  hr(),
                                  textOutput("locomotive_zhi_1")#支向量回归预测值
                                  # actionButton("predictCAR","预测新客车量") 
                                  
                                ), 
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回归", plotOutput("linearplot_1")), #多元线性回归图
                                    tabPanel("随机森林回归", plotOutput("rfplot_1")), #随机森林回归图
                                    tabPanel("支持向量机回归", plotOutput("svmplot_1"))#支持向量机回归图
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("table_1")   )#将预测值放入table_1
                                )
                              ) 
                              
                             ),

                     tabPanel("机车台数-客运量",
                              
                              titlePanel("机车台数-客运量"),
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="locomotivePV_stat_data",   #时间选择框
                                                label=strong("历史统计值"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "locomotivePV_predict_data", #计算的回归预测值
                                                label = strong("回归预测值"),
                                                value = TRUE),
                                  selectInput(inputId = "locomotivePV_year_start",
                                              label = "自:", 
                                              choices = locomotive_PV_y,
                                              selected = min(locomotive_PV_y) ),
                                  selectInput(inputId="locomotivePV_year_end",
                                              label="至:",
                                              choice=locomotive_PV_y,
                                              selected=max(locomotive_PV_y) ),
                                  textInput(inputId="locomotivePV_PV_input",#输入客运量
                                            label=strong("预测输入值——客运量(亿人)"),
                                            value=mean(locomotive_PV$PV)),
                                  hr("预测结果—机车台数（台）"),
                                  hr(),
                                  textOutput("locomotivePV_locomotive_output") ,#多元回归预测值
                                  hr(),
                                  textOutput("locomotivePV_locomotive_FRR"),# 随机森林预测值
                                  hr(),
                                  textOutput("locomotivePV_locomotive_zhi")#支向量回归预测值
                                   
                                  
                                ), 
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回归", plotOutput("locomotivePV_linearplot")), #多元线性回归图
                                    tabPanel("随机森林回归", plotOutput("locomotivePV_rfplot")), #随机森林回归图
                                    tabPanel("支持向量机回归", plotOutput("locomotivePV_svmplot"))#支持向量机回归图
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("locomotivePV_table")   )#将预测值放入locomotivePV_table_1
                                )
                              )
                     ),

                     #----------------------------
                     #显示机车车辆-货运量适配性分析  
                     tabPanel("机车数量-货运量",
                     titlePanel("机车数量-货运量"),
                     sidebarLayout(
                       sidebarPanel(
                         checkboxInput(inputId="Locomotive_stat_data1",
                                       label=strong("历史统计值"),
                                       value=TRUE),
                         
                         checkboxInput(inputId = "Locomotive_predict_data1",
                                       label = strong("回归预测值"),
                                       value = TRUE),
                         selectInput(inputId = "Locomotive_year_start1",
                                     label = "自:", 
                                     choices = Locomotive_tm,
                                     selected = min(Locomotive_tm) ),
                         selectInput(inputId="Locomotive_year_end1",
                                     label="至:",
                                     choice=Locomotive_tm,
                                     selected=max(Locomotive_tm) ),
                         textInput(inputId="ton_input",
                                   label=strong("预测输入值——货运量（万吨）"),
                                   value=mean(Locomotive_fre$freight)),
                         hr("预测结果——机车数量（辆）"),
                         hr(),
                         textOutput("locomotive_output1") ,
                         hr(),
                         textOutput("locomotive_FRR1"),
                         hr(),
                         textOutput("locomotive_zhi1")
                         # actionButton("predictCAR","预测新客车量") 
                         
                       ), 
                       
                       mainPanel(
                         tabsetPanel(
                           tabPanel("多元线性回归", plotOutput("freightlinearplot")), 
                           tabPanel("随机森林回归", plotOutput("freightrfplot")), 
                           tabPanel("支持向量机回归", plotOutput("freightsvmplot"))
                         ),
                         
                         fluidRow(  DT::dataTableOutput("freighttable")   )
                       )
                     )
                   ),
                     
                     
                     

    #----------------------------
    #显示货车车辆-营业里程适配性分析
                     tabPanel("货车车辆-营业里程",
                       titlePanel("货车车辆-营业里程"),
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="stat_data_21",
                                                label=strong("历史统计值"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "predict_data_21",
                                                label = strong("回归预测值"),
                                                value = TRUE),
                                  selectInput(inputId = "year_start_21",
                                              label = "自:", 
                                              choices = freightcar_dis_y,
                                              selected = min(freightcar_dis_y) ),
                                  selectInput(inputId="year_end_21",
                                              label="至:",
                                              choice=freightcar_dis_y,
                                              selected=max(freightcar_dis_y) ),
                                  textInput(inputId="km_input_21",
                                            label=strong("预测输入值——营业里程（公里）"),
                                            value=round(mean(freightcar_dis$distance),2)),
                                  hr("预测结果——货车辆数（辆）"),
                                  hr(),
                                  textOutput("truck_output_21") ,
                                  hr(),
                                  textOutput("truck_FRR_21"),
                                  hr(),
                                  textOutput("truck_zhi_21")
                                  
                                  
                                ), 
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回归", plotOutput("linearplot_21")), 
                                    tabPanel("随机森林回归", plotOutput("rfplot_21")), 
                                    tabPanel("支持向量机回归", plotOutput("svmplot_21"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("table_21")   )
                                )
                              )
                              
                              ),
    tabPanel("客车车辆-营业里程",
             titlePanel("客车车辆-营业里程"),
             sidebarLayout(
               sidebarPanel(
                 checkboxInput(inputId="stat_data_ky",     #ky表示客运量和营业里程
                               label=strong("历史统计值"),
                               value=TRUE),
                 
                 checkboxInput(inputId = "predict_data_ky",
                               label = strong("回归预测值"),
                               value = TRUE),
                 selectInput(inputId = "year_start_ky",
                             label = "自:", 
                             choices = Carriagey,
                             selected = min(Carriagey) ),
                 selectInput(inputId="year_end_ky",
                             label="至:",
                             choice=Carriagey,
                             selected=max(Carriagey) ),
                 textInput(inputId="km_input_ky",
                           label=strong("预测输入值——营业里程（公里）"),
                           value=round(mean(Carriagedf$distance),2)),
                 hr("预测结果—客车辆数（辆）"),
                 hr(),
                 textOutput("ky_carriage_output") ,
                 hr(),
                 textOutput("ky_carriage_FRR"),
                 hr(),
                 textOutput("ky_carriage_zhi")
                 
               ), 
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("多元线性回归", plotOutput("ky_linearplot")), 
                   tabPanel("随机森林回归", plotOutput("ky_rfplot")), 
                   tabPanel("支持向量机回归", plotOutput("ky_svmplot"))
                 ),
                 
                 fluidRow(  DT::dataTableOutput("ky_table")   )
               )
             )
             ),
    
    tabPanel("货运量-营业里程",
             titlePanel("货运量-营业里程"),
             
             sidebarLayout(
               sidebarPanel(
                 checkboxInput(inputId="freight_mileage_stat_data",
                               label=strong("历史统计值"),
                               value=TRUE),
                 
                 checkboxInput(inputId = "freight_mileage_predict_data",
                               label = strong("回归预测值"),
                               value = TRUE),
                 selectInput(inputId = "freight_mileage_year_start",
                             label = "自:", 
                             choices = freight_car_y,
                             selected = min(freight_car_y) ),
                 selectInput(inputId="freight_mileage_year_end",
                             label="至:",
                             choice=freight_car_y,
                             selected=max(freight_car_y) ),
                 textInput(inputId="freightcar_input",
                           label=strong("货车车辆数"),
                           value=round(mean(freight_car_df$freightcar),0)),
                 textInput(inputId="freightolm_input",
                           label=strong("营业里程"),
                           value=round(mean(freight_car_df$olm),0)),
                 hr("预测结果——货运量"),
                 hr(),
                 textOutput("f_car_output") ,
                 hr(),
                 textOutput("f_car_FRR"),
                 hr(),
                 textOutput("f_car_zhi")
                 
               ),                                                    
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("多元线性回归", plotOutput("f_car_linearplot")), 
                   tabPanel("随机森林回归", plotOutput("f_car_rfplot")), 
                   tabPanel("支持向量机回归", plotOutput("f_car_svmplot"))
                 ),
                 
                 fluidRow(  DT::dataTableOutput("f_car_table")   )
               )
             )
             )
                     
                     ),


#----------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#铁路货运量预测

    tabPanel("货运量预测",
             titlePanel("铁路货运量预测"),
             hr(),
             
             sidebarLayout(
               sidebarPanel(
                 checkboxInput(inputId="stat_data",
                               label=strong("历史统计值"),
                               value=TRUE),
                 
                 checkboxInput(inputId = "predict_data",
                               label = strong("回归预测值"),
                               value = TRUE),
                 selectInput(inputId = "year_start",
                             label = "自:", 
                             choices = y,
                             selected = min(y) ),
                 selectInput(inputId="year_end",
                             label="至:",
                             choice=y,
                             selected=max(y) ),
                 numericInput(inputId="iron_input",
                              label=strong("预测输入值--成品钢材产量(万吨)"),
                              value=9822                                                      
                 ),
                 numericInput(inputId="coal_input",
                              label=strong("预测输入值--原煤产量输入值（万吨）"),
                              value=33000),
                 hr("预测结果——货运量（万吨）"),
                 hr(),
                 textOutput("freight_output") ,
                 hr(),
                 textOutput("freight_FRR"),
                 hr(),
                 textOutput("freight_zhi")
              
                 
               ), 
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("多元线性回归", plotOutput("linearplot")), 
                   tabPanel("随机森林回归", plotOutput("rfplot")), 
                   tabPanel("支持向量机回归", plotOutput("svmplot"))
                 ),
                 
                 fluidRow(  DT::dataTableOutput("table")   )
               )
             )
    ),

#----------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#铁路客运量预测

tabPanel("客运量预测",
         titlePanel("铁路客运量预测"),
         hr(),
         
         sidebarLayout(
           sidebarPanel(
             checkboxInput(inputId="passagerpre_stat_data",
                           label=strong("历史统计值"),
                           value=TRUE),
             
             checkboxInput(inputId = "passagerpre_predict_data",
                           label = strong("回归预测值"),
                           value = TRUE),
             selectInput(inputId = "passagerpre_year_start",
                         label = "自:", 
                         choices = passagerpre_y,
                         selected = min(passagerpre_y) ),
             selectInput(inputId="passagerpre_year_end",
                         label="至:",
                         choice=passagerpre_y,
                         selected=max(passagerpre_y) ),
             
             numericInput(inputId="passagerpre_GDP_input",
                          label=strong("预测输入值--国内生产总值(亿元)"),
                          value=636138.7),
             numericInput(inputId="passagerpre_population_input",
                          label=strong("预测输入值--年末总人口(万人)"),
                          value=136782),
             numericInput(inputId="passsagerpre_income_input",
                          label=strong("预测输入值--城镇居民家庭人均可支配收入(元)"),
                          value=28843.85),                                                      
             numericInput(inputId="passagerpre_thirdindustry_input",
                          label=strong("预测输入值--第三产业增加值（%）"),
                          value=48.1),
             numericInput(inputId="passagerpre_aviation_input",
                          label=strong("预测输入值--民用航空客运量(万人)"),
                          value=39165.8),
             numericInput(inputId="passagerpre_EMU_input",
                          label=strong("预测输入值--动车组数量（组）"),
                          value=1445),
             numericInput(inputId="passagepre_railcar_input",
                          label=strong("预测输入值--客车辆数（辆）"),
                          value=60795),
             
             hr("预测结果——客运量（万人）"),
             hr(),
             textOutput("passagerpre_output") ,
             hr(),
             textOutput("passagerpre_FRR"),
             hr(),
             textOutput("passagerpre_zhi")
             
             
           ), 
           
           mainPanel(
             tabsetPanel(
               tabPanel("多元线性回归", plotOutput("passagerpre_linearplot")), 
               tabPanel("随机森林回归", plotOutput("passagerpre_rfplot")), 
               tabPanel("支持向量机回归", plotOutput("passagerpre_svmplot"))
             ),
             
             fluidRow(  DT::dataTableOutput("passagerpre_table")   )
           )
         )
),

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
#时间序列预测界面

    navbarMenu("时间序列预测",
               tabPanel("货运量",
                        titlePanel("货运量时间序列预测"),
                        
                        fluidRow(
                          plotOutput(outputId = "freight_forecast", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("freight_forecast_table"))
                        ) 
                        ),
               tabPanel("工业增加值增长量",
                        titlePanel("工业增加值增长量时间序列预测"),
                        
                        fluidRow(
                          plotOutput(outputId = "gyzjz_forecast_timesery", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("gyzjz_forecast_table_timesery"))
                        )
                        ),
               tabPanel("铁路固定资产",
                        titlePanel("铁路固定资产时间序列预测"),
                        
                        fluidRow(
                          plotOutput(outputId = "gdzctz_forecast_timesery", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("gdzctz_forecast_table_timesery"))
                        )
                        ),
               tabPanel("货车车辆数",
                        titlePanel("货车辆数时间序列预测"),
                        
                        fluidRow(
                          plotOutput(outputId = "TruckTime_forecast", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("TruckTime_forecast_table"))
                        )
                        ),
               tabPanel("原煤产量",
                        titlePanel("原煤产量时间序列预测"),
                        
                        fluidRow(
                          plotOutput(outputId = "CoalTime_forecast", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("CoalTime_forecast_table"))
                        ) 
                        ),
               tabPanel("成品钢材产量",
                        titlePanel("成品钢材量时间序列预测"),
                        fluidRow(
                          plotOutput(outputId = "SteelTime_forecast", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("SteelTime_forecast_table"))
                        ) 
                        ),
               tabPanel("原油加工量",
                        titlePanel("原油加工量时间序列预测"),
                        
                        fluidRow(
                          plotOutput(outputId = "OilTime_forecast", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("OilTime_forecast_table"))
                        )
                        )
               
               
    ),




#-------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#原始数据界面

tabPanel("原始数据",
         titlePanel("铁路景气指数原始数据"),
         
         
         fluidRow(
           column(12, tabsetPanel(type="tabs",
                                  
                                  #-------------------页签：相关行业数据
                                  
                                  tabPanel( "相关行业数据", 
                                            fluidRow(
                                              
                                              sidebarLayout(
                                                sidebarPanel(
                                                  
                                                  radioButtons(inputId="xghysj.yssj", #xghysj.yssj 原始数据显示中的相关行业数据页签的单选框，以下是5个类别的变量代码
                                                               label=NULL,
                                                               choices = c("成品钢材产量(亿吨)"="cpgccl.yssj",
                                                                           "原油加工量(亿吨)"="yyjgl.yssj",
                                                                           "原煤产量(亿吨)"="ymcl.yssj",
                                                                           "火力发电量(亿千瓦时)"="hlfdl.yssj",
                                                                           "工业增加值(%)"="gyzjz.yssj") ),
                                                  hr(),
                                                  selectInput(inputId = "year_start_xghy", #year_start_xghy 相关行业数据中的起始年下拉框，以下终止年雷同
                                                              label = "自:", 
                                                              choices = y.wenjing.yssj,
                                                              selected = min(y.wenjing.yssj) ),
                                                  selectInput(inputId="year_end_xghy",
                                                              label="至:",
                                                              choice=y.wenjing.yssj,
                                                              selected=max(y.wenjing.yssj) ),
                                                  width=3
                                                ),     #siderbarpanel
                                                mainPanel(plotOutput(outputId = "yssj.xghy.plot", height = "400px"),width=9)#yssj.xghy.plot原始数据中相关行业的画图
                                              )  #mainpanel
                                            ),
                                            
                                            fluidRow(
                                              column(12,DT::dataTableOutput("yssj.xghy.table"))#yssj.xghy.table原始数据中相关行业的数据表输出
                                            )
                                  ), #第一个页签
                                  
                                  
                                  #-------------------页签：运量相关 
                                  
                                  tabPanel("运量相关", 
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="ylxg.yssj",#ylxg.yssj 原始数据显示中的运量相关数据页签的单选框，以下是4个类别的变量代码
                                                              label=NULL,
                                                              choices = c("货运量(亿吨)"="hyl.yssj",
                                                                          "货运周转量(亿吨)"="hyzzl.yssj",
                                                                          "客运量(亿人)"="kyl.yssj",
                                                                          "客运周转量(亿人)"="kyzzl.yssj") ),
                                                 hr(),
                                                 selectInput(inputId = "year_start_ylxg",#year_start_xghy 运量相关数据中的起始年下拉框，以下终止年雷同
                                                             label = "自:", 
                                                             choices = y.wenjing.yssj,
                                                             selected = min(y.wenjing.yssj) ),
                                                 selectInput(inputId="year_end_ylxg",
                                                             label="至:",
                                                             choice=y.wenjing.yssj,
                                                             selected=max(y.wenjing.yssj) ),
                                                 width=3
                                               ),
                                               
                                               mainPanel(plotOutput(outputId = "yssj.ylxg.plot", height = "380px"),width=9)#yssj.ylxg.plot原始数据中运量相关的画图
                                             )),
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.ylxg.table")) #yssj.ylxg.table原始数据中运量相关的数据表输出
                                           )
                                  ), #第二个页签
                                  
                                  #-------------------页签：运营相关 
                                  
                                  tabPanel("运营相关", 
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="yyxg.yssj",#与上雷同，yyxg:运营相关原始数据
                                                              label=NULL,
                                                              choices = c("营业里程(km)"="yylc.yssj",
                                                                          "日均运用车(万辆)"="rjyyc.yssj",
                                                                          "日均现在车(万辆)"="rjxzc.yssj",
                                                                          "客运机车日车公里(km)"="kyjcrcgl.yssj",
                                                                          "货运机车日车公里(km)"="hyjcrcgl.yssj",
                                                                          "机车总行走里程(1000km)"="jczxzlc.yssj") ),
                                                 hr(),     
                                                 selectInput(inputId = "year_start_yyxg",
                                                             label = "自:", 
                                                             choices = y.wenjing.yssj,
                                                             selected = min(y.wenjing.yssj) ),
                                                 selectInput(inputId="year_end_yyxg",
                                                             label="至:",
                                                             choice=y.wenjing.yssj,
                                                             selected=max(y.wenjing.yssj) ),
                                                 width=3
                                               ),
                                               mainPanel(plotOutput(outputId = "yssj.yyxg.plot", height = "440px"),width=9 ))
                                           ),
                                           
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.yyxg.table"))
                                           )
                                  ), #第三个页签
                                  
                                  
                                  #-------------------页签：规模相关---------------------------------------------    
                                  tabPanel("规模相关",           #第四个页签
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="zcxg.yssj",#与上雷同，zcxg:资产相关原始数据
                                                              label=NULL,
                                                              choices = c("客车辆数(辆)"="kcls.yssj",
                                                                          "货车辆数(万辆)"="hcls.yssj",
                                                                          "机车台数(辆)"="jcts.yssj",
                                                                          "动车台数(台)"="dcts.yssj",
                                                                          "铁路固定资产投资(亿元)"="tlgdzctz.yssj",
                                                                          "从业人员数量(万人)"="cyrysl.yssj",
                                                                          "新线铺轨里程(km)"="xxpglc.yssj",
                                                                          "复线铺轨里程(km)"="fxpglc.yssj") ),
                                                 
                                                 hr(),   
                                                 selectInput(inputId = "year_start_zcxg",
                                                             label = "自:", 
                                                             choices = y.wenjing.yssj,
                                                             selected = min(y.wenjing.yssj) ),
                                                 selectInput(inputId="year_end_zcxg",
                                                             label="至:",
                                                             choice=y.wenjing.yssj,
                                                             selected=max(y.wenjing.yssj) ),
                                                 width=3
                                               ),
                                               mainPanel(plotOutput(outputId = "yssj.zcxg.plot", height = "400px"),width=9)
                                             )),
                                           
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.zcxg.table"))
                                           )
                                  ), #第四个页签
                                  
                                  
                                  #-------------------页签：黑货运量---------------------------------------------    
                                  tabPanel("黑货运量",           #第五个页签
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="heihuo.yssj",#与上雷同，heihuo:黑货种类的铁路货运量原始数据
                                                              label=NULL,
                                                              choices = c("矿建(万吨)"="kuangjian.yssj",
                                                                          "钢材(万吨)"="gangcai.yssj",
                                                                          "石油(万吨)"="shiyou.yssj",
                                                                          "煤(万吨)"="mei.yssj",
                                                                          "金属矿石(万吨)"="jsks.yssj") ),
                                                 
                                                 hr(),   
                                                 selectInput(inputId = "year_start_heihuo.yssj",
                                                             label = "自:", 
                                                             choices = y.wenjing.yssj.hhbh,
                                                             selected = min(y.wenjing.yssj.hhbh) ),
                                                 selectInput(inputId="year_end_heihuo.yssj",
                                                             label="至:",
                                                             choice=y.wenjing.yssj.hhbh,
                                                             selected=max(y.wenjing.yssj.hhbh) ),
                                                 width=3
                                               ),
                                               mainPanel(plotOutput(outputId = "yssj.heihuo.plot", height = "400px"),width=9)
                                             )),
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.heihuo.table"))
                                           )
                                  ), #第五个页签
                                  
                                  
                                  #-------------------页签：白货运量---------------------------------------------    
                                  tabPanel("白货运量",           #第六个页签
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="baihuo.yssj",#与上雷同，baihuo:白货种类的铁路货运量原始数据
                                                              label=NULL,
                                                              choices = c("工业机械(万吨)"="gyjx.yssj",
                                                                          "电子电气(万吨)"="dzdq.yssj",
                                                                          "农副产品(万吨)"="nfcp.yssj",
                                                                          "饮食烟草(万吨)"="ysyc.yssj",
                                                                          "文教用品(万吨)"="wjyp.yssj", 
                                                                          "零担(吨)"="ldld.yssj" ,
                                                                          "集装箱(万吨)"="jzx.yssj" )),
                                                 hr(),   
                                                 selectInput(inputId = "year_start_baihuo.yssj",
                                                             label = "自:", 
                                                             choices = y.wenjing.yssj.hhbh,
                                                             selected = min(y.wenjing.yssj.hhbh) ),
                                                 selectInput(inputId="year_end_baihuo.yssj",
                                                             label="至:",
                                                             choice=y.wenjing.yssj.hhbh,
                                                             selected=max(y.wenjing.yssj.hhbh) ),
                                                 width=3
                                               ),
                                               mainPanel(plotOutput(outputId = "yssj.baihuo.plot", height = "400px"),width=9)
                                             )),
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.baihuo.table"))
                                           )
                                  )
                                  
                                  
                                  
                                  
           ) 
           ))
         )
  )
 )




  




