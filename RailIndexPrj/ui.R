require(shiny)
require(ggplot2)
require(DT)
require(markdown)
require(rJava)
require(xlsx)


#-----------------------------------界面所需数据集------------------------------
#-----------------------------------------
#---------原始数据------------------------
df_monthly<-read.xlsx("rawdata_monthly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
dfrawdata<-df_monthly
dfrawdata$tm<-as.Date.POSIXct(dfrawdata$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  
y_wenjing_rawdata_monthly<-unique(substr(dfrawdata$tm,1,4))

y_wenjing_rawdata_black_white<-y_wenjing_rawdata_monthly

df_yearly<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_wenjing_rawdata_yearly<-substr(df_yearly$tm,1,4)

#-----------------------------------------
#-------铁路景气指数----------------------
dftrans<-read.xlsx("trans_index_x12.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
dftrans$tm<-as.Date.POSIXct(dftrans$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
y_wenjing<-c(2000:max(y_wenjing_rawdata_yearly))
y_wangyang<-c(2002:max(y_wenjing_rawdata_yearly))
y_yiheng<-y_wenjing

#-----------------------------------------
#-------黑货白货指数----------------------

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
wb_index$tm<-as.Date.POSIXct(wb$tm[85:wb_len],"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
index_tm<-unique(substr(wb_index$tm,1,4))

#-----------------------------------------
#-------适配性研究----------------------

#-------------------------------------------------------------------------------------------------
#---------------机车数量适配性研究----------------------------------------------------------------

Locomotive_fre<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
Locomotive_fre$tm<-as.Date.POSIXct(Locomotive_fre$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))
Locomotive_tm<-unique(substr(Locomotive_fre$tm,1,4))



#-------------------------------------------------------------------------------------------------
#---------------固定资产适配性研究----------------------------------------------------------------

investment_fre<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
a<-length(investment_fre$passenger_car_delta)
tm_delta<-investment_fre$tm[11:a]
fixed_assets_investment_delta<-investment_fre$fixed_assets_investment_delta[11:a]
passenger_car_delta<-investment_fre$passenger_car_delta[11:a]
bullettrain_number_delta<-investment_fre$bullettrain_number_delta[11:a]
investment_data<-data.frame(tm_delta,fixed_assets_investment_delta,passenger_car_delta,bullettrain_number_delta)
investment_data$tm_delta<-as.Date.POSIXct(investment_data$tm_delta,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))
investment_y<-unique(substr(investment_data$tm_delta,1,4))
#---------------------------------------------------------------------------------------------------------
#-----------------------营业里程适配性研究-----------------------------------------------------------------
distance_fre<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
distance_fre$tm<-as.Date.POSIXct(distance_fre$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))
distance_tm<-unique(substr(distance_fre$tm,1,4))

#---------------------------------------------------------------------------------------------------------



PVdf<-read.xlsx("rawdata_yearly.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
PVdf$tm<-as.Date.POSIXct(PVdf$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))
PVy<-unique(substr(PVdf$tm,1,4))

freight_car_df<-read.csv("货运量-营业里程.csv",head=T)  
freight_car_df$tm<-as.Date.POSIXct(freight_car_df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
year_slection<-unique(substr(freight_car_df$tm,1,4))

year_slection<-unique(substr(dfrawdata[1:180,]$tm,1,4))                     #2001-2014时间选取数据
year_slection_passenger<-unique(substr(dfrawdata[49:180,]$tm,1,4))          #2005-2014时间选取数据
#-------------------------------其它铁路原始数据----------------------------------
#--------马少蒙--------------------------
mengmeng_yearly<-read.xlsx("3-1 全国铁路线路、铁路复线、电气化、内燃牵引里程.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
railway_mileage_yearly<-substr(mengmeng_yearly$tm,1,4)

mengmeng1_yearly<-read.xlsx("3-9 全国铁路机车拥有量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
Locomotive_ownership_yearly<-substr(mengmeng1_yearly$tm,1,4)

mengmeng2_yearly<-read.xlsx("3-2 全国铁路分地区营业里程.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
sub_regional_mileage_yearly<-mengmeng2_yearly$tm

mengmeng3_yearly<-read.xlsx("3-10 国家铁路分机型机车拥有量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
model_locomotive_ownership_yearly<-substr(mengmeng3_yearly$tm,1,4)
##-------聪聪3.13，全国客车货车拥有量情况
fcc_holding<-read.xlsx("3-13 全国铁路客、货车拥有量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_fcc_holding_tm<-substr(fcc_holding$tm,1,4)
fcc_availability<-read.xlsx("3-12 国家铁路机、客、货车利用率.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_fcc_availability_tm<-substr(fcc_availability$tm,1,4)
fcc_kecheholding<-read.xlsx("3-14 国家铁路客车拥有量（分座次）.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_fcc_kecheholding_tm<-substr(fcc_kecheholding$tm,1,4)
fcc_huocheholding<-read.xlsx("3-15 国家铁路分车型货车拥有量（分车辆种类）.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_fcc_huocheholding_tm<-substr(fcc_huocheholding$tm,1,4)
fcc_sheshishebei<-read.xlsx("3-22 国家铁路车站主要客货运设施、设备.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_fcc_sheshishebei_tm<-substr(fcc_sheshishebei$tm,1,4)
#------景钊---------
df_yslzzl_yearly<-read.xlsx("4-1 全国铁路运营量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_jingzhao_rawdata_yearly<-substr(df_yslzzl_yearly$tm,1,4)
#------李雪妍-------
lxy1_yearly<-read.xlsx("4_16国家铁路省、市、自治区货运量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_lixueyan_4_16_yearly<-lxy1_yearly$tm

lxy2_yearly<-read.xlsx("4_17国家铁路省、市、自治区货运周转量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_lixueyan_4_17_yearly<-lxy2_yearly$tm

lxy3_yearly<-read.xlsx("4_18国家铁路省、市、自治区客运量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_lixueyan_4_18_yearly<-lxy3_yearly$tm

lxy4_yearly<-read.xlsx("4_19国家铁路省、市、自治区客运周转量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_lixueyan_4_19_yearly<-lxy4_yearly$tm

lxy5_yearly<-read.xlsx("5_1国家铁路机车运用指标.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_lixueyan_5_1_yearly<-substr(lxy5_yearly$tm,1,4)
#-------李亚芳-----------
#-------铁路列车正点率原始数据---------------------
df_yearly_55<-read.xlsx("5-5 国家铁路列车正点率.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_liyafang_rawdata_55<-substr(df_yearly_55$tm,1,4)

#-------国家铁路机车能源消耗原始数据---------------
df_yearly_52<-read.xlsx("5-2 国家铁路机车能源消耗.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_liyafang_rawdata_52<-substr(df_yearly_52$tm,1,4)

#-------国家铁路机车工作量原始数据-----------------
df_yearly_53<-read.xlsx("5-3 国家铁路机车工作量.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_liyafang_rawdata_53<-substr(df_yearly_53$tm,1,4)

#-------国家铁路货车运用指标原始数据---------------
df_yearly_54<-read.xlsx("5-4 国家铁路货车运用指标.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_liyafang_rawdata_54<-substr(df_yearly_54$tm,1,4)

#-------国家铁路固定资产投资原始数据---------------
df_yearly_61<-read.xlsx("6-1 全国铁路固定资产投资.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_liyafang_rawdata_61<-substr(df_yearly_61$tm,1,4)

#-------国家铁路基本建设投资原始数据---------------
df_yearly_62<-read.xlsx("6-2 全国铁路基本建设投资.xlsx",1,head=T,startRow=2,encoding = "UTF-8")
y_liyafang_rawdata_62<-substr(df_yearly_62$tm,1,4)
#----------李亚芳---------
table6.3 <- read.xlsx("6-3 铁道部基本建设投资的资金来源.xls",1,header = T,head=T,startRow=2,encoding = "UTF-8")
choice6.3 <- na.omit(table6.3[,1])
table6.4 <- read.xlsx("6-4 全国铁路基本建设铺轨及投产里程.xls",1,header = T,startRow=2,encoding = "UTF-8")
choice6.4 <- na.omit(table6.4[,1])
table6.7 <- read.xlsx("6-7 国家铁路机车车辆购置.xls",1,header = T,startRow=2,encoding = "UTF-8")
choice6.7 <- na.omit(table6.7[,1])
#———————————————————————————————————————————————————————————————————————————————
#-----------------------------------------主界面--------------------------------

shinyUI(navbarPage(p(strong("铁路景气指数"),responsive=T,fluid=T),
                   
                   
                   #-------------------------------------------------------------------------------
                   #----------------------------------------------------------------------------------
                   #预警信号系统界面
                   
                   tabPanel("预警信号系统",
                            titlePanel("铁路预警信号灯(黑线代表货运，蓝线代表客运)"),
                            hr(), 
                            fluidRow(
                                column(2,checkboxInput(inputId="freight_index",
                                  label=("货运预警"),
                                  value=TRUE)),
                                column(2,checkboxInput(inputId="passenger_index",
                                   label=("客运预警"),
                                   value=TRUE)),
                                column(1,actionButton("updata_x12", "更新x12数据"))
                                ),
                            plotOutput(outputId = "plot_index", height = "400px"),
                            hr(),
                            wellPanel(
                              h4("铁路运输景气预警信号系统，是借助于相关计量经济分析方法，将多个指标进行数据处理，合并为一个综合性"),
                              h4("的指标，对这组指标和综合指标所代表的铁路运输波动状况发出预警信号，通过观察信号的变化情况，来判断"), 
                              h4("未来铁路运输增长的趋势。在本APP中，三种颜色信号的含义如下："),
                              #h4("蓝灯&浅蓝灯---运输市场景气偏冷"),
                              h4("绿灯---铁路运输发展稳定"),
                              h4("黄灯---铁路运输短期内有转热和加速的可能"),
                              h4("红灯---铁路运输市场较热")
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
                                                                                                         choices=y_wangyang,
                                                                                                         selected = min(y_wangyang) )),
                                                                                  column(3, selectInput(inputId="year_end_trans",#year_end_trans运输合成指数中的终止年下拉框
                                                                                                        label="至:",
                                                                                                        choices=y_wangyang,
                                                                                                        selected=max(y_wangyang)))
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
                                                                                          label=h5("货运量%"),
                                                                                          value="17.87"),
                                                                                textInput(inputId="scale_gyzjz_percent_input",
                                                                                          label=h5("工业增加值量%"),
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
                   #-----------------------------------------黑白货指数界面----------------------------------
                   
                   tabPanel("黑货白货指数",
                            titlePanel("黑货白货指数"),
                            fluidRow(
                              column(12, tabsetPanel(type="tabs",
                                                     
                                                     
                                                     #黑货指数数的页签-----------------------------------------------
                                                     
                                                     tabPanel( "黑货指数", 
                                                               fluidRow(
                                                                 sidebarLayout(
                                                                   sidebarPanel(
                                                                     selectInput(inputId = "balck_index_year_start",
                                                                                 label = "自:", 
                                                                                 choices = index_tm,
                                                                                 selected = min(index_tm),
                                                                                 width =('100%')),
                                                                     selectInput(inputId="balck_index_year_end",
                                                                                 label="至:",
                                                                                 choice=index_tm,
                                                                                 selected=max(index_tm),
                                                                                 width =('100%')),
                                                                     
                                                                     numericInput(inputId="weightcoal_input",
                                                                                  label=h6("原煤权重权重(%)"),
                                                                                  value='66.93',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightoil_input",
                                                                                  label=h6("石油权重(%)"),
                                                                                  value='5.22',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightmetal_input",
                                                                                  label=h6("金属矿石权重(%)"),
                                                                                  value='14.97',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightiron_input",
                                                                                  label=h6("钢铁权重(%)"),
                                                                                  value='8.02',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightmine_input",
                                                                                  label=h6("矿建权重(%)"),
                                                                                  value= "4.84",
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     width =3
                                                                   ),
                                                                   mainPanel(plotOutput(outputId="heihuo_index", height = "400px"),
                                                                             fluidRow( DT::dataTableOutput("heihuotable",width = "100%", height = "auto")),
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
                                                                     selectInput(inputId = "white_index_year2_start",
                                                                                 label = "自:", 
                                                                                 choices = index_tm,
                                                                                 selected = min(index_tm),
                                                                                 width =('100%')),
                                                                     selectInput(inputId="white_index_year2_end",
                                                                                 label="至:",
                                                                                 choice=index_tm,
                                                                                 selected=max(index_tm),
                                                                                 width =('100%')),
                                                                     numericInput(inputId="weightmachinery_input",
                                                                                  label=h6("工程机械权重(%)"),
                                                                                  value='18.10',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightelectronic_input",
                                                                                  label=h6("电子电器权重(%)"),
                                                                                  value='18.80',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightagricultural_input",
                                                                                  label=h6("农副产品权重(%)"),
                                                                                  value='11.10',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightfood_input",
                                                                                  label=h6("饮食烟草权重(%)"),
                                                                                  value='17.19',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weighteducation_input",
                                                                                  label=h6("科教用品权重(%)"),
                                                                                  value='17.77',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightltl_input",
                                                                                  label=h6("零担权重(%)"),
                                                                                  value='4.29',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     numericInput(inputId="weightcontainer_input",
                                                                                  label=h6("集装箱权重(%)"),
                                                                                  value='12.75',
                                                                                  min=0,
                                                                                  max=100,
                                                                                  step=0.1),
                                                                     width = 3 
                                                                   ),
                                                                   mainPanel(
                                                                     plotOutput(outputId="baihuo_index", height = "400px"),
                                                                     fluidRow(DT::dataTableOutput("baihuotable")),
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
                              tabPanel("基本建设投资-新增营业里程",
                                       titlePanel("基本建设投资-新增营业里程"),
                                       
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
                                                       choices = y_wenjing_rawdata_yearly,
                                                       selected = min(y_wenjing_rawdata_yearly) ),
                                           selectInput(inputId="mileage_year_end",
                                                       label="至:",
                                                       choice=y_wenjing_rawdata_yearly,
                                                       selected=max(y_wenjing_rawdata_yearly) ),
                                           textInput(inputId="mileage_input",
                                                     label=strong("新增营业里程"),
                                                     value=round(mean(df_yearly$mileage),2)),
                                           hr("预测结果—基本建设投资值（亿元）"),
                                           hr(),
                                           textOutput("mileage_fixed_assets_investment_output") ,
                                           hr(),
                                           textOutput("mileage_fixed_assets_investment_FRR"),
                                           hr(),
                                           textOutput("mileage_fixed_assets_investment_zhi")
                                           
                                           # actionButton("predictFre","预测新货运量") 
                                         ),                                                       #sidebarPanel
                                         
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("多元线性回归", plotOutput("mileage_linearplot")), 
                                             tabPanel("随机森林回归", plotOutput("mileage_rfplot")), 
                                             tabPanel("支持向量机回归", plotOutput("mileage_svmplot"))
                                           ),
                                           
                                           fluidRow(  DT::dataTableOutput("mileage_table")   )
                                         )
                                       )
                              ),
                              
                              tabPanel("基本建设投资-新线铺轨里程",
                                       titlePanel("基本建设投资-新线铺轨里程"),
                                       
                                       sidebarLayout(
                                         sidebarPanel(
                                           checkboxInput(inputId="tracklaying_mileage_stat_data",
                                                         label=strong("历史统计值"),
                                                         value=TRUE),
                                           
                                           checkboxInput(inputId = "tracklaying_mileage_predict_data",
                                                         label = strong("回归预测值"),
                                                         value = TRUE),
                                           selectInput(inputId = "tracklaying_mileage_year_start",
                                                       label = "自:", 
                                                       choices = y_wenjing_rawdata_yearly,
                                                       selected = min(y_wenjing_rawdata_yearly) ),
                                           selectInput(inputId="tracklaying_mileage_year_end",
                                                       label="至:",
                                                       choice=y_wenjing_rawdata_yearly,
                                                       selected=max(y_wenjing_rawdata_yearly) ),
                                           textInput(inputId="newline_tracklaying_mileage_input",
                                                     label=strong("新线铺轨里程（公里）"),
                                                     value=round(mean(df_yearly$newline_tracklaying_mileage),2)),
                                           #textInput(inputId="oldline_tracklaying_mileage_input",
                                           #label=strong("复线铺轨里程（公里）"),
                                           #value=round(mean(df_yearly$oldline_tracklaying_mileage),2)),
                                           hr("预测结果——固定资产值（亿元）"),
                                           hr(),
                                           textOutput("tracklaying_mileage_output") ,
                                           hr(),
                                           textOutput("tracklaying_mileage_FRR"),
                                           hr(),
                                           textOutput("tracklaying_mileage_zhi")
                                           
                                         ),                                                    
                                         
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("多元线性回归", plotOutput("tracklaying_mileage_linearplot")), 
                                             tabPanel("随机森林回归", plotOutput("tracklaying_mileage_rfplot")), 
                                             tabPanel("支持向量机回归", plotOutput("tracklaying_mileage_svmplot"))
                                             
                                           ),
                                           
                                           fluidRow(  DT::dataTableOutput("tracklaying_mileage_table")   )
                                         )
                                       )
                              ),
                              
#-----------------------------------------固定资产适配性研究----------------------------------------
#-----------------------------------------固定资产适配性研究----------------------------------------
                              
                              tabPanel("固定资产-新增普客车辆数-新增动车组数",
                                       titlePanel("固定资产-新增普客车辆数-新增动车组数"),
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
                                                     label=strong("预测输入值—新增普客车辆数（辆）"),
                                                     value=round(mean(investment_data$passenger_car_delta),0)),
                                           textInput(inputId="htrain_input",
                                                     label=strong("预测输入值—动车组增加数（组）"),
                                                     value=round(mean(investment_data$bullettrain_number_delta),0)),      
                                           
                                           hr("预测结果——固定资产投资额（亿元）"),
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
                                           
                                           fluidRow(  DT::dataTableOutput("investmenttable"))
                                         )
                                       )
                                       ),

tabPanel("客运量-动车组数-客运机车日车公里数",
         titlePanel("客运量-动车组数-客运机车日车公里数"),
         
         sidebarLayout(
           sidebarPanel(
             checkboxInput(inputId="passenger_volume_stat_data",
                           label=strong("历史统计值"),
                           value=TRUE),
             
             checkboxInput(inputId = "passenger_volume_predict_data",
                           label = strong("回归预测值"),
                           value = TRUE),
             selectInput(inputId = "passenger_volume_year_start",
                         label = "自:", 
                         choices = PVy,
                         selected = min(PVy) ),
             selectInput(inputId="passenger_volume_year_end",
                         label="至:",
                         choice=PVy,
                         selected=max(PVy) ),
             textInput(inputId="bullettrain_number_input",
                       label=strong("动车组数（辆）"),
                       value=round(mean(PVdf$bullettrain_number),0)),
             textInput(inputId="locomotive_mileage_pcar_input",
                       label=strong("客车机车日车公里（公里）"),
                       value=round(mean(PVdf$locomotive_mileage_pcar),2)),
             hr("预测结果——客运量（万人）"),
             hr(),
             textOutput("passenger_volume_output") ,
             hr(),
             textOutput("passenger_volume_FRR"),
             hr(),
             textOutput("passenger_volume_zhi")
             
             
           ),                                                     
           
           mainPanel(
             tabsetPanel(
               tabPanel("多元线性回归", plotOutput("passenger_volume_linearplot")), 
               tabPanel("随机森林回归", plotOutput("passenger_volume_rfplot")), 
               tabPanel("支持向量机回归", plotOutput("passenger_volume_svmplot"))
             ),
             
             fluidRow(  DT::dataTableOutput("passenger_volume_table")   )
           )
         )
),

#-----------------------------------------营业里程适配性研究----------------------------------------
#-----------------------------------------营业里程适配性研究----------------------------------------

                              tabPanel("营业里程-机车台数-动车组数",
                                       titlePanel("营业里程-机车台数-动车组数"),
                                       sidebarLayout(
                                         sidebarPanel(
                                           checkboxInput(inputId="distance_stat_data1",
                                                         label=strong("历史统计值"),
                                                         value=TRUE),
                                           checkboxInput(inputId = "distance_predict_data1",
                                                         label = strong("回归预测值"),
                                                         value = TRUE),
                                           selectInput(inputId = "distance_year_start1",
                                                       label = "自:", 
                                                       choices = distance_tm,
                                                       selected = min(distance_tm)),
                                           selectInput(inputId="distance_year_end1",
                                                       label="至:",
                                                       choice=distance_tm,
                                                       selected=max(distance_tm)),
                                           textInput(inputId="locomotive_input",
                                                     label=strong("预测输入值——机车数量（辆）"),
                                                     value=round(mean(distance_fre$locomotive_number),0)),
                                           textInput(inputId="bullettrain_input",
                                                     label=strong("预测输入值——动车组数量（组）"),
                                                     value=round(mean(distance_fre$bullettrain_number),0)),
                                           hr("预测结果——营业里程（公里）"),
                                           hr(),
                                           textOutput("distance_output") ,
                                           hr(),
                                           textOutput("distance_FRR"),
                                           hr(),
                                           textOutput("distance_zhi")
                                           
                                         ), 
                                         
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("多元线性回归", plotOutput("distancelinearplot")), 
                                             tabPanel("随机森林回归", plotOutput("distancerfplot")), 
                                             tabPanel("支持向量机回归", plotOutput("distancesvmplot"))
                                           ),
                                           
                                           fluidRow(  DT::dataTableOutput("distancetable") )
                                         )
                                       )
                              ),

#---------------------------------------------------------------------------------------------------------
#-----------------------------------------机车数量适配性研究----------------------------------------------       

                              tabPanel("机车数量-普铁客运量-货运量",
                                       titlePanel("机车数量-普铁客运量-货运量"),
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
                                                       selected = min(Locomotive_tm)),
                                           selectInput(inputId="Locomotive_year_end1",
                                                       label="至:",
                                                       choice=Locomotive_tm,
                                                       selected=max(Locomotive_tm)),
                                           textInput(inputId="ton_input",
                                                     label=strong("预测输入值——货运量（万吨）"),
                                                     value=round(mean(Locomotive_fre$freight_volume_yearly),2)),
                                           textInput(inputId="ptpassenger_input",
                                                     label=strong("预测输入值——普铁客运量（万人）"),
                                                     value=round(mean(Locomotive_fre$ptpassenger_volume),0)),
                                           hr("预测结果——机车数量（辆）"),
                                           hr(),
                                           textOutput("locomotive_output1") ,
                                           hr(),
                                           textOutput("locomotive_FRR1"),
                                           hr(),
                                           textOutput("locomotive_zhi1")
                                           
                                         ), 
                                         
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("多元线性回归", plotOutput("freightlinearplot")), 
                                             tabPanel("随机森林回归", plotOutput("freightrfplot")), 
                                             tabPanel("支持向量机回归", plotOutput("freightsvmplot"))
                                           ),
                                           
                                           fluidRow(  DT::dataTableOutput("freighttable") )
                                         )
                                       )
                              ), 
#-------------------------------------------------------------------------------------------------------                                      
tabPanel("货运量-货车车辆数-营业里程",          
#------------------------------------------货运量-营业里程---------------------------------------------
                                       titlePanel("货运量-货车车辆数-营业里程"),
                                       
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
                                                       choices = year_slection,
                                                       selected = min(year_slection) ),
                                           selectInput(inputId="freight_mileage_year_end",
                                                       label="至:",
                                                       choice=year_slection,
                                                       selected=max(year_slection) ),
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
                                            choices = year_slection,
                                            selected = min(year_slection) ),
                                selectInput(inputId="year_end",
                                            label="至:",
                                            choice=year_slection,
                                            selected=max(year_slection) ),
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
                                textOutput("freight_zhi"),
                                hr(),
                                actionButton('model_feight','更新模型')
                                
                                
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
                                            choices = year_slection_passenger,
                                            selected = min(year_slection_passenger) ),
                                selectInput(inputId="passagerpre_year_end",
                                            label="至:",
                                            choice=year_slection_passenger,
                                            selected=max(year_slection_passenger) ),
                                
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
                                textOutput("passagerpre_output"),
                                hr(),
                                textOutput("passagerpre_FRR"),
                                hr(),
                                textOutput("passagerpre_zhi"),
                                hr(),
                                actionButton("model_passager","更新模型")
                                
                                
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
                              tabPanel("客运量",
                                       titlePanel("客运量时间序列预测"),
                                       
                                       fluidRow(
                                         plotOutput(outputId = "passenger_volume_forecast", height = "600px")
                                       ), 
                                       fluidRow(
                                         column(12,DT::dataTableOutput("passenger_volume_forecast_table"))
                                       ) 
                              ),
                              tabPanel("工业增加值增长率",
                                       titlePanel("工业增加值增长率时间序列预测"),
                                       
                                       fluidRow(
                                         plotOutput(outputId = "Industrial_Added_Value_Rate_forecast_timesery", height = "600px")
                                       ), 
                                       fluidRow(
                                         column(12,DT::dataTableOutput("Industrial_Added_Value_Rate_forecast_timesery_table"))
                                       )
                              ),
                              tabPanel("铁路固定资产",
                                       titlePanel("铁路固定资产时间序列预测"),
                                       
                                       fluidRow(
                                         plotOutput(outputId = "Investment_in_Fixed_Assets_forecast_timesery", height = "600px")
                                       ), 
                                       fluidRow(
                                         column(12,DT::dataTableOutput("Investment_in_Fixed_Assets_forecast_table_timesery"))
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

                  navbarMenu("原始数据",                  
                   tabPanel("景气指数相关原始数据",
                            titlePanel("铁路景气指数原始数据"),
                            
                            
                            fluidRow(
                              column(12, tabsetPanel(type="tabs",
                                                     
                                                     #-------------------页签：相关行业数据
                                                     
                                                     tabPanel( "相关行业数据", 
                                                               fluidRow(
                                                                 
                                                                 sidebarLayout(
                                                                   sidebarPanel(
                                                                     
                                                                     radioButtons(inputId="relevant_industry_rawdata", #xghysj_rawdata 原始数据显示中的相关行业数据页签的单选框，以下是5个类别的变量代码
                                                                                  label=NULL,
                                                                                  choices = c("成品钢材产量(万吨)"="iron_output_rawdata",
                                                                                              "原油加工量(万吨)"="oil_processing_volume_rawdata",
                                                                                              "原煤产量(万吨)"="coal_output_rawdata",
                                                                                              "火力发电量(亿千瓦时)"="coalfired_power_generation_rawdata",
                                                                                              "工业增加值(%)"="industrial_added_value_rawdata") ),
                                                                     hr(),
                                                                     selectInput(inputId = "year_start_relevant_industry", #year_start_xghy 相关行业数据中的起始年下拉框，以下终止年雷同
                                                                                 label = "自:", 
                                                                                 choices = y_wenjing_rawdata_monthly,
                                                                                 selected = min(y_wenjing_rawdata_monthly) ),
                                                                     selectInput(inputId="year_end_relevant_industry",
                                                                                 label="至:",
                                                                                 choice=y_wenjing_rawdata_monthly,
                                                                                 selected=max(y_wenjing_rawdata_monthly) ),
                                                                     width=3
                                                                   ),     #siderbarpanel
                                                                   mainPanel(plotOutput(outputId = "rawdata_relevant_industry_plot", height = "400px"),width=9)#rawdata_relevant_industry_plot原始数据中相关行业的画图
                                                                 )  #mainpanel
                                                               ),
                                                               
                                                               fluidRow(
                                                                 column(12,DT::dataTableOutput("rawdata_relevant_industry_table"))#rawdata_relevant_industry_table原始数据中相关行业的数据表输出
                                                               )
                                                     ), #第一个页签
                                                     
                                                     
                                                     #-------------------页签：运量相关 
                                                     
                                                     tabPanel("运量相关", 
                                                              fluidRow(
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    radioButtons(inputId="transport_rawdata",#transport_rawdata 原始数据显示中的运量相关数据页签的单选框，以下是4个类别的变量代码
                                                                                 label=NULL,
                                                                                 choices = c("货运量(万吨)"="freight_volume_rawdata",
                                                                                             "货运周转量(亿吨公里)"="freight_rotation_volume_rawdata",
                                                                                             "客运量(亿人)"="passenger_volume_rawdata",
                                                                                             "客运周转量(亿人公里)"="passenger_person_km_rawdata") ),
                                                                    hr(),
                                                                    selectInput(inputId = "year_start_rawdata_transport",#year_start_xghy 运量相关数据中的起始年下拉框，以下终止年雷同
                                                                                label = "自:", 
                                                                                choices = y_wenjing_rawdata_monthly,
                                                                                selected = min(y_wenjing_rawdata_monthly) ),
                                                                    selectInput(inputId="year_end_rawdata_transport",
                                                                                label="至:",
                                                                                choice=y_wenjing_rawdata_monthly,
                                                                                selected=max(y_wenjing_rawdata_monthly) ),
                                                                    width=3
                                                                  ),
                                                                  
                                                                  mainPanel(plotOutput(outputId = "rawdata_transport_plot", height = "380px"),width=9)#rawdata_transport_plot原始数据中运量相关的画图
                                                                )),
                                                              
                                                              fluidRow(
                                                                column(12,DT::dataTableOutput("rawdata_transport_table")) #rawdata_transport_table原始数据中运量相关的数据表输出
                                                              )
                                                     ), #第二个页签
                                                     
                                                     #-------------------页签：运营相关 
                                                     
                                                     tabPanel("运营相关", 
                                                              fluidRow(
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    radioButtons(inputId="operation_rawdata",#与上雷同，operation:运营相关原始数据
                                                                                 label=NULL,
                                                                                 choices = c("营业里程(公里)"="mileage_rawdata",
                                                                                             "日均运用车(辆)"="dailycar_run_rawdata",
                                                                                             "日均现在车(辆)"="dailycar_now_rawdata",
                                                                                             "客运机车日车公里(公里)"="locomotive_mileage_pcar_rawdata",
                                                                                             "货运机车日车公里(公里)"="locomotive_mileage_fcar_rawdata",
                                                                                             "机车总行走里程(百万公里)"="locomotive_mileage_sum_rawdata") ),
                                                                    hr(),     
                                                                    selectInput(inputId = "year_start_operation",
                                                                                label = "自:", 
                                                                                choices = y_wenjing_rawdata_yearly,
                                                                                selected = min(y_wenjing_rawdata_yearly) ),
                                                                    selectInput(inputId="year_end_operation",
                                                                                label="至:",
                                                                                choice=y_wenjing_rawdata_yearly,
                                                                                selected=max(y_wenjing_rawdata_yearly) ),
                                                                    width=3
                                                                  ),
                                                                  mainPanel(plotOutput(outputId = "rawdata_operation_plot", height = "440px"),width=9 ))
                                                              ),
                                                              
                                                              
                                                              fluidRow(
                                                                column(12,DT::dataTableOutput("rawdata_operation_table"))
                                                              )
                                                     ), #第三个页签
                                                     
                                                     
                                                     #-------------------页签：资产相关---------------------------------------------    
                                                     tabPanel("资产相关",           #第四个页签
                                                              fluidRow(
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    radioButtons(inputId="property_rawdata",#与上雷同，property:资产相关原始数据
                                                                                 label=NULL,
                                                                                 choices = c("客车辆数(辆)"="passenger_car_rawdata",
                                                                                             "货车辆数(辆)"="freight_car_rawdata",
                                                                                             "机车台数(台)"="locomotive_number_rawdata",
                                                                                             "动车组数(组)"="bullettrain_number_rawdata",
                                                                                             "铁路固定资产投资(亿元)"="fixed_assets_investment_rawdata",
                                                                                             "从业人员数量(万人)"="practitioner_number_rawdata",
                                                                                             "新线铺轨里程(公里)"="newline_tracklaying_mileage_rawdata",
                                                                                             "复线铺轨里程(公里)"="oldline_tracklaying_mileage_rawdata") ),
                                                                    
                                                                    hr(),   
                                                                    selectInput(inputId = "year_start_property",
                                                                                label = "自:", 
                                                                                choices = y_wenjing_rawdata_yearly,
                                                                                selected = min(y_wenjing_rawdata_yearly) ),
                                                                    selectInput(inputId="year_end_property",
                                                                                label="至:",
                                                                                choice=y_wenjing_rawdata_yearly,
                                                                                selected=max(y_wenjing_rawdata_yearly) ),
                                                                    width=3
                                                                  ),
                                                                  mainPanel(plotOutput(outputId = "rawdata_property_plot", height = "400px"),width=9)
                                                                )),
                                                              
                                                              
                                                              fluidRow(
                                                                column(12,DT::dataTableOutput("rawdata_property_table"))
                                                              )
                                                     ), #第四个页签
                                                     
                                                     
                                                     #-------------------页签：黑货运量---------------------------------------------    
                                                     tabPanel("黑货运量",           #第五个页签
                                                              fluidRow(
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    radioButtons(inputId="black_rawdata",#与上雷同，black_rawdata黑货种类的铁路货运量原始数据
                                                                                 label=NULL,
                                                                                 choices = c("矿建(万吨)"="mine_rawdata",
                                                                                             "钢材(万吨)"="iron_rawdata",
                                                                                             "石油(万吨)"="oil_rawdata",
                                                                                             "煤(万吨)"="coal_rawdata",
                                                                                             "金属矿石(万吨)"="metal_rawdata") ),
                                                                    
                                                                    hr(),   
                                                                    selectInput(inputId = "year_start_black_rawdata",
                                                                                label = "自:", 
                                                                                choices = y_wenjing_rawdata_black_white,
                                                                                selected = "2008" ),
                                                                    selectInput(inputId="year_end_black_rawdata",
                                                                                label="至:",
                                                                                choice=y_wenjing_rawdata_black_white,
                                                                                selected=max(y_wenjing_rawdata_black_white) ),
                                                                    width=3
                                                                  ),
                                                                  mainPanel(plotOutput(outputId = "rawdata_black_plot", height = "400px"),width=9)
                                                                )),
                                                              
                                                              fluidRow(
                                                                column(12,DT::dataTableOutput("rawdata_black_table"))
                                                              )
                                                     ), #第五个页签
                                                     
                                                     
                                                     #-------------------页签：白货运量---------------------------------------------    
                                                     tabPanel("白货运量",           #第六个页签
                                                              fluidRow(
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    radioButtons(inputId="white_rawdata",#与上雷同，baihuo:白货种类的铁路货运量原始数据
                                                                                 label=NULL,
                                                                                 choices = c("工业机械(万吨)"="machinery_rawdata",
                                                                                             "电子电气(万吨)"="electronic_rawdata",
                                                                                             "农副产品(万吨)"="agricultural_rawdata",
                                                                                             "饮食烟草(万吨)"="food_tobacco_rawdata",
                                                                                             "文教用品(万吨)"="education_rawdata", 
                                                                                             "零担(吨)"="ltl_rawdata" ,
                                                                                             "集装箱(万吨)"="container_rawdata" )),
                                                                    hr(),   
                                                                    selectInput(inputId = "year_start_white_rawdata",
                                                                                label = "自:", 
                                                                                choices = y_wenjing_rawdata_black_white,
                                                                                selected = "2008" ),
                                                                    selectInput(inputId="year_end_white_rawdata",
                                                                                label="至:",
                                                                                choice=y_wenjing_rawdata_black_white,
                                                                                selected=max(y_wenjing_rawdata_black_white) ),
                                                                    width=3
                                                                  ),
                                                                  mainPanel(plotOutput(outputId = "rawdata_white_plot", height = "400px"),width=9)
                                                                )),
                                                              
                                                              fluidRow(
                                                                column(12,DT::dataTableOutput("rawdata_white_table"))
                                                              )
                                                     )
                                                     
                                                     
                                                     
                                                     
                              ) 
                              ))
                   ),

                   tabPanel("其他原始数据",
                            titlePanel("其他铁路原始数据"),
                            fluidRow(
                                column(12,tabsetPanel(type="tabs",
                                    navbarMenu("机车车辆",
#-------------------页签一：里程相关数据
                         
                         tabPanel( "里程相关",titlePanel('里程相关'), 
                                   fluidPage(
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         h5(strong("1.铁路营业里程"),style="color:black"),
                                         checkboxInput(inputId="total_railway_mileage",
                                                       label=("合计铁路"),
                                                       value=TRUE),
                                         checkboxInput(inputId="national_railway1",
                                                       label=("国家铁路"),
                                                       value=TRUE),
                                         checkboxInput(inputId="joint_investment_railway1",
                                                       label=("合资铁路"),
                                                       value=TRUE),
                                         checkboxInput(inputId="local_railway1",
                                                       label=("地方铁路"),
                                                       value=TRUE),
                                         
                                         hr(),
                                         h5(strong("2.铁路延展里程"),style="color:black"),
                                         checkboxInput(inputId="total_railway_extended_mileage",
                                                       label=("合计铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="national_railway2",
                                                       label=("国家铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="joint_investment_railway2",
                                                       label=("合资铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="local_railway2",
                                                       label=("地方铁路"),
                                                       value=FALSE),
                                         
                                         hr(),
                                         h5(strong("3.铁路正线延展里程"),style="color:black"),
                                         checkboxInput(inputId="total_railway_main_track_extended_mileage",
                                                       label=("合计铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="national_railway3",
                                                       label=("国家铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="joint_investment_railway3",
                                                       label=("合资铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="local_railway3",
                                                       label=("地方铁路"),
                                                       value=FALSE),
                                         
                                         hr(),
                                         h5(strong("4.铁路复线里程"),style="color:black"),
                                         checkboxInput(inputId="total_railway_double_track_mileage",
                                                       label=("合计铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="national_railway4",
                                                       label=("国家铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="joint_investment_railway4",
                                                       label=("合资铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="local_railway4",
                                                       label=("地方铁路"),
                                                       value=FALSE),
                                         
                                         hr(),
                                         h5(strong("5.铁路电气化里程"),style="color:black"),
                                         checkboxInput(inputId="total_railway_electric_mileage",
                                                       label=("合计铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="national_railway5",
                                                       label=("国家铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="double_track5",
                                                       label=("复线"),
                                                       value=FALSE),
                                         checkboxInput(inputId="joint_investment_railway5",
                                                       label=("合资铁路"),
                                                       value=FALSE),
                                         checkboxInput(inputId="local_railway5",
                                                       label=("地方铁路"),
                                                       value=FALSE),
                                         
                                         hr(),
                                         h5(strong("6.铁路内燃化里程"),style="color:black"),
                                         checkboxInput(inputId="national_railway_diesel_mileage",
                                                       label=("国家铁路"),
                                                       value=FALSE),
                                         
                                         width=3
                                       ),     #siderbarpanel
                                       mainPanel(
                                         fluidRow(
                                           column(3,  selectInput(inputId = "year_start_mileage",
                                                                  label = "自:", 
                                                                  choices = railway_mileage_yearly,
                                                                  selected = min(railway_mileage_yearly) ) ),
                                           column(3, selectInput(inputId="year_end_mileage",
                                                                 label="至:",
                                                                 choice=railway_mileage_yearly,
                                                                 selected=max(railway_mileage_yearly) ) )
                                         ),
                                         
                                         plotOutput(outputId = "rawdata_relevant_mileage_plot", height = "440px"),
                                         fluidRow(
                                           column(12,DT::dataTableOutput("rawdata_relevant_mileage_table"))
                                         )
                                         ,width=9
                                       )  #mainpanel
                                     )
                                   )         
                                   
                         ),#第一个页签
                         #-------------------页签：全国铁路分地区营业里程数据
                         
                         tabPanel( "分地区营业里程", titlePanel('分地区营业里程'),
                                   fluidRow(
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4(strong("总计"),style="color:black"),
                                         checkboxInput(inputId="total", 
                                                       label=("合计"),
                                                       value=FALSE),
                                         hr(),
                                         h4(strong("1.华北地区"),style="color:black"),
                                         checkboxInput(inputId="beijing", 
                                                       label=("北京"),
                                                       value=TRUE),
                                         checkboxInput(inputId="tianjin",
                                                       label=("天津"),
                                                       value=TRUE),
                                         checkboxInput(inputId = "hebei",
                                                       label = ("河北"),
                                                       value = TRUE),
                                         checkboxInput(inputId="shanxi1",
                                                       label=("山西"),
                                                       value=TRUE),
                                         checkboxInput(inputId = "neimenggu",
                                                       label = ("内蒙古"),
                                                       value = TRUE),
                                         hr(),
                                         h4(strong("2.东北地区"),style="color:black"),
                                         checkboxInput(inputId="liaoning", #
                                                       label=("辽宁"),
                                                       value=FALSE),
                                         checkboxInput(inputId="jilin",
                                                       label=("吉林"),
                                                       value=FALSE),
                                         checkboxInput(inputId = "heilongjiang",
                                                       label = ("黑龙江"),
                                                       value = FALSE),
                                         hr(),
                                         h4(strong("3.华东地区"),style="color:black"),
                                         checkboxInput(inputId="shanghai",
                                                       label=("上海"),
                                                       value=FALSE),
                                         checkboxInput(inputId="jiangsu",
                                                       label=("江苏"),
                                                       value=FALSE),
                                         checkboxInput(inputId = "zhejiang",
                                                       label = ("浙江"),
                                                       value = FALSE),
                                         checkboxInput(inputId="anhui", 
                                                       label=("安徽"),
                                                       value=FALSE),
                                         checkboxInput(inputId="fujian",
                                                       label=("福建"),
                                                       value=FALSE),
                                         checkboxInput(inputId = "jiangxi",
                                                       label = ("江西"),
                                                       value =FALSE),
                                         checkboxInput(inputId = "shandong",
                                                       label = ("山东"),
                                                       value =FALSE),
                                         hr(),
                                         h4(strong("4.华中地区"),style="color:black"),
                                         checkboxInput(inputId="henan", 
                                                       label=("河南"),
                                                       value=FALSE),
                                         checkboxInput(inputId="hunan",
                                                       label=("湖南"),
                                                       value=FALSE),
                                         checkboxInput(inputId = "hubei",
                                                       label = ("湖北"),
                                                       value = FALSE),
                                         hr(),
                                         h4(strong("5.华南地区"),style="color:black"),
                                         checkboxInput(inputId="guangdong", 
                                                       label=("广东"),
                                                       value=FALSE),
                                         checkboxInput(inputId="guangxi",
                                                       label=("广西"),
                                                       value=FALSE),
                                         checkboxInput(inputId = "hainan",
                                                       label = ("海南"),
                                                       value = FALSE),
                                         hr(),
                                         h4(strong("6.西南地区"),style="color:black"),
                                         checkboxInput(inputId="sichuan", 
                                                       label=("四川"),
                                                       value=FALSE),
                                         checkboxInput(inputId="guizhou",
                                                       label=("贵州"),
                                                       value=FALSE),
                                         checkboxInput(inputId = "yunnan",
                                                       label = ("云南"),
                                                       value = FALSE),
                                         checkboxInput(inputId="xizang",
                                                       label=("西藏"),
                                                       value=FALSE),
                                         hr(),
                                         h4(strong("7.西北地区"),style="color:black"),
                                         checkboxInput(inputId="shanxi2", 
                                                       label=("陕西"),
                                                       value=FALSE),
                                         checkboxInput(inputId="gansu",
                                                       label=("甘肃"),
                                                       value=FALSE),
                                         checkboxInput(inputId = "qinghai",
                                                       label = ("青海"),
                                                       value = FALSE),
                                         checkboxInput(inputId="ningxia",
                                                       label=("宁夏"),
                                                       value=FALSE),
                                         checkboxInput(inputId = "xinjiang",
                                                       label = ("新疆"),
                                                       value = FALSE),
                                         
                                         
                                         
                                         width=3
                                       ),     #siderbarpanel
                                       mainPanel(
                                         fluidRow(
                                           column(3,  selectInput(inputId = "year_start_regional_mileage", 
                                                                  label = "自:", 
                                                                  choices = sub_regional_mileage_yearly,
                                                                  selected = min(sub_regional_mileage_yearly) ) ),
                                           column(3, selectInput(inputId="year_end_regional_mileage",
                                                                 label="至:",
                                                                 choice=sub_regional_mileage_yearly,
                                                                 selected=max(sub_regional_mileage_yearly) ) )
                                         ),
                                         
                                         plotOutput(outputId = "sub_regional_mileage_yearly_plot", height = "440px"),
                                         
                                         sliderInput("year3.2",
                                         "Year",
                                         min = min(sub_regional_mileage_yearly),
                                         max = max(sub_regional_mileage_yearly),
                                         value = max(sub_regional_mileage_yearly)),
                                        
                                         plotOutput(outputId = "map_plot3.2", height = "440px"),
                                         fluidRow(
                                           column(12,DT::dataTableOutput("sub_regional_mileage_yearly_table"))#sub_regional_mileage_yearly_table原始数据中相关的数据表输出
                                         )
                                         ,width=9
                                       )  #mainpanel
                                     )
                                     
                                   )     
                                   
                         ),#第二个页签
                         #-------------------页签：机车拥有量数据
                         
                         tabPanel( "机车拥有量", titlePanel('机车拥有量'),
                                   fluidRow(
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         h5(strong("1.全国铁路机车"),style="color:black"),
                                         checkboxInput(inputId="total_railway_locomotive",
                                                       label=("合计"),
                                                       value=TRUE),
                                         checkboxInput(inputId="diesel_locomotive1",
                                                       label=("内燃机车"),
                                                       value=TRUE),
                                         checkboxInput(inputId="electic_locomotive1",
                                                       label=("电力机车"),
                                                       value=TRUE),
                                         checkboxInput(inputId="steam_locomotive1",
                                                       label=("蒸汽机车"),
                                                       value=TRUE),
                                         hr(),
                                         h5(strong("2.国家铁路机车"),style="color:black"),
                                         checkboxInput(inputId="national_railway_locomotive",
                                                       label=("合计"),
                                                       value=FALSE),
                                         checkboxInput(inputId="diesel_locomotive2",
                                                       label=("内燃机车"),
                                                       value=FALSE),
                                         checkboxInput(inputId="electic_locomotive2",
                                                       label=("电力机车"),
                                                       value=FALSE),
                                         checkboxInput(inputId="steam_locomotive2",
                                                       label=("蒸汽机车"),
                                                       value=FALSE),
                                         hr(),
                                         h5(strong("3.合资铁路机车"),style="color:black"),
                                         checkboxInput(inputId="joint_investment_locomotive",
                                                       label=("合计"),
                                                       value=FALSE),
                                         checkboxInput(inputId="diesel_locomotive3",
                                                       label=("内燃机车"),
                                                       value=FALSE),
                                         checkboxInput(inputId="electic_locomotive3",
                                                       label=("电力机车"),
                                                       value=FALSE),
                                         checkboxInput(inputId="steam_locomotive3",
                                                       label=("蒸汽机车"),
                                                       value=FALSE),
                                         hr(),
                                         h5(strong("4.地方铁路机车"),style="color:black"),
                                         checkboxInput(inputId="local_railway_locomotive",
                                                       label=("合计"),
                                                       value=FALSE),
                                         checkboxInput(inputId="diesel_locomotive4",
                                                       label=("内燃机车"),
                                                       value=FALSE),
                                         checkboxInput(inputId="electic_locomotive4",
                                                       label=("电力机车"),
                                                       value=FALSE),
                                         checkboxInput(inputId="steam_locomotive4",
                                                       label=("蒸汽机车"),
                                                       value=FALSE),
                                         hr(),
                                         
                                         
                                         width=3
                                       ),     #siderbarpanel
                                       mainPanel(
                                         fluidRow(
                                           column(3,  selectInput(inputId = "year_start_locomotive", 
                                                                  label = "自:", 
                                                                  choices = Locomotive_ownership_yearly,
                                                                  selected = min(Locomotive_ownership_yearly) ) ),
                                           column(3,  selectInput(inputId="year_end_locomotive",
                                                                  label="至:",
                                                                  choice=Locomotive_ownership_yearly,
                                                                  selected=max(Locomotive_ownership_yearly) ) )
                                           
                                         ),
                                         
                                         
                                         plotOutput(outputId = "rawdata_Locomotive_ownership_plot", height = "440px"),
                                         fluidRow(
                                           column(12,DT::dataTableOutput("rawdata_Locomotive_ownership_table"))#rawdata_Locomotive_ownership_table原始数据中相关的数据表输出
                                         )
                                         ,width=9#rawdata_Locomotive_ownership_plot原始数据中相关画图
                                       )  #mainpanel
                                     )
                                   )     
                                   
                         ),#第三个页签   
                         #-------------------页签：分机型机车拥有量数据
                         
                         tabPanel( "分机型机车拥有量", titlePanel('分机型机车拥有量'),
                                   fluidRow(
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         h5(strong("1.机车总量"),style="color:black"),
                                         checkboxInput(inputId="total",
                                                       label=("总计"),
                                                       value=TRUE),
                                         
                                         hr(),
                                         
                                         h5(strong("2.内燃机机车车型"),style="color:black"),
                                         
                                         checkboxInput(inputId="diesel_locomotive",
                                                       label=("总计"),
                                                       value=TRUE),
                                         checkboxInput(inputId="dongfeng4",
                                                       label=("东风4"),
                                                       value=FALSE),
                                         checkboxInput(inputId="dongfeng6",
                                                       label=("东风6"),
                                                       value=FALSE),
                                         checkboxInput(inputId="dongfeng7",
                                                       label=("东风7"),
                                                       value=FALSE),
                                         checkboxInput(inputId="dongfeng8",
                                                       label=("东风8"),
                                                       value=FALSE),
                                         checkboxInput(inputId="dongfeng10",
                                                       label=("东风10"),
                                                       value=FALSE),
                                         checkboxInput(inputId="dongfeng11",
                                                       label=("东风11"),
                                                       value=FALSE),
                                         checkboxInput(inputId="dongfeng12",
                                                       label=("东风12"),
                                                       value=FALSE),
                                         checkboxInput(inputId="beijing",
                                                       label=("北京"),
                                                       value=FALSE),
                                         checkboxInput(inputId="nd5",
                                                       label=("ND5"),
                                                       value=FALSE),
                                         checkboxInput(inputId="ny6",
                                                       label=("NY6"),
                                                       value=FALSE),
                                         checkboxInput(inputId="ny7",
                                                       label=("NY7"),
                                                       value=FALSE),
                                         checkboxInput(inputId="qt1",
                                                       label=("其他"),
                                                       value=FALSE),
                                         hr(),
                                         h5(strong("3.电力机车车型"),style="color:black"),
                                         checkboxInput(inputId="electric_locomotive",
                                                       label=("总计"),
                                                       value=TRUE),
                                         checkboxInput(inputId="shaoshan1",
                                                       label=("韶山1"),
                                                       value=FALSE),
                                         checkboxInput(inputId="shaoshan3",
                                                       label=("韶山3"),
                                                       value=FALSE),
                                         checkboxInput(inputId="shaoshan4",
                                                       label=("韶山4"),
                                                       value=FALSE),
                                         checkboxInput(inputId="shaoshan6",
                                                       label=("韶山6"),
                                                       value=FALSE),
                                         checkboxInput(inputId="shaoshan7",
                                                       label=("韶山7"),
                                                       value=FALSE),
                                         checkboxInput(inputId="shaoshan8",
                                                       label=("韶山8"),
                                                       value=FALSE),
                                         checkboxInput(inputId="shaoshan9",
                                                       label=("韶山9"),
                                                       value=FALSE),
                                         checkboxInput(inputId="g6",
                                                       label=("6G"),
                                                       value=FALSE),
                                         checkboxInput(inputId="g8",
                                                       label=("8G"),
                                                       value=FALSE),
                                         checkboxInput(inputId="k6",
                                                       label=("6K"),
                                                       value=FALSE),
                                         checkboxInput(inputId="k8",
                                                       label=("8K"),
                                                       value=FALSE),
                                         checkboxInput(inputId="hxd1",
                                                       label=("HXD1"),
                                                       value=FALSE),
                                         checkboxInput(inputId="hxd2",
                                                       label=("HXD2"),
                                                       value=FALSE),
                                         checkboxInput(inputId="hxd3",
                                                       label=("HXD3"),
                                                       value=FALSE),
                                         checkboxInput(inputId="qt2",
                                                       label=("其他"),
                                                       value=FALSE),
                                         hr(),
                                         h5(strong("4.蒸汽机车车型"),style="color:black"),
                                         checkboxInput(inputId="steam_locomotive",
                                                       label=("总计"),
                                                       value=TRUE),
                                         checkboxInput(inputId="renmin",
                                                       label=("人民"),
                                                       value=FALSE),
                                         checkboxInput(inputId="shengli",
                                                       label=("胜利"),
                                                       value=FALSE),
                                         checkboxInput(inputId="jianshe",
                                                       label=("建设"),
                                                       value=FALSE),
                                         checkboxInput(inputId="qianjin",
                                                       label=("前进"),
                                                       value=FALSE),
                                         checkboxInput(inputId="jiefang",
                                                       label=("解放"),
                                                       value=FALSE),
                                         checkboxInput(inputId="kd7",
                                                       label=("KD7"),
                                                       value=FALSE),
                                         checkboxInput(inputId="qt3",
                                                       label=("其他"),
                                                       value=FALSE),
                                         
                                         width=3
                                       ),     #siderbarpanel
                                       mainPanel(
                                         fluidRow(
                                           column(3, selectInput(inputId = "year_start_model_locomotive", #year_start_model_locomotive 相关数据中的起始年下拉框，以下终止年雷同
                                                                 label = "自:", 
                                                                 choices = model_locomotive_ownership_yearly,
                                                                 selected = min(model_locomotive_ownership_yearly) ) ),
                                           column(3,  selectInput(inputId="year_end_model_locomotive",
                                                                  label="至:",
                                                                  choice=model_locomotive_ownership_yearly,
                                                                  selected=max(model_locomotive_ownership_yearly) ) )
                                           
                                         ),
                                         
                                         plotOutput(outputId = "rawdata_model_Locomotive_ownership_plot", height = "440px"),
                                         fluidRow(
                                           column(12,DT::dataTableOutput("rawdata_model_Locomotive_ownership_table"))#rawdata_model_Locomotive_ownership_table原始数据中相关的数据表输出
                                         )
                                         ,width=9#rawdata_model_Locomotive_ownership_plot原始数据中相关的画图
                                       )  #mainpanel
                                     )
                                   )   
                                   
                         ),#第四个页签
                    #--------fancongcong--start------
#-------------------第一个页签：客车货车拥有量

tabPanel( "全国客车货车拥有量情况",titlePanel("全国客车货车拥有量情况"),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                h4(strong("客车拥有量"),style="color:black"),
                checkboxInput(inputId="national_railway_passenger", 
                              label=("国家铁路"),
                              value=FALSE),
                checkboxInput(inputId="joint_investment_railway_passenger", 
                              label=("合资铁路"),
                              value=FALSE),
                checkboxInput(inputId="local_railway_passenger", 
                              label=("地方铁路"),
                              value=FALSE),
                checkboxInput(inputId="passenger_car_total", 
                              label=("合计"),
                              value=TRUE),
                hr(),
                h4(strong("货车拥有量"),style="color:black"),
                checkboxInput(inputId="national_railway_feight", 
                              label=("国家铁路"),
                              value=FALSE),
                checkboxInput(inputId="joint_investment_railway_feight", 
                              label=("合资铁路"),
                              value=FALSE),
                checkboxInput(inputId="local_railway_feight", 
                              label=("地方铁路"),
                              value=FALSE),
                checkboxInput(inputId="freight_car_total", 
                              label=("合计"),
                              value=TRUE),
                hr(),
                width=3
              ),     #siderbarpanel
              mainPanel(
                fluidRow(
                  column(3, selectInput(inputId = "year_start_holding", #year_start_model_locomotive 相关数据中的起始年下拉框，以下终止年雷同
                                        label = "自:", 
                                        choices = y_fcc_holding_tm,
                                        selected = min(y_fcc_holding_tm) ) ),
                  column(3,  selectInput(inputId="year_end_holding",
                                         label="至:",
                                         choice=y_fcc_holding_tm,
                                         selected=max(y_fcc_holding_tm) ) )
                  
                ),
                plotOutput(outputId = "holding_plot", height = "400px"),width=9)#rawdata_relevant_industry_plot原始数据中相关行业的画图
            )  #mainpanel
          ),
          
          fluidRow(
            column(12,DT::dataTableOutput("holding_table"))#rawdata_relevant_industry_table原始数据中相关行业的数据表输出
          )
),#第一个页签，加标签加逗号
#-------------------第二个页签：机车客车利用率
tabPanel( "机车客车货车利用率", titlePanel("机车客车货车利用率"),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                
                radioButtons(inputId="availablity", #xghysj_rawdata 原始数据显示中的相关行业数据页签的单选框，以下是5个类别的变量代码
                             label=NULL,
                             choices = c("平均每台机车换算周转量(万换算吨公里/台)"="locomotive_rotation_average" ,
                                         "平均每台机车总重吨公里(万吨公里/台)"="locomotive_weight_average", 
                                         "每百营业公里拥有机车台数(台/百公里)"="locomotive_ownership_volume",
                                         "平均每辆客车客运量(万人/辆)"="passenger_car_volume_average", 
                                         "平均每辆客车客运周转量(万人公里/辆)"="passenger_rotation_average",
                                         "每百营业公里拥有客车辆数(辆/百公里)"="passenger_car_ownership_number",
                                         "平均每辆货车完成货运量(吨/辆)"="freight_car_volume_average",
                                         "平均每辆货车完成货运周转量(万吨公里/辆)"="feight_rotation_average",
                                         "每百营业公里拥有货车数(辆/百公里)"="mbyyglyyhcs")),
                hr(),
                selectInput(inputId = "year_start_availablity", #year_start_xghy 相关行业数据中的起始年下拉框，以下终止年雷同
                            label = "自:", 
                            choices = y_fcc_availability_tm,
                            selected = min(y_fcc_availability_tm) ),
                selectInput(inputId="year_end_availablity",
                            label="至:",
                            choice=y_fcc_availability_tm,
                            selected=max(y_fcc_availability_tm) ),
                width=3
              ),     #siderbarpanel
              mainPanel(plotOutput(outputId = "availability_plot", height = "520px"),width=9)#rawdata_relevant_industry_plot原始数据中相关行业的画图
            )  #mainpanel
          ),
          
          fluidRow(
            column(12,DT::dataTableOutput("availability_table"))#rawdata_relevant_industry_table原始数据中相关行业的数据表输出
          )
),#第二个页签，加标签加逗号


#-------------------第三个页签：客车拥有量（分座次）

tabPanel("客车拥有量（分座次）", titlePanel("客车拥有量（分座次）"),
         fluidRow(
           
           sidebarLayout(
             sidebarPanel(
               h4(strong("国家货车拥有量（分车型）"),style="color:black"),
               
               checkboxInput(inputId="soft_sleeper", 
                             label=("软卧车厢(节)"),
                             value=TRUE),
               checkboxInput(inputId="hard_sleeper", 
                             label=("硬卧车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="sleeper", 
                             label=("软硬卧车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="soft_seat", 
                             label=("软座车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="hard_seat", 
                             label=("硬座车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="seat", 
                             label=("软硬座车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="restaurant_car", 
                             label=("餐车车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="package", 
                             label=("行李车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="business", 
                             label=("公务车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="other_car", 
                             label=("其他车厢(节)"),
                             value=FALSE),
               checkboxInput(inputId="air_conditiona_car", 
                             label=("空调车(辆)"),
                             value=FALSE),
               hr(),
               width=3
             ),     #siderbarpanel
             mainPanel(
               
               fluidRow(
                 column(3, selectInput(inputId = "year_start_kecheholding", #year_start_model_locomotive 相关数据中的起始年下拉框，以下终止年雷同
                                       label = "自:", 
                                       choices = y_fcc_kecheholding_tm,
                                       selected = min(y_fcc_kecheholding_tm) ) ),
                 column(3,  selectInput(inputId="year_end_kecheholding",
                                        label="至:",
                                        choice=y_fcc_kecheholding_tm,
                                        selected=max(y_fcc_kecheholding_tm) ) )),
               plotOutput(outputId = "kecheholding_plot", height = "480px"),width=9)#rawdata_relevant_industry_plot原始数据中相关行业的画图
           )  #mainpanel
         ),
         
         fluidRow(
           column(12,DT::dataTableOutput("kecheholding_table"))#rawdata_relevant_industry_table原始数据中相关行业的数据表输出
         )
),#第三个页签，加标签加逗号

#-------------------第四个页签：分车型货车拥有量（分车辆种类）

tabPanel( "货车拥有量（分车型）", titlePanel("货车拥有量（分车型）"),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                h4(strong("国家货车拥有量（分车型）"),style="color:black"),
                
                checkboxInput(inputId="box_car", 
                              label=("篷车(辆)"),
                              value=FALSE),
                checkboxInput(inputId="open_car", 
                              label=("敞车(辆)"),
                              value=FALSE),
                checkboxInput(inputId="flat_car", 
                              label=("平车(辆)"),
                              value=FALSE),
                checkboxInput(inputId="poison_car", 
                              label=("毒品车(辆)"),
                              value=FALSE),
                checkboxInput(inputId="refeigerator_car", 
                              label=("冷藏车(辆)"),
                              value=FALSE),
                checkboxInput(inputId="light_oil", 
                              label=("轻油罐车(辆)"),
                              value=FALSE),
                checkboxInput(inputId="viscous_oil", 
                              label=("粘油罐车(辆)"),
                              value=FALSE),
                checkboxInput(inputId="other_oil_car", 
                              label=("其他罐车(辆)"),
                              value=FALSE),
                checkboxInput(inputId="tank_car", 
                              label=("罐车合计"),
                              value=FALSE),
                checkboxInput(inputId="other_car", 
                              label=("其他(辆)"),
                              value=FALSE),
                checkboxInput(inputId="freight_car_total", 
                              label=("货车辆数合计(辆)"),
                              value=TRUE),
                hr(),
                width=3
              ),     #siderbarpanel
              mainPanel(
                fluidRow(
                  column(3, selectInput(inputId = "year_start_huocheholding", #year_start_model_locomotive 相关数据中的起始年下拉框，以下终止年雷同
                                        label = "自:", 
                                        choices = y_fcc_huocheholding_tm,
                                        selected = min(y_fcc_huocheholding_tm) ) ),
                  column(3,  selectInput(inputId="year_end_huocheholding",
                                         label="至:",
                                         choice=y_fcc_huocheholding_tm,
                                         selected=max(y_fcc_huocheholding_tm) ) )),
                plotOutput(outputId = "huocheholding_plot", height = "480px"),width=9)#rawdata_relevant_industry_plot原始数据中相关行业的画图
            )  #mainpanel
          ),
          
          fluidRow(
            column(12,DT::dataTableOutput("huocheholding_table"))#rawdata_relevant_industry_table原始数据中相关行业的数据表输出
          )
),#第四个页签，加标签加逗号

#-------------------第五个页签：车站主要客货运设施、设备

tabPanel( "车站主要客货运设施、设备",titlePanel("车站主要客货运设施、设备"), 
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                
                radioButtons(inputId="sheshishebei", #xghysj_rawdata 原始数据显示中的相关行业数据页签的单选框，以下是5个类别的变量代码
                             label=NULL,
                             choices = c("办理客运业务车站(个)"="passenger_service_station",
                                         "候车室数量(个)"="waiting_room",
                                         "有行包房车站(个)"="luggage_parcel_station",
                                         "有雨棚站台(个)"="rainshed_platform",
                                         "办理客运乘降的处所(个)"="passenger_take_down_position",
                                         "货物仓库数量(座)"="freight_warehouse_number",
                                         "货物仓库建筑面积(平方米)"="freight_warehouse_area",
                                         "货物雨棚数量(座)"="freight_rainshed_number",
                                         "货物雨棚建筑面积(平方米)"="freight_rainshed_area",
                                         "货物站台数量(座)"="freight_platform_number",
                                         "货物站台建筑面积(平方米)"="freight_platform_area",
                                         "装卸线数量(条)"="load_unload_line_number",
                                         "装卸线全长(米)"="load_unload_line_lenghth",
                                         "装卸线装卸线有效长(米)"="load_unload_line_effective_lenghth") ),
                
                hr(),
                selectInput(inputId = "year_start_sheshishebei", #year_start_xghy 相关行业数据中的起始年下拉框，以下终止年雷同
                            label = "自:", 
                            choices = y_fcc_sheshishebei_tm,
                            selected = min(y_fcc_sheshishebei_tm) ),
                selectInput(inputId="year_end_sheshishebei",
                            label="至:",
                            choice=y_fcc_sheshishebei_tm,
                            selected=max(y_fcc_sheshishebei_tm) ),
                width=3
              ),     #siderbarpanel
              mainPanel(plotOutput(outputId = "sheshishebei_plot", height = "550px"),width=9)#rawdata_relevant_industry_plot原始数据中相关行业的画图
            )  #mainpanel
          ),
          
          fluidRow(
            column(12,DT::dataTableOutput("sheshishebei_table"))#rawdata_relevant_industry_table原始数据中相关行业的数据表输出
          )
) #第五个页签，加标签加逗号
                                 ),
                                    navbarMenu("货客运量",
                                               tabPanel( "运输量-周转量", titlePanel("运输量-周转量"),
                                              fluidRow(
                                              
                                              sidebarLayout(
                                                sidebarPanel(
                                                  
                                                  radioButtons(inputId="yslzzl_rawdata", #yslzzl_rawdata 原始数据显示中的运输量-周转量页签的单选框，以下是6个类别的变量代码
                                                               label=NULL,
                                                               choices = c("全国铁路客运量(万人)"="national_passenger_volume_rawdata",
                                                                           "全国铁路客运周转量(亿人公里)"="national_passenger_turnover_volume_rawdata",
                                                                           "全国铁路货运量(万吨)"="national_freight_volume_rawdata",
                                                                           "全国铁路客运周转量(亿吨公里)"="national_freight_turnover_volume_rawdata",
                                                                           "全国铁路行包运量(万吨)"="national_parcel_volume_rawdata",
                                                                           "全国铁路行包周转量(亿吨公里)"="national_parcel_turnover_volume_rawdata",
                                                                           "全国旅客平均里程(公里)"="national_passenger_average_mileage") ),
                                                  hr(),
                                                  selectInput(inputId = "year_start_yslzzl", #year_start_yslzzl运输量-周转量数据中的起始年下拉框，以下终止年雷同
                                                              label = "自:", 
                                                              choices = y_jingzhao_rawdata_yearly,
                                                              selected = "1949" ),
                                                  selectInput(inputId="year_end_yslzzl",
                                                              label="至:",
                                                              choice=y_jingzhao_rawdata_yearly,
                                                              selected=max(y_jingzhao_rawdata_yearly) ),
                                                  width=3
                                                ),     #siderbarpanel
                                                mainPanel(plotOutput(outputId = "rawdata_yslzzl_plot", height = "400px"),width=9)#rawdata_yslzzl_plot原始数据中运输量-周转量的画图
                                              )  #sidebarLayout
                                            ),#fluidRow
                                            
                                            fluidRow(
                                              column(12,DT::dataTableOutput("rawdata_yslzzl_table"))#rawdata_yslzzl_table原始数据中运输量-周转量的数据表输出
                                            )
                                  ), #tabpane1
#--------jingzhao--end------||
#-------------------lixueyan第二个页签：国家铁路省、市、自治区货物运量

tabPanel("货物运量",titlePanel("货物运量"),
         fluidRow(
           sidebarLayout(
             sidebarPanel(
               h4(strong("总计（万吨）"),style="color:black"),
               checkboxInput(inputId="zj1", #zj1默认的货物运量总计数复选框
                             label=("总计"),
                             value=FALSE),
               hr(),
               h4(strong("1.华北地区"),style="color:black"),
               checkboxInput(inputId="beijing1", #beijing默认的北京数复选框
                             label=("北京"),
                             value=TRUE),
               checkboxInput(inputId="tianjin1",#tianjin默认的天津复选框
                             label=("天津"),
                             value=TRUE),
               checkboxInput(inputId = "hebei1",#hebei默认的河北复选框
                             label = ("河北"),
                             value = FALSE),
               checkboxInput(inputId="shanxi11",#shanxi1默认的山西复选框
                             label=("山西"),
                             value=FALSE),
               checkboxInput(inputId = "neimenggu1",#neimenggu默认的内蒙古复选框
                             label = ("内蒙古"),
                             value = FALSE),
               hr(),
               h4(strong("2.东北地区"),style="color:black"),
               checkboxInput(inputId="liaoning1", #liaoning默认的辽宁复选框
                             label=("辽宁"),
                             value=FALSE),
               checkboxInput(inputId="jilin1",#jilin默认的吉林复选框
                             label=("吉林"),
                             value=FALSE),
               checkboxInput(inputId = "heilongjiang1",#heilongjiang默认的黑龙江复选框
                             label = ("黑龙江"),
                             value = FALSE),
               hr(),
               h4(strong("3.华东地区"),style="color:black"),
               checkboxInput(inputId="shanghai1", #shanghai默认的上海复选框
                             label=("上海"),
                             value=FALSE),
               checkboxInput(inputId="jiangsu1",#jiangsu默认的江苏复选框
                             label=("江苏"),
                             value=FALSE),
               checkboxInput(inputId = "zhejiang1",#zhejiang默认的浙江复选框
                             label = ("浙江"),
                             value = FALSE),
               checkboxInput(inputId="anhui1", #anhui默认的安徽复选框
                             label=("安徽"),
                             value=FALSE),
               checkboxInput(inputId="fujian1",#fujian默认的福建复选框
                             label=("福建"),
                             value=FALSE),
               checkboxInput(inputId = "jiangxi1",#jiangxi默认的江西复选框
                             label = ("江西"),
                             value = FALSE),
               checkboxInput(inputId = "shandong1",#shandong默认的山东复选框
                             label = ("山东"),
                             value = FALSE),
               hr(),
               h4(strong("4.华中地区"),style="color:black"),
               checkboxInput(inputId="henan1", #henan默认的河南复选框
                             label=("河南"),
                             value=FALSE),
               checkboxInput(inputId="hunan1",#hunan默认的湖南复选框
                             label=("湖南"),
                             value=FALSE),
               checkboxInput(inputId = "hubei1",#hubei默认的湖北复选框
                             label = ("湖北"),
                             value = FALSE),
               hr(),
               h4(strong("5.华南地区"),style="color:black"),
               checkboxInput(inputId="guangdong1", #guangdong默认的广东复选框
                             label=("广东"),
                             value=FALSE),
               checkboxInput(inputId="guangxi1",#guangxi默认的广西复选框
                             label=("广西"),
                             value=FALSE),
               checkboxInput(inputId = "hainan1",#hainan默认的海南复选框
                             label = ("海南"),
                             value = FALSE),
               hr(),
               h4(strong("6.西南地区"),style="color:black"),
               checkboxInput(inputId="chongqing1", #chongqing默认的重庆复选框
                             label=("重庆"),
                             value=FALSE),
               checkboxInput(inputId="sichuan1", #sichuan默认的四川复选框
                             label=("四川"),
                             value=FALSE),
               checkboxInput(inputId="guizhou1",#guizhou默认的贵州复选框
                             label=("贵州"),
                             value=FALSE),
               checkboxInput(inputId = "yunnan1",#yunnan默认的云南复选框
                             label = ("云南"),
                             value = FALSE),
               checkboxInput(inputId="xizang1",#xizang默认的西藏复选框
                             label=("西藏"),
                             value=FALSE),
               hr(),
               
               h4(strong("7.西北地区"),style="color:black"),
               checkboxInput(inputId="shanxi21", #shanxi2默认的陕西复选框
                             label=("陕西"),
                             value=FALSE),
               checkboxInput(inputId="gansu1",#gansu默认的甘肃复选框
                             label=("甘肃"),
                             value=FALSE),
               checkboxInput(inputId = "qinghai1",#qinghai默认的青海复选框
                             label = ("青海"),
                             value = FALSE),
               checkboxInput(inputId="ningxia1",#ningxia默认的宁夏复选框
                             label=("宁夏"),
                             value=FALSE),
               checkboxInput(inputId = "xinjiang1",#xinjiang默认的新疆复选框
                             label = ("新疆"),
                             value = FALSE),
               hr(),           
               
               width=3
             ),     #siderbarpanel
             
             
             
             
             mainPanel(fluidRow(
               column(4,selectInput(inputId = "year_start_hyl", #year_start_hyl 货运量的起始年下拉框，以下终止年雷同
                                    label = "自:", 
                                    choices = y_lixueyan_4_16_yearly,
                                    selected = min(y_lixueyan_4_16_yearly) )),
               column(4,selectInput(inputId="year_end_hyl",
                                    label="至:",
                                    choice=y_lixueyan_4_16_yearly,
                                    selected=max(y_lixueyan_4_16_yearly) ))),
               plotOutput(outputId = "hyl_plot", height = "400px"),
               
               sliderInput("year4.16",
               "Year",
               min = min(y_lixueyan_4_16_yearly),
               max = max(y_lixueyan_4_16_yearly),
               value = max(y_lixueyan_4_16_yearly)),
                                        
               plotOutput(outputId = "map_plot4.16", height = "440px"),
               DT::dataTableOutput("hyl_table"),
               width=9)#hyl_plot原始数据中货运量的画图
           )  #mainpanel
         )
         
         
),#第二个页签
#-------------------lixueyan第三个页签：国家铁路省、市、自治区货运周转量

tabPanel("货运周转量",titlePanel("货运周转量"),
         fluidRow(
           sidebarLayout(
             sidebarPanel(
               h4(strong("总计"),style="color:black"),
               checkboxInput(inputId="zj2", #zj2默认的总计复选框
                             label=("总计"),
                             value=FALSE),
               hr(),
               h4(strong("1.华北地区"),style="color:black"),
               checkboxInput(inputId="beijing2", #beijing2默认的北京复选框
                             label=("北京"),
                             value=TRUE),
               checkboxInput(inputId="tianjin2",#tianjin2默认的天津复选框
                             label=("天津"),
                             value=TRUE),
               checkboxInput(inputId = "hebei2",#hebei2默认的河北复选框
                             label = ("河北"),
                             value = TRUE),
               checkboxInput(inputId="shanxi12",#shanxi12默认的shanxi12复选框
                             label=("山西"),
                             value=FALSE),
               checkboxInput(inputId = "neimenggu2",#neimenggu2默认的内蒙古复选框
                             label = ("内蒙古"),
                             value = FALSE),
               hr(),
               h4(strong("2.东北地区"),style="color:black"),
               checkboxInput(inputId="liaoning2", #liaoning2辽宁复选框
                             label=("辽宁"),
                             value=FALSE),
               checkboxInput(inputId="jilin2",#jilin2默认的吉林复选框
                             label=("吉林"),
                             value=FALSE),
               checkboxInput(inputId = "heilongjiang2",#heilongjiang2默认的黑龙江复选框
                             label = ("黑龙江"),
                             value = FALSE),
               hr(),
               h4(strong("3.华东地区"),style="color:black"),
               checkboxInput(inputId="shanghai2", #shanghai2默认的上海复选框
                             label=("上海"),
                             value=FALSE),
               checkboxInput(inputId="jiangsu2",#jiangsu2默认的江苏复选框
                             label=("江苏"),
                             value=FALSE),
               checkboxInput(inputId = "zhejiang2",#zhejiang2默认的浙江复选框
                             label = ("浙江"),
                             value = FALSE),
               checkboxInput(inputId="anhui2", #anhui2默认的安徽复选框
                             label=("安徽"),
                             value=FALSE),
               checkboxInput(inputId="fujian2",#fujian2默认的福建复选框
                             label=("福建"),
                             value=FALSE),
               checkboxInput(inputId = "jiangxi2",#jiangxi2默认的江西复选框
                             label = ("江西"),
                             value = FALSE),
               checkboxInput(inputId = "shandong2",#shandong2默认的山东数复选框
                             label = ("山东"),
                             value = FALSE),
               hr(),
               h4(strong("4.华中地区"),style="color:black"),
               checkboxInput(inputId="henan2", #henan2默认的河南复选框
                             label=("河南"),
                             value=FALSE),
               checkboxInput(inputId="hunan2",#hunan2默认的湖南复选框
                             label=("湖南"),
                             value=FALSE),
               checkboxInput(inputId = "hubei2",#hubei2默认的湖北复选框
                             label = ("湖北"),
                             value = FALSE),
               hr(),
               h4(strong("5.华南地区"),style="color:black"),
               checkboxInput(inputId="guangdong2", #guangdong2默认的广东复选框
                             label=("广东"),
                             value=FALSE),
               checkboxInput(inputId="guangxi2",#guangxi2默认的广西复选框
                             label=("广西"),
                             value=FALSE),
               checkboxInput(inputId = "hainan2",#hainan2默认的海南复选框
                             label = ("海南"),
                             value = FALSE),
               hr(),
               h4(strong("6.西南地区"),style="color:black"),
               checkboxInput(inputId="chongqing2", #chongqing2默认的重庆复选框
                             label=("重庆"),
                             value=FALSE),
               checkboxInput(inputId="sichuan2", #sichuan2默认的四川复选框
                             label=("四川"),
                             value=FALSE),
               checkboxInput(inputId="guizhou2",#guizhou2默认的贵州复选框
                             label=("贵州"),
                             value=FALSE),
               checkboxInput(inputId = "yunnan2",#yunnan2默认的云南复选框
                             label = ("云南"),
                             value = FALSE),
               checkboxInput(inputId="xizang2",#xizang2默认的西藏复选框
                             label=("西藏"),
                             value=FALSE),
               hr(),
               
               h4(strong("7.西北地区"),style="color:black"),
               checkboxInput(inputId="shanxi22", #shanxi22默认的陕西复选框
                             label=("陕西"),
                             value=FALSE),
               checkboxInput(inputId="gansu2",#gansu2默认的甘肃复选框
                             label=("甘肃"),
                             value=FALSE),
               checkboxInput(inputId = "qinghai2",#qinghai2默认的青海复选框
                             label = ("青海"),
                             value = FALSE),
               checkboxInput(inputId="ningxia2",#ningxia2默认的宁夏复选框
                             label=("宁夏"),
                             value=FALSE),
               checkboxInput(inputId = "xinjiang2",#xinjiang2默认的新疆复选框
                             label = ("新疆"),
                             value = FALSE),
               hr(),           
               
               width=3
             ),     #siderbarpanel
             
             mainPanel(fluidRow(
               column(4,selectInput(inputId = "year_start_hyzzl", #year_start_hyl 货运量的起始年下拉框，以下终止年雷同
                                    label = "自:", 
                                    choices = y_lixueyan_4_17_yearly,
                                    selected = min(y_lixueyan_4_17_yearly) )),
               column(4,selectInput(inputId="year_end_hyzzl",
                                    label="至:",
                                    choice=y_lixueyan_4_17_yearly,
                                    selected=max(y_lixueyan_4_17_yearly) ))),
               plotOutput(outputId = "hyzzl_plot", height = "400px"),
               
               sliderInput("year4.17",
               "Year",
               min = min(y_lixueyan_4_17_yearly),
               max = max(y_lixueyan_4_17_yearly),
               value = max(y_lixueyan_4_17_yearly)),
                                        
               plotOutput(outputId = "map_plot4.17", height = "440px"),
               DT::dataTableOutput("hyzzl_table"),
               width=9)#hyl_plot原始数据中货运量的画图
             
           )  #mainpanel
         )
         
         
),#第三个页签
#-------------------lixueyan第四个页签：国家铁路省、市、自治区客运量

tabPanel("客运量",titlePanel("客运量"),
         fluidRow(
           sidebarLayout(
             sidebarPanel(
               h4(strong("总计"),style="color:black"),
               checkboxInput(inputId="zj3", #zj3默认的总计（万人）复选框
                             label=("总计（万人）"),
                             value=FALSE),
               hr(),
               h4(strong("1.华北地区"),style="color:black"),
               checkboxInput(inputId="beijing3", #beijing3默认的北京复选框
                             label=("北京"),
                             value=TRUE),
               checkboxInput(inputId="tianjin3",#tianjin3默认的天津复选框
                             label=("天津"),
                             value=TRUE),
               checkboxInput(inputId = "hebei3",#hebei3默认的河北复选框
                             label = ("河北"),
                             value = TRUE),
               checkboxInput(inputId="shanxi13",#shanxi13默认的山西复选框
                             label=("山西"),
                             value=FALSE),
               checkboxInput(inputId = "neimenggu3",#neimenggu3默认的内蒙古复选框
                             label = ("内蒙古"),
                             value = FALSE),
               hr(),
               h4(strong("2.东北地区"),style="color:black"),
               checkboxInput(inputId="liaoning3", #liaoning3默认的辽宁复选框
                             label=("辽宁"),
                             value=FALSE),
               checkboxInput(inputId="jilin3",#jilin3默认的吉林复选框
                             label=("吉林"),
                             value=FALSE),
               checkboxInput(inputId = "heilongjiang3",#heilongjiang3默认的黑龙江复选框
                             label = ("黑龙江"),
                             value = FALSE),
               hr(),
               h4(strong("3.华东地区"),style="color:black"),
               checkboxInput(inputId="shanghai3", #shanghai3默认的上海复选框
                             label=("上海"),
                             value=FALSE),
               checkboxInput(inputId="jiangsu3",#jiangsu3默认的江苏复选框
                             label=("江苏"),
                             value=FALSE),
               checkboxInput(inputId = "zhejiang3",#zhejiang3默认的浙江复选框
                             label = ("浙江"),
                             value = FALSE),
               checkboxInput(inputId="anhui3", #anhui3默认的安徽复选框
                             label=("安徽"),
                             value=FALSE),
               checkboxInput(inputId="fujian3",#fujian3默认的福建复选框
                             label=("福建"),
                             value=FALSE),
               checkboxInput(inputId = "jiangxi3",#jiangxi3默认的江西复选框
                             label = ("江西"),
                             value = FALSE),
               checkboxInput(inputId = "shandong3",#shandong3默认的山东复选框
                             label = ("山东"),
                             value = FALSE),
               hr(),
               h4(strong("4.华中地区"),style="color:black"),
               checkboxInput(inputId="henan3", #henan3默认的河南复选框
                             label=("河南"),
                             value=FALSE),
               checkboxInput(inputId="hunan3",#hunan3默认的湖南复选框
                             label=("湖南"),
                             value=FALSE),
               checkboxInput(inputId = "hubei3",#hubei3默认的湖北复选框
                             label = ("湖北"),
                             value = FALSE),
               hr(),
               h4(strong("5.华南地区"),style="color:black"),
               checkboxInput(inputId="guangdong3", #guangdong3默认的广东复选框
                             label=("广东"),
                             value=FALSE),
               checkboxInput(inputId="guangxi3",#guangxi3默认的广西复选框
                             label=("广西"),
                             value=FALSE),
               checkboxInput(inputId = "hainan3",#hainan3默认的海南复选框
                             label = ("海南"),
                             value = FALSE),
               hr(),
               h4(strong("6.西南地区"),style="color:black"),
               checkboxInput(inputId="chongqing3", #chongqing3默认的重庆复选框
                             label=("重庆"),
                             value=FALSE),
               checkboxInput(inputId="sichuan3", #sichuan3默认的四川复选框
                             label=("四川"),
                             value=FALSE),
               checkboxInput(inputId="guizhou3",#guizhou3默认的贵州复选框
                             label=("贵州"),
                             value=FALSE),
               checkboxInput(inputId = "yunnan3",#yunnan3默认的云南复选框
                             label = ("云南"),
                             value = FALSE),
               checkboxInput(inputId="xizang3",#xizang3默认的西藏复选框
                             label=("西藏"),
                             value=FALSE),
               hr(),
               
               h4(strong("7.西北地区"),style="color:black"),
               checkboxInput(inputId="shanxi23", #shanxi23默认的陕西复选框
                             label=("陕西"),
                             value=FALSE),
               checkboxInput(inputId="gansu3",#gansu3默认的甘肃复选框
                             label=("甘肃"),
                             value=FALSE),
               checkboxInput(inputId = "qinghai3",#qinghai3默认的青海复选框
                             label = ("青海"),
                             value = FALSE),
               checkboxInput(inputId="ningxia3",#ningxia3默认的宁夏复选框
                             label=("宁夏"),
                             value=FALSE),
               checkboxInput(inputId = "xinjiang3",#xinjiang3默认的新疆复选框
                             label = ("新疆"),
                             value = FALSE),
               hr(),           
               
               width=3
             ),     #siderbarpanel
             
             mainPanel(fluidRow(
               column(4,selectInput(inputId = "year_start_kyl", #year_start_hyl 货运量的起始年下拉框，以下终止年雷同
                                    label = "自:", 
                                    choices = y_lixueyan_4_18_yearly,
                                    selected = min(y_lixueyan_4_18_yearly) )),
               column(4,selectInput(inputId="year_end_kyl",
                                    label="至:",
                                    choice=y_lixueyan_4_18_yearly,
                                    selected=max(y_lixueyan_4_18_yearly) ))),
               plotOutput(outputId = "kyl_plot", height = "400px"),
               
               sliderInput("year4.18",
               "Year",
               min = min(y_lixueyan_4_18_yearly),
               max = max(y_lixueyan_4_18_yearly),
               value = max(y_lixueyan_4_18_yearly)),
                                        
               plotOutput(outputId = "map_plot4.18", height = "440px"),
               DT::dataTableOutput("kyl_table"),
               width=9)#hyl_plot原始数据中货运量的画图
             
             
           )  #mainpanel
         )
         
         
),#第四个页签
#-------------------lixueyan第五个页签：国家铁路省、市、自治区客运周转量

tabPanel("客运周转量",titlePanel("客运周转量"),
         fluidRow(
           sidebarLayout(
             sidebarPanel(
               h4(strong("总计"),style="color:black"),
               checkboxInput(inputId="zj4", 
                             label=("总计（亿人公里）"),
                             value=FALSE),
               hr(),
               h4(strong("1.华北地区"),style="color:black"),
               checkboxInput(inputId="beijing4", 
                             label=("北京"),
                             value=TRUE),
               checkboxInput(inputId="tianjin4",
                             label=("天津"),
                             value=TRUE),
               checkboxInput(inputId = "hebei4",
                             label = ("河北"),
                             value = TRUE),
               checkboxInput(inputId="shanxi14",
                             label=("山西"),
                             value=FALSE),
               checkboxInput(inputId = "neimenggu4",
                             label = ("内蒙古"),
                             value = FALSE),
               hr(),
               h4(strong("2.东北地区"),style="color:black"),
               checkboxInput(inputId="liaoning4", 
                             label=("辽宁"),
                             value=FALSE),
               checkboxInput(inputId="jilin4",
                             label=("吉林"),
                             value=FALSE),
               checkboxInput(inputId = "heilongjiang4",
                             label = ("黑龙江"),
                             value = FALSE),
               hr(),
               h4(strong("3.华东地区"),style="color:black"),
               checkboxInput(inputId="shanghai4", 
                             label=("上海"),
                             value=FALSE),
               checkboxInput(inputId="jiangsu4",
                             label=("江苏"),
                             value=FALSE),
               checkboxInput(inputId = "zhejiang4",
                             label = ("浙江"),
                             value = FALSE),
               checkboxInput(inputId="anhui4", 
                             label=("安徽"),
                             value=FALSE),
               checkboxInput(inputId="fujian4",
                             label=("福建"),
                             value=FALSE),
               checkboxInput(inputId = "jiangxi4",
                             label = ("江西"),
                             value = FALSE),
               checkboxInput(inputId = "shandong4",
                             label = ("山东"),
                             value = FALSE),
               hr(),
               h4(strong("4.华中地区"),style="color:black"),
               checkboxInput(inputId="henan4", 
                             label=("河南"),
                             value=FALSE),
               checkboxInput(inputId="hunan4",
                             label=("湖南"),
                             value=FALSE),
               checkboxInput(inputId = "hubei4",
                             label = ("湖北"),
                             value = FALSE),
               hr(),
               h4(strong("5.华南地区"),style="color:black"),
               checkboxInput(inputId="guangdong4", 
                             label=("广东"),
                             value=FALSE),
               checkboxInput(inputId="guangxi4",
                             label=("广西"),
                             value=FALSE),
               checkboxInput(inputId = "hainan4",
                             label = ("海南"),
                             value = FALSE),
               hr(),
               h4(strong("6.西南地区"),style="color:black"),
               checkboxInput(inputId="chongqing4", 
                             label=("重庆"),
                             value=FALSE),
               checkboxInput(inputId="sichuan4", 
                             label=("四川"),
                             value=FALSE),
               checkboxInput(inputId="guizhou4",
                             label=("贵州"),
                             value=FALSE),
               checkboxInput(inputId = "yunnan4",
                             label = ("云南"),
                             value = FALSE),
               checkboxInput(inputId="xizang4",
                             label=("西藏"),
                             value=FALSE),
               hr(),
               
               h4(strong("7.西北地区"),style="color:black"),
               checkboxInput(inputId="shanxi24", 
                             label=("陕西"),
                             value=FALSE),
               checkboxInput(inputId="gansu4",
                             label=("甘肃"),
                             value=FALSE),
               checkboxInput(inputId = "qinghai4",
                             label = ("青海"),
                             value = FALSE),
               checkboxInput(inputId="ningxia4",
                             label=("宁夏"),
                             value=FALSE),
               checkboxInput(inputId = "xinjiang4",
                             label = ("新疆"),
                             value = FALSE),
               hr(),           
               
               width=3
             ),     #siderbarpanel
             
             mainPanel(fluidRow(
               column(4,selectInput(inputId = "year_start_kyzzl", #year_start_hyl 货运量的起始年下拉框，以下终止年雷同
                                    label = "自:", 
                                    choices = y_lixueyan_4_19_yearly,
                                    selected = min(y_lixueyan_4_19_yearly) )),
               column(4,selectInput(inputId="year_end_kyzzl",
                                    label="至:",
                                    choice=y_lixueyan_4_19_yearly,
                                    selected=max(y_lixueyan_4_19_yearly) ))),
               plotOutput(outputId = "kyzzl_plot", height = "400px"),
               
               sliderInput("year4.19",
               "Year",
               min = min(y_lixueyan_4_19_yearly),
               max = max(y_lixueyan_4_19_yearly),
               value = max(y_lixueyan_4_19_yearly)),
                                        
               plotOutput(outputId = "map_plot4.19", height = "440px"),
               DT::dataTableOutput("kyzzl_table"),
               width=9)#hyl_plot原始数据中货运量的画图
             
             
           )  #mainpanel
         )
         
         
)#第五个页签
),
                                    navbarMenu("国家铁路指标",
tabPanel("机车运用指标",titlePanel("机车运用指标"),
                    fluidRow(
                      sidebarLayout(
                        sidebarPanel(
                          h5(strong("1.机车"),style="color:black"),
                          checkboxInput(inputId="daily_possessed_locomotive_number", 
                                        label=("平均一日支配机车台数（台日）"),
                                        value=TRUE),
                          checkboxInput(inputId="used_locomotive_number",
                                        label=("平均一日支配运用机车台数（台日）"),
                                        value=TRUE),
                          checkboxInput(inputId="locomotive_opration_rate",
                                        label=("机车运用率（%）"),
                                        value=FALSE),
                          checkboxInput(inputId="locomotive_repair_rate",
                                        label=("机车检修率（%）"),
                                        value=FALSE),
                          hr(),
                          h5(strong("2.客运机车"),style="color:black"),
                          checkboxInput(inputId="daily_passenger_locomotive_mileage", 
                                        label=("客运机车日车公里(公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="diesel_locomotive1",
                                        label=("内燃客运机车日车公里(公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="electic_locomotive1",
                                        label=("电力客运机车日车公里(公里)"),
                                        value=TRUE),
                          checkboxInput(inputId="steam_locomotive1",
                                        label=("蒸汽客运机车日车公里(公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="passenger_locomotive_speed",
                                        label=("客运机车旅行速度(公里/小时)"),
                                        value=FALSE),
                          checkboxInput(inputId="passenger_locomotive_tech_speed",
                                        label=("客运机车技术速度(公里/小时)"),
                                        value=FALSE),
                          hr(),
                          h5(strong("3.货运机车"),style="color:black"),
                          checkboxInput(inputId="daily_freight_locomotive_mileage", 
                                        label=("货运机车日车公里(公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="diesel_locomotive2",
                                        label=("内燃货运机车日车公里(公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="electic_locomotive2",
                                        label=("电力货运机车日车公里(公里)"),
                                        value=TRUE),
                          checkboxInput(inputId="steam_locomotive2",
                                        label=("蒸汽货运机车日车公里(公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="freight_locomotive_speed",
                                        label=("货运机车旅行速度(公里/小时)"),
                                        value=FALSE),
                          checkboxInput(inputId="freight_locomotive_tech_speed",
                                        label=("货运机车技术速度(公里/小时)"),
                                        value=FALSE),
                          hr(),
                          checkboxInput(inputId="daily_freight_locomotive_volume", 
                                        label=("货运机车日产量(万吨公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="diesel_locomotive3",
                                        label=("内燃货运机车日产量(万吨公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="electic_locomotive3",
                                        label=("电力货运机车日产量(万吨公里)"),
                                        value=FALSE),
                          checkboxInput(inputId="steam_locomotive3",
                                        label=("蒸汽货运机车日产量(万吨公里)"),
                                        value=FALSE),
                          hr(),
                          checkboxInput(inputId="daily_freight_locomotive_height", 
                                        label=("货运列车平均总重(吨)"),
                                        value=FALSE),
                          checkboxInput(inputId="diesel_locomotive4",
                                        label=("内燃货运列车平均总重(吨)"),
                                        value=FALSE),
                          checkboxInput(inputId="electic_locomotive4",
                                        label=("电力货运列车平均总重(吨)"),
                                        value=FALSE),
                          checkboxInput(inputId="steam_locomotive4",
                                        label=("蒸汽货运列车平均总重(吨)"),
                                        value=FALSE),
                          hr(),
                          
                          width=3
                        ),     #siderbarpanel
                        
                        mainPanel(fluidRow(
                          column(4,selectInput(inputId = "year_start_jcyyzb", #year_start_hyl 货运量的起始年下拉框，以下终止年雷同
                                               label = "自:", 
                                               choices = y_lixueyan_5_1_yearly,
                                               selected = min(y_lixueyan_5_1_yearly) )),
                          column(4,selectInput(inputId="year_end_jcyyzb",
                                               label="至:",
                                               choice=y_lixueyan_5_1_yearly,
                                               selected=max(y_lixueyan_5_1_yearly) ))),
                          plotOutput(outputId = "jcyyzb_plot", height = "400px"),
                          DT::dataTableOutput("jcyyzb_table"),
                          width=9)#hyl_plot原始数据中货运量的画图
                        
                        
                      )  #mainpanel
                    )
                    
                    
           ),#第六个页签
tabPanel( "列车正点率", titlePanel('列车正点率'),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                
                checkboxInput(inputId="passenger_depart_on_shedule_rate",
                              label=("旅客列车出发正点率(%)"),
                              value=TRUE),
                checkboxInput(inputId="passenger_running_on_shedule_rate",
                              label=("旅客列车运行正点率(%)"),
                              value=TRUE),
                checkboxInput(inputId = "freight_depart_on_shedule_rate",
                              label = ("货物列车出发正点率(%)"),
                              value = TRUE),
                checkboxInput(inputId = "freight_running_on_shedule_rate",
                              label = ("货物列车运行正点率(%)"),
                              value = TRUE),
                hr(),   
                selectInput(inputId = "year_start_railwaytrain_correct_point", #year_start_tllczdl 铁路列车正点率数据中的起始年下拉框，以下终止年雷同
                            label = "自:", 
                            choices = y_liyafang_rawdata_55,
                            selected = min(y_liyafang_rawdata_55) ),
                selectInput(inputId="year_end_railwaytrain_correct_point",
                            label="至:",
                            choice=y_liyafang_rawdata_55,
                            selected=max(y_liyafang_rawdata_55)),
                width=3
              ),     #siderbarpanel
              mainPanel( plotOutput(outputId = "railwaytrain_correct_point_plot", height = "400px"),width=9)#railwaytrain_correct_point_plot原始数据中铁路列车正点率的画图
            )  #mainpanel
          ),
          
          fluidRow(
            column(12,DT::dataTableOutput("railwaytrain_correct_point_table"))#railwaytrain_correct_point_table原始数据中铁路列车正点率的数据表输出
          )
), #第一个页签
#-------------------页签：国家铁路机车能源消耗

tabPanel( "机车能源消耗", titlePanel('机车能源消耗'),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                
                radioButtons(inputId="railwaytrain_consumption",
                             label=NULL,
                             choices = c("内燃机车油消耗总量(万吨)"="oil_consume_volume",
                                         "内燃机车每万吨公里耗油量(公斤)"="oil_consume_volume_kilometer",
                                         "电力机车电消耗总量(亿千瓦小时)"="electric_consume_volume",
                                         "电力机车每万吨公里耗电量(千瓦小时)"="electric_consume_volume_kilometer",
                                         "蒸汽机车煤消耗总量(万吨)"="coal_consume_volume",
                                         "蒸汽机车每万吨公里耗煤量(公斤)"="coal_consume_volume_kilometer")),
                hr(),   
                selectInput(inputId = "year_start_railwaytrain_energy", 
                            label = "自:", 
                            choices = y_liyafang_rawdata_52,
                            selected = min(y_liyafang_rawdata_52 )),
                selectInput(inputId="year_end_railwaytrain_energy",
                            label="至:",
                            choice=y_liyafang_rawdata_52,
                            selected=max(y_liyafang_rawdata_52)),
                width=3
              ),     #siderbarpanel
              mainPanel( plotOutput(outputId = "railwaytrain_energy_plot", height = "400px"),width=9)#railwaytrain_energy_plot原始数据中铁路机车能源消耗的画图
            )  #mainpanel
          ),
          
          fluidRow(
            column(12,DT::dataTableOutput("railwaytrain_energy_table"))#railwaytrain_energy_table#原始数据中铁路机车能源消耗的数据表输出
          )
), #第二个页签
#-------------------页签：国家铁路机车工作量

tabPanel( "机车工作量", titlePanel('机车工作量'),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                h4(strong("机车走行公里(百万)"),style="color:black"),
                checkboxInput(inputId="locomotive_running_mileage",
                              label=("合计"),
                              value=TRUE),
                checkboxInput(inputId="diesel_locomotive1",
                              label=("内燃"),
                              value=TRUE),
                checkboxInput(inputId = "electic_locomotive1",
                              label = ("电力"),
                              value = TRUE),
                checkboxInput(inputId = "steam_locomotive1",
                              label = ("蒸汽"),
                              value = TRUE),
                h4(strong("本务机走行公里(百万)"),style="color:black"),
                checkboxInput(inputId="service_running_mileage",
                              label=("合计"),
                              value=FALSE),
                checkboxInput(inputId="diesel_locomotive2",
                              label=("内燃"),
                              value=FALSE),
                checkboxInput(inputId = "electic_locomotive2",
                              label = ("电力"),
                              value = FALSE),
                checkboxInput(inputId = "steam_locomotive2",
                              label = ("蒸汽"),
                              value = FALSE),
                h4(strong("牵引重吨公里(亿)"),style="color:black"),
                checkboxInput(inputId="traction_running_mileage",
                              label=("合计"),
                              value=FALSE),
                checkboxInput(inputId="diesel_locomotive3",
                              label=("内燃"),
                              value=FALSE),
                checkboxInput(inputId = "electic_locomotive3",
                              label = ("电力"),
                              value = FALSE),
                checkboxInput(inputId = "steam_locomotive3",
                              label = ("蒸汽"),
                              value = FALSE),
                
                width=3
              ),     #siderbarpanel
              mainPanel(fluidRow(
                column(3,  selectInput(inputId = "year_start_railwaytrain_work", 
                                       label = "自:", 
                                       choices = y_liyafang_rawdata_53,
                                       selected = min(y_liyafang_rawdata_53))),
                column(3,  selectInput(inputId="year_end_railwaytrain_work",
                                       label="至:",
                                       choice=y_liyafang_rawdata_53,
                                       selected=max(y_liyafang_rawdata_53)))
              ), 
              plotOutput(outputId = "railwaytrain_work_plot", height = "450px"), width=9)#railwaytrain_work_plot原始数据中铁路机车工作量的画图
            )  
          ),
          
          fluidRow(
            column(12,DT::dataTableOutput("railwaytrain_work_table"))#railwaytrain_work_table原始数据中铁路机车能源消耗的数据表输出
          )
), #第三个页签
#-------------------页签：国家铁路货车运用指标

tabPanel( "货车运用指标", titlePanel('货车运用指标'),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                h5(strong("日均装卸车(车)"),style="color:black"),
                checkboxInput(inputId="daily_load",
                              label=("总日均装车"),
                              value=TRUE),
                checkboxInput(inputId="daily_load_coal",
                              label=("日均装车_煤"),
                              value=TRUE),
                checkboxInput(inputId = "daily_unload",
                              label = ("日均卸车"),
                              value = TRUE),
                h5(strong("货车静载重(吨)"),style="color:black"),
                checkboxInput(inputId="freight_car_static_weight",
                              label=("静载重"),
                              value=FALSE),
                h5(strong("日均现在/运用车(辆)"),style="color:black"),
                checkboxInput(inputId="daily_ownership_car",
                              label=("日均现在车"),
                              value=FALSE),
                checkboxInput(inputId="daily_using_car",
                              label=("日均运用车"),
                              value=FALSE),
                h5(strong("运用率(%)"),style="color:black"),
                checkboxInput(inputId="feight_car_using_percent",
                              label=("货车运用率"),
                              value=FALSE),
                checkboxInput(inputId="empty_car_running_percent",
                              label=("空车走行率"),
                              value=FALSE),
                h5(strong("货车周转时间(天)"),style="color:black"),
                checkboxInput(inputId="feight_car_turnover",
                              label=("周转时间"),
                              value=FALSE),
                h5(strong("停留时间(小时)"),style="color:black"),
                checkboxInput(inputId="once_detention_time",
                              label=("一次作业停留时间"),
                              value=FALSE),
                checkboxInput(inputId="transit_detention_time",
                              label=("中转停留时间"),
                              value=FALSE),
                h5(strong("距离(公里)"),style="color:black"),
                checkboxInput(inputId="complete_round",
                              label=("全周距"),
                              value=FALSE),
                checkboxInput(inputId="heavy_round",
                              label=("重周距"),
                              value=FALSE),
                checkboxInput(inputId="transit_round",
                              label=("中转距离"),
                              value=FALSE),
                
                width=3
              ),     #siderbarpanel
              mainPanel(fluidRow(
                column(3,  selectInput(inputId = "year_start_railway_freightusing", 
                                       label = "自:", 
                                       choices = y_liyafang_rawdata_54,
                                       selected = min(y_liyafang_rawdata_54))),
                column(3,  selectInput(inputId="year_end_railway_freightusing",
                                       label="至:",
                                       choice=y_liyafang_rawdata_54,
                                       selected=max(y_liyafang_rawdata_54)))
              ), 
              plotOutput(outputId = "railway_freightusing_plot", height = "400px"),
              fluidRow(
                column(12,DT::dataTableOutput("railway_freightusing_table"))#railway_freightusing_table原始数据中国家铁路货车运用指标的数据表输出
              ),
              width=9)#railway_freightusing_plot原始数据中国家铁路货车运用指标的画图
            )  #mainpanel
          )    
) #第四个页签
),
                                    navbarMenu("固定资产",
#-------------------页签：国家铁路固定资产投资

tabPanel( "固定资产投资", titlePanel('固定资产投资'),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                h4(strong("固定资产投资(亿元)"),style="color:black"),
                checkboxInput(inputId="fixed_assets_investment",
                              label=("合计"),
                              value=TRUE),
                checkboxInput(inputId="infrastructure_investment",
                              label=("基本建设"),
                              value=TRUE),
                checkboxInput(inputId = "renovation_investment",
                              label = ("更新改造"),
                              value = TRUE),
                checkboxInput(inputId = "locomotive_purchase_investment",
                              label = ("机车车辆购置"),
                              value = TRUE),
                hr(),   
                selectInput(inputId = "year_start_railway_gdzctz", 
                            label = "自:", 
                            choices = y_liyafang_rawdata_61,
                            selected = min(y_liyafang_rawdata_61)),
                selectInput(inputId="year_end_railway_gdzctz",
                            label="至:",
                            choice=y_liyafang_rawdata_61,
                            selected=max(y_liyafang_rawdata_61)),
                width=3,
                height=200
                
              ),     #siderbarpanel
              mainPanel(plotOutput(outputId = "railway_gdzctz_plot", height = "400px"),width=9)#railway_gdzctz_plot原始数据中固定资产投资的画图
            )  #mainpanel
          ),
          
          fluidRow(
            column(12,DT::dataTableOutput("railway_gdzctz_table"))#railway_gdzctz_table#原始数据中固定资产投资的数据表输出
          )
) ,#第五个页签     
#-------------------页签：国家铁路基本建设投资

tabPanel( "基本建设投资", titlePanel('基本建设投资'),
          fluidRow(
            
            sidebarLayout(
              sidebarPanel(
                h4(strong("基本建设投资(亿元)"),style="color:black"),
                checkboxInput(inputId="infrastructure_investment_sum",
                              label=("合计"),
                              value=TRUE),
                h5(strong("1.按铁路类别分"),style="color:black"),
                checkboxInput(inputId="national_joint_railway",
                              label=("国家和合资铁路"),
                              value=TRUE),
                checkboxInput(inputId="local_railway",
                              label=("地方铁路"),
                              value=TRUE),
                h5(strong("2.按资金来源分"),style="color:black"),
                h6(strong("1)铁道部投资"),style="color:black"),
                checkboxInput(inputId="MOR_investment",
                              label=("合计"),
                              value=FALSE),
                checkboxInput(inputId="MOR_national_joint_railway",
                              label=("国家和合资铁路"),
                              value=FALSE),
                checkboxInput(inputId = "MOR_local_railway",
                              label = ("地方铁路"),
                              value = FALSE),
                h6(strong("2)地方政府与企业投资"),style="color:black"),
                checkboxInput(inputId = "local_government_enterprise_investment",
                              label = ("合计"),
                              value = FALSE),
                checkboxInput(inputId = "local_railway_local",
                              label = ("地方铁路"),
                              value = FALSE),
                h5(strong("3.按建设种类分"),style="color:black"),
                checkboxInput(inputId="old_line",
                              label=("既有线"),
                              value=FALSE),
                checkboxInput(inputId="new_line",
                              label=("新线"),
                              value=FALSE),
                checkboxInput(inputId="other_line",
                              label=("其他"),
                              value=FALSE),
                width=3
                
              ),     #siderbarpanel
              mainPanel(fluidRow(
                column(3,  selectInput(inputId = "year_start_railway_jbjstz", 
                                       label = "自:", 
                                       choices = y_liyafang_rawdata_62,
                                       selected = min(y_liyafang_rawdata_62))),
                column(3,  selectInput(inputId="year_end_railway_jbjstz",
                                       label="至:",
                                       choice=y_liyafang_rawdata_62,
                                       selected=max(y_liyafang_rawdata_62)))
              ), 
              plotOutput(outputId = "railway_jbjstz_plot", height = "450px"),width=9)#railway_jbjstz_plot原始数据中基本建设投资的画图
            )  #mainpanel
          ),
          fluidRow(
            column(12,DT::dataTableOutput("railway_jbjstz_table"))#railway_jbjstz_table#原始数据中基本建设投资的数据表输出
          )
), #第六个页签                                                   
                                       tabPanel("铁路基本建设投资资金来源",titlePanel("铁路基本建设投资的资金来源"),
                                                fluidRow(
                                                        sidebarLayout(
                                                            sidebarPanel(
                                                                h4(strong("基建资金来源合计"),style="color:black"),
                                                                checkboxInput(inputId="df6.3_capital_toal", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("合计"),
                                                                              value=T),
                                                                checkboxInput(inputId = "df6.3_national_budget",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                              label=("国家预算内"),
                                                                              value=T),
                                                                checkboxInput(inputId = "df6.3_internal_loan",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("国内贷款"),
                                                                              value =T),
                                                                checkboxInput(inputId = "df6.3_foreign_capital",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("利用外资"),
                                                                              value =F),
                                                                checkboxInput(inputId="df6.3_special_fund", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("专项基金"),
                                                                              value=FALSE),
                                                                checkboxInput(inputId="df6.3_bond",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                              label=("债券"),
                                                                              value=FALSE),
                                                                checkboxInput(inputId = "df6.3_coal_oil",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("煤代油"),
                                                                              value = FALSE),
                                                                checkboxInput(inputId = "df6.3_MOR_self",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("铁道部自筹"),
                                                                              value = FALSE),
                                                                checkboxInput(inputId="df6.3_enterprise_self", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("企事业自筹"),
                                                                              value=FALSE),
                                                                checkboxInput(inputId="df6.3_others", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("其他资金"),
                                                                              value=FALSE),
                                                                checkboxInput(inputId = "df6.3_vehicle_tax",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("车辆购置税"),
                                                                              value = FALSE),
                                                                width=3
                                                                
                                                            ),
                                                            mainPanel(
                                                                fluidRow(
                                                                column(3,selectInput(inputId = "tm_start6.3",
                                                                                 label = "自:", 
                                                                                 choices = choice6.3,
                                                                                 selected = min(choice6.3),
                                                                                 width =('100%'))),
                                                                column(3,selectInput(inputId="tm_end6.3",
                                                                                 label="至:",
                                                                                 choice=choice6.3,
                                                                                 selected=max(choice6.3),
                                                                                 width =('100%')))),
                                                                plotOutput("plot6.3",height='400px')
                                                                )
                                                )),##fluidpage
                                                fluidRow(column(12,DT::dataTableOutput("table6.3")) )
                                       ),## tabPanel subtest1
                                       tabPanel("全国铁路基本建设铺轨及投产里程",titlePanel("全国铁路基本建设铺轨及投产里程"),
                                                    fluidRow(
                                                        sidebarLayout(
                                                            sidebarPanel(
                                                                h4(strong("新线铺轨里程"),style="color:black"),
                                                                checkboxInput(inputId="df6.4_new_lay_total", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("合计"),
                                                                              value=T),
                                                                checkboxInput(inputId="df6.4_new_lay_national",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                              label=("国家和合资铁路"),
                                                                              value=T),
                                                                checkboxInput(inputId = "df6.4_new_lay_regional",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("地方铁路"),
                                                                              value =T),
                                                                h4(strong("复线铺轨里程"),style="color:black"),
                                                                checkboxInput(inputId="df6.4_multi_lay_total", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("合计"),
                                                                              value=F),
                                                                checkboxInput(inputId="df6.4_multi_lay_national",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                              label=("国家和合资铁路"),
                                                                              value=F),
                                                                checkboxInput(inputId = "df6.4_multi_lay_regional",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("地方铁路"),
                                                                              value =F),
                                                                h4(strong("新线投产里程"),style="color:black"),
                                                                checkboxInput(inputId="df6.4_new_product_total", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("合计"),
                                                                              value=F),
                                                                checkboxInput(inputId="df6.4_new_product_national",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                              label=("国家和合资铁路"),
                                                                              value=F),
                                                                checkboxInput(inputId = "df6.4_new_product_regional",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("地方铁路"),
                                                                              value =F),
                                                                h4(strong("复线投产里程"),style="color:black"),
                                                                checkboxInput(inputId="df6.4_multi_product_total", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("合计"),
                                                                              value=F),
                                                                checkboxInput(inputId="df6.4_multi_product_national",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                              label=("国家和合资铁路"),
                                                                              value=F),
                                                                checkboxInput(inputId = "df6.4_multi_product_regional",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("地方铁路"),
                                                                              value =F),
                                                                h4(strong("电气化铁路投产里程"),style="color:black"),
                                                                checkboxInput(inputId="df6.4_electr_product_total", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("合计"),
                                                                              value=F),
                                                                checkboxInput(inputId="df6.4_electr_product_national",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                              label=("国家和合资铁路"),
                                                                              value=F),
                                                                checkboxInput(inputId = "df6.4_electr_product_regional",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("地方铁路"),
                                                                              value =F),
                                                                width=3
                                                                
                                                            ),
                                                            mainPanel(
                                                                fluidRow(
                                                                    column(3,selectInput(inputId = "tm_start6.4",
                                                                                 label = "自:", 
                                                                                 choices = choice6.4,
                                                                                 selected = min(choice6.4),
                                                                                 width =('100%'))),
                                                                    column(3,selectInput(inputId="tm_end6.4",
                                                                                 label="至:",
                                                                                 choice=choice6.4,
                                                                                 selected=max(choice6.4),
                                                                                 width =('100%')))
                                                                ),
                                                                plotOutput("plot6.4",height='400px'))
                                                ),
                                                DT::dataTableOutput("table6.4"))##fluidpage
                                                ),
                                       tabPanel("国家铁路机车车辆购置",titlePanel("国家铁路机车车辆购置"),
                                                fluidRow(
                                                        sidebarLayout(
                                                            sidebarPanel(
                                                                h4(strong("投资"),style="color:black"),
                                                                checkboxInput(inputId="df6.7_invest_toal", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("投资完成合计"),
                                                                              value=T),
                                                                checkboxInput(inputId = "df6.7_invest_infruatructure",#trans_advanced_Index默认的运输合成先行指数复选框
                                                                              label=("基建资金"),
                                                                              value=T),
                                                                checkboxInput(inputId = "df6.7_invest_change",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("更改资金"),
                                                                              value =T),
                                                                checkboxInput(inputId = "df6.7_invest_others",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("其他资金"),
                                                                              value =T),
                                                                h4(strong("机车"),style="color:black"),
                                                                checkboxInput(inputId="df6.7_motor_total",
                                                                              label=("机车合计"),
                                                                              value=FALSE),
                                                                checkboxInput(inputId="df6.7_motor_fuel",
                                                                              label = ("内燃"),
                                                                              value = FALSE),
                                                                checkboxInput(inputId = "df6.7_motor_electric",
                                                                              label = ("电力"),
                                                                              value = FALSE),
                                                                h4(strong("车辆"),style="color:black"),
                                                                checkboxInput(inputId="df6.7_bus7", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("客车"),
                                                                              value=FALSE),
                                                                checkboxInput(inputId="df6.7_truck", #trans_coor_Index默认的运输合成同步指数复选框
                                                                              label=("货车"),
                                                                              value=FALSE),
                                                                h4(strong("动车组"),style="color:black"),
                                                                checkboxInput(inputId = "df6.7_bullet",#trans_cdelay_Index默认的运输合成滞后指数复选框
                                                                              label = ("动车组"),
                                                                              value = FALSE),
                                                                width=3
                                                                
                                                            ),
                                                            mainPanel(
                                                                fluidRow(
                                                                    column(3,selectInput(inputId = "tm_start6.7",
                                                                                 label = "自:", 
                                                                                 choices = choice6.7,
                                                                                 selected = min(choice6.7),
                                                                                 width =('100%'))),
                                                                    column(3,selectInput(inputId="tm_end6.7",
                                                                                 label="至:",
                                                                                 choice=choice6.7,
                                                                                 selected=max(choice6.7),
                                                                                 width =('100%')))),#fluidRow
                                                                plotOutput("plot6.7",height='400px'))
                                                ),
                                                DT::dataTableOutput("table6.7"))##fluidpage
                                       )## tabPanel subtest3
                                       )#navvarMune
                                    )
                                    )))
)
)
)









