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
                              #h4("蓝灯&浅蓝灯---运输市场景气偏冷"),
                              h4("绿灯---铁路运输发展稳定"),
                              h4("黄灯---铁路运输短期内有转热和加速的可能"),
                              h4("红灯---铁路运输市场景气较热")
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
                              
                              tabPanel("固定资产-新增客车-新增动车",
                                       titlePanel("固定资产-新增客车-新增动车"),
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
                                                     label=strong("预测输入值—客车增加数量（辆）"),
                                                     value=round(mean(investment_data$passenger_car_delta),0)),
                                           textInput(inputId="htrain_input",
                                                     label=strong("预测输入值—动车组增加数量（组）"),
                                                     value=round(mean(investment_data$bullettrain_number_delta),0)),      
                                           
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
                                           
                                           fluidRow(  DT::dataTableOutput("investmenttable"))
                                         )
                                       )
                                       ),

tabPanel("客运量-动车组数-客运机车日行公里数",
         titlePanel("客运量-动车组数-客运机车日行公里数"),
         
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

                              tabPanel("营业里程-机车-动车",
                                       titlePanel("营业里程-机车-动车"),
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

                              tabPanel("机车数量-客运量-货运量",
                                       titlePanel("机车数量-客运量-货运量"),
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
                                           textInput(inputId="passenger_input",
                                                     label=strong("预测输入值——客运量（万人）"),
                                                     value=round(mean(Locomotive_fre$passenger_volume),0)),
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
tabPanel("货运量-营业里程",          
#------------------------------------------货运量-营业里程---------------------------------------------
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
                                                                                             "货运周转量(亿吨)"="freight_rotation_volume_rawdata",
                                                                                             "客运量(亿人)"="passenger_volume_rawdata",
                                                                                             "客运周转量(亿人)"="passenger_person_km_rawdata") ),
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
                                                                                 choices = c("营业里程(km)"="mileage_rawdata",
                                                                                             "日均运用车(辆)"="dailycar_run_rawdata",
                                                                                             "日均现在车(辆)"="dailycar_now_rawdata",
                                                                                             "客运机车日车公里(km)"="locomotive_mileage_pcar_rawdata",
                                                                                             "货运机车日车公里(km)"="locomotive_mileage_fcar_rawdata",
                                                                                             "机车总行走里程(百万km)"="locomotive_mileage_sum_rawdata") ),
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
                                                                                             "机车台数(辆)"="locomotive_number_rawdata",
                                                                                             "动车台数(台)"="bullettrain_number_rawdata",
                                                                                             "铁路固定资产投资(亿元)"="fixed_assets_investment_rawdata",
                                                                                             "从业人员数量(万人)"="practitioner_number_rawdata",
                                                                                             "新线铺轨里程(km)"="newline_tracklaying_mileage_rawdata",
                                                                                             "复线铺轨里程(km)"="oldline_tracklaying_mileage_rawdata") ),
                                                                    
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

                   tabPanel("其它原始数据"))
)
)









