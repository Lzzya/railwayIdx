
#-------------------------------------------------����-------------------�ٴ��޸�
#github

require(shiny)
require(ggplot2)
require(DT)
require(markdown)

df<-read.csv("freight.csv",head=T)
df$tm<-as.Date.POSIXct(df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
y<-as.numeric(unique(substr(df$tm,1,4)))

df_1<-read.csv("Locomotive-dis.csv",head=T)
y_1<-unique(substr(df_1$tm,1,4))

df_21<-read.csv("货车车辆预测.csv",head=T)
y_21<-unique(substr(df_21$tm,1,4))

df_index<-read.csv("预警.csv",header=T)
df_index$tm<-as.Date.POSIXct(df_index$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据

dftrans<-read.csv("trans-coor.csv",head=T)
dftrans$tm<-as.Date.POSIXct(dftrans$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
y_wenjing<-unique(substr(dftrans$tm,1,4))

cw_df<-read.csv("动车增加数量.csv",head=T)
cw_y<-unique(substr(cw_df$tm,1,4))

pg_cw_df<-read.csv("固定资产指标.csv",head=T)  #固定资产和铺轨里程（新线铺轨历程，旧线铺轨里程）
pg_cw_y<-unique(substr(pg_cw_df$tm,1,4))

Carriagedf<-read.csv("客车车辆预测.csv",head=T)
Carriagey<-unique(substr(Carriagedf$tm,1,4))

liaozili<-read.csv("index-black.csv",head=T)
liaozili$tm<-as.Date.POSIXct(liaozili$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
liaozili_y<-unique(substr(liaozili$tm,1,4))

operatingmileage_df<-read.csv("营业里程.csv",head=T)
operatingmileage_y<-unique(substr(operatingmileage_df$tm,1,4))

SteelTimedf<-read.csv("成品钢材产量.csv",head=T)

TruckTimedf<-read.csv("货车辆数.csv",head=T)

CoalTimedf<-read.csv("原煤产量.csv",head=T)

OilTimedf<-read.csv("原油加工�?.csv",head=T)

dfyssj<-read.csv("compidx-qitahangye.csv",head=T)
dfyssj$tm<-as.Date.POSIXct(dfyssj$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  
y.wenjing.yssj<-unique(substr(dfyssj$tm,1,4))

shinyUI(navbarPage(p(strong("铁路景气指数"),responsive=T,fluid=T),
#-----------------------------------------
#-----------------------------------------
#预警信号系统界面

                   tabPanel("预警信号系统",
                            titlePanel("铁路预警信号�?"),
                            hr(),                          
                            plotOutput(outputId = "plot_index", height = "400px"),
                            hr(),
                            wellPanel(
                              h4("铁路运输景气预警信号系统，是借助于相关计量经济分析方法，将多个指标进行数据处理，合并为一个综合�?"),
                              h4("的指标，对这组指标和综合指标所代表的铁路运输波动状况发出预警信号，通过观察信号的变化情况，来判�?"), 
                              h4("未来铁路运输增长的趋势。在本APP中，五种颜色信号的含义如下："),
                              h4("绿灯---铁路运输发展很稳�?"),
                              h4("蓝灯&浅蓝�?---运输市场景气偏热"),
                              h4("黄灯---铁路运输短期内有转稳和萎缩的可能"),
                              h4("红灯---铁路运输市场景气偏冷")
                            )
                   ),


#-----------------------------------------------------------------------
#----------------------------------------------------------------------
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
                                                                                h4(strong("1.各要素权重默�?"),style="color:black"),
                                                                                checkboxInput(inputId="trans_coor_Index",
                                                                                              label=("同步指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId="trans_advanced_Index",
                                                                                              label=("先行指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId = "trans_delay_Index",
                                                                                              label = ("滞后指数"),
                                                                                              value = TRUE),
                                                                                
                                                                                h4(strong("2.各要素权重手动调�?"),style="color:black"),
                                                                                checkboxInput(inputId="trans_qz_coor_input",
                                                                                              label = strong("2.1 同步指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="trans_hyl_qz_input",
                                                                                          label=h5("货运�?%"),
                                                                                          value="38.66"),
                                                                                textInput(inputId="trans_gyzjz_qz_input",
                                                                                          label=h5("工业增加�?%"),
                                                                                          value="29.74"),
                                                                                textInput(inputId="trans_hyzzl_qz_input",
                                                                                          label=h5("货运周转�?%"),
                                                                                          value="31.60"),
                                                                                
                                                                                checkboxInput(inputId="trans_qz_adv_input",
                                                                                              label = strong("2.2 先行指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="trans_gc_qz_input",
                                                                                          label=h5("成品钢材%"),
                                                                                          value="42.60"),
                                                                                textInput(inputId="trans_ym_qz_input",
                                                                                          label=h5("原煤%"),
                                                                                          value="25.80"),
                                                                                textInput(inputId="trans_yy_qz_input",
                                                                                          label=h5("原油%"),
                                                                                          value="10.31"),
                                                                                textInput(inputId="trans_hlfdl_qz_input",
                                                                                          label=h5("火力发电�?%"),
                                                                                          value="21.29"),
                                                                                
                                                                                checkboxInput(inputId="trans_qz_delay_input",
                                                                                              label = strong("2.3 滞后指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="trans_kyl_qz_input",
                                                                                          label=h5("客运�?%"),
                                                                                          value="9.46"),
                                                                                textInput(inputId="trans_kyzzl_qz_input",
                                                                                          label=h5("客运周转�?%"),
                                                                                          value="10.06"),
                                                                                textInput(inputId="trans_gdzctz_qz_input",
                                                                                          label=h5("固定资产投资%"),
                                                                                          value="80.48"),
                                                                                width=3
                                                                              ),#侧边�?
                                                                              
                                                                              
                                                                              mainPanel(
                                                                                fluidRow(
                                                                                  column(3,  selectInput(inputId = "year_start_trans",
                                                                                                         label = "�?:", 
                                                                                                         choices = y_wenjing,
                                                                                                         selected = min(y_wenjing) )),
                                                                                  column(3, selectInput(inputId="year_end_trans",
                                                                                                        label="�?:",
                                                                                                        choice=y_wenjing,
                                                                                                        selected=max(y_wenjing)))
                                                                                ),
                                                                                plotOutput(outputId="trans_index", height = "400px"),
                                                                                fluidRow(
                                                                                  column(12,DT::dataTableOutput("table_trans_index"))  ),
                                                                                width=9
                                                                              )#主显示区
                                                                              
                                                                            ))), #运输指数的页�?
                                                                
                                                                tabPanel( "设备合成指数", 
                                                                          fluidRow(
                                                                            sidebarLayout(
                                                                              sidebarPanel(
                                                                                h4(strong("1.各要素权重默�?"),style="color:black"),
                                                                                checkboxInput(inputId="equip_coor_Index",
                                                                                              label=("同步指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId = "equip_delay_Index",
                                                                                              label = ("滞后指数"),
                                                                                              value = TRUE),
                                                                                checkboxInput(inputId="equip_advanced_Index",
                                                                                              label=("先行指数"),
                                                                                              value=TRUE),
                                                                                
                                                                                h4(strong("2.各要素权重手动调�?"),style="color:black"),
                                                                                checkboxInput(inputId="equip_qz_coor_input",
                                                                                              label = strong("2.1 同步指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_jczxzlc_qz_input",
                                                                                          label=h5("机车总行走里�?%"),
                                                                                          value="81.28"),
                                                                                textInput(inputId="equip_rjyyc_qz_input",
                                                                                          label=h5("日均运用�?%"),
                                                                                          value="18.72"),
                                                                                
                                                                                checkboxInput(inputId="equip_qz_adv_input",
                                                                                              label = strong("2.2 先行指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_gc_qz_input",
                                                                                          label=h5("成品钢材%"),
                                                                                          value="43.39"),
                                                                                textInput(inputId="equip_ym_qz_input",
                                                                                          label=h5("原煤%"),
                                                                                          value="26.53"),
                                                                                textInput(inputId="equip_yy_qz_input",
                                                                                          label=h5("原油%"),
                                                                                          value="10.56"),
                                                                                textInput(inputId="equip_hlfdl_qz_input",
                                                                                          label=h5("火力发电�?%"),
                                                                                          value="19.51"),
                                                                                
                                                                                checkboxInput(inputId="equip_qz_delay_input",
                                                                                              label = strong("2.3 滞后指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="equip_rjxzc_qz_input",
                                                                                          label=h5("日均现在�?%"),
                                                                                          value="15.44"),
                                                                                textInput(inputId="equip_kyjclc_qz_input",
                                                                                          label=h5("客运机车里程%"),
                                                                                          value="35.92"),
                                                                                textInput(inputId="equip_hyjclc_qz_input",
                                                                                          label=h5("货运机车里程%"),
                                                                                          value="1.80"),
                                                                                textInput(inputId="equip_kcls_qz_input",
                                                                                          label=h5("客车辆数%"),
                                                                                          value="21.42"),
                                                                                textInput(inputId="equip_hcls_qz_input",
                                                                                          label=h5("货车辆数%"),
                                                                                          value="16.34"),
                                                                                textInput(inputId="equip_jcts_qz_input",
                                                                                          label=h5("机车台数%"),
                                                                                          value="9.08"),
                                                                                
                                                                                width=3
                                                                              ),
                                                                              
                                                                              mainPanel(
                                                                                fluidRow(
                                                                                  column(3,  selectInput(inputId = "year_start_equip",
                                                                                                         label = "�?:", 
                                                                                                         choices = y_wenjing,
                                                                                                         selected = min(y_wenjing) )),
                                                                                  column(3, selectInput(inputId="year_end_equip",
                                                                                                        label="�?:",
                                                                                                        choice=y_wenjing,
                                                                                                        selected=max(y_wenjing)))
                                                                                ),
                                                                                plotOutput(outputId="equip_index", height = "400px"),
                                                                                fluidRow(
                                                                                  column(12,DT::dataTableOutput("table_equip_index"))  ),
                                                                                width=9
                                                                              )
                                                                              
                                                                            ))), #设备指数的页�?
                                                                
                                                                tabPanel( "规模合成指数", 
                                                                          fluidRow(
                                                                            sidebarLayout(
                                                                              sidebarPanel(
                                                                                h4(strong("1.各要素权重默�?"),style="color:black"),
                                                                                checkboxInput(inputId="scale_coor_Index",
                                                                                              label=strong("同步指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId="scale_advanced_Index",
                                                                                              label=strong("先行指数"),
                                                                                              value=TRUE),
                                                                                checkboxInput(inputId = "scale_delay_Index",
                                                                                              label = strong("滞后指数"),
                                                                                              value = TRUE),
                                                                                
                                                                                h4(strong("2.各要素权重手动调�?"),style="color:black"),
                                                                                checkboxInput(inputId="scale_qz_coor_input",
                                                                                              label = strong("2.1 同步指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_hyl_qz_input",
                                                                                          label=h5("货运�?%"),
                                                                                          value="17.87"),
                                                                                textInput(inputId="scale_gyzjz_qz_input",
                                                                                          label=h5("工业增加�?%"),
                                                                                          value="67.71"),
                                                                                textInput(inputId="scale_hyzzl_qz_input",
                                                                                          label=h5("货运周转�?%"),
                                                                                          value="14.42"),
                                                                                
                                                                                checkboxInput(inputId="scale_qz_adv_input",
                                                                                              label = strong("2.2 先行指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_gc_qz_input",
                                                                                          label=h5("成品钢材%"),
                                                                                          value="43.39"),
                                                                                textInput(inputId="scale_ym_qz_input",
                                                                                          label=h5("原煤%"),
                                                                                          value="26.53"),
                                                                                textInput(inputId="scale_yy_qz_input",
                                                                                          label=h5("原油%"),
                                                                                          value="10.56"),
                                                                                textInput(inputId="scale_hlfdl_qz_input",
                                                                                          label=h5("火力发电�?%"),
                                                                                          value="19.51"),
                                                                                
                                                                                checkboxInput(inputId="scale_qz_delay_input",
                                                                                              label = strong("2.3 滞后指数要素权重"),
                                                                                              value = FALSE),
                                                                                textInput(inputId="scale_yylc_qz_input",
                                                                                          label=h5("营业里程%"),
                                                                                          value="23.62"),
                                                                                textInput(inputId="scale_cyrysl_qz_input",
                                                                                          label=h5("从业人员数量%"),
                                                                                          value="6.62"),
                                                                                textInput(inputId="scale_kcls_qz_input",
                                                                                          label=h5("客车辆数%"),
                                                                                          value="31.90"),
                                                                                textInput(inputId="scale_hcls_qz_input",
                                                                                          label=h5("货车辆数%"),
                                                                                          value="24.33"),
                                                                                textInput(inputId="scale_jcts_qz_input",
                                                                                          label=h5("机车台数%"),
                                                                                          value="13.53"),
                                                                                
                                                                                width=3
                                                                                
                                                                              ),
                                                                              
                                                                              mainPanel(
                                                                                fluidRow(
                                                                                  column(3,  selectInput(inputId = "year_start_scale",
                                                                                                         label = "�?:", 
                                                                                                         choices = y_wenjing,
                                                                                                         selected = min(y_wenjing) )),
                                                                                  column(3, selectInput(inputId="year_end_scale",
                                                                                                        label="�?:",
                                                                                                        choice=y_wenjing,
                                                                                                        selected=max(y_wenjing)))
                                                                                ),
                                                                                plotOutput(outputId="scale_index", height = "400px"),
                                                                                fluidRow(
                                                                                  column(12,DT::dataTableOutput("table_scale_index")) ),
                                                                                width=9
                                                                              )
                                                                              
                                                                            ))) #规模指数的页�?
                                                                
                                                                
                                         )))
                                       
                                       ),
                              tabPanel("扩散指数")
                   ),

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#黑白货指数界�?

tabPanel("黑货白货指数",
         titlePanel("黑货白货指数"),
         fluidRow(
           column(12, tabsetPanel(type="tabs",
                                  
                                  
#黑货指数数的页签-----------------------------------------------
                                  
                                  tabPanel( "黑货指数", 
                                            fluidRow(
                                              sidebarLayout(
                                                sidebarPanel(
                                                  selectInput(inputId = "liaozili_year_start",
                                                              label = "�?:", 
                                                              choices = liaozili_y,
                                                              selected = min(liaozili_y),
                                                              width =('100%')),
                                                  selectInput(inputId="liaozili_year_end",
                                                              label="�?:",
                                                              choice=liaozili_y,
                                                              selected=max(liaozili_y),
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
                                                  selectInput(inputId = "liaozili_year2_start",
                                                              label = "�?:", 
                                                              choices = liaozili_y,
                                                              selected = min(liaozili_y),
                                                              width =('100%')),
                                                  selectInput(inputId="liaozili_year2_end",
                                                              label="�?:",
                                                              choice=liaozili_y,
                                                              selected=max(liaozili_y),
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
                                                               label=h6("集装箱权�?(%)"),
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

#---------------------------------------------------------------------
#---------------------------------------------------------------------
#适配性研究界�?


                   navbarMenu("适配性研�?",
                     tabPanel("固定资产-营业里程",
                              titlePanel("固定资产-营业里程"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="operatingmileage_stat_data",
                                                label=strong("历史统计�?"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "operatingmileage_predict_data",
                                                label = strong("回归预测�?"),
                                                value = TRUE),
                                  selectInput(inputId = "operatingmileage_year_start",
                                              label = "�?:", 
                                              choices = operatingmileage_y,
                                              selected = min(operatingmileage_y) ),
                                  selectInput(inputId="operatingmileage_year_end",
                                              label="�?:",
                                              choice=operatingmileage_y,
                                              selected=max(operatingmileage_y) ),
                                  textInput(inputId="operatingmileage_input",
                                            label=strong("营业里程"),
                                            value=mean(operatingmileage_df$operatingmileage)),
                                  hr("预测结果——固定资产值（亿元�?"),
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
                                    tabPanel("多元线性回�?", plotOutput("operatingmileage_linearplot")), 
                                    tabPanel("随机森林回归", plotOutput("operatingmileage_rfplot")), 
                                    tabPanel("支持向量机回�?", plotOutput("operatingmileage_svmplot"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("operatingmileage_table")   )
                                )
                              )
                              ),
                     tabPanel("固定资产-铺轨里程",
                              titlePanel("固定资产投资--铺轨里程"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="mileage_stat_data",
                                                label=strong("历史统计�?"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "mileage_predict_data",
                                                label = strong("回归预测�?"),
                                                value = TRUE),
                                  selectInput(inputId = "mileage_year_start",
                                              label = "�?:", 
                                              choices = pg_cw_y,
                                              selected = min(pg_cw_y) ),
                                  selectInput(inputId="mileage_year_end",
                                              label="�?:",
                                              choice=pg_cw_y,
                                              selected=max(pg_cw_y) ),
                                  textInput(inputId="nlm_input",
                                            label=strong("新线铺轨里程（公里）"),
                                            value=mean(pg_cw_df$nlm)),
                                  textInput(inputId="olm_input",
                                            label=strong("复线铺轨里程（公里）"),
                                            value=mean(pg_cw_df$olm)),
                                  hr("预测结果——固定资产值（亿元�?"),
                                  hr(),
                                  textOutput("pg_asset_output") ,
                                  hr(),
                                  textOutput("pg_asset_FRR"),
                                  hr(),
                                  textOutput("pg_asset_zhi")
                                  
                                ),                                                    
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回�?", plotOutput("pg_asset_linearplot")), 
                                    tabPanel("随机森林回归", plotOutput("pg_asset_rfplot")), 
                                    tabPanel("支持向量机回�?", plotOutput("pg_asset_svmplot"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("pg_assettable")   )
                                )
                              )
                              ),
                     tabPanel("固定资产-动车�?",
                              titlePanel("固定资产投资--动车�?"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="emu_stat_data",
                                                label=strong("历史统计�?"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "emu_predict_data",
                                                label = strong("回归预测�?"),
                                                value = TRUE),
                                  selectInput(inputId = "emu_year_start",
                                              label = "�?:", 
                                              choices = cw_y,
                                              selected = min(cw_y) ),
                                  selectInput(inputId="emu_year_end",
                                              label="�?:",
                                              choice=cw_y,
                                              selected=max(cw_y) ),
                                  textInput(inputId="emu_input",
                                            label=strong("动车新增数量"),
                                            value=mean(cw_df$emu)),
                                  hr("预测结果——固定资产值（亿元�?"),
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
                                    tabPanel("多元线性回�?", plotOutput("emu_asset_linearplot")), 
                                    tabPanel("随机森林回归", plotOutput("emu_asset_rfplot")), 
                                    tabPanel("支持向量机回�?", plotOutput("emu_asset_svmplot"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("emu_asset_table")   )
                                )
                              )
                              ),
                     tabPanel("客运�?-客车车辆�?"),
                     tabPanel("机车车辆-营业里程",
                 
                              titlePanel("机车车辆-营业里程"),
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="stat_data_1",
                                                label=strong("历史统计�?"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "predict_data_1",
                                                label = strong("回归预测�?"),
                                                value = TRUE),
                                  selectInput(inputId = "year_start_1",
                                              label = "�?:", 
                                              choices = y_1,
                                              selected = min(y_1) ),
                                  selectInput(inputId="year_end_1",
                                              label="�?:",
                                              choice=y_1,
                                              selected=max(y_1) ),
                                  textInput(inputId="km_input_1",
                                            label=strong("预测输入值——营业里程（公里�?"),
                                            value=mean(df_1$distance)),
                                  hr("预测结果——机车车辆数（辆�?"),
                                  hr(),
                                  textOutput("locomotive_output_1") ,
                                  hr(),
                                  textOutput("locomotive_FRR_1"),
                                  hr(),
                                  textOutput("locomotive_zhi_1")
                                  # actionButton("predictCAR","预测新客车量") 
                                  
                                ), 
                                
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("多元线性回�?", plotOutput("linearplot_1")), 
                                    tabPanel("随机森林回归", plotOutput("rfplot_1")), 
                                    tabPanel("支持向量机回�?", plotOutput("svmplot_1"))
                                  ),
                                  
                                  fluidRow(  DT::dataTableOutput("table_1")   )
                                )
                              ) 
                              
                             ),
    #----------------------------
    #显示货车车辆-营业里程适配性分�?---
                     tabPanel("货车车辆-营业里程",
                       titlePanel("货车车辆-营业里程"),
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxInput(inputId="stat_data_21",
                                                label=strong("历史统计�?"),
                                                value=TRUE),
                                  
                                  checkboxInput(inputId = "predict_data_21",
                                                label = strong("回归预测�?"),
                                                value = TRUE),
                                  selectInput(inputId = "year_start_21",
                                              label = "�?:", 
                                              choices = y_21,
                                              selected = min(y_21) ),
                                  selectInput(inputId="year_end_21",
                                              label="�?:",
                                              choice=y_21,
                                              selected=max(y_21) ),
                                  textInput(inputId="km_input_21",
                                            label=strong("预测输入值——营业里程（公里�?"),
                                            value=mean(df_21$distance)),
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
                                    tabPanel("多元线性回�?", plotOutput("linearplot_21")), 
                                    tabPanel("随机森林回归", plotOutput("rfplot_21")), 
                                    tabPanel("支持向量机回�?", plotOutput("svmplot_21"))
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
                               label=strong("历史统计�?"),
                               value=TRUE),
                 
                 checkboxInput(inputId = "predict_data_ky",
                               label = strong("回归预测�?"),
                               value = TRUE),
                 selectInput(inputId = "year_start_ky",
                             label = "�?:", 
                             choices = Carriagey,
                             selected = min(Carriagey) ),
                 selectInput(inputId="year_end_ky",
                             label="�?:",
                             choice=Carriagey,
                             selected=max(Carriagey) ),
                 textInput(inputId="km_input_ky",
                           label=strong("预测输入值——营业里程（公里�?"),
                           value=mean(Carriagedf$distance)),
                 hr("预测结果——客车辆数（辆）"),
                 hr(),
                 textOutput("ky_carriage_output") ,
                 hr(),
                 textOutput("ky_carriage_FRR"),
                 hr(),
                 textOutput("ky_carriage_zhi")
                 
               ), 
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("多元线性回�?", plotOutput("ky_linearplot")), 
                   tabPanel("随机森林回归", plotOutput("ky_rfplot")), 
                   tabPanel("支持向量机回�?", plotOutput("ky_svmplot"))
                 ),
                 
                 fluidRow(  DT::dataTableOutput("ky_table")   )
               )
             )
             )
                     
                     ),


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#铁路货运量预测界�?

    tabPanel("货运量预�?",
             titlePanel("铁路货运量预�?"),
             hr(),
             
             sidebarLayout(
               sidebarPanel(
                 checkboxInput(inputId="stat_data",
                               label=strong("历史统计�?"),
                               value=TRUE),
                 
                 checkboxInput(inputId = "predict_data",
                               label = strong("回归预测�?"),
                               value = TRUE),
                 selectInput(inputId = "year_start",
                             label = "�?:", 
                             choices = y,
                             selected = min(y) ),
                 selectInput(inputId="year_end",
                             label="�?:",
                             choice=y,
                             selected=max(y) ),
                 numericInput(inputId="iron_input",
                              label=strong("预测输入�?--成品钢材产量(万吨)"),
                              value=9822                                                      
                 ),
                 numericInput(inputId="coal_input",
                              label=strong("预测输入�?--原煤产量输入值（万吨�?"),
                              value=33000),
                 hr("预测结果——货运量（万吨）"),
                 hr(),
                 textOutput("freight_output") ,
                 hr(),
                 textOutput("freight_FRR"),
                 hr(),
                 textOutput("freight_zhi")
                 # actionButton("predictCAR","预测新客车量") 
                 
               ), 
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("多元线性回�?", plotOutput("linearplot")), 
                   tabPanel("随机森林回归", plotOutput("rfplot")), 
                   tabPanel("支持向量机回�?", plotOutput("svmplot"))
                 ),
                 
                 fluidRow(  DT::dataTableOutput("table")   )
               )
             )
    ),



    navbarMenu("时间序列预测",
               tabPanel("货运�?",
                        titlePanel("货运量时间序列预�?"),
                        
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
               tabPanel("货车车辆�?",
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
                        titlePanel("成品钢材量时间序列预�?"),
                        fluidRow(
                          plotOutput(outputId = "SteelTime_forecast", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("SteelTime_forecast_table"))
                        ) 
                        ),
               tabPanel("原油加工�?",
                        titlePanel("原油加工量时间序列预�?"),
                        
                        fluidRow(
                          plotOutput(outputId = "OilTime_forecast", height = "600px")
                        ), 
                        fluidRow(
                          column(12,DT::dataTableOutput("OilTime_forecast_table"))
                        )
                        )
               
               
    ),
tabPanel("原始数据",
         titlePanel("铁路景气指数原始数据"),
         
         
         fluidRow(
           column(12, tabsetPanel(type="tabs",
                                  
                                  #-------------------页签：相关行业数�?
                                  
                                  tabPanel( "相关行业数据", 
                                            fluidRow(
                                              
                                              sidebarLayout(
                                                sidebarPanel(
                                                  
                                                  radioButtons(inputId="xghysj.yssj",
                                                               label=NULL,
                                                               choices = c("成品钢材产量(亿吨)"="cpgccl.yssj",
                                                                           "原油加工�?(亿吨)"="yyjgl.yssj",
                                                                           "原煤产量(亿吨)"="ymcl.yssj",
                                                                           "火力发电�?(亿千瓦时)"="hlfdl.yssj",
                                                                           "工业增加�?(%)"="gyzjz.yssj") ),
                                                  hr(),
                                                  selectInput(inputId = "year_start_xghy",
                                                              label = "�?:", 
                                                              choices = y.wenjing.yssj,
                                                              selected = min(y.wenjing.yssj) ),
                                                  selectInput(inputId="year_end_xghy",
                                                              label="�?:",
                                                              choice=y.wenjing.yssj,
                                                              selected=max(y.wenjing.yssj) ),
                                                  width=3
                                                ),     #siderbarpanel
                                                mainPanel(plotOutput(outputId = "yssj.xghy.plot", height = "400px"),width=9)
                                              )  #mainpanel
                                            ),
                                            
                                            fluidRow(
                                              column(12,DT::dataTableOutput("yssj.xghy.table"))
                                            )
                                  ), #第一个页�?
                                  
                                  
                                  #-------------------页签：运量相�? 
                                  
                                  tabPanel("运量相关", 
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="ylxg.yssj",
                                                              label=NULL,
                                                              choices = c("货运�?(亿吨)"="hyl.yssj",
                                                                          "货运周转�?(亿吨)"="hyzzl.yssj",
                                                                          "客运�?(亿人)"="kyl.yssj",
                                                                          "客运周转�?(亿人)"="kyzzl.yssj") ),
                                                 hr(),
                                                 selectInput(inputId = "year_start_ylxg",
                                                             label = "�?:", 
                                                             choices = y.wenjing.yssj,
                                                             selected = min(y.wenjing.yssj) ),
                                                 selectInput(inputId="year_end_ylxg",
                                                             label="�?:",
                                                             choice=y.wenjing.yssj,
                                                             selected=max(y.wenjing.yssj) ),
                                                 width=3
                                               ),
                                               
                                               mainPanel(plotOutput(outputId = "yssj.ylxg.plot", height = "380px"),width=9)
                                             )),
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.ylxg.table"))
                                           )
                                  ), #第二个页�?
                                  
                                  #-------------------页签：运营相�? 
                                  
                                  tabPanel("运营相关", 
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="yyxg.yssj",
                                                              label=NULL,
                                                              choices = c("营业里程(km)"="yylc.yssj",
                                                                          "日均运用�?(万辆)"="rjyyc.yssj",
                                                                          "日均现在�?(万辆)"="rjxzc.yssj",
                                                                          "客运机车日车公里(km)"="kyjcrcgl.yssj",
                                                                          "货运机车日车公里(km)"="hyjcrcgl.yssj",
                                                                          "机车总行走里�?(1000km)"="jczxzlc.yssj") ),
                                                 hr(),     
                                                 selectInput(inputId = "year_start_yyxg",
                                                             label = "�?:", 
                                                             choices = y.wenjing.yssj,
                                                             selected = min(y.wenjing.yssj) ),
                                                 selectInput(inputId="year_end_yyxg",
                                                             label="�?:",
                                                             choice=y.wenjing.yssj,
                                                             selected=max(y.wenjing.yssj) ),
                                                 width=3
                                               ),
                                               mainPanel(plotOutput(outputId = "yssj.yyxg.plot", height = "440px"),width=9 ))
                                           ),
                                           
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.yyxg.table"))
                                           )
                                  ), #第三个页�?
                                  
                                  
                                  #-------------------页签：运营相�?---------------------------------------------    
                                  tabPanel("资产相关",           #第四个页�?
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="zcxg.yssj",
                                                              label=NULL,
                                                              choices = c("客车辆数(�?)"="kcls.yssj",
                                                                          "货车辆数(万辆)"="hcls.yssj",
                                                                          "机车台数(�?)"="jcts.yssj",
                                                                          "动车台数(�?)"="dcts.yssj",
                                                                          "铁路固定资产投资(亿元)"="tlgdzctz.yssj",
                                                                          "从业人员数量(万人)"="cyrysl.yssj",
                                                                          "新线铺轨里程(km)"="xxpglc.yssj",
                                                                          "复线铺轨里程(km)"="fxpglc.yssj") ),
                                                 
                                                 hr(),   
                                                 selectInput(inputId = "year_start_zcxg",
                                                             label = "�?:", 
                                                             choices = y.wenjing.yssj,
                                                             selected = min(y.wenjing.yssj) ),
                                                 selectInput(inputId="year_end_zcxg",
                                                             label="�?:",
                                                             choice=y.wenjing.yssj,
                                                             selected=max(y.wenjing.yssj) ),
                                                 width=3
                                               ),
                                               mainPanel(plotOutput(outputId = "yssj.zcxg.plot", height = "400px"),width=9)
                                             )),
                                           
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.zcxg.table"))
                                           )
                                  ), #第四个页�?
                                  
                                  
                                  #-------------------页签：黑货白货相�?---------------------------------------------    
                                  tabPanel("黑货白货",           #第五个页�?
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(
                                                 radioButtons(inputId="hhbh.yssj",
                                                              label=NULL,
                                                              choices = c("工业机械(万吨)"="gyjx.yssj",
                                                                          "电子电气(万吨)"="dzdq.yssj",
                                                                          "农副产品(万吨)"="nfcp.yssj",
                                                                          "饮食烟草(万吨)"="ysyc.yssj",
                                                                          "文教用品(万吨)"="wjyp.yssj", 
                                                                          "零担(�?)"="ldld.yssj" ,
                                                                          "集装�?(万吨)"="jzx.yssj" ,
                                                                          "金属矿石(万吨)"="jsks.yssj")),
                                                 
                                                 hr(),   
                                                 selectInput(inputId = "year_start_hhbh",
                                                             label = "�?:", 
                                                             choices = y.wenjing.yssj,
                                                             selected = min(y.wenjing.yssj) ),
                                                 selectInput(inputId="year_end_hhbh",
                                                             label="�?:",
                                                             choice=y.wenjing.yssj,
                                                             selected=max(y.wenjing.yssj) ),
                                                 width=3
                                               ),
                                               mainPanel(plotOutput(outputId = "yssj.hhbh.plot", height = "400px"),width=9)
                                             )),
                                           
                                           fluidRow(
                                             column(12,DT::dataTableOutput("yssj.hhbh.table"))
                                           )
                                  ) #第五个页�?
                                  
                                  
                                  
                                  
           ) #页签套的总括�?
           ))
         )
  )
 )




  




