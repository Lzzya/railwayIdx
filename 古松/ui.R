require(shiny)
require(ggplot2)
require(DT)
require(markdown)

df<-read.csv("freight.csv",head=T)
df$tm<-as.Date.POSIXct(df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE)) #转化为日期型数据
y<-as.numeric(unique(substr(df$tm,1,4)))

shinyUI(navbarPage(title=p(strong("铁路景气指数")),  
                   collapsible=TRUE,
                   tabPanel("适配性"),
#-----------------------------------
#生成一个名字为"适配性研究"的菜单，包含"机车-营业里程" "truck-test" 两个选项                 
                   navbarMenu("适配性研究",
                     
                        tabPanel("机车-营业里程"),                            
                        tabPanel("truck-test")
                     
                   ), 
#-------------------------------------  
#生成"预警信号系统"导航
                  tabPanel("预警信号系统"),              
#------------------------------------- 
#生成"货运量预测"导航
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
                   tabPanel("更多")
#--------------------------------------------------------------
  )
        )




  




