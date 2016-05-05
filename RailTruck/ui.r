df<-read.csv("客车车辆预测.csv",head=T)
y<-unique(substr(df$tm,1,4))

shinyUI(fluidPage(
  titlePanel("客车车辆-营业里程适配性分析"),
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
      textInput(inputId="km_input",
                label=strong("预测输入值——营业里程（公里）"),
                value=mean(df$distance)),
      hr("预测结果——客车辆数（辆）"),
      hr(),
      textOutput("carriage_output") ,
      hr(),
      textOutput("carriage_FRR"),
      hr(),
      textOutput("carriage_zhi")
      # actionButton("predictCAR","预测新客车量") 
      
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
)

) 



