require(shiny)
require(ggplot2)
require(DT)
df<-read.csv("freightReg.csv",head=T)
df$tm<-as.Date.POSIXct(df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
y<-unique(substr(df$tm,1,4))

shinyUI(fluidPage(
  titlePanel("铁路货运量多元回归预测"),
  
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
      textInput(inputId="iron_input",
                       label=strong("成品钢材产量输入值（万吨）"),
                       value=mean(df$iron)),
      textInput(inputId="coal_input",
                       label=strong("原煤产量输入值（万吨）"),
                       value=mean(df$coal)),
      textOutput("freight_output") ,
      textOutput("freight_FRR")
     # actionButton("predictFre","预测新货运量") 
    
    ), #sidebarPanel
  
    mainPanel(
      plotOutput(outputId = "main_plot", height = "400px"),
      fluidRow(  DT::dataTableOutput("table")   )
    )
  )
  )
 
) 


  
