library(shiny)
library(ggplot2)
library(DT)
df<-read.csv("compidx-trans.csv",head=T)
df$tm<-as.Date.POSIXct(df$tm,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
y<-unique(substr(df$tm,1,4))
shinyUI(fluidPage(
  titlePanel("铁路合成景气指数"),
  
  fluidRow(
    column(6,selectInput(inputId="csvfile",
                         label="选择景气指数类别：",
                         choices=c("铁路运输合成景气指数","铁路规模合成景气指数","铁路设备合成景气指数"),
                         selected="铁路运输合成景气指数",
                         ))),
  fluidRow(
    column(2, checkboxInput(inputId="coord_Index",
                            label=strong("同步指数"),
                            value=TRUE)
    ),
    column(2, checkboxInput(inputId = "delay_Index",
                            label = strong("滞后指数"),
                            value = TRUE)
    ),
    column(2,checkboxInput(inputId="advanced_Index",
                           label=strong("先行指数"),
                           value=TRUE)
    ),
    column(2, selectInput(inputId = "year_start",
                          label = "自:", 
                          choices = y,
                          selected = min(y) )
    ),
    column(2,  selectInput(inputId="year_end",
                           label="至:",
                           choice=y,
                           selected=max(y) )
    )
  ),
  plotOutput(outputId = "main_plot", height = "400px"),
  fluidRow(
    DT::dataTableOutput("table")
  )
 
  )
 
) 
  
