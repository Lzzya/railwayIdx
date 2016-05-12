library(shiny)
library(ggplot2)
df<-read.csv("compidx-trans.csv",head=T)
df$mon<-as.Date.POSIXct(df$mon,"%Y-%m-%d",tz=Sys.timezone(location = TRUE))  #转化为日期型数据
y<-unique(substr(df$mon,1,4))
shinyUI(fluidPage(
  titlePanel("铁路运输合成景气指数"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year_start",
                  label = "选择起始年份:",
                  choices = y,
                  selected = min(y) ),
      selectInput(inputId="year_end",
                  label="选择结束年份:",
                  choice=y,
                  selected=max(y) ),
      
      checkboxInput(inputId="coord_Index",
                    label=strong("同步指数"),
                    value=TRUE),
      checkboxInput(inputId = "delay_Index",
                    label = strong("滞后指数"),
                    value = TRUE),
      checkboxInput(inputId="advanced_Index",
                    label=strong("先行指数"),
                    value=TRUE)
    ),
    mainPanel(
     plotOutput(outputId = "main_plot", height = "500px")
     
     )
  
  )
 
  )
  
)