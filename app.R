#install the unavailable library and load the library
packages = c("shiny",'plyr',
             "scales","tidyverse","readxl","lubridate",'DT','zoo','plotly','shinythemes')
## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

library(shiny)
library(plyr)
library(scales)
library(tidyverse)
library(readxl)
library(lubridate)
library(DT)
library(zoo)
library(plotly)
library(shinythemes)

df<-read_excel('PCOR File.xlsx')
#remove nulls
df_complete <- df[is.na(df$MarginStatus)==FALSE&is.na(df$Stage)==FALSE,]
#standardise cancer stage
df_complete$Cancer_stage=substr(df_complete$Stage, 1, 2)
#format date data
df_complete$SurgeryDt=format(as.Date(df_complete$SurgeryDt), "%Y-%m-%d")
df_complete$Month<-format(as.Date(df_complete$SurgeryDt),'%m')
df_complete<-df_complete %>%
  mutate(Quarter = paste0("Q",quarter(SurgeryDt)))
df_complete$Year<-format(as.Date(df_complete$SurgeryDt),'%Y')

group.colour<-c(Mo='#FF0000',Bo='#99FFFF',Zo='#FF99FF',Al='#CCFFCC',Cy='#FFCC66',Di='009933',Ed='#CCCC99',Oz='663366',Ti='#3300FF',Vi='#FF8000')

#the function helps to calculate the monthly/quarterly/yearly percentage positive margin.
group_data <- function(df,groupby) {
  if (groupby=='Monthly'){
    df1<-df %>% group_by(Year,Month,Cancer_stage,Surgeon)%>%tally()
    df2<-df[df$MarginStatus==0,] %>% group_by(Year,Month,Cancer_stage,Surgeon)%>%count()
    names(df2)[5]<-paste('count_0')
  }
  if (groupby=='Quarterly'){
    df1<-df %>% group_by(Year,Quarter,Cancer_stage,Surgeon)%>%tally()
    df2<-df[df$MarginStatus==0,] %>% group_by(Year,Quarter,Cancer_stage,Surgeon)%>%count()
    names(df2)[5]<-paste('count_0')
  }
  
  if (groupby=='Yearly'){
    df1<-df %>% group_by(Year,Cancer_stage,Surgeon)%>%tally()
    df2<-df[df$MarginStatus==0,] %>% group_by(Year,Cancer_stage,Surgeon)%>%count()
    names(df2)[4]<-paste('count_0')
  }
  
  df3<-join(df1,df2,type='left')
  df3$count_0[is.na(df3$count_0)] = 0
  df3$count_0=as.integer(df3$count_0)
  df3$count_0[is.na(df3$count_0)] = 0
  df3$count_0=as.integer(df3$count_0)
  df3$count_1<-df3$n-df3$count_0
  df3$Perc_Positive_Margin<-paste0(round((df3$count_1/df3$n)*100,3),"%")
  df3$Perc<-round((df3$count_1/df3$n)*100,3)
  df3$Surgery_count=df3$n
  return(df3)
}




ui <- fluidPage(
  
  theme = shinytheme("paper"),
  h1("2018-2020 PCOR Doctors Performance", align = "center"),
  fluidRow( column(3,
    selectInput("timeint", "Time Interval:",
                choices = c("Monthly", "Quarterly", "Yearly"),selected = 'Quarterly')),
    column(3,dateRangeInput("daterange", "Date Range:",
                            start  = min(df_complete$SurgeryDt),
                            end    = max(df_complete$SurgeryDt),
                            min    = min(df_complete$SurgeryDt),
                            max    = max(df_complete$SurgeryDt),
                            format = "yyyy-mm-dd",
                            separator = " - ")),
    column(3,selectInput("doctors", "Comparing Dr Mo's Performance To:",
                choices = c("Al", "Bo", "Cy","Di","Ed","Oz","Ti","Vi","Zo"),multiple=TRUE,selected = c("Al", "Bo", "Cy","Di","Ed","Oz","Ti","Vi","Zo"))),
    column(3,selectInput("stage", "Cancer Stage:", choices = c("T2", "T3"),selected = 'T2'))
    
    ),tabsetPanel(type = "tabs",
                  tabPanel("Plot",h3("The Line Graph Of Percentage Margin Status For Doctors",align = "center"), plotlyOutput("plot",height = 550)),
                  tabPanel("Table", 
                           h3("The Table Of Percentage Margin Status For Doctors",align = "center"),
                           DTOutput("results")))
  )

server <- function(input, output) {
 
  
   output$results <- renderDT({
     
    
    df1<-df_complete[df_complete['Cancer_stage']==input$stage,] %>% filter(SurgeryDt>=input$daterange[1]) %>% filter(SurgeryDt<=input$daterange[2])
    if(input$timeint=='Quarterly'){
      a=c('Year','Quarter','Cancer_stage','Surgeon','Perc_Positive_Margin') 
      b=c('Year','Quarter','Cancer_stage','Mo')}
    if(input$timeint=='Monthly'){
      a=c('Year','Month','Cancer_stage','Surgeon','Perc_Positive_Margin') 
      b=c('Year','Month','Cancer_stage','Mo')}
    if(input$timeint=='Yearly'){
      a=c('Year','Cancer_stage','Surgeon','Perc_Positive_Margin') 
      b=c('Year','Cancer_stage','Mo')}
    
    df2<-group_data(df1,input$timeint)%>% select(a)
    df2 %>% spread('Surgeon','Perc_Positive_Margin', fill = NA, convert = FALSE)%>% select(c(b,input$doctors))%>%datatable(rownames=FALSE)%>%formatStyle(columns = "Mo",backgroundColor='grey')
      })
   output$plot<-renderPlotly({
     df1<-df_complete[df_complete['Cancer_stage']==input$stage,] %>% filter(SurgeryDt>=input$daterange[1]) %>% filter(SurgeryDt<=input$daterange[2])
     df2<-group_data(df1,input$timeint)
     df2<-df2 %>% filter(Surgeon %in% c('Mo',input$doctors))
                         
    if (input$timeint=='Monthly'){
      df2<-df2%>% unite('Date', c(Year, Month), sep = " ", remove = FALSE)
      df2$Date=as.Date(as.yearmon(df2$Date, format = "%Y %m"))}
if(input$timeint=='Quarterly'){
  df2<-df2%>% unite('Date', c(Year, Quarter), sep = " ", remove = FALSE)
  df2$Date=as.yearqtr(df2$Date, format = "%Y Q%q")} 
if(input$timeint=='Yearly'){
  df2$Date=as.Date(paste(df2$Year,01,01,sep="-"))}
  
     p <- ggplot(data=subset(df2,Surgeon=='Mo'), aes(x=Date, y=Perc,group=Surgeon,colour=Surgeon,text=paste("Cancer Stage:",Cancer_stage,'<br>Surgery Count:',Surgery_count))) +
       theme_bw()+theme(
         axis.title.x = element_text(size = 12),
         axis.text.x = element_text(size = 10,angle=45),
         axis.text.y = element_text(size = 10,angle=30),
         axis.title.y = element_text(size = 12),
         legend.title = element_text( size = 11),
         legend.text = element_text(size = 9))+scale_color_manual(values=group.colour) + ylab("Percentage of the Positive margin(%)") 
  if(length(input$doctors)>0){
       p<-p+geom_line(data=subset(df2,Surgeon!='Mo'), size=0.8,linetype = "dotdash")+geom_point(data=subset(df2,Surgeon!='Mo'),size=1)
       }
       p<-p+geom_line(size=1.5)+geom_point(size=2.5)
     if (input$timeint=='Monthly'){
       p<-p+scale_x_date(breaks="2 months", labels=date_format("%Y-%b"),minor_breaks='1 month')+  xlab("Month Year")}
     if(input$timeint=='Quarterly'){
       p<-p+scale_x_yearqtr(format = "%Y Q%q",breaks=unique(df2$Date)) + xlab("Quarter Year") } 
     if(input$timeint=='Yearly'){
       p<-p+scale_x_date(breaks="1 year", labels=date_format("%Y"))+ xlab("Year")}
     
     
     
     p %>% ggplotly(tooltip = c('x','y','group','text'))
     
   })}
shinyApp(ui = ui, server = server)
