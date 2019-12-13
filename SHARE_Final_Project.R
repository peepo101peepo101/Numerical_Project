###Final Project

library(dataRetrieval)
library(dplyr)

## Import csv excel files
#Winning_Times<-read.csv('Winning_Times_csv.csv',header=TRUE,sep=",")
#Dates_Excel<-read.csv('Dates_csv.csv',header=TRUE,sep=",")
library(rdrop2)
token<-drop_auth()
saveRDS(token,file="droptoken.rds")
#library(dplyr)
token<-readRDS("droptoken.rds")
drop_acc(dtoken=token)

Winning_Times<-read.csv(file="https://www.dropbox.com/s/40exspm2s0wkp1f/Winning_Times_csv.csv?dl=1",header=TRUE,sep=",")
Dates_Excel<-read.csv(file="https://www.dropbox.com/s/icdj2m6mlhit7y4/Dates_csv.csv?dl=1",header=TRUE,sep=",")


## Create vector of the yearly winning times for mens, mixed, and womens
Mns_Time<-(Winning_Times$Mns_hr+(Winning_Times$Mns_min/60)+(Winning_Times$Mns_sec/3600))
Mx_Time<-(Winning_Times$Mx_hr+(Winning_Times$Mx_min/60)+(Winning_Times$Mx_sec/3600))
Wms_Time<-(Winning_Times$Wms_hr+(Winning_Times$Wms_min/60)+(Winning_Times$Wms_sec/3600))

## Create a table of the race year and the mens, mixed, and womens winning times
Time_Table<-data.frame("Year"=1999:2019,"Mns_Time"=Mns_Time,"Mx_Time"=Mx_Time,"Wms_Time"=Wms_Time)

## Access the USGS Streamflow data for the AuSable River Basin
siteNumbers<-c("04135700","04136000", "04136500","04136900","04137005","04137500")
siteINFO <- readNWISsite(siteNumbers)
#comment(siteINFO)

## Edit the date excel csv to get into R date format
New_Dates<-as.Date(as.character(Dates_Excel$Date),"%Y%m%d")
Dates_Excel<-cbind(Dates_Excel,New_Dates)

## Here is our date range and streamflow discharge variables
startDate <- "1999-07-24"  
endDate <- "2019-07-28" 
parameterCd<- "00060"

## Create a Dataframe for each gauge
siteNumber_1<- "04135700"
SouthBranch_discharge<- readNWISdv(siteNumber_1,parameterCd,startDate,endDate)

## Then filter the results to keep only the two days the race occurrs for each year
for(i in 1:nrow(Dates_Excel)){
  for(j in i:nrow(SouthBranch_discharge)){
   if(SouthBranch_discharge[j,3]==Dates_Excel[i,2]){
    Dates_Excel[i,3]<-SouthBranch_discharge[j,4]
   }
  }
}
names(Dates_Excel)[3] <-"SouthBranch_2day"

## Then take the mean of those two days to create only one value for for each year
SouthBranch<-c(NA)
Time_Table<-cbind(Time_Table,SouthBranch)
for(i in 1:nrow(Time_Table)){
  Time_Table[i,5]<-(Dates_Excel[2*i-1,3]+Dates_Excel[2*i,3])/2
}

## Create a Dataframe for each gauge
siteNumber_2<- "04136000"
RedOak_discharge <- readNWISdv(siteNumber_2,parameterCd,startDate,endDate)

## Then filter the results to keep only the two days the race occurrs for each year
for(i in 1:nrow(Dates_Excel)){
  for(j in i:nrow(RedOak_discharge)){
    if(RedOak_discharge[j,3]==Dates_Excel[i,2]){
      Dates_Excel[i,4]<-RedOak_discharge[j,4]
    }
  }
}
names(Dates_Excel)[4] <-"RedOak_2day"

## Then take the mean of those two days to create only one value for for each year
RedOak<-c(NA)
Time_Table<-cbind(Time_Table,RedOak)
for(i in 1:nrow(Time_Table)){
  Time_Table[i,6]<-(Dates_Excel[2*i-1,4]+Dates_Excel[2*i,4])/2
}

## Create a Dataframe for each gauge
siteNumber_3<- "04136500"
Mio_discharge <- readNWISdv(siteNumber_3,parameterCd,startDate,endDate)

## Then filter the results to keep only the two days the race occurrs for each year
for(i in 1:nrow(Dates_Excel)){
  for(j in i:nrow(Mio_discharge)){
    if(Mio_discharge[j,3]==Dates_Excel[i,2]){
      Dates_Excel[i,5]<-Mio_discharge[j,4]
    }
  }
}
names(Dates_Excel)[5] <-"Mio_2day"

## Then take the mean of those two days to create only one value for for each year
Mio<-c(NA)
Time_Table<-cbind(Time_Table,Mio)
for(i in 1:nrow(Time_Table)){
  Time_Table[i,7]<-(Dates_Excel[2*i-1,5]+Dates_Excel[2*i,5])/2
}

## Create a Dataframe for each gauge
siteNumber_4<- "04136900"
McKinley_discharge <- readNWISdv(siteNumber_4,parameterCd,startDate,endDate)

## Then filter the results to keep only the two days the race occurrs for each year
for(i in 1:nrow(Dates_Excel)){
  for(j in i:nrow(McKinley_discharge)){
    if(McKinley_discharge[j,3]==Dates_Excel[i,2]){
      Dates_Excel[i,6]<-Mio_discharge[j,4]
    }
  }
}
names(Dates_Excel)[6] <-"McKinley_2day"

## Then take the mean of those two days to create only one value for for each year
McKinley<-c(NA)
Time_Table<-cbind(Time_Table,McKinley)
for(i in 1:nrow(Time_Table)){
  Time_Table[i,8]<-(Dates_Excel[2*i-1,6]+Dates_Excel[2*i,6])/2
}

## Create a Dataframe for each gauge
siteNumber_5<- "04137005"
Curtisville_discharge <- readNWISdv(siteNumber_5,parameterCd,startDate,endDate)

## Then filter the results to keep only the two days the race occurrs for each year
for(i in 1:nrow(Dates_Excel)){
  for(j in i:nrow(Curtisville_discharge)){
    if(Curtisville_discharge[j,3]==Dates_Excel[i,2]){
      Dates_Excel[i,7]<-Curtisville_discharge[j,4]
    }
  }
}
names(Dates_Excel)[7] <-"Curtisville_2day"

## Then take the mean of those two days to create only one value for for each year
Curtisville<-c(NA)
Time_Table<-cbind(Time_Table,Curtisville)
for(i in 1:nrow(Time_Table)){
  Time_Table[i,9]<-(Dates_Excel[2*i-1,7]+Dates_Excel[2*i,7])/2
}

## Create a Dataframe for each gauge
siteNumber_6<- "04137500"
AuSable_discharge <- readNWISdv(siteNumber_6,parameterCd,startDate,endDate)

## Then filter the results to keep only the two days the race occurrs for each year
for(i in 1:nrow(Dates_Excel)){
  for(j in i:nrow(AuSable_discharge)){
    if(AuSable_discharge[j,3]==Dates_Excel[i,2]){
      Dates_Excel[i,8]<-AuSable_discharge[j,4]
    }
  }
}
names(Dates_Excel)[8] <-"AuSable_2day"

## Then take the mean of those two days to create only one value for for each year
AuSable<-c(NA)
Time_Table<-cbind(Time_Table,AuSable)
for(i in 1:nrow(Time_Table)){
  Time_Table[i,10]<-(Dates_Excel[2*i-1,8]+Dates_Excel[2*i,8])/2
}

## Start of Shiny Application
library(shiny)
# Shiny Checkbox widget ui for classes
ui<-fluidPage(
  titlePanel("AuSable River Canoe Marathon VS Streamflow"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", label = h3("Year Range"), min = Time_Table$Year[1], max = Time_Table$Year[21], value = c(Time_Table$Year[1], Time_Table$Year[21]),step=1), hr(),fluidRow(column(1,verbatimTextOutput("Range"),div(style = "font-size: 10px; padding: 14px 0px; margin-bottom:-10em"))),
      
      radioButtons("radioClass", label = h3("Canoe Classes"), choices = list("Mens" = 1, "Mixed" = 2, "Womens" = 3,"Mens + Mixed"=4,"Mens + Womens"=5,"Mixed + Womens"=6,"All"=7), selected = 7), hr(), fluidRow(column(1,plotOutput("Class"),div(style = "font-size: 10px; padding: 14px 0px; margin-bottom:-90em"))),
      
      radioButtons("radioGauge", label = h3("Stream Gauges"), choices = list("South Branch AuSable River near Luzerne, MI" =1, "AuSable River near Red Oak, MI"=2, "AuSable River at Mio, MI" =3, "AuSable River near McKinley, MI" =4, "AuSable River near Curtisville, MI" =5, "AuSable River near AuSable, MI" =6), selected =6), hr(),fluidRow(column(1,plotOutput("Gauge"),div(style = "font-size: 10px; padding: 14px 0px; margin-top:-2em")))
      
    ), # ends sidebarPanel
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Plot",plotOutput(outputId = "main_plot")),
                  tabPanel("Table",tableOutput("table"))
      )
    ) # ends mainPanel
  ) # ends sidebarLayout
) # ends ui  


# Shiny Checkbox widget server logic required to create plots based off checkboxes inputs
server <- function(input, output,session) {
  output$table<-renderTable({
    Summary_Table<-as.data.frame(Time_Table)
    Summary_Table
  })
  
  output$main_plot <- renderPlot({
    
    if(input$radioClass==1){ # "Mens"
      par(mar=c(4, 5, 3, 5))
      par(oma=c(3, 4, 2, 4))
      ClassPlot1<-plot(Time_Table$Year,Time_Table$Mns_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="dodgerblue",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
      
    }
    
    if(input$radioClass==2){ # "Mixed"
      par(mar=c(4, 5, 3, 5))
      par(oma=c(3, 4, 2, 4))
      ClassPlot2<-plot(Time_Table$Year,Time_Table$Mx_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="lightslateblue",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
    }
    
    if(input$radioClass==3){ # "Womens"
      par(mar=c(4, 5, 3, 5))
      par(oma=c(3, 4, 2, 4))
      ClassPlot3<-plot(Time_Table$Year,Time_Table$Wms_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="green3",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
    }
    ######################
    if(input$radioClass==4){ # "Mens + Mixed"
      par(mar=c(4, 5, 3, 5))
      par(oma=c(3, 4, 2, 4))
      ClassPlot1<-plot(Time_Table$Year,Time_Table$Mns_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="dodgerblue",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
      
      par(new=TRUE)
      ClassPlot2<-plot(Time_Table$Year,Time_Table$Mx_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="lightslateblue",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
      
    }
    
    if(input$radioClass==5){ # "Mens + Womens"
      par(mar=c(4, 5, 3, 5))
      par(oma=c(3, 4, 2, 4))
      ClassPlot1<-plot(Time_Table$Year,Time_Table$Mns_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="dodgerblue",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
      
      par(new=TRUE)
      ClassPlot3<-plot(Time_Table$Year,Time_Table$Wms_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="green3",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
    }
    
    if(input$radioClass==6){ # "Mixed + Womens"
      par(mar=c(4, 5, 3, 5))
      par(oma=c(3, 4, 2, 4))
      ClassPlot2<-plot(Time_Table$Year,Time_Table$Mx_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="lightslateblue",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
      
      par(new=TRUE)
      ClassPlot3<-plot(Time_Table$Year,Time_Table$Wms_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="green3",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
    }
    
    if(input$radioClass==7){ # "All"
      par(mar=c(4, 5, 3, 5))
      par(oma=c(3, 4, 2, 4))
      ClassPlot1<-plot(Time_Table$Year,Time_Table$Mns_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="dodgerblue",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
      
      par(new=TRUE)
      ClassPlot2<-plot(Time_Table$Year,Time_Table$Mx_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="lightslateblue",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
      
      par(new=TRUE)
      ClassPlot3<-plot(Time_Table$Year,Time_Table$Wms_Time,main=c("AuSable River Canoe Marathon:\nWinning Race Times VS River Flow"),col="green3",pch=16,type="b",axes=FALSE,xlim=c(input$slider[1],input$slider[2]),ylim=c(14,20),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
      axis(2,ylim=c(14,20),col="black",col.axis="black",las=1,at=c(seq(from=14,to=20,by=0.5)))
      mtext("Winning Time (hrs)",side=2,line=3)
    }
    
    if(input$radioGauge==1){
      par(new=TRUE)
      GaugePlot<-plot(Time_Table$Year,Time_Table$SouthBranch, main="AuSable River Canoe Marathon:\nWinning Race Times VS River Flow",col="red",pch=2,type="b",axes=FALSE, xlim=c(input$slider),ylim=c(0,2000),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
    }
    if(input$radioGauge==2){
      par(new=TRUE)
      GaugePlot<-plot(Time_Table$Year,Time_Table$RedOak, main="AuSable River Canoe Marathon:\nWinning Race Times VS River Flow",col="red",pch=3,type="b",axes=FALSE, xlim=c(input$slider),ylim=c(0,2000),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
    }
    if(input$radioGauge==3){
      par(new=TRUE)
      GaugePlot<-plot(Time_Table$Year,Time_Table$Mio, main="AuSable River Canoe Marathon:\nWinning Race Times VS River Flow",col="red",pch=4,type="b",axes=FALSE, xlim=c(input$slider),ylim=c(0,2000),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
    }
    if(input$radioGauge==4){
      par(new=TRUE)
      GaugePlot<-plot(Time_Table$Year,Time_Table$McKinley, main="AuSable River Canoe Marathon:\nWinning Race Times VS River Flow",col="red",pch=5,type="b",axes=FALSE, xlim=c(input$slider),ylim=c(0,2000),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
    }
    if(input$radioGauge==5){
      par(new=TRUE)
      GaugePlot<-plot(Time_Table$Year,Time_Table$Curtisville, main="AuSable River Canoe Marathon:\nWinning Race Times VS River Flow",col="red",pch=6,type="b",axes=FALSE, xlim=c(input$slider),ylim=c(0,2000),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
    }
    if(input$radioGauge==6){
      par(new=TRUE)
      GaugePlot<-plot(Time_Table$Year,Time_Table$AuSable, main="AuSable River Canoe Marathon:\nWinning Race Times VS River Flow",col="red",pch=7,type="b",axes=FALSE, xlim=c(input$slider),ylim=c(0,2000),xlab="",ylab="",cex.main=1.3,cex.lab=0.7)
    }
    
    #################
    #ClassPlot
    
    par(new=TRUE)
    
    axis(1,xlim=c(input$slider[1],input$slider[2]),col.axis="black",las=2,cex.axis=0.8,at=c(seq(from=input$slider[1],to=input$slider[2],by=1)))
    mtext("Year",side=1,line=3)
    
    #GaugePlot
    axis(side=4,ylim=c(0,2000),col="red",col.axis="red",las=1,at=c(seq(from=0,to=2000,by=200)))
    mtext("Streamflow (cfs)",side=4,col="red",line=3)
    
    ## Add Legend
    # setup for no margins on the legend
    par(mar=c(0, 0, 0, 0))
    legend("top",legend=c("Mens Class","Mixed Class","Womens Class","South Branch", "Red Oak", "Mio", "McKinley", "Curtisville", "AuSable"),text.col=c("dodgerblue","lightslateblue","green3","red","red","red","red","red","red"),pch=c(16,16,16,2,3,4,5,6,7),col=c("dodgerblue","lightslateblue","green3","red","red","red","red","red","red"),ncol=3,bty="n")
    
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)