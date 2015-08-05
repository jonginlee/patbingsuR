
foldername <- "jonginlee_data0623"

filelist <- c(
  "data_watch_intentservice1",
  "data_watch_intentservice2",
  "data_watch_intentservice3",
  "data_watch_intentservice4"
)

# data_watch_intentservice1
ranglist <- c(
  "9728", "17100",
  "28662", "36114",
  "48582", "57454",
  "69166", "78633", 
  "92249", "104132",
  "115915", "124012",
  "135130", "144073"
  )

# , , , , , , 
sitelist <- c("head", "neck", "chest", "right arm", "right wrist", "left thigh", "right thigh")


getMotionFromRanges <- function(foldername, filename, ranglist, idx)
{
  data <- read.table(paste("./data_raw/",foldername, "/", filename ,".txt",sep=""),sep=",",header=TRUE)
  res<-createPlot(data, idx, foldername,saveFile = FALSE)
  
  resdata <- list()
  windata <- list()
  for(i in 1:as.integer(length(ranglist)/3))
  {
    startT <- ranglist[3*i-2]
    endT <- ranglist[3*i-1]
    print(paste(i, startT, endT))
    
    data.sub <- data
    data.sub <- subset(data.sub, subset=(data.sub$time > as.integer(startT) ))
    data.sub <- subset(data.sub, subset=(data.sub$time < as.integer(endT) ))
    
    data.sub <- subset(data.sub,grepl(list[idx], data.sub$type))
    window_data <- as.data.frame(data.sub)
    
    tmp_x<-createIntegralGraph(window_data$time, window_data$x,paste("x-axis"),type = "vel",plotting = TRUE)
    tmp_y<-createIntegralGraph(window_data$time, window_data$y,paste("y-axis"),type = "vel",plotting = TRUE)
    tmp_z<-createIntegralGraph(window_data$time, window_data$z,paste("z-axis"),type = "vel",plotting = TRUE)      
    
    dis <- sqrt(tmp_x[length(tmp_x)]^2 + tmp_y[length(tmp_y)]^2 + tmp_z[length(tmp_z)]^2)
    print(paste("x:",tmp_x[length(tmp_x)],"y:",tmp_y[length(tmp_y)],"z:",tmp_z[length(tmp_z)], "dis",sum(dis) ))
    
    scatterplot3d(tmp_x,tmp_y,tmp_z, pch=16, highlight.3d=TRUE,type="h", main=paste(sitelist[i],i))
    
    resdata <- rbind(resdata, data.sub)
    
    #View(data.sub)
    #break
    
    #win<-doSimulationAllFeatures(data.sub, FALSE, idx, 150, 50, FALSE, plotting = TRUE, thresholdvar = 0.01 )
    #windata <- rbind(windata, win)
    
    #View(win)
  }
  
  #View(resdata)
  return(resdata)
}

resdata<-getFeaturesFromRanges("jonginlee_data0623", "data_watch_intentservice1", ranglist1, 8)


createIntegralGraph<-function(x,y,graph_title,type="vel",plotting=FALSE)
{
  if(type=="vel")
  {
    new_y <- 0
    for(i in 1:length(y)){
      sum <- 0
      for(j in 1:i){
        sum <- sum + round(y[j],digits=2)
      }
      new_y[i] <- sum
    }

    ylabel <- "Velocity (m/s)"
    
    
    #View(new_y)
    #if( new_y[length(y)] != 0)
    #  print(paste("b new_y",new_y[length(y)]))
    
    offset <- new_y[length(y)]
    #print(paste("offset ",offset,"length(y)",length(y),"offset/length(y)",offset/length(y)))
    
    toffset <-  offset/length(y) 
    for(i in 1:length(y)){
#      if(new_y[i] < 0)
        new_y[i] <- new_y[i] - i*toffset
#      else if(new_y[i] > 0)
#        new_y[i] <- new_y[i] - toffset
    }
   
    #print(paste("a new_y",new_y[length(y)]))

    res<-new_y
    
  }else if(type=="loc")
  {
    new_y <- 0
    for(i in 1:length(y)){
      sum <- 0
      for(j in 1:i){
        sum <- sum + round(y[j],digits=2)
      }
      new_y[i] <- sum
    }
    
    offset <- new_y[length(y)]
    toffset <-  offset/length(y) 
    for(i in 1:length(y)){
      new_y[i] <- new_y[i] - i*toffset
    }
    

    
    new_yy <- 0
    for(i in 1:length(new_y)){
      sum <- 0
      for(j in 1:i){
        sum <- sum + round(new_y[j],digits=2)
      }
      new_yy[i] <- sum
    }
    res<-new_yy
    ylabel <- "distance (m/s^2)"
  }
  
  
  if(plotting){
    returnValue <- qplot(x,res, geom=c("line","point") ) + ggtitle(paste(graph_title," - (",sensor_name_list[idx],")")) + 
      ylab(ylabel) +
      xlab("time") +  theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"))
    
    print(returnValue)
  }
  
  return (res)
}

nonlist<- c(
  "scratching_data0521(2_45_02)",
  "scratching_data0521(3_07_46)",
  "scratching_data0521(3_38_20)",
  "scratching_data0521(4_40_18)",
  "scratching_data0521(4_53_23)",
  "scratching_data0521(5_12_04)",
  "scratching_data0521(5_23_31)",
  "scratching_data0521(5_42_37)",
  "scratching_data0521(6_47_09)",
  "scratching_data0521(2_07_47)",
  "scratching_data0521(2_12_43)"
)


getFeaturesFromNons <- function(nonlist)
{
  windata <- list()
  resdata <- list()
  for(i in 1:length(nonlist) )
  {
    filename <- nonlist[i]
    data <- read.table(paste("./data_csv/",filename,".csv",sep=""),sep=",",header=TRUE)
    resdata <- rbind(resdata,data)
    #win<-doSimulationAllFeatures(data, FALSE, idx, 150, 50, FALSE, plotting = TRUE, thresholdvar = 0.01 )
    #win <- as.data.frame(win)
    #win$label <- "non_scratch"
    #windata <- rbind(windata, win)
    
  }
  
  return(resdata)
}

filelist <- c(
  "", "9728",
  "data_watch_intentservice2",
  "data_watch_intentservice3",
  "data_watch_intentservice4"
)

fordername <- "junhong_data0623"

filelist <- c(
  "data_watch_intentservice1",
  "data_watch_intentservice3",
  "data_watch_intentservice4",
  "data_watch_intentservice5"
  )


filelist <- c(
"data1",
"data2",
"data3",
"data4"
)

filelist <- c(
  "data1",
  "data2"
)


py <- plotly("jjonginlee", "1ff4u64c19")
uploadIntoPlotly("jonginlee_data0623", filelist, 3)

uploadIntoPlotly <- function(foldername, filelist, idx)
{
  
  for(i in 1: length(filelist))
  {
    filename <- filelist[i]
    data <- read.table(paste("./data_raw/",foldername, "/", filename ,".txt",sep=""),sep=",",header=TRUE)
    res<-createPlot(data, idx, paste(fordername, i) ,saveFile = FALSE)
    py$ggplotly(res, session="knitr")
  }

}

py <- plotly("jjonginlee", "1ff4u64c19")
uploadIntoPlotly("jonginlee_data0623", filelist, 3)
uploadPlotlyWith<- function(data, title, idx, set_btw = FALSE, startMilli = 0.1, endMilli = 0.1)
{
    res<-createPlot(data, idx, paste(title) ,saveFile = FALSE,start_hour = startMilli, end_hour = endMilli)
    py$ggplotly(res, session="knitr")
}


jonginlee0623_4 <- read.table("./data_raw/jonginlee_data0623//data_watch_intentservice4.txt",sep=",",header=TRUE)
res<-createPlot(jonginlee0623_4, 1, "jonginlee0623_1",saveFile = FALSE)
#py$ggplotly(res, session="knitr")

jonginlee0623_3 <- read.table("./data_raw/jonginlee_data0623//data_watch_intentservice3.txt",sep=",",header=TRUE)
res<-createPlot(jonginlee0623_3, 1, "jonginlee0623_1",saveFile = FALSE)

jonginlee0623_2 <- read.table("./data_raw/jonginlee_data0623//data_watch_intentservice2.txt",sep=",",header=TRUE)
res<-createPlot(jonginlee0623_2, 1, "jonginlee0623_1",saveFile = FALSE)

jonginlee0623_1 <- read.table("./data_raw/jonginlee_data0623//data_watch_intentservice1.txt",sep=",",header=TRUE)
res<-createPlot(jonginlee0623_1, 1, "jonginlee0623_1",saveFile = FALSE)


junhong_data0623_4 <- read.table("./data_raw/junhong_data0623//data_watch_intentservice4.txt",sep=",",header=TRUE)
res<-createPlot(junhong_data0623_4, 1, "junhong_data0623_4",saveFile = FALSE)
#py$ggplotly(res, session="knitr")

junhong_data0623_3 <- read.table("./data_raw/junhong_data0623//data_watch_intentservice3.txt",sep=",",header=TRUE)
res<-createPlot(junhong_data0623_3, 1, "junhong_data0623_3",saveFile = FALSE)

junhong_data0623_2 <- read.table("./data_raw/junhong_data0623//data_watch_intentservice2.txt",sep=",",header=TRUE)
res<-createPlot(junhong_data0623_2, 1, "junhong_data0623_2",saveFile = FALSE)

junhong_data0623_1 <- read.table("./data_raw/junhong_data0623//data_watch_intentservice1.txt",sep=",",header=TRUE)
res<-createPlot(junhong_data0623_1, 1, "junhong_data0623_!",saveFile = FALSE)






data_watch_intentservice_0619_1_2_2e <- read.table("./data_raw/test0619_1_2/data_watch_intentservice2.txt",sep=",",header=TRUE)
data_watch_intentservice_0619_1_2e <- read.table("./data_raw/test0619_1/data_watch_intentservice2.txt",sep=",",header=TRUE)


filelist<-c(
  "test0619_1_2/data_watch_intentservice1.txt",
  "test0619_1_2/data_watch_intentservice2.txt"
  )

x = c(5,
      7,
      5,
      3,
      6,
      6,
      5,
      7)

t.test(x, alternative="greater", mu=4)
