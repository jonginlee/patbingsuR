
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
    
    tmp_x<-createIntegralGraph(window_data$time, window_data$x,paste("x-axis",start_milli,"~",end_milli),type = "loc")
    tmp_y<-createIntegralGraph(window_data$time, window_data$y,paste("y-axis",start_milli,"~",end_milli),type = "loc")
    tmp_z<-createIntegralGraph(window_data$time, window_data$z,paste("z-axis",start_milli,"~",end_milli),type = "loc")      
    
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

forderlist <- c("jonginlee_data0623")
forderlist <- c("junhong_data0623")


forderlist <- c(
  "jonginlee_data0623",
  "junhong_data0623")



getDataN <- function(forderlist,ranglist)
{
  windata <- list()
  fn_index <- 0
  
  for(k in 0:(as.integer(nrow(ranglist)/7)-1))
  {
    rangitem <- ranglist[c((7*k+1):(7*k+7) ),]
    print(rangitem)
    index <- k%%4 + 1
    if(index==1){
      fn_index <- fn_index + 1
      fordername <- forderlist[fn_index]
      print(paste("===================> fordername",fordername))
    }
    
    win1 <- getFeaturesFromRanges(fordername, paste("data",index,sep=""), rangitem, c(1,3,8))
    
    if(k==0)
      windata <- win1
    else{
      windata <- rbind(win1, windata)
    }
    
  }
    
  View(windata)
  return (windata)
}


getFeaturesFromRanges <- function(foldername, filename, ranglist, sensor_indexes)
{
  data <- read.table(paste("./data_raw/",foldername, "/", filename ,".txt",sep=""),sep=",",header=TRUE)
#  res<-createPlot(data, idx, foldername,saveFile = FALSE)
  
  resdata <- list()
  windata <- list()
  for(i in 1:nrow(ranglist))
  {
    startT <- ranglist$scr_start[i]
    endT <- ranglist$scr_end[i]
    print(paste(i, startT, endT))
    #View(data)
    
    data.sub <- data
    data.sub <- subset(data.sub, subset=(data.sub$time > as.integer(startT) ))
    data.sub <- subset(data.sub, subset=(data.sub$time < as.integer(endT) ))
    
    data.sub$label <- i
    resdata <- rbind(resdata, data.sub)
    #View(data.sub)
    
    for(k in sensor_indexes)
    {
      idx <- k
      win<-doSimulationAllFeatures(data.sub, FALSE, idx, 150, 50, FALSE, plotting = TRUE, thresholdvar = 0.01 )
      row.names(win) <- NULL
      win <- as.data.frame(win)
      print(paste("win len", length(win)))
      #View(win)
      if(k=='1'){
        sum_data <- win[,4:length(win)]
      }else{
        sum_data <- cbind(sum_data,win[,5:length(win)])
      }
      
      print(paste("index : ",k, "len : ",nrow(win)))
      
    }
    
    #win<-doSimulationAllFeatures(data.sub, FALSE, idx, 150, 50, FALSE, plotting = TRUE, thresholdvar = 0.01 )
    #sum_data <- as.data.frame(sum_data)
    sum_data$label <- "scratch"
    #View(sum_data)
    
    print(paste("----- index : ",i, "len : ",nrow(sum_data), length(sum_data)))
    
    if(i==1)
      windata <- sum_data
    else
      windata <- rbind(windata, sum_data)

    #print(paste("windata nrow", nrow(windata)))
    
  }
  
  View(resdata)
  View(windata)
  return(windata)
  
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
