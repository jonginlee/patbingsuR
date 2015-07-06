
folderlist <- c("jonginlee_data0623")
folderlist <- c("junhong_data0623")


folderlist <- c(
  "jonginlee_data0623",
  "junhong_data0623",
  "chang",
  "soku",
  "eunji",
  "mins",
  "seungho"
)


ranglist <- read.csv("dataset.csv")
#ranglist <-  lapply(ranglist, as.numeric)



getDataN <- function(folderlist,ranglist)
{
  windata <- list()
  fn_index <- 0
  
  for(k in 0:(as.integer(length(ranglist$mv_start)/7)-1))
  {
    rangitem <- ranglist[c((7*k+1):(7*k+7) ),]
    print(rangitem)
    index <- k%%4 + 1
    if(index==1){
      fn_index <- fn_index + 1
      foldername <- folderlist[fn_index]
      print(paste("===================> foldername",foldername))
    }
    
    win1 <- getFeaturesFromRanges(foldername, paste("data",index,sep=""), rangitem, c(1,3,8))
    win1$p <- fn_index
    win1$scrtype <- index
    
    if(k==0)
      windata <- win1
    else{
      windata <- rbind(windata,win1)
    }
    
  }
  
  View(windata)
  return (windata)
}


getFeaturesFromRanges <- function(foldername, filename, ranglist, sensor_indexes)
{
  data <- read.table(paste("./data_raw/",foldername, "/", filename ,".txt",sep=""),sep=",",header=TRUE)
  #  res <- createPlot(data, idx, foldername,saveFile = FALSE)
  #  print(paste("data nrow1 ", nrow(data)))  
  
  resdata <- list()
  windata <- list()
  for(i in 1:nrow(ranglist))
  {
    startT <- as.integer(as.character(ranglist$scr_start[i]))
    endT <- as.integer(as.character(ranglist$scr_end[i]))
    print(paste(i, startT, endT))
    #View(data)
    
    data.sub <- data
    data.sub <- subset(data.sub, subset=(data.sub$time > as.integer(startT) ))
    data.sub <- subset(data.sub, subset=(data.sub$time < as.integer(endT) ))
    
    #data.sub$label[c(1:nrow(data.sub))] <- i
    
    #data.sub$label <- i
    resdata <- rbind(resdata, data.sub)
    #View(data.sub)
    
    for(k in sensor_indexes)
    {
      print(paste("sensor ",list[k],"=======>" ))
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



for(i in c(1,3,8))
{
  tt<- getDataset2(NULL, c(
    "data5",
    "data6"
  ),i, 150, 50, TRUE, NULL)
  
  if(i=='1'){
    sum_data <- tt[,5:length(tt)]
  }else{
    sum_data <- c(sum_data,tt[,6:length(tt)])
  }
  
  print(paste("index : ",i, "len : ",nrow(tt) ))
  
}

