load("~/.RData")
subject1_1$time[length(subject1_1$time)]
subject1_left <- read.table("./data_raw/subject1_left_1.txt",sep=",",header=TRUE)
subject1_left$time[length(subject1_left$time)]
timee <- subject1_left$time[length(subject1_left$time)]


subject1_1 <- read.table("./data_raw/subject1_1.txt",sep=",",header=TRUE)
timee <- subject1_1$time[length(subject1_1$time)]
timee/4
subject1_1_sub <- subset(subject1_1, subset=(subject1_1$time <  timee/4 ))
t<-doSimulationAllFeatures(subject1_1_sub, TRUE, 8, 150, 50, "subject1_1_sub", TRUE,delay=1,startMilli = 15*1000,endMilli = 0*1000, thresholdvar = 0.1)


library("caret", lib.loc="/usr/local/lib/R/site-library")
t<-doSimulationAllFeatures(subject1_1_sub, TRUE, 8, 150, 50, "subject1_1_sub", TRUE,delay=1,startMilli = 15*1000,endMilli = 0*1000, thresholdvar = 0.1)


getAverageDistances <- function(folderlist,ranglist,idx)
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
    
    win1 <- getDistance(foldername, paste("data",index,sep=""), rangitem, idx)
    #win1$p <- fn_index
    #win1$scrtype <- index
    
    if(k==0)
      windata <- win1
    else{
      windata <- rbind(windata,win1)
    }
    
  }
  
  View(windata)
  return (windata)
}



getDistance <- function(foldername, filename, ranglist, index)
{
  data <- read.table(paste("./data_raw/",foldername, "/", filename ,".txt",sep=""),sep=",",header=TRUE)
  #  res <- createPlot(data, idx, foldername,saveFile = FALSE)
  #  print(paste("data nrow1 ", nrow(data)))  
  
  resdata <- list()
  for(i in 1:nrow(ranglist))
  {
    startT <- as.integer(as.character(ranglist$mv_start[i]))
    endT <- as.integer(as.character(ranglist$scr_start[i]))
    #print(paste(i, startT, endT))
    #View(data)
    
    data.sub <- data
    data.sub <- subset(data.sub, subset=(data.sub$time > as.integer(startT) ))
    data.sub <- subset(data.sub, subset=(data.sub$time < as.integer(endT) ))
    
    if(index==3)
    {
      data.sub <- subset(data.sub,grepl(list[index], data.sub$type))
      dis <- sqrt(sum(data.sub$x)^2 + sum(data.sub$y)^2 + sum(data.sub$z)^2)
      
      resdata$Xdis[i] <- sum(data.sub$x)
      resdata$Ydis[i] <- sum(data.sub$y)
      resdata$Zdis[i] <- sum(data.sub$z)
      resdata$total[i] <- dis
      
    }else{
      data.sub <- subset(data.sub,grepl(list[index], data.sub$type))
      
      tmp_x <- createIntegralGraph(data.sub$time, data.sub$x, paste("x-axis"),type = "loc",plotting = FALSE)
      tmp_y <- createIntegralGraph(data.sub$time, data.sub$y, paste("y-axis"),type = "loc",plotting = FALSE)
      tmp_z <- createIntegralGraph(data.sub$time, data.sub$z, paste("z-axis"),type = "loc",plotting = FALSE)
      
      dis <- sqrt(tmp_x[length(tmp_x)]^2 + tmp_y[length(tmp_y)]^2 + tmp_z[length(tmp_z)]^2)
      
      print(paste("x:",tmp_x[length(tmp_x)],"y:",tmp_y[length(tmp_y)],"z:",tmp_z[length(tmp_z)], "dis",sum(dis) ))     
      scatterplot3d(tmp_x,tmp_y,tmp_z, pch=16, highlight.3d=TRUE,type="h", main=paste(sitelist[i],i))
      
      
      resdata$Xdis[i] <- tmp_x[length(tmp_x)]
      resdata$Ydis[i] <- tmp_y[length(tmp_y)]
      resdata$Zdis[i] <- tmp_z[length(tmp_z)]
      resdata$total[i] <- dis
    }
  }
  
  print(resdata)
  return(resdata)
  
}


folderlist <- c("jonginlee_data0623")
folderlist <- c("junhong_data0623")


folderlist <- c(
  "jonginlee_data0623",
  "junhong_data0623",
  "soku",
  "eunji",
  "mins",
  "seungho",
  "chul"
)

ranglist <- read.csv("dataset_2.csv")


length(ranglist$mv_start)

#ranglist <-  lapply(ranglist, as.numeric)

data <- getDataN(folderlist, ranglist)

sim_scr_data0805 <- data
sim_scr_data0805[c(2:length(sim_scr_data0805))] <-  lapply((sim_scr_data0805[c(2:length(sim_scr_data0805))]), as.numeric)
sim_nonscr_data0805 <- sum_data
View(sum_data[5:6])



saveFairTrainData(sim_scr_data0805, sim_nonscr_data0805[5:length(sim_nonscr_data0805)], "sim_data0805")

saveFairTrainData(sim_scr_data0805, sim_nonscr_data0805[5:length(sim_nonscr_data0805)], "sim_data0805")


saveFairTrainData<-function(scr_data, nonscr_data, savefile)
{
  scr_data.sub <-scr_data[sample(length(scr_data$label), length(nonscr_data$label)), ]
  scr_data.sub$scrtype <- NULL
  scr_data.sub$p <- NULL
  nonscr_data<-as.data.frame(nonscr_data)
  #colnames() <- colnames()
  colnames(scr_data.sub) <- colnames(nonscr_data)
  
  #length(sim_nonscr_data0803)
  #length(sim_scr_data0803.sub)
  fair_data <- rbind(scr_data.sub, nonscr_data)
  #View(sim_nonscr_scr_data0803_far)
  fair_data$label <- as.factor(fair_data$label)
  write.arff(fair_data, file=paste("./data_csv/",savefile,".arff",sep=""), relation = savefile)
}

sim_scr_data0803 <- data
sim_scr_data0803[c(2:length(sim_scr_data0803))] <-  lapply((sim_scr_data0803[c(2:length(sim_scr_data0803))]), as.numeric)

sim_nonscr_data0806 <- sum_data
sim_scr_data0806 <- data

sim_scr_data0803.sub <-sim_scr_data0803[sample(length(sim_scr_data0803$label), length(sim_nonscr_data0803$label)), ]
sim_scr_data0803.sub$scrtype <- NULL
sim_scr_data0803.sub$p <- NULL
sim_nonscr_data0803<-as.data.frame(sim_nonscr_data0803)
colnames(sim_nonscr_data0803) <- colnames(sim_scr_data0803.sub)
#length(sim_nonscr_data0803)
#length(sim_scr_data0803.sub)
sim_nonscr_scr_data0803_far <- rbind(sim_scr_data0803.sub,sim_nonscr_data0803)
#View(sim_nonscr_scr_data0803_far)
sim_nonscr_scr_data0803_far$label <- as.factor(sim_nonscr_scr_data0803_far$label)
write.arff(sim_nonscr_scr_data0803_far, file=paste("./data_csv/sim_nonscr_scr_data0803_fair.arff",sep=""), relation = "0803_scr_nonscr")



length(sim_scr_data0803.sub_fair$label)
length(sim_nonscr_data0803.sub_fair$label)
sim_nonscr_data0803.sub_fair <- sim_nonscr_data0803[sample(length(sim_nonscr_data0803$label), 171), ]

sim_scr_data0803.sub_fair <-sim_scr_data0803[sample(length(sim_scr_data0803$label), 171), ]

sim_scr_data0803.sub_fair$scrtype <- NULL
sim_scr_data0803.sub_fair$p <- NULL
sim_nonscr_data0803.sub_fair<-as.data.frame(sim_nonscr_data0803.sub_fair)
colnames(sim_scr_data0803.sub_fair) <- colnames(sim_nonscr_data0803.sub_fair)

sim_nonscr_scr_data0803_sub_fair <- rbind(sim_nonscr_data0803.sub_fair,sim_scr_data0803.sub_fair)

length(sim_nonscr_scr_data0803_sub_fair$label)

write.arff(sim_nonscr_scr_data0803_sub_fair, file=paste("./data_csv/sim_nonscr_scr_data0803_sub_fair.arff",sep=""), relation = "0803_sub_scr_nonscr")


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
      win<-doSimulationAllFeatures( data.sub, FALSE, idx, 150, 50, FALSE, plotting = TRUE, thresholdvar = 0.01 ,doFineWindow = FALSE)
      row.names(win) <- NULL
      win <- as.data.frame(win)
      print(paste("win len", length(win)))
      View(win)
      if(k=='1'){
        sum_data <- win[,1:length(win)]
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


tt<- getDataset2(NULL, c(
"non_scratch/jongin_ns1R",FALSE,0,0,
"non_scratch/jongin_ns1L",FALSE,0,0,
"non_scratch/jongin_ns2R",FALSE,0,0,
"non_scratch/jongin_ns2L",FALSE,0,0,
"non_scratch/eunji_ns1R",FALSE,0,0,
"non_scratch/eunji_ns1L",FALSE,0,0,
"non_scratch/eunji_ns2R",FALSE,0,0,
"non_scratch/eunji_ns2L",FALSE,0,0,
"non_scratch/junhong_ns1R",FALSE,0,0,
"non_scratch/junhong_ns1L",FALSE,0,0,
"non_scratch/junhong_ns2R",FALSE,0,0,
"non_scratch/junhong_ns2L",FALSE,0,0
),i, 150, 50, TRUE, NULL)


tt<- getDataset2(NULL, c(
  "p2/p2_left_data",FALSE,0,0
),i, 150, 50, FALSE, NULL)



turnoverdata <- c(
  "non_scratch/jongin_ns1R_turnover",FALSE,0,0,
  "non_scratch/jongin_ns1L_turnover",FALSE,0,0,
  "non_scratch/eunji_ns1R_turnover",FALSE,0,0,
  "non_scratch/eunji_ns1L_turnover",FALSE,0,0,
  "non_scratch/junhong_ns1R_turnover",FALSE,0,0,
  "non_scratch/junhong_ns1L_turnover",FALSE,0,0
)

pullblankdata <- c(
  "non_scratch/jongin_ns1R_pullblank",FALSE,0,0,
  "non_scratch/jongin_ns1L_pullblank",FALSE,0,0,
  "non_scratch/eunji_ns1R_pullblank",FALSE,0,0,
  "non_scratch/eunji_ns1L_pullblank",FALSE,0,0,
  "non_scratch/junhong_ns1R_pullblank",FALSE,0,0,
  "non_scratch/junhong_ns1L_pullblank",FALSE,0,0
)

stretchdata <- c(
  "non_scratch/jongin_ns1R_stretch",FALSE,0,0,
  "non_scratch/jongin_ns1L_stretch",FALSE,0,0,
  "non_scratch/eunji_ns1R_stretch",FALSE,0,0,
  "non_scratch/eunji_ns1L_stretch",FALSE,0,0,
  "non_scratch/junhong_ns1R_stretch",FALSE,0,0,
  "non_scratch/junhong_ns1L_stretch",FALSE,0,0
)

walkdata <- c(
  "non_scratch/jongin_ns2R",FALSE,0,0,
  "non_scratch/jongin_ns2L",FALSE,0,0,
  "non_scratch/eunji_ns2R",FALSE,0,0,
  "non_scratch/eunji_ns2L",FALSE,0,0,
  "non_scratch/junhong_ns2R",FALSE,0,0,
  "non_scratch/junhong_ns2L",FALSE,0,0
)

total <- c(
  "non_scratch/jongin_ns1R",FALSE,0,0,
  "non_scratch/jongin_ns1L",FALSE,0,0,
  "non_scratch/jongin_ns2R",FALSE,0,0,
  "non_scratch/jongin_ns2L",FALSE,0,0,
  "non_scratch/eunji_ns1R",FALSE,0,0,
  "non_scratch/eunji_ns1L",FALSE,0,0,
  "non_scratch/eunji_ns2R",FALSE,0,0,
  "non_scratch/eunji_ns2L",FALSE,0,0,
  "non_scratch/junhong_ns1R",FALSE,0,0,
  "non_scratch/junhong_ns1L",FALSE,0,0,
  "non_scratch/junhong_ns2R",FALSE,0,0,
  "non_scratch/junhong_ns2L",FALSE,0,0
)


data1 <- getNonscratch(total, c(1,3,8)) # 499
data2 <- getNonscratch(turnoverdata, c(1,3,8)) # 148
data3 <- getNonscratch(pullblankdata, c(1,3,8)) # 46
data4 <- getNonscratch(stretchdata, c(1,3,8)) # 77
data5 <- getNonscratch(walkdata, c(1,3,8)) # 206


data1v2 <- getNonscratch(total, c(1,3,8)) # 499
data2v2 <- getNonscratch(turnoverdata, c(1,3,8)) # 148
data3v2 <- getNonscratch(pullblankdata, c(1,3,8)) # 46
data4v2 <- getNonscratch(stretchdata, c(1,3,8)) # 77
data5v2 <- getNonscratch(walkdata, c(1,3,8)) # 206

data2v3 <- getNonscratch(turnoverdata, c(1,3,8)) # 148
data3v3 <- getNonscratch(pullblankdata, c(1,3,8)) # 46
data4v3 <- getNonscratch(stretchdata, c(1,3,8)) # 77
data5v3 <- getNonscratch(walkdata, c(1,3,8)) # 206
data1v3 <- getNonscratch(total, c(1,3,8)) # 499

length(data3v3$label)



scrdata <- getDataN(folderlist, ranglist)


getNonscratch<-function(filelist, sensorlist)
{
  first <- sensorlist[1]
  for(i in sensorlist)
  {
    tt<- getDataset2(NULL,filelist ,i, 150, 50, TRUE, NULL)
    
    if(i==first){
      sum_data <- tt[,1:length(tt)]
    }else{
      sum_data <- c(sum_data,tt[,6:length(tt)])
    }
    
    print(paste("index : ", i, "len : ", nrow(tt) ))
    
  }
  
  return(sum_data)
  
}

  

