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
non_data <- getNonscratch(total, c(3,8)) # 499
data_t<-data
View(data)
#View(data[c(5:length(data))])
data[c(5:length(data))] <-  lapply((data[c(5:length(data))]), as.numeric)
data$label <- factor(data$label)
data[c(1:3)] <-  lapply((data[c(1:3)]), as.character)

write.csv(data, "./data_csv/sim_scratch09016.csv")
write.csv(non_data, "./data_csv/sim_non_scratch09016_2.csv")
########### autocorrelation split

tes <- read.csv("./subject_data/scratch_log_total.csv", header=TRUE)

getRelabeld(c(2), tes, limit_t = 5000)

length(labeled$start_milli)


getRelabeld <- function(sublist, tes, limit_t=3000)
{
  for(sub_n in sublist){
    #limit_t <- 5000
    tes2 <- subset(tes, subset=(tes$sub_num == sub_n))
    tes2_right <- subset(tes2, subset=(tes2$watch == "right"))
    scr_info <- subset(tes2_right, subset=(tes2_right$data_type == "log"))
    labeled <-read.csv(paste("./subject_data/extracted/subject",sub_n,"_right_915.csv",sep=""),header=TRUE)
  #  View(labeled)
    
    relabeld<-list()
    for(i in 1:length(labeled$label))
    {
      start<-as.numeric(as.character(data.frame(strsplit(as.character(labeled$start_milli[i])," "))[4,]))
      end<-as.numeric(as.character(data.frame(strsplit(as.character(labeled$end_milli[i])," "))[4,]))
      relabeld$relabel[i] <- isScratchIn(start, end, scr_info,limit_t = limit_t) 
      relabeld$ori[i]<-as.character(labeled$label[i])
      #print(paste("right - st:",start, ", ed:",end,", ori ",relabeld$ori[i], ", relabel ",relabeld$relabel[i]))
      
    }
    #View(relabeld)
    labeled$label <- relabeld$relabel
    write.csv(labeled, file=paste("./subject_data/extracted/subject",sub_n,"_right_915.csv",sep=""), row.names=F)
  
  ##############
  
    tes2 <- subset(tes, subset=(tes$sub_num == sub_n))
    tes2_left <- subset(tes2, subset=(tes2$watch == "left"))
    scr_info <- subset(tes2_left, subset=(tes2_left$data_type == "log"))
    labeled <-read.csv(paste("./subject_data/extracted/subject",sub_n,"_left_915.csv",sep=""),header=TRUE)
    #  View(labeled)
    
    relabeld<-list()
    for(i in 1:length(labeled$label))
    {
      start<-as.numeric(as.character(data.frame(strsplit(as.character(labeled$start_milli[i])," "))[4,]))
      end<-as.numeric(as.character(data.frame(strsplit(as.character(labeled$end_milli[i])," "))[4,]))
      relabeld$relabel[i] <- isScratchIn(start, end, scr_info,limit_t = limit_t) 
      relabeld$ori[i]<-as.character(labeled$label[i])
      #print(paste("left - st:",start, ", ed:",end,", ori ",relabeld$ori[i], ", relabel ",relabeld$relabel[i]))
    }
    #View(relabeld)
    labeled$label <- relabeld$relabel
    write.csv(labeled, file=paste("./subject_data/extracted/subject",sub_n,"_left_915.csv",sep=""), row.names=F)
  
  }
}

getRSmodel <- function(data, data_non, real_tdata, model_num, title, fair = TRUE, thr = 1)
{
  if(!is.null(data)){
    data$p<-NULL
    data$scrtype<-NULL
  }
  
  if(model_num=="1")
  {
    if(!is.null(real_tdata)){
      real_tdata.scr <- subset(real_tdata, subset=(real_tdata$label == "scratch"))
      real_tdata.non <- subset(real_tdata, subset=(real_tdata$label == "non"))
      
      if(!is.null(data))
        data<- rbind(real_tdata.scr[4:length(real_tdata.scr)],data[5:length(data)])
      else
        data<-real_tdata.scr[4:length(real_tdata.scr)]
      
      if(!is.null(data_non))
        data_non<- rbind(real_tdata.non[4:length(real_tdata.non)], data_non[6:length(data_non)])
      else
        data_non<- real_tdata.non[4:length(real_tdata.non)]
    }

    data$var_th <- data$Linearaccel_variance_avg.PC_x./((data$Linearaccel_variance_avg.PC_y.))
    data_non$var_th <- data_non$Linearaccel_variance_avg.PC_x./((data_non$Linearaccel_variance_avg.PC_y.))
    
    data.model1data<-data[which((data$var_th < thr)),]
    data_non.model1data<-data_non[which((data_non$var_th < thr)),]
    if(!is.null(real_tdata))
      saveFairTrainData(data.model1data, data_non.model1data, title, fair)
    else
      saveFairTrainData(data.model1data[,5:length(data.model1data)], data_non.model1data[,6:length(data_non.model1data)], title, fair)
    
  }else if(model_num=="2")
  {
    if(!is.null(real_tdata))
    {
      real_tdata.scr <- subset(real_tdata, subset=(real_tdata$label == "scratch"))
      real_tdata.non <- subset(real_tdata, subset=(real_tdata$label == "non"))
      if(!is.null(data))
        data<- rbind(real_tdata.scr[4:length(real_tdata.scr)], data[5:length(data)])
      else
        data<-real_tdata.scr[4:length(real_tdata.scr)]
      
      if(!is.null(data_non))
        data_non<- rbind(real_tdata.non[4:length(real_tdata.non)], data_non[6:length(data_non)])
      else
        data_non<- real_tdata.non[4:length(real_tdata.non)]
    }

    data$var_th <- data$Linearaccel_variance_avg.PC_x./((data$Linearaccel_variance_avg.PC_y.))
    data_non$var_th <- data_non$Linearaccel_variance_avg.PC_x./((data_non$Linearaccel_variance_avg.PC_y.))
    
    data.model2data<-data[which((data$var_th >= thr)),]
    data_non.model2data<-data_non[which((data_non$var_th >= thr)),]
    if(!is.null(real_tdata))
      saveFairTrainData(data.model2data, data_non.model2data, title, fair)
    else
      saveFairTrainData(data.model2data[,5:length(data.model2data)], data_non.model2data[,6:length(data_non.model2data)], title, fair)
    
    
  }else if(model_num=="all")
  {
    if(!is.null(real_tdata))
    {
      real_tdata.scr <- subset(real_tdata, subset=(real_tdata$label == "scratch"))
      real_tdata.non <- subset(real_tdata, subset=(real_tdata$label == "non"))
      if(!is.null(data))
        data<- rbind(real_tdata.scr[4:length(real_tdata.scr)], data[5:length(data)])
      else
        data<-real_tdata.scr[4:length(real_tdata.scr)]
      
      if(!is.null(data_non))
        data_non<- rbind(real_tdata.non[4:length(real_tdata.non)], data_non[6:length(data_non)])
      else
        data_non<- real_tdata.non[4:length(real_tdata.non)]
    }
    if(!is.null(real_tdata))
      saveFairTrainData(data, data_non, title, fair)
    else
      saveFairTrainData(data[,5:length(data)], data_non[,6:length(data_non)], title, fair)
  }
  
  model_data <- read.arff(paste("./data_csv/",title ,".arff",sep=""))
  #View(model_data)
  #train_control <- trainControl(method="cv", number=10)
  #model <- train(label~., data=model_data[features], trControl = train_control,  method="glm")  
  
  return(model_data)
}

data<-read.csv("./data_csv/sim_scratch09016.csv")
data_non<-read.csv("./data_csv/sim_non_scratch09016_2.csv")
nrow(data)
nrow(data_non)


sublist <- c(1,2,3,4,5,6,7,8,9,11,12,13,14,15)
model1 <- train(label~., data=model_data[c(1, (72:83),(282:293) )], trControl = train_control,  method="svmLinear") 
model2 <- train(label~., data=model_data[c(1,152:187,362:397)], trControl = train_control,  method="svmLinear")
model1
model2
sub_n<-2
for(sub_n in sublist)
{
  test_data<-getRTrainData(sub_n, T, 3000)
#  test_data<-getRTrainData(c(7), T, 3000)
  
  data_log <- doAcc(test_data, model1,c(1,(72:83),(282:293)), model2,c(1,152:187,362:397))
#  View(data_log)  
  write.csv(data_log, paste("./data_csv/pred_p",sub_n,".csv",sep=""))
}



{
  real_data<-getRTrainData(c(1,2,3,4,5,6,7), T, 3000)
  real_data <- subset(real_data, subset=(real_data$label == "non"))
  nrow(real_data)  
  data$p<-NULL
  data$scrtype<-NULL
  saveFairTrainData(real_data[,4:length(real_data)], data[,5:length(data)], "simulatedScratch_realNon0919_v2", TRUE)
  
  model_data3d<-getRSmodel(data, NULL, real_data,2, "simulatedScratch_realNon0919_3D")
}


real_data<-getRTrainData(c(1,2,3,4,5,6,7,8), T, 3000)

model_data_r1234567<-getRSmodel(NULL,NULL,real_data,"all","0918_R23")
model_data_sr1234567_3s<-getRSmodel(data,data_non,real_data,"all","0918_SR1234567")

model_data_r1234567_5s<-getRSmodel(NULL,NULL,real_data,"all","0918_R456")
model_data_r1234567_10s<-getRSmodel(NULL,NULL,real_data,"all","0918_R1234567_3s")
model_data_sr1234567_6s<- getRSmodel(data, data_non, real_data,"all", "0918_SR1234567" )

model_data <- model_data_sr1234567_3s
write.arff(real_data, file=paste("./data_csv/sub8_0919.arff",sep=""), relation= "sub8_0919")

View(real_data)
nrow(real_data)
# c(1,5,6,8,9,11,72:84,215,216,218,219,221 )
# c(1,5,6,8,9,11,72:84,215,216,218,219,221 )
model_data <- getRSmodel(data, data_non, real_data,1, "0918_allmodel_simonly", thr = 1)
model_data <- getRSmodel(data, data_non, NULL,"all", "0918_allmodel_simonly",FALSE)

model_data <- getRSmodel(NULL,NULL,real_data,"all","0918_R456")

model_data <- getRSmodel(data, data_non, real_data,"all", "0918_allmodel_th1" )
train_control <- trainControl(method="cv", number=10)
model1 <- train(label~., data=model_data[c(1,5,6,8,9,11,72:83,215,216,218,219,221)], trControl = train_control,  method="svmLinear") 
model1 <- train(label~., data=model_data[c(1,5,6,8,9,11,72:83,215,216,218,219,221,(282:293))], trControl = train_control,  method="svmLinear") 

model1 <- train(label~., data=model_data[c(1, (72:83) )], trControl = train_control,  method="svmLinear") 
model1 <- train(label~., data=model_data[c(1, (282:293) )], trControl = train_control,  method="svmLinear") 

model1 <- train(label~., data=model_data[c(1, (72:83),(282:293) )], trControl = train_control,  method="svmLinear") 
model1

#  c(1,152:190,362:400)
#  c(1,152:190,362:400)
# c(1,51:56, 60:65,69:71,152:190,261:266,270:275,279:281,362:400)
model_data <- getRSmodel(data, data_non, real_data, 2, "0918_3Dmodel" , thr = 1)
model_data <- getRSmodel(data, data_non, NULL, 2, "0918_3Dmodel_simonly" , thr = 1)
model_data <- getRSmodel(data, data_non, real_data, 2, "0918_SR1234567_3D" )
train_control <- trainControl(method="cv", number=10)
model2 <- train(label~., data=model_data[c(1,152:187,362:397)], trControl = train_control,  method="svmLinear")
model2 <- train(label~., data=model_data[c(1,51:56, 60:65,69:71,152:190,261:266,270:275,279:281,362:400)], trControl = train_control,  method="svmLinear")
model2 <- train(label~., data=model_data[c(1,51:56, 60:65,69:71,152:187,261:266,270:275,279:281,362:397)], trControl = train_control,  method="svmLinear")

model2




getRTrainData<-function(sublist, relabel = F, limit_t = 3000)
{
  tes <- read.csv("./subject_data/scratch_log_total.csv", header=TRUE)
  
  if(relabel)
    for( sub_n in sublist)
    {
      getRelabeld(sub_n, tes, limit_t = limit_t)
    }
  
  total_data<-list()
  for(sub_n in sublist)
  {

    right_data <-read.csv(paste("./subject_data/extracted/subject",sub_n,"_right_915.csv",sep=""),header=TRUE)
    left_data <-read.csv(paste("./subject_data/extracted/subject",sub_n,"_left_915.csv",sep=""),header=TRUE)
    tmp<-rbind(right_data, left_data)
    total_data<-rbind(total_data, tmp)
  }
  
  total_data.scr <- subset(total_data, subset=(total_data$label == "scratch"))
  total_data.non <- subset(total_data, subset=(total_data$label == "non"))
  
  print(paste("nrow(total_data.scr):", nrow(total_data.scr), ", nrow(total_data.non):", nrow(total_data.non) ))
  return(total_data)
}

test_data<-getRTrainData(c(8), T, 3000)
test_data<-getRTrainData(c(2), T, 3000)
test_data<-getRTrainData(c(7), T, 3000)

sub7res3s_m12 <- doAcc(test_data, model1,c(1,5,6,8,9,11,72:84,215,216,218,219,221), model2,c(1,51:56, 60:65,69:71,152:190,261:266,270:275,279:281,362:400))
View(sub7res3s_m12)


sub2res8s_m12 <- doAcc(test_data, model1,c(1,5,6,8,9,11,72:84,215,216,218,219,221), model2,c(1,51:56, 60:65,69:71,152:190,261:266,270:275,279:281,362:400))
View(sub2res8s_m12)


sub8res <- doAcc(test_data, model1,c(1,5,6,8,9,11,72:84,215,216,218,219,221), model2,c(1,152:190,362:400))
View(sub8res)

sub8res3s <- doAcc(test_data, model1,c(1,5,6,8,9,11,72:84,215,216,218,219,221), model2,c(1,51:56, 60:65,69:71,152:190,261:266,270:275,279:281,362:400))
View(sub8res3s)

sub7res_2 <- doAcc(test_data, model1,c(1,5,6,8,9,11,72:84,215,216,218,219,221), model2,c(1,51:56, 60:65,69:71,152:190,261:266,270:275,279:281,362:400))
View(sub7res_2)


sub7res_3 <- doAcc(test_data, model1,c(1,72:84), model2,c(1,152:190,362:400))
View(sub7res_3)

doAcc <- function(data, model1, features1, model2, features2)
{  
  predicted_label<-list()
  mylist.names <- c("start_h","end_h", "start", "end", "th", "pred1", "pred2", "combined","truth")
  predicted_label <- vector("list", length(mylist.names))
  names(predicted_label) <- mylist.names
  
  #temp <- data$Linearaccel_variance_avg.PC_x./((data$Linearaccel_variance_avg.PC_y. + data$Linearaccel_variance_avg.PC_z.)/2)  
  thr <- data$Linearaccel_variance_avg.PC_x./((data$Linearaccel_variance_avg.PC_y.))
  for(i in 1:length(thr))
  {
    predicted_label$pred2[i] <- predict(model2, data[i,(features2+3) ])   
    predicted_label$pred1[i] <- predict(model1, data[i, (features1+3)])
    
    predicted_label$start[i] <- as.numeric(as.character(data.frame(strsplit(as.character(data$start_milli[i])," "))[4,]))
    predicted_label$end[i] <- as.numeric(as.character(data.frame(strsplit(as.character(data$end_milli[i])," "))[4,]))
    predicted_label$start_h[i] <- as.character(data$start_milli[i])
    predicted_label$end_h[i] <-as.character(data$end_milli[i])
    
    predicted_label$th[i] <- thr[i]
    predicted_label$truth[i] <- as.character(data$label[i])
    
    #print(paste( , csv_data$end_milli[i], csv_data$label[i],"temp(",temp[i],")", "temp>1", , ", all ",  ))
    
    if( (predicted_label$pred1[i] == 2) & (predicted_label$pred2[i] == 2) )
      predicted_label$combined[i] <- 2
    else if( (predicted_label$pred1[i] == 2) & (predicted_label$pred2[i] == 1) )
      predicted_label$combined[i] <- 2
    else if( (predicted_label$pred1[i] == 1) & (predicted_label$pred2[i] == 2) )
      predicted_label$combined[i] <- 2
    else if( (predicted_label$pred1[i] == 1) & (predicted_label$pred2[i] == 1) )
      predicted_label$combined[i] <- 1

  }
  #View(predicted_label)
  
  predicted_label$pred1 <- factor(predicted_label$pred1, labels=c("non","scratch"))
  predicted_label$pred2 <- factor(predicted_label$pred2, labels=c("non","scratch"))
  
  predicted_label$combined <- factor(predicted_label$combined, labels=c("non","scratch"))
  
  
  #View(predicted_label)
  #levels(predicted_label$model1_pred )
  #levels(csv_data$label)
  #predicted_label<-ordered(predicted_label, levels = c("non", "scratch"))
  print(confusionMatrix(predicted_label$pred1, data$label))
  print(confusionMatrix(predicted_label$pred2, data$label))
  print(confusionMatrix(predicted_label$combined, data$label))
  
  return(predicted_label)
}






nrow(data_non)
real_tdata<-rbind(s2_left_csv, s2_right_csv)
View(real_tdata[4:length(real_tdata)])

real_tdata.scr <- subset(real_tdata, subset=(real_tdata$label == "scratch"))
real_tdata.non <- subset(real_tdata, subset=(real_tdata$label == "non"))
nrow(real_tdata.scr)
nrow(real_tdata.non)
data$p<-NULL
data$scrtype<-NULL

data<- rbind(real_tdata.scr[4:length(real_tdata.scr)], data[5:length(data)])
data_non<- rbind(real_tdata.non[4:length(real_tdata.non)], data_non[6:length(data_non)])



nrow(data) # 1699

#data$var_th <- data$Linearaccel_variance_avg.PC_x./((data$Linearaccel_variance_avg.PC_y. + data$Linearaccel_variance_avg.PC_z.)/2)
data$var_th <- data$Linearaccel_variance_avg.PC_x./((data$Linearaccel_variance_avg.PC_y.))

data.model1data<-data[which((data$var_th < 1)),]
data.model2data<-data[which((data$var_th >= 1)),]
nrow(data.model1data) # 816
nrow(data.model2data) # 883

nrow(data_non) # 283
#data_non$var_th <- data_non$Linearaccel_variance_avg.PC_x./((data_non$Linearaccel_variance_avg.PC_y. + data_non$Linearaccel_variance_avg.PC_z.)/2)
data_non$var_th <- data_non$Linearaccel_variance_avg.PC_x./((data_non$Linearaccel_variance_avg.PC_y.))

data_non.model1data<-data_non[which((data_non$var_th < 1)),]
data_non.model2data<-data_non[which((data_non$var_th >= 1)),]
nrow(data_non.model1data) # 183
nrow(data_non.model2data) # 100
View(data_non.model2data)

saveFairTrainData(data, data_non, "sim_data0913_model1_v12_all_rxy_th1")
View(data)
saveFairTrainData(data.model1data, data_non.model1data, "sim_data0913_model1_v12_rxy_th1")
saveFairTrainData(data.model2data, data_non.model2data, "sim_data0913_model2_v12_rxy_th1")

saveFairTrainData(data[,5:length(data)], data_non[,6:length(data_non)], "sim_data0917_all_2")

saveFairTrainData(data.model1data[,5:length(data.model1data)], data_non.model1data[,6:length(data_non.model1data)], "sim_data0910_model1_v11_rxyz_th1")
saveFairTrainData(data.model2data[,5:length(data.model2data)], data_non.model2data[,6:length(data_non.model2data)], "sim_data0917_3Dmodel2_v11_xy_th1")

sim_data0910_model2 <- read.arff("./data_csv/sim_data0913_model2_v12_rxy_th1.arff")
sim_data0910_model1 <- read.arff("./data_csv/sim_data0913_model1_v12_rxy_th1.arff")

View(sim_data0910_model2)


############ PCA variance dist
data <- read.csv("./data_csv/sim_scratch0906_PCA3D.csv")
data_non <- read.csv("./data_csv/sim_non_scratch0906_PCA3D.csv")

data[4:length(data)], non_data[5:length(non_data)]

nrow(data) # 1699
length(data)
data$var_th <- data$Linearaccel_var_x.PC./(data$Linearaccel_var_y.PC.)
data.model1data<-data[which((data$var_th < 1)),]
data.model2data<-data[which((data$var_th >= 1)),]
nrow(data.model1data) # 919
nrow(data.model2data) # 780

nrow(data_non) # 283
length(data_non)
data_non$var_th <- data_non$Linearaccel_var_x.PC./(data_non$Linearaccel_var_y.PC.)
data_non.model1data<-data_non[which((data_non$var_th < 1)),]
data_non.model2data<-data_non[which((data_non$var_th >= 1)),]
nrow(data_non.model1data) # 224
nrow(data_non.model2data) # 59

View(data.model1data[,5:length(data.model1data)])
View(data_non.model1data[,6:length(data_non.model1data)])
data.model1data$scrtype<-NULL
data.model1data$p<-NULL
combined.model1data <- rbind(data.model1data[,5:length(data.model1data)], data_non.model1data[,6:length(data_non.model1data)])

write.arff(combined.model1data, file=paste("./data_csv/sim_combined.model1data.arff",sep=""), relation = "combined.model1data")
View(combined.model1data)
hist(combined.model1data$Gyro_th_avg, col= factor(combined.model1data$label))

View(combined.model1data$label)

View(data.model2data[,5:length(data.model2data)])
View(data_non.model2data[,6:length(data_non.model2data)])
data.model2data$scrtype<-NULL
data.model2data$p<-NULL

combined.model2data <- rbind(data.model2data[,5:length(data.model2data)], data_non.model2data[,6:length(data_non.model2data)])

write.arff(combined.model2data, file=paste("./data_csv/sim_combined.3Dmodel2data_0917.arff",sep=""), relation = "combined.3Dmodel2data0917")
View(combined.model2data)

scr_data34 <- subset(data, subset=(data$scrtype == 3 | data$scrtype == 4))
scr_data12 <- subset(data, subset=(data$scrtype == 1 | data$scrtype == 2))


scr_data34_ellipse <- (abs(scr_data34$Linearaccel_max_x.PC.) + abs(scr_data34$Linearaccel_min_x.PC.)) /
  ((abs(scr_data34$Linearaccel_max_y.PC.) + abs(scr_data34$Linearaccel_min_y.PC.)))

hist(scr_data34_ellipse,breaks = c(0, seq(0,5,0.5)), xlim = c(0,5), freq = T, main = "scr_type34_ellipse",las=1)

scr_data12_ellipse <- (abs(scr_data12$Linearaccel_max_x.PC.) + abs(scr_data12$Linearaccel_min_x.PC.)) /
  ((abs(scr_data12$Linearaccel_max_y.PC.) + abs(scr_data12$Linearaccel_min_y.PC.)))

hist(scr_data12_ellipse,breaks = c(0, seq(0,5,0.5)), xlim = c(0,5), freq = T, main = "scr_type12_ellipse",las=1)

##

scr_data34_var <- scr_data34$Linearaccel_var_x.PC./((scr_data34$Linearaccel_var_y.PC.+scr_data34$Linearaccel_var_z.PC.)/2)
scr_data34_var <- scr_data34$Linearaccel_var_x.PC./(scr_data34$Linearaccel_var_y.PC.)
scr_data34_var <- scr_data34$Linearaccel_variance_avg.PC_x./((scr_data34$Linearaccel_variance_avg.PC_y. +scr_data34$Linearaccel_variance_avg.PC_z.)/2)

#scr_data34_var <- (scr_data34$Linearaccel_var_x.PC.,scr_data34$Linearaccel_var_y.PC.,scr_data34$Linearaccel_var_z.PC.)
hist(scr_data34_var,breaks = c(0, seq(0,25,1)), xlim = c(0,25), freq = T, main = "scr_type34_ellipse",las=1)
hist(scr_data34_var,breaks = c(0, seq(0,25,1)), ylim = c(0,0.5), freq = F, main = "scr_type34_ellipse",las=1)


scr_data12_var <- scr_data12$Linearaccel_var_x.PC./(scr_data12$Linearaccel_var_y.PC.)
scr_data12_var <- scr_data12$Linearaccel_var_x.PC./((scr_data12$Linearaccel_var_y.PC.+scr_data12$Linearaccel_var_z.PC.)/2)
scr_data12_var <- scr_data12$Linearaccel_variance_avg.PC_x./((scr_data12$Linearaccel_variance_avg.PC_y.+scr_data12$Linearaccel_variance_avg.PC_z.)/2)


hist(scr_data12_var,breaks = c(0, seq(0,25,1)),xlim = c(0,25),  freq = T, main = "scr_type12_ellipse",las=1)
hist(scr_data12_var,breaks = c(0, seq(0,25,1)), ylim = c(0,0.5), freq = F, main = "scr_type12_ellipse",las=1)

data_non_var <- data_non$Linearaccel_var_x.PC./(data_non$Linearaccel_var_y.PC.)
data_non_var <- data_non$Linearaccel_var_x.PC./((data_non$Linearaccel_var_y.PC.+data_non$Linearaccel_var_z.PC.)/2)
data_non_var <- data_non$Linearaccel_variance_avg.PC_x./((data_non$Linearaccel_variance_avg.PC_y.+data_non$Linearaccel_variance_avg.PC_z.)/2)

hist(data_non_var,breaks = c(0, seq(0,25,1)),xlim = c(0,25),  freq = T, main = "data_non_var",las=1)
hist(data_non_var,breaks = c(0, seq(0,25,1)), ylim = c(0,0.5), freq = F, main = "data_non_var",las=1)

##

s3_left_csv.scratch <- subset(s3_left_csv, subset=(s3_left_csv$label == "scratch"))

s3_left_csv.non <- subset(s3_left_csv, subset=(s3_left_csv$label == "non"))

s3_left_csv.scratch_var <- s3_left_csv.scratch$Linearaccel_var_x.PC./(s3_left_csv.scratch$Linearaccel_var_y.PC.)
hist(s3_left_csv.scratch_var,  freq = T, main = "s3_left_csv.scratch_var",las=1)

s3_left_csv.non_var <- s3_left_csv.non$Linearaccel_var_x.PC./(s3_left_csv.non$Linearaccel_var_y.PC.)
hist(s3_left_csv.non_var,  freq = T, main = "s3_left_csv.non_var",las=1)

s3_left_csv_var <- s3_left_csv$Linearaccel_var_x.PC./(s3_left_csv$Linearaccel_var_y.PC.)
hist(s3_left_csv_var,  freq = T, main = "s3_left_csv_var",las=1)


#############
non_data <- getNonscratch(total, c(3,8)) # 499
View(non_data)

sim_scr_data0805 <- data
sim_scr_data0805[c(2:length(sim_scr_data0805))] <-  lapply((sim_scr_data0805[c(2:length(sim_scr_data0805))]), as.numeric)
sim_nonscr_data0805 <- sum_data

View(data[4:length(data)])
View(non_data[5:length(non_data)])

saveFairTrainData(sim_scr_data0805, sim_nonscr_data0805[5:length(sim_nonscr_data0805)], "sim_data0805")

saveFairTrainData(sim_scr_data0805, sim_nonscr_data0805[5:length(sim_nonscr_data0805)], "sim_data0805")


non_data <- read.csv("./data_csv/sim_non_scratch0903_raw.csv")
data <- read.csv("./data_csv/sim_scratch0903_raw.csv")


non_data$X.1<- NULL
non_data$X <- NULL
non_data$epoches <- NULL
non_data$start_milli <- NULL
non_data$end_milli <-NULL

data$X <- NULL
data$epoches <- NULL
data$start_milli <- NULL
data$end_milli <-NULL


View(data[4:length(data)])
View(non_data[5:length(non_data)])

saveFairTrainData(data[4:length(data)], non_data[5:length(non_data)], "sim_data_0906_scr_type34")


saveFairTrainData<-function(scr_data, nonscr_data, savefile, fair=TRUE)
{
  #scr_data <- subset(scr_data, subset=(scr_data$scrtype == 3 | scr_data$scrtype == 4))
  
  if(fair==TRUE)
  {
    if(length(scr_data$label)  > length(nonscr_data$label))
      scr_data <-scr_data[sample(length(scr_data$label), length(nonscr_data$label)), ]
    
    if(length(scr_data$label) < length(nonscr_data$label))
      nonscr_data <-nonscr_data[sample(length(nonscr_data$label), length(scr_data$label)), ]    
  }
  
  scr_data$scrtype <- NULL
  scr_data$p <- NULL
  nonscr_data<-as.data.frame(nonscr_data)
  #colnames() <- colnames()
  colnames(scr_data) <- colnames(nonscr_data)
  
  #length(sim_nonscr_data0803)
  #length(sim_scr_data0803.sub)
  fair_data <- rbind(scr_data, nonscr_data)
  print(paste("saveFairTrainData - scr n:",length(scr_data$label), ", non_scr n:",length(nonscr_data$label)))
  
  #View(sim_nonscr_scr_data0803_far)
  fair_data$label <- as.factor(fair_data$label)
  fair_data[c(2:length(fair_data))] <-  lapply((fair_data[c(2:length(fair_data))]), as.numeric)    
  write.arff(x =fair_data, file=paste("./data_csv/",savefile,".arff",sep=""), relation = savefile)
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
    
#    if( (index==1) | (index==2) )
#    {
      win1 <- getFeaturesFromRanges(foldername, paste("data",index,sep=""), rangitem, c(3,8))
      win1$p <- fn_index
      win1$scrtype <- index      
#    }
    
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
      win<-doSimulationAllFeatures( data.sub, FALSE, idx, 150, 50, FALSE, plotting = FALSE, thresholdvar = 0.01 ,doFineWindow = FALSE)
      row.names(win) <- NULL
      win <- as.data.frame(win)
      print(paste("win len", length(win)))
      View(win)
      if(k=='3'){
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
  
  #View(resdata)
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

test_data_list <- c(
  "watch1_sensor_data0911",FALSE,0,0
)

test_data <- getNonscratch(test_data_list, c(3,8)) # 



test_data_list2 <- c(
  "watch2_sensor_data0911",FALSE,0,0
)
test_data_list3 <- c(
  "watch3_sensor_data0911",FALSE,0,0
)
test_data2 <- getNonscratch(test_data_list2, c(3,8)) # 
test_data3 <- getNonscratch(test_data_list3, c(3,8)) # 



getNonscratch<-function(filelist, sensorlist)
{
  first <- sensorlist[1]
  for(i in sensorlist)
  {
    tt<- getDataset2(NULL,filelist ,i, 150, 50, TRUE, NULL,thresholdvar = 0.01)
    
    if(i==first){
      sum_data <- tt[,1:length(tt)]
    }else{
      sum_data <- c(sum_data,tt[,6:length(tt)])
    }
    
    print(paste("index : ", i, "len : ", nrow(tt) ))
    
  }
  
  return(sum_data)
  
}

  

