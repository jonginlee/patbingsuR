
scratching_real <- c("scratching_data0521(1_57_38)", FALSE, 0, 0,
                     "scratching_data0521(3_14_16)", FALSE, 0, 0,
                     "scratching_data0521(3_15_58)", FALSE, 0, 0,
                     "scratching_data0521(3_42_22)", FALSE, 0, 0,
                     "scratching_data0521(3_55_20)", FALSE, 0, 0,
                     "scratching_data0521(4_21_17)", FALSE, 0, 0)

selected <- ar_temp[selected_indexs]
data<-as.data.frame(selected)
print(colnames(data))
data$label <- factor(data$label)
train_control <- trainControl(method="cv", number=10)
gmodel0701 <- train(label~., data=data, trControl = train_control,  method=ML_method)


scratch_data_list <- c(
  "jonginlee_data0623/data5", FALSE, 0, 0,
  "jonginlee_data0623/data6", FALSE, 0, 0
    )

scratch_data_list <- c(
  "scratching_data0521(2_45_02)",FALSE, 0, 0,
  "scratching_data0521(3_07_46)",FALSE, 0, 0,
  "scratching_data0521(3_38_20)",FALSE, 0, 0,
  "scratching_data0521(4_40_18)",FALSE, 0, 0,
  "scratching_data0521(4_53_23)",FALSE, 0, 0,
  "scratching_data0521(5_12_04)",FALSE, 0, 0,
  "scratching_data0521(5_23_31)",FALSE, 0, 0,
  "scratching_data0521(5_42_37)",FALSE, 0, 0,
  "scratching_data0521(6_47_09)",FALSE, 0, 0,
  "scratching_data0521(2_07_47)",FALSE, 0, 0,
  "scratching_data0521(2_12_43)",FALSE, 0, 0
)

for(i in 0:((length(scratch_data_list)/4)-1))
{
  labelname <- scratch_data_list[i*4+1]
  isCut <- scratch_data_list[i*4+2]
  startMilli <- scratch_data_list[i*4+3]
  endMilli <- scratch_data_list[i*4+4]
  
  detectScratchMovs2(c( labelname, isCut, startMilli, endMilli), TRUE, 
                     c(1,3,8), c(1,49,50,51,82,83,84,88,111,112,113,123,124,125), 8, 150, 50, gmodel0705, TRUE, isTraindata = FALSE,thresholdvar = 0.01 )

  
}

for(i in 0:((length(scratch_filelist)/4)-1))
{
  labelname <- scratch_filelist[i*4+1]
  isCut <- scratch_filelist[i*4+2]
  startMilli <- scratch_filelist[i*4+3]
  endMilli <- scratch_filelist[i*4+4]
  
  detectScratchMovs2(c( labelname, isCut, startMilli, endMilli), TRUE, 
                     c(1,3,8), c(1,49,50,51,82,83,84,88,111,112,113,123,124,125), 8, 150, 50, gmodel0609, TRUE,thresholdvar = 0.01 )
  
}


for(i in 0:((length(non_scratch_filelist)/4)-1))
{
  labelname <- non_scratch_filelist[i*4+1]
  isCut <- non_scratch_filelist[i*4+2]
  startMilli <- non_scratch_filelist[i*4+3]
  endMilli <- non_scratch_filelist[i*4+4]
  
  detectScratchMovs2(c( labelname, isCut, startMilli, endMilli), FALSE, 
                     c(1,3,8), c(1,49,50,51,82,83,84,88,111,112,113,123,124,125), 8, 150, 50, gmodel0609, TRUE,thresholdvar = 0.01  )
  
}





scratch_filelist<- c(
 #  "data_watch_jongin_one_finger_0607",TRUE, 16000, 2000,
  "data_watch_jongin_one_finger_2_0607",TRUE, 2000, 0,
  "data_watch_jongin_one_finger_0608", TRUE, 2000, 2000,
  "data_watch20150412_scratching_x",TRUE, 2000, 0,
  "data_watch20150412_scratching_xy", TRUE, 2000, 2000,
  "data_watch20150412_scratching_y",TRUE, 2000, 2000,
  "data_watch20150412_scratching_z",TRUE, 2000, 30000,
# "data_watch20150413_eunji_scratch",TRUE, 2000, 3000,
  "data_watch20150413_eunji_scratch_2",TRUE, 2000, 4000,
  "data_watch20150413_seungho",TRUE,32000,7000,
  "data_watch20150413_seungho_3",TRUE,38000,5000,
  "data_watch20150412_scratching_no_wrist",TRUE, 2000, 2000,
  "data_watch20150412_scratching_no_wrist_2",TRUE, 2000, 2000,
  "data_watch20150416_scratching_no_wrist_3",TRUE, 2000, 2000,
  "data_watch20150416_scratching_no_wrist_4",TRUE, 4000, 2000,
  "data_watch20150417_scratchingtest1",TRUE, 20000,2000,
  "data_watch20150417_scratchingtest2",TRUE, 23000,10000,
  "data_watch20150417_scratchingtest3",TRUE, 42000,12000,
  "data_watch20150417_scratchingtest4",TRUE, 30000,6000
)


for(i in 0:((length(non_scratch_filelist)/4)-1))
{
  labelname <- non_scratch_filelist[i*4+1]
  isCut <- non_scratch_filelist[i*4+2]
  startMilli <- non_scratch_filelist[i*4+3]
  endMilli <- non_scratch_filelist[i*4+4]
  
  data<- read.table(paste("./data_raw/",labelname,".txt",sep=""),sep=",",header=TRUE)
  test1<-doSimulationAllFeatures(data, FALSE, 8, 150, 50, labelname, 
                                 TRUE, startMilli = startMilli, endMilli = endMilli, thresholdvar = 0.01 )  
}

sensor_indexes <- c(1,3,8)

non_scratch_filelist <-  c(
  "scratching_data0521(2_45_02)",FALSE, -1,-1,
  "scratching_data0521(3_07_46)",FALSE, -1,-1,
  "scratching_data0521(3_38_20)",FALSE, -1,-1,
  "scratching_data0521(4_40_18)",FALSE, -1,-1,
  "scratching_data0521(4_53_23)",FALSE, -1,-1,
  "scratching_data0521(5_12_04)",FALSE, -1,-1,
  "scratching_data0521(5_23_31)",FALSE, -1,-1,
#  "scratching_data0521(5_42_37)",FALSE, -1,-1,
  "scratching_data0521(6_47_09)",FALSE, -1,-1,
  "scratching_data0521(2_07_47)",FALSE, -1,-1,
  "scratching_data0521(2_12_43)",FALSE, -1,-1
)


truning_over <- c(


  ,
)

scratch_filelist<- c(
  "data_watch_jongin_one_finger_2_0607",TRUE, 2000, 0,
  "data_watch_jongin_one_finger_0608", TRUE, 2000, 2000
)

non_scratch_filelist <-  c(
  "scratching_data0521(2_45_02)",FALSE, -1,-1
)

selected_indexs <- c(1,49,50,51,82,83,84,88,111,112,113,123,124,125)

window_size <- 150
window_step <- 50
ML_method <- "J48"

getWindowset<-function(scratch_filelist, non_scratch_filelist, sensor_indexes, window_size, window_step,
                       selected_indexs, plotting=FALSE,thresholdvar = 0.1)
{
  for(i in sensor_indexes)
  {
    tt<- getDataset2(scratch_filelist, non_scratch_filelist, i, window_size, window_step, plotting, NULL,thresholdvar = thresholdvar)
    
    if(i=='1'){
      sum_data <- tt[,5:length(tt)]
    }else{
      sum_data <- c(sum_data,tt[,6:length(tt)])
    }
    
    print(paste("index : ",i, "len : ",nrow(tt) ))
    
  }
  
#  write.csv(sum_data, file=paste("./data_raw/testsetForWeka7.arff",sep=""), row.names=FALSE)
  
#  selected <- sum_data[selected_indexs]
#  data<-as.data.frame(selected)
#  print(colnames(data))
#  data$label <- factor(data$label)
  
#  return(data)
  return(sum_data)
}

getModelBy <- function(scratch_filelist, non_scratch_filelist, sensor_indexes, window_size, window_step,
                       selected_indexs, ML_method, plotting=FALSE,thresholdvar = 0.1)
{
  for(i in sensor_indexes)
  {
    tt<- getDataset2(scratch_filelist, non_scratch_filelist, i, window_size, window_step, plotting, NULL,thresholdvar = thresholdvar)
    
    if(i=='1'){
      sum_data <- tt[,5:length(tt)]
    }else{
      sum_data <- c(sum_data,tt[,6:length(tt)])
    }
    
    print(paste("index : ",i, "len : ",nrow(tt) ))
    
  }


  #write.csv(sum_data, file=paste("./data_raw/testsetForWeka7.arff",sep=""), row.names=FALSE)

  selected <- sum_data[selected_indexs]
  data<-as.data.frame(selected)
  print(colnames(data))
  data$label <- factor(data$label)
  
  train_control <- trainControl(method="cv", number=10)
  model <- train(label~., data=data, trControl = train_control,  method=ML_method)

  return(model)
}