scratching_real <- c("scratching_data0521(1_57_38)", TRUE, 888, 0,
                     "scratching_data0521(3_14_16)", TRUE, 888, 0,
                     "scratching_data0521(3_15_58)", TRUE, 888, 0,
                     "scratching_data0521(3_42_22)", TRUE, 888, 0,
                     "scratching_data0521(3_55_20)", TRUE, 888, 0,
                     "scratching_data0521(4_21_17)", TRUE, 888, 0)


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

scratch_data_list<-scratching_real

scratch_data_list <- c(
  "subject1_1", FALSE, 0,0
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

sim_data <-read.arff("./data_csv/sim_data_0906_scr_type1234.arff")
sim_data$label <- factor(sim_data$label)
train_control <- trainControl(method="cv", number=10)

View(sim_data[c(1,267:302)])
sim_model1 <- train(label~., data=sim_data[c(1,267:302)], trControl = train_control,  method="glm")

View(sim_data[c(1,50:85)])
sim_model1_gyro <- train(label~., data=sim_data[c(1,50:85)], trControl = train_control,  method="glm")

View(sim_data[c(1,231:266)])
sim_model1_noPCA <- train(label~., data=sim_data[c(1,231:266)], trControl = train_control,  method="glm")

View(sim_data[c(1,379:414)])
auto_sim_model1_PCA <- train(label~., data=sim_data[c(1,379:414)], trControl = train_control,  method="glm")

View(sim_data[c(1,303:314)])
auto_sim_model1_max <- train(label~., data=sim_data[c(1,303:314)], trControl = train_control,  method="glm")

View(sim_data[c(1,86:97)])
auto_sim_model1_PCA_gyro <- train(label~., data=sim_data[c(1,86:97)], trControl = train_control,  method="glm")

####

View(combined.model1data[c(1,2:13)]) # 1D gyro spacial features (mag)
View(combined.model1data[c(1,86:97)]) # 1D gyro time related features (max)
train_control <- trainControl(method="cv", number=10)
sim_1Dmodel1 <- train(label~., data=combined.model1data[c(1,2:13,86:97)], trControl = train_control,  method="glm")

View(combined.model2data[c(1,267:302)]) # 3D accel spacial features 
View(combined.model2data[c(1,379:414)]) # 3D accel time related features 
sim_1Dmodel1 <- train(label~., data=combined.model1data[c(1,2:13,86:97)], trControl = train_control,  method="glm")
sim_3Dmodel2 <- train(label~., data=combined.model2data[c(1,267:302,379:414)], trControl = train_control,  method="glm")

#

View(combined.model1data[c(1,2:13,219:230)]) # 1D gyro spacial features (mag), + 1D accel spacial features (mag)
View(combined.model1data[c(1,86:97,303:314)]) # 1D gyro time related features (max), + 1D accel time-related features (max)
train_control <- trainControl(method="cv", number=10)
sim_1Dmodel1 <- train(label~., data=combined.model1data[c(1,2:13,86:97,219:230,303:314)], trControl = train_control,  method="glm")
sim_1Dmodel1

View(combined.model2data[c(1,267:302)]) # 3D accel spacial features
View(combined.model2data[c(1,379:414)]) # 3D accel time related features 
sim_3Dmodel2 <- train(label~., data=combined.model2data[c(1,267:302,379:414)], trControl = train_control,  method="glm")
sim_3Dmodel2

#

View(sim_data0910_model1[c(1,72:83,282:293)]) # 1D gyro spacial features (mag)
View(sim_data0910_model1[c(1,2:11,212:221)]) # 1D gyro time related features (max)
train_control <- trainControl(method="cv", number=10)
sim_1Dmodel1 <- train(label~., data=sim_data0910_model1[c(1,2:11,72:83,212:221,282:293)], trControl = train_control,  method="glm")
sim_1Dmodel1


View(sim_data0910_model2[c(1,152:187,362:397)]) # 3D accel, gyro auto features
#View(combined.model2data[c(1,379:414)]) # 3D accel time related features 
sim_3Dmodel2 <- train(label~., data=sim_data0910_model2[c(1,152:187,362:397)], trControl = train_control,  method="glm")

summary(sim_3Dmodel2)
sim_3Dmodel2

#####

View(sim_data[c(1,98:109)])
sim_model1 <- train(label~., data=sim_data[c(1,98:109)], trControl = train_control,  method="glm")
sim_model2 <- train(label~., data=sim_data[c(1,98:109)], trControl = train_control,  method="J48")
sim_model3 <- train(label~., data=sim_data[c(1,98:109)], trControl = train_control,  method="svmLinear")

predictions <- predict(sim_model1, sim_data[c(1,98:109)])
confusionMatrix(predictions, sim_data$label)
sim_model1


View(sim_data[c(1,127:138)])
sim_model1_auto <- train(label~., data=sim_data[c(1,127:138)], trControl = train_control,  method="glm")
sim_model2_auto <- train(label~., data=sim_data[c(1,127:138)], trControl = train_control,  method="J48")
sim_model3_auto <- train(label~., data=sim_data[c(1,127:138)], trControl = train_control,  method="svmLinear")

sim_model1_auto
predictions <- predict(sim_model1_auto, sim_data[c(1,127:138)],type = "prob")
predictions$label <- sim_data$label
plot(c(1:length(predictions$non)), sort(predictions$scratch), col = (predictions$label[order(predictions$scratch)]))

confusionMatrix(predictions, sim_data$label)

View(sim_data[c(1,98:109,127:138)])
sim_model1_auto_g <- train(label~., data=sim_data[c(1,98:109,127:138)], trControl = train_control,  method="glm")
sim_model2_auto_g <- train(label~., data=sim_data[c(1,98:109,127:138)], trControl = train_control,  method="J48")
sim_model3_auto_g <- train(label~., data=sim_data[c(1,98:109,127:138)], trControl = train_control,  method="svmLinear")

predictions <- predict(sim_model3_auto_g, sim_data[c(98:109,127:138)])
confusionMatrix(predictions, sim_data$label)


##

real_data$label <- factor(real_data$label)
train_control <- trainControl(method="cv", number=10)
View(real_data[c(1,50:61)])
real_data.sub <- real_data[c(1,50:61)]
real_data.sub[c(2:length(real_data.sub))] <-  lapply((real_data.sub[c(2:length(real_data.sub))]), as.numeric)
View(real_data.sub)
model <- train(label~., data=real_data[c(1,50:61)], trControl = train_control,  method="J48")
predictions <- predict(model, real_data[c(1,50:61)])
confusionMatrix(predictions, real_data$label)
remove(predictions)

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