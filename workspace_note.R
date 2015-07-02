
#history
library("caret", lib.loc="/usr/local/lib/R/site-library")
library("e1071", lib.loc="/usr/local/lib/R/site-library")
library("tuneR", lib.loc="/usr/local/lib/R/site-library")
library("RWeka", lib.loc="/usr/local/lib/R/site-library")
library("signal", lib.loc="/usr/local/lib/R/site-library")


# 0521 scratch file creating!!
## scratching

scratching_real <- c("scratching_data0521(1_57_38)",
                     "scratching_data0521(3_14_16)",
                     "scratching_data0521(3_15_58)",
                     "scratching_data0521(3_42_22)",
                     "scratching_data0521(3_55_20)",
                     "scratching_data0521(4_21_17)"
                     )

createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(1_57_38)", TRUE, "0521",TRUE, getMilliFromHMS(1,57,30,TRUE,1,41,51), getMilliFromHMS(1,57,55,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_14_16)", TRUE, "0521",TRUE, getMilliFromHMS(3,14,13,TRUE,1,41,51), getMilliFromHMS(3,14,30,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_15_58)", TRUE, "0521",TRUE, getMilliFromHMS(3,15,55,TRUE,1,41,51), getMilliFromHMS(3,16,12,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_42_22)", TRUE, "0521",TRUE, getMilliFromHMS(3,42,20,TRUE,1,41,51), getMilliFromHMS(3,42,32,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_55_20)", TRUE, "0521",TRUE, getMilliFromHMS(3,55,15,TRUE,1,41,51), getMilliFromHMS(3,55,30,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(4_21_17)", TRUE, "0521",TRUE, getMilliFromHMS(4,21,10,TRUE,1,41,51), getMilliFromHMS(4,21,35,TRUE,1,41,51))

data_watch_0521_walking_1_57_38 <- read.table("./data_raw/scratching_data0521(1_57_38).txt",sep=",",header=TRUE)
data_watch_0521_walking_3_14_16 <- read.table("./data_raw/scratching_data0521(3_14_16).txt",sep=",",header=TRUE)
data_watch_0521_walking_3_15_58 <- read.table("./data_raw/scratching_data0521(3_15_58).txt",sep=",",header=TRUE)
data_watch_0521_walking_3_42_22 <- read.table("./data_raw/scratching_data0521(3_42_22).txt",sep=",",header=TRUE)
data_watch_0521_walking_3_55_20 <- read.table("./data_raw/scratching_data0521(3_55_20).txt",sep=",",header=TRUE)
data_watch_0521_walking_4_21_17 <- read.table("./data_raw/scratching_data0521(4_21_17).txt",sep=",",header=TRUE)

detectScratchMovs(data_watch_0521_walking_1_57_38 , 8, "scratching_data0521(1_57_38)", 50, 25, smodel3)
detectScratchMovs(data_watch_0521_walking_3_14_16 , 8, "scratching_data0521(3_14_16)", 50, 25, smodel3)
detectScratchMovs(data_watch_0521_walking_3_15_58 , 8, "scratching_data0521(3_15_58)", 50, 25, smodel3)
detectScratchMovs(data_watch_0521_walking_3_42_22 , 8, "scratching_data0521(3_42_22)", 50, 25, smodel3)
detectScratchMovs(data_watch_0521_walking_3_55_20 , 8, "scratching_data0521(3_55_20)", 50, 25, smodel3)
detectScratchMovs(data_watch_0521_walking_4_21_17 , 8, "scratching_data0521(4_21_17)", 50, 25, smodel3)


## turning over
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(2_45_02)", TRUE, "0521",TRUE, getMilliFromHMS(2,44,55,TRUE,1,41,51), getMilliFromHMS(2,45,15,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_07_46)", TRUE, "0521",TRUE, getMilliFromHMS(3,07,36,TRUE,1,41,51), getMilliFromHMS(3,08,00,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_38_20)", TRUE, "0521",TRUE, getMilliFromHMS(3,38,15,TRUE,1,41,51), getMilliFromHMS(3,38,30,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(4_40_18)", TRUE, "0521",TRUE, getMilliFromHMS(4,40,13,TRUE,1,41,51), getMilliFromHMS(4,40,33,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(4_53_23)", TRUE, "0521",TRUE, getMilliFromHMS(4,53,18,TRUE,1,41,51), getMilliFromHMS(4,53,33,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(5_12_04)", TRUE, "0521",TRUE, getMilliFromHMS(5,12,00,TRUE,1,41,51), getMilliFromHMS(5,12,14,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(5_23_31)", TRUE, "0521",TRUE, getMilliFromHMS(5,23,26,TRUE,1,41,51), getMilliFromHMS(5,23,45,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(5_42_37)", TRUE, "0521",TRUE, getMilliFromHMS(5,42,37,TRUE,1,41,51), getMilliFromHMS(5,42,48,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(6_47_09)", TRUE, "0521",TRUE, getMilliFromHMS(6,47,08,TRUE,1,41,51), getMilliFromHMS(6,47,24,TRUE,1,41,51))

## stretching
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(2_07_47)", TRUE, "0521",TRUE, getMilliFromHMS(2,07,35,TRUE,1,41,51), getMilliFromHMS(2,07,58,TRUE,1,41,51))

## quick movements (10x)
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(2_12_43)", TRUE, "0521",TRUE, getMilliFromHMS(2,12,38,TRUE,1,41,51), getMilliFromHMS(2,12,53,TRUE,1,41,51))





# 0520 scratch file creating!!

createPlot(data_watch_intentservice1_05_20_jongin, 8, "scratching_data0520(2_30_41)", TRUE, "0520",TRUE, getMilliFromHMS(2,30,35,TRUE,2,15,38), getMilliFromHMS(2,31,0,TRUE,2,15,38))
createPlot(data_watch_intentservice1_05_20_jongin, 8, "scratching_data0520(06_07_00)", TRUE, "0520",TRUE, getMilliFromHMS(6,6,55,TRUE,2,15,38), getMilliFromHMS(6,7,20,TRUE,2,15,38))
createPlot(data_watch_intentservice1_05_20_jongin, 8, "scratching_data0520(06_11_24)", TRUE, "0520",TRUE, getMilliFromHMS(6,11,20,TRUE,2,15,38), getMilliFromHMS(6,11,40,TRUE,2,15,38))

test_data <- getDataset(c(
  "scratching_data0521(1_57_38)", 
  "scratching_data0521(3_14_16)",
  "scratching_data0521(3_15_58)",
  "scratching_data0521(3_42_22)",
  "scratching_data0521(3_55_20)",
  "scratching_data0521(4_21_17)",
  "scratching_data0520(2_30_41)",
  "scratching_data0520(06_07_00)",
  "scratching_data0520(06_11_24)"
), NULL,8, window_size, window_step, FALSE)

filelist <- c(
  "scratching_data0521(1_57_38)", 
  "scratching_data0521(3_14_16)",
  "scratching_data0521(3_15_58)",
  "scratching_data0521(3_42_22)",
  "scratching_data0521(3_55_20)",
  "scratching_data0521(4_21_17)",
  "scratching_data0520(2_30_41)",
  "scratching_data0520(06_07_00)",
  "scratching_data0520(06_11_24)"
)


for(i in 1: length(filelist))
{
  name <- filelist[i]
  data <- read.table(paste("./data_raw/", name ,".txt",sep=""),sep=",",header=TRUE)
  detectScratchMovs(data , 8, name, window_size, window_step, smodel3)
}


detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0521(1_57_38)", window_size, window_step, smodel3)
detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0521(3_14_16)", window_size, window_step, smodel3)
detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0521(3_15_58)", window_size, window_step, smodel3)
detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0521(3_42_22)", window_size, window_step, smodel3)
detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0521(3_55_20)", window_size, window_step, smodel3)
detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0521(4_21_17)", window_size, window_step, smodel3)
detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0520(2_30_41)", window_size, window_step, smodel3)
detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0520(06_07_00)", window_size, window_step, smodel3)
detectScratchMovs(scratching_data0521.1.57.38 , 8, "scratching_data0520(06_11_24)", window_size, window_step, smodel3)

labelname <- "data_watch20150413_eunji_scratch"
window_size <- 150
window_stp <- 50
plotting <- FALSE
delay <- 1
idx <- 8

data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
doSimulation(data, TRUE, idx, window_size, window_stp, labelname, plotting,delay=delay)
t<-autolabeling(paste(labelname,".csv",sep=""), "scratch")

filter <- "scratch|non|scratch_finger"
sum_data <- subset(t, grepl(filter, t$label))

View(sum_data)
sum_data <- sum_data[,5:length(sum_data)]

View(sum_data)

write.csv(sum_data, file=paste("./data_raw/testsetForWeka.txt",sep=""), row.names=FALSE)

================================================================================================================

t<-doSimulationAllFeatures(data_watch20150413_seungho , TRUE, 8, 150, 50, "labelname", TRUE,delay=1,startMilli = 32000, endMilli = 5000)
t<-doSimulationAllFeatures(data_watch20150413_seungho_3 , TRUE, 8, 150, 50, "labelname", TRUE,delay=1,startMilli = 38000, endMilli = 5000)

t<-doSimulationAllFeatures(data_watch20150417_scratchingtest1 , TRUE, 8, 150, 50, "labelname", TRUE,delay=1,startMilli = 20*1000)
t<-doSimulationAllFeatures(data_watch20150417_scratchingtest2 , TRUE, 8, 150, 50, "labelname", TRUE,delay=1,startMilli = 20*1000,endMilli = 6000)
t<-doSimulationAllFeatures(data_watch20150417_scratchingtest3, TRUE, 8, 150, 50, "labelname", TRUE,delay=1,startMilli = 20*1000,endMilli = 6000)
t<-doSimulationAllFeatures(data_watch20150417_scratchingtest4, TRUE, 8, 150, 50, "labelname", TRUE,delay=1,startMilli = 15*1000,endMilli = 6000)

for(i in c(1,3,8))
{
  tt<- getDataset2(c(
    "data_watch20150412_scratching_x",TRUE, 2000, 2000,
    "data_watch20150412_scratching_xy", TRUE, 2000, 2000,
    "data_watch20150412_scratching_y",TRUE, 2000, 2000,
    "data_watch20150412_scratching_z",TRUE, 2000, 2000,
    "data_watch20150413_eunji_scratch",TRUE, 2000, 2000,
    "data_watch20150413_eunji_scratch_2",TRUE, 2000, 2000,
    "data_watch20150413_seungho",TRUE,32000,5000,
    "data_watch20150413_seungho_3",TRUE,38000,5000,
    "data_watch20150412_scratching_no_wrist",TRUE, 2000, 2000,
    "data_watch20150412_scratching_no_wrist_2",TRUE, 2000, 2000,
    "data_watch20150416_scratching_no_wrist_3",TRUE, 2000, 2000,
    "data_watch20150416_scratching_no_wrist_4",TRUE, 2000, 2000,
    "data_watch20150417_scratchingtest1",TRUE, 20000,2000,
    "data_watch20150417_scratchingtest2",TRUE, 20000,6000,
    "data_watch20150417_scratchingtest3",TRUE, 20000,6000,
    "data_watch20150417_scratchingtest4",TRUE, 15000,6000
  ), c(
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
  ),i, 150, 50, TRUE, NULL)
  
  if(i=='1'){
    sum_data <- tt[,5:length(tt)]
  }else{
    sum_data <- c(sum_data,tt[,6:length(tt)])
  }
  
  print(paste("index : ",i, "len : ",nrow(tt) ))
  
}


write.csv(sum_data, file=paste("./data_raw/testsetForWeka7.arff",sep=""), row.names=FALSE)

selected <- sum_data[c(1,49,50,51,82,83,84,88,111,112,113,123,124,125)]
data<-as.data.frame(selected)
data$label <- factor(data$label)

ML_method <- "J48"
train_control <- trainControl(method="cv", number=10)
model <- train(label~., data=data, trControl = train_control,  method=ML_method)




================================================================================================================

t<-doSimulationAllFeatures(data_watch20150412_scratching_x , TRUE, 2, window_size, window_stp, "labelname", FALSE,delay=1)

sum_data <- tt[,5:length(tt)]

View(sum_data)

write.csv(sum_data, file=paste("./data_raw/testsetForWeka3.arff",sep=""), row.names=FALSE)
====================================
  
@RELATION data_watch20150523

@ATTRIBUTE label {non, scratch, scratch_finger}
@ATTRIBUTE accel_mean_x NUMERIC
@ATTRIBUTE accel_mean_y NUMERIC
@ATTRIBUTE accel_mean_z NUMERIC
@ATTRIBUTE accel_max_x NUMERIC
@ATTRIBUTE accel_max_y NUMERIC
@ATTRIBUTE accel_max_z NUMERIC
@ATTRIBUTE accel_min_x NUMERIC
@ATTRIBUTE accel_min_y NUMERIC
@ATTRIBUTE accel_min_z NUMERIC
@ATTRIBUTE accel_entropy_x NUMERIC
@ATTRIBUTE accel_entropy_y NUMERIC
@ATTRIBUTE accel_entropy_z NUMERIC
@ATTRIBUTE accel_energy_x NUMERIC
@ATTRIBUTE accel_energy_y NUMERIC
@ATTRIBUTE accel_energy_z NUMERIC
@ATTRIBUTE accel_cor_x NUMERIC
@ATTRIBUTE accel_cor_y NUMERIC
@ATTRIBUTE accel_cor_z NUMERIC
@ATTRIBUTE accel_autocor1_x NUMERIC
@ATTRIBUTE accel_autocor1_y NUMERIC
@ATTRIBUTE accel_autocor1_z NUMERIC
@ATTRIBUTE accel_th_x NUMERIC
@ATTRIBUTE accel_th_y NUMERIC
@ATTRIBUTE accel_th_z NUMERIC
@ATTRIBUTE accel_autocor2_x NUMERIC
@ATTRIBUTE accel_autocor2_y NUMERIC
@ATTRIBUTE accel_autocor2_z NUMERIC
@ATTRIBUTE accel_var_x NUMERIC
@ATTRIBUTE accel_var_y NUMERIC
@ATTRIBUTE accel_var_z NUMERIC
@ATTRIBUTE accel_preakfreq_x NUMERIC
@ATTRIBUTE accel_preakfreq_y NUMERIC
@ATTRIBUTE accel_preakfreq_z NUMERIC
@ATTRIBUTE accel_mean_mag NUMERIC
@ATTRIBUTE accel_max_mag NUMERIC
@ATTRIBUTE accel_min_mag NUMERIC
@ATTRIBUTE accel_entropy_mag NUMERIC
@ATTRIBUTE accel_energy_mag NUMERIC
@ATTRIBUTE accel_autocor1_mag NUMERIC
@ATTRIBUTE accel_th_mag NUMERIC
@ATTRIBUTE accel_autocor2_mag NUMERIC
@ATTRIBUTE accel_var_mag NUMERIC
@ATTRIBUTE accel_preakfreq_mag NUMERIC
@ATTRIBUTE magnet_mean_x NUMERIC
@ATTRIBUTE magnet_mean_y NUMERIC
@ATTRIBUTE magnet_mean_z NUMERIC
@ATTRIBUTE magnet_max_x NUMERIC
@ATTRIBUTE magnet_max_y NUMERIC
@ATTRIBUTE magnet_max_z NUMERIC
@ATTRIBUTE magnet_min_x NUMERIC
@ATTRIBUTE magnet_min_y NUMERIC
@ATTRIBUTE magnet_min_z NUMERIC
@ATTRIBUTE magnet_entropy_x NUMERIC
@ATTRIBUTE magnet_entropy_y NUMERIC
@ATTRIBUTE magnet_entropy_z NUMERIC
@ATTRIBUTE magnet_energy_x NUMERIC
@ATTRIBUTE magnet_energy_y NUMERIC
@ATTRIBUTE magnet_energy_z NUMERIC
@ATTRIBUTE magnet_cor_x NUMERIC
@ATTRIBUTE magnet_cor_y NUMERIC
@ATTRIBUTE magnet_cor_z NUMERIC
@ATTRIBUTE magnet_autocor1_x NUMERIC
@ATTRIBUTE magnet_autocor1_y NUMERIC
@ATTRIBUTE magnet_autocor1_z NUMERIC
@ATTRIBUTE magnet_th_x NUMERIC
@ATTRIBUTE magnet_th_y NUMERIC
@ATTRIBUTE magnet_th_z NUMERIC
@ATTRIBUTE magnet_autocor2_x NUMERIC
@ATTRIBUTE magnet_autocor2_y NUMERIC
@ATTRIBUTE magnet_autocor2_z NUMERIC
@ATTRIBUTE magnet_var_x NUMERIC
@ATTRIBUTE magnet_var_y NUMERIC
@ATTRIBUTE magnet_var_z NUMERIC
@ATTRIBUTE magnet_preakfreq_x NUMERIC
@ATTRIBUTE magnet_preakfreq_y NUMERIC
@ATTRIBUTE magnet_preakfreq_z NUMERIC
@ATTRIBUTE magnet_mean_mag NUMERIC
@ATTRIBUTE magnet_max_mag NUMERIC
@ATTRIBUTE magnet_min_mag NUMERIC
@ATTRIBUTE magnet_entropy_mag NUMERIC
@ATTRIBUTE magnet_energy_mag NUMERIC
@ATTRIBUTE magnet_autocor1_mag NUMERIC
@ATTRIBUTE magnet_th_mag NUMERIC
@ATTRIBUTE magnet_autocor2_mag NUMERIC
@ATTRIBUTE magnet_var_mag NUMERIC
@ATTRIBUTE magnet_preakfreq_mag NUMERIC
@ATTRIBUTE gyro_mean_x NUMERIC
@ATTRIBUTE gyro_mean_y NUMERIC
@ATTRIBUTE gyro_mean_z NUMERIC
@ATTRIBUTE gyro_max_x NUMERIC
@ATTRIBUTE gyro_max_y NUMERIC
@ATTRIBUTE gyro_max_z NUMERIC
@ATTRIBUTE gyro_min_x NUMERIC
@ATTRIBUTE gyro_min_y NUMERIC
@ATTRIBUTE gyro_min_z NUMERIC
@ATTRIBUTE gyro_entropy_x NUMERIC
@ATTRIBUTE gyro_entropy_y NUMERIC
@ATTRIBUTE gyro_entropy_z NUMERIC
@ATTRIBUTE gyro_energy_x NUMERIC
@ATTRIBUTE gyro_energy_y NUMERIC
@ATTRIBUTE gyro_energy_z NUMERIC
@ATTRIBUTE gyro_cor_x NUMERIC
@ATTRIBUTE gyro_cor_y NUMERIC
@ATTRIBUTE gyro_cor_z NUMERIC
@ATTRIBUTE gyro_autocor1_x NUMERIC
@ATTRIBUTE gyro_autocor1_y NUMERIC
@ATTRIBUTE gyro_autocor1_z NUMERIC
@ATTRIBUTE gyro_th_x NUMERIC
@ATTRIBUTE gyro_th_y NUMERIC
@ATTRIBUTE gyro_th_z NUMERIC
@ATTRIBUTE gyro_autocor2_x NUMERIC
@ATTRIBUTE gyro_autocor2_y NUMERIC
@ATTRIBUTE gyro_autocor2_z NUMERIC
@ATTRIBUTE gyro_var_x NUMERIC
@ATTRIBUTE gyro_var_y NUMERIC
@ATTRIBUTE gyro_var_z NUMERIC
@ATTRIBUTE gyro_preakfreq_x NUMERIC
@ATTRIBUTE gyro_preakfreq_y NUMERIC
@ATTRIBUTE gyro_preakfreq_z NUMERIC
@ATTRIBUTE gyro_mean_mag NUMERIC
@ATTRIBUTE gyro_max_mag NUMERIC
@ATTRIBUTE gyro_min_mag NUMERIC
@ATTRIBUTE gyro_entropy_mag NUMERIC
@ATTRIBUTE gyro_energy_mag NUMERIC
@ATTRIBUTE gyro_autocor1_mag NUMERIC
@ATTRIBUTE gyro_th_mag NUMERIC
@ATTRIBUTE gyro_autocor2_mag NUMERIC
@ATTRIBUTE gyro_var_mag NUMERIC
@ATTRIBUTE gyro_preakfreq_mag NUMERIC
@ATTRIBUTE orientation_mean_x NUMERIC
@ATTRIBUTE orientation_mean_y NUMERIC
@ATTRIBUTE orientation_mean_z NUMERIC
@ATTRIBUTE orientation_max_x NUMERIC
@ATTRIBUTE orientation_max_y NUMERIC
@ATTRIBUTE orientation_max_z NUMERIC
@ATTRIBUTE orientation_min_x NUMERIC
@ATTRIBUTE orientation_min_y NUMERIC
@ATTRIBUTE orientation_min_z NUMERIC
@ATTRIBUTE orientation_entropy_x NUMERIC
@ATTRIBUTE orientation_entropy_y NUMERIC
@ATTRIBUTE orientation_entropy_z NUMERIC
@ATTRIBUTE orientation_energy_x NUMERIC
@ATTRIBUTE orientation_energy_y NUMERIC
@ATTRIBUTE orientation_energy_z NUMERIC
@ATTRIBUTE orientation_cor_x NUMERIC
@ATTRIBUTE orientation_cor_y NUMERIC
@ATTRIBUTE orientation_cor_z NUMERIC
@ATTRIBUTE orientation_autocor1_x NUMERIC
@ATTRIBUTE orientation_autocor1_y NUMERIC
@ATTRIBUTE orientation_autocor1_z NUMERIC
@ATTRIBUTE orientation_th_x NUMERIC
@ATTRIBUTE orientation_th_y NUMERIC
@ATTRIBUTE orientation_th_z NUMERIC
@ATTRIBUTE orientation_autocor2_x NUMERIC
@ATTRIBUTE orientation_autocor2_y NUMERIC
@ATTRIBUTE orientation_autocor2_z NUMERIC
@ATTRIBUTE orientation_var_x NUMERIC
@ATTRIBUTE orientation_var_y NUMERIC
@ATTRIBUTE orientation_var_z NUMERIC
@ATTRIBUTE orientation_preakfreq_x NUMERIC
@ATTRIBUTE orientation_preakfreq_y NUMERIC
@ATTRIBUTE orientation_preakfreq_z NUMERIC
@ATTRIBUTE orientation_mean_mag NUMERIC
@ATTRIBUTE orientation_max_mag NUMERIC
@ATTRIBUTE orientation_min_mag NUMERIC
@ATTRIBUTE orientation_entropy_mag NUMERIC
@ATTRIBUTE orientation_energy_mag NUMERIC
@ATTRIBUTE orientation_autocor1_mag NUMERIC
@ATTRIBUTE orientation_th_mag NUMERIC
@ATTRIBUTE orientation_autocor2_mag NUMERIC
@ATTRIBUTE orientation_var_mag NUMERIC
@ATTRIBUTE orientation_preakfreq_mag NUMERIC
@ATTRIBUTE linearaccel_mean_x NUMERIC
@ATTRIBUTE linearaccel_mean_y NUMERIC
@ATTRIBUTE linearaccel_mean_z NUMERIC
@ATTRIBUTE linearaccel_max_x NUMERIC
@ATTRIBUTE linearaccel_max_y NUMERIC
@ATTRIBUTE linearaccel_max_z NUMERIC
@ATTRIBUTE linearaccel_min_x NUMERIC
@ATTRIBUTE linearaccel_min_y NUMERIC
@ATTRIBUTE linearaccel_min_z NUMERIC
@ATTRIBUTE linearaccel_entropy_x NUMERIC
@ATTRIBUTE linearaccel_entropy_y NUMERIC
@ATTRIBUTE linearaccel_entropy_z NUMERIC
@ATTRIBUTE linearaccel_energy_x NUMERIC
@ATTRIBUTE linearaccel_energy_y NUMERIC
@ATTRIBUTE linearaccel_energy_z NUMERIC
@ATTRIBUTE linearaccel_cor_x NUMERIC
@ATTRIBUTE linearaccel_cor_y NUMERIC
@ATTRIBUTE linearaccel_cor_z NUMERIC
@ATTRIBUTE linearaccel_autocor1_x NUMERIC
@ATTRIBUTE linearaccel_autocor1_y NUMERIC
@ATTRIBUTE linearaccel_autocor1_z NUMERIC
@ATTRIBUTE linearaccel_th_x NUMERIC
@ATTRIBUTE linearaccel_th_y NUMERIC
@ATTRIBUTE linearaccel_th_z NUMERIC
@ATTRIBUTE linearaccel_autocor2_x NUMERIC
@ATTRIBUTE linearaccel_autocor2_y NUMERIC
@ATTRIBUTE linearaccel_autocor2_z NUMERIC
@ATTRIBUTE linearaccel_var_x NUMERIC
@ATTRIBUTE linearaccel_var_y NUMERIC
@ATTRIBUTE linearaccel_var_z NUMERIC
@ATTRIBUTE linearaccel_preakfreq_x NUMERIC
@ATTRIBUTE linearaccel_preakfreq_y NUMERIC
@ATTRIBUTE linearaccel_preakfreq_z NUMERIC
@ATTRIBUTE linearaccel_mean_mag NUMERIC
@ATTRIBUTE linearaccel_max_mag NUMERIC
@ATTRIBUTE linearaccel_min_mag NUMERIC
@ATTRIBUTE linearaccel_entropy_mag NUMERIC
@ATTRIBUTE linearaccel_energy_mag NUMERIC
@ATTRIBUTE linearaccel_autocor1_mag NUMERIC
@ATTRIBUTE linearaccel_th_mag NUMERIC
@ATTRIBUTE linearaccel_autocor2_mag NUMERIC
@ATTRIBUTE linearaccel_var_mag NUMERIC
@ATTRIBUTE linearaccel_preakfreq_mag NUMERIC

@DATA


=========
  

@RELATION data_watch20150412


@ATTRIBUTE label {non, scratch}
@ATTRIBUTE mean_x NUMERIC
@ATTRIBUTE mean_y NUMERIC
@ATTRIBUTE mean_z NUMERIC
@ATTRIBUTE entropy_x NUMERIC
@ATTRIBUTE entropy_y NUMERIC
@ATTRIBUTE entropy_z NUMERIC
@ATTRIBUTE cor_x NUMERIC
@ATTRIBUTE cor_y NUMERIC
@ATTRIBUTE cor_z NUMERIC
@ATTRIBUTE autocor_x NUMERIC
@ATTRIBUTE autocor_y NUMERIC
@ATTRIBUTE autocor_z NUMERIC
@ATTRIBUTE th_x NUMERIC
@ATTRIBUTE th_y NUMERIC
@ATTRIBUTE th_z NUMERIC
@ATTRIBUTE autocor_x2 NUMERIC
@ATTRIBUTE autocor_y2 NUMERIC
@ATTRIBUTE autocor_z2 NUMERIC
@ATTRIBUTE preakfreq_x NUMERIC
@ATTRIBUTE preakfreq_y NUMERIC
@ATTRIBUTE preakfreq_z NUMERIC

@DATA


# 

save.image("~/workspace/patbingsuR/rworkspace.RData")
