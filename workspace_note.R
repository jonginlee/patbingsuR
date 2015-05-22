
#history
library("caret", lib.loc="/usr/local/lib/R/site-library")
library("e1071", lib.loc="/usr/local/lib/R/site-library")
library("tuneR", lib.loc="/usr/local/lib/R/site-library")
library("RWeka", lib.loc="/usr/local/lib/R/site-library")
library("signal", lib.loc="/usr/local/lib/R/site-library")


# 0521 scratch file creating!!

createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(1_57_38)", TRUE, "0521",TRUE, getMilliFromHMS(1,57,30,TRUE,1,41,51), getMilliFromHMS(1,57,55,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_14_16)", TRUE, "0521",TRUE, getMilliFromHMS(3,14,13,TRUE,1,41,51), getMilliFromHMS(3,14,30,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_15_58)", TRUE, "0521",TRUE, getMilliFromHMS(3,15,55,TRUE,1,41,51), getMilliFromHMS(3,16,12,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_42_22)", TRUE, "0521",TRUE, getMilliFromHMS(3,42,20,TRUE,1,41,51), getMilliFromHMS(3,42,32,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_55_20)", TRUE, "0521",TRUE, getMilliFromHMS(3,55,15,TRUE,1,41,51), getMilliFromHMS(3,55,30,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(4_21_17)", TRUE, "0521",TRUE, getMilliFromHMS(4,21,10,TRUE,1,41,51), getMilliFromHMS(4,21,35,TRUE,1,41,51))


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

tt<- getDataset2(c(
  "data_watch20150412_scratching_x"
), c(
  "data_watch20150412_normal"
),8, window_size, window_stp, FALSE)



tt<- getDataset2(c(
  "data_watch20150412_scratching_x",
  "data_watch20150412_scratching_y",
  "data_watch20150412_scratching_z",
  "data_watch20150413_eunji_scratch",
  "data_watch20150413_eunji_scratch_2",
  "data_watch20150413_seungho",
  "data_watch20150413_seungho_3"
), c(
  "data_watch20150412_normal",
  "data_watch20150412_normal_2",
  "data_watch20150413_normal4",
  "data_watch20150413_normal3"
),8, window_size, window_stp, FALSE)

tt2<- getDataset2(c(
  "data_watch20150412_scratching_x",
  "data_watch20150412_scratching_y",
  "data_watch20150412_scratching_z",
  "data_watch20150413_eunji_scratch",
  "data_watch20150413_eunji_scratch_2",
  "data_watch20150413_seungho",
  "data_watch20150413_seungho_3"
), c(
  "data_watch20150412_normal",
  "data_watch20150412_normal_2",
  "data_watch20150413_normal4",
  "data_watch20150413_normal3"
),2, window_size, window_stp, FALSE)


for(i in c(1,2,3,8))
{
  tt<- getDataset2(c(
    "data_watch20150412_scratching_x",
    "data_watch20150412_scratching_y",
    "data_watch20150412_scratching_z",
    "data_watch20150413_eunji_scratch",
    "data_watch20150413_eunji_scratch_2",
    "data_watch20150413_seungho",
    "data_watch20150413_seungho_3"
  ), c(
    "data_watch20150412_normal",
    "data_watch20150412_normal_2",
    "data_watch20150413_normal4",
    "data_watch20150413_normal3"
  ),i, window_size, window_stp, FALSE)
  
  if(i=='1'){
    sum_data <- tt[,5:length(tt)]
  }else{
    sum_data <- c(sum_data,tt[,6:length(tt)])
  }

  print(paste("index : ",i, "len : ",nrow(tt) ))
}
write.csv(sum_data, file=paste("./data_raw/testsetForWeka3.arff",sep=""), row.names=FALSE)



t<-doSimulationAllFeatures(data_watch20150412_scratching_x , TRUE, 8, window_size, window_stp, "labelname", FALSE,delay=1)

sum_data <- tt[,5:length(tt)]

View(sum_data)

write.csv(sum_data, file=paste("./data_raw/testsetForWeka3.arff",sep=""), row.names=FALSE)
====================================
  
  @RELATION data_watch20150412


@ATTRIBUTE label {non, scratch}
@ATTRIBUTE mean_x NUMERIC
@ATTRIBUTE mean_y NUMERIC
@ATTRIBUTE mean_z NUMERIC
@ATTRIBUTE max_x NUMERIC
@ATTRIBUTE max_y NUMERIC
@ATTRIBUTE max_z NUMERIC
@ATTRIBUTE min_x NUMERIC
@ATTRIBUTE min_y NUMERIC
@ATTRIBUTE min_z NUMERIC
@ATTRIBUTE entropy_x NUMERIC
@ATTRIBUTE entropy_y NUMERIC
@ATTRIBUTE entropy_z NUMERIC
@ATTRIBUTE energy_x NUMERIC
@ATTRIBUTE energy_y NUMERIC
@ATTRIBUTE energy_z NUMERIC
@ATTRIBUTE cor_x NUMERIC
@ATTRIBUTE cor_y NUMERIC
@ATTRIBUTE cor_z NUMERIC
@ATTRIBUTE autocor1_x NUMERIC
@ATTRIBUTE autocor1_y NUMERIC
@ATTRIBUTE autocor1_z NUMERIC
@ATTRIBUTE th_x NUMERIC
@ATTRIBUTE th_y NUMERIC
@ATTRIBUTE th_z NUMERIC
@ATTRIBUTE autocor2_x NUMERIC
@ATTRIBUTE autocor2_y NUMERIC
@ATTRIBUTE autocor2_z NUMERIC
@ATTRIBUTE var_x NUMERIC
@ATTRIBUTE var_y NUMERIC
@ATTRIBUTE var_z NUMERIC
@ATTRIBUTE preakfreq_x NUMERIC
@ATTRIBUTE preakfreq_y NUMERIC
@ATTRIBUTE preakfreq_z NUMERIC
@ATTRIBUTE mean_mag NUMERIC
@ATTRIBUTE max_mag NUMERIC
@ATTRIBUTE min_mag NUMERIC
@ATTRIBUTE entropy_mag NUMERIC
@ATTRIBUTE energy_mag NUMERIC
@ATTRIBUTE autocor1_mag NUMERIC
@ATTRIBUTE th_mag NUMERIC
@ATTRIBUTE autocor2_mag NUMERIC
@ATTRIBUTE var_mag NUMERIC
@ATTRIBUTE preakfreq_mag NUMERIC

@DATA


================================
  

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
