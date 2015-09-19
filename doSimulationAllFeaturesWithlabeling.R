

for(varV in c("0.0001", "0.001", "0.01", "0.1","1","10"))
{
  varD <- as.double(varV)
  print(paste("varD ======>", varD))
  
  varD <- 0.1
  for(i in c(1,3,8))
  {
    if(i=='1'){
      tt<-doSimulationAllFeaturesWithLabeling(subject3_left_data, FALSE, i, 150, 50, s3_left_scr_info, "subject3_left_data", FALSE, thresholdvar = varD,labeling = TRUE)
      rownames(tt) <- NULL
      sum_data <- tt[,4:length(colnames(tt))]
    }else{
      tt<-doSimulationAllFeaturesWithLabeling(subject3_left_data, FALSE, i, 150, 50, s3_left_scr_info, "subject3_left_data", FALSE, thresholdvar = varD,labeling = FALSE)
      sum_data <- cbind(sum_data,tt[,5:length(colnames(tt))])
    }
  }
  
  write.csv(sum_data, paste("./data_csv/p3/p3leftdata_",varD,".csv",sep=""))
  
}


subject3_left_data <- read.table("./data_raw/p3//subject3_left.txt",sep=",",header=TRUE)


write.csv(sum_data, "./data_csv/p3/p3leftdata_0.424.csv")


subject2_left_data <- read.table("./data_raw/p2/subject2_left.txt",sep=",",header=TRUE)
s2_left_scr_info <-  read.csv("./data_raw/p2/subject2_left_scratch.csv",header=TRUE)

sum_data<-getLabeledData(subject2_left_data,s2_left_scr_info,"s2_left_scr_0915") # 0806
write.csv(sum_data, file=paste("./data_csv/p2/subject2_left_0915.csv",sep=""), row.names=F)

remove(subject2_left_data)


subject2_right_data <- read.table("./data_raw/p2/subject2_right.txt",sep=",",header=TRUE)
s2_right_scr_info <-  read.csv("./data_raw/p2/subject2_right_scratch.csv",header=TRUE)

sum_data<-getLabeledData(subject2_right_data,s2_right_scr_info,"s2_right_scr_0915")
write.csv(sum_data, file=paste("./data_csv/p2/subject2_right_0915.csv",sep=""), row.names=F)

remove(subject2_right_data)


subject3_left_data <- read.table("./data_raw/p3/subject3_left.txt",sep=",",header=TRUE)
s3_left_scr_info <-  read.csv("./data_raw/p3/subject3_left_scratch.csv",header=TRUE)

sum_data<-getLabeledData(subject3_left_data,s3_left_scr_info,"s3_left_scr_0915")
write.csv(sum_data, file=paste("./data_csv/p3/subject3_left_0915.csv",sep=""), row.names=F)

remove(subject3_left_data)


subject3_right_data <- read.table("./data_raw/p3/subject3_right.txt",sep=",",header=TRUE)
s3_right_scr_info <-  read.csv("./data_raw/p3/subject3_right_scratch.csv",header=TRUE)

sum_data<-getLabeledData(subject3_right_data,s3_right_scr_info,"s3_right_scr_0915")
write.csv(sum_data, file=paste("./data_csv/p3/subject3_right_0915.csv",sep=""), row.names=F)

remove(subject3_right_data)

###

subject4_right_data <- read.table("./subject_data/p4_gold/watch1_sensor_data.txt",sep=",",header=TRUE)
s4_right_scr_info <-  read.csv("./subject_data/p4_gold/scratch_log.csv",header=TRUE)

sum_data<-getLabeledData(subject4_right_data,s4_right_scr_info,"s4_right_scr_0912")
write.csv(sum_data, file=paste("./subject_data/p4_gold/subject4_right_0912.csv",sep=""), row.names=F)

remove(subject4_right_data)

subject4_left_data <- read.table("./subject_data/p4_silver/watch1_sensor_data.txt",sep=",",header=TRUE)
s4_left_scr_info <-  read.csv("./subject_data/p4_silver/scratch_log.csv",header=TRUE)

sum_data<-getLabeledData(subject4_left_data,s4_left_scr_info,"s4_right_scr_0912")
write.csv(sum_data, file=paste("./subject_data/p4_silver/subject4_right_0912.csv",sep=""), row.names=F)

remove(subject4_left_data)

###

subject8_right_data <- read.table("./subject_data/p8_gold/watch1_sensor_data.txt",sep=",",header=TRUE)
s8_right_scr_info <-  read.csv("./subject_data/p8_gold/scratch_log.csv",header=TRUE)

sum_data<-getLabeledData(subject8_right_data,s8_right_scr_info,"s4_right_scr_0913")
write.csv(sum_data, file=paste("./subject_data/p8_gold/subject8_right_0913.csv",sep=""), row.names=F)

remove(subject8_right_data)

subject8_left_data <- read.table("./subject_data/p8_silver/watch1_sensor_data.txt",sep=",",header=TRUE)
s8_left_scr_info <-  read.csv("./subject_data/p8_silver/scratch_log.csv",header=TRUE)

sum_data<-getLabeledData(subject8_left_data,s8_left_scr_info,"s4_left_scr_0913")
write.csv(sum_data, file=paste("./subject_data/p8_silver/subject8_left_0913.csv",sep=""), row.names=F)

remove(subject8_left_data)

#############

tes <- read.csv("./subject_data/scratch_log_total.csv", header=TRUE)
#View(tes)
length(unique(tes$sub_num))
sub_n <- 12

for(sub_n in unique(tes$sub_num)){
  
  tes_sub <- subset(tes, subset=(tes$sub_n == sub_n)) 
  tes_sub <- subset(tes_sub, subset=(tes_sub$data_type == "log"))
  tes_sub.right <- subset(tes_sub, subset=(tes_sub$watch == "right"))
  if(length(tes_sub.right$scrtime)>1)    
    for(i in 2:length(tes_sub.right$scrtime))
    {
      if( (tes_sub.right$scr_start[i] - tes_sub.right$scr_end[i-1]) < 3000 )
        print(paste("sub_n ", sub_n, tes_sub.right$watch[i], tes_sub.right$scr_start[i] , tes_sub.right$scr_end[i-1], (tes_sub.right$scr_start[i] - tes_sub.right$scr_end[i-1])))
    }
  
  tes_sub.left <- subset(tes_sub, subset=(tes_sub$watch == "left"))
  if(length(tes_sub.left$scrtime)>1)    
    for(i in 2:length(tes_sub.left$scrtime))
    {
      if( (tes_sub.left$scr_start[i] - tes_sub.left$scr_end[i-1]) < 3000 )
        print(paste("sub_n ", sub_n, tes_sub.left$watch[i], tes_sub.left$scr_start[i] , tes_sub.left$scr_end[i-1], (tes_sub.left$scr_start[i] - tes_sub.left$scr_end[i-1])))
    }
  
  #View(tes_sub)
  scr_n3 <- length(subset(tes_sub, subset=(tes_sub$scrtime > 3000))$scrtime)
  scr_n5 <- length(subset(tes_sub, subset=(tes_sub$scrtime > 5000))$scrtime)
  scr_n10 <- length(subset(tes_sub, subset=(tes_sub$scrtime > 10000))$scrtime)
  scr_n20 <- length(subset(tes_sub, subset=(tes_sub$scrtime > 20000))$scrtime)
  
#  print(paste("sub_n ", sub_n, "max ", max(tes_sub$scrtime), "min ", min(tes_sub$scrtime), "times ", length(tes_sub$scrtime)))
  print(paste(" sub_n",sub_n,"  - 3s:",scr_n3,", 5s:",scr_n5,", 10s:",scr_n10,", 20s:",scr_n20 ))
}


sub_n <- 9
for(i in 7)
{
  print(paste("i",i))
}

sub_n <- 8

cdate<-0917
for(sub_n in c(9)){
  tes2 <- subset(tes, subset=(tes$sub_num == sub_n))
  tes2
  tes2_right <- subset(tes2, subset=(tes2$watch == "right"))
  tes2_right_range <- subset(tes2_right, subset=(tes2_right$data_type == "meta"))
  right_data <- read.table(paste("./subject_data/p",sub_n,"_gold/watch1_sensor_data.txt",sep=""),sep=",",header=TRUE)
  DT <- as.data.table(right_data)
  right_data<-DT[ (as.numeric(as.character(tes2_right_range$st)) <= time) & (time <= as.numeric(as.character(tes2_right_range$end))) ]
  right_scr_info <- subset(tes2_right, subset=(tes2_right$data_type == "log"))
  sum_data<-getLabeledData(right_data,right_scr_info,paste("s",sub_n,"_right_scr_",cdate,sep=""))
  write.csv(sum_data, file=paste("./subject_data/p",sub_n,"_gold/subject",sub_n,"_right_",cdate,".csv",sep=""), row.names=F)
  remove(right_data)
  remove(DT)
  gc()

  
  tes2_left <- subset(tes2, subset=(tes2$watch == "left"))
  tes2_left_range<- subset(tes2_left, subset=(tes2_left$data_type == "meta"))
  left_data <- read.table(paste("./subject_data/p",sub_n,"_silver/watch1_sensor_data.txt",sep=""),sep=",",header=TRUE)
  DT <- as.data.table(left_data)

  left_data<-DT[ (as.numeric(as.character(tes2_left_range$st)) <= time) & (time <= as.numeric(as.character(tes2_left_range$end))) ]
  left_scr_info <- subset(tes2_left, subset=(tes2_left$data_type == "log"))
  sum_data<-getLabeledData(left_data,left_scr_info,paste("s",sub_n,"_left_scr_",cdate,sep=""))
  write.csv(sum_data, file=paste("./subject_data/p",sub_n,"_silver/subject",sub_n,"_left_",cdate,".csv",sep=""), row.names=F)
  remove(left_data)
  remove(DT)
  gc()
}



View(sum_data)
remove(sum_data)


getLabeledData<-function(raw_data, label_info, title , threshold = 0.01)
{
  for(i in c(3,8))
  {
    tt<-doSimulationAllFeaturesWithLabeling4(raw_data, FALSE, i, 150, 50, label_info, title, FALSE, thresholdvar = threshold)
    rownames(tt) <- NULL
    if(i==3){
      sum_data <- tt[,1:length(colnames(tt))]
    }else{
      sum_data <- cbind(sum_data,tt[,5:length(colnames(tt))])
    }
  }
  
  return(sum_data)
}





write.csv(sum_data, file=paste("./data_csv/p3/subject3_left_data0804.csv",sep=""), row.names=F)
View(sum_data)
test<-read.csv("./data_csv/p3/subject3_data_gyro_max_11.csv",header=TRUE)
View(test)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0804.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0804.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0804.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0804.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0805.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0805.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0805.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0805.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0806.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0806.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0806.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0806.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0806v2.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0806v2.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0806v2.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0806v2.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data08010.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data08010.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data08010.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data08010.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data08010v2.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data08010v2.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data08010v2.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data08010v2.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data08017.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data08017.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data08017.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data08017.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data08018.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data08018.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data08018.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data08018.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data08019.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data08019.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data08019.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data08019.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data08024.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data08024.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data08024.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data08024.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data08024v2.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data08024v2.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data08024v2.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data08024v2.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data08024v3.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data08024v3.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data08024v3.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data08024v3.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0826v3.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0826.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0826.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0826csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0826_final.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0826_final.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0826_final.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0826_final.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0826_final_nofilter.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0826_final_nofilter.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0826_final_nofilter.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0826_final_nofilter.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0826_final_nofilter_step1.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0826_final_nofilter_step1.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0826_final_step1.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0826_final_nofilter_step1.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0826_final_nofilter2.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0826_final_nofilter2.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0826_final_nofilter2.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0826_final_nofilter2.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0826_final_nofilter3.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0826_final_nofilter3.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0826_final_nofilter3.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0826_final_nofilter3.csv",header=TRUE)


s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0828_butterworth0.8_12.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0828_butterworth0.8_12.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0828_butterworth0.8_12.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0828_butterworth0.8_12.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0828_butterworth0.8_12_smooth.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0828_butterworth0.8_12_smooth.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0828_butterworth0.8_12_smooth.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0828_butterworth0.8_12_smooth.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0828_butterworth0_12_mag.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0828_butterworth0_12_mag.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0828_butterworth0_12_mag.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0828_butterworth0_12_mag.csv",header=TRUE)


s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0828_butterworth0_12_PC.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0828_butterworth0_12_PC.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0828_butterworth0_12_PC.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0828_butterworth0_12_PC.csv",header=TRUE)


s3_left_csv<-read.csv("./data_csv/p3/subject3_left_data0828_butterworth0_12_rot.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_data0828_butterworth0_12_rot.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_data0828_butterworth0_12_rot.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_data0828_butterworth0_12_rot.csv",header=TRUE)


s3_left_csv<-read.csv("./data_csv/p3/subject3_left_0905_PCA3D.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_0905_PCA3D.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_0905_PCA3D.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_0905_PCA3D.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_0906_PCA3D.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_0906_PCA3D.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_0906_PCA3D.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_0906_PCA3D.csv",header=TRUE)

s3_left_csv<-read.csv("./data_csv/p3/subject3_left_0910.csv",header=TRUE)
s3_right_csv<-read.csv("./data_csv/p3/subject3_right_0910.csv",header=TRUE)
s2_left_csv<-read.csv("./data_csv/p2/subject2_left_0910.csv",header=TRUE)
s2_right_csv<-read.csv("./data_csv/p2/subject2_right_0910.csv",header=TRUE)

s3_left_csv<-read.csv("./subject_data/p3_gold/subject3_right_915.csv",header=TRUE)
s3_right_csv<-read.csv("./subject_data/p3_silver/subject3_left_915.csv",header=TRUE)
s2_left_csv<-read.csv("./subject_data/p2_gold/subject2_right_915.csv",header=TRUE)
s2_right_csv<-read.csv("./subject_data/p2_silver/subject2_left_915.csv",header=TRUE)


s4_right_csv <- read.csv("./subject_data/p4_gold//subject4_right_0912.csv",header=TRUE)

s8_right_csv <- read.csv("./subject_data/p8_gold/subject8_right_0913.csv",header=TRUE)
s8_left_csv <- read.csv("./subject_data/p8_silver/subject8_left_0913.csv",header=TRUE)

s8_right_csv <- read.csv("./subject_data/extracted/subject8_left_915.csv",header=TRUE)
s8_left_csv <- read.csv("./subject_data/extracted/subject8_right_915.csv",header=TRUE)

###

hist(temp)


csv_data <- s2_left_csv
csv_data <- s2_right_csv
csv_data <- s3_left_csv
csv_data <- s3_right_csv
csv_data <- s4_right_csv
csv_data <- s8_right_csv
csv_data <- s8_left_csv
#temp <- csv_data$Linearaccel_var_x.PC./csv_data$Linearaccel_var_y.PC.

test_data$X<-NULL
View(test_data)
csv_data <- test_data
csv_data <- as.data.frame(csv_data)

View(csv_data)
hist(temp)
#length(temp)
#View( csv_data[1,c(1+3,(267+3):(302+3),(379+3):(414+3))])
View(csv_data[c(1+3,(72+3):(83+3),(282+3):(293+3))])

View(csv_data[c(1+3,(212+3):(221+3),(282+3):(293+3))])


i<-1

predicted_label<-list()
temp <- csv_data$Linearaccel_variance_avg.PC_x./((csv_data$Linearaccel_variance_avg.PC_y. + csv_data$Linearaccel_variance_avg.PC_z.)/2)  
#temp <- csv_data$Linearaccel_variance_avg.PC_x./((csv_data$Linearaccel_variance_avg.PC_y.))
for(i in 1:length(temp))
{
#  print(i)
#  if(temp[i]>1){
    
  
  #predicted_label$model2_pred[i] <- predict(sim_3Dmodel2, csv_data[i, c(1,(92+3):(127+3), (302+3):(337+3))])
  
  #  predicted_label$model_combined[i] <- predict(sim_3Dmodel2, csv_data[i, c(1+3,(152+3):(187+3),(362+3):(397+3))])
#    predicted_label$model2_pred[i] <- predict(sim_3Dmodel2, csv_data[i, c(1+3,(152+3):(187+3),(362+3):(397+3))])
    
#    predicted_label$model2_pred[i] <- predict(sim_3Dmodel2, csv_data[i, c(1+3,(92+3):(127+3),(302+3):(337+3))])
    predicted_label$model2_pred[i] <- predict(sim_3Dmodel2, csv_data[i, c(1+3,(51+3):(56+3), (60+3):(65+3), (69+3):(71+3), (152+3):(187+3), (261+3):(266+3), (270+3):(275+3), (270+3):(281+3), (362+3):(397+3))])
    
    #predicted_label[i] <- predict(sim_3Dmodel2, csv_data[i,c(1+3,(267+3):(302+3),(379+3):(414+3))])
#    print(paste("temp>2", csv_data$label[i]," / prediction ", predicted_label$model_combined[i]))    
#  }else{
    #predicted_label[i] <- predict(sim_1Dmodel1, csv_data[i, c(1+3,(2+3):(11+3),(72+3):(83+3),(212+3):(221+3),(282+3):(293+3))])
#    predicted_label$model_combined[i] <- predict(sim_1Dmodel1, csv_data[i,c(1+3,(72+3):(83+3))])
#predicted_label$model1_pred[i] <- predict(sim_1Dmodel1, csv_data[i,c(1+3,(72+3):(83+3))])

    predicted_label$model1_pred[i] <- predict(sim_1Dmodel1, csv_data[i, c(1+3,(152+3):(187+3),(362+3):(397+3))])
#    predicted_label$model1_pred[i] <- predict(sim_1Dmodel1, csv_data[i,c(1+3,5+3,6+3,8+3,9+3,11+3,(72+3):(83+3),215+3,216+3,218+3,219+3,221+3 )])

    
#    predicted_label[i] <- predict(sim_1Dmodel1, csv_data[i,c(1+3,(2+3):(13+3),(86+3):(97+3),(219+3):(230+3),(303+3):(314+3))])
    #predicted_label[i] <- predict(sim_1Dmodel1, csv_data[i,c(1+3,(2+3):(13+3),(86+3):(97+3))])    
#    print(paste("temp<=2", csv_data$label[i]," / prediction ", predicted_label$model_combined[i]))
#  }
    #print(paste("temp<=2", csv_data$label[i]," / prediction ", predicted_label[i]))
    print(paste( csv_data$start_milli[i], csv_data$end_milli[i], csv_data$label[i],"temp(",temp[i],")", "temp>1", predicted_label$model2_pred[i], ", all ", predicted_label$model1_pred[i] ))
        
    if( (predicted_label$model1_pred[i] == 2) & (predicted_label$model2_pred[i] == 2) )
      predicted_label$model_combined[i] <- 2
    else if( (predicted_label$model1_pred[i] == 2) & (predicted_label$model2_pred[i] == 1) )
      predicted_label$model_combined[i] <- 2
    else if( (predicted_label$model1_pred[i] == 1) & (predicted_label$model2_pred[i] == 2) )
      predicted_label$model_combined[i] <- 2
    else if( (predicted_label$model1_pred[i] == 1) & (predicted_label$model2_pred[i] == 1) )
      predicted_label$model_combined[i] <- 1

#  }
}
#View(predicted_label)

predicted_label$model1_pred <- factor(predicted_label$model1_pred,labels=c("non","scratch"))
predicted_label$model2_pred <- factor(predicted_label$model2_pred,labels=c("non","scratch"))
predicted_label$model_combined <- factor(predicted_label$model_combined,labels=c("non","scratch"))


#View(predicted_label)
#levels(predicted_label$model1_pred )
#levels(csv_data$label)
#predicted_label<-ordered(predicted_label, levels = c("non", "scratch"))
confusionMatrix(predicted_label$model1_pred, csv_data$label)
confusionMatrix(predicted_label$model2_pred, csv_data$label)
confusionMatrix(predicted_label$model_combined, csv_data$label)




View(sim_data0910_model1[c(1,72:83,282:293)]) # 1D gyro spacial features (mag)
View(sim_data0910_model1[c(1,5,6,8,9,11, 215,216,218,219,221)]) # 1D gyro, accel features

View(sim_data0910_model1[c(1,2:11,212:221)]) # 1D gyro time related features (max)
train_control <- trainControl(method="repeatedcv", number=10)
sim_1Dmodel1 <- train(label~., data=sim_data0910_model1[c(1,72:83)], trControl = train_control,  method="svmLinear")
sim_1Dmodel1 <- train(label~., data=sim_data0910_model1[c(1,5,6,8,9,11,72:83,215,216,218,219,221 )], trControl = train_control,  method="svmLinear")
sim_1Dmodel1 <- train(label~., data=sim_data0910_model1[c(1,152:190,362:400)], trControl = train_control,  method="svmLinear")

sim_1Dmodel1

#delay.model <- glm(label~., data=sim_data0910_model1[c(1,72:83)], control = list(maxit = 50))
#delay.model



View(sim_data0910_model2[c(1,51:53, 60:65, 69:71, 261:263, 270:275, 279:281)]) # 3D accel, gyro auto features
View(sim_data0910_model2[c(1,152:187,362:397)]) # 3D accel, gyro auto features
#View(combined.model2data[c(1,379:414)]) # 3D accel time related features 
#sim_3Dmodel2 <- train(label~., data=sim_data0910_model2[c(1,5,6,8,9,11,152:187,215,216,218,219,221,362:397)], trControl = train_control,  method="svmLinear")
sim_3Dmodel2 <- train(label~., data=sim_data0910_model2[c(1,51:56, 60:65, 69:71, 152:187, 261:266, 270:275, 270:281, 362:397)], trControl = train_control,  method="svmLinear")
sim_3Dmodel2 <- train(label~., data=sim_data0910_model1[c(1,152:187,362:397)], trControl = train_control,  method="svmLinear")
sim_3Dmodel2 <- train(label~., data=sim_data0910_model2[c(1,152:187,362:397)], trControl = train_control,  method="svmLinear")

sim_3Dmodel2 <- train(label~., data=sim_data0910_model2[c(1,92:127,302:337)], trControl = train_control,  method="svmLinear")

sim_3Dmodel2

summary(sim_3Dmodel2)

####

test_model <- train(label~., data=sim_data0910_model1[c(1,92:127, 302:337)], trControl = train_control,  method="svmLinear")
test_model <- train(label~., data=sim_data0910_model1[c(1,152:187, 362:397)], trControl = train_control,  method="svmLinear")
test_model <- train(label~., data=sim_data0910_model2[c(1,152:187,362:397)], trControl = train_control,  method="svmLinear")
test_model
sim_3Dmodel2<-test_model


########
sim_1Dmodel1 
sim_1Dmodel1

sim_1Dmodel1 <- train(label~., data=combined.model1data[c(1,2:13,86:97)], trControl = train_control,  method="glm")
sim_3Dmodel2 <- train(label~., data=combined.model2data[c(1,267:302,379:414)], trControl = train_control,  method="glm")



##

View(s3_left_csv)
View(csv_data[c(4,53:64)])
train_control <- trainControl(method="cv", number=10)
View(sim_data[c(1,98:109)])
model <- train(label~., data=sim_data[c(1,98:109)], trControl = train_control,  method="glm")
predictions <- predict(model, sim_data[c(1,98:109)])

View(csv_data[c(4,82:93)])




confusionMatrix(predictions, sim_data$label)


csv_data <- s2_left_csv
csv_data <- s2_right_csv
csv_data <- s3_left_csv
csv_data <- s3_right_csv

csv_data <- real_data

csv_data$label <- factor(csv_data$label)

View(csv_data[c(4,(267+3):(302+3))])
csv_data$predicted_label <- predict(sim_model1, csv_data[c(4,(267+3):(302+3))])
confusionMatrix(csv_data$predicted_label, csv_data$label)

View(csv_data[c(4,(231+3):(266+3))])
csv_data$predicted_label <- predict(sim_model1_noPCA, csv_data[c(4,(231+3):(266+3))])
confusionMatrix(csv_data$predicted_label, csv_data$label)

View(csv_data[c(4,(379+3):(414+3))])
csv_data$predicted_label <- predict(auto_sim_model1_PCA, csv_data[c(4,(379+3):(414+3))])
confusionMatrix(csv_data$predicted_label, csv_data$label)

View(csv_data[c(4,(303+3):(314+3))])
csv_data$predicted_label <- predict(auto_sim_model1_max, csv_data[c(4,(303+3):(314+3))])
confusionMatrix(csv_data$predicted_label, csv_data$label)

View(csv_data[c(4,(50+3):(85+3))])
csv_data$predicted_label <- predict(sim_model1_gyro, csv_data[c(4,(50+3):(85+3))])
confusionMatrix(csv_data$predicted_label, csv_data$label)

View(csv_data[c(4,(86+3):(97+3))])
csv_data$predicted_label <- predict(auto_sim_model1_PCA_gyro, csv_data[c(4,(86+3):(97+3))])
confusionMatrix(csv_data$predicted_label, csv_data$label)
 

csv_data$predicted_label <- predict(sim_model1, csv_data[c(4,53:64)])
confusionMatrix(csv_data$predicted_label, csv_data$label)


csv_data$predicted_label <- predict(sim_model1_auto, csv_data[c(4,82:93)])
confusionMatrix(csv_data$predicted_label, csv_data$label)

csv_data$predicted_label <- predict(sim_model1_auto_g, csv_data[c(4,53:64,82:93)])
confusionMatrix(csv_data$predicted_label, csv_data$label)

write.csv(csv_data, "./data_csv/predicted_result_s2_rightGLM_auto_g.csv")




View(s2_left_csv)
############



real_data<-rbind(s3_left_csv, s3_right_csv)
#real_data<-rbind(real_data, s8_right_csv)
#real_data<-rbind(real_data, s8_left_csv)
real_data<-rbind(s8_left_csv, s8_right_csv)

real_data<-rbind(real_data, s2_left_csv)
real_data<-rbind(real_data, s2_right_csv)
real_data$var_th <-  real_data$Linearaccel_variance_avg.PC_x./((real_data$Linearaccel_variance_avg.PC_y.))
hist(real_data$var_th, breaks = c(0,seq(0,15,0.5)), ylim = c(0,1))
real_data$epoches<-NULL
real_data$start_milli<-NULL; real_data$end_milli<-NULL;
rownames(real_data) <- NULL;
nrow(real_data)

real_data<-real_data[which((real_data$var_th < 1)),]
real_data<-real_data[which((real_data$var_th >= 1)),]


View(real_data)
t<-subset(real_data, subset=(real_data$label == "scratch"))
t2<-subset(real_data, subset=(real_data$label == "non"))
length(t2$label)
length(t$label)

t2.sub <-t2[sample(length(t2$label), length(t$label)), ]

length(t2.sub$label)

length(t$label)

real_data_fair<-rbind(t2.sub,t)
rownames(real_data_fair)<-NULL

View(real_data_fair)

real_data_fair[c(2:length(real_data_fair))] <-  lapply((real_data_fair[c(2:length(real_data_fair))]), as.numeric)
real_data_fair$label <- as.factor(real_data_fair$label)
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0804_fair.arff",sep=""), relation = "real_0804_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0805_fair.arff",sep=""), relation = "real_0805_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0806_fair.arff",sep=""), relation = "real_0806_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0806ver2_2_fair.arff",sep=""), relation = "real_0806ver2_scr_nonscr")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08010.arff",sep=""), relation = "real_0810_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08010v2.arff",sep=""), relation = "real_0810_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08017.arff",sep=""), relation = "real_0817_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08018.arff",sep=""), relation = "real_0818_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08019.arff",sep=""), relation = "real_0819_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08024.arff",sep=""), relation = "real_0824_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08024v2.arff",sep=""), relation = "real_0824v2_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08024v3.arff",sep=""), relation = "real_0824v3_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08026.arff",sep=""), relation = "real_0826_scr_nonscr")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08026_2.arff",sep=""), relation = "real_0826_2_scr_nonscr")
write.arff(real_data, file=paste("./data_csv/real_data_fair_data08026_3.arff",sep=""), relation = "real_0826_3_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08026_final.arff",sep=""), relation = "real_0826_FINAL_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08026_final_nofilter.arff",sep=""), relation = "real_0826_FINAL_nofilter_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08026_final_step1.arff",sep=""), relation = "real_0826_FINAL_step1_scr_nonscr")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08026_final_nofilter2.arff",sep=""), relation = "real_0826_FINAL_nofilter2_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08026_final_nofilter3.arff",sep=""), relation = "real_0826_FINAL_nofilter3_scr_nonscr")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08028_butterworth0.8_12.arff",sep=""), relation = "real_0828_butterworth0.8_12")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08028_butterworth0.8_12_smooth.arff",sep=""), relation = "real_0828_butterworth0.8_12_smooth")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08028_butterworth0_12_mag.arff",sep=""), relation = "real_0828_butterworth012mag")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08028_butterworth0_12_PC.arff",sep=""), relation = "real_0828_butterworth012PC")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data08028_butterworth0_12_rot.arff",sep=""), relation = "real_0828_butterworth012rot")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0905_PCA3D.arff",sep=""), relation = "real_0905_PCA3D")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0911_model1.arff",sep=""), relation = "real_0911_model1")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0911_model2.arff",sep=""), relation = "real_0911_model2")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0913_3Dmodel2.arff",sep=""), relation = "real_0913_3Dmodel2")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0913_only_1Dmodel1.arff",sep=""), relation = "real_0913_1Dmodel1")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0913_3Dmodel2_th2.arff",sep=""), relation = "real_0913_3Dmodel2+th2")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0913_3Dmodel2_th1_xy.arff",sep=""), relation = "real_0913_3Dmodel2_th1_xy")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0913_1Dmodel1_th1_xy.arff",sep=""), relation = "real_0913_1Dmodel1_th1_xy")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0915_1Dmodel1_th1_xy.arff",sep=""), relation = "real_0915_1Dmodel1_th1_xy")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0915_3Dmodel2_th1_xy.arff",sep=""), relation = "real_0915_3Dmodel2_th1_xy")


write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0915_allS2S3.arff",sep=""), relation = "real_0915_allS2S3")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0910_allS2S3.arff",sep=""), relation = "real_0910_allS2S3")

write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0918_s8.arff",sep=""), relation = "real_0918_s8")
write.arff(real_data_fair, file=paste("./data_csv/real_data_fair_data0918_s8_pre.arff",sep=""), relation = "real_0918_s8_pre")



s3_left_scr_info <- read.csv("./data_raw/p3/subject3_left_scratch.csv",header=TRUE)
s3_right_scr_info <- read.csv("./data_raw/p3/subject3_right_scratch.csv",header=TRUE)

isScratchIn <- function (startTime, endTime, scr_info, limit_t=3000)
{
  #window_size_milli <- (window_size/50)*1000
  #print(paste("startTime",startTime))
  
  res <- "non"

  
  len <- nrow(scr_info)
  for(i in 1:len)
  {
    scrS <- as.integer(scr_info$scr_start[i])
    scrE <- as.integer(scr_info$scr_end[i])
    if(scrE - scrS < limit_t){
      next
    }
    
    if ( endTime < scrS )
      break
    
    if( (startTime < scrS) & ( scrE < endTime) ) # There is no case beccause minimum scratch time is 3s
    {
      print("===================================>")
      print(paste("scrS",scrS, "scrE", scrE))
      
      win <- endTime - startTime
      win2 <- scrE - scrS
      if(win2 > (win/4) )
      {
        res <- "scratch"
        print(res)
      }
      else{
        print(res)
        break
      }
    }
    
    if ( (scrS < endTime) & (endTime < scrE) )
    {
      print("===================================>")
      print(paste("scrS",scrS, "scrE", scrE))
      if( scrS < ((startTime + endTime)/2) ){
        res <- "scratch"
        print(res)
      }
      else
      {
        print(res)
        break
      }
    }
    
    if ( (scrS < startTime) & (startTime < scrE) )
    {
      print("===================================>")
      print(paste("scrS",scrS, "scrE", scrE))
      
      leftT <- (scrE - startTime)
      print(paste("leftT",leftT))
      #if( (window_size_milli/2) <= leftT ){
      #  res <- "scratch"
      #  print(res)
      #}
      if( ((startTime + endTime)/2) < scrE ){
        res <- "scratch"
        print(res)
      }
      else
        {
        print(res)
        break
      }
    }
    
  }
  if(res=="non"){
    print("non")
  }
  return(res)
  
}

library(data.table)


data.sub <- subject3_left_data

btwData <- function(data.sub, start_milli, end_milli, saveFile = FALSE)
{
  data.sub <- subset(data.sub, subset=(data.sub$time < end_milli ))
  data.sub <- subset(data.sub, subset=(data.sub$time > start_milli ))
  
  if(saveFile)
    write.csv(data.sub, file=paste("./data_raw/",graph_title,".txt",sep=""), row.names=T)
  
return (data.sub)
}



doSimulationAllFeaturesWithLabeling <- function(data, cut, idx, window_size, window_step, scr_info, save_filename=FALSE, plotting = FALSE, type = 1, delay=1, startMilli=2000, endMilli=2000, thresholdvar = 0.1,labeling=TRUE)
{
  data.mag <- subset(data,grepl(list[8],data$type))
  data.sub <- subset(data,grepl(list[idx], data$type))
  data.sub$hour <- data.sub$time/(1000*60*60)
  
  
  #  if(idx==8)
  #  {
  #    data.sub$x <- data.sub$x - mean(data.sub$x)
  #    data.sub$y <- data.sub$y - mean(data.sub$y)
  #    data.sub$z <- data.sub$z - mean(data.sub$z)
  #  }
  #  data.sub <- subset(data.sub, subset=(data.sub$time > 2000 ))
  
  #data.sub <- getDelayedData(data.sub, delay)
  
  # cut 5 minute
  if(cut){
    print(paste("cutting...",startMilli," - ",endMilli))
    data.sub <- subset(data.sub, subset=(data.sub$time > as.numeric(startMilli) ))
    data.mag <- subset(data.mag, subset=(data.mag$time > as.numeric(startMilli) ))
    
    e_idx <- nrow(data.sub)
    e_time <- data.sub$time[e_idx]
    
    data.sub <- subset(data.sub, subset=(data.sub$time < (e_time - as.numeric(endMilli)) ))
    data.mag <- subset(data.mag, subset=(data.mag$time < (e_time - as.numeric(endMilli)) ))
  }
  
  View(data.sub)
  print(paste("data.sub.nrow", nrow(data.sub)))
  window_num <- round( (nrow(data.mag)/window_step) )
  print(paste(list[8], "win num ", window_num, list[idx],"win num ", (nrow(data.sub)/window_step)))
  window_idx <- 1
  
  if(plotting == TRUE)
  {
    graph_title <- save_filename
    max_value <- (as.integer(max(data.sub$time)))
    print(paste("max_value ",max_value))
    spliting <- seq(0,max_value,max_value/10)
    xlablename <- "Time (millisecond)"
    
    df <- data.frame(time =data.sub$time, x=data.sub$x, y=data.sub$y, z=data.sub$z)
    #df$mag <- sqrt((data.sub$x+50)^2 + (data.sub$y+50)^2 + (data.sub$z+50)^2)
    #df$mag <- df$mag - mean(df$mag)
    
    returnValue <- ggplot(df, aes(x=time,colour="axis")) +
      geom_line(aes(y=x, colour="X")) +
      geom_line(aes(y=y, colour="Y")) +
      geom_line(aes(y=z, colour="Z")) +
      #geom_line(aes(y=mag, colour="_Magnitude")) + 
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")",sep="")) + 
      scale_color_manual(values=c("red","blue","black","violet")) +
      xlab(paste("Time(milli)", ", window_size(", window_size,"), window_step(", window_step,")",sep="")) +
      ylab(y_label_list[idx]) +
      scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
    window_idx <- 1
    for(i in 1:window_num){
      returnValue <- returnValue + geom_vline(xintercept = data.sub$time[window_idx], colour="black", alpha=0.8)
      window_idx <- window_idx + window_step  
    }
    #print(returnValue)
  }
  
  # Window setting
  #print(paste("window_num : ",window_num, " nrow(data.sub) : ", nrow(data.sub), " window_step ", window_step))
  
  #  window_num <- as.integer(nrow(data.sub)/window_step)
  window_idx <- 1
  #  window_set <- vector(mode="list", length=(31 - 12) ) # extended previous 2
  #  window_set <- vector(mode="list", length=(31) ) # extended previous & CHI
  #  window_set <- vector(mode="list", length=(31+4) ) # all
  #  window_set <- vector(mode="list", length=(19+6) ) # selected
  # window_set <- vector(mode="list", length=(19+6) ) # selected + 1
  window_set <- vector(mode="list", length=(4 + 3*(15+16+3) + (14+16+3) + (14+16+3) ) ) #   
  sname <- list[idx]
  #window_set <- vector(mode="list", length=(31 - 8) ) # previous work
  names(window_set) <- c(
    "epoches","start_milli","end_milli","label",
    
    paste(sname,"_mean_x",sep=""),paste(sname,"_mean_y",sep=""), paste(sname,"_mean_z", sep=""),
    paste(sname,"_max_x",sep=""), paste(sname,"_max_y",sep=""),paste(sname,"_max_z",sep=""),
    paste(sname,"_min_x",sep=""), paste(sname,"_min_y",sep=""),paste(sname,"_min_z",sep=""),
    paste(sname,"_entropy_x",sep=""), paste(sname,"_entropy_y",sep=""),paste(sname,"_entropy_z",sep=""),
    paste(sname,"_energy_x",sep=""),paste(sname,"_energy_y",sep=""),paste(sname,"_energy_z",sep=""),
    paste(sname,"_cor_x",sep=""), paste(sname,"_cor_y",sep=""),paste(sname,"_cor_z",sep=""),
    paste(sname,"_autocor1_x",sep=""),paste(sname,"_autocor1_y",sep=""),paste(sname,"_autocor1_z",sep=""),
    paste(sname,"_th_x",sep=""),paste(sname,"_th_y",sep=""),paste(sname,"_th_z",sep=""),
    paste(sname,"_autocor2_x",sep=""),paste(sname,"_autocor2_y",sep=""),paste(sname,"_autocor2_z",sep=""),
    paste(sname,"_autocorV2_x",sep=""),paste(sname,"_autocorV2_y",sep=""),paste(sname,"_autocorV2_z",sep=""),
    paste(sname,"_var_x",sep=""),paste(sname,"_var_y",sep=""),paste(sname,"_var_z",sep=""),  
    paste(sname,"_peakfreq_x",sep=""),paste(sname,"_peakfreq_y",sep=""),paste(sname,"_peakfreq_z",sep=""),
    paste(sname,"_energyFrom_0to10Hz_x",sep=""),paste(sname,"_energyFrom_0to10Hz_y",sep=""),paste(sname,"_energyFrom_0to10Hz_z",sep=""),
    paste(sname,"_RMS_x",sep=""),paste(sname,"_RMS_y",sep=""),paste(sname,"_RMS_z",sep=""),   
    paste(sname,"_SD_x",sep=""),paste(sname,"_SD_y",sep=""),paste(sname,"_SD_z",sep=""),
    
    paste(sname,"_integrated_RMS_x",sep=""),paste(sname,"_integrated_RMS_y",sep=""),paste(sname,"_integrated_RMS_z",sep=""),
    paste(sname,"_numPeak_x",sep=""),paste(sname,"_numPeak_y",sep=""),paste(sname,"_numPeak_z",sep=""),
    paste(sname,"_promientPeak_x",sep=""),paste(sname,"_promientPeak_y",sep=""),paste(sname,"_promientPeak_z",sep=""),
    paste(sname,"_weakPeak_x",sep=""),paste(sname,"_weakPeak_y",sep=""),paste(sname,"_weakPeak_z",sep=""),
    paste(sname,"_maxAuto_x",sep=""),paste(sname,"_maxAuto_y",sep=""),paste(sname,"_maxAuto_z",sep=""),
    paste(sname,"_height1stAuto_x",sep=""),paste(sname,"_height1stAuto_y",sep=""),paste(sname,"_height1stAuto_z",sep=""),
    paste(sname,"_powerBand0_2.5x",sep=""),paste(sname,"_powerBand0_2.5_y",sep=""),paste(sname,"_powerBand0_2.5_z",sep=""),
    paste(sname,"_powerBand2.5_5x",sep=""),paste(sname,"_powerBand2.5_5_y",sep=""),paste(sname,"_powerBand2.5_5_z",sep=""),
    paste(sname,"_powerBand5_7.5x",sep=""),paste(sname,"_powerBand5_7.5_y",sep=""),paste(sname,"_powerBand5_7.5_z",sep=""),
    paste(sname,"_powerBand7.5_10x",sep=""),paste(sname,"_powerBand7.5_10_y",sep=""),paste(sname,"_powerBand7.5_10_z",sep=""),
    paste(sname,"_powerBand10_12.5x",sep=""),paste(sname,"_powerBand10_12.5_y",sep=""),paste(sname,"_powerBand10_12.5_z",sep=""),
    paste(sname,"_powerBand12.5_15x",sep=""),paste(sname,"_powerBand12.5_15_y",sep=""),paste(sname,"_powerBand12.5_15_z",sep=""),
    paste(sname,"_powerBand15_17.5x",sep=""),paste(sname,"_powerBand15_17.5_y",sep=""),paste(sname,"_powerBand15_17.5_z",sep=""),
    paste(sname,"_powerBand17.5_20x",sep=""),paste(sname,"_powerBand17.5_20_y",sep=""),paste(sname,"_powerBand17.5_20_z",sep=""),
    paste(sname,"_powerBand20_22.5x",sep=""),paste(sname,"_powerBand20_22.5_y",sep=""),paste(sname,"_powerBand20_22.5_z",sep=""),
    paste(sname,"_powerBand22.5_25x",sep=""),paste(sname,"_powerBand22.5_25_y",sep=""),paste(sname,"_powerBand22.5_25_z",sep=""),
    paste(sname,"_numPeakSum_x",sep=""),paste(sname,"_numPeakSum_y",sep=""),paste(sname,"_numPeakSum_z",sep=""),
    paste(sname,"_promientPeakSum_x",sep=""),paste(sname,"_promientPeakSum_y",sep=""),paste(sname,"_promientPeakSum_z",sep=""),
    paste(sname,"_weakPeakSum_x",sep=""),paste(sname,"_weakPeakSum_y",sep=""),paste(sname,"_weakPeakSum_z",sep=""),
    
    #
    paste(sname,"_mean_avg",sep=""),
    paste(sname,"_max_avg",sep=""),
    paste(sname,"_min_avg",sep=""),
    paste(sname,"_entropy_avg",sep=""),
    paste(sname,"_energy_avg",sep=""),
    paste(sname,"_autocor1_avg",sep=""), 
    paste(sname,"_th_avg",sep=""),
    paste(sname,"_autocor2_avg",sep=""),
    paste(sname,"_autocorV2_avg",sep=""),                    
    paste(sname,"_var_avg",sep=""),
    paste(sname,"_peakfreq_avg",sep=""),
    paste(sname,"_energyFrom_0to10Hz_avg",sep=""),
    paste(sname,"_RMS_avg",sep=""),
    paste(sname,"_SD_avg",sep=""),
    
    paste(sname,"_integrated_RMS_avg",sep=""),
    paste(sname,"_numPeak_avg",sep=""),
    paste(sname,"_promientPeak_avg",sep=""),
    paste(sname,"_weakPeak_avg",sep=""),
    paste(sname,"_maxAuto_avg",sep=""),
    paste(sname,"_height1stAuto_avg",sep=""),
    paste(sname,"_powerBand0_2.5avg",sep=""),
    paste(sname,"_powerBand2.5_5avg",sep=""),
    paste(sname,"_powerBand5_7.5avg",sep=""),
    paste(sname,"_powerBand7.5_10avg",sep=""),
    paste(sname,"_powerBand10_12.5avg",sep=""),
    paste(sname,"_powerBand12.5_15avg",sep=""),
    paste(sname,"_powerBand15_17.5avg",sep=""),
    paste(sname,"_powerBand17.5_20avg",sep=""),
    paste(sname,"_powerBand20_22.5avg",sep=""),
    paste(sname,"_powerBand22.5_25avg",sep=""),
    paste(sname,"_numPeakSum_avg",sep=""),
    paste(sname,"_promientPeakSum_avg",sep=""),
    paste(sname,"_weakPeakSum_avg",sep=""),
    
    #
    paste(sname,"_mean_avg(PC)",sep=""),
    paste(sname,"_max_avg(PC)",sep=""),
    paste(sname,"_min_avg(PC)",sep=""),
    paste(sname,"_entropy_avg(PC)",sep=""),
    paste(sname,"_energy_avg(PC)",sep=""),
    paste(sname,"_autocor1_avg(lag12PC)",sep=""),                         
    paste(sname,"_th_avg(PC)",sep=""),
    paste(sname,"_autocor2_avg(lag1PC)",sep=""),
    paste(sname,"_autocorV2_avg(PC)",sep=""),                        
    paste(sname,"_var_avg(PC)",sep=""),
    paste(sname,"_peakfreq_avg(PC)",sep=""),
    paste(sname,"_energyFrom_0to10Hz_avg(PC)",sep=""),
    paste(sname,"_RMS_avg(PC)",sep=""),
    paste(sname,"_SD_avg(PC)",sep=""),
    
    paste(sname,"_integrated_RMS_avg(PC)",sep=""),
    paste(sname,"_numPeak_avg(PC)",sep=""),
    paste(sname,"_promientPeak_avg(PC)",sep=""),
    paste(sname,"_weakPeak_avg(PC)",sep=""),
    paste(sname,"_maxAuto_avg(PC)",sep=""),
    paste(sname,"_height1stAuto_avg(PC)",sep=""),
    paste(sname,"_powerBand0_2.5avg(PC)",sep=""),
    paste(sname,"_powerBand2.5_5avg(PC)",sep=""),
    paste(sname,"_powerBand5_7.5avg(PC)",sep=""),
    paste(sname,"_powerBand7.5_10avg(PC)",sep=""),
    paste(sname,"_powerBand10_12.5avg(PC)",sep=""),
    paste(sname,"_powerBand12.5_15avg(PC)",sep=""),
    paste(sname,"_powerBand15_17.5avg(PC)",sep=""),
    paste(sname,"_powerBand17.5_20avg(PC)",sep=""),
    paste(sname,"_powerBand20_22.5avg(PC)",sep=""),
    paste(sname,"_powerBand22.5_25avg(PC)",sep=""),
    paste(sname,"_numPeakSum_avg(PC)",sep=""),
    paste(sname,"_promientPeakSum_avg(PC)",sep=""),
    paste(sname,"_weakPeakSum_avg(PC)",sep="")
    
  )
  
  candidates_idx <- 1
  # p1  3.58333+0.013888
  # p2  2.267 + 0.0219448 -0.0061112 - 0.02138888 + 0.02194444 -0.0011111 +0.0000277777 left
  # p2  2.267 + 0.0219448 -0.0061112 - 0.02138888 right\
  # recent 2.267 + 0.0219448 -0.0061112 - 0.02138888 +2.6 + 0.004166
  # recent 2.267 + 0.0219448 -0.0061112-0.02138888 +2.6 + 0.004166
  print(paste("window_num : ",window_num))
  
  DT <- as.data.table(data.sub)
  
  prevMovIndex <- 0
  
  for(i in 1:window_num)
  {
    window_data_for_mag <- getWindow(data.mag, window_idx, window_size)
    magnitude <- sqrt( (window_data_for_mag$x+50)^2+(window_data_for_mag$y+50)^2+(window_data_for_mag$z+50)^2)
    magnitude <- magnitude - mean(magnitude)
    
    #window_data <- subset(data.sub, subset=(data.sub$time >= as.numeric(window_data_for_mag$time[1]) ))    
    #window_data <- subset(window_data, subset=(window_data$time <= as.numeric(window_data_for_mag$time[nrow(window_data_for_mag)]) ))

    
    #print(paste(sname, window_data$time[1] ,  window_data$time[nrow(window_data)] , var(magnitude)))
    #print(paste(list[8], window_data_for_mag$time[1] ,  window_data_for_mag$time[nrow(window_data_for_mag)]  , var(magnitude)))
    if(var(magnitude)>thresholdvar){
      
      window_data<-DT[ (window_data_for_mag$time[1] <= time) & (time <= window_data_for_mag$time[nrow(window_data_for_mag)]) ]
      #window_data <- getWindow(data.sub,window_idx,window_size)
      
      window_df <- data.frame(time_hour=window_data$hour, time_milli =window_data$time, x=window_data$x, y=window_data$y, 
                              z=window_data$z, saxis=(window_data$x+window_data$y+window_data$z))
      
      start_milli <-paste( getHMS(window_data$hour[1]), window_data$time[1] )
      end_milli <- paste( getHMS(window_data$hour[nrow(window_data)]), window_data$time[nrow(window_data)] )
      
      if(idx==3)
      {
        gyroMax  <- max(abs(window_df$x), abs(window_df$y), abs(window_df$z))
        if(gyroMax > 9)
          label <- paste("turningover", gyroMax)
        else
          label <- paste("not_turningover", gyroMax)
      }else
        label<-""
      epoch<- "con"
      
      if(labeling){
        label <- isScratchIn( window_data$time[1], window_data$time[nrow(window_data)], scr_info )
      }else{
        label <- paste(label,"_NA",sep="")
      }
      
      if(plotting == TRUE)
      {
        rect <- data.frame(xmin=data.sub$time[window_idx], xmax=data.sub$time[window_idx+nrow(window_data)-2], ymin=-Inf, ymax=Inf)
        returnValue <- returnValue + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill="blue", inherit.aes = FALSE)             
      }
      
      if( (prevMovIndex + 1) != i ){
          epoch <- "start"
      }
      
      prevMovIndex <- i
      p <- c(epoch,start_milli,end_milli,label,
             getFeatureBy(window_df,"mean"),
             getFeatureBy(window_df,"max"),
             getFeatureBy(window_df,"min"),
             getFeatureBy(window_df,"entropy"),   
             getFeatureBy(window_df,"energy"),
             getFeatureBy(window_df,"correlation"),
             getFeatureBy(window_df,"autocorrelation",12),
             getFeatureBy(window_df,"threshold"),
             getFeatureBy(window_df,"autocorrelation", 1),
             #getFeatureBy(window_df,"autocorrelationV2"),
             0,0,0,
             getFeatureBy(window_df,"variance"),
             getFeatureBy(window_df,"peakfreq"),
             getFeatureBy(window_df,"energyFrom_0to10Hz"),
             getFeatureBy(window_df,"RMS"),
             getFeatureBy(window_df,"SD"),
             getFeatureBy(window_df,"integratedRMS"),
             getFeatureBy(window_df,"peaknumAuto",filtering = TRUE),
             getFeatureBy(window_df,"prominentAuto",filtering = TRUE),
             getFeatureBy(window_df,"weakpeakAuto",filtering = TRUE),
             getFeatureBy(window_df,"maximumAuto",filtering = TRUE),
             getFeatureBy(window_df,"height1stAuto",filtering = TRUE),
             
             getFeatureBy(window_df,"powerband",powerband_from = 0,powerband_to = 2.5),
             getFeatureBy(window_df,"powerband",powerband_from = 2.5,powerband_to = 5),
             getFeatureBy(window_df,"powerband",powerband_from = 5,powerband_to = 7.5),
             getFeatureBy(window_df,"powerband",powerband_from = 7.5,powerband_to = 10),
             getFeatureBy(window_df,"powerband",powerband_from = 10,powerband_to = 12.5),
             getFeatureBy(window_df,"powerband",powerband_from = 12.5,powerband_to = 15),
             getFeatureBy(window_df,"powerband",powerband_from = 15,powerband_to = 17.5),
             getFeatureBy(window_df,"powerband",powerband_from = 17.5,powerband_to = 20),
             getFeatureBy(window_df,"powerband",powerband_from = 20,powerband_to = 22.5),
             getFeatureBy(window_df,"powerband",powerband_from = 22.5,powerband_to = 25),
             getFeatureBy(window_df,"peaknumAutoSum",filtering = TRUE),
             getFeatureBy(window_df,"prominentAutoSum",filtering = TRUE),
             getFeatureBy(window_df,"weakpeakAutoSum",filtering = TRUE),
             
             ##           
             getFeatureBy(window_df,"mean",avg=TRUE),
             getFeatureBy(window_df,"max",avg=TRUE),
             getFeatureBy(window_df,"min",avg=TRUE),
             getFeatureBy(window_df,"entropy",avg=TRUE),   
             getFeatureBy(window_df,"energy",avg=TRUE),
             getFeatureBy(window_df,"autocorrelation",12,avg=TRUE),
             getFeatureBy(window_df,"threshold",avg=TRUE),
             getFeatureBy(window_df,"autocorrelation", 1,avg=TRUE),
             #getFeatureBy(window_df,"autocorrelationV2",avg=TRUE),
             0,           
             getFeatureBy(window_df,"variance",avg=TRUE),
             getFeatureBy(window_df,"peakfreq",avg=TRUE),
             getFeatureBy(window_df,"energyFrom_0to10Hz",avg=TRUE),
             getFeatureBy(window_df,"RMS",avg=TRUE),
             getFeatureBy(window_df,"SD",avg=TRUE), 
             getFeatureBy(window_df,"integratedRMS",avg=TRUE),
             getFeatureBy(window_df,"peaknumAuto",avg=TRUE,filtering = TRUE),
             getFeatureBy(window_df,"prominentAuto",avg=TRUE,filtering = TRUE),
             getFeatureBy(window_df,"weakpeakAuto",avg=TRUE,filtering = TRUE),
             getFeatureBy(window_df,"maximumAuto",avg=TRUE,filtering = TRUE),
             getFeatureBy(window_df,"height1stAuto",avg=TRUE,filtering = TRUE),
             
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 0,powerband_to = 2.5),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 2.5,powerband_to = 5),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 5,powerband_to = 7.5),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 7.5,powerband_to = 10),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 10,powerband_to = 12.5),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 12.5,powerband_to = 15),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 15,powerband_to = 17.5),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 17.5,powerband_to = 20),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 20,powerband_to = 22.5),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 22.5,powerband_to = 25),
             getFeatureBy(window_df,"peaknumAutoSum",avg=TRUE,filtering = TRUE),
             getFeatureBy(window_df,"prominentAutoSum",avg=TRUE,filtering = TRUE),
             getFeatureBy(window_df,"weakpeakAutoSum",avg=TRUE,filtering = TRUE),
             
             ##
             getFeatureBy(window_df,"mean",avg=TRUE,type="PC"),
             getFeatureBy(window_df,"max",avg=TRUE,type="PC"),
             getFeatureBy(window_df,"min",avg=TRUE,type="PC"),
             getFeatureBy(window_df,"entropy",avg=TRUE,type="PC"),   
             getFeatureBy(window_df,"energy",avg=TRUE,type="PC"),
             getFeatureBy(window_df,"autocorrelation",12,avg=TRUE,type="PC"),
             getFeatureBy(window_df,"threshold",avg=TRUE,type="PC"),
             getFeatureBy(window_df,"autocorrelation", 1,avg=TRUE,type="PC"),
             #getFeatureBy(window_df,"autocorrelationV2",avg=TRUE),
             0,           
             getFeatureBy(window_df,"variance",avg=TRUE,type="PC"),
             getFeatureBy(window_df,"peakfreq",avg=TRUE,type="PC"),
             getFeatureBy(window_df,"energyFrom_0to10Hz",avg=TRUE,type="PC"),
             getFeatureBy(window_df,"RMS",avg=TRUE, type="PC"),
             getFeatureBy(window_df,"SD",avg=TRUE, type="PC"),
             getFeatureBy(window_df,"integratedRMS",avg=TRUE, type="PC"),
             getFeatureBy(window_df,"peaknumAuto",avg=TRUE, type="PC",filtering = TRUE),
             getFeatureBy(window_df,"prominentAuto",avg=TRUE, type="PC",filtering = TRUE),
             getFeatureBy(window_df,"weakpeakAuto",avg=TRUE, type="PC",filtering = TRUE),
             getFeatureBy(window_df,"maximumAuto",avg=TRUE, type="PC",filtering = TRUE),
             getFeatureBy(window_df,"height1stAuto",avg=TRUE, type="PC",filtering = TRUE),
             
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 0,powerband_to = 2.5, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 2.5,powerband_to = 5, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 5,powerband_to = 7.5, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 7.5,powerband_to = 10, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 10,powerband_to = 12.5, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 12.5,powerband_to = 15, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 15,powerband_to = 17.5, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 17.5,powerband_to = 20, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 20,powerband_to = 22.5, type="PC"),
             getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 22.5,powerband_to = 25, type="PC"),
             getFeatureBy(window_df,"peaknumAutoSum",avg=TRUE,type="PC",filtering = TRUE),
             getFeatureBy(window_df,"prominentAutoSum",avg=TRUE,type="PC",filtering = TRUE),
             getFeatureBy(window_df,"weakpeakAutoSum",avg=TRUE,type="PC",filtering = TRUE),
             
             
             
             
             
      )
      
      window_set<-rbind(window_set,p) 
      
    }else{
      epoch<-0
      label<-"sleep"
    }
    

    
    window_idx <- window_idx + window_step
  }
  
  window_set <- window_set[-1,]
  View(window_set)
  
  if(save_filename!=FALSE){
    write.csv(window_set, file=paste("./data_csv/",save_filename,".csv",sep=""), row.names=T)
    print(paste("* window_num",window_num))
    print(paste("* saved file: ", save_filename,".csv", sep=""))
  }
  
  if(plotting)
    print(returnValue)
  
  return (window_set)
  
}

