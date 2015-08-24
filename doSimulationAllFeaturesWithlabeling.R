

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

sum_data<-getLabeledData(subject2_left_data,s2_left_scr_info,"s2_left_scr_08024v3") # 0806
write.csv(sum_data, file=paste("./data_csv/p2/subject2_left_data08024v3.csv",sep=""), row.names=F)

remove(subject2_left_data)


subject2_right_data <- read.table("./data_raw/p2/subject2_right.txt",sep=",",header=TRUE)
s2_right_scr_info <-  read.csv("./data_raw/p2/subject2_right_scratch.csv",header=TRUE)

sum_data<-getLabeledData(subject2_right_data,s2_right_scr_info,"s2_right_scr_08024v3")
write.csv(sum_data, file=paste("./data_csv/p2/subject2_right_data08024v3.csv",sep=""), row.names=F)

remove(subject2_right_data)


subject3_left_data <- read.table("./data_raw/p3/subject3_left.txt",sep=",",header=TRUE)
s3_left_scr_info <-  read.csv("./data_raw/p3/subject3_left_scratch.csv",header=TRUE)

sum_data<-getLabeledData(subject3_left_data,s3_left_scr_info,"s3_left_scr_0824v3")
write.csv(sum_data, file=paste("./data_csv/p3/subject3_left_data08024v3.csv",sep=""), row.names=F)

remove(subject3_left_data)


subject3_right_data <- read.table("./data_raw/p3/subject3_right.txt",sep=",",header=TRUE)
s3_right_scr_info <-  read.csv("./data_raw/p3/subject3_right_scratch.csv",header=TRUE)

sum_data<-getLabeledData(subject3_right_data,s3_right_scr_info,"s3_right_scr_08024v3")
write.csv(sum_data, file=paste("./data_csv/p3/subject3_right_data08024v3.csv",sep=""), row.names=F)


remove(subject3_right_data)




View(sum_data)


getLabeledData<-function(raw_data, label_info, title , threshold = 0.05)
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


View(s2_left_csv)

real_data<-rbind(s3_left_csv, s3_right_csv)
real_data<-rbind(real_data, s2_left_csv)
real_data<-rbind(real_data, s2_right_csv)
real_data$epoches<-NULL
real_data$start_milli<-NULL; real_data$end_milli<-NULL;

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



s3_left_scr_info <- read.csv("./data_raw/p3/subject3_left_scratch.csv",header=TRUE)
s3_right_scr_info <- read.csv("./data_raw/p3/subject3_right_scratch.csv",header=TRUE)

isScratchIn <- function (startTime, endTime, scr_info)
{
  #window_size_milli <- (window_size/50)*1000
  #print(paste("startTime",startTime))
  
  res <- "non"

  
  len <- nrow(scr_info)
  for(i in 1:len)
  {
    scrS <- as.integer(scr_info$scr_start[i])
    scrE <- as.integer(scr_info$scr_end[i])
    if(scrE - scrS < 3000){
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
             getFeatureBy(window_df,"weakpeakAutoSum",avg=TRUE,type="PC",filtering = TRUE)
             
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

