getDataset2 <- function(scr_filelist, non_scratch_file,idx, window_size, window_stp, plotting = FALSE, scr_small_scratching = NULL,delay=1)
{
  filter <- "scratch|non|scratch_finger"
  sum_data <- NULL
  
  if(length(scr_filelist)!=0){
    for(i in 1:length(scr_filelist))
    {
      labelname <- scr_filelist[i]
      print(paste("filename",labelname))
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulationAllFeatures(data, TRUE, idx, window_size, window_stp, labelname, plotting,delay=delay)
      t<-autolabeling(paste(labelname,".csv",sep=""), "scratch")
      
      if(length(sum_data)==0)
        sum_data <- subset(t, grepl(filter, t$label))
      else
        sum_data <-sumData(sum_data, t, filter)
    }
  }
  
  if(length(non_scratch_file)!=0){
    for(i in 1:length(non_scratch_file))
    {
      labelname <- non_scratch_file[i]
      print(paste("filename",labelname))
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulationAllFeatures(data, TRUE, idx, window_size, window_stp, labelname, plotting,delay=delay)
      t<-autolabeling(paste(labelname,".csv",sep=""), "non", TRUE)
      
      if(length(sum_data)==0)
        sum_data <- subset(t, grepl(filter, t$label))
      else
        sum_data <- sumData(sum_data, subset(t, grepl(filter, t$label)), filter)
    }
  }
  
  if(length(scr_small_scratching)!=0){
    for(i in 1:length(scr_small_scratching))
    {
      labelname <- scr_small_scratching[i]
      print(paste("filename",labelname))
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulationAllFeatures(data, TRUE, idx, window_size, window_stp, labelname, plotting,delay=delay)
      t<-autolabeling(paste(labelname,".csv",sep=""),"scratch_finger")
      
      if(length(sum_data)==0)
        sum_data <- subset(t, grepl(filter, t$label))
      else
        sum_data <- sumData(sum_data, subset(t, grepl(filter, t$label)), filter)
    }
  }
  
  View(sum_data)
  #levels(sum_data$label)
  #sum_data <- sumData(sum_data2, sum_data, filter)
  
  return(sum_data)
}


doSimulationAllFeatures <- function(data, cut, idx, window_size, window_step, save_filename, plotting = FALSE, type = 1, delay=1)
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
  
  data.sub <- getDelayedData(data.sub, delay)
  
  
  # cut 5 minute
  if(cut){
    data.sub <- subset(data.sub, subset=(data.sub$time > 1000*3 ))
    e_idx <- nrow(data.sub)
    e_time <- data.sub$time[e_idx]
    data.sub <- subset(data.sub, subset=(data.sub$time < (e_time - (1000*3)) ))
  }
  
  # Window setting
  #print(paste("window_num : ",window_num, " nrow(data.sub) : ", nrow(data.sub), " window_step ", window_step))
  
  window_num <- as.integer(nrow(data.sub)/window_step)
  window_idx <- 1
  #  window_set <- vector(mode="list", length=(31 - 12) ) # extended previous 2
  #  window_set <- vector(mode="list", length=(31) ) # extended previous & CHI
  #  window_set <- vector(mode="list", length=(31+4) ) # all
  #  window_set <- vector(mode="list", length=(19+6) ) # selected
  # window_set <- vector(mode="list", length=(19+6) ) # selected + 1
  window_set <- vector(mode="list", length=(4 + 3*11 + 10) ) # selected + 1
  
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
                         paste(sname,"_var_x",sep=""),paste(sname,"_var_y",sep=""),paste(sname,"_var_z",sep=""),
                         paste(sname,"_peakfreq_x",sep=""),paste(sname,"_peakfreq_y",sep=""),paste(sname,"_peakfreq_z",sep=""),
                         paste(sname,"_mean_avg",sep=""),
                         paste(sname,"_max_avg",sep=""),
                         paste(sname,"_min_avg",sep=""),
                         paste(sname,"_entropy_avg",sep=""),
                         paste(sname,"_energy_avg",sep=""),
                         paste(sname,"_autocor1_avg",sep=""),
                         paste(sname,"_th_avg",sep=""),
                         paste(sname,"_autocor2_avg",sep=""),
                         paste(sname,"_var_avg",sep=""),
                         paste(sname,"_peakfreq_avg",sep="")
                         
                         )
  
  
  candidates_idx <- 1
  # p1  3.58333+0.013888
  # p2  2.267 + 0.0219448 -0.0061112 - 0.02138888 + 0.02194444 -0.0011111 +0.0000277777 left
  # p2  2.267 + 0.0219448 -0.0061112 - 0.02138888 right\
  # recent 2.267 + 0.0219448 -0.0061112 - 0.02138888 +2.6 + 0.004166
  # recent 2.267 + 0.0219448 -0.0061112-0.02138888 +2.6 + 0.004166
  print(paste("window_num : ",window_num))
  
  for(i in 1:window_num)
  {
    window_data_for_mag <- getWindow(data.mag, window_idx, window_size)
    magnitude <- sqrt( (window_data_for_mag$x+50)^2+(window_data_for_mag$y+50)^2+(window_data_for_mag$z+50)^2)
    magnitude <- magnitude - mean(magnitude)
    window_data <- getWindow(data.sub,window_idx,window_size)
    window_df <- data.frame(time_hour=window_data$hour, time_milli =window_data$time, x=window_data$x, y=window_data$y, 
                            z=window_data$z, saxis=(window_data$x+window_data$y+window_data$z))
    start_milli <-paste( getHMS(window_data$hour[1]), window_data$time[1] )
    end_milli <- paste( getHMS(window_data$hour[nrow(window_data)]), window_data$time[nrow(window_data)] )
    if(var(magnitude)>0.1){
      epoch<-1
      label<-"TODO"
    }else{
      epoch<-0
      label<-"sleep"
    }
    
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
           getFeatureBy(window_df,"variance"),
           getFeatureBy(window_df,"peakfreq"),
           getFeatureBy(window_df,"mean",avg=TRUE),
           getFeatureBy(window_df,"max",avg=TRUE),
           getFeatureBy(window_df,"min",avg=TRUE),
           getFeatureBy(window_df,"entropy",avg=TRUE),   
           getFeatureBy(window_df,"energy",avg=TRUE),
           getFeatureBy(window_df,"autocorrelation",12,avg=TRUE),
           getFeatureBy(window_df,"threshold",avg=TRUE),
           getFeatureBy(window_df,"autocorrelation", 1,avg=TRUE),
           getFeatureBy(window_df,"variance",avg=TRUE),
           getFeatureBy(window_df,"peakfreq",avg=TRUE)   
    )
    
    window_set<-rbind(window_set,p)
    
    window_idx <- window_idx + window_step
  }
  window_set <- window_set[-1,]
  #View(window_set)
  write.csv(window_set, file=paste("./data_csv/",save_filename,".csv",sep=""), row.names=T)
  print(paste("* window_num",window_num))
  print(paste("* saved file: ", save_filename,".csv", sep=""))
  
  max_value <- (as.integer(max(data.sub$time)))
  spliting <- seq(0,max_value,max_value/10)
  
  df <- data.frame(time =data.sub$time, x=data.sub$x, y=data.sub$y, z=data.sub$z)
  df$mag <- sqrt((data.sub$x+50)^2 + (data.sub$y+50)^2 + (data.sub$z+50)^2)
  df$mag <- df$mag - mean(df$mag)
  
  
  if(plotting == TRUE)
  {
    
    returnValue <- ggplot(df, aes(x=time,colour="axis")) +
      geom_line(aes(y=x, colour="X")) +
      geom_line(aes(y=y, colour="Y")) +
      geom_line(aes(y=z, colour="Z")) +
      #geom_line(aes(y=mag, colour="_Magnitude")) + 
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")","(range - ",start_hour," ~ ",end_hour,")",sep="")) + 
      scale_color_manual(values=c("red","blue","black","violet")) +
      xlab(xlablename) +
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
    print(returnValue)
  }
  
  return (window_set)
  
}

