getDataset2 <- function(scr_filelist, non_scratch_file,idx, window_size, window_stp, plotting = FALSE, scr_small_scratching = NULL,delay=1,thresholdvar = 0.1)
{
  filter <- "scratch|non|scratch_finger"
  sum_data <- NULL
  
  if(length(scr_filelist)!=0){
    for(i in 0:((length(scr_filelist)/4)-1))
    {
      labelname <- scr_filelist[i*4+1]
      isCut <- scr_filelist[i*4+2]
      startMilli <- scr_filelist[i*4+3]
      endMilli <- scr_filelist[i*4+4]
      
      print(paste("filename",labelname))
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulationAllFeatures(data, isCut, idx, window_size, window_stp, labelname, plotting,delay=delay, startMilli = startMilli, endMilli = endMilli, thresholdvar = thresholdvar)
      t<-autolabeling(paste(labelname,".csv",sep=""), "scratch")
      
      if(length(sum_data)==0)
        sum_data <- subset(t, grepl(filter, t$label))
      else
        sum_data <-sumData(sum_data, t, filter)
    }
  }
  
  if(length(non_scratch_file)!=0){
    for(i in 0:((length(non_scratch_file)/4)-1) )
    {
      labelname <- non_scratch_file[i*4+1]
      isCut <- non_scratch_file[i*4+2]
      startMilli <- non_scratch_file[i*4+3]
      endMilli <- non_scratch_file[i*4+4]
      
      #labelname <- non_scratch_file[i]
      print(paste("filename",labelname))
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulationAllFeatures(data, FALSE, idx, window_size, window_stp, labelname, plotting,delay=delay,thresholdvar = thresholdvar)
      t<-autolabeling(paste(labelname,".csv",sep=""), "non", TRUE)
      
      if(length(sum_data)==0)
        sum_data <- subset(t, grepl(filter, t$label))
      else
        sum_data <- sumData(sum_data, subset(t, grepl(filter, t$label)), filter)
    }
  }
  
  if(length(scr_small_scratching)!=0){
    for(i in 0:((length(scr_small_scratching)/4)-1))
    {
      labelname <- scr_small_scratching[4*i+1]
      isCut <- scr_small_scratching[4*i+2]
      startMilli <- scr_small_scratching[4*i+3]
      endMilli <- scr_small_scratching[4*i+4]
      
      print(paste("filename",labelname))
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulationAllFeatures(data, isCut, idx, window_size, window_stp, labelname, plotting,delay=delay, startMilli = startMilli, endMilli = endMilli,thresholdvar = thresholdvar)
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


doSimulationAllFeatures <- function(data, cut, idx, window_size, window_step, save_filename, plotting = FALSE, 
                                    type = 1, delay=1, startMilli=2000, endMilli=2000, thresholdvar = 0.1, doFineWindow = TRUE)
{
  data.mag <- subset(data,grepl(list[8],data$type))
  data.gyro <- subset(data,grepl(list[3], data$type))
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
  window_num <- window_num -2
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

  window_idx <- 1
  window_set <- vector(mode="list", length=(4 + 3*(14+16+6+5) + (13+16+6+5) + (13+16+6+5) + 12) ) #   

  sname <- list[idx]
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
    paste(sname,"_energyFrom_0to10Hz_x",sep=""),paste(sname,"_energyFrom_0to10Hz_y",sep=""),paste(sname,"_energyFrom_0to10Hz_z",sep=""),
    paste(sname,"_RMS_x",sep=""),paste(sname,"_RMS_y",sep=""),paste(sname,"_RMS_z",sep=""),   
    paste(sname,"_SD_x",sep=""),paste(sname,"_SD_y",sep=""),paste(sname,"_SD_z",sep=""),
    
    paste(sname,"_integrated_RMS_x",sep=""),paste(sname,"_integrated_RMS_y",sep=""),paste(sname,"_integrated_RMS_z",sep=""),
    paste(sname,"_numPeak_x",sep=""),paste(sname,"_numPeak_y",sep=""),paste(sname,"_numPeak_z",sep=""),
    paste(sname,"_promientPeak_x",sep=""),paste(sname,"_promientPeak_y",sep=""),paste(sname,"_promientPeak_z",sep=""),
    paste(sname,"_weakPeak_x",sep=""),paste(sname,"_weakPeak_y",sep=""),paste(sname,"_weakPeak_z",sep=""),
    paste(sname,"_maxAuto_x",sep=""),paste(sname,"_maxAuto_y",sep=""),paste(sname,"_maxAuto_z",sep=""),
    paste(sname,"_height1stAuto_x",sep=""),paste(sname,"_height1stAuto_y",sep=""),paste(sname,"_height1stAuto_z",sep=""),
    paste(sname,"_powerBand0_1_x",sep=""),paste(sname,"_powerBand0_1_y",sep=""),paste(sname,"_powerBand0_1_z",sep=""),
    paste(sname,"_powerBand1_2_x",sep=""),paste(sname,"_powerBand1_2_y",sep=""),paste(sname,"_powerBand1_2_z",sep=""),
    paste(sname,"_powerBand2_3_x",sep=""),paste(sname,"_powerBand2_3_y",sep=""),paste(sname,"_powerBand2_3_z",sep=""),
    paste(sname,"_powerBand3_4_x",sep=""),paste(sname,"_powerBand3_4_y",sep=""),paste(sname,"_powerBand3_4_z",sep=""),
    paste(sname,"_powerBand4_5_x",sep=""),paste(sname,"_powerBand4_5_y",sep=""),paste(sname,"_powerBand4_5_z",sep=""),
    paste(sname,"_powerBand5_6_x",sep=""),paste(sname,"_powerBand5_6_y",sep=""),paste(sname,"_powerBand5_6_z",sep=""),
    paste(sname,"_powerBand6_7_x",sep=""),paste(sname,"_powerBand6_7_y",sep=""),paste(sname,"_powerBand6_7_z",sep=""),
    paste(sname,"_powerBand7_8_x",sep=""),paste(sname,"_powerBand7_8_y",sep=""),paste(sname,"_powerBand7_8_z",sep=""),
    paste(sname,"_powerBand8_9_x",sep=""),paste(sname,"_powerBand8_9_y",sep=""),paste(sname,"_powerBand8_9_z",sep=""),
    paste(sname,"_powerBand9_10_x",sep=""),paste(sname,"_powerBand9_10_y",sep=""),paste(sname,"_powerBand9_10_z",sep=""),
    
    paste(sname,"_getProminantPeakfreq_x",sep=""),paste(sname,"_getProminantPeakfreq_y",sep=""),paste(sname,"_getProminantPeakfreq_z",sep=""),
    paste(sname,"_getWeakPeakfreq_x",sep=""),paste(sname,"_getWeakPeakfreq_y",sep=""),paste(sname,"_getWeakPeakfreq_z",sep=""),
    paste(sname,"_prominentAutoPeakValley_x",sep=""),paste(sname,"_prominentAutoPeakValley_y",sep=""),paste(sname,"_prominentAutoPeakValley_z",sep=""),   
    paste(sname,"_weakpeakAutoPeakValley_x",sep=""),paste(sname,"_weakpeakAutoPeakValley_y",sep=""),paste(sname,"_weakpeakAutoPeakValley_z",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_x",sep=""),paste(sname,"_height1stPeakValleyAuto_y",sep=""),paste(sname,"_height1stPeakValleyAuto_z",sep=""),
    paste(sname,"_zerocrossingrate_x",sep=""),paste(sname,"_zerocrossingrate_y",sep=""),paste(sname,"_zerocrossingrate_z",sep=""),
    
    paste(sname,"_maximumCepstrumPeak_x",sep=""),paste(sname,"_maximumCepstrumPeak_y",sep=""),paste(sname,"_maximumCepstrumPeak_z",sep=""),
    paste(sname,"_prominentCepstrumPeak_x",sep=""),paste(sname,"_prominentCepstrumPeak_y",sep=""),paste(sname,"_prominentCepstrumPeak_z",sep=""),
    paste(sname,"_weakCepstrumPeak_x",sep=""),paste(sname,"_weakCepstrumPeak_y",sep=""),paste(sname,"_weakCepstrumPeak_z",sep=""),
    paste(sname,"_numberCepstrumPeak_x",sep=""),paste(sname,"_numberCepstrumPeak_y",sep=""),paste(sname,"_numberCepstrumPeak_z",sep=""),
    paste(sname,"_zerocrossingrateCepstrum_x",sep=""),paste(sname,"_zerocrossingrateCepstrum_y",sep=""),paste(sname,"_zerocrossingrateCepstrum_z",sep=""),
    
    
    #
    paste(sname,"_mean_avg",sep=""),
    paste(sname,"_max_avg",sep=""),
    paste(sname,"_min_avg",sep=""),
    paste(sname,"_entropy_avg",sep=""),
    paste(sname,"_energy_avg",sep=""),
    paste(sname,"_autocor1_avg",sep=""), 
    paste(sname,"_th_avg",sep=""),
    paste(sname,"_autocor2_avg",sep=""),
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
    paste(sname,"_powerBand0_1avg",sep=""),
    paste(sname,"_powerBand1_2avg",sep=""),
    paste(sname,"_powerBand2_3avg",sep=""),
    paste(sname,"_powerBand3_4avg",sep=""),
    paste(sname,"_powerBand4_5avg",sep=""),
    paste(sname,"_powerBand5_6avg",sep=""),
    paste(sname,"_powerBand6_7avg",sep=""),
    paste(sname,"_powerBand7_8avg",sep=""),
    paste(sname,"_powerBand8_9avg",sep=""),
    paste(sname,"_powerBand9_10avg",sep=""),
    paste(sname,"_getProminantPeakfreq_avg",sep=""),
    paste(sname,"_getWeakPeakfreq_avg",sep=""),
    paste(sname,"_prominentAutoPeakValley_avg",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_avg",sep=""),
    paste(sname,"_zerocrossingrate_avg",sep=""),
    
    paste(sname,"_maximumCepstrumPeak_avg",sep=""),
    paste(sname,"_prominentCepstrumPeak_avg",sep=""),
    paste(sname,"_weakCepstrumPeak_avg",sep=""),
    paste(sname,"_numberCepstrumPeak_avg",sep=""),
    paste(sname,"_zerocrossingrateCepstrum_avg",sep=""),
    
    
    #
    paste(sname,"_mean_avg(PC)",sep=""),
    paste(sname,"_max_avg(PC)",sep=""),
    paste(sname,"_min_avg(PC)",sep=""),
    paste(sname,"_entropy_avg(PC)",sep=""),
    paste(sname,"_energy_avg(PC)",sep=""),
    paste(sname,"_autocor1_avg(lag12PC)",sep=""),                         
    paste(sname,"_th_avg(PC)",sep=""),
    paste(sname,"_autocor2_avg(lag1PC)",sep=""),
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
    paste(sname,"_powerBand0_1avg(PC)",sep=""),
    paste(sname,"_powerBand1_2avg(PC)",sep=""),
    paste(sname,"_powerBand2_3avg(PC)",sep=""),
    paste(sname,"_powerBand3_4avg(PC)",sep=""),
    paste(sname,"_powerBand4_5avg(PC)",sep=""),
    paste(sname,"_powerBand5_6avg(PC)",sep=""),
    paste(sname,"_powerBand6_7avg(PC)",sep=""),
    paste(sname,"_powerBand7_8avg(PC)",sep=""),
    paste(sname,"_powerBand8_9avg(PC)",sep=""),
    paste(sname,"_powerBand9_10avg(PC)",sep=""),
    paste(sname,"_getProminantPeakfreq_avg(PC)",sep=""),
    paste(sname,"_getWeakPeakfreq_avg(PC)",sep=""),
    paste(sname,"_prominentAutoPeakValley_avg(PC)",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg(PC)",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_avg(PC)",sep=""),
    paste(sname,"_zerocrossingrate(PC)",sep=""),
    
    paste(sname,"_maximumCepstrumPeak_avg(PC)",sep=""),
    paste(sname,"_prominentCepstrumPeak_avg(PC)",sep=""),
    paste(sname,"_weakCepstrumPeak_avg(PC)",sep=""),
    paste(sname,"_numberCepstrumPeak_avg(PC)",sep=""),
    paste(sname,"_zerocrossingrateCepstrum_avg(PC)",sep=""),
    
    ###
    paste(sname,"_RMS_avg(b_a)",sep=""),
    paste(sname,"_cor_avg(b_a)",sep=""),
    paste(sname,"_peakfreq_avg(b_a)",sep=""),
    paste(sname,"_entropy_avg(b_a)",sep=""),
    paste(sname,"_energy_avg(b_a)",sep=""),
    paste(sname,"_maxAuto_avg(b_a)",sep=""),
    paste(sname,"_getProminantPeakfreq_avg(b_a)",sep=""),
    paste(sname,"_getWeakPeakfreq_avg(b_a)",sep=""),
    paste(sname,"_prominentAutoPeakValley_avg(b_a)",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg(b_a)",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_avg(b_a)",sep=""),
    paste(sname,"_zerocrossingrate(b_a)",sep="")
    
  )
  candidates_idx <- 1

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
    #print(paste("var(magnitude)",var(magnitude)))
    if(var(magnitude)>thresholdvar){
      #print("var(magnitude)>thresholdvar")
      if(doFineWindow){
        first_1s <- getWindow(data.mag, window_idx, window_size/3)
        last_1s <- getWindow(data.mag, window_idx+window_size/3*2,window_size/3)
        
        first_1s <- sqrt( (first_1s$x+50)^2+(first_1s$y+50)^2+(first_1s$z+50)^2 )
        first_1s <- first_1s - mean(first_1s)
        
        last_1s <- sqrt( (last_1s$x+50)^2+(last_1s$y+50)^2+(last_1s$z+50)^2 )
        last_1s <- last_1s - mean(last_1s)
      }else{
        first_1s <-c(0,0)
        last_1s <-c(0,0)
      }
      
      if( (doFineWindow==FALSE) | ((var(first_1s) > thresholdvar) & (var(last_1s) > thresholdvar)) )
      {
        
        epoch<-1
        label<-"TODO"
        rect <- data.frame(xmin=data.sub$time[window_idx], xmax=data.sub$time[window_idx+nrow(window_data)-2], ymin=-Inf, ymax=Inf)
        returnValue <- returnValue + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill="blue", inherit.aes = FALSE)        
        
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
               getFeatureBy(window_df,"energyFrom_0to10Hz"),
               getFeatureBy(window_df,"RMS"),
               getFeatureBy(window_df,"SD"),
               
               getFeatureBy(window_df,"integratedRMS"),
               getFeatureBy(window_df,"peaknumAuto",filtering = TRUE),
               getFeatureBy(window_df,"prominentAuto",filtering = TRUE),
               getFeatureBy(window_df,"weakpeakAuto",filtering = TRUE),
               getFeatureBy(window_df,"maximumAuto",filtering = TRUE),
               getFeatureBy(window_df,"height1stAuto",filtering = TRUE),             
               getFeatureBy(window_df,"powerband",powerband_from = 0,powerband_to = 1),
               getFeatureBy(window_df,"powerband",powerband_from = 1,powerband_to = 2),
               getFeatureBy(window_df,"powerband",powerband_from = 2,powerband_to = 3),
               getFeatureBy(window_df,"powerband",powerband_from = 3,powerband_to = 4),
               getFeatureBy(window_df,"powerband",powerband_from = 4,powerband_to = 5),
               getFeatureBy(window_df,"powerband",powerband_from = 5,powerband_to = 6),
               getFeatureBy(window_df,"powerband",powerband_from = 6,powerband_to = 7),
               getFeatureBy(window_df,"powerband",powerband_from = 7,powerband_to = 8),
               getFeatureBy(window_df,"powerband",powerband_from = 8,powerband_to = 9),
               getFeatureBy(window_df,"powerband",powerband_from = 9,powerband_to = 10),
               
               getFeatureBy(window_df,"getProminantPeakfreq",filtering = TRUE),
               getFeatureBy(window_df,"getWeakPeakfreq",filtering = TRUE),
               getFeatureBy(window_df,"prominentAutoPeakValley",filtering = TRUE),
               getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = TRUE),
               getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = TRUE),
               getFeatureBy(window_df,"zerocrossingrate",filtering = TRUE),
               
               getFeatureBy(window_df,"maximumCepstrumPeak",filtering = TRUE),
               getFeatureBy(window_df,"prominentCepstrumPeak",filtering = TRUE,min_th = 0.1),
               getFeatureBy(window_df,"weakCepstrumPeak",filtering = TRUE,max_th = 0.1),
               getFeatureBy(window_df,"numberCepstrumPeak",filtering = TRUE),
               getFeatureBy(window_df,"zerocrossingrateCepstrum",filtering = TRUE),
               
               ##           
               getFeatureBy(window_df,"mean",avg=TRUE),
               getFeatureBy(window_df,"max",avg=TRUE),
               getFeatureBy(window_df,"min",avg=TRUE),
               getFeatureBy(window_df,"entropy",avg=TRUE),   
               getFeatureBy(window_df,"energy",avg=TRUE),
               getFeatureBy(window_df,"autocorrelation",12,avg=TRUE),
               getFeatureBy(window_df,"threshold",avg=TRUE),
               getFeatureBy(window_df,"autocorrelation", 1,avg=TRUE),
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
               
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 0,powerband_to = 1),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 1,powerband_to = 2),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 2,powerband_to = 3),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 3,powerband_to = 4),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 4,powerband_to = 5),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 5,powerband_to = 6),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 6,powerband_to = 7),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 7,powerband_to = 8),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 8,powerband_to = 9),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 9,powerband_to = 10),
               
               getFeatureBy(window_df,"getProminantPeakfreq",filtering = TRUE,avg=TRUE),
               getFeatureBy(window_df,"getWeakPeakfreq",filtering = TRUE,avg=TRUE),
               getFeatureBy(window_df,"prominentAutoPeakValley",filtering = TRUE,avg=TRUE),
               getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = TRUE,avg=TRUE),
               getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = TRUE,avg=TRUE),
               getFeatureBy(window_df,"zerocrossingrate",filtering = TRUE,avg=TRUE),
               
               getFeatureBy(window_df,"maximumCepstrumPeak",filtering = TRUE,avg=TRUE),
               getFeatureBy(window_df,"prominentCepstrumPeak",filtering = TRUE,avg=TRUE,min_th = 0.1),
               getFeatureBy(window_df,"weakCepstrumPeak",filtering = TRUE,avg=TRUE,max_th = 0.1),
               getFeatureBy(window_df,"numberCepstrumPeak",filtering = TRUE,avg=TRUE),
               getFeatureBy(window_df,"zerocrossingrateCepstrum",filtering = TRUE,avg=TRUE),
               
               ##
               getFeatureBy(window_df,"mean",avg=TRUE,type="PC"),
               getFeatureBy(window_df,"max",avg=TRUE,type="PC"),
               getFeatureBy(window_df,"min",avg=TRUE,type="PC"),
               getFeatureBy(window_df,"entropy",avg=TRUE,type="PC"),   
               getFeatureBy(window_df,"energy",avg=TRUE,type="PC"),
               getFeatureBy(window_df,"autocorrelation",12,avg=TRUE,type="PC"),
               getFeatureBy(window_df,"threshold",avg=TRUE,type="PC"),
               getFeatureBy(window_df,"autocorrelation", 1,avg=TRUE,type="PC"),
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
               
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 0,powerband_to = 1, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 1,powerband_to = 2, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 2,powerband_to = 3, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 3,powerband_to = 4, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 4,powerband_to = 5, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 5,powerband_to = 6, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 6,powerband_to = 7, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 7,powerband_to = 8, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 8,powerband_to = 9, type="PC"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 9,powerband_to = 10, type="PC"),
               
               getFeatureBy(window_df,"getProminantPeakfreq",filtering = TRUE,avg=TRUE, type="PC"),
               getFeatureBy(window_df,"getWeakPeakfreq",filtering = TRUE,avg=TRUE, type="PC"),
               getFeatureBy(window_df,"prominentAutoPeakValley",filtering = TRUE,avg=TRUE, type="PC"),
               getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = TRUE,avg=TRUE, type="PC"),
               getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = TRUE,avg=TRUE, type="PC"),
               getFeatureBy(window_df,"zerocrossingrate",filtering = TRUE,avg=TRUE, type="PC"),
               
               getFeatureBy(window_df,"maximumCepstrumPeak",filtering = TRUE,avg=TRUE, type="PC"),
               getFeatureBy(window_df,"prominentCepstrumPeak",filtering = TRUE,avg=TRUE, type="PC",min_th = 0.1),
               getFeatureBy(window_df,"weakCepstrumPeak",filtering = TRUE,avg=TRUE, type="PC",max_th = 0.1),
               getFeatureBy(window_df,"numberCepstrumPeak",filtering = TRUE,avg=TRUE, type="PC"),
               getFeatureBy(window_df,"zerocrossingrateCepstrum",filtering = TRUE,avg=TRUE, type="PC"),
               
               ####
               getFeatureBy(window_df,"RMS",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"correlation",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"peakfreq",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"entropy",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"energy",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"maximumAuto",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"getProminantPeakfreq",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"getWeakPeakfreq",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"prominentAutoPeakValley",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"zerocrossingrate",filtering = TRUE, b_avg = TRUE)         
               
        )
        
        
        window_set<-rbind(window_set,p) 
      }
      
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

