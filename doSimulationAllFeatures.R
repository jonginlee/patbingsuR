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
  window_set <- vector(mode="list", length=(4  + (12-2) +(12-2)*3 + (12-2)*3  + (18+1+1) + (18+1+1)*3 + (18+1+1)*3 )) #   
  sname <- list[idx]
  #window_set <- vector(mode="list", length=(31 - 8) ) # previous work
  names(window_set) <- c(
    "epoches","start_milli","end_milli","label",
    
    #
    paste(sname,"_mean_avg",sep=""),
    paste(sname,"_max_avg",sep=""),
    paste(sname,"_min_avg",sep=""),
    paste(sname,"_entropy_avg",sep=""),
    paste(sname,"_autocor1_avg",sep=""), 
    paste(sname,"_th_avg",sep=""),
    paste(sname,"_var_avg",sep=""),
    paste(sname,"_peakfreq_avg",sep=""),
    paste(sname,"_RMS_avg",sep=""),
    paste(sname,"_integrated_RMS_avg",sep=""),
    
    #
    paste(sname,"_mean_x",sep=""), paste(sname,"_mean_y",sep=""), paste(sname,"_mean_z",sep=""),
    paste(sname,"_max_x",sep=""), paste(sname,"_max_y",sep=""), paste(sname,"_max_z",sep=""),
    paste(sname,"_min_x",sep=""),  paste(sname,"_min_y",sep=""),  paste(sname,"_min_z",sep=""),
    paste(sname,"_entropy_x",sep=""),   paste(sname,"_entropy_y",sep=""),   paste(sname,"_entropy_z",sep=""),
    paste(sname,"_autocor1_x",sep=""),   paste(sname,"_autocor1_y",sep=""),   paste(sname,"_autocor1_z",sep=""), 
    paste(sname,"_th_x",sep=""),    paste(sname,"_th_y",sep=""),    paste(sname,"_th_z",sep=""),
    paste(sname,"_var_x",sep=""), paste(sname,"_var_y",sep=""), paste(sname,"_var_z",sep=""),
    paste(sname,"_peakfreq_x",sep=""),paste(sname,"_peakfreq_y",sep=""),paste(sname,"_peakfreq_z",sep=""),
    paste(sname,"_RMS_x",sep=""), paste(sname,"_RMS_y",sep=""), paste(sname,"_RMS_z",sep=""),
    paste(sname,"_integrated_RMS_x",sep=""), paste(sname,"_integrated_RMS_y",sep=""), paste(sname,"_integrated_RMS_z",sep=""),
    

    #
    paste(sname,"_mean_x(PC)",sep=""), paste(sname,"_mean_y(PC)",sep=""), paste(sname,"_mean_z(PC)",sep=""),
    paste(sname,"_max_x(PC)",sep=""), paste(sname,"_max_y(PC)",sep=""), paste(sname,"_max_z(PC)",sep=""),
    paste(sname,"_min_x(PC)",sep=""),  paste(sname,"_min_y(PC)",sep=""),  paste(sname,"_min_z(PC)",sep=""),
    paste(sname,"_entropy_x(PC)",sep=""),   paste(sname,"_entropy_y(PC)",sep=""),   paste(sname,"_entropy_z(PC)",sep=""),
    paste(sname,"_autocor1_x(PC)",sep=""),   paste(sname,"_autocor1_y(PC)",sep=""),   paste(sname,"_autocor1_z(PC)",sep=""), 
    paste(sname,"_th_x(PC)",sep=""),    paste(sname,"_th_y(PC)",sep=""),    paste(sname,"_th_z(PC)",sep=""),
    paste(sname,"_var_x(PC)",sep=""), paste(sname,"_var_y(PC)",sep=""), paste(sname,"_var_z(PC)",sep=""),
    paste(sname,"_peakfreq_x(PC)",sep=""),paste(sname,"_peakfreq_y(PC)",sep=""),paste(sname,"_peakfreq_z(PC)",sep=""),
    paste(sname,"_RMS_x(PC)",sep=""), paste(sname,"_RMS_y(PC)",sep=""), paste(sname,"_RMS_z(PC)",sep=""),
    paste(sname,"_integrated_RMS_x(PC)",sep=""), paste(sname,"_integrated_RMS_y(PC)",sep=""), paste(sname,"_integrated_RMS_z(PC)",sep=""),
    
    
    paste(sname,"_auto_powerBand0_1avg(b_a)",sep=""),
    paste(sname,"_auto_powerBand1_2avg(b_a)",sep=""),
    paste(sname,"_auto_powerBand2_3avg(b_a)",sep=""),
    paste(sname,"_auto_powerBand3_4avg(b_a)",sep=""),
    paste(sname,"_auto_powerBand4_5avg(b_a)",sep=""),
    paste(sname,"_prominentAutoPeakValley_avg(b_a)",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg(b_a)",sep=""),   
    paste(sname,"_variance_avg(b_a)",sep=""),      
    
    paste(sname,"_height1stPeakValleyAuto(b_a)",sep=""),    
    paste(sname,"_crossingAmplitudePower(b_a)",sep=""),
    paste(sname,"_getCrossingSpacePower(b_a)",sep=""),   
    paste(sname,"_harmPeak(b_a)",sep=""),   
    
    paste(sname,"_getCrossingKurtosisPower(b_a)",sep=""),
    paste(sname,"_strongAmplitudeBins(b_a)",sep=""),
    paste(sname,"_weakAmplitudeBins(b_a)",sep=""),
    paste(sname,"_autocorrelationBins_1_30_(b_a)",sep=""),
    paste(sname,"_autocorrelationBins_30_60_(b_a)",sep=""),
    paste(sname,"_autocorrelationBins_60_90_(b_a)",sep=""),
    paste(sname,"_autocorrelationBins_90_120_(b_a)",sep=""),
    paste(sname,"_autocorrelationBins_120_150_(b_a)",sep=""),
    #
    paste(sname,"_auto_powerBand0_1avg(x)",sep=""),  paste(sname,"_auto_powerBand0_1avg(y)",sep=""),  paste(sname,"_auto_powerBand0_1avg(z)",sep=""),
    paste(sname,"_auto_powerBand1_2avg(x)",sep=""), paste(sname,"_auto_powerBand1_2avg(y)",sep=""), paste(sname,"_auto_powerBand1_2avg(z)",sep=""),
    paste(sname,"_auto_powerBand2_3avg(x)",sep=""), paste(sname,"_auto_powerBand2_3avg(y)",sep=""), paste(sname,"_auto_powerBand2_3avg(z)",sep=""),
    paste(sname,"_auto_powerBand3_4avg(x)",sep=""),  paste(sname,"_auto_powerBand3_4avg(y)",sep=""),  paste(sname,"_auto_powerBand3_4avg(z)",sep=""),
    paste(sname,"_auto_powerBand4_5avg(x)",sep=""),  paste(sname,"_auto_powerBand4_5avg(y)",sep=""),  paste(sname,"_auto_powerBand4_5avg(z)",sep=""),
    paste(sname,"_prominentAutoPeakValley_avg(x)",sep=""), paste(sname,"_prominentAutoPeakValley_avg(y)",sep=""), paste(sname,"_prominentAutoPeakValley_avg(z)",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg(x)",sep=""),  paste(sname,"_weakpeakAutoPeakValley_avg(y)",sep=""),   paste(sname,"_weakpeakAutoPeakValley_avg(z)",sep=""),      
    paste(sname,"_variance_avg(x)",sep=""),  paste(sname,"_variance_avg(y)",sep=""),   paste(sname,"_variance_avg(z)",sep=""),      
    
    paste(sname,"_height1stPeakValleyAuto(x)",sep=""),     paste(sname,"_height1stPeakValleyAuto(y)",sep=""), paste(sname,"_height1stPeakValleyAuto(z)",sep=""),
    paste(sname,"_crossingAmplitudePower(x)",sep=""),  paste(sname,"_crossingAmplitudePower(y)",sep=""),  paste(sname,"_crossingAmplitudePower(z)",sep=""),
    paste(sname,"_getCrossingSpacePower(x)",sep=""),  paste(sname,"_getCrossingSpacePower(y)",sep=""),  paste(sname,"_getCrossingSpacePower(z)",sep=""),   
    paste(sname,"_harmPeak(x)",sep=""),   paste(sname,"_harmPeak(y)",sep=""),  paste(sname,"_harmPeak(z)",sep=""), 
    
    paste(sname,"_getCrossingKurtosisPower(x)",sep=""),  paste(sname,"_getCrossingKurtosisPower(y)",sep=""),  paste(sname,"_getCrossingKurtosisPower(z)",sep=""),
    paste(sname,"_strongAmplitudeBins(x)",sep=""), paste(sname,"_strongAmplitudeBins(y)",sep=""), paste(sname,"_strongAmplitudeBins(z)",sep=""),
    paste(sname,"_weakAmplitudeBins(x)",sep=""),  paste(sname,"_weakAmplitudeBins(y)",sep=""),  paste(sname,"_weakAmplitudeBins(z)",sep=""),
    paste(sname,"_autocorrelationBins_1_30_(x)",sep=""),  paste(sname,"_autocorrelationBins_1_30_(y)",sep=""),  paste(sname,"_autocorrelationBins_1_30_(z)",sep=""),
    paste(sname,"_autocorrelationBins_30_60_(x)",sep=""),  paste(sname,"_autocorrelationBins_30_60_(y)",sep=""),  paste(sname,"_autocorrelationBins_30_60_(z)",sep=""),
    paste(sname,"_autocorrelationBins_60_90_(x)",sep=""), paste(sname,"_autocorrelationBins_60_90_(y)",sep=""), paste(sname,"_autocorrelationBins_60_90_(z)",sep=""),
    paste(sname,"_autocorrelationBins_90_120_(x)",sep=""),  paste(sname,"_autocorrelationBins_90_120_(y)",sep=""),  paste(sname,"_autocorrelationBins_90_120_(z)",sep=""),
    paste(sname,"_autocorrelationBins_120_150_(x)",sep=""),  paste(sname,"_autocorrelationBins_120_150_(y)",sep=""),  paste(sname,"_autocorrelationBins_120_150_(z)",sep=""),
    
    
    #
    paste(sname,"_auto_powerBand0_1avg(PC_x)",sep=""),  paste(sname,"_auto_powerBand0_1avg(PC_y)",sep=""),  paste(sname,"_auto_powerBand0_1avg(PC_z)",sep=""),
    paste(sname,"_auto_powerBand1_2avg(PC_x)",sep=""), paste(sname,"_auto_powerBand1_2avg(PC_y)",sep=""), paste(sname,"_auto_powerBand1_2avg(PC_z)",sep=""),
    paste(sname,"_auto_powerBand2_3avg(PC_x)",sep=""), paste(sname,"_auto_powerBand2_3avg(PC_y)",sep=""), paste(sname,"_auto_powerBand2_3avg(PC_z)",sep=""),
    paste(sname,"_auto_powerBand3_4avg(PC_x)",sep=""),  paste(sname,"_auto_powerBand3_4avg(PC_y)",sep=""),  paste(sname,"_auto_powerBand3_4avg(PC_z)",sep=""),
    paste(sname,"_auto_powerBand4_5avg(PC_x)",sep=""),  paste(sname,"_auto_powerBand4_5avg(PC_y)",sep=""),  paste(sname,"_auto_powerBand4_5avg(PC_z)",sep=""),
    paste(sname,"_prominentAutoPeakValley_avg(PC_x)",sep=""), paste(sname,"_prominentAutoPeakValley_avg(PC_y)",sep=""), paste(sname,"_prominentAutoPeakValley_avg(PC_z)",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg(PC_x)",sep=""),  paste(sname,"_weakpeakAutoPeakValley_avg(PC_y)",sep=""),   paste(sname,"_weakpeakAutoPeakValley_avg(PC_z)",sep=""),      
    paste(sname,"_variance_avg(PC_x)",sep=""),  paste(sname,"_variance_avg(PC_y)",sep=""),   paste(sname,"_variance_avg(PC_z)",sep=""),      
    
    paste(sname,"_height1stPeakValleyAuto(PC_x)",sep=""),     paste(sname,"_height1stPeakValleyAuto(PC_y)",sep=""), paste(sname,"_height1stPeakValleyAuto(PC_z)",sep=""),
    paste(sname,"_crossingAmplitudePower(PC_x)",sep=""),  paste(sname,"_crossingAmplitudePower(PC_y)",sep=""),  paste(sname,"_crossingAmplitudePower(PC_z)",sep=""),
    paste(sname,"_getCrossingSpacePower(PC_x)",sep=""),  paste(sname,"_getCrossingSpacePower(PC_y)",sep=""),  paste(sname,"_getCrossingSpacePower(PC_z)",sep=""),   
    paste(sname,"_harmPeak(PC_x)",sep=""),   paste(sname,"_harmPeak(PC_y)",sep=""),  paste(sname,"_harmPeak(PC_z)",sep=""), 
    
    paste(sname,"_getCrossingKurtosisPower(PC_x)",sep=""),  paste(sname,"_getCrossingKurtosisPower(PC_y)",sep=""),  paste(sname,"_getCrossingKurtosisPower(PC_z)",sep=""),
    paste(sname,"_strongAmplitudeBins(PC_x)",sep=""), paste(sname,"_strongAmplitudeBins(PC_y)",sep=""), paste(sname,"_strongAmplitudeBins(PC_z)",sep=""),
    paste(sname,"_weakAmplitudeBins(PC_x)",sep=""),  paste(sname,"_weakAmplitudeBins(PC_y)",sep=""),  paste(sname,"_weakAmplitudeBins(PC_z)",sep=""),
    paste(sname,"_autocorrelationBins_1_30_(PC_x)",sep=""),  paste(sname,"_autocorrelationBins_1_30_(PC_y)",sep=""),  paste(sname,"_autocorrelationBins_1_30_(PC_z)",sep=""),
    paste(sname,"_autocorrelationBins_30_60_(PC_x)",sep=""),  paste(sname,"_autocorrelationBins_30_60_(PC_y)",sep=""),  paste(sname,"_autocorrelationBins_30_60_(PC_z)",sep=""),
    paste(sname,"_autocorrelationBins_60_90_(PC_x)",sep=""), paste(sname,"_autocorrelationBins_60_90_(PC_y)",sep=""), paste(sname,"_autocorrelationBins_60_90_(PC_z)",sep=""),
    paste(sname,"_autocorrelationBins_90_120_(PC_x)",sep=""),  paste(sname,"_autocorrelationBins_90_120_(PC_y)",sep=""),  paste(sname,"_autocorrelationBins_90_120_(PC_z)",sep=""),
    paste(sname,"_autocorrelationBins_120_150_(PC_x)",sep=""),  paste(sname,"_autocorrelationBins_120_150_(PC_y)",sep=""),  paste(sname,"_autocorrelationBins_120_150_(PC_z)",sep="")
    
    
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
                            z=window_data$z,  time =window_data$time)
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
        
        if(length(window_data_prev)!=0){
          epoch<-1
          label<-"TODO"
          
          if(plotting){
            rect <- data.frame(xmin=data.sub$time[window_idx], xmax=data.sub$time[window_idx+nrow(window_data)-2], ymin=-Inf, ymax=Inf)
            returnValue <- returnValue + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill="blue", inherit.aes = FALSE)        
          }
          
          p <- c(epoch,start_milli,end_milli,label,
                 
                 getFeatureBy(window_df,prefiltering = T,"mean",avg=TRUE),
                 getFeatureBy(window_df,prefiltering = T,"max",avg=TRUE),
                 getFeatureBy(window_df,prefiltering = T,"min",avg=TRUE),
                 getFeatureBy(window_df,prefiltering = T,"entropy",avg=TRUE),   
                 getFeatureBy(window_df,prefiltering = T,"autocorrelation",1,avg=TRUE),
                 getFeatureBy(window_df,prefiltering = T,"threshold",avg=TRUE),
                 getFeatureBy(window_df,prefiltering = T,"variance",avg=TRUE),
                 getFeatureBy(window_df,prefiltering = T,"peakfreq",avg=TRUE),
                 getFeatureBy(window_df,prefiltering = T,"RMS",avg=TRUE),
                 getFeatureBy(window_df,prefiltering = T,"integratedRMS",avg=TRUE),
                 
                 ####           
                 getFeatureBy(window_df,prefiltering = T,"mean",avg=F),
                 getFeatureBy(window_df,prefiltering = T,"max",avg=F),
                 getFeatureBy(window_df,prefiltering = T,"min",avg=F),
                 getFeatureBy(window_df,prefiltering = T,"entropy",avg=F),   
                 getFeatureBy(window_df,prefiltering = T,"autocorrelation",1,avg=F),
                 getFeatureBy(window_df,prefiltering = T,"threshold",avg=F),
                 getFeatureBy(window_df,prefiltering = T,"variance",avg=F),
                 getFeatureBy(window_df,prefiltering = T,"peakfreq",avg=F),
                 getFeatureBy(window_df,prefiltering = T,"RMS",avg=F),
                 getFeatureBy(window_df,prefiltering = T,"integratedRMS",avg=F),
                 
                 
                 ####
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"mean",avg=F,type="PC"),
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"max",avg=F,type="PC"),
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"min",avg=F,type="PC"),
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"entropy",avg=F,type="PC"),   
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"autocorrelation",1,avg=F,type="PC"),
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"threshold",avg=F,type="PC"),
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"variance",avg=F,type="PC"),
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"peakfreq",avg=F,type="PC"),
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"RMS",avg=F, type="PC"),
                 getFeatureBy(window_df,prefiltering = T,rotationBy = T,"integratedRMS",avg=F, type="PC"),
                                
                 ###
                            
                 ###
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=TRUE,powerband_from = 0,powerband_to = 1, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=TRUE,powerband_from = 1,powerband_to = 2, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=TRUE,powerband_from = 2,powerband_to = 3, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=TRUE,powerband_from = 3,powerband_to = 4, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=TRUE,powerband_from = 4,powerband_to = 5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"prominentAutoPeakValley", prefiltering = T,rotationBy = F, filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"weakpeakAutoPeakValley",prefiltering = T,rotationBy = F, filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"variance",prefiltering = T,rotationBy = F, filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"height1stPeakValleyAuto",prefiltering = T,rotationBy = F, filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"crossingAmplitudePower",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"getCrossingSpacePower",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"harmPeak",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"getCrossingKurtosisPower",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"strongAmplitudeBins",prefiltering = T,rotationBy = F, filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"weakAmplitudeBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE,powerband_from = 1,powerband_to = 29, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE,powerband_from = 29,powerband_to = 58, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE,powerband_from = 58,powerband_to = 87, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE,powerband_from = 87,powerband_to = 116, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins", prefiltering = T,rotationBy = F,filtering = F, b_avg = TRUE,powerband_from = 116,powerband_to = 145, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 ###
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=F,powerband_from = 0,powerband_to = 1, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=F,powerband_from = 1,powerband_to = 2, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=F,powerband_from = 2,powerband_to = 3, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=F,powerband_from = 3,powerband_to = 4, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = F,filtering = F,b_avg=F,powerband_from = 4,powerband_to = 5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"prominentAutoPeakValley", prefiltering = T,rotationBy = F, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"weakpeakAutoPeakValley",prefiltering = T,rotationBy = F, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"variance",prefiltering = T,rotationBy = F, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"height1stPeakValleyAuto",prefiltering = T,rotationBy = F, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"crossingAmplitudePower",prefiltering = T,rotationBy = F,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"getCrossingSpacePower",prefiltering = T,rotationBy = F,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"harmPeak",prefiltering = T,rotationBy = F,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"getCrossingKurtosisPower",prefiltering = T,rotationBy = F,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"strongAmplitudeBins",prefiltering = T,rotationBy = F, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"weakAmplitudeBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = F,powerband_from = 1,powerband_to = 29, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = F,powerband_from = 29,powerband_to = 58, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = F,powerband_from = 58,powerband_to = 87, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = F,filtering = F, b_avg = F,powerband_from = 87,powerband_to = 116, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins", prefiltering = T,rotationBy = F,filtering = F, b_avg = F,powerband_from = 116,powerband_to = 145, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 ###
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = T,filtering = F,b_avg=F,powerband_from = 0,powerband_to = 1, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = T,filtering = F,b_avg=F,powerband_from = 1,powerband_to = 2, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = T,filtering = F,b_avg=F,powerband_from = 2,powerband_to = 3, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = T,filtering = F,b_avg=F,powerband_from = 3,powerband_to = 4, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",prefiltering = T,rotationBy = T,filtering = F,b_avg=F,powerband_from = 4,powerband_to = 5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"prominentAutoPeakValley", prefiltering = T,rotationBy = T, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"weakpeakAutoPeakValley",prefiltering = T,rotationBy = T, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"variance",prefiltering = T,rotationBy = T, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"height1stPeakValleyAuto",prefiltering = T,rotationBy = T, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"crossingAmplitudePower",prefiltering = T,rotationBy = T,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"getCrossingSpacePower",prefiltering = T,rotationBy = T,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"harmPeak",prefiltering = T,rotationBy = T,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"getCrossingKurtosisPower",prefiltering = T,rotationBy = T,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"strongAmplitudeBins",prefiltering = T,rotationBy = T, filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"weakAmplitudeBins",prefiltering = T,rotationBy = T,filtering = F, b_avg = F, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = T,filtering = F, b_avg = F,powerband_from = 1,powerband_to = (29), signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = T,filtering = F, b_avg = F,powerband_from = 29,powerband_to = (29+29), signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = T,filtering = F, b_avg = F,powerband_from = 58,powerband_to = (58+29), signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins",prefiltering = T,rotationBy = T,filtering = F, b_avg = F,powerband_from = 87,powerband_to = (87+29), signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"autocorrelationBins", prefiltering = T,rotationBy = T,filtering = F, b_avg = F,powerband_from = 116,powerband_to = 145, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.4, filtering2 = F,window_data_prev = window_data_prev)
                 
                 
          )
          window_set<-rbind(window_set,p) 
        }
        window_data_prev <- window_df
        
        
       
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

