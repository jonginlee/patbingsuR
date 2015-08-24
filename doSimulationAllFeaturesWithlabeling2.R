

doSimulationAllFeaturesWithLabeling2 <- function(data, cut, idx, window_size, window_step, scr_info, save_filename=FALSE, plotting = FALSE, type = 1, delay=1, startMilli=2000, endMilli=2000, thresholdvar = 0.1,labeling=TRUE)
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
  window_set <- vector(mode="list", length=(4 + 3*(14+16+6) + (13+16+6) + (13+16+6) + 12) ) #   
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
    
    paste(sname,"_getProminantPeakfreq_x",sep=""),paste(sname,"_getProminantPeakfreq_y",sep=""),paste(sname,"_getProminantPeakfreq_z",sep=""),
    paste(sname,"_getWeakPeakfreq_x",sep=""),paste(sname,"_getWeakPeakfreq_y",sep=""),paste(sname,"_getWeakPeakfreq_z",sep=""),
    paste(sname,"_prominentAutoPeakValley_x",sep=""),paste(sname,"_prominentAutoPeakValley_y",sep=""),paste(sname,"_prominentAutoPeakValley_z",sep=""),   
    paste(sname,"_weakpeakAutoPeakValley_x",sep=""),paste(sname,"_weakpeakAutoPeakValley_y",sep=""),paste(sname,"_weakpeakAutoPeakValley_z",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_x",sep=""),paste(sname,"_height1stPeakValleyAuto_y",sep=""),paste(sname,"_height1stPeakValleyAuto_z",sep=""),
    paste(sname,"_zerocrossingrate_x",sep=""),paste(sname,"_zerocrossingrate_y",sep=""),paste(sname,"_zerocrossingrate_z",sep=""),
    
    
    
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
    paste(sname,"_getProminantPeakfreq_avg",sep=""),
    paste(sname,"_getWeakPeakfreq_avg",sep=""),
    paste(sname,"_prominentAutoPeakValley_avg",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_avg",sep=""),
    paste(sname,"_zerocrossingrate_avg",sep=""),
    
    
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
    paste(sname,"_getProminantPeakfreq_avg(PC)",sep=""),
    paste(sname,"_getWeakPeakfreq_avg(PC)",sep=""),
    paste(sname,"_prominentAutoPeakValley_avg(PC)",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg(PC)",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_avg(PC)",sep=""),
    paste(sname,"_zerocrossingrate(PC)",sep=""),
    
    
    
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
             
             getFeatureBy(window_df,"getProminantPeakfreq",filtering = TRUE),
             getFeatureBy(window_df,"getWeakPeakfreq",filtering = TRUE),
             getFeatureBy(window_df,"prominentAutoPeakValley",filtering = TRUE),
             getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = TRUE),
             getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = TRUE),
             getFeatureBy(window_df,"zerocrossingrate",filtering = TRUE),
             
             
             
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
             
             getFeatureBy(window_df,"getProminantPeakfreq",filtering = TRUE,avg=TRUE),
             getFeatureBy(window_df,"getWeakPeakfreq",filtering = TRUE,avg=TRUE),
             getFeatureBy(window_df,"prominentAutoPeakValley",filtering = TRUE,avg=TRUE),
             getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = TRUE,avg=TRUE),
             getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = TRUE,avg=TRUE),
             getFeatureBy(window_df,"zerocrossingrate",filtering = TRUE,avg=TRUE),
             
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
             
             getFeatureBy(window_df,"getProminantPeakfreq",filtering = TRUE,avg=TRUE, type="PC"),
             getFeatureBy(window_df,"getWeakPeakfreq",filtering = TRUE,avg=TRUE, type="PC"),
             getFeatureBy(window_df,"prominentAutoPeakValley",filtering = TRUE,avg=TRUE, type="PC"),
             getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = TRUE,avg=TRUE, type="PC"),
             getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = TRUE,avg=TRUE, type="PC"),
             getFeatureBy(window_df,"zerocrossingrate",filtering = TRUE,avg=TRUE, type="PC"),
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

staticFeatures<-c("mean","max","min","entropy","energy","threshold","variance","peakfreq","RMS", "SD", "integrated_RMS")
powerbandFeatures <- c("powerBand0_2.5","powerBand2.5_5","powerBand5_7.5","powerBand7.5_10","powerBand10_12.5","powerBand12.5_15",
                       "powerBand15_17.5","powerBand17.5_20","powerBand20_22.5","powerBand22.5_25")
peakFeatures<-c("numPeak", "prominentPeak","prominentValley","weakValley", "weakPeak", "maxPeak","minPeak","kurtosis","height1stPeak", 
                "height1stValley", "zerocrossingRate", "harmonic")


getFeatureNames <- function(sname, setNum = 1, d=1)
{
  for(i in )
  
}



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
paste(sname,"_RMS_x",sep=""),paste(sname,"_RMS_y",sep=""),paste(sname,"_RMS_z",sep=""),   
paste(sname,"_SD_x",sep=""),paste(sname,"_SD_y",sep=""),paste(sname,"_SD_z",sep=""),



doSimulationAllFeaturesWithLabeling3 <- function(data, cut, idx, window_size, window_step, scr_info, save_filename=FALSE, plotting = FALSE, type = 1, delay=1, startMilli=2000, endMilli=2000, thresholdvar = 0.1,labeling=TRUE)
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
  window_set <- vector(mode="list", length=(4 + 3*(14+16-2+5) + (13+16-2+5) + (13+16-2+5) + (12-2+5+5) )) #   
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
    
    paste(sname,"_prominentAutoPeakValley_x",sep=""),paste(sname,"_prominentAutoPeakValley_y",sep=""),paste(sname,"_prominentAutoPeakValley_z",sep=""),   
    paste(sname,"_weakpeakAutoPeakValley_x",sep=""),paste(sname,"_weakpeakAutoPeakValley_y",sep=""),paste(sname,"_weakpeakAutoPeakValley_z",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_x",sep=""),paste(sname,"_height1stPeakValleyAuto_y",sep=""),paste(sname,"_height1stPeakValleyAuto_z",sep=""),
    paste(sname,"_zerocrossingrate_x",sep=""),paste(sname,"_zerocrossingrate_y",sep=""),paste(sname,"_zerocrossingrate_z",sep=""),
    
    
    
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
    
    paste(sname,"_prominentAutoPeakValley_avg",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_avg",sep=""),
    paste(sname,"_zerocrossingrate_avg",sep=""),
    
    
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
    
    paste(sname,"_prominentAutoPeakValley_avg(PC)",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg(PC)",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_avg(PC)",sep=""),
    paste(sname,"_zerocrossingrate(PC)",sep=""),
    
    
    
    ###
    paste(sname,"_RMS_avg(b_a)",sep=""),
    paste(sname,"_cor_avg(b_a)",sep=""),
    paste(sname,"_peakfreq_avg(b_a)",sep=""),
    paste(sname,"_entropy_avg(b_a)",sep=""),
    paste(sname,"_energy_avg(b_a)",sep=""),
    
    paste(sname,"_powerBand0_2.5avg(b_a)",sep=""),
    paste(sname,"_powerBand2.5_5avg(b_a)",sep=""),
    paste(sname,"_powerBand5_7.5avg(b_a)",sep=""),
    paste(sname,"_powerBand7.5_10avg(b_a)",sep=""),
    paste(sname,"_powerBand10_12.5avg(b_a)",sep=""),
    paste(sname,"_powerBand12.5_15avg(b_a)",sep=""),
    paste(sname,"_powerBand15_17.5avg(b_a)",sep=""),
    paste(sname,"_powerBand17.5_20avg(b_a)",sep=""),
    paste(sname,"_powerBand20_22.5avg(b_a)",sep=""),
    paste(sname,"_powerBand22.5_25avg(b_a)",sep=""),
    
    paste(sname,"_maxAuto_avg(b_a)",sep=""),    
    paste(sname,"_prominentAutoPeakValley_avg(b_a)",sep=""),
    paste(sname,"_weakpeakAutoPeakValley_avg(b_a)",sep=""),   
    paste(sname,"_height1stPeakValleyAuto_avg(b_a)",sep=""),
    paste(sname,"_zerocrossingrate(b_a)",sep="")
    
    
    
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
      
      first_1s <- getWindow(data.mag, window_idx, window_size/3)
      last_1s <- getWindow(data.mag, window_idx+window_size/3*2,window_size/3)
      
      first_1s <- sqrt( (first_1s$x+50)^2+(first_1s$y+50)^2+(first_1s$z+50)^2 )
      first_1s <- first_1s - mean(first_1s)
      
      last_1s <- sqrt( (last_1s$x+50)^2+(last_1s$y+50)^2+(last_1s$z+50)^2 )
      last_1s <- last_1s - mean(last_1s)
      
      if(var(first_1s) > thresholdvar & var(last_1s) > thresholdvar)
      {
      
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
               getFeatureBy(window_df,"variance"),
               getFeatureBy(window_df,"peakfreq"),
               getFeatureBy(window_df,"RMS"),
               getFeatureBy(window_df,"SD"),
               
               getFeatureBy(window_df,"integratedRMS"),
               getFeatureBy(window_df,"peaknumAuto",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"prominentAuto",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"weakpeakAuto",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"maximumAuto",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"height1stAuto",filtering = F, signal_type = "autocorrelation"),             
               getFeatureBy(window_df,"powerband",powerband_from = 0,powerband_to = 2.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 2.5,powerband_to = 5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 5,powerband_to = 7.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 7.5,powerband_to = 10, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 10,powerband_to = 12.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 12.5,powerband_to = 15, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 15,powerband_to = 17.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 17.5,powerband_to = 20, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 20,powerband_to = 22.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",powerband_from = 22.5,powerband_to = 25, signal_type = "autocorrelation"),
               
               getFeatureBy(window_df,"prominentAutoPeakValley",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"zerocrossingrate",filtering = F, signal_type = "autocorrelation"),
               
               
               
               ####           
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
               getFeatureBy(window_df,"RMS",avg=TRUE),
               getFeatureBy(window_df,"SD",avg=TRUE), 
               getFeatureBy(window_df,"integratedRMS",avg=TRUE),
               getFeatureBy(window_df,"peaknumAuto",avg=TRUE,filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"prominentAuto",avg=TRUE,filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"weakpeakAuto",avg=TRUE,filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"maximumAuto",avg=TRUE,filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"height1stAuto",avg=TRUE,filtering = F, signal_type = "autocorrelation"),
               
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 0,powerband_to = 2.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 2.5,powerband_to = 5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 5,powerband_to = 7.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 7.5,powerband_to = 10, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 10,powerband_to = 12.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 12.5,powerband_to = 15, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 15,powerband_to = 17.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 17.5,powerband_to = 20, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 20,powerband_to = 22.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 22.5,powerband_to = 25, signal_type = "autocorrelation"), 
               
               getFeatureBy(window_df,"prominentAutoPeakValley",filtering = F,avg=TRUE, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = F,avg=TRUE, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = F,avg=TRUE, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"zerocrossingrate",filtering = F,avg=TRUE, signal_type = "autocorrelation"),
               
               ####
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
               getFeatureBy(window_df,"RMS",avg=TRUE, type="PC"),
               getFeatureBy(window_df,"SD",avg=TRUE, type="PC"),
               getFeatureBy(window_df,"integratedRMS",avg=TRUE, type="PC"),
               getFeatureBy(window_df,"peaknumAuto",avg=TRUE, type="PC",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"prominentAuto",avg=TRUE, type="PC",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"weakpeakAuto",avg=TRUE, type="PC",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"maximumAuto",avg=TRUE, type="PC",filtering = F, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"height1stAuto",avg=TRUE, type="PC",filtering = F, signal_type = "autocorrelation"),
               
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 0,powerband_to = 2.5, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 2.5,powerband_to = 5, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 5,powerband_to = 7.5, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 7.5,powerband_to = 10, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 10,powerband_to = 12.5, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 12.5,powerband_to = 15, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 15,powerband_to = 17.5, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 17.5,powerband_to = 20, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 20,powerband_to = 22.5, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",avg=TRUE,powerband_from = 22.5,powerband_to = 25, type="PC", signal_type = "autocorrelation"),
               
               getFeatureBy(window_df,"prominentAutoPeakValley",filtering = F,avg=TRUE, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = F,avg=TRUE, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = F,avg=TRUE, type="PC", signal_type = "autocorrelation"),
               getFeatureBy(window_df,"zerocrossingrate",filtering = F,avg=TRUE, type="PC", signal_type = "autocorrelation"),
               
               
               ####
               getFeatureBy(window_df,"RMS",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"correlation",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"peakfreq",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"entropy",filtering = TRUE, b_avg = TRUE),
               getFeatureBy(window_df,"energy",filtering = TRUE, b_avg = TRUE),
               
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 0,powerband_to = 2.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 2.5,powerband_to = 5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 5,powerband_to = 7.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 7.5,powerband_to = 10, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 10,powerband_to = 12.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 12.5,powerband_to = 15, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 15,powerband_to = 17.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 17.5,powerband_to = 20, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 20,powerband_to = 22.5, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"powerband",b_avg=TRUE,powerband_from = 22.5,powerband_to = 25, signal_type = "autocorrelation"),
               
               getFeatureBy(window_df,"maximumAuto",filtering = TRUE, b_avg = TRUE, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"prominentAutoPeakValley",filtering = TRUE, b_avg = TRUE, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = TRUE, b_avg = TRUE, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = TRUE, b_avg = TRUE, signal_type = "autocorrelation"),
               getFeatureBy(window_df,"zerocrossingrate",filtering = TRUE, b_avg = TRUE, signal_type = "autocorrelation")         
               
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

