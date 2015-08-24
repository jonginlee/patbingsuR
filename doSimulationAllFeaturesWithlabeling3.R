
doSimulationAllFeaturesWithLabeling4 <- function(data, cut, idx, window_size, window_step, scr_info, save_filename=FALSE, plotting = FALSE, type = 1, delay=1, startMilli=2000, endMilli=2000, thresholdvar = 0.1,labeling=TRUE)
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
  window_num <- window_num - 2
  
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
  window_set <- vector(mode="list", length=(4  + (13) + (13) + (23) )) #   
  sname <- list[idx]
  #window_set <- vector(mode="list", length=(31 - 8) ) # previous work
  names(window_set) <- c(
    "epoches","start_milli","end_milli","label",
  
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
    paste(sname,"_zerocrossingrate(b_a)",sep=""),
    paste(sname,"_auto_entropy_avg(b_a)",sep=""),
    paste(sname,"_auto_energy_avg(b_a)",sep=""),
    paste(sname,"_harmPeak_avg(b_a)",sep="")
    
    
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
  window_data_prev <- NULL
  
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
                                z=window_data$z, time =window_data$time)
        
        start_milli <-paste( getHMS(window_data$hour[1]), window_data$time[1] )
        end_milli <- paste( getHMS(window_data$hour[nrow(window_data)]), window_data$time[nrow(window_data)] )
        
        if(length(window_data_prev)!=0)
        {
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
                 
                 
                 ####
                 getFeatureBy(window_df,"RMS",filtering = F, b_avg = TRUE, filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"correlation",filtering = F, b_avg = TRUE, filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"peakfreq",filtering = F, b_avg = TRUE, filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"entropy",filtering = F, b_avg = TRUE, filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"energy",filtering = F, b_avg = TRUE, filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 0,powerband_to = 2.5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 2.5,powerband_to = 5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 5,powerband_to = 7.5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 7.5,powerband_to = 10, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 10,powerband_to = 12.5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 12.5,powerband_to = 15, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 15,powerband_to = 17.5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 17.5,powerband_to = 20, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 20,powerband_to = 22.5, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"powerband",filtering = F,b_avg=TRUE,powerband_from = 22.5,powerband_to = 25, signal_type = "autocorrelation", filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 
                 getFeatureBy(window_df,"maximumAuto",filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"prominentAutoPeakValley",filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"weakpeakAutoPeakValley",filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"height1stPeakValleyAuto",filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"zerocrossingrate",filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"entropy",filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"energy",filtering = F, b_avg = TRUE, signal_type = "autocorrelation" , filter_num = 3, spanV = 0.5, filtering2 = T,window_data_prev = window_data_prev),
                 getFeatureBy(window_df,"harmPeak",filtering = F, b_avg = TRUE, signal_type = "autocorrelation" ,filtering2 = T, filter_num = 3, spanV = 0.5)
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