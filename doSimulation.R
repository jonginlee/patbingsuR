
doSimulation <- function(data, cut, idx, window_size, window_step, save_filename, plotting = FALSE, type = 1, delay=1)
{
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
    data.sub <- subset(data.sub, subset=(data.sub$time > 1000*2 ))
    e_idx <- nrow(data.sub)
    e_time <- data.sub$time[e_idx]
    data.sub <- subset(data.sub, subset=(data.sub$time < (e_time - (1000*2)) ))
  }
  
  # Window setting
  #print(paste("window_num : ",window_num, " nrow(data.sub) : ", nrow(data.sub), " window_step ", window_step))
  
  window_num <- as.integer(nrow(data.sub)/window_step)
  window_idx <- 1
  
  if(plotting == TRUE)
  {
    
    max_value <- (as.integer(max(data.sub$time)))
    spliting <- seq(0,max_value,max_value/10)
    xlablename <- "time (millisecond)"
    
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
  
  #  window_set <- vector(mode="list", length=(31 - 12) ) # extended previous 2
  #  window_set <- vector(mode="list", length=(31) ) # extended previous & CHI
  #  window_set <- vector(mode="list", length=(31+4) ) # all
  #  window_set <- vector(mode="list", length=(19+6) ) # selected
  window_set <- vector(mode="list", length=(19+6) ) # selected + 1
  window_idx <- 1
  
  #window_set <- vector(mode="list", length=(31 - 8) ) # previous work
  names(window_set) <- c("epoches","start_milli","end_milli","label",
                         "mean_x","mean_y","mean_z",
                         "entropy_x","entropy_y","entropy_z",
                         #          "energy_x","energy_y","energy_z",
                         "cor_x","cor_y","cor_z",
                         "autocor_x","autocor_y","autocor_z",
                         #          "th_avg",
                         #           "autocor_avg",
                         #                "var_avg",
                         #           "peakfreq_avg",
                         "th_x","th_y","th_z",
                         "autocor_x2","autocor_y2","autocor_z2",
                         
                         #         "var_x","var_y","var_z",
                         #       "peakratio_x","peakratio_y","peakratio_z", 
                         "peakfreq_x","peakfreq_y","peakfreq_z")
  
  
  candidates_idx <- 1
  # p1  3.58333+0.013888
  # p2  2.267 + 0.0219448 -0.0061112 - 0.02138888 + 0.02194444 -0.0011111 +0.0000277777 left
  # p2  2.267 + 0.0219448 -0.0061112 - 0.02138888 right\
  # recent 2.267 + 0.0219448 -0.0061112 - 0.02138888 +2.6 + 0.004166
  # recent 2.267 + 0.0219448 -0.0061112-0.02138888 +2.6 + 0.004166
  print(paste("window_num : ",window_num))
  
  for(i in 1:window_num)
  {
    window_data <- getWindow(data.sub,window_idx,window_size)
    window_df <- data.frame(time_hour=window_data$hour, time_milli =window_data$time, x=window_data$x, y=window_data$y, 
                            z=window_data$z, saxis=(window_data$x+window_data$y+window_data$z), magnitude=sqrt(window_data$x^2+window_data$y^2+window_data$z^2))
    start_milli <-paste( getHMS(window_data$hour[1]), window_data$time[1] )
    end_milli <- paste( getHMS(window_data$hour[nrow(window_data)]), window_data$time[nrow(window_data)] )
    
    if(var(window_df$saxis)>0.05){
      epoch<-1
      label<-"TODO"
      rect <- data.frame(xmin=data.sub$time[window_idx], xmax=data.sub$time[window_idx+nrow(window_data)-2], ymin=-Inf, ymax=Inf)
      returnValue <- returnValue + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill="blue", inherit.aes = FALSE)        
      
    }else{
      epoch<-0
      label<-"sleep"
    }
    
    p <- c(epoch,start_milli,end_milli,label,
           getFeatureBy(window_df,"mean"),
           getFeatureBy(window_df,"entropy"),   
           #         getFeatureBy(window_df,"energy"), # variance 사실상 비슷
           getFeatureBy(window_df,"correlation"),
           getFeatureBy(window_df,"autocorrelation",12),
           ##
           #       getFeatureBy(window_df,"threshold",avg = TRUE),
           #       getFeatureBy(window_df,"autocorrelation", 1, avg = TRUE),
           #       getFeatureBy(window_df,"variance", avg = TRUE),
           #       getFeatureBy(window_df,"peakfreq", avg = TRUE),
           
           getFeatureBy(window_df,"threshold",avg = FALSE),
           getFeatureBy(window_df,"autocorrelation", 1, avg = FALSE),
           #       getFeatureBy(window_df,"variance", avg = FALSE),
           getFeatureBy(window_df,"peakfreq", avg = FALSE)
    )
    
    window_set<-rbind(window_set,p)
    
    window_idx <- window_idx + window_step
  }
  window_set <- window_set[-1,]
  #View(window_set)
  
  if(save_filename!=FALSE){
    write.csv(window_set, file=paste("./data_csv/",save_filename,".csv",sep=""), row.names=T)
    print(paste("* window_num",window_num))
    print(paste("* saved file: ", save_filename,".csv", sep=""))
  }

  
  if(plotting)
    print(returnValue)

  
}

