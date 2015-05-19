
detectScratchMovs <- function(data, idx, graph_title,  window_size, window_step, model,sparValue=0.3, fileWrite=FALSE, set_btw=FALSE, start_milli=0.1, end_milli=0.1,delay=1)
{
  data.sub <- subset(data, grepl(list[idx], data$type))
  data.sub$hour <- data.sub$time/(1000*60*60)
  print(paste("file",graph_title))
  ##
  if(set_btw){
    data.sub <- subset(data.sub, subset=(data.sub$time > start_milli ))
    data.sub <- subset(data.sub, subset=(data.sub$time < end_milli ))  
  }
  
  
  data.sub <- subset(data.sub, subset=(data.sub$time > 1000*3 ))
  e_idx <- nrow(data.sub)
  e_time <- data.sub$time[e_idx]
  data.sub <- subset(data.sub, subset=(data.sub$time < (e_time - (1000*3)) ))
  
  ##
  # data.sub <- subset(data.sub, subset=(data.sub$time > 2000 ))
  
  data.sub <- getDelayedData(data.sub, delay)
  
  #  print(paste("len",nrow(data.sub)))
  df <- data.frame(time_milli = data.sub$time,
                   x=(data.sub$x), y=(data.sub$y), z=(data.sub$z) )
  #df$st <- df$magnitude < 0.01
  df$magnitude=sqrt( (df$x+50)^2 + (df$y+50)^2 + (df$z+50)^2 )
  
  bf <- butter(2, sparValue, type="low")
  df$x_s <- filtfilt(bf,df$x)
  df$y_s <- filtfilt(bf,df$y)
  df$z_s <- filtfilt(bf,df$z)
  df$magnitude <- df$magnitude - mean(df$magnitude)
  
  #  df$x_s = predict(smooth.spline(df$time_milli, df$x, spar = sparValue), df$time_milli)$y
  #  df$y_s = predict(smooth.spline(df$time_milli, df$y, spar = sparValue), df$time_milli)$y
  #  df$z_s = predict(smooth.spline(df$time_milli, df$z, spar = sparValue), df$time_milli)$y
  
  returnValue3 <- ggplot(df, aes(x=time_milli)) +
    #geom_point() +
    #geom_line(aes(y=x_s, col="X")) +
    #geom_line(aes(y=y_s, col="Y")) +
    #geom_line(aes(y=z_s, col="Z")) + 
    geom_line(aes(y=magnitude, col="mag")) +
    #geom_line(aes(y=st, col="stationary"))
    ggtitle(paste(graph_title," - (",sensor_name_list[idx],")")) + 
    #  coord_fixed(ratio=1/4) +
    xlab(paste("Time(milli)", ", window_size(", window_size,"), window_step(", window_step,")",sep="")) +
    ylab(y_label_list[idx])+
    scale_x_continuous(breaks = seq(0,(as.integer(max(data.sub$time))),5000)) +
    #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
    #    scale_x_continuous(breaks = spliting) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  
  #print(returnValue3)
  
  returnValue <- ggplot(df, aes(x=time_milli)) +
    #geom_point() +
    geom_line(aes(y=x, col="X")) +
    geom_line(aes(y=y, col="Y")) +
    geom_line(aes(y=z, col="Z")) + 
    #    geom_line(aes(y=magnitude, col="mag")) +
    #geom_line(aes(y=st, col="stationary"))
    ggtitle(paste(graph_title," - (",sensor_name_list[idx],")")) + 
    #  coord_fixed(ratio=1/4) +
    xlab(paste("Time(milli)", ", window_size(", window_size,"), window_step(", window_step,")",sep="")) +
    ylab(y_label_list[idx])+
    scale_x_continuous(breaks = seq(0,(as.integer(max(data.sub$time))),5000)) +
    #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
    #    scale_x_continuous(breaks = spliting) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  
  df$smooth.spline = predict(smooth.spline(df$time_milli, df$magnitude, spar = sparValue), df$time_milli)$y
  returnValue2 <- ggplot(df, aes(x=time_milli)) +
    #geom_point() +
    #    geom_line(aes(y=x, col="X")) +
    #    geom_line(aes(y=y, col="Y")) +
    #    geom_line(aes(y=z, col="Z")) + 
    geom_line(aes(y=magnitude, col="magnitude")) +
    geom_line(aes(y=smooth.spline, col="magnitude_smooth")) +
    #geom_line(aes(y=st, col="stationary"))
    ggtitle(paste(graph_title," - (",sensor_name_list[idx],")")) + 
    #  coord_fixed(ratio=1/4) +
    xlab(paste("Time(milli)", ", window_size(", window_size,"), window_step(", window_step,")",sep="")) +
    ylab(y_label_list[idx])+
    scale_x_continuous(breaks = seq(0,(as.integer(max(data.sub$time))),5000)) +
    #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
    #    scale_x_continuous(breaks = spliting) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  
  #  print(paste("window_num : ",window_num, " nrow(data.sub) : ", nrow(data.sub), " window_step ", window_step))
  window_num <- as.integer(nrow(data.sub)/window_step)
  window_idx <- 1
  # print(paste("window_num",window_num))
  # print(returnValue)
  
  #  window_set <- vector(mode="list", length=16)
  window_set <- data.frame(number = numeric(0), start_milli = numeric(0), end_milli = numeric(0), label = numeric(0),
                           mean_x = numeric(0), mean_y = numeric(0), mean_z = numeric(0),
                           entropy_x = numeric(0), entropy_y = numeric(0), entropy_z = numeric(0),
                           #energy_x = numeric(0), energy_y = numeric(0), energy_z = numeric(0),
                           cor_x = numeric(0), cor_y = numeric(0), cor_z = numeric(0),
                           autocor_x = numeric(0), autocor_y = numeric(0), autocor_z = numeric(0),
                           #th_avg = numeric(0), autocor_avg = numeric(0), 
                           #             var_avg = numeric(0), 
                           #                 peakfreq_avg = numeric(0),
                           th_x = numeric(0), th_y = numeric(0), th_z = numeric(0),
                           autocor_x2 = numeric(0), autocor_y2 = numeric(0), autocor_z2 = numeric(0),
                           #                           var_x = numeric(0), var_y = numeric(0), var_z = numeric(0),
                           peakfreq_x = numeric(0), peakfreq_y = numeric(0), peakfreq_z = numeric(0)
                           #                           )
                           #                           peakratio_x = numeric(0), peakratio_y = numeric(0), peakratio_z = numeric(0)
  )
  
  #  names(window_set) <- c("label",
  #                         "mean_x","mean_y","mean_z",
  #                         "entropy_x","entropy_y","entropy_z",
  #                         "energy_x","energy_y","energy_z",
  #                         "cor_x","cor_y","cor_z",
  #                         "autocor_x","autocor_y","autocor_z")
  
  
  for(i in 1:window_num)
  {    
    window_data <- getWindow(data.sub,window_idx,window_size)
    window_df <- data.frame(time_hour=window_data$hour, time_milli =window_data$time, x=window_data$x, y=window_data$y, 
                            z=window_data$z, saxis=(window_data$x+window_data$y+window_data$z), magnitude=sqrt(window_data$x^2+window_data$y^2+window_data$z^2))
    start_milli <-paste( getHMS(window_data$hour[1]), window_data$time[1] )
    end_milli <- paste( getHMS(window_data$hour[nrow(window_data)]), window_data$time[nrow(window_data)] )
    
    if(var(window_df$saxis)>0.05){
      label<-"TODO"
      p <- c(i, start_milli, end_milli, label,
             getFeatureBy(window_df,"mean"),
             getFeatureBy(window_df,"entropy"),   # variance 사실상 비슷
             #       getFeatureBy(window_df,"energy"),
             getFeatureBy(window_df,"correlation"),
             getFeatureBy(window_df,"autocorrelation",12),
             ##
             #            getFeatureBy(window_df,"threshold", avg = TRUE),
             #             getFeatureBy(window_df,"autocorrelation", 1,avg = TRUE),
             #       getFeatureBy(window_df,"variance",avg = TRUE),
             #getFeatureBy(window_df,"peakratio"),
             #             getFeatureBy(window_df,"peakfreq", avg = TRUE),
             getFeatureBy(window_df,"threshold", avg = FALSE),
             getFeatureBy(window_df,"autocorrelation", 1,avg = FALSE),
             #              getFeatureBy(window_df,"variance",avg = TRUE),
             #getFeatureBy(window_df,"peakratio"),
             getFeatureBy(window_df,"peakfreq", avg = FALSE)
      )
      
      
      #      getFeatureBy(window_df,"mean")  ,
      #       getFeatureBy(window_df,"entropy") ,
      #       getFeatureBy(window_df,"energy") ,
      #       getFeatureBy(window_df,"correlation") ,
      #       getFeatureBy(window_df,"autocorrelation"))
      
      #       getFeatureBy(window_df,"peakratio") )
      window_set[nrow(window_set)+1,] <- p
    }else{
      label<-"sleep"
    }
    
    
    window_idx <- window_idx + window_step
    #  print(paste("window_idx", window_idx, data.sub$time[window_idx]))      
  }
  
  #View(window_set)
  #                         "mean_x","mean_y","mean_z",
  #                         "entropy_x","entropy_y","entropy_z",
  #                         "energy_x","energy_y","energy_z",
  #                         "cor_x","cor_y","cor_z",
  #                         "autocor_x","autocor_y","autocor_z"
  window_set$mean_x <- as.numeric(window_set$mean_x)
  window_set$mean_y <- as.numeric(window_set$mean_y)
  window_set$mean_z <- as.numeric(window_set$mean_z)
  window_set$entropy_x <- as.numeric(window_set$entropy_x)
  window_set$entropy_y <- as.numeric(window_set$entropy_y)
  window_set$entropy_z <- as.numeric(window_set$entropy_z)
  #  window_set$energy_x <- as.numeric(window_set$energy_x)
  #  window_set$energy_y <- as.numeric(window_set$energy_y)
  #  window_set$energy_z <- as.numeric(window_set$energy_z)
  window_set$cor_x <- as.numeric(window_set$cor_x)
  window_set$cor_y <- as.numeric(window_set$cor_y)
  window_set$cor_z <- as.numeric(window_set$cor_z)
  window_set$autocor_x <- as.numeric(window_set$autocor_x)
  window_set$autocor_y <- as.numeric(window_set$autocor_y)
  window_set$autocor_z <- as.numeric(window_set$autocor_z)
  window_set$th_x <- as.numeric(window_set$th_x)
  window_set$th_y <- as.numeric(window_set$th_y)
  window_set$th_z <- as.numeric(window_set$th_z)
  window_set$autocor_x2 <- as.numeric(window_set$autocor_x2)
  window_set$autocor_y2 <- as.numeric(window_set$autocor_y2)
  window_set$autocor_z2 <- as.numeric(window_set$autocor_z2)
  #  window_set$var_x <- as.numeric(window_set$var_x)
  #  window_set$var_y <- as.numeric(window_set$var_y)
  #  window_set$var_z <- as.numeric(window_set$var_z)
  window_set$peakfreq_x <- as.numeric(window_set$peakfreq_x)
  window_set$peakfreq_y <- as.numeric(window_set$peakfreq_y)
  window_set$peakfreq_z <- as.numeric(window_set$peakfreq_z)
  
  
  #  window_set$th_avg <-as.numeric(window_set$th_avg)
  #  window_set$autocor_avg <- as.numeric(window_set$autocor_avg)
  #    window_set$var_avg <- as.numeric(window_set$var_avg)
  #  window_set$peakfreq_avg <- as.numeric(window_set$peakfreq_avg)
  
  
  #  th_avg = numeric(0), autocor_avg = numeric(0), var_avg = numeric(0), peakfreq_avg = numeric(0)
  #  window_set$peakratio_x <- as.numeric(window_set$peakratio_x)
  #  window_set$peakratio_y <- as.numeric(window_set$peakratio_y)
  #  window_set$peakratio_z <- as.numeric(window_set$peakratio_z)
  
  
  if(fileWrite==TRUE){
    write.csv(window_set, file=paste("./data_csv/",graph_title,".csv",sep=""), row.names=T)
  }
  
  #print(paste("window_mean_x", typeof(window_set$mean_x) ))
  #View(window_set)
  
  
  #View(window_set[,5:(19+4)])
  #  predictions <- predict(model, window_set[,5:(19)])
  #  predictions <- predict(model, window_set[,5:(19+12)])
  #  predictions <- predict(model, window_set[,c(4,20:31)])  # extended previous work
  #  predictions2 <- predict(model, window_set[,c(4,20:31)],type="prob")  # extended previous work
  
  #  predictions <- predict(model, window_set[,c(4,20:31-8)]) # previous work 
  window_set$label <- factor(window_set$label)
  
  predictions <- predict(model, window_set[,5:(19+6)]) # selected
  predictions2 <- predict(model, window_set[,5:(19+6)],type="prob")  # extended previous work
  
  
  View(predictions)
  t<-autolabeling(paste(graph_title,".csv",sep=""), "scratch")
  filter <- "scratch|non|scratch_finger"
  t <- subset(t, grepl(filter, t$label))
  View(t)
  
  sumres<-1
  if(length(t$label) == length(predictions))
  {
    for(i in 1:length(t$label))
    {
      if( (t$label[i]=="scratch") & ((predictions[i]=="scratch") | (predictions[i]=="scratch_finger")) )
        sumres[i] <- "tp"
      else if( (t$label[i]=="non") & (predictions[i]=="non") )
        sumres[i] <- "tn"
      else if( (t$label[i]=="non") & ((predictions[i]=="scratch") | (predictions[i]=="scratch_finger")) )
        sumres[i] <- "fp"
      else if( (t$label[i]=="scratch") & (predictions[i]=="non") )
        sumres[i] <- "fn"
    }
  }
  
  sumres <- as.factor(sumres)
  print(summary(sumres))
  #View(predictions2)
  
  #  predictions3 <- apply(predictions2, 1, which.max)
  #print(levels(window_set$label))
  #View(predictions3)
  
  #window_set$label <- factor(window_set$label)
  #predictions <- factor(predictions)
  #print(confusionMatrix(predictions, window_set$label))
  
  window_idx <- 1
  pre_idx <- 1
  ei <- 1  
  
  epoches <- vector(mode="list", length=(3))
  names(epoches) <- c("number", "start_milli","end_milli")
  
  starting = FALSE
  continue = FALSE
  epoches$number[ei] <- 1
  epoches$start_milli[ei] <- 1 
  epoches$end_milli[ei] <- 1
  
  
  for(i in 1:window_num)
  {        
    window_data <- getWindow(data.sub,window_idx,window_size)
    #print(paste("nrow-window_data",nrow(window_data)))
    window_df <- data.frame(x=window_data$x, y=window_data$y, 
                            z=window_data$z, saxis=(window_data$x+window_data$y+window_data$z), magnitude=sqrt(window_data$x^2+window_data$y^2+window_data$z^2))
    
    
    if(var(window_df$saxis)>0.05){
      if( (predictions[pre_idx]=="scratch") | predictions[pre_idx]=="scratch_finger" ){        
        #print(paste("pre_idx",pre_idx))
        if(starting==FALSE){
          epoches$number[ei] = i
          epoches$start_milli[ei] = window_data$time[1]
          epoches$end_milli[ei] = window_data$time[nrow(window_data)]
          starting=TRUE
          returnValue <- returnValue +  geom_vline(xintercept = epoches$start_milli[ei], alpha=1, colour="blue")      
          returnValue2 <- returnValue2 +  geom_vline(xintercept = epoches$start_milli[ei], alpha=1, colour="blue")      
          
        }else if( window_data$time[1] <= epoches$end_milli[ei] ){
          continue=TRUE
          epoches$end_milli[ei] <- window_data$time[nrow(window_data)]
        }
        
        #        if(predictions2$scratch[pre_idx] == 1)
        #          defined_color <- "red"
        #        else if(predictions2$scratch[pre_idx] >= 0.8)
        #          defined_color <- "blue"
        #        else if(predictions2$scratch[pre_idx] >= 0.6)
        #          defined_color <- "yellow"
        #        else
        #          defined_color <- "gray"
        
        if(predictions[pre_idx]=="scratch_finger")
          defined_color <- "yellow"
        else
          defined_color <- "red"
        
        if(t$label[pre_idx]!="scratch")
          defined_color <- "black"
        
        if( (predictions[pre_idx]=="scratch") | predictions[pre_idx]=="scratch_finger" ){    
          rect <- data.frame(xmin=data.sub$time[window_idx], xmax=data.sub$time[window_idx+nrow(window_data)-2], ymin=-Inf, ymax=Inf)
          returnValue <- returnValue + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill=defined_color, inherit.aes = FALSE)        
        }
        
      }else if( starting==TRUE ) {
        if(window_data$time[1] >= epoches$end_milli[ei])
        {
          ## ending part
          epoch = subset(df, (time_milli>=epoches$start_milli[ei]) & (time_milli<=epoches$end_milli[ei]))
          #print(paste("epoch_len",nrow(epoch),nrow(df)))
          #createIntegralPlot(epoch,"mag", paste("mag","(",start_milli,end_milli,")"), "accel" ,sparValue,sparValue,sparValue)
          
          starting=FALSE
          returnValue <- returnValue +  geom_vline(xintercept = epoches$end_milli[ei], alpha=1, colour="red",linetype=4)
          returnValue2 <- returnValue2 +  geom_vline(xintercept = epoches$end_milli[ei], alpha=1, colour="red",linetype=4)          
          ei <- ei + 1         
        }
      }
      
      pre_idx <- pre_idx +1
    }else if( starting==TRUE ) {
      if(window_data$time[1] >= epoches$end_milli[ei])
      {
        ## ending part
        epoch = subset(df, (time_milli>=epoches$start_milli[ei]) & (time_milli<=epoches$end_milli[ei]))
        #print(paste("epoch_len",nrow(epoch),nrow(df)))
        #createIntegralPlot(epoch,"mag", paste("mag","(",start_milli,end_milli,")"), "accel" ,sparValue,sparValue,sparValue)        
        
        starting=FALSE
        returnValue <- returnValue +  geom_vline(xintercept = epoches$end_milli[ei], alpha=1, colour="red",linetype=4)
        returnValue2 <- returnValue2 +  geom_vline(xintercept = epoches$end_milli[ei], alpha=1, colour="red",linetype=4)
        ei <- ei + 1         
      }
    }
    
    returnValue <- returnValue + geom_vline(xintercept = data.sub$time[window_idx], colour="black", alpha=0.8)
    window_idx <- window_idx + window_step
    #  print(paste("window_idx", window_idx, data.sub$time[window_idx]))      
  }
  
  
  
  #  View(epoches)
  print(paste("ei",ei))
  
  print(returnValue)
  #  print(returnValue2)
  
  
}