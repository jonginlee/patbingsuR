# Created by jonginlee on 16. 10. 04.


#extracted_features_running<- feature_extraction(foldername, 8, 150, 50, "running")

#foldername <- "./walking"
#filename <- list.files(foldername)
#filename <- paste(foldername,"/",filename[1],sep="") 
#test <- feature_extraction_from_file(filename, 8, 150, 50, "walking")
#colnames(test[6:length(test)])
#predictions <- predict(model, window_set) 

#t<-detectEventBy(filename, 8, 150, 50, model, "walking")
#detectEventBy <- function(filename, idx, window_size, window_step, model, targetLabel, selected_features = "all", plotting=FALSE , isTraindata = TRUE,thresholdvar=0.1 )


#t<-detectEventByWindow(filename, 8, 150, 50, model)


detectEventByWindow <- function(filename, idx, window_size, window_step, model,
                                selected_features = "all")
{
  data <- read.table(paste(filename ,sep=""), sep="," ,header=TRUE)
  data <- as.data.frame(data)
  data.sub <- subset(data, grepl(list[idx], data$type))
  #View(data[1:(window_size+1),])
  write.csv(data.sub[1:(window_size+1),], paste(filename,"_sub",sep=""))
  #data <- read.csv(paste(filename,"_sub",sep=""), sep="," ,header=TRUE)
  #as.data.frame(data)
  #View(data)
  
  window_set <- feature_extraction_from_file(paste(filename,"_sub",sep="") , idx, window_size, window_step, "anything" )
  #nrow(data)
  #View(data[1:(window_size+1),])
  window_set <-window_set[6:length(window_set)]
  
  if(selected_features == "all"){
    window_set <- window_set
  }else{
    window_set <- window_set[selected_features]
  }
  
  # if(fileWrite==TRUE)
  #    write.csv(window_set, file=paste("./data_csv/",labelname,".csv",sep=""), row.names=T)
  
  #window_set$label <- factor(window_set$label)
  predictions <- predict(model, window_set) # selected
  
  return(predictions[1])
}



detectEventBy <- function(filename, idx, window_size, window_step, model, targetLabel,
                               selected_features = "all", plotting=FALSE, thresholdvar=0.1 )

{
  data <- read.table(paste(filename ,sep=""), sep="," ,header=TRUE)

  
  data.sub <- subset(data, grepl(list[idx], data$type))

  
  df <- data.frame(time_milli = data.sub$time, x=(data.sub$x), y=(data.sub$y), z=(data.sub$z) )
  #View(df)
  df$magnitude <- sqrt( (df$x+50)^2 + (df$y+50)^2 + (df$z+50)^2 )
  df$magnitude <- df$magnitude - mean(df$magnitude)
  
  returnValue <- ggplot(df, aes(x=time_milli)) +
    #geom_point() +
    geom_line(aes(y=x, col="X")) +
    geom_line(aes(y=y, col="Y")) +
    geom_line(aes(y=z, col="Z")) + 
    #    geom_line(aes(y=magnitude, col="mag")) +
    #geom_line(aes(y=st, col="stationary"))
    ggtitle(paste(filename," - (",sensor_name_list[idx],")")) + 
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
  
#  if(isscratch)
#    window_set<-getWindowset(fileinfo, NULL, sensor_indexes, window_size, window_step, selected_indexs, plotting=FALSE, thresholdvar =thresholdvar)
#  else
#    window_set<-getWindowset(NULL, fileinfo, sensor_indexes, window_size, window_step, selected_indexs, plotting=FALSE, thresholdvar =thresholdvar)
  window_set <- feature_extraction_from_file(paste(filename,sep="") , 8, 150, 50,targetLabel)
  window_set <-window_set[6:length(window_set)]
  
  if(selected_features == "all"){
    window_set <- window_set
  }else{
    window_set <- window_set[selected_features]
  }
  
 # if(fileWrite==TRUE)
#    write.csv(window_set, file=paste("./data_csv/",labelname,".csv",sep=""), row.names=T)
  
  #window_set$label <- factor(window_set$label)
  predictions <- predict(model, window_set) # selected
  #View(predictions)
  
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
  
  returnValue_2 <- returnValue
  
  for(i in 1:window_num)
  {        
    window_data_for_mag <- getWindow(data.sub, window_idx, window_size)
    magnitude <- sqrt( (window_data_for_mag$x+50)^2+(window_data_for_mag$y+50)^2+(window_data_for_mag$z+50)^2)
    magnitude <- magnitude - mean(magnitude)
    
    window_data <- getWindow(data.sub,window_idx,window_size)
    #print(paste("nrow-window_data",nrow(window_data)))
    window_df <- data.frame(x=window_data$x, y=window_data$y, 
                            z=window_data$z, saxis=(window_data$x+window_data$y+window_data$z), magnitude=sqrt(window_data$x^2+window_data$y^2+window_data$z^2))
    
    
    if(var(magnitude)>thresholdvar){
      
      if( predictions[pre_idx] != targetLabel ) {
        defined_color <-"red"
        rect <- data.frame(xmin=data.sub$time[window_idx], xmax=data.sub$time[window_idx+nrow(window_data)-2], ymin=-Inf, ymax=Inf)
        returnValue_2 <- returnValue_2 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill=defined_color, inherit.aes = FALSE)       
        
      }
      
      
      if( (predictions[pre_idx]==targetLabel) ){        
        #print(paste("pre_idx",pre_idx))
        if(starting==FALSE){
          epoches$number[ei] = i
          epoches$start_milli[ei] = window_data$time[1]
          epoches$end_milli[ei] = window_data$time[nrow(window_data)]
          starting=TRUE
          returnValue <- returnValue +  geom_vline(xintercept = epoches$start_milli[ei], alpha=1, colour="blue")      

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
        
        if( (predictions[pre_idx]==targetLabel) ){    
          defined_color <- "blue"
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
        ei <- ei + 1         
      }
    }
    
    returnValue <- returnValue + geom_vline(xintercept = data.sub$time[window_idx], colour="black", alpha=0.8)
    returnValue_2 <- returnValue_2 + geom_vline(xintercept = data.sub$time[window_idx], colour="black", alpha=0.8)
    
    window_idx <- window_idx + window_step
    #  print(paste("window_idx", window_idx, data.sub$time[window_idx]))      
  }
  
  
  #View(epoches)
  #print(paste("ei",ei))
  
  print(returnValue +  ggtitle(paste(filename," - (",sensor_name_list[idx],") rightly detected as ", targetLabel)))
  print(returnValue_2 +  ggtitle(paste(filename," - (",sensor_name_list[idx],") false recognition ")))
  
}
