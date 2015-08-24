

ltime <- subject1_1$time[length(subject1_1$time)]
stime <- ltime/4

subject1_1_part1 <- subset(subject1_1, subset=(subject1_1$time < stime ))

subject1_1_part2 <- subset(subject1_1, subset=(subject1_1$time > stime ))
subject1_1_part2 <- subset(subject1_1_part2, subset=(subject1_1_part2$time < (2*stime) ))

subject1_1_part3 <- subset(subject1_1, subset=(subject1_1$time > 2*stime ))
subject1_1_part3 <- subset(subject1_1_part3, subset=(subject1_1_part3$time < 3*stime ))

subject1_1_part4 <- subset(subject1_1, subset=(subject1_1$time > 3*stime ))

res<-createPlot(subject1_1_part1, 1, "PART 1" ,saveFile = FALSE)
py$ggplotly(res, session="knitr")


========


t<-doSimulationAllFeatures(subject1_left, FALSE, 8, 150, 50, "subject1_left", FALSE,delay=1,startMilli = 0*1000,endMilli = 0*1000, thresholdvar = 0.1)
t<-doSimulationAllFeatures(subject1_1, FALSE, 8, 150, 50, "subject1_1", FALSE,delay=1,startMilli = 0*1000,endMilli = 0*1000, thresholdvar = 0.1)

subject1_1log <- read.table("./data_csv/subject1_1.csv",sep=",",header=TRUE)
subject1_leftlog <- read.table("./data_csv/subject1_left.csv",sep=",",header=TRUE)


subject1_rightlog <- read.table("./data_raw/subject1_left_1activelog.txt",sep=",",header=TRUE)
subject1_leftlog <- read.table("./data_raw/subject1_activelog1.txt",sep=",",header=TRUE)

resleft<-getHMSFromMillis(( 10*60*60*1000 + 37*60*1000 + 4*1000 +6*1000 - 2*60*1000 - 9*1000+ subject1_rightlog$elapsed)/((1000)*60*60))
#subject1_leftlog$time <- resleft
write.csv(resleft, "./data_csv/subject1_rightlog_o5.csv")

res<-getHMSFromMillis(( 10*60*60*1000 + 37*60*1000  + 5*1000  - 2*60*1000 - 9*1000 + subject1_leftlog$elapsed)/((1000)*60*60))
#subject1_1log$time <- res
write.csv(res, "./data_csv/subject1_leftlog_o5.csv")




doSimulationAllFeatures <- function(data, cut, idx, window_size, window_step, save_filename, plotting = FALSE, type = 1, delay=1, startMilli=2000, endMilli=2000, thresholdvar = 0.1)
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
  #print(paste("window_num : ",window_num, " nrow(data.sub) : ", nrow(data.sub), " window_step ", window_step))
  
  #  window_num <- as.integer(nrow(data.sub)/window_step)
  window_idx <- 1
  #  window_set <- vector(mode="list", length=(31 - 12) ) # extended previous 2
  #  window_set <- vector(mode="list", length=(31) ) # extended previous & CHI
  #  window_set <- vector(mode="list", length=(31+4) ) # all
  #  window_set <- vector(mode="list", length=(19+6) ) # selected
  # window_set <- vector(mode="list", length=(19+6) ) # selected + 1
  # window_set <- vector(mode="list", length=(4 + 3*12 + 11) ) # selected + 1
  window_set <- vector(mode="list", length=(4) ) # selected + 1
  
  sname <- list[idx]
  #window_set <- vector(mode="list", length=(31 - 8) ) # previous work
  names(window_set) <- c(
    "epoches","start_milli","end_milli","label")
  
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
    start_milli <-paste(window_data$time[1] )
    end_milli <- paste(window_data$time[nrow(window_data)] )
    if(var(magnitude)>thresholdvar){
      epoch<-1
      label<-"TODO"
      #rect <- data.frame(xmin=data.sub$time[window_idx], xmax=data.sub$time[window_idx+nrow(window_data)-2], ymin=-Inf, ymax=Inf)
      #returnValue <- returnValue + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill="blue", inherit.aes = FALSE)        
      
    }else{
      epoch<-0
      label<-"sleep"
    }
    
    p <- c(epoch,start_milli,
           getHMSFromMillis(( 10*60*60*1000 + 37*60*1000  + 5*1000+ start_milli)/((1000)*60*60)),
           end_milli,
           getHMSFromMillis(( 10*60*60*1000 + 37*60*1000  + 5*1000+ end_milli)/((1000)*60*60)),
           label
    )
    
    window_set<-rbind(window_set,p)
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

