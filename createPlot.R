
list = c("Accel","Magnet","Gyro","Hum","Temp","Press","orientation","Linearaccel")
sensor_name_list = c("Accelerometer","Magnetometer", "Gyroscope", "Humidity", "Temperature", "Pressure","Orientation(x-azimuth,y-pitch,z-roll)","Linear Accelerometer")
y_label_list = c("acceleration (m/s^2)", "micro-Tesla (uT) ", "angular velocity (rad/s)", "relative humidity (RH)","celsius degrees (°C)"," hectopascal (hPa)","rotation round", "acceleration (m/s^2)")


createPlot <- function(data, idx, graph_title, saveFile, source_date, 
                         set_btw=FALSE, start_hour=1.1, end_hour=1.1, type=1, spanValueraw = 0.5, window_step=64 ) {
  if(set_btw){
    data.sub <- subset(data.sub, subset=(data.sub$time > start_hour ))
    data.sub <- subset(data.sub, subset=(data.sub$time < end_hour ))
    graph_title <- paste(graph_title, "(", start_hour, " ~ ", end_hour, ")")
  }
  
  if(saveFile){  
    # TODO
    #    png(filename=gsub(" ","",paste(graph_title,"_",sensor_name_list[idx],"_",source_date,".png")),
    #        height=450,width=650)
    #    returnValue
    #    Sys.sleep(1)
    #    dev.off()
    write.csv(data.sub, file=paste("./data_csv/",graph_title,".csv",sep=""), row.names=T)
  }
  
  
  data.sub <- subset(data,grepl(list[idx], data$type))
  

  max_value <- (as.integer(max(data.sub$time)))
  spliting <- seq(0,max_value,max_value/10)
  xlablename <- "time (millisecond)"
  
  df <- data.frame(time =data.sub$time, x=data.sub$x, y=data.sub$y, z=data.sub$z)
  df$mag <- sqrt((data.sub$x+50)^2 + (data.sub$y+50)^2 + (data.sub$z+50)^2)
  df$mag <- df$mag - mean(df$mag)
  
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
  
  returnValue <- returnValue + geom_hline(aes(yintercept=threshold), colour="yellow")
  
  print(returnValue)
  
  if(type==2){
    
    df$x = predict(smooth.spline(df$time, df$x, spar = spanValueraw), df$time)$y
    df$y = predict(smooth.spline(df$time, df$y, spar = spanValueraw), df$time)$y
    df$z = predict(smooth.spline(df$time, df$z, spar = spanValueraw), df$time)$y  
    
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
    
    #print(returnValue)
    
    p <- findValleys(df$z,0.0005)
    print(findpeaks(df$z, sortstr = TRUE,threshold=0.5)[,1])
    p <- p -1
    if(length(p)!=0)
      returnValue <- returnValue + geom_vline(xintercept = df$time[p], alpha=1, colour="red",linetype=4)
    print(returnValue)
    
    window_num <- as.integer(nrow(data.sub)/window_step)
    window_idx <- 1
    for(i in 1:window_num){
      returnValue <- returnValue + geom_vline(xintercept = data.sub$time[window_idx], colour="black", alpha=0.8)
      window_idx <- window_idx + window_step  
    }
    
    print(returnValue)
    
  }
  
  if(type==3)
  {
    df <- data.frame(time =data.sub$time, x=data.sub$x, y=data.sub$y, z=data.sub$z)
    
    df$dx[1]<-df$x[1]
    df$dy[1]<-df$y[1]
    df$dz[1]<-df$z[1]
    for(i in 2:length(df$time))
    {
      df$dx[i] <- df$x[i]-df$x[i-1]
      df$dy[i] <- df$y[i]-df$y[i-1]
      df$dz[i] <- df$z[i]-df$z[i-1]      
    }
    df$mag <- sqrt((df$dx+50)^2 + (df$dy+50)^2 + (df$dz+50)^2)
    df$mag <- df$mag - mean(df$mag)
    
    returnValue <- ggplot(df, aes(x=time,colour="axis")) +
      geom_line(aes(y=dx, colour="X")) +
      geom_line(aes(y=dy+10, colour="Y")) +
      geom_line(aes(y=dz+20, colour="Z")) +
      geom_line(aes(y=mag+30, colour="_Magnitude")) + 
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")","(range - ",start_hour," ~ ",end_hour,")",sep="")) + 
      scale_color_manual(values=c("red","blue","black","violet")) +
      xlab(xlablename) +
      ylab(y_label_list[idx]) +
      scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
    print(returnValue)
    
    df$idx[1]<-0
    df$idy[1]<-0
    df$idz[1]<-0
    
    for(i in 2:length(df$time))
    {
      sumx<-0
      sumy<-0
      sumz<-0
      for(k in 1:i){
        sumx <- sumx + df$dx[k]
        sumy <- sumy + df$dy[k]
        sumz <- sumz + df$dz[k]
      }
      
      df$idx[i] <- sumx
      df$idy[i] <- sumy
      df$idz[i] <- sumz
    }
    
    returnValue <- ggplot(df, aes(x=time,colour="axis")) +
      geom_line(aes(y=idx, colour="X")) +
      geom_line(aes(y=idy, colour="Y")) +
      geom_line(aes(y=idz, colour="Z")) +
      #geom_line(aes(y=mag, colour="_Magnitude")) + 
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")","(range - ",start_hour," ~ ",end_hour,")",sep="")) + 
      scale_color_manual(values=c("red","blue","black","violet")) +
      xlab(xlablename) +
      ylab(y_label_list[idx]) +
      scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
    print(returnValue)
    
  }
  

  
#  max_value <- (max(data.sub$time))
#  min_value <- (min(data.sub$time))
#  range <- max_value - min_value
#  tindex<-1
#  for(i in 0:((range/1000)-1) )
#  {
#    time_i <- min_value + i*1000
#    returnValue <- returnValue + geom_vline(xintercept = time_i, colour="black", alpha=0.8)
#   }
  
  #print(returnValue)
  
  
  
}
