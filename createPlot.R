# Created by jonginlee on 16. 10. 04.

list = c("accelerometer","magnetometer","Gyro","Hum","Temp","Press","orientation","Linearaccel","HeartRate","RotationVector")
sensor_name_list = c("Accelerometer","Magnetometer", "Gyroscope", "Humidity", "Temperature", "Pressure","Orientation(x-azimuth,y-pitch,z-roll)","Linear Accelerometer" ,"Heart Rate", "Rotation Vector")
y_label_list = c("acceleration (m/s^2)", "micro-Tesla (uT) ", "angular velocity (rad/s)", "relative humidity (RH)","celsius degrees (°C)", "hectopascal (hPa)", "rotation round", "acceleration (m/s^2)", "beats per minute (bpm)", "Rotation Vector Value")

createPlot <- function(data, idx, graph_title, saveFile, source_date, 
                         set_btw=FALSE, start_hour=1.1, end_hour=1.1, type=1, spanValueraw = 0.5, window_step=64 ,windowing=FALSE, cutoff=FALSE, f_l=0.3, butter_type="high", showing_legend = TRUE) {
  data.sub <- data
  if(set_btw){
    data.sub <- subset(data.sub, subset=(data.sub$time < end_hour ))
    data.sub <- subset(data.sub, subset=(data.sub$time > start_hour ))
  }
  
  if(saveFile){  
    # TODO
    #    png(filename=gsub(" ","",paste(graph_title,"_",sensor_name_list[idx],"_",source_date,".png")),
    #        height=450,width=650)
    #    returnValue
    #    Sys.sleep(1)
    #    dev.off()
    write.csv(data.sub, file=paste("./data_raw/",graph_title,".txt",sep=""), row.names=T)
  }
  
  if(set_btw)
    graph_title <- paste(graph_title, "(", start_hour, " ~ ", end_hour, ")")
  else
    graph_title <- paste(graph_title)
  
  
  data.sub <- subset(data.sub,grepl(list[idx], data.sub$type))
  

  max_value <- (as.integer(max(data.sub$time)))
  spliting <- seq(0,max_value,max_value/10)
  xlablename <- "time (millisecond)"
  
  df <- data.frame(time =data.sub$time, x=data.sub$x, y=data.sub$y, z=data.sub$z)
  df$mag <- sqrt((data.sub$x+50)^2 + (data.sub$y+50)^2 + (data.sub$z+50)^2)
  df$mag <- df$mag - mean(df$mag)
  threshold<-0
  if(idx==9 || idx ==6)
  {
    if(idx==6)
      threshold<-1000
    if(idx==9)
      threshold<-60
    
    returnValue <- ggplot(df, aes(x=time,colour="axis")) +
      geom_line(aes(y=x, colour="X")) +
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")","(range - ",start_hour," ~ ",end_hour,")",sep="")) + 
      scale_color_manual(values=c("red")) +
      xlab(xlablename) +
      ylab(y_label_list[idx]) +
      scale_x_continuous(breaks = spliting, expand = c(0, 0)) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
  }
  else{

    
    
    returnValue <- ggplot(df, aes(x=time,colour="axis")) +
      geom_line(aes(y=x, colour="X")) +
      geom_line(aes(y=y, colour="Y")) +
      geom_line(aes(y=z, colour="Z")) +
      geom_line(aes(y=mag, colour="_Magnitude")) + 
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")",sep="")) + 
      scale_color_manual(values=c("red","blue","black","violet")) +
      xlab(xlablename) +
      ylab(y_label_list[idx]) +
      scale_x_continuous(breaks = spliting,expand = c(0, 0)) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
    if(cutoff){
      bf2 <- butter(1, (2*f_l)/(50), type=butter_type)
      df$x <- filtfilt(bf2,df$x)
      
      bf2 <- butter(1, (2*f_l)/(50), type=butter_type)
      df$y <- filtfilt(bf2,df$y)
      bf2 <- butter(1, (2*f_l)/(50), type=butter_type)
      df$z <- filtfilt(bf2,df$z)
      
      returnValue3 <- ggplot(df, aes(x=time,colour="axis")) +
        geom_line(aes(y=x, colour="X")) +
        geom_line(aes(y=y, colour="Y")) +
        geom_line(aes(y=z, colour="Z")) +
        ggtitle(paste(graph_title," (",sensor_name_list[idx],")","(range - ",start_hour," ~ ",end_hour,")",sep="")) + 
        scale_color_manual(values=c("red","blue","black","violet")) +
        xlab(xlablename) +
        ylab(y_label_list[idx]) +
        scale_x_continuous(breaks = spliting) +
        theme_bw() +
        theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
              axis.text.x = element_text(angle=40,hjust=1,vjust=1))
      
      print(returnValue3)
      
      bf2 <- butter(1, (2*f_l)/(50), type=butter_type)
      df$mag <- filtfilt(bf2,df$mag)
      
      returnValue2 <- ggplot(df, aes(x=time,colour="axis")) +
        geom_line(aes(y=mag, colour="_Magnitude")) + 
        ggtitle(paste(graph_title," (",sensor_name_list[idx],")","(range - ",start_hour," ~ ",end_hour,")",sep="")) + 
        scale_color_manual(values=c("red","blue","black","violet")) +
        xlab(xlablename) +
        ylab(y_label_list[idx]) +
        scale_x_continuous(breaks = spliting) +
        theme_bw() +
        theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
              axis.text.x = element_text(angle=40,hjust=1,vjust=1))
      print(returnValue2)
      
    }


  }

  returnValue <- returnValue + geom_hline(aes(yintercept=threshold), colour="yellow")
  
  #print(returnValue)
  
 # line.rcharts <- hPlot(x="cyl", y="mpg_mean", group="am", data=mtcars.mean, type="line")
  # Use this with 'Knit HTML' button
  # line.rcharts$print(include_assets=TRUE)
  # Use this with jekyll blog
#  line.rcharts$show('iframesrc', cdn=TRUE)
  
  res <- returnValue
  
  
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
  

  if(windowing){
    max_value <- (max(data.sub$time))
    min_value <- (min(data.sub$time))
    range <- max_value - min_value
    tindex<-1
    for(i in 0:((range/1000)-1) )
    {
      time_i <- min_value + i*1000
      returnValue <- returnValue + geom_vline(xintercept = time_i, colour="black", alpha=0.8)
    }
    
    print(returnValue)
  }

  if(showing_legend==FALSE)
    returnValue <- returnValue + theme(legend.position="none")


  return (ggplotly(returnValue))
}
