
# getFirstWindow(data_watch_jongin0524DATASET5, 8, 20, 50, TRUE, "test", 0.05, FALSE)
# getFirstWindow(data_watch_jongin0524DATASET1, 8, 25, 50, TRUE, "test", 0.03, FALSE)

data<-ji_loc_test
data.sub <- subset(data,grepl(list[8], data$type))

window_data <- data.sub
tmp_x<-createIntegralGraph(window_data$time, window_data$x,paste("x-axis"),type = "loc")
tmp_y<-createIntegralGraph(window_data$time, window_data$y,paste("y-axis"),type = "loc")
tmp_z<-createIntegralGraph(window_data$time, window_data$z,paste("z-axis"),type = "loc")      

dis <- sqrt(tmp_x[length(tmp_x)]^2 + tmp_y[length(tmp_y)]^2 + tmp_z[length(tmp_z)]^2)
print(paste("x:",tmp_x[length(tmp_x)],"y:",tmp_y[length(tmp_y)],"z:",tmp_z[length(tmp_z)], "dis",sum(dis) ))

scatterplot3d(tmp_x,tmp_y,tmp_z, pch=16, highlight.3d=TRUE,type="h", main="distance")


filelist <- c(
  "data_watch_jongin0524DATASET1", 
  "data_watch_jongin0524DATASET2",
  "data_watch_jongin0524DATASET3",
  "data_watch_jongin0524DATASET4",
  "data_watch_jongin0524DATASET1_2", 
  "data_watch_jongin0524DATASET2_2",
  "data_watch_jongin0524DATASET3_2",
  "data_watch_jongin0524DATASET4_2"
)

filelist <- c(
  "data_watch_jongin0524DATASET4"
)


filelist <- c(
  "data_watch_test0617_4"
)

#for(idx in c(1,2,3,7,8))

for(idx in c(1,8))
{

  for(i in 1:length(filelist))
  {
    name <- filelist[i]
    data <- read.table(paste("./data_raw/", name ,".txt",sep=""),sep=",",header=TRUE)
    t3 <- getFirstWindow(data, idx, 17, 50, TRUE, name, 0.2, FALSE)
    
    if(i==1)
    {
      t <- t3
    }else
      t <- rbind(t,t3)
  }
   
  if(idx=='1'){
    sum_data <- t[,4:length(colnames(t))]
  }else{
    sum_data <- cbind(sum_data,t[,5:length(colnames(t))])
  }
  
  print(paste("index : ",idx, "len : ",nrow(t) ))
  
}


write.csv(sum_data, file=paste("./data_raw/location_test2.arff",sep=""), row.names=FALSE)


createIntegralGraph <- function(x,y,graph_title,type="vel",plotting=FALSE)
{
  
  if(type=="vel")
  {
    new_y <- 0
    for(i in 1:length(y)){
      sum <- 0
      for(j in 1:i){
        sum <- sum + round(y[j],digits=2)
      }
      new_y[i] <- sum
    }
    res<-new_y
    ylabel <- "Velocity (m/s)"
    
  }else if(type=="loc")
  {
    new_y <- 0
    for(i in 1:length(y)){
      sum <- 0
      for(j in 1:i){
        sum <- sum + round(y[j],digits=2)
      }
      new_y[i] <- sum
    }
    
    new_yy <- 0
    for(i in 1:length(new_y)){
      sum <- 0
      for(j in 1:i){
        sum <- sum + round(new_y[j],digits=2)
      }
      new_yy[i] <- sum
    }
    res<-new_yy
    ylabel <- "distance (m/s^2)"
  }

    
  if(plotting){
    returnValue <- qplot(x,res, geom=c("line","point") ) + ggtitle(paste(graph_title," - (",sensor_name_list[idx],")")) + 
      ylab(ylabel) +
      xlab("time") +  theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"))
    
    print(returnValue)
  }

  return (res)
  
}



getFirstWindow <- function(data, idx, window_size, period, ploting=FALSE, graph_title ="noname", threshold = 0.2, set_btw = FALSE, startT =0, endT=0)
{
  data.mag <- subset(data,grepl(list[8],data$type))
  data.sub <- subset(data,grepl(list[idx], data$type))
  
  if(set_btw){
    data.sub <- subset(data.sub, subset=(data.sub$time > startT ))
    data.sub <- subset(data.sub, subset=(data.sub$time < endT ))
  }
  
  df <- data.frame(time_milli = data.sub$time, x=(data.sub$x), y=(data.sub$y), z=(data.sub$z) )
  window_step <- window_size
  
  if(ploting){
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
    
    print(returnValue)
  }
  
  window_set <- vector(mode="list", length=(4 + 3*11 + 10) ) # selected + 1
  
  sname <- list[idx]
  #window_set <- vector(mode="list", length=(31 - 8) ) # previous work
  names(window_set) <- c(
    "win_number","start_milli","end_milli","label",
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
    paste(sname,"_mean_avg",sep=""),
    paste(sname,"_max_avg",sep=""),
    paste(sname,"_min_avg",sep=""),
    paste(sname,"_entropy_avg",sep=""),
    paste(sname,"_energy_avg",sep=""),
    paste(sname,"_autocor1_avg",sep=""),
    paste(sname,"_th_avg",sep=""),
    paste(sname,"_autocor2_avg",sep=""),
    paste(sname,"_var_avg",sep=""),
    paste(sname,"_peakfreq_avg",sep="")   
  )

  
  window_num <- as.integer(nrow(data.sub)/window_step)
  print(paste("window_num",window_num))
  window_idx <- 1
  bWin <- FALSE
  
  epoches <- vector(mode="list", length=(2))
  names(epoches) <- c("win_number", "start_milli")
  ei <- 1
  
  epoches$win_number[ei] <- 1
  epoches$start_milli[ei] <- 1
  ind <- 1
  
  delayNum<-1
  for(i in 1:window_num)
  {
    window_data <- getWindow(data.sub,window_idx,window_size)
    window_data_for_mag <- getWindow(data.mag, window_idx, window_size)
    magnitude <- sqrt( (window_data_for_mag$x+50)^2+(window_data_for_mag$y+50)^2+(window_data_for_mag$z+50)^2)
    magnitude <- magnitude - mean(magnitude)
    
      if(var(magnitude)>threshold ){  # 0.05 
      
      if(bWin == FALSE)
      {
        delayNum <- i
        epoches$win_number[ei] = i
        epoches$start_milli[ei] = window_data$time[1]
        ei <- ei + 1
        if(ploting){
          defined_color <- "blue"
          rect <- data.frame(xmin=data.sub$time[window_idx], xmax=data.sub$time[window_idx+period], ymin=-Inf, ymax=Inf)
          returnValue <- returnValue + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill=defined_color, inherit.aes = FALSE)
        }
        
        window_data <- getWindow(data.sub,window_idx,period)
        window_df <- data.frame(x=window_data$x, y=window_data$y,
                                z=window_data$z, saxis=(window_data$x+window_data$y+window_data$z), magnitude=sqrt(window_data$x^2+window_data$y^2+window_data$z^2))  
        
        start_milli <-paste( data.sub$time[window_idx] )
        end_milli <- paste( data.sub$time[window_idx+period] )
        
        tmp_x<-createIntegralGraph(window_data$time, window_data$x,paste("x-axis",start_milli,"~",end_milli),type = "vel")
        tmp_y<-createIntegralGraph(window_data$time, window_data$y,paste("y-axis",start_milli,"~",end_milli),type = "vel")
        tmp_z<-createIntegralGraph(window_data$time, window_data$z,paste("z-axis",start_milli,"~",end_milli),type = "vel")
        
        tmp_x<-createIntegralGraph(window_data$time, window_data$x,paste("x-axis",start_milli,"~",end_milli),type = "loc")
        tmp_y<-createIntegralGraph(window_data$time, window_data$y,paste("y-axis",start_milli,"~",end_milli),type = "loc")
        tmp_z<-createIntegralGraph(window_data$time, window_data$z,paste("z-axis",start_milli,"~",end_milli),type = "loc")      
        
        dis <- sqrt(tmp_x[length(tmp_x)]^2 + tmp_y[length(tmp_y)]^2 + tmp_z[length(tmp_z)]^2)
        print(paste("x:",tmp_x[length(tmp_x)],"y:",tmp_y[length(tmp_y)],"z:",tmp_z[length(tmp_z)], "dis",sum(dis) ))
        
        scatterplot3d(tmp_x,tmp_y,tmp_z, pch=16, highlight.3d=TRUE,type="h", main="distance")
        
        
        p <- c(i,start_milli,end_milli,"TODO",
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
               getFeatureBy(window_df,"mean",avg=TRUE),
               getFeatureBy(window_df,"max",avg=TRUE),
               getFeatureBy(window_df,"min",avg=TRUE),
               getFeatureBy(window_df,"entropy",avg=TRUE),   
               getFeatureBy(window_df,"energy",avg=TRUE),
               getFeatureBy(window_df,"autocorrelation",12,avg=TRUE),
               getFeatureBy(window_df,"threshold",avg=TRUE),
               getFeatureBy(window_df,"autocorrelation", 1,avg=TRUE),
               getFeatureBy(window_df,"variance",avg=TRUE),
               getFeatureBy(window_df,"peakfreq",avg=TRUE)   
        )
        
        window_set<-rbind(window_set,p)
      }
      bWin <- TRUE
      
    }else{
      if(delayNum!=1 & (i-delayNum > 5))
        bWin <- FALSE
    }
    
    #returnValue <- returnValue + geom_vline(xintercept = data.sub$time[window_idx], colour="black", alpha=0.8)    
    window_idx <- window_idx + window_step
    
  }
  
  if(ploting)
    print(returnValue)
  
  View(epoches)
  window_set <- window_set[-1,]
  View(window_set)
  
  #write.csv(window_set, file=paste("./data_csv/",graph_title,".csv",sep=""), row.names=T)
  
  
  return(window_set)
}

