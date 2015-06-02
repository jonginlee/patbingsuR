


filelist <- c(
  "data_watch20150412_scratching_xy",
  "data_watch20150412_scratching_x",
  "data_watch20150412_scratching_y",
  "data_watch20150412_scratching_z",
  "data_watch20150413_eunji_scratch_2",
  "data_watch20150413_seungho_3",
  "data_watch20150412_scratching_no_wrist",
  "data_watch20150416_scratching_no_wrist_4",
  "scratching_data0521(2_45_02)",
  "scratching_data0521(3_07_46)",
  "scratching_data0521(4_40_18)",
  "scratching_data0521(5_12_04)",
  "scratching_data0521(5_42_37)",
  "scratching_data0521(6_47_09)",
  "scratching_data0521(2_07_47)",
  "scratching_data0521(2_12_43)"
)


creatingPlotsByFeaturesSensors <- function( filelist, idxlist, feature_type )
{
  
  for(idx in idxlist)
  {
    for(i in 1:length(filelist))
    {
      name <- filelist[i]
      data <- read.table(paste("./data_raw/", name ,".txt",sep=""),sep=",",header=TRUE)
      t3 <- createPlotByFeature(name, data, idx, 150,50,feature_type,x_type = "step")
      
      print(t3)

    }
  }
  
}


createPlotByFeature <- function(graph_title, data, idx, window_size, window_step, feature_type, 
                                 set_btw=FALSE, start_hour=1.1, end_hour=1.1, x_type="time")
{
  data.sub <- subset(data,grepl(list[idx], data$type))
  
  data.sub$hour <- data.sub$time/(1000*60*60)
  #  print(typeof(data.sub))
  if(set_btw){
    data.sub <- subset(data.sub, subset=(data.sub$hour > start_hour ))
    data.sub <- subset(data.sub, subset=(data.sub$hour < end_hour ))  
    graph_title <- paste(graph_title," - (",start_hour,",",end_hour,")")
  }
  
  window_num <- as.integer(nrow(data.sub)/window_step)
  
  data_feature <- vector(mode="list", length=4)
  names(data_feature) <- c("x","y","z","step_num")
  window_idx <- 1
  for(i in 1:window_num)
  {
    #  print(c( window_idx, i))
    window_data <- getWindow(data.sub,window_idx,window_size)
    features <- getFeatureBy(window_data, feature_type)
    data_feature$x[i] <- features[1]
    data_feature$y[i] <- features[2]
    data_feature$z[i] <- features[3]
    data_feature$step_num[i] <- i
    window_idx <- window_idx + window_step
  }
  

  data_feature$time <- (data_feature$step_num*(window_step/50))*1000
  max_value <- (as.integer(max(data_feature$time)))
  spliting <- seq(0,max_value,max_value/10)
  xlablename <- "time(millisecond)"

  
  
  if(set_btw){
    data_feature$time <- data_feature$time + start_hour
  }
  
  
  df <- data.frame(step_num = data_feature$step_num, time = data_feature$time, x=data_feature$x, y=data_feature$y, z=data_feature$z)
  
  if(x_type=="time"){
    returnValue <- ggplot(df, aes(time, y=sensor_value, color=axis)) +
      geom_line(if (feature_type=="correlation") aes(y=x, col="XY") else aes(y=x, col="X")) +
      geom_line(if (feature_type=="correlation") aes(y=y, col="XZ") else aes(y=y, col="Y")) +
      geom_line(if (feature_type=="correlation") aes(y=z, col="YZ") else aes(y=z, col="Z")) +
      #    ggtitle(paste(graph_title," - (", feature_type,", ",sensor_name_list[idx],")")) + 
      ggtitle(paste(feature_type,"feature")) + 
      
      #    geom_vline(xintercept = data_itching_list, col="purple",alpha=0.1 ) +
      #  coord_fixed(ratio=1/4) +
      #    xlab(paste("window step index","(size:",window_size,",step:",window_step,")") ) +
      xlab(paste(xlablename,"(size:",window_size,",step:",window_step,")")) +
      ylab(paste(feature_type," value")) +
      #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
      # scale_x_continuous(breaks = seq(0,window_num+1,as.integer(window_num/10))) +
      scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
  }else if(x_type=="step"){
    returnValue <- ggplot(df, aes(step_num, y=sensor_value, color=axis)) +
      geom_line(if (feature_type=="correlation") aes(y=x, col="XY") else aes(y=x, col="X")) +
      geom_line(if (feature_type=="correlation") aes(y=y, col="XZ") else aes(y=y, col="Y")) +
      geom_line(if (feature_type=="correlation") aes(y=z, col="YZ") else aes(y=z, col="Z")) +
      ggtitle(paste(graph_title," - (", feature_type,", ",sensor_name_list[idx],")")) + 
      #  coord_fixed(ratio=1/4) +
      xlab(paste("window step index","(size:",window_size,",step:",window_step,")") ) +
      ylab(paste(feature_type," value"))+
      #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
      scale_x_continuous(breaks = seq(0,window_num+1,as.integer(window_num/10))) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"))
  }
  
  return (returnValue)
  
}