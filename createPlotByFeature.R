


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

scratching_data052130746
scratching_data052164709
scratching_data052120747
scratching_data052124502
data_watch_intentservice_changing_sites<- read.table("./data_raw/data_watch_intentservice_changing_sites.txt",sep=",",header=TRUE)

t<-rbind(data_watch_intentservice_4type_scratching_jongin[,1:6], data_watch_intentservice_changing_sites[,1:6])
t<-rbind(t, scratching_data052120747[,2:7]) # 기지게
t<-rbind(t, scratching_data052130746[,2:7]) # 
t<-rbind(t, scratching_data052164709[,2:7])
t<-rbind(t, scratching_data052124502[,2:7])

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


featureList <- c("mean", "max", "min", "entropy", "energy", "autocorrelation", "threshold", "variance", "peakfreq", "energyFrom_0to10Hz", 
  "RMS", "SD", "integratedRMS", "peaknumAuto", "prominentAuto", "weakpeakAuto", "maximumAuto", "height1stAuto","peaknumAutoSum",
  "prominentAutoSum", "weakpeakAutoSum", "getWeakPeakfreq" "getProminantPeakfreq"
  )


best_opt_list <-c("correlation",
                  "entropy",
                  "energy",
                  "peakfreq",
                  "RMS",
                  "maximumAuto")

featurename<-"getWeakPeakfreq"
idx<-3
createPlotByFeature(paste(featurename), total_combined,idx, 150, 50, featurename, x_type = "time",neith_th = 0.01,
                    saveFile = F,opt_lag = 15,mag = F, filtering = T, f_l2 = 10)
idx<-8
createPlotByFeature(paste(featurename), total_combined, idx, 150, 50, featurename, x_type = "time",neith_th = 0.01,doublecnt = 5,
                    saveFile = F,opt_lag = 15, mag = F, filtering = T, f_l2 = 5)

featurename<-"height1stPeakValleyAuto"

featurename<-"prominentAutoPeakValley"
idx<-3
createPlotByFeature(paste(featurename), total_combined,idx, 150, 50, featurename, x_type = "time",saveFile = F,
                    opt_lag = 6,mag = F, filtering = T, f_l2 = 5, b_avg = TRUE)
idx<-8
createPlotByFeature(paste(featurename), total_combined, idx, 150, 50, featurename, x_type = "time",saveFile = F,
                    opt_lag = 6, mag = F, filtering = T, f_l2 = 5, b_avg = TRUE)


for(i in 1:length(featureList) )
{
  featurename <- featureList[i]
  createPlotByFeature(paste("gyro",featurename), total_combined, 3, 150, 50, featurename, x_type = "time",saveFile = T)
}




featurename <-"entropy"
filtering <- TRUE
startLag <- 26
createPlotByFeature(paste(featurename,"_filtering",filtering), total_combined, 8, 150, 50, featurename, x_type = "time",saveFile = F, neith_th = 0.01, min_th=0.1, filtering = filtering, startLag=2)


createPlotByFeature(paste(featurename,2,"_filtering",filtering), total_combined, 8, 150, 50, featurename, x_type = "time",saveFile = F, neith_th = 0.01, min_th=0.1, filtering = filtering, startLag=2)
createPlotByFeature(paste(featurename,5,"_filtering",filtering), total_combined, 8, 150, 50, featurename, x_type = "time",saveFile = F, neith_th = 0.01, min_th=0.1,filtering = filtering, startLag=5)
createPlotByFeature(paste(featurename,10,"_filtering",filtering), total_combined, 8, 150, 50, featurename, x_type = "time",saveFile = F, neith_th = 0.01, min_th=0.1,filtering = filtering, startLag=10)
createPlotByFeature(paste(featurename,15,"_filtering",filtering), total_combined, 8, 150, 50, featurename, x_type = "time",saveFile = F, neith_th = 0.01, min_th=0.1,filtering = filtering, startLag=15)
createPlotByFeature(paste(featurename,20,"_filtering",filtering), total_combined, 8, 150, 50, featurename, x_type = "time",saveFile = F, neith_th = 0.01, min_th=0.1,filtering = filtering, startLag=20)


createPlotByFeature <- function(graph_title, data, idx, window_size, window_step, feature_type, 
                                 set_btw=FALSE, start_hour=1.1, end_hour=1.1, x_type="time", opt_lag=12,mag=FALSE,
                                cutoff=FALSE, f_l=0.1, butter_type="low", saveFile=FALSE,
                                neith_th = 0.01,  min_th = 0.01, max_th = 0.01, filtering = FALSE, filtering2 = FALSE,
                                startLag = 26, thresholdvar=0.1,f_l2=10,b_avg =FALSE,doublecnt=FALSE, type = "mag", signal_type = "no",
                                powerband_from = 0, powerband_to = 0, filter_type = "low", order = 1, filter_num = 1, spanV = 0.4
                                )
{
  data.sub <- subset(data,grepl(list[idx], data$type))
  data.mag <- subset(data,grepl(list[8], data$type))
  
  
  data.sub$hour <- data.sub$time/(1000*60*60)
  #  print(typeof(data.sub))
  if(set_btw){
    data.sub <- subset(data.sub, subset=(data.sub$hour > start_hour ))
    data.sub <- subset(data.sub, subset=(data.sub$hour < end_hour ))  
    graph_title <- paste(graph_title," - (",start_hour,",",end_hour,")")
  }
  
  if(cutoff){
    bf2 <- butter(1, (2*f_l)/(50), type=butter_type)
    data.sub$x <- filtfilt(bf2,data.sub$x)
    bf2 <- butter(1, (2*f_l)/(50), type=butter_type)
    data.sub$y <- filtfilt(bf2,data.sub$y)
    bf2 <- butter(1, (2*f_l)/(50), type=butter_type)
    data.sub$z <- filtfilt(bf2,data.sub$z)
  }
  
  window_num <- as.integer(nrow(data.sub)/window_step)
  
  data_feature <- vector(mode="list", length=4)
  names(data_feature) <- c("x","y","z","step_num")
  window_idx <- 1
  window_data_prev <- NULL
  for(i in 1:window_num)
  {
    #  print(c( window_idx, i))
    window_data_for_mag <- getWindow(data.mag, window_idx, window_size)
    
    window_data <- getWindow(data.sub,window_idx,window_size)
    magnitude <- sqrt( (window_data_for_mag$x+50)^2+(window_data_for_mag$y+50)^2+(window_data_for_mag$z+50)^2)
    magnitude <- magnitude - mean(magnitude)
    
    if(var(magnitude)>thresholdvar){
      
      first_1s <- getWindow(data.mag, window_idx, window_size/3)
      last_1s <- getWindow(data.mag, window_idx+window_size/3*2,window_size/3)
      
      first_1s <- sqrt( (first_1s$x+50)^2+(first_1s$y+50)^2+(first_1s$z+50)^2 )
      first_1s <- first_1s - mean(first_1s)
      
      last_1s <- sqrt( (last_1s$x+50)^2+(last_1s$y+50)^2+(last_1s$z+50)^2 )
      last_1s <- last_1s - mean(last_1s)
      
      if(var(first_1s) > thresholdvar & var(last_1s) > thresholdvar)
      {
#        if(signal_type == "autocorrelation")
#        {
#          signal_x <- getAutocorrelationSignal(window_data$x, startLag)
#          signal_y <- getAutocorrelationSignal(window_data$y, startLag)
#          signal_z <- getAutocorrelationSignal(window_data$z, startLag)
#          window_data <- data.frame(x=signal_x, y=signal_y, z=signal_z)
#        }else if(signal_type == "cepstrum")
#        {
#          
#        }
     #   print(paste("window_data len ", length(window_data$x), ",   window_data_prev len", length(window_data_prev$x)))
        
        features <- getFeatureBy(window_data, feature_type, opt_lag, avg = mag, neith_th = neith_th,  min_th = min_th, max_th = max_th, powerband_from = powerband_from, powerband_to = powerband_to, 
                                 filtering = filtering,startLag = startLag, type = type, doublecnt = doublecnt, filter_type = filter_type, order = order,
                                 filter_num = filter_num, spanV = spanV, window_data_prev = window_data_prev, filtering2 = filtering2,
                                 f_l=f_l2,b_avg = b_avg,signal_type = signal_type)

        if(mag)
          data_feature$x[i] <- features[1]
        else{
          data_feature$x[i] <- features[1]
          data_feature$y[i] <- features[2]
          data_feature$z[i] <- features[3]
        }
        
      }else{
        #print(paste("mov. of starting / ending", var(first_1s), var(last_1s)) )
        #createPlot(window_data, idx, "test", FALSE, "p3")  
        data_feature$x[i] <- 0
        data_feature$y[i] <- 0
        data_feature$z[i] <- 0
      }
    }
    else{
      data_feature$x[i] <- 0
      data_feature$y[i] <- 0
      data_feature$z[i] <- 0
    }
    window_data_prev <- window_data
    
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
  
  if(x_type=="time"){
    df2 <- data.frame(time =(1:length(data.sub$time) )*(1/50)*1000, x=data.sub$x, y=data.sub$y, z=data.sub$z)
    
    df <- data.frame(step_num = data_feature$step_num, time = data_feature$time, x=data_feature$x, y=data_feature$y, z=data_feature$z)
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
    
    returnValue2 <- ggplot(df2, aes(x=time,colour="axis")) +
      geom_line(aes(y=x, colour="X")) +
      geom_line(aes(y=y, colour="Y")) +
      geom_line(aes(y=z, colour="Z")) +
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")",sep="")) + 
      scale_color_manual(values=c("red","blue","violet")) +
      xlab("time order") +
      ylab(y_label_list[idx]) +
      scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
    #print(returnValue2)
    
  }else if(x_type=="step"){
  
    df2 <- data.frame(time =1:length(data.sub$time), x=data.sub$x, y=data.sub$y, z=data.sub$z)
    
    if(mag)
    {   
      df <- data.frame(step_num = data_feature$step_num, x=data_feature$x)
      
      returnValue <- ggplot(df, aes(step_num, y=sensor_value, color=axis)) +
           geom_line(if (feature_type=="correlation") aes(y=x, col="XY") else aes(y=x, col="X")) +
           ggtitle(paste(graph_title," - (", feature_type,", ",sensor_name_list[idx],")")) + 
           #  coord_fixed(ratio=1/4) +
           xlab(paste("window step index","(size:",window_size,",step:",window_step,")") ) +
           ylab(paste(feature_type," value"))+
           #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
           scale_x_continuous(breaks = seq(0,window_num+1,as.integer(window_num/10))) +
           theme_bw() +
           theme(panel.border = element_blank(), axis.line = element_line(colour="black"))
      
    }else{
      df <- data.frame(step_num = data_feature$step_num, 
                       x=data_feature$x, y=data_feature$y, z=data_feature$z)
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

    
    returnValue2 <- ggplot(df2, aes(x=time,colour="axis")) +
      geom_line(aes(y=x, colour="X")) +
      geom_line(aes(y=y, colour="Y")) +
      geom_line(aes(y=z, colour="Z")) +
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")",sep="")) + 
      scale_color_manual(values=c("red","blue","violet")) +
      xlab("time order") +
      ylab(y_label_list[idx]) +
      scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    print(returnValue2)
  }
 
  if(saveFile){
    pdf(paste("./plot_results/",graph_title,"_raw",".pdf",sep=""),width = 30)
    print(returnValue2)
    dev.off()
    
    pdf(paste("./plot_results/",graph_title,"_feature",".pdf",sep=""),width = 30)
    print(returnValue)
    dev.off()
  }
  
  #marrangeGrob(returnValue, returnValue2, ncol=2, nrow=2, main="Multiple plots on the same page")
  multiplot(returnValue2, returnValue, cols=1,saveFile=saveFile)
 
  #if(saveFile)
  #  dev.off()
  #plot_grid(returnValue2, returnValue , ncol = 1, nrow = 2)
  
#  multiplot(returnValue2, returnValue, cols=1,saveFile=saveFile)
  
}




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL,saveFile=FALSE) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    
 #   if(saveFile)
#      pdf(paste("./plot_results/",graph_title,".pdf",sep=""))
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
  #   Sys.sleep(1000)

      
    }
    
    
#    if(saveFile)
#      dev.off()
  }
}