getAutocorrelation <- function(data, lag)
{
  u <- mean(data)
  N <- length(data)
  sum <- 0
  for(i in 1:(N-lag))
  {
    sum <- sum + (data[i]-u)*(data[i+lag]-u)
  }
  sum2 <- 0
  for(i in 1:N)
  {
    sum2 <- sum2 + (data[i]-u)^2
  }
  #  sum <- sum/(N-lag)
  res <- sum/sum2
  
  return (res)
}

getAutocorrelationV2 <- function(data)
{
  n <- length(data)
  max <- 0 
  maxlag <- 0
  v <- 0
  for( i in 5:(n-5)){
    v <- getAutocorrelation(data,i)
    if( abs(v) > abs(max) ){
      max <- abs(v)
      maxlag <- i
    }
  }
  #print(paste("max-autocorrelation",max,", maxlag",maxlag))
  
  return (max)
}

acf_data[1]<-10
data <-my.ts
for(i in 0:length(data))
{
  acf_data[i+1] <- getAutocorrelation(data,i)
} 

data <- read.table("./data_raw/jonginlee_data0623/data2.txt",sep=",",header=TRUE)
data.sub <- subset(data, grepl(list[8], data$type))
res <- createPlot(data.sub, 8, "test", TRUE, "p3")

end_hour <- 15000
start_hour <- 12000
data.sub <- subset(data.sub, subset=(data.sub$time < end_hour ))
data.sub <- subset(data.sub, subset=(data.sub$time > start_hour ))
res <- createPlot(data.sub, 8, "test", TRUE, "p3")


10000,13000
12000,15000
12000,15000
12000,15000

signal_x <- getAutocorrelationSignal(data.sub$x, 26)
mximumAtuoValue <- max(signal_x)
mximumAtuoValue

getFirstHeightBy <- function(signal, type="peaks")
{ # after zerocrossing
  resValue<-0
  if(type=="peaks")
  {
    p <- findPeaks(signal)
    if( (length(p)==0)){
      #print(p)
      resValue <-0
    }else{
      for(i in 1:length(p))
      {
        if(p[i]!=1){
          t<-signal[1:p[i]]
          if( (length(t[t>0]) > 0) & (length(t[t<0]) > 0) )
          {
            #print(paste(min(t), signal[p[i]]))
            resValue <- min(t) - signal[p[i]]
            break
          }
        }
      }
    }
    

    
  }
  #print(paste("resValue",resValue))
  
  return(abs(resValue))
}



getNumPeaksBy(signal_x, 0.1,0.4, "peaks", "weak")

getNumPeaksBy <- function(signal, neigh_th, min_th, type = "peaks", filtering = "prominent", calType = "num")
{
  require("quantmod")
  
  if( (type=="peaks") & (filtering=="prominent") )
  {
    p <- findPeaks(signal, thresh = neigh_th)
    res <- getNumThreshold(signal[p], min_th, type ="up", calType = calType)
    
  }else if( (type=="peaks") & (filtering=="weak") )
  {
    #p_raw <- findPeaks(signal,thresh = 0)
    #print(p_raw)
    p <- findPeaks(signal,thresh = neigh_th)
    #print(p)
    #weak_p <- p_raw[-which( p_raw %in% p)]
    #print(weak_p)
    res <- getNumThreshold(signal[p], min_th, type="down", calType = calType)
    #print(res)
  }else if( type=="num")
  {
    p <- findPeaks(signal,thresh = 0.01)
    if(calType == "num")
      res <- length(p)
    else
      res <- sum(signal[p])
  }
  
  return (res)
}


getNumThreshold <- function(peakArray, th, type="up", calType="num"){
  count <-0
  sum <- 0
  if(length(peakArray)==0){
    return (0)
  }
  for(i in 1:length(peakArray)){
    if(type == "up"){
      #print(paste(peakArray[i],"-",i))
      if(peakArray[i] > th){
        count <- count + 1  
        sum <- sum + peakArray[i]
      }
    }else if(type == "down"){
      #print(paste(peakArray[i],"-",i))
      if(peakArray[i] <= th){
        count <- count + 1
        sum <- sum + peakArray[i]
      }
    }
  }
  if(calType=="num")
    res <- count
  else 
    res <- sum
  return(res)
}

createAutocorrelationSignal <- function(data, idx, startLag,
                                        setbtw=FALSE, start_milli=0.1, end_milli=0.1, avg=FALSE, type="mag", # mag, PC
                                        graph_title="noname",threshold = 0,peak_threshold=0)
{
  data.sub <- subset(data, grepl(list[idx], data$type))
  
  if(setbtw)
  {
    data.sub <- subset(data.sub, subset=(data.sub$time < end_milli ))
    data.sub <- subset(data.sub, subset=(data.sub$time > start_milli ))
  }
  
  
  if(avg == FALSE){
    signal_x <- getAutocorrelationSignal(data.sub$x, startLag)
    signal_y <- getAutocorrelationSignal(data.sub$y, startLag)
    signal_z <- getAutocorrelationSignal(data.sub$z, startLag)
    
    raw_x_len <- length(findPeaks(signal_x,thresh = 0)) + length(findValleys(signal_x,thresh = 0))
    th_x_len <- length(findPeaks(signal_x,thresh = threshold)) + length(findValleys(signal_x,thresh = threshold))
    #print(paste(" (x axis) # of peaks and velly : ", raw_x_len, "   # of peaks and velly (threshold) : ", th_x_len ))
    
    
    raw_y_len <- length(findPeaks(signal_y,thresh = 0)) + length(findValleys(signal_y,thresh = 0))
    th_y_len <- length(findPeaks(signal_y,thresh = threshold)) + length(findValleys(signal_y,thresh = threshold))
    #print(paste(" (y axis) # of peaks and velly : ", raw_y_len, "   # of peaks and velly (threshold) : ", th_y_len ))
    
    raw_z_len <- length(findPeaks(signal_z,thresh = 0)) + length(findValleys(signal_z,thresh = 0))
    th_z_len <- length(findPeaks(signal_z,thresh = threshold)) + length(findValleys(signal_z,thresh = threshold))
    #print(paste(" (z axis) # of peaks and velly : ", raw_z_len, "   # of peaks and velly (threshold) : ", th_z_len ))
    
    ##
    data.sub$time <- data.sub$time - data.sub$time[1]
    #print(paste("len check : ",length(signal_x),length(signal_y),length(signal_z),length(data.sub$time)))
    
    max_value <- (as.integer(max(data.sub$time)))
    spliting <- seq(0,max_value,max_value/10)
    graph_title <- "3-axis "
    
    df <- data.frame(time = data.sub$time[startLag:length(data.sub$time)], x=signal_x, y=signal_y, z=signal_z)
    #print(paste("len check - time : ",length(data.sub$time[startLag:length(data.sub$time)])))
    #View(df)
    
    returnValue <- ggplot(df, aes(x=time,colour="axis")) +
      geom_line(aes(y=x, colour="X")) +
      geom_line(aes(y=y, colour="Y")) +
      geom_line(aes(y=z, colour="Z")) +
      #geom_line(aes(y=mag, colour="_Magnitude")) + 
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")",sep="")) + 
      scale_color_manual(values=c("red","blue","black","violet")) +
      xlab("Lag (milliseconds)") +
      ylab("Amplitude") +
      ylim(-1, 1) +
      scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  }else{
    
    if(type=="mag")
    { 
      data.sub$mag <- sqrt( (data.sub$x+50)^2 + (data.sub$y+50)^2 + (data.sub$z+50)^2)
      data.sub$avg <- data.sub$mag - mean(data.sub$mag)    
      graph_title <- paste("average - magnitude")
      
    }
    else if(type=="PC")
    {
      trans <- preProcess(data.sub[,3:5], method=c("BoxCox", "center", "scale", "pca"))
      PC <- predict(trans, data.sub[,3:5])
      #plot(1:length(PC$PC1), PC$PC1, type="l")
      data.sub$avg <- PC$PC1
      graph_title <- paste("average - 1st comp. from PCA analysis")
    }
    signal_x <- getAutocorrelationSignal(data.sub$avg, startLag)
    
    
    raw_p <- findPeaks(signal_x,thresh = 0)
    raw_p2 <- findValleys(signal_x,thresh = 0)
    
    p <- findPeaks(signal_x,thresh = threshold)
    p2 <- findValleys(signal_x,thresh = threshold)
    
    num <- getNumThreshold(signal_x[findPeaks(signal_x,thresh=threshold)], peak_threshold)
    #print(paste("# of peaks and veally (more than a 0.01)", num ))
    
    
    
    data.sub$time <- data.sub$time - data.sub$time[1]
    #print(paste("len check : ",length(signal_x),length(data.sub$time)))
    
    max_value <- (as.integer(max(data.sub$time)))
    spliting <- seq(0,max_value,max_value/10)
    
    df <- data.frame(time = data.sub$time[startLag:length(data.sub$time)], x=signal_x)
    #print(paste("len check - time : ",length(data.sub$time[startLag:length(data.sub$time)])))
    
    returnValue <- ggplot(df, aes(x=time,colour="axis")) +
      geom_line(aes(y=x, colour="X")) +
      ggtitle(paste(graph_title," (",sensor_name_list[idx],")",sep="")) + 
      scale_color_manual(values=c("red")) +
      xlab("Lag (milliseconds)") +
      ylab("Amplitude") +
      ylim(-1, 1) +
      scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
    returnValue <- returnValue + geom_vline(xintercept = df$time[p-1], alpha=1, colour="red",linetype=4)
    returnValue <- returnValue + geom_vline(xintercept = df$time[p2-1], alpha=1, colour="blue",linetype=4)
    
    #print(paste("# of peaks and velly (", threshold, ") :",sum(length(p)+length(p2)),sep=""))
    #print(paste("# of peaks and velly : ", sum(length(raw_p)+length(raw_p2))))
  }
  
  
  
  
  print(returnValue)
  
}


getAutocorrelationSignal <- function(data_a, startLag)
{
  data_list_a <- 1
  
  i<-1
  endLag<-length(data_a)
  for(k in (startLag-1):(endLag-1))
  {
    data_list_a[i] <- getAutocorrelation(data_a, k)
    i <- i+1
  }
  #View(data_list_a)
  
  return (data_list_a)
}



createHeatmapByFeature <- function(graph_title, data, idx, window_size, window_step, 
                                   cut=TRUE, set_btw=FALSE, start_hour=1.1, end_hour=1.1, x_type="time")
{
  
  data.sub <- subset(data,grepl(list[idx], data$type))
  if(cut){
    data.sub <- subset(data.sub, subset=(data.sub$time > 1000*60*2 ))  
    e_idx <- nrow(data.sub)
    e_time <- data.sub$time[e_idx]
    data.sub <- subset(data.sub, subset=(data.sub$time < (e_time - (1000*60*2)) ))
    ##
  }
  
  data.sub$hour <- data.sub$time/(1000*60*60)
  #  print(typeof(data.sub))
  if(set_btw){
    data.sub <- subset(data.sub, subset=(data.sub$hour > start_hour ))
    data.sub <- subset(data.sub, subset=(data.sub$hour < end_hour ))  
    graph_title <- paste(graph_title," - (",start_hour,",",end_hour,")")
    ##
  }
  
  window_num <- as.integer(nrow(data.sub)/window_step)
  
  data_feature <- vector(mode="list", length=4)
  names(data_feature) <- c("x","y","z","step_num")
  window_idx <- 1
  heatmap_matrix_x <- vector(mode="list",length = length(window_num))
  heatmap_matrix_y <- vector(mode="list",length = length(window_num))
  heatmap_matrix_z <- vector(mode="list",length = length(window_num))
  
  
  len <- window_size/2
  for(k in 0:len)
  {
    window_idx <- 1
    for(i in 1:window_num)
    {
      #  print(c( window_idx, i))
      window_data <- getWindow(data.sub,window_idx,window_size)
      #
      
      data_feature$x[i] <- getAutocorrelation(window_data$x, k)
      data_feature$y[i] <- getAutocorrelation(window_data$y, k)
      data_feature$z[i] <- getAutocorrelation(window_data$z, k)
      #
      data_feature$step_num[i] <- i
      window_idx <- window_idx + window_step
    }
    heatmap_matrix_x <- rbind(heatmap_matrix_x, data_feature$x)
    heatmap_matrix_y <- rbind(heatmap_matrix_y, data_feature$y)
    heatmap_matrix_z <- rbind(heatmap_matrix_z, data_feature$z)
  }
  heatmap_matrix_x<-heatmap_matrix_x[2:nrow(heatmap_matrix_x),]
  heatmap_matrix_y<-heatmap_matrix_y[2:nrow(heatmap_matrix_y),]
  heatmap_matrix_z<-heatmap_matrix_z[2:nrow(heatmap_matrix_z),]
  
  
  data_feature$time <- (data_feature$step_num*(window_step/50))*1000
  max_value <- (as.integer(max(data_feature$time)))
  spliting <- seq(0,max_value,max_value/10)
  xlablename <- "time(millisecond)"
  #  View(heatmap_matrix)
  pal <- colorRampPalette(c("red", "yellow"), space = "rgb")
  p1 <- levelplot(t(heatmap_matrix_x), main="accelerometer(X-axis)", ylim=c(0,len), cex.axis=1.5, cex.lab=1.5, xlab="window_step", ylab="lag ", col.regions=colorRampPalette(c("blue", "yellow","red", "black")), at=seq(-1,1,length=200))
  p2 <- levelplot(t(heatmap_matrix_y), main="accelerometer(Y-axis)", ylim=c(0,len), cex.axis=1.5, cex.lab=1.5,xlab="window_step", ylab="lag ", col.regions=colorRampPalette(c("blue", "yellow","red", "black")), at=seq(-1,1,length=200))
  p3 <- levelplot(t(heatmap_matrix_z), main="accelerometer(Z-axis)", ylim=c(0,len),cex.axis=1.5, cex.lab=1.5,xlab="window_step", ylab="lag ", col.regions=colorRampPalette(c("blue", "yellow","red", "black")), at=seq(-1,1,length=200))
  
  returnValue <-p1
  print(p1)
  print(p2)
  print(p3)
  #returnValue <- grid.arrange(p1, p2, p3, ncol=2, main=textGrob("autocorrelation",gp = gpar(fontsize = 30, face = "bold", col = "black")))   
  #return (returnValue)
  
}
