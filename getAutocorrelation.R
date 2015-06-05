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
