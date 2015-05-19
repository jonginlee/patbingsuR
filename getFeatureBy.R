
#max<-0
#maxi<-0
#http://www.abecedarical.com/zenosamples/zs_complexnumbers.html

getPeakFrequency <- function(data_z)
{
  N <- length(data_z)
  #data_z <- data_z - mean(data_z)
  c <- 50/N
  max <- 0
  mk <- 0
  
  for(k in 0:(N/2-1)){
    sum<-0
    for(n in 0:(N-1))
    {
      sum <-sum + data_z[n+1]*exp(-1*sqrt(as.complex(-1))*pi*k*n/N)
    }  
    print(Mod(sum))
    v <- Mod(sum)
    if(v>max)
    {
      max <- v
      mk <- k
    }
  }
  
  sampling.rate = 50 
  nUniquePts <- ceiling((N+1)/2)
  freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
  
  print(paste("k",mk,"max_value",max,"fpk",c*mk,"mt",freqArray[mk+1]))
  
  return (c*mk)
}

for(i in 1:length(data$x)-1){
  #print(i)
  if(getAutocorrelation(data$x,i)>max){
    maxi<-i
    max<-getAutocorrelation(data$x,i)
  }
}


normalize <- function(x) {
  a <- min(x, na.rm=TRUE)
  b <- max(x, na.rm=TRUE)
  (x - a)/(b - a)
}


getEntropy <- function(data_z)
{
  y <- data_z - mean(data_z)
  y <- fft(y)
  y <- Mod(y)^2
  energySum <- sum(y)
  y <- y/energySum
  e <- -1*y*log2(y+0.000001)
  entropy_z <- sum(e)
  if(is.na(entropy_z)){
    View(y)
    View(e)
  }
  
  return (entropy_z)
}


getEnergy <- function(data_x)
{
  y <- data_x - mean(data_x)
  y <- fft(y)
  y <- Mod(y)^2
  SOSM_x <- (sum(y))/(length(y))
}

getCth <- function(data_x,th)
{
  Cnt<-0
  for(i in 1:length(data_x))
  {
    if(data_x[i]>th)
      Cnt <- Cnt + 1
  }
  
  return (Cnt)
}


getFeatureBy <- function(window_data, feature_type, opt_lag = 12, avg = FALSE, thr = 0.01)
{
  res<-0
  switch(feature_type,
         threshold={
           g<- 9.81
           #plot(window_data$x, type='l')
           if(avg == TRUE){
             window_data$avg <- (window_data$x + window_data$y + window_data$z)/3
             res <- c( getCth(window_data$avg,g*thr) )
           }else{
             res <- c(getCth(window_data$x-mean(window_data$x),g*thr), 
                      getCth(window_data$y-mean(window_data$y),g*thr),
                      getCth(window_data$z-mean(window_data$z),g*thr))
           }
           #plot(window_data$x-mean(window_data$x), type='l')
         },
         peakfreq={
           if(avg == TRUE){
             window_data$avg <- (window_data$x + window_data$y + window_data$z)/3
             res <- c(trans_to_frequency(window_data$avg-mean(window_data$avg),"maxfreq"))
           }else{
             res <- c(trans_to_frequency(window_data$x-mean(window_data$x),"maxfreq"), trans_to_frequency(window_data$y-mean(window_data$y),"maxfreq"), trans_to_frequency(window_data$z-mean(window_data$z),"maxfreq"))
           }
         },
         gyrowfeature={
           window_data$sx = predict(smooth.spline(window_data$time, window_data$x, spar = 0.3), window_data$time)$y
           window_data$sy = predict(smooth.spline(window_data$time, window_data$y, spar = 0.3), window_data$time)$y
           window_data$sz = predict(smooth.spline(window_data$time, window_data$z, spar = 0.3), window_data$time)$y
           
           #           p <- findpeaks(window_data$sx, sortstr = TRUE, npeaks = 3)[,1] 
           #           p2 <- findpeaks(-1*window_data$sx,sortstr = TRUE, npeaks = 3)[,1] 
           #           xf <- (sum(p)+sum(abs(p2)))- findpeaks(window_data$sx, sortstr = TRUE, npeaks = 1)[1,1]- findpeaks(-1*window_data$sx, sortstr = TRUE, npeaks = 1)[1,1]
           #           p <- findpeaks(window_data$sy, sortstr = TRUE, npeaks = 3)[,1] 
           #           p2 <- findpeaks(-1*window_data$sy,sortstr = TRUE, npeaks = 3)[,1] 
           #           yf <- (sum(p)+sum(abs(p2)))- findpeaks(window_data$sy, sortstr = TRUE, npeaks = 1)[1,1]- findpeaks(-1*window_data$sy, sortstr = TRUE, npeaks = 1)[1,1]
           #           p <- findpeaks(window_data$sz, sortstr = TRUE, npeaks = 3)[,1]
           #           p2 <- findpeaks(-1*window_data$sz,sortstr = TRUE, npeaks = 3)[,1] 
           #           zf <- (sum(p)+sum(abs(p2))) - findpeaks(window_data$sz, sortstr = TRUE, npeaks = 1)[1,1]- findpeaks(-1*window_data$sz, sortstr = TRUE, npeaks = 1)[1,1]
           
           p <- findPeaks(window_data$sx)
           p2 <- findValleys(window_data$sx)
           p<-p-1
           p2<-p2-1
           xf <- (sum(abs(window_data$sx[p]))+sum(abs(window_data$sx[p2]))) - abs(max(window_data$sx[p])) - abs(min(window_data$sx[p2]))
           p <- findPeaks(window_data$sy)
           p2 <- findValleys(window_data$sy)
           p<-p-1
           p2<-p2-1
           yf <- (sum(abs(window_data$sy[p]))+sum(abs(window_data$sy[p2]))) - abs(max(window_data$sy[p])) - abs(min(window_data$sy[p2]))
           p <- findPeaks(window_data$sz)
           p2 <- findValleys(window_data$sz)
           p<-p-1
           p2<-p2-1
           zf <- (sum(abs(window_data$sz[p]))+sum(abs(window_data$sz[p2]))) - abs(max(window_data$sz[p])) - abs(min(window_data$sz[p2]))
           
           #           xf <- abs(max(window_data$x)+abs(min(window_data$x)))
           #           yf <- abs(max(window_data$y)+abs(min(window_data$y)))
           #           zf <- abs(max(window_data$z)+abs(min(window_data$z)))
           
           #           xf <- abs(max(window_data$sx)+abs(min(window_data$sx)))
           #           yf <- abs(max(window_data$sy)+abs(min(window_data$sy)))
           #           zf <- abs(max(window_data$sz)+abs(min(window_data$sz)))
           
           res <- c( xf,yf,zf )
         },
         peakratio={
           #View(window_data)
           Px = trans_to_frequency(window_data$x - mean(window_data$x))
           Py = trans_to_frequency(window_data$y - mean(window_data$y))
           Pz = trans_to_frequency(window_data$z - mean(window_data$z))
           
           n = length(window_data$x)
           sampling.rate = 50 
           nUniquePts <- ceiling((n+1)/2)
           freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
           
           df <- data.frame(fre = freqArray, perX = Px, perY = Py, perZ = Pz)  
           #View(df)
           res <- c(getPeakRatioFeature(df$perX,df$fre, 0.005,1,FALSE),getPeakRatioFeature(df$perY,df$fre, 0.005,1,FALSE),getPeakRatioFeature(df$perZ,df$fre, 0.005,1,FALSE))
         },
         autocorrelation={
           if(avg==TRUE){
             window_data$avg <- (window_data$x + window_data$y + window_data$z)/3
             res <- c( getAutocorrelation(window_data$avg,opt_lag) )
           }else{
             res <- c(getAutocorrelation(window_data$x,opt_lag), getAutocorrelation(window_data$y,opt_lag), getAutocorrelation(window_data$z,opt_lag))             
           }
         },
         autocorrelationV2={ # return max autocorrelation
           res <- c(getAutocorrelationV2(window_data$x), getAutocorrelationV2(window_data$y), getAutocorrelationV2(window_data$z))
         },
         mean={
           res <- c(mean(window_data$x),mean(window_data$y),mean(window_data$z))
           #print(paste('mean',mean))
         },
         max={
           res <- c(max(window_data$x),max(window_data$y),max(window_data$z))
           #print(paste('max',max))
         },
         min={
           res <- c(min(window_data$x),min(window_data$y),min(window_data$z))
           #print(paste('min',res))
         },
         variance={
           if(avg == TRUE){
             window_data$avg <- (window_data$x + window_data$y + window_data$z)/3
             res <- c( var(window_data$avg) )
           }else{
             res <- c(var(window_data$x),var(window_data$y),var(window_data$z))
           }
           #print(paste('var',res))
         },
         correlation={
           res <- c(cor(window_data$x, window_data$y),cor(window_data$x, window_data$z),cor(window_data$y, window_data$z))
           #print(paste('cov', res))
         },
         energy={
           data<-window_data
           ## TODO : mean acceleration 뺀다고??? 왜??
           y <- data$x - mean(data$x)
           y <- fft(y)
           y <- Mod(y)^2
           SOSM_x <- (sum(y))/(length(y))
           
           y <- data$y - mean(data$y)
           y <- fft(y)
           y <- Mod(y)^2
           SOSM_y <- (sum(y))/(length(y))
           
           y <- data$z - mean(data$z)
           y <- fft(y)
           y <- Mod(y)^2
           SOSM_z <- (sum(y))/(length(y))
           
           res <- c(SOSM_x, SOSM_y, SOSM_z)
           #print(paste("energy",res))
         },
         entropy={
           data<-window_data
           y <- data$x - mean(data$x)
           y <- fft(y)
           y <- Mod(y)^2
           energySum <- sum(y)
           y <- y/energySum
           e <- -1*y*log2(y+0.000001)         
           entropy_x <- sum(e)
           if(is.na(entropy_x)){
             View(y)
             View(e)
           }
           
           y <- data$y - mean(data$y)
           y <- fft(y)
           y <- Mod(y)^2
           energySum <- sum(y)
           y <- y/energySum
           e <- -1*y*log2(y+0.000001)         
           entropy_y <- sum(e)
           if(is.na(entropy_y)){
             View(y)
             View(e)
           }
           
           y <- data$z - mean(data$z)
           y <- fft(y)
           y <- Mod(y)^2
           energySum <- sum(y)
           y <- y/energySum
           e <- -1*y*log2(y+0.000001)         
           entropy_z <- sum(e)
           if(is.na(entropy_z)){
             View(y)
             View(e)
           }
           
           res <- c(entropy_x, entropy_y, entropy_z)
           
           #print(res)
         }
  )
  return (res)
}


getFeaturesFromEpoch <- function(df, start_milli, end_milli, spanValue, title, type)
{
  
  epoch = subset(df, (time_milli>=start_milli) & (time_milli<=end_milli))  
  #View(epoch)
  if(type=="detail")
  {
    epoch$saxis_loess <- predict(loess(z~time_milli,epoch,span=spanValue), epoch$time_milli)
    returnValue <- ggplot(epoch, aes(x=time_milli, y=saxis_loess)) +
      geom_line() +
      geom_point() +
      #  geom_line(aes(y=x, col="X")) +
      #    geom_point(aes(y=x, col="X")) +
      #    geom_line(aes(y=y, col="Y")) +
      #    geom_line(aes(y=z, col="Z")) +
      ggtitle(title) + 
      #  coord_fixed(ratio=1/4) +
      xlab("time(milli)") +
      ylab("Acceleration(g)")+
      #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
      #    scale_x_continuous(breaks = spliting) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
  }else if(type=="getfeatures")
  {
    returnValue <- c(start_milli,end_milli,
                     getFeature(epoch,"mean"),
                     getFeature(epoch,"entropy"),
                     getFeature(epoch,"energy"),
                     getFeature(epoch,"correlation"),
                     getFeature(epoch,"autocorrelation")
    )
    
  }
  
  return (returnValue)
}

