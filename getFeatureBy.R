
#max<-0
#maxi<-0
#http://www.abecedarical.com/zenosamples/zs_complexnumbers.html

Px = trans_to_frequency(data.sub$x - mean(data.sub$x))
data <- data.sub$x - mean(data.sub$x)



getPowerBandBy <- function(data, from, to, sampling.rate = 50)
{
  avgV = trans_to_frequency(data)
  
  n = length(data)
  #sampling.rate = 50 
  nUniquePts <- ceiling((n+1)/2)
  freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
  
  sum <- 0
  for(i in 1:length(freqArray))
  {
    if( (from < freqArray[i]) & (freqArray[i] <= to) ){
      #print(paste("i",i))
      sum <- sum + avgV[i]
    }
    
    if(freqArray[i] > to)
      break
  }
  return(sum)
}

getPeakRatioFeature<-function(power, freq, cutoff=0.005, type=1, ploting=FALSE, title="no")
{
  cutoff<-0.0005
  p <- findPeaks(power)
  p <- p-1
  max <- 0
  maxP <- 0
  #View(power)
  #View(p)
  if(length(p)!=0)
  {
    for(i in 1:length(p))
    {
      if(power[p[i]]>max){
        maxP <- p[i]
        max <- power[p[i]]
      }
    }
  }
  
  
  if(freq[maxP-1]==0){
    rect <- data.frame(xmin=freq[maxP-1], xmax=freq[maxP+2], ymin=-Inf, ymax=Inf)
    x <- c(freq[maxP-1], freq[maxP], freq[maxP+1], freq[maxP+2])
    y <- c(power[maxP-1], power[maxP], power[maxP+1], power[maxP+2])
  }else{
    rect <- data.frame(xmin=freq[maxP-2], xmax=freq[maxP+2], ymin=-Inf, ymax=Inf)
    x <- c(freq[maxP-2], freq[maxP-1], freq[maxP], freq[maxP+1], freq[maxP+2])
    y <- c(power[maxP-2], power[maxP-1], power[maxP], power[maxP+1], power[maxP+2])
  }
  
  
  A <- integrateTrapezoid(x, y)
  left <- integrateTrapezoid(freq, power)
  peakSum <- sum(power[p])
  
  if(ploting==TRUE){
    print(paste("peak - integrate",A,"sum",sum(y),"peakValue",power[maxP],"peakSum",peakSum))  
    print(paste("ratio",A/left,"peakRatio",power[maxP]/peakSum))      
  }
  
  returnValue <- 0
  if(length(p)==0){
    returnValue <- 0
  }else if(type==1){
    returnValue <- power[maxP]/peakSum
  }else if(type==2){
    returnValue <- A/left
  }
  
  sampling.rate <- 50
  max_value <- max(sampling.rate/2)
  spliting <- seq(0,max_value,max_value/10)
  
  if(ploting == TRUE)
  {
    df <- data.frame(fre = freq, perM = power)  
    
    graphTemp <- ggplot(df, aes(x=fre, colour="axis")) +
      geom_line(aes(y=perM, colour="axis")) +
      #geom_point() +
      #geom_point(data = d2, aes(y=po,colour="red")) +
      ggtitle(paste("Power Spectrum",title)) + 
      #  coord_fixed(ratio=1/4) +
      xlab("Frequency (Hz)") +
      ylab("Power") +
      scale_color_manual(values=c("red","blue","black","violet")) +
      scale_x_continuous(breaks = spliting) +
      #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
      #scale_x_continuous(breaks = seq(start_time,end_time,(as.integer((end_time-start_time)/10)) )) +
      #scale_x_continuous(breaks = 10) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    graphTemp <- graphTemp + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill="blue", inherit.aes = FALSE)        
    graphTemp <- graphTemp + geom_vline(xintercept = df$fre[p], alpha=1, colour="red",linetype=4)
    print(graphTemp) 
  }
  
  return (returnValue)
}



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

getIntegrateSignal <- function(signal)
{
  require("oce")
  res<-i
  for(i in 1:length(signal))
  {
    xrange <- c(1:i)
    yrange <- signal[1:i]
    res[i]<-integrateTrapezoid(xrange, yrange)
  }
  
  return(res)
}

paste(sname,"_integrated_RMS_avg(PC)",sep=""),
paste(sname,"_numPeak_avg(PC)",sep=""),
paste(sname,"_promientPeak_avg(PC)",sep=""),
paste(sname,"_weakPeak_avg(PC)",sep=""),
paste(sname,"_maxAuto_avg(PC)",sep=""),
paste(sname,"_height1stAuto_avg(PC)",sep=""),
paste(sname,"_powerBand0_2.5avg(PC)",sep=""),
paste(sname,"_powerBand2.5_5avg(PC)",sep=""),
paste(sname,"_powerBand5_7.5avg(PC)",sep=""),
paste(sname,"_powerBand7.5_10avg(PC)",sep=""),
paste(sname,"_powerBand10_12.5avg(PC)",sep=""),
paste(sname,"_powerBand12.5_15avg(PC)",sep=""),
paste(sname,"_powerBand15_17.5avg(PC)",sep=""),
paste(sname,"_powerBand17.5_20avg(PC)",sep=""),
paste(sname,"_powerBand20_22.5avg(PC)",sep=""),
paste(sname,"_powerBand22.5_25avg(PC)",sep="")




getFeatureBy <- function(window_data, feature_type, opt_lag = 12, avg = FALSE, thr = 0.01, type = "mag", sampling.rate = 50,
                         powerband_from=0, powerband_to=0, startLag=26)
{
 
  #window_data$x <- window_data$x - mean(window_data$x)
  #window_data$y <- window_data$y - mean(window_data$y)
  #window_data$z <- window_data$z - mean(window_data$z)
  
  res<-0
  if(avg){
    if(type == "PC") ## PC1 is used in the principal component
    {
      trans <- preProcess(window_data[,3:5], method=c("BoxCox", "center", "scale", "pca"))
      PC <- predict(trans, window_data[,3:5])
      #plot(1:length(PC$PC1), PC$PC1, type="l")
      window_data$avg <- PC$PC1
    }else if(type=="mag") ## magnitude is used
    {
      window_data$mag <- sqrt( (window_data$x+50)^2 + (window_data$y+50)^2 + (window_data$z+50)^2)
      window_data$avg <- window_data$mag - mean(window_data$mag)    
    }
  }
  
  switch(feature_type,
         peaknumAutoSum={
           if(avg==TRUE){
             signal <- getAutocorrelationSignal(window_data$avg, startLag)
             res <- c( getNumPeaksBy(signal, 0.01,0.05, "num",calType = "sum") )
           }else{
             signal_x <- getAutocorrelationSignal(window_data$x, startLag)
             signal_y <- getAutocorrelationSignal(window_data$y, startLag)
             signal_z <- getAutocorrelationSignal(window_data$z, startLag)
             res <- c( getNumPeaksBy(signal_x, 0.01, 0.05, "num",calType = "sum") ,
                       getNumPeaksBy(signal_y, 0.01, 0.05, "num",calType = "sum") ,
                       getNumPeaksBy(signal_z, 0.01, 0.05, "num",calType = "sum") 
             )
           }
         },
         prominentAutoSum={
           if(avg==TRUE){
             signal <- getAutocorrelationSignal(window_data$avg, startLag)
             res <- c( getNumPeaksBy(signal, 0.01, 0.1, "peaks", "prominent",calType = "sum") )
             
           }else{
             signal_x <- getAutocorrelationSignal(window_data$x, startLag)
             signal_y <- getAutocorrelationSignal(window_data$y, startLag)
             signal_z <- getAutocorrelationSignal(window_data$z, startLag)
             res <- c( getNumPeaksBy(signal_x, 0.01, 0.1, "peaks", "prominent",calType = "sum"), # 0.05 -> 0.1
                       getNumPeaksBy(signal_y, 0.01, 0.1, "peaks", "prominent",calType = "sum"),
                       getNumPeaksBy(signal_z, 0.01, 0.1, "peaks", "prominent",calType = "sum")
             )
           }
         },
         weakpeakAutoSum={
           if(avg==TRUE){
             signal <- getAutocorrelationSignal(window_data$avg, startLag)
             res <- c( getNumPeaksBy(signal, 0.01, 0.1, "peaks", "weak",calType = "sum") )
             
           }else{
             signal_x <- getAutocorrelationSignal(window_data$x, startLag)
             signal_y <- getAutocorrelationSignal(window_data$y, startLag)
             signal_z <- getAutocorrelationSignal(window_data$z, startLag)
             res <- c( getNumPeaksBy(signal_x, 0.01, 0.1, "peaks", "weak",calType = "sum"),
                       getNumPeaksBy(signal_y, 0.01, 0.1, "peaks", "weak",calType = "sum"),
                       getNumPeaksBy(signal_z, 0.01, 0.1, "peaks", "weak",calType = "sum")
             )
           }
         },
         peaknumAuto={
           if(avg==TRUE){
             signal <- getAutocorrelationSignal(window_data$avg, startLag)
             res <- c( getNumPeaksBy(signal, 0.01,0.05, "num") )
           }else{
             signal_x <- getAutocorrelationSignal(window_data$x, startLag)
             signal_y <- getAutocorrelationSignal(window_data$y, startLag)
             signal_z <- getAutocorrelationSignal(window_data$z, startLag)
             res <- c( getNumPeaksBy(signal_x, 0.01, 0.05, "num") ,
                       getNumPeaksBy(signal_y, 0.01, 0.05, "num") ,
                       getNumPeaksBy(signal_z, 0.01, 0.05, "num") 
                       )
           }
         },
         prominentAuto={
           if(avg==TRUE){
             signal <- getAutocorrelationSignal(window_data$avg, startLag)
             res <- c( getNumPeaksBy(signal, 0.01, 0.1, "peaks", "prominent") )
             
           }else{
             signal_x <- getAutocorrelationSignal(window_data$x, startLag)
             signal_y <- getAutocorrelationSignal(window_data$y, startLag)
             signal_z <- getAutocorrelationSignal(window_data$z, startLag)
             res <- c( getNumPeaksBy(signal_x, 0.01, 0.1, "peaks", "prominent"), # 0.05 -> 0.1
                       getNumPeaksBy(signal_y, 0.01, 0.1, "peaks", "prominent"),
                       getNumPeaksBy(signal_z, 0.01, 0.1, "peaks", "prominent")
                       )
           }
         },
         weakpeakAuto={
           if(avg==TRUE){
             signal <- getAutocorrelationSignal(window_data$avg, startLag)
             res <- c( getNumPeaksBy(signal, 0.01, 0.1, "peaks", "weak") )
             
           }else{
             signal_x <- getAutocorrelationSignal(window_data$x, startLag)
             signal_y <- getAutocorrelationSignal(window_data$y, startLag)
             signal_z <- getAutocorrelationSignal(window_data$z, startLag)
             res <- c( getNumPeaksBy(signal_x, 0.01, 0.1, "peaks", "weak"),
                       getNumPeaksBy(signal_y, 0.01, 0.1, "peaks", "weak"),
                       getNumPeaksBy(signal_z, 0.01, 0.1, "peaks", "weak")
                       )
           }
         },
         maximumAuto={
           if(avg==TRUE){   
             signal <- getAutocorrelationSignal(window_data$avg, startLag)
             res <- c( max(signal) )
           }else{
             signal_x <- getAutocorrelationSignal(window_data$x, startLag)
             signal_y <- getAutocorrelationSignal(window_data$y, startLag)
             signal_z <- getAutocorrelationSignal(window_data$z, startLag)
             res <- c( max(signal_x), max(signal_y), max(signal_z) )
           }
         },
         powerband={
           if(avg == TRUE){
             res <- c( getPowerBandBy(window_data$avg, powerband_from, powerband_to, sampling.rate = sampling.rate) )
           }else {
             res <- c( getPowerBandBy(window_data$x, powerband_from, powerband_to, sampling.rate = sampling.rate),
                       getPowerBandBy(window_data$y, powerband_from, powerband_to, sampling.rate = sampling.rate),
                       getPowerBandBy(window_data$z, powerband_from, powerband_to, sampling.rate = sampling.rate)
             )
           }
         },
         height1stAuto={
           if(avg == TRUE){
             signal <- getAutocorrelationSignal(window_data$avg, startLag)             
             res <- c( getFirstHeightBy(signal) )
           }else {
             signal_x <- getAutocorrelationSignal(window_data$x, startLag)
             signal_y <- getAutocorrelationSignal(window_data$y, startLag)
             signal_z <- getAutocorrelationSignal(window_data$z, startLag)
             
             res <- c( getFirstHeightBy(signal_x), getFirstHeightBy(signal_y), getFirstHeightBy(signal_z) )
           }
         },
         SD={
           if(avg == TRUE){
             res <- c( sd(window_data$avg) )
           }else{
             res <- c(sd(window_data$x),sd(window_data$y),sd(window_data$z))
           }
         },
         RMS = {
           if(avg == TRUE){
             res <- c( sqrt(sum(window_data$avg^2)/length(window_data$avg)) )
           }else{
             res <- c( sqrt(sum(window_data$x^2)/length(window_data$x)), sqrt(sum(window_data$y^2)/length(window_data$y)) ,
                       sqrt(sum(window_data$z^2)/length(window_data$z)) )
           }
         },
         integratedRMS = {
           if(avg == TRUE){
             signal <- getIntegrateSignal(window_data$avg)
             res <- c( sqrt(sum(signal^2)/length(signal)) )
           }else{
             signal_x <- getIntegrateSignal(window_data$x)
             signal_y <- getIntegrateSignal(window_data$y)
             signal_z <- getIntegrateSignal(window_data$z)
             res <- c( sqrt(sum(signal_x^2)/length(signal_x)), sqrt(sum(signal_y^2)/length(signal_y)) ,
                       sqrt(sum(signal_z^2)/length(signal_z)) )
           }
         },
         energyFrom_0to10Hz = {
           if(avg == TRUE){
             avgV = trans_to_frequency(window_data$avg)
             
             n = length(window_data$x)
             #sampling.rate = 50 
             nUniquePts <- ceiling((n+1)/2)
             freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
             
             sum <- 0
             for(i in 1:length(freqArray))
             {
               if(freqArray[i]>10){
                 #print(paste("i",i))
                 break;
               }
               else
                 sum <- sum + avgV[i]
             }
             
             res <- c(sum)
           }else{          
             Px = trans_to_frequency(window_data$x - mean(window_data$x) )
             Py = trans_to_frequency(window_data$y - mean(window_data$y) )
             Pz = trans_to_frequency(window_data$z - mean(window_data$z) )
             
             n = length(window_data$x)
             #sampling.rate = 50 
             nUniquePts <- ceiling((n+1)/2)
             freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
             
             sumX <- 0; sumY <- 0; sumZ <- 0;
             for(i in 1:length(freqArray))
             {
               if(freqArray[i]>10){
                 #print(paste("i",i))
                 break;
               }
               else{
                 sumX <- sumX + Px[i]; sumY <- sumY + Py[i]; sumZ <- sumZ + Pz[i];
               }
             }
             res <- c(sumX, sumY, sumZ)
           }
         },         
         sum_abs = {
           if(avg == TRUE){
             res <- c( sum(abs(window_data$avg) ))
           }else{
             res <- c( sum( abs(window_data$x) ), sum( abs(window_data$y) ),
                       sum( abs(window_data$z) ) )
           }
         },
         threshold={
           g<- 9.81
           #plot(window_data$x, type='l')
           if(avg == TRUE){
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
             res <- c(trans_to_frequency(window_data$avg,"maxfreq"))
           }else{
             res <- c(trans_to_frequency(window_data$x,"maxfreq"), trans_to_frequency(window_data$y,"maxfreq"), trans_to_frequency(window_data$z,"maxfreq"))
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
           
           if(avg==TRUE){
             Px = trans_to_frequency(window_data$avg)       
             n = length(window_data$x)
             sampling.rate = 50 
             nUniquePts <- ceiling((n+1)/2)
             freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
             
             df <- data.frame(fre = freqArray, perX = Px)  
             
             res <- c( getPeakRatioFeature(df$perX,df$fre, 0.005,1,FALSE) )
           }else{
             Px = trans_to_frequency(window_data$x )
             Py = trans_to_frequency(window_data$y )
             Pz = trans_to_frequency(window_data$z )
             
             n = length(window_data$x)
             sampling.rate = 50 
             nUniquePts <- ceiling((n+1)/2)
             freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
             
             df <- data.frame(fre = freqArray, perX = Px, perY = Py, perZ = Pz)  
             #View(df)
             res <- c(getPeakRatioFeature(df$perX,df$fre, 0.005,1,FALSE),getPeakRatioFeature(df$perY,df$fre, 0.005,1,FALSE),getPeakRatioFeature(df$perZ,df$fre, 0.005,1,FALSE))
           }
         },
         autocorrelation={
           if(avg==TRUE){
             res <- c( getAutocorrelation(window_data$avg,opt_lag) )
           }else{
             res <- c(getAutocorrelation(window_data$x,opt_lag), getAutocorrelation(window_data$y,opt_lag), getAutocorrelation(window_data$z,opt_lag))             
           }
         },
         autocorrelationAbs={
           if(avg==TRUE){
             res <- c( abs(getAutocorrelation(window_data$avg,opt_lag) ) )
           }else{
             res <- c(abs(getAutocorrelation(window_data$x,opt_lag)), abs(getAutocorrelation(window_data$y,opt_lag)), abs(getAutocorrelation(window_data$z,opt_lag)))             
           }
         },
         autocorrelationV2={ # return max autocorrelation
           if(avg==TRUE){
             res <- c(getAutocorrelationV2(window_data$avg))
           }
           else{
             res <- c(getAutocorrelationV2(window_data$x), getAutocorrelationV2(window_data$y), getAutocorrelationV2(window_data$z))
           }
         },
         mean={
           if(avg == TRUE){
             res <- c(mean(window_data$avg))
           }
           else
             res <- c(mean(window_data$x),mean(window_data$y),mean(window_data$z))
         },
         max={
           if(avg == TRUE){
             res <- c(max(window_data$avg))
           }else
             res <- c(max(window_data$x),max(window_data$y),max(window_data$z))
         },
         min={
           if(avg == TRUE){
             res <- c(min(window_data$avg))
           }else
             res <- c(min(window_data$x),min(window_data$y),min(window_data$z))
         }
         ,mintomax={
           if(avg == TRUE){
             res <- c( abs(max(window_data$avg)-min(window_data$avg)) )
           }else
             res <- c( abs(max(window_data$x)-min(window_data$x)), abs(max(window_data$y)-min(window_data$y)),
                       abs(max(window_data$z)-min(window_data$z)) )
         },
         variance={
           if(avg == TRUE){
             res <- c( var(window_data$avg) )
           }else{
             res <- c(var(window_data$x),var(window_data$y),var(window_data$z))
           }
         },
         correlation={
           res <- c(cor(window_data$x, window_data$y),cor(window_data$x, window_data$z),cor(window_data$y, window_data$z))
         },
         energy={
           if(avg == TRUE){
             data<-window_data
             ## TODO : mean acceleration 뺀다고??? 왜??
             y <- data$avg - mean(data$avg)
             y <- fft(y)
             y <- Mod(y)^2
             SOSM_avg <- (sum(y))/(length(y))
             res <- c(SOSM_avg)
           }
           else{
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
           }
           #print(paste("energy",res))
         },
         entropy={
           if(avg == TRUE)
           {
             data<-window_data
             y <- data$avg - mean(data$avg)
             y <- fft(y)
             y <- Mod(y)^2
             energySum <- sum(y)
             y <- y/energySum
             e <- -1*y*log2(y+0.000001)         
             entropy_avg <- sum(e)
             if(is.na(entropy_avg)){
               View(y)
               View(e)
             }
             
             res <- c(entropy_avg)
           }else{
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
           }
         }
  )
  
  #print(paste("feature Type ", feature_type, " opt_lag = ", opt_lag, ", res ", res))
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

