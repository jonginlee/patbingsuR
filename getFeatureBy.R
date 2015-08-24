
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
]


createFrequencyBy <- function(adata,graph_title="no")
{
  Px <- trans_to_frequency(adata - mean(adata))
  n = length(adata)
  sampling.rate = 50
  nUniquePts <- ceiling((n+1)/2)
  freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
  df <- data.frame(fre = freqArray, perX = Px)
  getPeakRatioFeature(df$perX,df$fre, 0.005,1,TRUE,title=graph_title) 
}

getPeakRatioFeature<-function(power, freq, cutoff=0.005, type=1, ploting=FALSE, title="no",  neigh_th = 0, min_th = 0)
{
  
  cutoff<-0.0005
  p <- findPeaks(power,thresh = neigh_th)
  p <- p-1
  maxV <- 0
  maxP <- -1
  #View(power)
  #View(p)
  if(length(p)!=0)
  {
    for(i in 1:length(p))
    {
      if(power[p[i]]>maxV){
        maxP <- p[i]
        maxV <- power[p[i]]
      }
    }
  }
  
  minThreshold <- as.double(maxV)*0.9
  cntDominant <- 0;
  #print(paste("minTh",minThreshold))
  
#  if((maxP>=0) & (length(p)!=0) )
#  {
#    for(i in 1:length(p))
#    {
#      if( (power[p[i]] > 0) & (power[p[i]]>minThreshold) ){
#        cntDominant <- cntDominant + 1
#      }
#    }
#  }
  testsum <- 0
  if(length(p) == 0){
   # print("her")
  }else{
  
  #  print(freq)
  for(i in 1:length(p))
    {
      if((freq[p[i]] > 1) & (freq[p[i]] <= 5) & (power[p[i]] > min_th) )
      {
        cntDominant <- cntDominant + 1
        testsum <- testsum + power[p[i]]
      }
    }
  }

  cntWeak <- length(p) - cntDominant

#  print(paste("prominant",cntDominant,"testsum",testsum, "cntWeak",cntWeak ))
  

  if(maxP>=0){
    if(freq[maxP-1]==0){
      rect <- data.frame(xmin=freq[maxP-1], xmax=freq[maxP+2], ymin=-Inf, ymax=Inf)
      x <- c(freq[maxP-1], freq[maxP], freq[maxP+1], freq[maxP+2])
      y <- c(power[maxP-1], power[maxP], power[maxP+1], power[maxP+2])
    }else{
      rect <- data.frame(xmin=freq[maxP-2], xmax=freq[maxP+2], ymin=-Inf, ymax=Inf)
      x <- c(freq[maxP-2], freq[maxP-1], freq[maxP], freq[maxP+1], freq[maxP+2])
      y <- c(power[maxP-2], power[maxP-1], power[maxP], power[maxP+1], power[maxP+2])
    }
  }
  
  
  A <- integrateTrapezoid(x, y)
  left <- integrateTrapezoid(freq, power)
  peakSum <- sum(power[p])
  #totalSum <- sum(power)
  
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
  }else if(type==3){
    returnValue <- cntDominant
  }else if(type==4){
    returnValue <- cntWeak
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

getNumCrossingRate<-function(signal)
{
  cnt<-0
  if(any(is.na(signal))){
    return (0)
  }
  if(length(signal)<2)
    return (0)
  
  for(i in 2:length(signal))
  {
    if(signal[i]*signal[i-1]<0)
      cnt<-cnt+1
  }
  return(cnt/(length(signal)-1))
}



getBestCase<-function(arrays, feature_type)
{
  res <- 0
  switch(feature_type,
         correlation={
           count = 0;
           for(i in 1:length(arrays)){
             #print(paste(i,arrays[i]))
             if(abs(as.double(arrays[i])) > 0.6)
               count <- count + 1;
           }
           res<-count
         },
         entropy={
           res <- min(arrays)
         },
         energy={
           res <- max(arrays)
         },
         peakfreq={
           res <- max(arrays)
         },
         RMS={
           res <- min(arrays)
         },
         maximumAuto={
           res <- max(arrays)
         },
         zerocrossingrate={
           res <- max(arrays)
         },
         height1stPeakValleyAuto={
           res <- max(arrays)
         },
         prominentAutoPeakValley={
           res <- max(arrays)
         },
         weakpeakAutoPeakValley={
           res <- max(arrays)
         },
         getProminantPeakfreq={
           res <- mean(arrays)
         },
         getWeakPeakfreq={
           res <- mean(arrays)
         },
         powerband = {
           res <- max(arrays)
         },
         harmPeak = {
           res <- max(arrays)
         }
         
         
  )

  
  return(res)
}



getFeatureBy <- function(window_data, feature_type, opt_lag = 12, avg = FALSE, thr = 0.01, type = "mag", sampling.rate = 50,
                         powerband_from=0, powerband_to=0, startLag=26, f_l=20, filtering=FALSE, selection=TRUE , filter_num =1, spanV = 0.4,
                         neith_th = 0.01,  min_th = 0.01, max_th = 0.01, b_avg = FALSE, doublecnt = FALSE, signal_type ="no_signal" ,filter_type ="low", 
                         order =5, window_data_prev = NULL, filtering2 = FALSE
                         )
{
 
  #print(paste("feature_type", feature_type))
  #window_data$x <- window_data$x - mean(window_data$x)
  #window_data$y <- window_data$y - mean(window_data$y)
  #window_data$z <- window_data$z - mean(window_data$z)
  
  


  
  res<-0
  if(avg == TRUE){
    
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
    
    if(signal_type=="autocorrelation")
    {
      signal <- getAutocorrelationSignal(window_data$avg, startLag)
      if(doublecnt!=FALSE)
      {
        for(i in 1:doublecnt){
          signal <- getAutocorrelationSignal(signal, 1)
        }
      }
      
      window_data <- data.frame( avg = signal, time = window_data$time[startLag:length(window_data$time)] )
      
    }
    
    # TODO filtering2 ==>
    # TODO filtering3 ==>
    
  }else{
    if( (signal_type == "autocorrelation") & (filtering2 == FALSE) )
    {
      signal_x <- getAutocorrelationSignal(window_data$x, startLag)
      signal_y <- getAutocorrelationSignal(window_data$y, startLag)
      signal_z <- getAutocorrelationSignal(window_data$z, startLag)
      if(doublecnt!=FALSE)
      {
        for(i in 1:doublecnt){
          signal_x <- getAutocorrelationSignal(signal_x, 1)
          signal_y <- getAutocorrelationSignal(signal_y, 1)
          signal_z <- getAutocorrelationSignal(signal_z, 1)
        }
      }
      
      window_data <- data.frame( x = signal_x, y = signal_y, z = signal_z, time = window_data$time[startLag:length(window_data$time)])
      
    }
    
    if( (signal_type == "autocorrelation") & (filtering2 == TRUE) & (length(window_data_prev)!=0) )
    {
      signal_x_prev <- getAutocorrelationSignal(window_data_prev$x, startLag)
      signal_x <- getAutocorrelationSignal(window_data$x, startLag)      
      signal_x <- getCrosscorrelationSignal( signal_x_prev, signal_x, startLag = 1 )

      
      signal_y_prev <- getAutocorrelationSignal(window_data_prev$y, startLag)
      signal_y <- getAutocorrelationSignal(window_data$y, startLag)      
      signal_y <- getCrosscorrelationSignal( signal_y_prev, signal_y, startLag = 1 )
      
      signal_z_prev <- getAutocorrelationSignal(window_data_prev$z, startLag)
      signal_z <- getAutocorrelationSignal(window_data$z, startLag)      
      signal_z <- getCrosscorrelationSignal( signal_z_prev, signal_z, startLag = 1 )

      window_data <- data.frame( x = signal_x, y = signal_y, z = signal_z, time = window_data$time[startLag:length(window_data$time)])
      
    }
    
    
    if(filtering == TRUE)
    {
      if(filtering & (filter_num == 1)){
        bf2 <- butter(order, (2*f_l)/(sampling.rate), type=filter_type)
        window_data$x <- filtfilt( bf2, window_data$x)
        window_data$y <- filtfilt( bf2, window_data$y)
        window_data$z <- filtfilt( bf2, window_data$z)
      }else if(filtering & (filter_num == 2))
      {
        df <- data.frame(time_hour = window_data$time, x = window_data$x, y = window_data$y, z = window_data$z )
        window_data$x <- predict(loess(x~time_hour, df, span = spanV), df$time_hour)
        window_data$y <- predict(loess(y~time_hour, df, span = spanV), df$time_hour)
        window_data$z <- predict(loess(z~time_hour, df, span = spanV), df$time_hour)
      }else if(filtering & (filter_num == 3))
      {
        df <- data.frame(time_hour = window_data$time, x = window_data$x, y = window_data$y, z = window_data$z )
        sx <- predict(smooth.spline(df$time_hour, df$x, spar = spanV), df$time_hour)
        sy <- predict(smooth.spline(df$time_hour, df$y, spar = spanV), df$time_hour)
        sz <- predict(smooth.spline(df$time_hour, df$z, spar = spanV), df$time_hour)
        #print(paste("x spar ", smooth.spline(df$time_hour, df$x, cv=T)$spar ))
        #print(paste("y spar ", smooth.spline(df$time_hour, df$y, cv=T)$spar ))
        #print(paste("z spar ", smooth.spline(df$time_hour, df$z, cv=T)$spar ))
        
        window_data$x <- sx$y
        window_data$y <- sy$y
        window_data$z <- sz$y
      }else if(filtering & (filter_num == 4))
      {
        df <- data.frame(time_hour = window_data$time, x = window_data$x, y = window_data$y, z = window_data$z )
        window_data$x <- movingAverage(df$x, spanV, TRUE)
        window_data$y <- movingAverage(df$y, spanV, TRUE)
        window_data$z <- movingAverage(df$z, spanV, TRUE)
        
      }
    }
  }
  
  

  switch(feature_type,      
         harmValley={
           if(avg==TRUE){
             res <- c(getHarm(window_data$avg, window_data$time, type = "valley"))
           }else{
             res <- c(getHarm(window_data$x, window_data$time, type = "valley"),getHarm(window_data$y, window_data$time, type = "valley"),
                      getHarm(window_data$z, window_data$time, type = "valley"))
           }
         },
         harmPeak={
           if(avg==TRUE){
             res <- c(getHarm(window_data$avg, window_data$time))
           }else{
             res <- c(getHarm(window_data$x, window_data$time),getHarm(window_data$y, window_data$time),getHarm(window_data$z, window_data$time))
           }
           if(b_avg==TRUE)
             res <- getBestCase(res, feature_type)
         },
         VarPeakSpacing={
           if(avg==TRUE){
             res <- c( getVarPeakSpacing(window_data$avg,  time =  window_data$time, thresh = 0) )
           }else{
             res <- c( getVarPeakSpacing(window_data$x,time =  window_data$time, thresh = 0) ,
                       getVarPeakSpacing(window_data$y,time =  window_data$time, thresh = 0) ,
                       getVarPeakSpacing(window_data$z,time =  window_data$time, thresh = 0) 
             )
           }
         },
         maximumCepstrumPeak={
           if(avg==TRUE){   
             signal <- as.double(ifft(log(abs(fft(window_data$avg)))))
             if(any(is.na(signal)))
               res <- c(0)
             else{
               if(length(signal)<=1)
                 max_x <- 0
               else
                 max_x <- max((signal[2:length(signal)]))
               
              res <- c(max_x)
             }
           }else{
             signal_x <- as.double(ifft(log(abs(fft(window_data$x)))))
             signal_y <- as.double(ifft(log(abs(fft(window_data$y)))))
             signal_z <- as.double(ifft(log(abs(fft(window_data$z)))))
             if(any(is.na(signal_x), is.na(signal_y), is.na(signal_z)))
               res <- c(0,0,0)
             else{ 
               if(length(signal_x)<=1)
                 max_x <- 0
               else
                 max_x <- max((signal_x[2:length(signal_x)]))
               
               if(length(signal_x)<=1)
                 max_y <- 0
               else
                 max_y <- max((signal_y[2:length(signal_y)]))
               
               if(length(signal_z)<=1)
                 max_z <- 0
               else
                 max_z <-  max((signal_z[2:length(signal_z)]))
               
               res <- c( max_x, max_y, max_z )
             }


           }
         },
         prominentCepstrumPeak={
           if(avg == TRUE){
             signal <- as.double(ifft(log(abs(fft(window_data$avg)))))
             if(any(is.na(signal)))
               res <- c(0)
             else
              res <- c( getNumPeaksBy(signal, neith_th, min_th, "peaks", "prominent") )
           }
           else{
             signal_x <- as.double(ifft(log(abs(fft(window_data$x)))))
             signal_y <- as.double(ifft(log(abs(fft(window_data$y)))))
             signal_z <- as.double(ifft(log(abs(fft(window_data$z)))))
             if(any(is.na(signal_x), is.na(signal_y), is.na(signal_z)))
               res <- c(0,0,0)
             else
               res <- c( getNumPeaksBy(signal_x, neith_th, min_th, "peaks", "prominent"), # 0 / 0.1
                       getNumPeaksBy(signal_y, neith_th, min_th, "peaks", "prominent"),
                       getNumPeaksBy(signal_z, neith_th, min_th, "peaks", "prominent")
                     )
           }
           
         },
         weakCepstrumPeak={
           if(avg == TRUE){
             signal <- as.double(ifft(log(abs(fft(window_data$avg)))))
             if(any(is.na(signal)))
               res <- c(0)
             else
               res <- c( getNumPeaksBy(signal, neith_th, max_th, "peaks", "weak") )
           }
           else{
             signal_x <- as.double(ifft(log(abs(fft(window_data$x)))))
             signal_y <- as.double(ifft(log(abs(fft(window_data$y)))))
             signal_z <- as.double(ifft(log(abs(fft(window_data$z)))))
             
             if(any(is.na(signal_x), is.na(signal_y), is.na(signal_z)))
               res <- c(0,0,0)
             else
               res <- c( getNumPeaksBy(signal_x, neith_th, max_th, "peaks", "weak"),
                       getNumPeaksBy(signal_y, neith_th, max_th, "peaks", "weak"),
                       getNumPeaksBy(signal_z, neith_th, max_th, "peaks", "weak")
               )
           }
           
         },
         numberCepstrumPeak={
           if(avg == TRUE){
             signal <- as.double(ifft(log(abs(fft(window_data$avg)))))
             if(any(is.na(signal)))
               res <- c(0)
             else
               res <- c( getNumPeaksBy(signal, neith_th, min_th, "num") )
           }
           else{
             signal_x <- as.double(ifft(log(abs(fft(window_data$x)))))
             signal_y <- as.double(ifft(log(abs(fft(window_data$y)))))
             signal_z <- as.double(ifft(log(abs(fft(window_data$z)))))
             
             if(any(is.na(signal_x), is.na(signal_y), is.na(signal_z)))
               res <- c(0,0,0)
             else
              res <- c( getNumPeaksBy(signal_x, neith_th, min_th, "num"), # 0.05, 0.05
                       getNumPeaksBy(signal_y, neith_th, min_th, "num"),
                       getNumPeaksBy(signal_z, neith_th, min_th, "num")
              )
           }
           
         },
         zerocrossingrateCepstrum={
           if(avg == TRUE){
             signal <- as.double(ifft(log(abs(fft(window_data$avg)))))
             if(any(is.na(signal)))
               res <- c(0)
             else
               res <- c( getNumCrossingRate(signal) )
           }
           else{
             signal_x <- as.double(ifft(log(abs(fft(window_data$x)))))
             signal_y <- as.double(ifft(log(abs(fft(window_data$y)))))
             signal_z <- as.double(ifft(log(abs(fft(window_data$z)))))
             
             if(any(is.na(signal_x), is.na(signal_y), is.na(signal_z)))
               res <- c(0,0,0)
             else
              res <- c( getNumCrossingRate(signal_x), getNumCrossingRate(signal_y), getNumCrossingRate(signal_z) )

           }
           
         },
         zerocrossingrate={
           if(avg == TRUE){
             res <- c( getNumCrossingRate(window_data$avg) )
           }else {
             res <- c( getNumCrossingRate(window_data$x), getNumCrossingRate(window_data$y), getNumCrossingRate(window_data$z) )
             if(b_avg==TRUE)
               res <- getBestCase(res, feature_type)
           }
         },
         height1stPeakValleyAuto={
           if(avg == TRUE){
             res <- c( getFirstHeightBy(window_data$avg, type="range") )
           }else {             
             res <- c( getFirstHeightBy(window_data$x, type="range"), getFirstHeightBy(window_data$y,type="range"), getFirstHeightBy(window_data$z,type="range") )
             if(b_avg==TRUE)
               res <- getBestCase(res, feature_type)
           }
         },
         prominentAutoPeakValley={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg, neith_th, min_th, "peakvalley", "prominent") )          
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, min_th, "peakvalley", "prominent"), # 0.05 -> 0.1
                       getNumPeaksBy(window_data$y, neith_th, min_th, "peakvalley", "prominent"),
                       getNumPeaksBy(window_data$z, neith_th, min_th, "peakvalley", "prominent")
             )
             if(b_avg==TRUE)
               res <- getBestCase(res, feature_type)

           }
         },
         weakpeakAutoPeakValley={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg, neith_th, max_th, "peakvalley", "weak") )
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, max_th, "peakvalley", "weak"),
                       getNumPeaksBy(window_data$y, neith_th, max_th, "peakvalley", "weak"),
                       getNumPeaksBy(window_data$z, neith_th, max_th, "peakvalley", "weak")
             )
             if(b_avg==TRUE)
               res <- getBestCase(res, feature_type)
           }
         },
         getProminantPeakfreq = {
           if(avg==TRUE){
             Px = trans_to_frequency(window_data$avg)       
             n = length(window_data$x)
             sampling.rate = 50 
             nUniquePts <- ceiling((n+1)/2)
             freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
             
             df <- data.frame(fre = freqArray, perX = Px)  
             
             res <- c( getPeakRatioFeature(df$perX,df$fre, 0.005,3,FALSE,neigh_th = neith_th, min_th = min_th) )
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
             res <- c(getPeakRatioFeature(df$perX,df$fre, 0.005,3,FALSE,neigh_th = neith_th, min_th = min_th),getPeakRatioFeature(df$perY,df$fre, 0.005,3,FALSE,neigh_th = neith_th, min_th = min_th),getPeakRatioFeature(df$perZ,df$fre, 0.005,3,FALSE,neigh_th = neith_th, min_th = min_th))
             if(b_avg==TRUE)
             {
               res <- getBestCase(res, feature_type)
             }
           }
         },
         getWeakPeakfreq = {
           if(avg==TRUE){
             Px = trans_to_frequency(window_data$avg)   
             n = length(window_data$x)
             sampling.rate = 50 
             nUniquePts <- ceiling((n+1)/2)
             freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
             
             df <- data.frame(fre = freqArray, perX = Px,neigh_th = neith_th, min_th = min_th)  
             
             res <- c( getPeakRatioFeature(df$perX,df$fre, 0.005,4,FALSE) )
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
             res <- c(getPeakRatioFeature(df$perX,df$fre, 0.005,4,FALSE,neigh_th = neith_th, min_th = min_th),getPeakRatioFeature(df$perY,df$fre, 0.005,4,FALSE,neigh_th = neith_th, min_th = min_th),getPeakRatioFeature(df$perZ,df$fre, 0.005,4,FALSE,neigh_th = neith_th, min_th = min_th))
             if(b_avg==TRUE)
             {
               res <- getBestCase(res, feature_type)
             }
           }
         },
         autoFirstRange={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg, neith_th, min_th, "num",calType = "sum") )
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, min_th, "num",calType = "sum") ,
                       getNumPeaksBy(window_data$y, neith_th, min_th, "num",calType = "sum") ,
                       getNumPeaksBy(window_data$z, neith_th, min_th, "num",calType = "sum") 
             )
           }
         },
         peaknumAutoSum={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg, neith_th, min_th, "num",calType = "sum") )
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, min_th, "num",calType = "sum") ,
                       getNumPeaksBy(window_data$y, neith_th, min_th, "num",calType = "sum") ,
                       getNumPeaksBy(window_data$z, neith_th, min_th, "num",calType = "sum") 
             )
           }
         },
         prominentAutoSum={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg, neith_th, min_th, "peaks", "prominent",calType = "sum") )            
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, min_th,"peaks", "prominent",calType = "sum"), # 0.05 -> 0.1 (잘 안나옴)
                       getNumPeaksBy(window_data$y, neith_th, min_th, "peaks", "prominent",calType = "sum"),
                       getNumPeaksBy(window_data$z, neith_th, min_th, "peaks", "prominent",calType = "sum")
             )
           }
         },
         weakpeakAutoSum={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg, neith_th, min_th, "peaks", "weak",calType = "sum") )        
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, max_th, "peaks", "weak",calType = "sum"),
                       getNumPeaksBy(window_data$y, neith_th, max_th, "peaks", "weak",calType = "sum"),
                       getNumPeaksBy(window_data$z, neith_th, max_th, "peaks", "weak",calType = "sum")
             )
           }
         },
         peaknumAuto={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg,  neith_th, min_th, "num") )
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, min_th, "num") ,
                       getNumPeaksBy(window_data$y, neith_th, min_th, "num") ,
                       getNumPeaksBy(window_data$z, neith_th, min_th, "num") 
                       )
           }
         },
         prominentAuto={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg,  neith_th, min_th, "peaks", "prominent") )          
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, min_th, "peaks", "prominent"), # 0.05 -> 0.1
                       getNumPeaksBy(window_data$y, neith_th, min_th, "peaks", "prominent"),
                       getNumPeaksBy(window_data$z, neith_th, min_th, "peaks", "prominent")
                       )
           }
         },
         weakpeakAuto={
           if(avg==TRUE){
             res <- c( getNumPeaksBy(window_data$avg,  neith_th, min_th, "peaks", "weak") )
           }else{
             res <- c( getNumPeaksBy(window_data$x, neith_th, max_th, "peaks", "weak"),
                       getNumPeaksBy(window_data$y, neith_th, max_th, "peaks", "weak"),
                       getNumPeaksBy(window_data$z, neith_th, max_th, "peaks", "weak")
                       )
           }
         },
         maximumAuto={
           if(avg==TRUE){   
             res <- c( max(window_data$avg) )
           }else{
             res <- c( max(abs(window_data$x) ), max(abs(window_data$y)), max(abs(window_data$z) ) )
             if(b_avg==TRUE)
             {
               res <- getBestCase(res, feature_type)
             }
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
             if(b_avg==TRUE)
               res <- getBestCase(res, feature_type)
           }
         },
         height1stAuto={
           if(avg == TRUE){
             res <- c( getFirstHeightBy(window_data$avg) )
           }else {             
             res <- c( getFirstHeightBy(window_data$x), getFirstHeightBy(window_data$y), getFirstHeightBy(window_data$z) )
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
             if(b_avg==TRUE)
               res <- getBestCase(res, feature_type)
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
             res <- c( getCth(window_data$avg - mean(window_data$avg),g*thr) )
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
             if(b_avg==TRUE)
             {
               res <- getBestCase(res, feature_type)
             }
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
            
           if(b_avg == TRUE)
           {
             res <- getBestCase(res,  feature_type)
           }
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
             if(b_avg==TRUE)
             {
               res <- getBestCase(res, feature_type)
             }
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
             
             if(b_avg==TRUE)
             {
               res <- getBestCase(res, feature_type)
             }
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

