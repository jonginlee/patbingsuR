# Created by jonginlee on 16. 10. 04.


autolabeling <- function(filename, labelname, a_label = FALSE )
{
  data_label<-read.csv(file=paste("./data_csv/feature_extracted_",filename,".csv",sep=""))
  data_label$label <- as.character(data_label$label)
  
  if(a_label == FALSE )
  {
    scturn <- FALSE
    
    for(i in 1:length(data_label$label))
    {
      if( (scturn == FALSE) & (data_label$label[i] == "TODO") )
        data_label$label[i] <- "non"
      else if( (scturn == TRUE) & (data_label$label[i] == "TODO") )
        data_label$label[i] <- labelname
      else if( (data_label$label[i] == "sleep") ){
        if((i!=1))
        {
          if((data_label$label[i-1] != "sleep"))
            scturn <- !scturn
        }
      }
    }
  }else{
    for(i in 1:length(data_label$label))
    {
      if( data_label$label[i] == "TODO" )
        data_label$label[i] <- labelname
    }
  }
  
  data_label$label<-as.factor(data_label$label)
  print(" * autolabeling .. success!!")
  
  #View(data_label)
  
  return(data_label)
}


sumData<-function(data1, data2,filter)
{
  data_sub1 <- subset(data1, grepl(filter, data1$label))
  data_sub2 <- subset(data2, grepl(filter, data2$label))
  rs <- rbind(data_sub1,data_sub2)
  return(rs)
}

getWindow <- function(data, start_index, window_size)
{
  if(start_index+window_size > nrow(data)){
    window_size <- (nrow(data) - start_index)
  }
  window <- data[c(start_index:(start_index+window_size)),]
  return (window)  
}


getHMS <- function(time)
{
  hour <- as.integer(time)
  minute_t <- (time - as.integer(time))*60
  minute <- as.integer(minute_t)
  second_t <- (minute_t - as.integer(minute_t))*60
  second <- as.integer(second_t)
  
  return ( paste(hour,minute,second))
}

#### feature fun

getIntegrateSignal <- function(signal)
{
  require("oce")
  res<-0
  for(i in 1:length(signal))
  {
    xrange <- c(1:i)
    yrange <- signal[1:i]
    res[i]<-integrateTrapezoid(xrange, yrange)
  }
  
  return(res)
}

trans_to_frequency<-function(s1, opt = "no_maxfreq")
{
  n <- length(s1)
  p <- fft(s1)
  nUniquePts <- ceiling((n+1)/2)
  p <- p[1:nUniquePts] 
  #select just the first half since the second half 
  # is a mirror image of the first
  p <- abs(p)  #take the absolute value, or the magnitude 
  p <- p / n #scale by the number of points so that
  # the magnitude does not depend on the length 
  # of the signal or on its sampling frequency  
  p <- p^2  # square it to get the power 
  
  # multiply by two (see technical document for details)
  # odd nfft excludes Nyquist point
  if (n %% 2 > 0){
    p[2:length(p)] <- p[2:length(p)]*2 # we've got odd number of points fft
  } else {
    p[2: (length(p) -1)] <- p[2: (length(p) -1)]*2 # we've got even number of points fft
  }
  
  sampling.rate <- 50
  freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n) #  create the frequency array 
  #View(p)
  
  if( opt == "maxfreq")
  {
    max<-0
    max_i <-0
    for(i in 1:length(p))
    {
      if(p[i]>max)
      {
        max <- p[i]
        max_i <- i
      }
    }
    #print(paste("max_i",max_i,"maxFreq",p[max_i],"maxHz",freqArray[max_i]))
    return (freqArray[max_i])
  }else{
    return (p)    
  }
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
  for(i in 1:(N))
  {
    sum2 <- sum2 + (data[i]-u)^2
  }
  #  sum <- sum/(N-lag)
  res <- sum/sum2
  
  return (res)
}


