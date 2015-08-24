createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 18302649+2000, 18302649+2000+3000, avg = F,type = "mag",graph_title ="머리 넘김" , filtering = T, f_l = 5) # 
createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 23426393+4000, 23426393+4000+3000, avg = F,type = "mag",graph_title ="똑바로 눕고 뒤척임", filtering = T, f_l = 5) # 똑바로 눕고 뒤척임 
createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 13148695 + 2000 , 13148695 +5000, avg = F,type = "mag",graph_title="오른쪽으로 누움" , threshold = 0.01, peak_threshold = 0.1, filtering = T, f_l = 5) # 오른쪽으로 누움
createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 14119837+ 2000 , 14119837 +5000, avg = F,type = "mag",graph_title="이마 넘기기", threshold = 0.01, peak_threshold = 0.1, filtering = T, f_l = 5) # 이마 넘기기
createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 15221856 , 15221856 +3000, avg = F,type = "mag",graph_title="팔올리기", filtering = T, f_l = 5) # 팔올리기 

res <- createPlot(subject3_left_data, 8, "stretching", TRUE, "p3",TRUE, 18302649-10000, 18302649+10000)


#,threshold = 0.3, peak_threshold = 0.1

createAutocorrelationSignal(subject3_left_data, 8, 25, TRUE, 6483892-1000, 6483892+2000, avg = F,type = "mag" , graph_title="손등 긁기",filtering = T,f_l=5) # 손등 긁기 
createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 21752813 + 3000,  21752813 + 3000 + 3000, avg = F,type = "mag", graph_title = "얼굴 긁기",filtering = TRUE,f_l=0.5) # 얼굴 긁기 
createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 19226500 , 19226500 + 3000, avg = F,type = "mag", graph_title = "오른쪽 이마 긁기" ,filtering = T,f_l=20) # 오른쪽 이마 긁기
createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 22549000 , 22549000 + 3000, avg = F,type = "mag", graph_title = "오른쪽 볼 긁기 ",filtering = TRUE,f_l=0.5) # 오른쪽 볼 긁기 
createAutocorrelationSignal(subject3_left_data, 8, 2, TRUE, 22878810-1000 , 22878810  +2000, avg = F,type = "mag",graph_title="이마? 얼굴 긁기",filtering = TRUE,f_l=0.5) # 이마? 얼굴 긁기


res <- createPlot(subject3_left_data, 1, "test", TRUE, "p3",TRUE,  19226500 , 19226500 + 3000)


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

getAutocorrelationFrom2 <- function(data, data2, lag)
{
  if(length(data)!= length(data2))
  {
    print(paste("error - length(data)!= length(data2)", length(data), length(data2)))
    return
  }
  
  u <- mean(data)
  u2 <- mean(data2)
  N <- length(data)
  sum <- 0
  for(i in 1:(N-lag))
  {
    sum <- sum + (data[i]-u)*(data2[i+lag]-u2)
  }
  sum2 <- 0
  sum3 <- 0
  for(i in 1:N)
  {
    sum2 <- sum2 + (data[i]-u)^2
  }
  
  for(i in 1:N)
  {
    sum3 <- sum3 + (data2[i]-u2)^2
  }
  
  #  sum <- sum/(N-lag)
  res <- sum/(sqrt(sum2)*sqrt(sum3))
  
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

createAutocorrelationSignal(data.sub, 8, 1, FALSE, 18302649+2000, 18302649+2000+3000, avg = T,type = "mag",graph_title ="머리 넘김" ) #

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
  else if(type=="range")
  {
    p <- findPeaks(signal)
    p <- p - 1
    maxV <- 0
    if(length(p)==0)
      return(0)
    
    for(i in 1:length(p))
    {
      if(signal[p[i]] > maxV)
        maxV <- signal[p[i]]
    }
    
    p2 <- findValleys(signal)
    p2 <- p2 - 1
    minV <- 999999999999
    if(length(p2)==0)
      return(0)
    
    for(i in 1:length(p2))
    {
      if(signal[p2[i]] < minV)
        minV <- signal[p2[i]]
    }
    
    resValue <- maxV - minV
  }
  #print(paste("resValue",resValue))
  
  return(abs(resValue))
}




getNumPeaksBy(signal_x, 0.1,0.4, "peaks", "weak")

getNumPeaksBy <- function(signal, neigh_th, min_th, type = "peaks", filtering = "prominent", calType = "num")
{
  require("quantmod")
  if(any(is.na(signal)))
    return (0)
  
  if( (type=="peaks") & (filtering=="prominent") )
  {
    p <- findPeaks(signal, thresh = neigh_th)
    res <- getNumThreshold(signal[p-1], min_th, type ="up", calType = calType)
    
  }else if( (type=="peaks") & (filtering=="weak") )
  {
    p_raw <- findPeaks(signal, thresh = 0)
    #print(p_raw)
    p <- findPeaks(signal, thresh = neigh_th)
    #print(p)
    weak_p <- p_raw[-which( p_raw %in% p)]
    #print(weak_p)
    res <- getNumThreshold(signal[weak_p-1], min_th, type="down", calType = calType)
    #print(res)
  }
  
  else if( (type=="valley") & (filtering=="prominent") )
  {
    p <- findPeaks(signal, thresh = neigh_th)
    res <- getNumThreshold(signal[p-1], min_th, type ="up", calType = calType)
    
  }
  else if( (type=="valley") & (filtering=="weak") )
  {
    p_raw <- findPeaks(signal, thresh = 0)
    #print(p_raw)
    p <- findPeaks(signal, thresh = neigh_th)
    #print(p)
    weak_p <- p_raw[-which( p_raw %in% p)]
    #print(weak_p)
    res <- getNumThreshold(signal[weak_p-1], min_th, type="down", calType = calType)
    #print(res)
  }
  
  else if( (type=="peakvalley") & (filtering=="prominent") )
  {
    signal_ab <- abs(signal)
    p <- findPeaks(signal_ab, thresh = neigh_th)
    res <- getNumThreshold(signal_ab[p-1], min_th, type ="up", calType = calType)
    
  }
  else if( (type=="peakvalley") & (filtering=="weak") )
  {
    signal_ab <- abs(signal)
    p_raw <- findPeaks(signal_ab, thresh = 0)
    #print(p_raw)
    p <- findPeaks(signal_ab, thresh = neigh_th)
    #print(p)
    weak_p <- p_raw[-which( p_raw %in% p)]
    #print(weak_p)
    res <- getNumThreshold(signal[weak_p-1], min_th, type="down", calType = calType)
    #print(res)   
  }
  else if( type=="num")
  {
    signal_ab <- abs(signal)
    p <- findPeaks(signal_ab,thresh = neigh_th)
    if(calType == "num"){
      if(length(p)<=0){
        return (0)
      }
      res <- length(p)
    }
    else{
      res <- sum(signal[p])
    }
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


createSignalcomparison(subject3_left_data, 8, TRUE,6483892-1000, 6483892+2000, filtering=TRUE,f_l=5) # 손등 긁기
createSignalcomparison(subject3_left_data, 8, TRUE, 21752813 + 3000,  21752813 + 3000 + 3000, filtering=TRUE,f_l=0.5 ) 
createSignalcomparison <- function(data, idx, setbtw=FALSE, start_milli=0.1, end_milli=0.1,filtering =TRUE, f_l = 10,
                                   samplingrate=50, filter_type = "low", filter_type2 = "high", f_l2 = 10, order1 = 1, order2 = 1
                                   )
{
  data.sub <- subset(data, grepl(list[idx], data$type))
  
  if(setbtw)
  {
    data.sub <- subset(data.sub, subset=(data.sub$time < end_milli ))
    data.sub <- subset(data.sub, subset=(data.sub$time > start_milli ))
  }
  
  res <- createPlot(data.sub, idx, "raw-siganl", FALSE, "0809")
  print(res)

  #bf <- butter(2, frequency, type=frtype)
  if(filtering){
    bf2 <- butter(order1, (2*f_l)/(samplingrate), type=filter_type)
    
    data.sub$x <- filtfilt( bf2, data.sub$x)
    data.sub$y <- filtfilt( bf2, data.sub$y)
    data.sub$z <- filtfilt( bf2, data.sub$z)
    
    res <- createPlot(data.sub, idx, paste("butterworth_",f_l,"_",filter_type,sep=""), FALSE, "0809")
    print(res)
    
    
    bf2 <- butter(order2, (2*f_l2)/(samplingrate), type=filter_type2)
    
    data.sub$x <- filtfilt( bf2, data.sub$x)
    data.sub$y <- filtfilt( bf2, data.sub$y)
    data.sub$z <- filtfilt( bf2, data.sub$z)
    
    res <- createPlot(data.sub, idx, paste("butterworth_",f_l2,"_",filter_type2,sep=""), FALSE, "0809")
    print(res)
  }
  
  data.sub$x <- data.sub$x/max(abs(data.sub$x))
  data.sub$y <- data.sub$y/max(abs(data.sub$y))
  data.sub$z <- data.sub$z/max(abs(data.sub$z))
  
  res <- createPlot(data.sub, idx, paste("namlized_butterworth_",f_l,"_low",sep=""), FALSE, "0809")
  print(res)
  
  
  
}


getSpacing <- function(pvlist, data, time)
{
  peaklist<-1
  for(i in 2:length(pvlist))
  {
    #print(paste("i-",i,"peak-",data[pvlist[i-1]-1],sep=""))
    #print(paste("spacing - ", time[pvlist[i]-1] - time[pvlist[i-1]-1] ))
    peaklist[i-1] <- time[pvlist[i]-1] - time[pvlist[i-1]-1]
  }
#  print(peaklist)
  return(peaklist)
}


getMaxidx<- function(px, signal)
{
  max<--10000000000
  maxidx<-0
  if(length(px)==0)
    return(-1)
  for(i in 1:length(px)){
    if(signal[px[i]-1] > max)
    {
      max <- signal[px[i]-1]
      maxidx <- (px[i]-1)
    }
  }
  
  return(maxidx)
}

getRemainder <- function(signal,  time, thresh = 0,  alpha = 0.15)
{
  px <- findPeaks(signal, thresh = thresh)
  #print(px)
  if(length(px)==0){
    print("px==0")
    selected[1] <- FALSE
    return(selected)
  }
  #print(paste("len -px",length(px)))
  
  
  maxidx <- getMaxidx(px, signal)
  t1 <- time[maxidx]
  #print(paste("maxidx", maxidx, " t1 ", t1))

  selected <- -99
  selected[1] <- -99
  


  idx <- 1
  for(i in 1:length(px))
  {
    v <- time[px[i]-1]
    re <- v%%t1
    #print(paste("res ",re, " v ",v," t1 ",t1))
    #print(paste(" [ ", t1*alpha, ", ", t1*(1-alpha), " ] "))
    if( (t1*alpha < re) & (re < t1*(1-alpha) ) ) {
      selected[idx] <- px[i]-1
      idx <- idx + 1
      #print(paste("yes v-",v))
  } else{
      #print(paste("no v-",v))
    }
  
  }
  
 # print(paste("selected", selected[1]))
  return(selected)
}

getHarm <- function(signal, time, thresh = 0, alpha = 0.15, beta = 1/(10000*10) )
{
  #print(paste("signal len",length(signal),"time len", length(time)))
  time <- time - time[1]

#  print(time[px-1])
  remainder <- getRemainder(signal, time, thresh, alpha)

  if(remainder[1]==FALSE){
    return (0)
  }
  else if(remainder[1] == -99){
      signal_sum <- 0; r0 <-1;      
  }
  else{
    px <- findPeaks(signal, thresh = thresh)
    #print(px)
    maxidx <- getMaxidx(px, signal)
    t1 <- time[maxidx]
    #print("remainder")
    #print(remainder)
    #print(signal[remainder])
    r0 <- t1
    #print(paste("t1",r0))
    #r0 <- sum(signal[px-1])
    signal_sum <- sum(abs(signal[remainder]))
    #print(signal[remainder])
    #print(paste(r0, signal_sum))
  }  
 
  
  return ( exp(-1/beta*(signal_sum/r0)) )
}




getVarPeakSpacing <- function(signal, time, thresh = 0)
{
  px <- findPeaks(signal,thresh = thresh)
  peaklist<-getSpacing(px, signal, time)
#  print(paste("variance", var(peaklist)))
  
  return (var(peaklist))
}

getVarPeakSpacing <- function(signal, time, thresh = 0)
{
  px <- findPeaks(signal,thresh = thresh)
  peaklist<-getSpacing(px, signal, time)
  #  print(paste("variance", var(peaklist)))
  
  return (var(peaklist))
}

getVarValleySpacing <- function(signal, time, thresh = 0)
{
  px <- findValleys(signal,thresh = thresh)
  peaklist<-getSpacing(px,signal,time)
  #  print(paste("variance", var(peaklist)))
  
  return (var(peaklist))
}

#print(paste("variance", var(peaklist)))


createAutocorrelationSignal <- function(data, idx, startLag,
                                        setbtw=FALSE, start_milli=0.1, end_milli=0.1, avg=FALSE, type="mag", # mag, PC
                                        filter_num = 1, spanV = 0.1,
                                        graph_title="noname",threshold = 0,peak_threshold=0, filtering =TRUE,
                                        f_l = 10, samplingrate=50)
{
  data.sub <- subset(data, grepl(list[idx], data$type))
  
  if(setbtw)
  {
    data.sub <- subset(data.sub, subset=(data.sub$time < end_milli ))
    data.sub <- subset(data.sub, subset=(data.sub$time > start_milli ))
  }
  
  res <- createPlot( data.sub, idx, "raw_data", FALSE, "0810")
  print(res)
  #bf <- butter(2, frequency, type=frtype)
  if(filtering & (filter_num == 1) ){
    bf2 <- butter(1, (2*f_l)/(samplingrate), type="low")
    data.sub$x <- filtfilt( bf2, data.sub$x)
    data.sub$y <- filtfilt( bf2, data.sub$y)
    data.sub$z <- filtfilt( bf2, data.sub$z) 
  }else if(filtering & (filter_num == 2))
  {
    df <- data.frame(time_hour = data.sub$time, x = data.sub$x, y = data.sub$y, z = data.sub$z )
    data.sub$x <- predict(loess(x~time_hour, df, span = spanV), df$time_hour)
    data.sub$y <- predict(loess(y~time_hour, df, span = spanV), df$time_hour)
    data.sub$z <- predict(loess(z~time_hour, df, span = spanV), df$time_hour)
  }else if(filtering & (filter_num == 3))
  {
    df <- data.frame(time_hour = data.sub$time, x = data.sub$x, y = data.sub$y, z = data.sub$z )
    sx <- predict(smooth.spline(df$time_hour, df$x, spar = spanV), df$time_hour, cv = T)
    sy <- predict(smooth.spline(df$time_hour, df$y, spar = spanV), df$time_hour, cv = T)
    sz <- predict(smooth.spline(df$time_hour, df$z, spar = spanV), df$time_hour, cv = T)
    print(paste("x spar ", smooth.spline(df$time_hour, df$x, cv=T)$spar ))
    print(paste("y spar ", smooth.spline(df$time_hour, df$y, cv=T)$spar ))
    print(paste("z spar ", smooth.spline(df$time_hour, df$z, cv=T)$spar ))
    
    data.sub$x <- sx$y
    data.sub$y <- sy$y
    data.sub$z <- sz$y
  }else if(filtering & (filter_num == 4))
  {
    df <- data.frame(time_hour = data.sub$time, x = data.sub$x, y = data.sub$y, z = data.sub$z )
    data.sub$x <- movingAverage(df$x, spanV, TRUE)
    data.sub$y <- movingAverage(df$y, spanV, TRUE)
    data.sub$z <- movingAverage(df$z, spanV, TRUE)
    
  }
  
  res<-createPlot( data.sub, idx, paste("butterworth_", f_l,sep=""), FALSE, "0810")
  print(res)
#  createFrequencyBy(data.sub$x, "x-axis(raw)")
#  createFrequencyBy(data.sub$y, "y-axis(raw)")
#  createFrequencyBy(data.sub$z, "z-axis(raw)")
  
  
  if(avg == FALSE){
    signal_x <- getAutocorrelationSignal(data.sub$x, startLag)
    signal_y <- getAutocorrelationSignal(data.sub$y, startLag)
    signal_z <- getAutocorrelationSignal(data.sub$z, startLag)

#    createFrequencyBy(signal_x, "x-axis(auto-cor)")
#    createFrequencyBy(signal_y, "y-axis(auto-cor)")
#    createFrequencyBy(signal_z, "z-axis(auto-cor)")
    
    px <- findPeaks(signal_x,thresh = 0)
    vx <- findValleys(signal_x,thresh = 0)
    py <- findPeaks(signal_y,thresh = 0.01)
    vy <- findValleys(signal_y,thresh = 0.01)
    pz <- findPeaks(signal_z,thresh = 0.01)
    vz <- findValleys(signal_z,thresh = 0.01)  
    
    raw_x_len <- length(findPeaks(signal_x,thresh = 0)) + length(findValleys(signal_x,thresh = 0))
    th_x_len <- length(findPeaks(signal_x,thresh = threshold)) + length(findValleys(signal_x,thresh = threshold))
    print(paste(" (x axis) # of peaks and velly : ", raw_x_len, "   # of peaks and velly (threshold) : ", th_x_len ))
    
    raw_y_len <- length(findPeaks(signal_y,thresh = 0)) + length(findValleys(signal_y,thresh = 0))
    th_y_len <- length(findPeaks(signal_y,thresh = threshold)) + length(findValleys(signal_y,thresh = threshold))
    print(paste(" (y axis) # of peaks and velly : ", raw_y_len, "   # of peaks and velly (threshold) : ", th_y_len ))
    
    raw_z_len <- length(findPeaks(signal_z,thresh = 0)) + length(findValleys(signal_z,thresh = 0))
    th_z_len <- length(findPeaks(signal_z,thresh = threshold)) + length(findValleys(signal_z,thresh = threshold))
    print(paste(" (z axis) # of peaks and velly : ", raw_z_len, "   # of peaks and velly (threshold) : ", th_z_len ))
    
    ##
    data.sub$time <- data.sub$time - data.sub$time[1]
    print(paste("len check : ",length(signal_x),length(signal_y),length(signal_z),length(data.sub$time)))
    
    max_value <- (as.integer(max(data.sub$time)))
    spliting <- seq(0,max_value,max_value/10)
    graph_title <- paste(graph_title, "3-axis")
    
    df <- data.frame(time = data.sub$time[startLag:length(data.sub$time)], x=signal_x, y=signal_y, z=signal_z)
    print(paste("len check - time : ",length(data.sub$time[startLag:length(data.sub$time)])))
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
    

#    print("px")
#    print(df$x[px-1])
#    print(df$y[px-1])
#    print(df$z[px-1])
    peaklist<-getSpacing(px,df$x,df$time)
    print(paste("variance", var(peaklist)))
    valleylist<-getSpacing(vx,df$x,df$time)
    print(paste("variance", var(valleylist)))


    print(px)
    print(df$time[px-1])
    harm <- getHarm(df$x, df$time)

#    print(paste("harm-",harm,sep=""))
    print(paste(getHarm(df$x, df$time),getHarm(df$y, df$time),getHarm(df$z, df$time)))


    returnValue <- returnValue + geom_vline(xintercept = df$time[px-1], alpha=1, colour="red",linetype=1)
    returnValue <- returnValue + geom_vline(xintercept = df$time[vx-1], alpha=1, colour="red",linetype=1)
    
#    returnValue <- returnValue + geom_vline(xintercept = df$time[py-1], alpha=1, colour="blue",linetype=1)
#    returnValue <- returnValue + geom_vline(xintercept = df$time[vy-1], alpha=1, colour="blue",linetype=1)
    
#    returnValue <- returnValue + geom_vline(xintercept = df$time[pz-1], alpha=1, colour="yellow",linetype=1)
#    returnValue <- returnValue + geom_vline(xintercept = df$time[vz-1], alpha=1, colour="yellow",linetype=1)
    
  }else{
    
    if(type=="mag")
    { 
      data.sub$mag <- sqrt( (data.sub$x+50)^2 + (data.sub$y+50)^2 + (data.sub$z+50)^2)
      data.sub$avg <- data.sub$mag - mean(data.sub$mag)    
      graph_title <- paste(graph_title,"average - magnitude")
      
    }
    else if(type=="PC")
    {
      trans <- preProcess(data.sub[,3:5], method=c("BoxCox", "center", "scale", "pca"))
      PC <- predict(trans, data.sub[,3:5])
      #plot(1:length(PC$PC1), PC$PC1, type="l")
      data.sub$avg <- PC$PC1
      graph_title <- paste(graph_title,"average - 1st comp. from PCA analysis")
    }
    signal_x <- getAutocorrelationSignal(data.sub$avg, startLag)
    
    
    raw_p <- findPeaks(signal_x,thresh = 0)
    raw_p2 <- findValleys(signal_x,thresh = 0)
    
    p <- findPeaks(signal_x,thresh = threshold)
    p2 <- findValleys(signal_x,thresh = threshold)
    
    #num <- getNumThreshold(signal_x[findPeaks(signal_x,thresh=threshold)], peak_threshold)
    
    sum1 <- sum( signal_x[p]  )
    sum2 <- sum( signal_x[p2] )
    
    
    print(signal_x[p])
    print(paste("peak sum (",threshold,") - ", sum1, "  len", length(p) ))
    
    print(signal_x[p2])
    print(paste("valley sum (",threshold,") - ", sum2, "  len", length(p2) ))    
    
    
    
    data.sub$time <- data.sub$time - data.sub$time[1]
    print(paste("len check : ",length(signal_x),length(data.sub$time)))
    
    max_value <- (as.integer(max(data.sub$time)))
    spliting <- seq(0,max_value,max_value/10)
    
    df <- data.frame(time = data.sub$time[startLag:length(data.sub$time)], x=signal_x)
    print(paste("len check - time : ",length(data.sub$time[startLag:length(data.sub$time)])))
    
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
    
    returnValue <- returnValue + geom_vline(xintercept = df$time[p-1], alpha=1, colour="red",linetype=1)
    returnValue <- returnValue + geom_vline(xintercept = df$time[p2-1], alpha=1, colour="blue",linetype=1)
    
    print(paste("# of peaks and velly (", threshold, ") :",sum(length(p)+length(p2)),sep=""))
    print(paste("# of peaks and velly : ", sum(length(raw_p)+length(raw_p2))))
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



getCrosscorrelationSignal <- function(data_a, data_b, startLag)
{
  data_list_a <- 1
  
  i<-1
  endLag<-length(data_a)
  for(k in (startLag-1):(endLag-1))
  {
    data_list_a[i] <- getAutocorrelationFrom2(data_a, data_b, k)
    i <- i+1
  }
  #View(data_list_a)
  
  return (data_list_a)
}



createHeatmapByFeature("test",sim_test_data, 8, 150, 50, FALSE, FALSE, 0, 15000, x_type="time", signal_type = "heatmap", thresholdvar = 0.005)




createHeatmapByFeature <- function(graph_title, data, idx, window_size, window_step, 
                                   cut=TRUE, set_btw=FALSE, start_milli=1.1, end_milli=1.1, x_type="time", 
                                   signal_type = "heatmap", thresholdvar = 0.1, forcedlen = FALSE,
                                   filter_num = 1 , spanV = 0.3, filtering2 = FALSE,
                                   filtering =FALSE, f_l=10, sampling.rate = 50, options ="no", filter_type = "low", order = 1)
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
    data.sub <- subset(data.sub, subset=(data.sub$time > start_milli ))
    data.sub <- subset(data.sub, subset=(data.sub$time < end_milli ))
    graph_title <- paste(graph_title," - (",start_milli,",",end_milli,")")
    ##
  }
  
  window_num <- as.integer(nrow(data.sub)/window_step)
  window_num <- window_num - 2 # filtering last windows lower than window size(150)
  
  data_feature <- vector(mode="list", length=4)
  names(data_feature) <- c("x","y","z","step_num")
  window_idx <- 1
  heatmap_matrix_x <- vector(mode="list",length = length(window_num))
  heatmap_matrix_y <- vector(mode="list",length = length(window_num))
  heatmap_matrix_z <- vector(mode="list",length = length(window_num))
  tmp_max <- -1000000000000
  tmp_min <- 1000000000000
  len <- window_size
  
  if(signal_type=="frequency")
    len <-ceiling((len+1)/2)
  
  if( forcedlen != FALSE )
    len <- forcedlen
  
  print(paste("window_num", window_num))
  for(k in 0:(len-1))
  {
    window_idx <- 1
    for(i in 1:window_num)
    {
      
      #print(c( window_idx, i))
      window_data <- getWindow(data.sub,window_idx,window_size)
      magnitude <- sqrt( (window_data$x+50)^2+(window_data$y+50)^2+(window_data$z+50)^2)
      magnitude <- magnitude - mean(magnitude)
      
      if(var(magnitude)>thresholdvar){
        
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
        
        #print(length(window_data$x))
        if(signal_type == "heatmap"){
          signal_x <- getAutocorrelation(window_data$x, k)
          signal_y <- getAutocorrelation(window_data$y, k)
          signal_z <- getAutocorrelation(window_data$z, k)
          data_feature$x[i] <- signal_x
          data_feature$y[i] <- signal_y
          data_feature$z[i] <- signal_z         

        }else if(signal_type =="cepstrum"){         
          data_feature$x[i] <- as.double(ifft(log(abs(fft(window_data$x)))))[k+1]
          data_feature$y[i] <- as.double(ifft(log(abs(fft(window_data$y)))))[k+1]
          data_feature$z[i] <- as.double(ifft(log(abs(fft(window_data$z)))))[k+1]         
          
        }else if(signal_type =="frequency"){
          data_feature$x[i] <- trans_to_frequency(window_data$x)[k+1]
          data_feature$y[i] <- trans_to_frequency(window_data$y)[k+1]
          data_feature$z[i] <- trans_to_frequency(window_data$z)[k+1]      
          
        }
        
        tmp_max <- max(data_feature$x[i], data_feature$y[i], data_feature$z[i], tmp_max)
        tmp_min <- min(data_feature$x[i], data_feature$y[i], data_feature$z[i], tmp_min)
      }else{
        data_feature$x[i] <- 100
        data_feature$y[i] <- 100
        data_feature$z[i] <- 100    
      }
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
  


  if(options=="auto_frequency"){
    tmp_max <- -1000000000000
    tmp_min <- 1000000000000
    data_feature <- vector(mode="list", length=4)
    names(data_feature) <- c("x","y","z","step_num")
    
    heatmap_matrix_x_tmp <- vector(mode="list")    
    heatmap_matrix_y_tmp <- vector(mode="list")
    heatmap_matrix_z_tmp <- vector(mode="list")
#    print(paste("heatmap_matrix_x num", nrow(heatmap_matrix_x), ncol(heatmap_matrix_x), typeof(heatmap_matrix_x)))
#    View(heatmap_matrix_x)
    
    for(i in 1:ncol(heatmap_matrix_x))
    {
      signal_x <- heatmap_matrix_x[,i]
      signal_y <- heatmap_matrix_y[,i]
      signal_z <- heatmap_matrix_z[,i]
      if(signal_x[1] !=100){
        signal_x <- trans_to_frequency( as.numeric( t(signal_x) ))
        signal_y <- trans_to_frequency( as.numeric( t(signal_y) ))
        signal_z <- trans_to_frequency( as.numeric( t(signal_z) ))
        heatmap_matrix_x_tmp <- cbind(heatmap_matrix_x_tmp, signal_x)
        heatmap_matrix_y_tmp <- cbind(heatmap_matrix_y_tmp, signal_y)
        heatmap_matrix_z_tmp <- cbind(heatmap_matrix_z_tmp, signal_z)
        tmp_max <- max(signal_x, signal_y, signal_z, tmp_max)
        tmp_min <- min(signal_x, signal_y, signal_z, tmp_min)
      }else{
        heatmap_matrix_x_tmp <- cbind(heatmap_matrix_x_tmp, signal_x[1:( (window_size/2) +1)])
        heatmap_matrix_y_tmp <- cbind(heatmap_matrix_y_tmp, signal_x[1:( (window_size/2) +1)])
        heatmap_matrix_z_tmp <- cbind(heatmap_matrix_z_tmp, signal_x[1:( (window_size/2) +1)])
      }
      
      data_feature$step_num[i] <- i
    }
#    View(heatmap_matrix_x_tmp)
#    print(paste("heatmap_matrix_x_tmp num", nrow(heatmap_matrix_x_tmp) , ncol(heatmap_matrix_x_tmp) ))
    
    row.names(heatmap_matrix_x_tmp) <-NULL
    colnames(heatmap_matrix_x_tmp) <- NULL
    row.names(heatmap_matrix_y_tmp) <-NULL
    colnames(heatmap_matrix_y_tmp) <- NULL
    row.names(heatmap_matrix_z_tmp) <-NULL
    colnames(heatmap_matrix_z_tmp) <- NULL
    heatmap_matrix_x <- heatmap_matrix_x_tmp
    heatmap_matrix_y <- heatmap_matrix_y_tmp
    heatmap_matrix_z <- heatmap_matrix_z_tmp
#    print(paste("typeof(heatmap_matrix_x_tmp))", typeof(heatmap_matrix_x_tmp)))
#    heatmap_matrix_x2 <- heatmap_matrix_x
#    View(heatmap_matrix_x2)
    
    
  }else if (options == "2depth")
  {
    tmp_max <- -1000000000000
    tmp_min <- 1000000000000
    data_feature <- vector(mode="list", length=4)
    names(data_feature) <- c("x","y","z","step_num")
    
    heatmap_matrix_x_tmp <- vector(mode="list")    
    heatmap_matrix_y_tmp <- vector(mode="list")
    heatmap_matrix_z_tmp <- vector(mode="list")
    #    print(paste("heatmap_matrix_x num", nrow(heatmap_matrix_x), ncol(heatmap_matrix_x), typeof(heatmap_matrix_x)))
    #    View(heatmap_matrix_x)
    
    for(i in 2:ncol(heatmap_matrix_x))
    {
      signal_x_prev <- heatmap_matrix_x[,(i-1)]
      signal_x <- heatmap_matrix_x[,i]
      
      if( (signal_x[1]!=100) & (signal_x_prev[1]!=100) )
      {
        print(paste("i",i))
        signal_x <- getCrosscorrelationSignal(  as.numeric( heatmap_matrix_x[,(i-1)] ),  as.numeric( heatmap_matrix_x[,(i)]), startLag = 1 )
        signal_y <- getCrosscorrelationSignal(  as.numeric( heatmap_matrix_y[,(i-1)] ),  as.numeric( heatmap_matrix_y[,(i)]), startLag = 1 )
        signal_z <- getCrosscorrelationSignal(   as.numeric( heatmap_matrix_z[,(i-1)]) ,  as.numeric( heatmap_matrix_z[,(i)]), startLag = 1 )
        
        if(filtering2 & (filter_num == 3))
        {
          df <- data.frame(time_hour = c(1:length(signal_x)), x = signal_x, y = signal_y, z = signal_z )   
          signal_x <-predict(smooth.spline(df$time_hour, df$x, spar = spanV), df$time_hour)$y
          signal_y <-predict(smooth.spline(df$time_hour, df$y, spar = spanV), df$time_hour)$y
          signal_z <-predict(smooth.spline(df$time_hour, df$z, spar = spanV), df$time_hour)$y
        }
        
        heatmap_matrix_x_tmp <- cbind(heatmap_matrix_x_tmp, signal_x)
        heatmap_matrix_y_tmp <- cbind(heatmap_matrix_y_tmp, signal_y)
        heatmap_matrix_z_tmp <- cbind(heatmap_matrix_z_tmp, signal_z)
        tmp_max <- max(signal_x, signal_y, signal_z, tmp_max)
        tmp_min <- min(signal_x, signal_y, signal_z, tmp_min)
      }else{
        heatmap_matrix_x_tmp <- cbind(heatmap_matrix_x_tmp, signal_x[1:window_size])
        heatmap_matrix_y_tmp <- cbind(heatmap_matrix_y_tmp, signal_x[1:window_size])
        heatmap_matrix_z_tmp <- cbind(heatmap_matrix_z_tmp, signal_x[1:window_size])
      }
      
      data_feature$step_num[i-1] <- i-1
    }
    #    View(heatmap_matrix_x_tmp)
    #    print(paste("heatmap_matrix_x_tmp num", nrow(heatmap_matrix_x_tmp) , ncol(heatmap_matrix_x_tmp) ))
    
    row.names(heatmap_matrix_x_tmp) <-NULL
    colnames(heatmap_matrix_x_tmp) <- NULL
    row.names(heatmap_matrix_y_tmp) <-NULL
    colnames(heatmap_matrix_y_tmp) <- NULL
    row.names(heatmap_matrix_z_tmp) <-NULL
    colnames(heatmap_matrix_z_tmp) <- NULL
    heatmap_matrix_x <- heatmap_matrix_x_tmp
    heatmap_matrix_y <- heatmap_matrix_y_tmp
    heatmap_matrix_z <- heatmap_matrix_z_tmp
    #    print(paste("typeof(heatmap_matrix_x_tmp))", typeof(heatmap_matrix_x_tmp)))
    #    heatmap_matrix_x2 <- heatmap_matrix_x
    #    View(heatmap_matrix_x2)

  }

  
  print(paste("max",tmp_max, "min", tmp_min))
  if(signal_type == "heatmap"){
    mincolor <- -1
    maxcolor <- 1
    myplate<-colorRampPalette(c("blue", "yellow","red", "black"))
    colorlen <- 1000
    if(options=="auto_frequency"){
      myplate<-colorRampPalette(c("black","red","yellow", "white"))
      
      for(i in 1:ncol(heatmap_matrix_x))
      {
        signal_x <- heatmap_matrix_x[,i]
        if(signal_x[1]!=100)
        {
#          if(i==7)
#            View(heatmap_matrix_x[,i])
#          print(paste("i - ",i))
          heatmap_matrix_x[,i] <- rescale(as.numeric(heatmap_matrix_x[,i]), to =c(0,1), from = c(as.numeric(tmp_min), as.numeric(tmp_min)*1000 ))
          heatmap_matrix_y[,i] <- rescale(as.numeric(heatmap_matrix_y[,i]), to =c(0,1), from =c(as.numeric(tmp_min), as.numeric(tmp_min)*1000 ))
          heatmap_matrix_z[,i] <- rescale(as.numeric(heatmap_matrix_z[,i]), to =c(0,1), from =c(as.numeric(tmp_min), as.numeric(tmp_min)*1000 )) 
        }
      }
      mincolor <- 0
      maxcolor <- 1
      len <-ceiling((len+1)/2)
      colorlen <- 1000
    }

  }
  else if(signal_type == "cepstrum"){
    mincolor <- -1
    maxcolor <- 1
    myplate<-colorRampPalette(c("blue", "yellow","red", "black"))
    colorlen <- 1000
  }else if(signal_type == "frequency"){
    
    myplate<-colorRampPalette(c("black","red","yellow", "white"))
    
    for(i in 1:ncol(heatmap_matrix_x))
    {
      signal_x <- heatmap_matrix_x[,i]
      if(signal_x[1]!=100)
      {
        #          if(i==7)
        #            View(heatmap_matrix_x[,i])
        #          print(paste("i - ",i))
        heatmap_matrix_x[,i] <- rescale(as.numeric(heatmap_matrix_x[,i]), to =c(0,1), from = c(as.numeric(tmp_min), as.numeric(tmp_min)*100000000 ))
        heatmap_matrix_y[,i] <- rescale(as.numeric(heatmap_matrix_y[,i]), to =c(0,1), from =c(as.numeric(tmp_min), as.numeric(tmp_min)*100000000 ))
        heatmap_matrix_z[,i] <- rescale(as.numeric(heatmap_matrix_z[,i]), to =c(0,1), from =c(as.numeric(tmp_min), as.numeric(tmp_min)*100000000 ))
      }
    }
    mincolor <- 0
    maxcolor <- 1
    colorlen <- 2000
  
    
    
    ###
#    mincolor <- tmp_min
#    maxcolor <- tmp_max
    
#    myplate<-colorRampPalette(c("blue", "yellow","red", "black"))
#    colorlen <- 1000
#    n =  window_size
#    sampling.rate = 50 
#    nUniquePts <- ceiling((n+1)/2)
#    freqArray <- (0:(nUniquePts-1)) * (sampling.rate / n)
  }
  
  data_feature$time <- (data_feature$step_num*(window_step/50))*1000
  print(paste("data_feature$time ", length(data_feature$time), first(data_feature$time), last(data_feature$time) ))
#  View(data_feature$time)
  max_value <- (as.integer(max(data_feature$time)))
  spliting <- seq(0,max_value,max_value/10)
  xlablename <- "time(millisecond)"
  #  View(heatmap_matrix)
  #print(paste(" length(column.values) == ncol(x)", length(data_feature$time), ncol(heatmap_matrix_x)  ))
  pal <- colorRampPalette(c("red", "yellow"), space = "rgb")
  p1 <- levelplot(t(heatmap_matrix_x), main=paste( "auto-correlation(", sensor_name_list[idx], ", X-axis)" ,sep=""), ylim=c(0,len), row.values=c(data_feature$time),
                  scales=list(x=list(at=seq(from=first(data_feature$time),to=last(data_feature$time),by=max_value/10)), y=list(at=seq(from=0,to=len,by=10)),cex=.7, alternating=3), aspect="fill",cex.axis=1.5, cex.lab=1.5,xlab="Time (millisecond)", ylab="Lag", col.regions=myplate, at=seq(mincolor,maxcolor,length=colorlen))
  p2 <- levelplot(t(heatmap_matrix_y), main=paste( "auto-correlation(", sensor_name_list[idx], ", Y-axis)" ,sep=""), ylim=c(0,len), row.values=c(data_feature$time),
                  scales=list(x=list(at=seq(from=first(data_feature$time),to=last(data_feature$time),by=max_value/10)), y=list(at=seq(from=0,to=len,by=10)),cex=.7, alternating=3), aspect="fill",cex.axis=1.5, cex.lab=1.5,xlab="Time (millisecond)", ylab="Lag", col.regions=myplate, at=seq(mincolor,maxcolor,length=colorlen))
  p3 <- levelplot(t(heatmap_matrix_z), main=paste( "auto-correlation(", sensor_name_list[idx], ", Z-axis)" ,sep=""), ylim=c(0,len),row.values=c(data_feature$time),
                  scales=list(x=list(at=seq(from=first(data_feature$time),to=last(data_feature$time),by=max_value/10)), y=list(at=seq(from=0,to=len,by=10)),cex=.7, alternating=3), aspect="fill",cex.axis=1.5, cex.lab=1.5,xlab="Time (millisecond)", ylab="Lag", col.regions=myplate, at=seq(mincolor,maxcolor,length=colorlen))
  
  or_plot <- createPlot(data, idx, paste("",sep=""), FALSE, "0815", showing_legend = FALSE, set_btw = set_btw, start_hour = start_milli, end_hour = end_milli)
  returnValue <-p1
  #multiplot(p2, p1, cols=1,saveFile=saveFile)
  #multiplot(returnValue2, returnValue, cols=1,saveFile=saveFile)
  #multiplot(returnValue2, returnValue, cols=1,saveFile=saveFile)
  
  #  print(p1)
  #  print(p2)
  #  print(p3)
  returnValue <- grid.arrange(or_plot, p1, p2, p3, ncol=1)
  print(returnValue)
  #return (returnValue)
  
}


movingAverage <- function(x, n=1, centered=FALSE) {
  
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x))
  count <- rep(0, length(x))
  
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), x[1:(length(x)-i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # return sum divided by count
  s/count
}

