###### for each type


sim_scrType1 <- read.table("./data_raw/eunji/data1.txt",sep=",",header=TRUE)

data.sub <- sim_scrType4
createPlot(data.sub, 8 , "test", FALSE, "0815", set_btw = TRUE, start_hour = 0, end_hour = 25000)
data.sub <- subset(data.sub, subset=(data.sub$time > 0 ))
data.sub <- subset(data.sub, subset=(data.sub$time < 25000 ))
createPlot(data.sub, 8 , "test", FALSE, "0815")

sim_scrType4<-data.sub

sim_scrType2 <- read.table("./data_raw/jonginlee_data0623/data2.txt",sep=",",header=TRUE)
sim_scrType3 <- read.table("./data_raw/junhong_data0623/data3.txt",sep=",",header=TRUE)
sim_scrType4 <- read.table("./data_raw/seungho/data4.txt",sep=",",header=TRUE) 

sim_scrType1234 <- rbind(sim_scrType1, sim_scrType2, sim_scrType3, sim_scrType4)
View(sim_scrType1234)

sim_pullblanket1 <- read.table("./data_raw/non_scratch/eunji_ns1L_pullblank.txt",sep=",",header=TRUE)
sim_pullblanket2 <- read.table("./data_raw/non_scratch/junhong_ns1R_pullblank.txt",sep=",",header=TRUE)
sim_pullblanket1$X <- NULL
sim_pullblanket2$X <- NULL

sim_turnover1 <- read.table("./data_raw/non_scratch/eunji_ns1L_turnover.txt",sep=",",header=TRUE)
sim_turnover2 <- read.table("./data_raw/non_scratch/junhong_ns1R_turnover.txt",sep=",",header=TRUE)
sim_turnover1$X <- NULL
sim_turnover2$X <- NULL

sim_stretch1 <- read.table("./data_raw/non_scratch/jongin_ns1L_stretch.txt",sep=",",header=TRUE)
sim_stretch2 <- read.table("./data_raw/non_scratch/eunji_ns1R_stretch.txt",sep=",",header=TRUE)
sim_stretch1$X <- NULL
sim_stretch2$X <- NULL

sim_walk1 <- read.table("./data_raw/non_scratch/eunji_ns2L.txt",sep=",",header=TRUE)
sim_walk2 <- read.table("./data_raw/non_scratch/junhong_ns2R.txt",sep=",",header=TRUE)
sim_walk1$X <- NULL
sim_walk2$X <- NULL

sim_non_scratch <- rbind(sim_pullblanket1, sim_pullblanket2, sim_turnover1, sim_turnover2, sim_stretch1, sim_stretch2, sim_walk1, sim_walk2)


sim_test_data <- rbind(sim_scrType1234, sim_non_scratch)

sim_test_data$time <- c( (1:length(sim_test_data$time))*(1/50)*1000 )



########

range <- c( # scratch
  3128600, 3131744, # 왼쪽 입가쪽 긁기 
  6715609, 6718766, # 오른팔 상완 긁기
  21753910, 21762920, # 오른쪽 볼 긁기 - 얼굴은 왼쪽 향함 
  22402240, 22406900, # 배에 손둠 + 오른쪽 볼 긁기 + 오른쪽으로 돌아높기(마지막에)
  22547660, 22552970 # 오른쪽 볼 긁기 + 똑바로 눕기 + 긁기 반동
  )


range2 <- c( # non-scratch
  6498048, 6498048+11530, # 왼쪽으로 돌아눕기 
  14566637, 14566637 + 9753, # 다리 내리기, 이불 정리, 주먹 쥐기
  20070332, 20070332 + 6765, # 다리 움직임, 오른쪽 돌아눕기 
  22648137, 22648137 + 5768, # 배에 손두고, 긁기 반동
  22527186, 22527186 + 8357 # 머리 다듬기 3회
)

combinded <-function (data, range, idx=1)
{
  sum_data <- 0
  
  num<-length(range)/2
  print(num)
  for(i in 1:num)
  {
    data.sub <- data
    start_hour <- range[2*i-1] - 1000*10
    end_hour <- range[2*i] + 1000*20
    print(paste("s",start_hour,"e",end_hour))
    data.sub <- subset(data.sub, subset=(data.sub$time < end_hour ))
    data.sub <- subset(data.sub, subset=(data.sub$time > start_hour ))
    res <- createPlot(data.sub, idx, "test", FALSE, "p3")

    if(i==1)
      sum_data <- data.sub
    else{
      sum_data <- rbind(sum_data, data.sub)
    }
  }
  return(sum_data)
}


data_combinded.sub <- subset(data_combinded,grepl(list[1], data_combinded$type))

data.sub <- subject3_left_data
data.sub <- subset(data.sub, subset=(data.sub$time < end_hour ))
data.sub <- subset(data.sub, subset=(data.sub$time > start_hour ))

length(data.sub$x)

total_combined <- rbind(data_combinded, data_combinded2, simulated_0807_for_features)
total_combined$time <- c( (1:length(total_combined$time))*(1/50)*1000 )

length(combined$x)







# "/Users/jonginlee/Documents/recorded6.wav"
# http://samcarcagno.altervista.org/blog/basic-sound-processing-r/
# http://samcarcagno.altervista.org/blog/basic-sound-processing-r/

addr <- "/Users/jonginlee/Documents/recorded0325_1.wav"
sndObj <- readWave(addr)
# data(wav)  # contains wav$rate, wav$sound
Fs <- sndObj@samp.rate
step <- trunc(5*Fs/1000)             # one spectral slice every 5 ms
window <- trunc(40*Fs/1000)          # 40 ms data window
fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
spg <- specgram(wav$sound, fftn, Fs, window, window-step)
S <- abs(spg$S[2:(fftn*4000/Fs),])   # magnitude in range 0<f<=4000 Hz.
S <- S/max(S)         # normalize magnitude so that max is 0 dB.
S[S < 10^(-40/10)] <- 10^(-40/10)    # clip below -40 dB.
S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
image(t(20*log10(S)), axes = FALSE)  #, col = gray(0:255 / 255))



getMFCCfeatureBy <- function(addr, title, setbtw = FALSE, startS = 1, endS=2, plotting=FALSE)
{
  if(setbtw)
    sndObj2 <- readWave(addr, from=startS, to=endS, units= "seconds")
  else
    sndObj2 <- readWave(addr)
  
  if(plotting)
  {
    sndObj <- readWave(addr)
    str(sndObj)
    s1 <- sndObj@left
    s1 <- s1 / 2^(sndObj@bit -1)
    timeArray <- (0:( length(sndObj@left)-1)) / sndObj@samp.rate
    #View(timeArray)
    df <- data.frame(time_milli = timeArray, amplitude = s1)  
    
    max_value <- round(max(df$time),2)
    spliting <- seq(0,max_value,1)
    
    returnValue <- ggplot(df, aes(x=time_milli, y=amplitude))  +
      geom_line() +
      ggtitle(paste("Sound signal", title)) + 
      #  coord_fixed(ratio=1/4) +
      xlab("Time (second)") +
      ylab("Amplitude")+
      scale_x_continuous(breaks = spliting) +
      #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
      #scale_x_continuous(breaks = seq(start_time,end_time,(as.integer((end_time-start_time)/10)) )) +
      #scale_x_continuous(breaks = 10) +
      theme_bw() +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
            axis.text.x = element_text(angle=40,hjust=1,vjust=1))
    
    if(setbtw){
      rect <- data.frame(xmin=startS, xmax=endS, ymin=-Inf, ymax=Inf)
      returnValue <- returnValue + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, fill="blue", inherit.aes = FALSE)
    }
    print(returnValue)
  }

  m1 <- melfcc(sndObj2, wintime = 0.5,hoptime = 0.25)
  return(m1)
}

createSPlot<-function(addr, title, set_btw=FALSE, start_milli=0, end_milli=0 ,freqprint=FALSE )
{
  
  sndObj <- readWave(addr)
  str(sndObj)
  s1 <- sndObj@left
  s1 <- s1 / 2^(sndObj@bit -1)
  timeArray <- (0:( length(sndObj@left)-1)) / sndObj@samp.rate
  View(timeArray)
  df <- data.frame(time_milli = timeArray, amplitude = s1)  
  
  max_value <- (as.integer(max(df$time)))
  spliting <- seq(0,max_value,max_value/10)
  
  if(set_btw){
    df <- subset(df, time_milli > start_milli & time_milli < end_milli)    
  }
  
  returnValue <- ggplot(df, aes(x=time_milli, y=amplitude))  +
    geom_line() +
    ggtitle(paste("Sound signal", title)) + 
    #  coord_fixed(ratio=1/4) +
    xlab("Time (second)") +
    ylab("Amplitude")+
    scale_x_continuous(breaks = spliting) +
    #  scale_y_continuous(breaks = seq(min_value,max_value,(as.integer((max_value-min_value)/10)) )) +
    #scale_x_continuous(breaks = seq(start_time,end_time,(as.integer((end_time-start_time)/10)) )) +
    #scale_x_continuous(breaks = 10) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  
  print(returnValue)
  
  if(freqprint)
  {
    n <- length(s1)
    p <- fft(s1)
    nUniquePts <- ceiling((n+1)/2)
    p <- p[1:nUniquePts] #select just the first half since the second half 
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
    
    freqArray <- (0:(nUniquePts-1)) * (sndObj@samp.rate / n) #  create the frequency array 
    
    plot(freqArray/1000, 10*log10(p), type='l', col='black', xlab='Frequency (kHz)', ylab='Power (dB)') 
    
    rms_val <- sqrt(mean(s1^2))
    print(rms_val)
    print(sqrt(sum(p)))
  }
}



n <- length(s1)
p <- fft(s1)
nUniquePts <- ceiling((n+1)/2)
p <- p[1:nUniquePts] #select just the first half since the second half
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
freqArray <- (0:(nUniquePts-1)) * (sndObj@samp.rate / n) #  create the frequency array
plot(freqArray/1000, 10*log10(p), type='l', col='black', xlab='Frequency (kHz)', ylab='Power (dB)')
sqrt(mean(s1^2))
sqrt(sum(p))
