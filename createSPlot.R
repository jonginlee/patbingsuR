# "/Users/jonginlee/Documents/recorded6.wav"
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


createSPlot<-function(addr, set_btw=FALSE, start_milli=0, end_milli=0 )
{
  
  sndObj <- readWave(addr)
  str(sndObj)
  s1 <- sndObj@left
  s1 <- s1 / 2^(sndObj@bit -1)
  timeArray <- (0:( length(sndObj@left)-1)) / sndObj@samp.rate
  df <- data.frame(time_milli = timeArray, amplitude = s1)  
  
  max_value <- (as.integer(max(df$time)))
  spliting <- seq(0,max_value,max_value/10)
  
  if(set_btw){
    df <- subset(df, time_milli > start_milli & time_milli < end_milli)    
  }
  
  returnValue <- ggplot(df, aes(x=time_milli, y=amplitude)) +
    geom_line() +
    ggtitle(paste("Sound signal")) + 
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
