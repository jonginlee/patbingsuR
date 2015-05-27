# utility
data <- data_watch_0521_walking_4_21_17

data <- subset(data, subset=(data$time > 1*1000 ))
data <- subset(data, subset=(data$time < 29*1000 ))

data1 <- subset(data,grepl(list[8], data$type))
data2 <- subset(data,grepl(list[2], data$type))
data3 <- subset(data,grepl(list[3], data$type))
data1<- data1[1:1595,]
data2 <- data2[1:1595,]
data3 <- data3[1:1595,]

nrow(data1)
nrow(data2)
nrow(data3)

df <- data.frame(p = data1$time, 
                 gyroX = data3$x, gyroY = data3$y, gyroZ = data3$z,
                 accelX = data1$x, accelY = data1$y, accelZ = data1$z,  
                 magnetX = data2$x, magnetY = data2$y, magnetZ = data2$z
                 )

colnames(df) <- c("Packet number",    "Gyroscope X (deg/s)",  "Gyroscope Y (deg/s)",	"Gyroscope Z (deg/s)",	"Accelerometer X (g)",	"Accelerometer Y (g)",	"Accelerometer Z (g)",	"Magnetometer X (G)",	"Magnetometer Y (G)",	"Magnetometer Z (G)")

View(df)

write.csv(df, file=paste("./data_raw/testWalkingforMATLAB7.txt",sep=""), row.names=FALSE)


getMilliFromHMS <- function(hour, minute, second, setStartT=FALSE, hour2=0, minute2=0, second2=0)
{
  time <- 0
  time <- time + hour*60*60 + minute*60 + second
  
  if(setStartT)
    time <- time - (hour2*60*60 + minute2*60 + second2)
  
  return (time*1000)
}

getHMSFromMillis <- function(time)
{
  hour <- as.integer(time)
  minute_t <- (time - as.integer(time))*60
  minute <- as.integer(minute_t)
  second_t <- (minute_t - as.integer(minute_t))*60
  second <- as.integer(second_t)
  
  return ( paste(hour,minute,second))
}



createSPlot("./data_raw/data_watch_intentservice1_06_11_24_56.wav", "06_11_24")
createSPlot("./data_raw/data_watch_intentservice1_05_41_15_52.wav", "05_41_15")
createSPlot("./data_raw/data_watch_intentservice1_05_20_24_50.wav", "05_20_24")
createSPlot("./data_raw/data_watch_intentservice1_04_56_31_47.wav", "04_56_31")
createSPlot("./data_raw/data_watch_intentservice1_04_30_57_40.wav", "04_30_57")
createSPlot("./data_raw/data_watch_intentservice1_04_14_48_37.wav", "04_14_48")
createSPlot("./data_raw/data_watch_intentservice1_03_51_24_32.wav", "03_51_24")
createSPlot("./data_raw/data_watch_intentservice1_03_46_06_30.wav", "03_46_06")
createSPlot("./data_raw/data_watch_intentservice1_02_42_11_13.wav", "02_42_11")
createSPlot("./data_raw/data_watch_intentservice1_02_30_41_4.wav", "02_30_41")




