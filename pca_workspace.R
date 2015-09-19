test <- read.table("./data_raw/jonginlee_data0623/data1.txt",sep=",",header=TRUE)
res <- createPlot(test, 8, "test", TRUE, "p3")
print(res)
test.sub <- subset(test, grepl(list[8], test$type))
end_hour <- 26000
start_hour <- 23000
test.sub <- subset(test.sub, subset=(test.sub$time < end_hour ))
test.sub <- subset(test.sub, subset=(test.sub$time > start_hour ))

res <- createPlot(test.sub, 8, "test", TRUE, "p3")
print(res)
test.sub$x <- filtfilt(bf, test.sub$x)
test.sub$y <- filtfilt(bf, test.sub$y)
test.sub$z <- filtfilt(bf, test.sub$z)


test.sub$mag <- sqrt( (test.sub$x+100)^2 + (test.sub$y+100)^2 + (test.sub$z+100)^2)
test.sub$mag <- test.sub$mag - mean(test.sub$mag)
plot(test.sub$time, test.sub$mag, type="l")

trans <- preProcess(test.sub[,3:5], method=c("BoxCox", "center", "scale", "pca"))
vmdata <- varimax(trans$rotation)
vmdata$loadings

temp_m<-list()
i<-1
for(i in 1:length(test.sub$x)){
  temp <-   c(test.sub$x[i], test.sub$y[i], test.sub$z[i]) %*% vmdata$loadings
  temp_m$x[i] <- temp[1]
  temp_m$y[i] <- temp[2]
  temp_m$z[i] <- temp[3]
  
  #print(i)
}
anyNA(temp_m)

plot(temp_m$x, type="l")
plot(test.sub$x, type="l")
  
PC <- predict(trans, test.sub[,3:5])
plot(1:length(PC$PC1), PC$PC1, type="l")

fc = 5
fs = 50

bf <- butter(10, (2*fc)/(fs), type="low")

plot(test.sub$time, test.sub$x, type="l", col="red")
points(test.sub$time, test.sub$y, col="green", type="l")
points(test.sub$time, test.sub$z, col="black", type="l")

plot(test.sub$time, filtfilt(bf, test.sub$x), type="l", col="red")
points(test.sub$time, filtfilt(bf, test.sub$y), col="green", type="l")
points(test.sub$time, filtfilt(bf, test.sub$z), col="black", type="l")

df$magnitude <- filtfilt(bf,df$magnitude)


fc = 1000; % Cut-off frequency (Hz)
fs = 8192; % Sampling rate (Hz)
order = 5; % Filter order
[B,A] = butter(order,2*fc/fs); % [0:pi] maps to [0:1] here
















