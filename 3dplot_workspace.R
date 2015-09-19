
deltaPlot <- function(data, idx, graph_title, saveFile, 
                      set_btw=FALSE, start_hour=1.1, end_hour=1.1) {
  data.sub <- data
  if(set_btw){
    data.sub <- subset(data.sub, subset=(data.sub$time < end_hour ))
    data.sub <- subset(data.sub, subset=(data.sub$time > start_hour ))
  }
  
  if(saveFile)
    write.csv(data.sub, file=paste("./data_raw/",graph_title,".txt",sep=""), row.names=T)
  
  if(set_btw)
    graph_title <- paste(graph_title, "(", start_hour, " ~ ", end_hour, ")")
  
  data.sub <- subset(data.sub,grepl(list[idx], data.sub$type))
  
  max_value <- (as.integer(max(data.sub$time)))
  spliting <- seq(0,max_value,max_value/10)
  xlablename <- "time (millisecond)"
  
  data.sub2 <- list()
  
  for(i in 2:length(data.sub$x))
  {
    data.sub2$x[i-1] <- (data.sub$x[i] - data.sub$x[i-1]) / (data.sub$time[i]-data.sub$time[i-1])
    data.sub2$y[i-1] <- (data.sub$y[i] - data.sub$y[i-1]) / (data.sub$time[i]-data.sub$time[i-1])
    data.sub2$z[i-1] <- (data.sub$z[i] - data.sub$z[i-1]) / (data.sub$time[i]-data.sub$time[i-1])
  }
  
  data.sub3 <- list()
  
  sumx<-0
  sumy<-0
  sumz<-0
  for(i in 1:length(data.sub$x))
  {
    sumx <- sumx + data.sub$x[i]
    data.sub3$x[i] <- sumx 
    sumy <- sumy + data.sub$y[i]
    data.sub3$y[i] <- sumy
    sumz <- sumz + data.sub$z[i]
    data.sub3$z[i] <- sumz
  }
  df1 <- data.frame(time =(1:length(data.sub$time) )*(1/50)*1000, x=data.sub3$x, y=data.sub3$y, z=data.sub3$z)
  
  returnValue1 <- ggplot(df1, aes(x=time,colour="axis")) +
    geom_line(aes(y=x, colour="X")) +
    geom_line(aes(y=y, colour="Y")) +
    geom_line(aes(y=z, colour="Z")) +
    ggtitle(paste(graph_title," (integrated )",sep="")) + 
    scale_color_manual(values=c("red","blue","violet")) +
    xlab("integrated ") +
    ylab(y_label_list[idx]) +
    scale_x_continuous(breaks = spliting) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  
  print(returnValue1)
  
  df2 <- data.frame(time =(1:length(data.sub$time) )*(1/50)*1000, x=data.sub$x, y=data.sub$y, z=data.sub$z)
  
  returnValue2 <- ggplot(df2, aes(x=time,colour="axis")) +
    geom_line(aes(y=x, colour="X")) +
    geom_line(aes(y=y, colour="Y")) +
    geom_line(aes(y=z, colour="Z")) +
    ggtitle(paste(graph_title," (",sensor_name_list[idx],")",sep="")) + 
    scale_color_manual(values=c("red","blue","violet")) +
    xlab(xlablename) +
    ylab(y_label_list[idx]) +
    scale_x_continuous(breaks = spliting) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  
  print(returnValue2)
  
  df3 <- data.frame(time =(1:(length(data.sub$time)-1) )*(1/50)*1000, x=data.sub2$x, y=data.sub2$y, z=data.sub2$z)
  
  returnValue3 <- ggplot(df3, aes(x=time,colour="axis")) +
    geom_line(aes(y=x, colour="X")) +
    geom_line(aes(y=y, colour="Y")) +
    geom_line(aes(y=z, colour="Z")) +
    ggtitle(paste(graph_title," (acceleration differentiation)",sep="")) + 
    scale_color_manual(values=c("red","blue","violet")) +
    xlab(xlablename) +
    ylab("m/s^3") +
    scale_x_continuous(breaks = spliting) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  
  print(returnValue3)
}


test <- read.table("../patbingsuR/data_raw/jonginlee_data0623/",sep=",",header=TRUE)
res <- createPlot(subject3_left_data, 8, "test", FALSE, "p3", set_btw = T, start_hour = 21752813 + 3000, end_hour =21752813 + 3000 + 3000 )

test.sub <- subset(subject3_left_data, grepl(list[8], subject3_left_data$type))
test.sub <- subset(test.sub, subset=(test.sub$time < 21752813 + 3000 + 3000 ))
test.sub <- subset(test.sub, subset=(test.sub$time >  21752813 + 3000 ))

print(res)

res <- createPlot(total_combined, 8, "test", FALSE, "p3" )
res <- createPlot(sim_test_data, 8, "test", FALSE, "p3" )
res <- createPlot(sim_test_data, 8, "test", FALSE, "p3", set_btw = T, start_hour =1600000, end_hour =1603000 ,cutoff = F)

print(res)

test <- total_combined
test <- sim_test_data
test.sub <- subset(test, grepl(list[8], test$type))
end_hour <- 41000
start_hour <- 38000

end_hour <- 72000
start_hour <- 69000

#not move, sim_test_data
start_hour <- 555000; end_hour <- 558000

#pull blanket, sim_test_data
start_hour <- 577000; end_hour <- 580000

# turn over, sim_test_data
start_hour <- 825000 ; end_hour <- 828000

# stretch, sim_test_data
start_hour <- 1370000 ; end_hour <- 1373000

# walking, sim_test_data
start_hour <- 1600000 ; end_hour <- 1603000

# real scratch, total_combined




test.sub <- subset(test.sub, subset=(test.sub$time < end_hour ))
test.sub <- subset(test.sub, subset=(test.sub$time > start_hour ))

View(test.sub)

res <- createPlot(test.sub, 8, "test", FALSE, "p3",cutoff = F)
print(res)
getVariancefrom3D(test.sub, TRUE)



test.sub$x <- filtfilt(bf, test.sub$x)
test.sub$y <- filtfilt(bf, test.sub$y)
test.sub$z <- filtfilt(bf, test.sub$z)


test.sub$mag <- sqrt( (test.sub$x+100)^2 + (test.sub$y+100)^2 + (test.sub$z+100)^2)
test.sub$mag <- test.sub$mag - mean(test.sub$mag)
plot(test.sub$time, test.sub$mag, type="l")

trans <- preProcess(test.sub[,3:5], method=c("BoxCox", "center", "scale", "pca"), thresh = 0.99999999)
vmdata <- varimax(trans$rotation)

temp_m<-list()
i<-1
for(i in 1:length(test.sub$x)){
  temp <-  c(test.sub$x[i], test.sub$y[i], test.sub$z[i]) %*%  trans$rotation
  temp_m$x[i] <- temp[1]
  temp_m$y[i] <- temp[2]
  temp_m$z[i] <- temp[3]
  #print(i)
}

open3d()
plot3d(temp_m$x, temp_m$y, temp_m$z, type="h")
spheres3d(temp_m$x, temp_m$y, temp_m$z, radiu=0.05, col=rainbow(length(test.sub$x)))
grid3d(side="z", at=list(z=0))
grid3d(side="x", at=list(x=0))
grid3d(side="y", at=list(y=0))


library("rgl")
open3d()
plot3d(test.sub$x, test.sub$y, test.sub$z, type="h")
spheres3d(test.sub$x, test.sub$y, test.sub$z, radiu=0.05, col=rainbow(length(test.sub$x)))
grid3d(side="z", at=list(z=0))
grid3d(side="x", at=list(x=0))
grid3d(side="y", at=list(y=0))

getVariancefrom3D <- function(data ,plotting=FALSE, radius=0.1 )
{
  #plot3d(x=data$x, y=data$y, z=data$z, type="s", col=rainbow(length(data$z))[rank(data$z)],radius=0.1, zlab="")
  npp<-10
  fit1=lm(data = data, z~x+y)
  y<-predict(fit1, newdata=data[c('x','y')])
  RMS1<-sqrt(sum((data$z-y)^2)/length(data$z)) # root mean sqaure
  
  
  print(paste("RMS1 z~x+y", RMS1))
  
  fit2=lm(data = data, x~z+y)
  y<-predict(fit2, newdata=data[c('z','y')])
  RMS2<-sqrt(sum((data$x-y)^2)/length(data$x)) # root mean sqaure
  print(paste("RMS2 x~z+y", RMS2))
  
  fit3=lm(data = data, y~x+z)
  y<-predict(fit3, newdata=data[c('x','z')])
  RMS3<-sqrt(sum((data$y-y)^2)/length(data$y)) # root mean sqaure
  print(paste("RMS3 y~x+z", RMS3))
  
  s<-which.max(c(RMS1, RMS2, RMS3))
  
  if(plotting==TRUE)
  {
    if(s==1){
      open3d()
      
      plot3d(x=data$x, y=data$y, z=data$z, type="s", col=rainbow(length(data$z))[rank(data$z)],radius=radius, zlab="")
      grd <- expand.grid(x=seq(min(data$x),max(data$x),length.out=npp),
                         y=seq(min(data$y),max(data$y),length.out=npp) )
      grd$pred <-predict(fit1, newdata=grd)
      persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
              z=matrix(grd[[3]],npp,npp),  color="lightgrey",alpha=0.7, lit=T, back="lines", add=TRUE)
      
    }else if(s==2){
      open3d()
      
      plot3d(x=data$x, y=data$y, z=data$z, type="s", col=rainbow(length(data$x))[rank(data$x)],radius=radius, xlab="")
      grd <- expand.grid(z=seq(min(data$z),max(data$z),length.out=npp),
                         y=seq(min(data$y),max(data$y),length.out=npp) )
      grd$pred <-predict(fit2, newdata=grd)
      persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
              z=matrix(grd[[3]],npp,npp),  color="lightgrey",alpha=0.7, lit=T, back="lines", add=TRUE)
      
    }else if(s==3){
      open3d()
      
      plot3d(x=data$x, y=data$y, z=data$z, type="s", col=rainbow(length(data$y))[rank(data$y)],radius=radius, ylab="")
      grd <- expand.grid(x=seq(min(data$x),max(data$x),length.out=npp),
                         z=seq(min(data$z),max(data$z),length.out=npp) )
      grd$pred <-predict(fit3, newdata=grd)
      persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
              z=matrix(grd[[3]],npp,npp),  color="lightgrey",alpha=0.7, lit=T, back="lines", add=TRUE)
    }
    
    grid3d(side="z", at=list(z=0))
    grid3d(side="x", at=list(x=0))
    grid3d(side="y", at=list(y=0))
  }
  
}


open3d()
fit=lm(data = test.sub,y~z+x)
npp=10
plot3d(x=test.sub$x, y=test.sub$y, z=test.sub$z, type="s", col=rainbow(length(test.sub$y))[rank(test.sub$y)],radius=0.1, zlab="")
grd <- expand.grid(z=seq(min(test.sub$z),max(test.sub$z),length.out=npp),
                   x=seq(min(test.sub$x),max(test.sub$x),length.out=npp) )
grd$pred <-predict(fit, newdata=grd)
persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
        z=matrix(grd[[3]],npp,npp),  color="lightgrey",alpha=0.7, lit=T, back="lines", add=TRUE)

persp3d(x=unique(grd[[2]]), y=matrix(grd[[3]],npp,npp) , 
        z=unique(grd[[1]]),  color="lightgrey",alpha=0.7, lit=T, back="lines", add=TRUE)


View(matrix(grd[[3]],npp,npp) )

View(grd[[3]])

View(matrix(grd[[3]],npp,npp))
View(unique(grd[[2]]))
View((grd[[2]]))


y<-predict(fit, newdata=temp_m[1:2])
sqrt(sum((temp_m$z-y)^2)/length(temp_m$z)) # root mean sqaure


library(rgl)
BLOOD_PRESSURE=c(132,143,153,162,154,168,137,149,159,128,166)
AGE=c(52,59,67,73,64,74,54,61,65,46,72)
WEIGHT=c(78,83,87,95,88,99,85,85,93,75,98)
fit=lm(BLOOD_PRESSURE~AGE+WEIGHT)
npp=10
plot3d(x=AGE, y=WEIGHT, z=BLOOD_PRESSURE, type="s", col=rainbow(length(BLOOD_PRESSURE))[rank(BLOOD_PRESSURE)],
       radius=1, zlab="")
grd <- expand.grid(AGE=seq(min(AGE),max(AGE),length.out=npp),
                   WEIGHT=seq(min(WEIGHT),max(WEIGHT),length.out=npp) )
grd$pred <-predict(fit, newdata=grd)
persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
        z=matrix(grd[[3]],npp,npp),  color="lightgrey",alpha=0.7, lit=T, back="lines", add=TRUE)


open3d()
fit=lm(data = temp_m,z~x+y)
npp=100
plot3d(x=temp_m$x, y=temp_m$y, z=temp_m$z, type="s", col=rainbow(length(temp_m$z))[rank(temp_m$z)],radius=0.1, zlab="")
grd <- expand.grid(x=seq(min(temp_m$x),max(temp_m$x),length.out=npp),
                   y=seq(min(temp_m$y),max(temp_m$y),length.out=npp) )
grd$pred <-predict(fit, newdata=grd)
persp3d(x=unique(grd[[1]]), y=unique(grd[[2]]), 
        z=matrix(grd[[3]],npp,npp),  color="lightgrey",alpha=0.7, lit=T, back="lines", add=TRUE)




var(temp_m$x) ; var(temp_m$y) ; var(temp_m$z)
var(test.sub$x) ; var(test.sub$y) ; var(test.sub$z)

plot(temp_m$x, type="l")
plot(test.sub$x, type="l")

plot(temp_m$y, type="l")
plot(test.sub$y, type="l")

plot(temp_m$z, type="l")
plot(test.sub$z, type="l")



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