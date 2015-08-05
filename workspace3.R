





qplot(data=data, x=as.numeric(data$Gyro_entropy_avg), fill=data$label, geom='histogram', binwidth=0.1) + scale_fill_manual(values=rainbow(12)) 

data4 <- rbind(scr_data0704, nonscr)

data4[,c("p","scrtype")] <- list(NULL)
data4[c(2:length(data4))] <-  lapply((data4[c(2:length(data4))]), as.numeric)
write.csv(data4, "0715data", row.names = FALSE)

scr_data0704.sub <-scr_data0704[sample(nrow(scr_data0704), length(nonscr$label)), ]
data<-scr_data0704
data<-scr_data0704.sub
data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)

total <-sum(data$Gyro_entropy_avg)
data$Gyro_entropy_avg <- data$Gyro_entropy_avg/total

minV <- min(data$Gyro_entropy_avg)
maxV <- max(data$Gyro_entropy_avg)
data$Gyro_entropy_avg  <- (data$Gyro_entropy_avg-minV)/(maxV - minV)

---
data2<-nonscr
data2[c(2:length(data2))] <-  lapply((data2[c(2:length(data2))]), as.numeric)

total <-sum(data2$Gyro_entropy_avg)
data2$Gyro_entropy_avg <- data2$Gyro_entropy_avg/total

minV <- min(data2$Gyro_entropy_avg)
maxV <- max(data2$Gyro_entropy_avg)
data2$Gyro_entropy_avg  <- (data2$Gyro_entropy_avg-minV)/(maxV - minV)

--
length(data2$Gyro_entropy_avg)


--
  
data3[,c("Gyro_entropy_avg")]

createHistFromfeatures("Gyro_entropy_avg", "label", data3, 0.1)

t<-getNormalize(data3, "Gyro_entropy_avg")



scr_data0704.sub <-scr_data0704[sample(nrow(scr_data0704), length(nonscr$label)), ]

p3_left<- read.csv("./data_csv/p3/p3leftdata.csv",sep=",",header=TRUE)
p3_right <- read.csv("./data_csv/p3/p3rightdata.csv",sep=",",header=TRUE)
p2_left<- read.table("./data_csv/p2/p2leftdata.csv",sep=",",header=TRUE)
p2_right <- read.csv("./data_csv/p2/p2rightdata.csv",sep=",",header=TRUE)

P2_P3_data <- rbind(p3_left, p3_right, p2_left, p2_right)

View(P2_P3_data[2:length(P2_P3_data)])
write.csv(P2_P3_data[2:length(P2_P3_data)], "./data_csv/P2_P3_data.csv", row.names = F)

createHistFromfeatures("Gyro_mean_y", "label", p3_left, 0.1)


#----
p3_left.scratch <- subset(p3_left, subset=(p3_left$label == "scratch") )
p3_left.non <- subset(p3_left, subset=(p3_left$label == "non") )
p3_left.non.sub <-p3_left.non[sample(nrow(p3_left.non), length(p3_left.scratch$label)), ]

p3_left_fair_data <- rbind(p3_left.non.sub,p3_left.scratch)

View(p3_left_fair_data)
p3_left_fair_data <- p3_left_fair_data[2:length(p3_left_fair_data)]
View(data3)
View(p3_left.non)

createHistFromfeatures("Gyro_autocor2_y", "label", p3_left_fair_data, 0.1, normalized = FALSE, title = "0727_p3_data")


colnamelist<-colnames(p3_left_fair_data)
colnamelist
res<-vector("list",2)
names(res)<-c("feature","distance")

for(i in 2: length(colnamelist))
{
  colname <- colnamelist[i]
  dis <- createHistFromfeatures(colname, "label", p3_left_fair_data, 0.1, normalized = FALSE,title = "0727_p3_data_0.1")
  res$feature[i-1] <- colname
  res$distance[i-1] <- dis
}

res <- as.data.frame(res)
t<-res[order(res$distance,decreasing=TRUE),]
t[c(1:40),]

length(res$distance)

-----

test[c("Gyro_entropy_avg")][1]<-123
View(test)



colnamelist<-colnames(data3)
res<-vector("list",2)
names(res)<-c("feature","distance")
createHistFromfeatures("Linearaccel_th_y", "scrtype", data3, 0.1, normalized = FALSE, title = "0727_p3_data")

for(i in 2: length(colnamelist))
{
  colname <- colnamelist[i]
  dis <- createHistFromfeatures(colname, "scrtype", data3, 0.1,scratch=FALSE,normalized = FALSE)
 # res$feature[i-1] <- colname
 # res$distance[i-1] <- dis
}

t<-res[order(res$distance,decreasing=TRUE),]
t[c(1:15),]

getNormalize<-function(data, afeature)
{
  minV <- min(data[,c(afeature)])
  maxV <- max(data[,c(afeature)])
  return ( (data[,c(afeature)]-minV)/(maxV - minV) )
}

library("monomvn")

createHistFromfeatures <- function(afeature, afillitem, data, bwidth, normalized = TRUE , scratch =TRUE, title ="noname" ,saveFile=FALSE)
{
  if(normalized)
  {
    data[,c(afeature)]<-getNormalize(data, afeature)
    desc<-paste("binwidth(",bwidth,")")
    xdesc<-"Normalized Feature Value (0~1)"
  }else{
    desc<-paste("not normalized, binwidth(",bwidth,")")
    minV <- min(data[,c(afeature)])
    maxV <- max(data[,c(afeature)])
    bwidth <- (maxV - minV)/(bwidth*100)
    xdesc <- paste("Range (",minV,"~",maxV,")")
  }
  
  kldis<-0
  
  
  if(scratch){
    scr_data <- subset(data, subset<-(data$label == "scratch"))
    non_data <- subset(data, subset<-(data$label == "non"))
    kldis<-kl.norm(mean(scr_data[,c(afeature)]),cov(as.matrix(scr_data[,c(afeature)])),mean(non_data[,c(afeature)]),cov(as.matrix(non_data[,c(afeature)])) )
    print(paste(afeature," , kldis",kldis))
    desc <- paste(desc,", kldis-",kldis)
  }
  
  
  resValue <- qplot(data=data, x=data[,c(afeature)], fill=as.factor(data[,c(afillitem)]), geom='histogram', binwidth=bwidth) + 
    scale_fill_manual(values=rainbow(12)) + 
    labs(list(title = paste(afeature, ",",desc ), x = xdesc, y = "Frequency ", fill = "class")) +
    theme_bw() +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  
  if(normalized)
    resValue <- resValue <- xlim(0, 1)
  
  if(saveFile){
    pdf(paste("./plot_results/",title,"_", afeature,"-",kldis,".pdf",sep=""))
    print(resValue)
    dev.off()
  }else{
    print(resValue)  
  }
  
  return(kldis)
}




data3 <- rbind(data,data2)

KL.dist(data[, c("Gyro_entropy_avg")], data2[, c("Gyro_entropy_avg")], k=10)

typeof(data[, c("Gyro_entropy_avg")])
typeof(data2[, c("Gyro_entropy_avg")])

data2<-as.data.frame(data2)


length(data[, c("Gyro_entropy_avg")])
length(data2[, c("Gyro_entropy_avg")])

qplot(data=data4, x=as.numeric(data4$Gyro_entropy_avg), fill=data4$label, geom='histogram', binwidth=0.3) + scale_fill_manual(values=rainbow(12)) 


