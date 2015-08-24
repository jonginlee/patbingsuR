

sim_scr_data0812
sim_nonscr_data

getCombinedFair<-function(data1, data2)
{
  if(length(data1)!=length(data2)){
    
    print(paste("[ERROR] length(data1)!=length(data2) ----- data1 len",length(data1), " / data2 len",length(data2) ))
    return(NULL)
  }
  data1<-as.data.frame(data1)
  data1.sub <-data1[sample(length(data1$label), length(data2$label)), ]
  print("spliting complete!")
#  View(data1.sub)
#  View(data2)
  data1.sub <- as.data.frame(data1.sub)
  data2 <- as.data.frame(data2)
  colnames(data1.sub) <- colnames(data2)
  
  res <- rbind(as.data.frame(data1.sub),as.data.frame(data2))
#  View(res)
  return(res)
}


data1 <- getNonscratch(total, c(1,3,8)) # 499
data2 <- getNonscratch(turnoverdata, c(1,3,8)) # 148
data3 <- getNonscratch(pullblankdata, c(1,3,8)) # 46
data4 <- getNonscratch(stretchdata, c(1,3,8)) # 77
data5 <- getNonscratch(walkdata, c(1,3,8)) # 206


data1v2 <- getNonscratch(total, c(1,3,8)) # 295
data2v2 <- getNonscratch(turnoverdata, c(1,3,8)) # 53
data3v2 <- getNonscratch(pullblankdata, c(1,3,8)) # 14
data4v2 <- getNonscratch(stretchdata, c(1,3,8)) # 28
data5v2 <- getNonscratch(walkdata, c(1,3,8)) # 186

write.csv(data1v2, "./data_csv/data1v2.txt")
write.csv(data2v2, "./data_csv/data2v2.txt")
write.csv(data3v2, "./data_csv/data3v2.txt")
write.csv(data4v2, "./data_csv/data4v2.txt")
write.csv(data5v2, "./data_csv/data5v2.txt")

write.csv(data1v3, "./data_csv/data1v3_0814.txt")
write.csv(data2v3, "./data_csv/data2v3_0814.txt")
write.csv(data3v3, "./data_csv/data3v3_0814.txt")
write.csv(data4v3, "./data_csv/data4v3_0814.txt")
write.csv(data5v3, "./data_csv/data5v3_0814.txt")
length(data1v3$label); length(data2v3$label) ; length(data3v3$label) ;length(data4v3$label) ;length(data5v3$label)

data1v2 <- data1v3
data2v2 <- data2v3
data3v2 <- data3v3
data4v2 <- data4v3
data5v2 <- data5v3


data <- getDataN(folderlist, ranglist) # 1895

length(scrdata$label)
data_tmp <- as.data.frame(scrdata)
data <- scrdata
View(data_tmp)
data_tmp[c(5:length(data_tmp))] <-  lapply((data_tmp[c(5:length(data_tmp))]), as.numeric)
data_tmp[c(1:3)] <-  lapply((data_tmp[c(1:3)]), as.character)
#data_tmp[4] <- lapply((data_tmp[4]), as.factor)
write.csv(data_tmp, "./data_csv/real_data0814v3.txt")


data.scrtype1 <- subset(data,grepl(1, data$scrtype))
length(data.scrtype1$label) # 456
data.scrtype2 <- subset(data,grepl(2, data$scrtype))
length(data.scrtype2$label) # 483
data.scrtype3 <- subset(data,grepl(3, data$scrtype))
length(data.scrtype3$label) # 480
data.scrtype4 <- subset(data,grepl(4, data$scrtype))
length(data.scrtype4$label) # 476



length(sim_nonscr_data$label)
#26,27, 26, 27
data.scrtype1 <-as.data.frame(data.scrtype1)
data.scrtype1 <-data.scrtype1[sample(length(data.scrtype1$label), 14), ]
length(data.scrtype1$label)

data.scrtype2 <-as.data.frame(data.scrtype2)
data.scrtype2 <-data.scrtype2[sample(length(data.scrtype2$label), 14), ]
length(data.scrtype2$label)

data.scrtype3 <-as.data.frame(data.scrtype3)
data.scrtype3 <-data.scrtype3[sample(length(data.scrtype3$label), 14), ]
length(data.scrtype3$label)

data.scrtype4 <-as.data.frame(data.scrtype4)
data.scrtype4 <-data.scrtype4[sample(length(data.scrtype4$label), 14), ]
length(data.scrtype4$label)

sim_scr_data <- getCombinedFair(rbind(data.scrtype1, data.scrtype2), rbind(data.scrtype3, data.scrtype4))
length(sim_scr_data$label)

sim_nonscr_data <- getCombinedFair(data5v2, data2v2)

data5v2$X <- NULL
data5v2.walking <-as.data.frame(data5v2)
data5v2.walking <-data5v2.walking[sample(length(data5v2.walking$label), 14), ]
length(data5v2.walking$label)
data5v2.walking$scrtype <- 5

data2v2$X <- NULL
data2v2.turnover <-as.data.frame(data2v2)
data2v2.turnover<-data2v2.turnover[sample(length(data2v2.turnover$label), 14), ]
length(data2v2.turnover$label)
data2v2.turnover$scrtype <- 6

data4v2$X <- NULL
data4v2.stretch <-as.data.frame(data4v2)
data4v2.stretch <-data4v2.stretch[sample(length(data4v2.stretch$label), 14), ]
length(data4v2.stretch$label)
data4v2.stretch$scrtype <- 7

data3v2$X <- NULL
data3v2$scrtype <- 8
sim_nonscr_data <- rbind(data2v2.turnover, data5v2.walking, data4v2.stretch, as.data.frame(data3v2))
length(sim_nonscr_data$label)

data.scrtype1$scrtype<-NULL
data.scrtype1$p<-NULL
data.scrtype2$scrtype<-NULL
data.scrtype2$p<-NULL
data.scrtype3$scrtype<-NULL
data.scrtype3$p<-NULL
data.scrtype4$scrtype<-NULL
data.scrtype4$p<-NULL

total <- getCombinedFair(data.scrtype1, sim_nonscr_data)
total <- getCombinedFair(data.scrtype2, sim_nonscr_data)
total <- getCombinedFair(data.scrtype3, sim_nonscr_data)
total <- getCombinedFair(data.scrtype4, sim_nonscr_data)

sim_scr_data <- data.scrtype4
length(data.scrtype2$label)
sim_scr_data$scrtype<-NULL
sim_scr_data$p<-NULL
total <- getCombinedFair(sim_scr_data, sim_nonscr_data)

total$scrtype
total$epoches<-NULL
total$start_milli<-NULL
total$end_milli<-NULL
rownames(total)<-NULL
total[c(2:length(total))] <-  lapply((total[c(2:length(total))]), as.numeric)
total$label <- as.factor(total$label)
total <- as.data.frame(total)
set.seed(001)
total <-total[sample(length(total$label)), ]
total <-total[sample(length(total$label)), ]
View(total)
write.arff(total, file=paste("./data_csv/sim_data0812_non_scrset1_total.arff",sep=""), relation = "sim_walk_turnover_pullblank_stretch_scr_type1234")
write.arff(total, file=paste("./data_csv/sim_data0814_non_scrset1_total.arff",sep=""), relation = "sim_walk_turnover_pullblank_stretch_scr_type1234_08014")



data.sub <- subset(data.sub, subset=(data.sub$time < (e_time - as.numeric(endMilli)) ))
