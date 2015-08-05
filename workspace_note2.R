
showAccuracy <- function (dataset, f_indexces, ML_method)


scr_data <- subset(scr_data0703, subset=(scr_data0703$p != 3 ))



 
data <- rbind(scr_data0704, nonscr) 

#--
p2_left<- read.table("./data_csv/p2/p2leftdata.csv",sep=",",header=TRUE)
p2_right<- read.table("./data_csv/p2/p2rightdata.csv",sep=",",header=TRUE)

t<-subset(p2_left, subset=(p2_left$label == "non"))
t<-t[2:length(t)]
t$p<-0; t$scrtype<-0

nonscrP2non <- rbind(nonscr,t)

t<-subset(p2_right, subset=(p2_right$label == "non"))
t<-t[2:length(t)]
t$p<-0; t$scrtype<-0

nonscrP2non <- rbind(nonscrP2non,t)

length(nonscrP2non$label)
length(scr_data0704P2scr$label)

#--
t<-subset(p2_left, subset=(p2_left$label == "scratch"))
t<-t[2:length(t)]
t$p<-0; t$scrtype<-0

scr_data0704P2scr <- rbind(scr_data0704,t)

t<-subset(p2_right, subset=(p2_right$label == "scratch"))
t<-t[2:length(t)]
t$p<-0; t$scrtype<-0

scr_data0704P2scr <- rbind(scr_data0704P2scr,t)

#---

scr_data0704P2scr.sub <-scr_data0704P2scr[sample(nrow(scr_data0704P2scr), length(nonscrP2non$label)), ]

p2Wsim <- rbind (scr_data0704P2scr.sub,nonscrP2non )

#==

data <- data3
data <- p2Wsim
#data <- rbind(nonscr_data, scr_data)
#View(data)
selected <- data[c(1,49,50,51,82,83,84,88,111,112,113,123,124,125)]
selected <- data[c(1,138,139,141,142)]
selected <- data[c(1,139,142)]
selected <- data[c(1,138,139,141,142, 91,92,94,95)]
selected <- data[c(1,91,92,94,95)]
selected <- data[c(1,117,118,119,120,121,122,126,127,128,129,130,131)]
selected <- data[c(1,70,71,72,73,74,75,79,80,81,82,83,84,117,118,119,120,121,122,126,127,128,129,130,131)]
selected <- data[c(1,119,138,74,118,117,71,44,24,70,25,35,26,94,89,91,23,103,33,15,114)]
selected <- data[c(1,49,50,51,82,83,84,88,138,139,141,142)]


featuresIdx <- c(1,138,139,141,142)
model<-getModelFrom(p2Wsim, featuresIdx,"J48")
showResult(p3_left, featuresIdx,model)

scr_test_data <- subset(scr_data0703, subset=(scr_data0703$p == 3 ))
scr_test_data <- subset(scr_test_data, subset=(scr_test_data$scrtype == 4 ))

p3_left<- read.csv("./data_csv/p3/p3leftdata.csv",sep=",",header=TRUE)
p3_right <- read.csv("./data_csv/p3/p3rightdata.csv",sep=",",header=TRUE)
scr_test_data<-p3_left[2:length(p3_left)]
scr_test_data<-p3_right[2:length(p3_right)]


p2_left<- read.table("./data_csv/p2/p2leftdata.csv",sep=",",header=TRUE)
p2_right <- read.csv("./data_csv/p2/p2rightdata.csv",sep=",",header=TRUE)
scr_test_data<-p2_left[2:length(p2_left)]
scr_test_data<-p2_right[2:length(p2_right)]


p3_left<- read.csv("./data_csv/p3/filteringAccel//p3leftdata_0.01.csv",sep=",",header=TRUE)


scr_test_data<-p2_left[2:length(p2_left)]
scr_test_data<-p2_right[2:length(p2_right)]


scr_test_data<-sub_p3_left[2:length(sub_p3_left)]
scr_test_data<-sub_p3_left_max_5[2:length(sub_p3_left_max_5)]
scr_test_data<-sub_p3_left_max_7[2:length(sub_p3_left_max_7)]
scr_test_data<-sub_p3_left_max_9[2:length(sub_p3_left_max_9)]
scr_test_data<-sub_p3_left_max_11[2:length(sub_p3_left_max_11)]


selected <- scr_test_data[c(1,49,50,51,82,83,84,88,111,112,113,123,124,125)]
selected <- scr_test_data[c(1,138,139,141,142)]
selected <- scr_test_data[c(1,139,142)]
selected <- scr_test_data[c(1,138,139,141,142, 91,92,94,95)]
selected <- scr_test_data[c(1,91,92,94,95)]

selected <- scr_test_data[c(1,117,118,119,120,121,122,126,127,128,129,130,131)]
selected <- scr_test_data[c(1,70,71,72,73,74,75,79,80,81,82,83,84,117,118,119,120,121,122,126,127,128,129,130,131)]

selected <- scr_test_data[c(1,119,138,74,118,117,71,44,24,70,25,35,26,94,89,91,23,103,33,15,114)]

selected <- scr_test_data[c(1,49,50,51,82,83,84,88,138,139,141,142)]

data<-as.data.frame(selected)
data$label <- factor(data$label, levels = c("non", "scratch"))
data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
pd<-predict(gmodel0724_3, data[2:length(data)])
pd<-predict(model, data[2:length(data)])


traindata<-p2Wsim
featureIdxs <- c(1,138,139,141,142) # related work
ML_method<-"J48"

getModelFrom <- function( traindata, featureIdxs, ML_method)
{
  selected <- traindata[featureIdxs]
  
  data<-as.data.frame(selected)
  data$label <- factor(data$label)
  data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
  train_control <- trainControl(method="cv", number=10)
  if(ML_method == "J48")
    model <- train(label~., data=data, trControl = train_control,  method="J48")
  else if(ML_method == "glm")
    model <- train(label~. , data=data, trControl = train_control,family = binomial, method="glm")
  else
    model <- NULL
  
  return(model)  
}

showResult<-function(testData, featureIdxs, model )
{
  scr_test_data<-testData[2:length(testData)]
  selected <- scr_test_data[featureIdxs]
  
  data<-as.data.frame(selected)
  data$label <- factor(data$label, levels = c("non", "scratch"))
  data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
  pd<-predict(model, data[2:length(data)])
  cm <- confusionMatrix(pd, data$label)
  print(cm)
  return(pd)
}



data3

p2Wsim
featureIdxs <- c(1,138,92,141,95) # gyro periodic pattern
featureIdxs <- c(1,117,118,119,  73,74,75,  126,127,128, 82,83,84) # gyro periodic pattern 3 axis

featureIdxs <- c(1,117,118,119, 120,121,122, 126,127,128,  82,83,84) # related work 3-axis


featureIdxs <- c(1,138,90,92,141,95) # gyro periodic pattern autocor (mag=1, mag=12)

featureIdxs <- c(1,138,139,141,142) # related work
featureIdxs <- c(1,117,118,119, 120,121,122, 126,127,128,  129,130,131) # related work 3-axis


featureIdxs <- c(1,138,139,141,142, 91,92,94,95) # related work +  gyro features


model<-getModelFrom(p2Wsim, featureIdxs,"J48")
testData<-p3_left
pd<-showResult(p3_left, featureIdxs,model)


featuresIdx <- c(1,83,84,73,74,68,69,138,141) # related work
model<-getModelFrom(p2Wsim, featuresIdx,"glm")
showResult(p3_left, featuresIdx,model)
summary(model)

featuresIdx <- c(1,95,138,139,141) # related work
model<-getModelFrom(p2Wsim, featuresIdx,"J48")
showResult(p3_left, featuresIdx,model)
summary(model)


p2Wsim$p <- NULL
p2Wsim$scrtype <- NULL
View(p2Wsim)
rownames(p2Wsim) <- NULL

data<-data3
data$p<-NULL
data$scrtype <- NULL
write.csv(as.data.frame(data), file="./data_csv/dataTrain_with_p2.csv" ,row.names=F)
write.csv(as.data.frame(data), file="./data_csv/dataTrain.csv",row.names=F)


for( i in 1:length(pd))
{
 if( ("scratch" == pd[i]) & ("scratch" == data$label[i]))
# if(pd[i] != data$label[i])
#  if( ("non" == pd[i]) & ("scratch" == data$label[i]) )
  {
    print(paste("index", i, "pd - ",pd[i], ", data$label - ", data$label[i]))
  }
}

cm <- confusionMatrix(pd, data$label)
cm
cm$overall[1]


#===
 
p3_left<- read.csv("./data_csv/p3/p3leftdata.csv",sep=",",header=TRUE)
View(p3_left)
p3_gyro_4<- read.csv("./data_csv/p3/subject3_data_gyro.csv",sep=",",header=TRUE)
p3_gyro_5<- read.csv("./data_csv/p3/subject3_data_gyro_max_5.csv",sep=",",header=TRUE)
p3_gyro_7 <- read.csv("./data_csv/p3/subject3_data_gyro_max_7.csv",sep=",",header=TRUE)
p3_gyro_9 <- read.csv("./data_csv/p3/subject3_data_gyro_max_9.csv",sep=",",header=TRUE) #==> loss
p3_gyro_11 <- read.csv("./data_csv/p3/subject3_data_gyro_max_11.csv",sep=",",header=TRUE)


sub_p3_left <- list()
sub_p3_left_max_5 <- list()
sub_p3_left_max_7 <- list()
sub_p3_left_max_9 <- list()
sub_p3_left_max_11 <- list()


length(subset(sub_p3_left_max_5$label, subset=(sub_p3_left_max_5$label == "scratch")) )


for( i in 1:length(p3_gyro_11$label))
{
  if(p3_gyro_11$label[i] != "turningover_NA") {
    sub_p3_left_max_11 <- rbind(p3_left[i,],sub_p3_left_max_11)
  }
}


for( i in 1:length(pd))
{
  
  # if( ("scratch" == pd[i]) & ("scratch" == data$label[i]))
  # if(pd[i] != data$label[i])
  if( ("scratch" == pd[i]) & ("non" == data$label[i]) )
  {
    print(paste("index", i, "pd - ",pd[i], ", data$label - ", data$label[i]))
  }
}








