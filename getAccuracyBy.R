library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("pracma", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("quantmod", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("oce", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("signal", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")


data_watch20150515_DATASET1 <- read.table("/Users/jonginlee/Documents/data_watch20150515_DATASET1.txt", sep="," ,header=TRUE)
data_watch20150515_DATASET2 <- read.table("/Users/jonginlee/Documents/data_watch20150515_DATASET2.txt", sep="," ,header=TRUE)
data_watch20150515_DATASET3 <- read.table("/Users/jonginlee/Documents/data_watch20150515_DATASET3.txt", sep="," ,header=TRUE)
data_watch20150515_DATASET4 <- read.table("/Users/jonginlee/Documents/data_watch20150515_DATASET4.txt", sep="," ,header=TRUE)
createPlotV2(data_watch20150515_DATASET4, 1, "data_watch20150515_DATASET4", FALSE, "tes",FALSE, 1335577,1345577)
createPlotV2(data_watch20150515_DATASET3, 1, "data_watch20150515_DATASET3", FALSE, "tes",FALSE, 1335577,1345577)
createPlotV2(data_watch20150515_DATASET2, 1, "data_watch20150515_DATASET2", FALSE, "tes",FALSE, 1335577,1345577)
createPlotV2(data_watch20150515_DATASET1, 1, "data_watch20150515_DATASET1", FALSE, "tes",FALSE, 1335577,1345577)




autolabeling <- function(filename, labelname, a_label = FALSE )
{
  #"non_scratch""scratch", "scratch_finger"
  # 3개로 나눠보기
  data_label<-read.csv(paste("./data_csv/",filename,sep=""))
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

getActiveMovements2(data_watch20150413_seungho, 8, "data_watch20150413_seungho", 150, 50, model, 0.1, FALSE)
getActiveMovements2(data_watch20150413_eunji_scratch, 8, "data_watch20150413_eunji_scratch", 150, 50, model, 0.1, FALSE)
getActiveMovements2(data_watch20150412_scratching_x, 8, "data_watch20150412_scratching_x", 150, 50, model, 0.1, FALSE)
getActiveMovements2(data_watch20150412_scratching_y, 8, "data_watch20150412_scratching_y", 150, 50, model, 0.1, FALSE)
getActiveMovements2(data_watch20150412_scratching_z, 8, "data_watch20150412_scratching_z", 150, 50, model, 0.1, FALSE)

data<-data_watch20150413_eunji_scratch_2
data <- subset(data, subset=(data$time > 1000*4 ))
data_watch20150413_eunji_scratch_2<-data

getActiveMovements2(data_watch20150412_walking_normal, 8, "data_watch20150412_walking_normal", 150, 50, model, 0.1, FALSE)
getActiveMovements2(data_watch20150412_walking_normal_2, 8, "data_watch20150412_walking_normal_2", 150, 50, model, 0.1, FALSE)

getActiveMovements2(data_watch20150412_normal, 8, "data_watch20150412_normal", 150, 50, model, 0.1, FALSE)
getActiveMovements2(data_watch20150412_normal_2, 8, "data_watch20150412_normal_2", 150, 50, model, 0.1, FALSE)

(137+294)/(137 +  48 +7+ 294) 





scr_filelist <- c("data_watch20150412_scratching_x",
                  "data_watch20150412_scratching_y",  
                  "data_watch20150412_scratching_z",
                  #             "data_watch20150412_scratching_xy",
                  "data_watch20150413_eunji_scratch",
                  "data_watch20150413_eunji_scratch_2",
                  "data_watch20150413_seungho",
                  "data_watch20150413_seungho_3",
                  "data_watch20150412_scratching_no_wrist",
                  "data_watch20150412_scratching_no_wrist_2"         
)

scratching_moving <- getDataset(scr_filelist, NULL, 8, 150,50)
scratching_moving_nowrist <- getDataset(scr_filelist, NULL, 8, 150,50)
scratching_moving_nowrist_normal <- getDataset(scr_filelist, non_scr_filelist, 8, 150,50)
scratching_moving_nowrist_normal_walking <- getDataset(scr_filelist, non_scr_filelist, 8, 150,50)


model_name<-"J48"
window_size <- 150
window_step <- 50

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

sumData<-function(data1, data2,filter)
{
  data_sub1 <- subset(data1, grepl(filter, data1$label))
  data_sub2 <- subset(data2, grepl(filter, data2$label))
  rs <- rbind(data_sub1,data_sub2)
  return(rs)
}

trans_to_frequency <- function(s1, opt = "no_maxfreq")
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


getDelayedData<-function(data, delay)
{
  index <- 1
  index[1] <- 1
  for(i in 2:nrow(data))
  {
    if( (index[i-1] + delay) > nrow(data) )
      break
    index[i] <- index[i-1] + delay
  }
  
  res <- data[index,]
  View(res)
  print(nrow(res))
  return (res)
}



smodel <- getAccuracy(c("data_watch20150412_scratching_x",
                        "data_watch20150412_scratching_y",  
                        "data_watch20150412_scratching_z",
                        "data_watch20150413_eunji_scratch",
                        "data_watch20150413_eunji_scratch_2",
                        "data_watch20150413_seungho",
                        "data_watch20150413_seungho_3"         
),NULL, 8, window_size, window_step, FALSE, 0.5, model_name)

smodel2 <- getAccuracy(c(
  "data_watch20150412_scratching_x",
  "data_watch20150412_scratching_y",
  "data_watch20150412_scratching_z",
  "data_watch20150413_eunji_scratch",
  "data_watch20150413_eunji_scratch_2",
  "data_watch20150413_seungho",
  "data_watch20150413_seungho_3"
),NULL, 8, window_size, window_step, FALSE, 0.5, model_name,NULL,
c(
  "data_watch20150412_scratching_no_wrist",
  "data_watch20150412_scratching_no_wrist_2",
  "data_watch20150416_scratching_no_wrist_3",
  "data_watch20150416_scratching_no_wrist_4"
)
)


smodel3_2 <- getAccuracy(c(
  "data_watch20150412_scratching_x",
  "data_watch20150412_scratching_y",
  "data_watch20150412_scratching_z",
  "data_watch20150413_eunji_scratch",
  "data_watch20150413_eunji_scratch_2",
  "data_watch20150413_seungho",
  "data_watch20150413_seungho_3",
  "data_watch20150412_scratching_no_wrist",
  "data_watch20150412_scratching_no_wrist_2",
  "data_watch20150416_scratching_no_wrist_3",
  "data_watch20150416_scratching_no_wrist_4"
),c(
  "data_watch20150412_normal",
  "data_watch20150412_normal_2",
  "data_watch20150413_normal4",
  "data_watch20150413_normal3"
), 8, 50*5, 50*5*0.5, FALSE, 0.5, model_name,NULL,
NULL,delay=1
)


smodel3 <- getAccuracyBy(c(
  "data_watch20150412_scratching_x",
  "data_watch20150412_scratching_y",
  "data_watch20150412_scratching_z",
  "data_watch20150413_eunji_scratch",
  "data_watch20150413_eunji_scratch_2",
  "data_watch20150413_seungho",
  "data_watch20150413_seungho_3"
),c(
  "data_watch20150412_normal",
  "data_watch20150412_normal_2",
  "data_watch20150413_normal4",
  "data_watch20150413_normal3"
), 8, window_size, window_step, FALSE, 0.5, model_name,NULL,
c(
  "data_watch20150412_scratching_no_wrist",
  "data_watch20150412_scratching_no_wrist_2",
  "data_watch20150416_scratching_no_wrist_3",
  "data_watch20150416_scratching_no_wrist_4"
),delay=1
)



smodel3 <- getAccuracyBy(c(
  "data_watch20150412_scratching_x"
),c(
  "data_watch20150412_normal"
), 3, 50*3, 50*1, FALSE, 0.5, "J48",NULL,
c(
  "data_watch20150412_scratching_no_wrist"
),delay=1
)

test_data2 <- getDataset(c(
  "data_watch20150412_scratching_x"
), c(
  "data_watch20150412_normal"
  
),8, window_size, window_step, FALSE)

  
  smodel4_2 <- getAccuracy(c(
    "data_watch20150412_scratching_x",
    "data_watch20150412_scratching_y",
    "data_watch20150412_scratching_z",
    "data_watch20150413_eunji_scratch",
    "data_watch20150413_eunji_scratch_2",
    "data_watch20150413_seungho",
    "data_watch20150413_seungho_3",
    "data_watch20150412_scratching_no_wrist",
    "data_watch20150412_scratching_no_wrist_2",
    "data_watch20150416_scratching_no_wrist_3",
    "data_watch20150416_scratching_no_wrist_4"
  ),c(
    "data_watch20150412_normal",
    "data_watch20150412_normal_2",
    "data_watch20150413_normal4",
    "data_watch20150413_normal3",
    "data_watch20150412_walking_normal",
    "data_watch20150412_walking_normal_2",
    "data_watch20150416_walking4",
    "data_watch20150416_walking5"
  ), 8, window_size, window_step, FALSE, 0.5, model_name, NULL, NULL,delay=1
  )

smodel4 <- getAccuracyBy(c(
  "data_watch20150412_scratching_x",
  "data_watch20150412_scratching_y",
  "data_watch20150412_scratching_z",
  "data_watch20150413_eunji_scratch",
  "data_watch20150413_eunji_scratch_2",
  "data_watch20150413_seungho",
  "data_watch20150413_seungho_3"
),c(
  "data_watch20150412_normal",
  "data_watch20150412_normal_2",
  "data_watch20150413_normal4",
  "data_watch20150413_normal3",
  "data_watch20150412_walking_normal",
  "data_watch20150412_walking_normal_2",
  "data_watch20150416_walking4",
  "data_watch20150416_walking5"
), 8, window_size, window_step, FALSE, 0.5, model_name, NULL, c(
  "data_watch20150412_scratching_no_wrist",
  "data_watch20150412_scratching_no_wrist_2",
  "data_watch20150416_scratching_no_wrist_3",
  "data_watch20150416_scratching_no_wrist_4"
)
)



smodel5 <- getAccuracy(c("data_watch20150412_scratching_x",
                         "data_watch20150412_scratching_y",
                         "data_watch20150412_scratching_z",
                         "data_watch20150413_eunji_scratch",
                         "data_watch20150413_eunji_scratch_2",
                         "data_watch20150413_seungho",
                         "data_watch20150413_seungho_3"
),c(
  "data_watch20150412_walking_normal",
  "data_watch20150412_walking_normal_2",
  "data_watch20150416_walking4",
  "data_watch20150416_walking5"
), 8, 150, 50, FALSE, 0.5, model_name, NULL, c(
  "data_watch20150412_scratching_no_wrist",
  "data_watch20150412_scratching_no_wrist_2",
  "data_watch20150416_scratching_no_wrist_3",
  "data_watch20150416_scratching_no_wrist_4"
),delay=1)


smodel33 <- getAccuracy(NULL,c(
  "data_watch20150413_normal4",
  "data_watch20150413_normal3",
  "data_watch20150412_walking_normal",
  "data_watch20150412_walking_normal_2"
), 8, window_size, window_step, FALSE, 0.5, model_name,NULL,
c(
  "data_watch20150412_scratching_no_wrist",
  "data_watch20150412_scratching_no_wrist_2",
  "data_watch20150416_scratching_no_wrist_3",
  "data_watch20150416_scratching_no_wrist_4"
))
summary(smodel)
summary(smodel2)
summary(smodel3)
summary(smodel4)
summary(smodel5)


smodel
smodel2
smodel3
smodel4
smodel5


gmodel
gmodel2
gmodel3
gmodel4
gmodel5



model4
confusionMatrix(model$pred$pred,model$pred$obs)
confusionMatrix(model2$pred$pred,model2$pred$obs)
confusionMatrix(model3$pred$pred,model3$pred$obs)
confusionMatrix(gmodel4$pred$pred,gmodel4$pred$obs)

confusionMatrix(tmodel4$pred$pred,tmodel4$pred$obs)


"data_watch20150416_scratching_no_wrist_5", 



#### test data set #######
window_size <- 150
window_step <- 50
test_data <- getDataset(c(
  "data_watch20150416_scratching_no_wrist_5", # minseouk data
  "data_watch20150417_scratchingtest1",  # 얼굴 - 뒤통수 - 다리 - 팔 - 등 (cheolmin)
  "data_watch20150417_scratchingtest2",  # 얼굴 - 뒤통수 - 다리 - 팔 - 등 (cheolmin)
  "data_watch20150417_scratchingtest3",  # 얼굴 - 뒤통수 - 다리 - 팔 - 등 (junhong)
  "data_watch20150417_scratchingtest4"   # 얼굴 - 뒤통수 - 다리 - 팔 - 등 (junhong)
), c(
  "data_watch20150417_scratchingtest_no1", # 일어나고 - 걷고 - 걸어서오고 - 앉고 - 타이핑(cheolming)
  "data_watch20150417_scratchingtest_no2"  # 일어나고 - 걷고 - 걸어서오고 - 앉고 - 타이핑(junhong)
),8, window_size, window_step, FALSE)




predictions <- predict(smodel3_2, test_data[,6:(26)])
test_data$label<- factor(test_data$label)
confusionMatrix(predictions, test_data$label)

#######################

model <- getAccuracy(scr_filelist,NULL, 8, 150, 50)
summary(model)

model <- getAccuracy(scr_filelist,non_scr_filelist, 8, 150, 50)
summary(model)

getDataset <- function(scr_filelist, non_scratch_file,idx, window_size, window_stp, plotting = FALSE, scr_small_scratching = NULL,delay=1)
{
  filter <- "scratch|non|scratch_finger"
  sum_data <- NULL
  
  if(length(scr_filelist)!=0){
    for(i in 1:length(scr_filelist))
    {
      labelname <- scr_filelist[i]
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulation(data, TRUE, idx, window_size, window_stp, labelname, plotting,delay=delay)
      t<-autolabeling(paste(labelname,".csv",sep=""), "scratch")
      
      if(length(sum_data)==0)
        sum_data <- subset(t, grepl(filter, t$label))
      else
        sum_data <-sumData(sum_data, t, filter)
    }
  }
  
  if(length(non_scratch_file)!=0){
    for(i in 1:length(non_scratch_file))
    {
      print(i)
      labelname <- non_scratch_file[i]
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulation(data, TRUE, idx, window_size, window_stp, labelname, plotting,delay=delay)
      t<-autolabeling(paste(labelname,".csv",sep=""), "non", TRUE)
      
      if(length(sum_data)==0)
        sum_data <- subset(t, grepl(filter, t$label))
      else
        sum_data <- sumData(sum_data, subset(t, grepl(filter, t$label)), filter)
    }
  }
  
  if(length(scr_small_scratching)!=0){
    for(i in 1:length(scr_small_scratching))
    {
      labelname <- scr_small_scratching[i]
      data <- read.table(paste("./data_raw/",labelname,".txt" ,sep=""), sep="," ,header=TRUE)
      doSimulation(data, TRUE, idx, window_size, window_stp, labelname, plotting,delay=delay)
      t<-autolabeling(paste(labelname,".csv",sep=""),"scratch_finger")
      
      if(length(sum_data)==0)
        sum_data <- subset(t, grepl(filter, t$label))
      else
        sum_data <- sumData(sum_data, subset(t, grepl(filter, t$label)), filter)
    }
  }
  
  View(sum_data)
  levels(sum_data$label)
  #sum_data <- sumData(sum_data2, sum_data, filter)
  
  return(sum_data)
}


getAccuracyBy <- function(scr_filelist, non_scratch_file, idx, window_size, window_stp,
                        plotting = FALSE, fit_p=0.3, ML_method = "J48", sum_data=NULL,
                        scr_small_scratching = NULL,delay=1
)
{
  if(length(sum_data)==0)
    sum_data <- getDataset(scr_filelist, non_scratch_file,idx, window_size, window_stp, plotting, scr_small_scratching,delay=delay)
  
  #  set.seed(1234)
  #  rand <- sample(nrow(sum_data))
  #  sum_data <- sum_data[rand,]
  
  #data_sub2$label <- as.factor(data_sub2$label)
  #  data_set <- sum_data[,5:20] # CHI work
  # data_set <- sum_data[,c(5,21:(32-8) )] # previous work
  
  #data_set <- sum_data[, 5:(20-3)] # entropy, autocor, th, var, peakfreq  
  #data_set <- sum_data[,c(5,21:(32) )] # extended previous work
  #   data_set <- sum_data[,5:(32+4)] # alll
  data_set <- sum_data[,5:(26)] # selected
  
  data_set$label <- factor(data_set$label)
  
  View(data_set)
  
  if(ML_method == "J48")
  {
    ML_method <- "J48"
    train_control <- trainControl(method="cv", number=10)
    model <- train(label~., data=data_set, trControl = train_control,  method=ML_method)
    
    #  predictions <- predict(model,data_set[,2:16]) # CHI work
    #  predictions <- predict(model,data_set[,2:(13-8)]) # previous work
    #  predictions <- predict(model,data_set[,2:(13)],type="prob") # extended previous work
    #  predictions <- predict(model,data_set[,2:(16-3)]) #  entropy, autocor, th, var, peakfreq
    #confusionMatrix(predictions, data_set$label)
    #  View(predictions)
    
    
  }else if(ML_method == "glm"){
    ML_method <- "glm"
    #    data_set$label <- as.factor(data_set$label)
    
    data_set$label <- factor(data_set$label)
    #data_set$label[which(data_set$label==1)]<-0
    #data_set$label[which(data_set$label==2)]<-1
    
    train_control <- trainControl(method="cv", number=10,savePredictions=T)
    model <- train(label~., data=data_set, trControl = train_control,family = binomial,  method=ML_method)
    
    # fitpred <- model$finalModel$fitted.values
    #  fitpredt <- function(t) ifelse(fitpred > t , 1, 0)
    #  print(confusionMatrix(fitpredt(fit_p),data_set$label))
    
  }
  
  return(model)
}




sum_data <- getDataset(scr_filelist, NULL, 8, 150,50)
model <- getAccuracy(scr_filelist, NULL, 8, 150, 50,fit_p = 0.8,ML_method = "glm")
dd<-sum_data

fitpred <- model$finalModel$fitted.values
fitpredt <- function(t) ifelse(fitpred > t , 1,0)
dd$v <- ifelse(dd$label == "scratch", 1, 0)    
confusionMatrix(fitpredt(0.3),dd$v)




sum_data$label <- factor(sum_data$label)
predictions2 <- predict(model,sum_data[,c(5,21:32)], type="prob")
t<-(modelpredict(predictions2,0.5))
sum_data$label <- factor(sum_data$label)
confusionMatrix(t, sum_data2$label)

data_watch20150414_vs_scratching_2 <- read.table("/Users/jonginlee/Documents/data_watch20150414_vs_scratching_2.txt", sep="," ,header=TRUE)
createPlotByFeature("test", data_watch20150414_vs_scratching_2, 8, 150, 50, "threshold",FALSE)
createPlotByFeature("test", data_watch20150414_vs_scratching_2, 1, 150, 50, "threshold",FALSE)
createPlotByFeature("test", data_watch20150414_vs_scratching_2, 8, 150, 50, "threshold",FALSE)
createPlotByFeature("test", data_watch20150414_vs_scratching_2, 8, 150, 50, "entropy",FALSE)
createPlotByFeature("test", data_watch20150414_vs_scratching_2, 8, 150, 50, "energy",FALSE)
createPlotByFeature("test", data_watch20150414_vs_scratching_2, 8, 150, 50, "autocorrelation",FALSE)
createPlotByFeature("test", data_watch20150414_vs_scratching_2, 8, 150, 50, "autocorrelationV2",FALSE)

