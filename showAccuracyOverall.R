


sum_data$p[c(1:length(sum_data$label))] <-  0
sum_data$scrtype[c(1:length(sum_data$label))] <-  0

scr_data <- scr_data0703
non_scr_data <- nonscr_data
f_indexces <- c(1,49,50,51,82,83,84,88,111,112,113,123,124,125)
ML_method <- "J48"



showAccuracy <- function (scr_data, non_scr_data, f_indexces, ML_method)
{
  np <- length(levels(factor(scr_data$p)))
  non_scr_data$p <- 0
  non_scr_data$scrtype <- 0
  accuracy <- list()
  
  for(testP in 1:np)
  {
    # Step 1 : making Model
    scr_data_train <- subset(scr_data, subset=(scr_data$p != testP ))
    #    print(scr_data_train$p)
    data <- rbind(non_scr_data, scr_data_train)
    sum_data<-data
    selected <- sum_data[f_indexces]
    data<-as.data.frame(selected)
    data$label <- factor(data$label)
    data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
    #    ML_method <- "J48"
    train_control <- trainControl(method="cv", number=10)
    gmodel <- train(label~., data=data, trControl = train_control,  method=ML_method)
    print(summary(gmodel))
    
    # Step 2 : predicting testP total
    scr_data_test <- subset(scr_data, subset=(scr_data$p == testP ))
    selected <- scr_data_test[f_indexces]
    data<-as.data.frame(selected)
    data$label <- factor(data$label, levels = c("non", "scratch"))
    data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
    pd<-predict(gmodel, data[2:length(data)])
    cm <- confusionMatrix(pd, data$label)
    cm$overall[1]
    
    accuracy$testPtotal[testP] <- cm$overall[1]
    
    
    # Step 3 : predicting nonscratch data
    selected <- non_scr_data[f_indexces]
    data<-as.data.frame(selected)
    data$label <- factor(data$label, levels = c("non", "scratch"))
    data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
    pd<-predict(gmodel, data[2:length(data)])
    cm <- confusionMatrix(pd, data$label)
    cm$overall[1]
    
    accuracy$nonscratch[testP] <- cm$overall[1]
    
    # Step 4 : predicting non scratch + scratch
    
#    data <- rbind(non_scr_data, scr_data_test)
#    sum_data<-data
#    selected <- sum_data[f_indexces]
#    data<-as.data.frame(selected)
#    data$label <- factor(data$label, levels = c("non", "scratch"))
#    data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
#    pd<-predict(gmodel, data[2:length(data)])
#    cm <- confusionMatrix(pd, data$label)
#    cm$overall[1]
#    accuracy$testAndNonscr[testP] <- cm$overall[1]
    
    for(scr_type in 1:4)
    {
      scr_test_data <- subset(scr_data, subset=(scr_data$p == testP ))
      scr_test_data <- subset(scr_test_data, subset=(scr_test_data$scrtype == scr_type ))
      
      
      selected <- scr_test_data[f_indexces]
      data<-as.data.frame(selected)
      data$label <- factor(data$label, levels = c("non", "scratch"))
      data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
      pd<-predict(gmodel, data[2:length(data)])
      cm <- confusionMatrix(pd, data$label)
      cm$overall[1]
      
      if(scr_type==1)
        accuracy$scratchType1[testP] <- cm$overall[1]
      else if(scr_type==2)
        accuracy$scratchType2[testP] <- cm$overall[1]
      else if(scr_type==3)
        accuracy$scratchType3[testP] <- cm$overall[1]
      else if(scr_type==4)
        accuracy$scratchType4[testP] <- cm$overall[1]
      
    }
    
  }
  
  return(accuracy)
}




#View(data)







