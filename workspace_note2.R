
data <- rbind(nonscr_data, scr_data)
#View(data)
sum_data<-data
selected <- sum_data[c(1,49,50,51,82,83,84,88,111,112,113,123,124,125)]
data<-as.data.frame(selected)
data$label <- factor(data$label)
data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
ML_method <- "J48"
train_control <- trainControl(method="cv", number=10)
gmodel0702 <- train(label~., data=data, trControl = train_control,  method=ML_method)

