
showAccuracy <- function (dataset, f_indexces, ML_method)


scr_data <- subset(scr_data0703, subset=(scr_data0703$p != 3 ))



data <- rbind(scr_data0704, non_scr_data)

#data <- rbind(nonscr_data, scr_data)
#View(data)
sum_data<-data
selected <- sum_data[c(1,49,50,51,82,83,84,88,111,112,113,123,124,125)]
data<-as.data.frame(selected)
data$label <- factor(data$label)
data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
ML_method <- "J48"
train_control <- trainControl(method="cv", number=10)
gmodel0705 <- train(label~., data=data, trControl = train_control,  method=ML_method)



scr_test_data <- subset(scr_data0703, subset=(scr_data0703$p == 3 ))
scr_test_data <- subset(scr_test_data, subset=(scr_test_data$scrtype == 4 ))


selected <- scr_test_data[c(1,49,50,51,82,83,84,88,111,112,113,123,124,125)]
data<-as.data.frame(selected)
data$label <- factor(data$label, levels = c("non", "scratch"))
data[c(2:length(data))] <-  lapply((data[c(2:length(data))]), as.numeric)
pd<-predict(gmodel0702, data[2:length(data)])
cm <- confusionMatrix(pd, data$label)
cm$overall[1]




