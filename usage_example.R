# Created by jonginlee on 16. 10. 04.
source("./library.R")
source("./utills.R")
source("./createPlot.R")
source("./feature_extraction.R")
source("./feature_comp.R")
source("./getDefaultFeatureBy.R")
source("./detectEventBy.R")

########################################################################################
# 0. required library & sample data set
########################################################################################
# 0.1 required library (you can install below library with 'install.packages(library_name)' )
library("utils", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("signal", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("moments", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("quantmod", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("pracma", lib.loc="~/Library/R/3.3/library")
library("plotly", lib.loc="~/Library/R/3.3/library")
library("caret", lib.loc="~/Library/R/3.3/library")
library("methods", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")


# 0.2 sample data set in below example
data<-read.csv("./walking/watch1_sensor_data.txt") # walking data
data2<-read.csv("./running/watch2_sensor_data.txt") # running data

########################################################################################
# 1. ploting sensor data
########################################################################################
#  1.1 basic plot
createPlot(data, 8, "linear_accel", FALSE,"1003")

#  1.2 ploting with ranges
createPlot(data, 8, "linear_accel", FALSE,"1003",TRUE,1000,8000)

#  1.3 plotting with smoothing function
createPlot(data, 8, "linear_accel", FALSE,"1003",TRUE,1000,8000, type = 2)

########################################################################################
# 2. feature extraction & building a model
########################################################################################
#  2.1 extracting default features
foldername <- "./walking"
extracted_features_walking <- feature_extraction(foldername, 8, 150, 50, "walking")
foldername <- "./running"
extracted_features_running<- feature_extraction(foldername, 8, 150, 50, "running")
data<-rbind(extracted_features_running, extracted_features_walking)
print(colnames(data[5:length(data)])) # print feature list

#  2.2 checking distributions between features
feature_name <- "Linearaccel_entropy_avg"
plotDist(data, feature_name, "bin")
plotDist(data, feature_name, "density")

#  2.3 training a model with the extracted features
model <- getModelBy(data, ML_method = "J48")

########################################################################################
# 3. recognition with the trained model 
########################################################################################
#  3.1 recognition with machine learning (J48 - decision tree algorithm)
foldername <- "./walking"
filename <- list.files(foldername)
filename <- paste(foldername,"/",filename[1],sep="")
print(paste("traget file : ",filename))
detectEventBy(filename, 8, 150, 50, model, "walking")

# 3.1 recognition first window in the file
t<-detectEventByWindow(filename, 8, 150, 50, model)
print(paste("recognition result :", t))



