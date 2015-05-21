
#history
library("caret", lib.loc="/usr/local/lib/R/site-library")
library("e1071", lib.loc="/usr/local/lib/R/site-library")
library("tuneR", lib.loc="/usr/local/lib/R/site-library")
library("RWeka", lib.loc="/usr/local/lib/R/site-library")
library("signal", lib.loc="/usr/local/lib/R/site-library")


# 0521 scratch file creating!!

createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(1_57_38)", TRUE, "0521",TRUE, getMilliFromHMS(1,57,30,TRUE,1,41,51), getMilliFromHMS(1,57,55,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_14_16)", TRUE, "0521",TRUE, getMilliFromHMS(3,14,13,TRUE,1,41,51), getMilliFromHMS(3,14,30,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_15_58)", TRUE, "0521",TRUE, getMilliFromHMS(3,15,55,TRUE,1,41,51), getMilliFromHMS(3,16,12,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_42_22)", TRUE, "0521",TRUE, getMilliFromHMS(3,42,20,TRUE,1,41,51), getMilliFromHMS(3,42,32,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(3_55_20)", TRUE, "0521",TRUE, getMilliFromHMS(3,55,15,TRUE,1,41,51), getMilliFromHMS(3,55,30,TRUE,1,41,51))
createPlot(data_watch_intentservice1_05_21_jongin, 8, "scratching_data0521(4_21_17)", TRUE, "0521",TRUE, getMilliFromHMS(4,21,10,TRUE,1,41,51), getMilliFromHMS(4,21,35,TRUE,1,41,51))


# 0520 scratch file creating!!

createPlot(data_watch_intentservice1_05_20_jongin, 8, "scratching_data0520(2_30_41)", TRUE, "0520",TRUE, getMilliFromHMS(2,30,35,TRUE,2,15,38), getMilliFromHMS(2,31,0,TRUE,2,15,38))
createPlot(data_watch_intentservice1_05_20_jongin, 8, "scratching_data0520(06_07_00)", TRUE, "0520",TRUE, getMilliFromHMS(6,6,55,TRUE,2,15,38), getMilliFromHMS(6,7,20,TRUE,2,15,38))
createPlot(data_watch_intentservice1_05_20_jongin, 8, "scratching_data0520(06_11_24)", TRUE, "0520",TRUE, getMilliFromHMS(6,11,20,TRUE,2,15,38), getMilliFromHMS(6,11,40,TRUE,2,15,38))


test_data <- getDataset(c(
  "scratching_data0521(1_57_38)", 
  "scratching_data0521(3_14_16)"
), NULL,8, window_size, window_step, FALSE)


# 

save.image("~/workspace/patbingsuR/rworkspace.RData")