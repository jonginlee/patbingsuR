

filelist <- c(
"data_watch_intentservice1-(2015-05-30)-14-17-36-0.wav",
"data_watch_intentservice1-(2015-05-30)-14-17-56-1.wav",
"data_watch_intentservice1-(2015-05-30)-14-18-16-2.wav",
"data_watch_intentservice1-(2015-05-30)-14-18-32-3.wav",
"data_watch_intentservice1-(2015-05-30)-14-18-44-4.wav",
"data_watch_intentservice1-(2015-05-30)-14-18-57-5.wav",
"data_watch_intentservice1-(2015-05-30)-14-19-21-6.wav",
"data_watch_intentservice1-(2015-05-30)-14-19-34-7.wav",
"data_watch_intentservice1-(2015-05-30)-14-19-48-8.wav",
"data_watch_intentservice1-(2015-05-30)-14-20-02-9.wav",
"data_watch_intentservice1-(2015-05-30)-14-20-19-10.wav",
"data_watch_intentservice1-(2015-05-30)-14-20-48-11.wav"
)

getMFCCfeatureBy("./data_raw/data_watch_intentservice1-(2015-05-30)-16-07-19-0.wav", "test", FALSE, 0, 1, TRUE)


for(i in 1: length(filelist))
{
  name <- filelist[i]
  data <- getMFCCfeatureBy(paste("./data_raw/",name,sep=""), name, FALSE, 0, 1, TRUE)
}