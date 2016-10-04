# Created by jonginlee on 16 10. 04

plotDist<- function(data, feature_name, dist="density")
{
  data <- as.data.frame(data)
  data_sub <- cbind(data$label, data[feature_name])
  #View(data_sub)
  
  colnames(data_sub) <- c("label", feature_name)
  #total_sum_data<-as.data.frame(total_sum_data)
  #row.names(total_sum_data) <- NULL
  
  #total_sum_data$truth <- factor(as.character(total_sum_data$truth))
  #total_sum_data$avg <- as.numeric(total_sum_data$avg)
  
  max_value <- ((max((data_sub[2]))))
  min_value <- ((min((data_sub[2]))))
  
  spliting <- seq(min_value, max_value, (max_value-min_value)/10)
  
  t<-ggplot(data.frame(data_sub), aes(x=data_sub[feature_name])) + 
    #geom_density(aes(group=truth, colour=truth), alpha=1) +
    #geom_histogram(binwidth=(max_value)/100, aes (fill = truth)) +
    ylab("Density") +
    scale_x_continuous(breaks = spliting) +
    theme_bw() +
    theme(axis.text=element_text(size=12), text=element_text(size=20), panel.border = element_blank(), axis.line = element_line(colour="black"), 
          axis.text.x = element_text(angle=40,hjust=1,vjust=1))
  if(dist == "density"){
    t<-t+geom_density(aes(group=label, colour=label), alpha=1)
  }else if(dist == "bin"){
    t<-t+geom_histogram(binwidth=(max_value)/100, aes (fill = label))
  }
  print(t)
}