

removeFirstLastWin <- function(data)
{
  start<-FALSE
  for( i in 1:length(data$label))
  {
    if(i!=1){
      if(!start & (data$label[i]=="scratch") & (data$label[i-1]=="non"))
      {
        data$label[i]<-"non"
        start<-TRUE
      }
      else if((data$label[i]=="non") & (data$label[i-1]=="scratch"))
      {
        data$label[i-1]<-"non"
      }else{
        start <- FALSE
      }
    }
  }
  
  return(data)
}


