###Selectivity Functions


#Define selectivity function
selective<-function(y){
  #Aggregate time by species and treatment
  Total_Time<-aggregate(y$Time_Feeder_Obs,by=list(y$Species,y$Treatment),sum, na.rm=TRUE)
  
  #Divide time on high value resource by total time on feeder
  melt.time<-melt(Total_Time)
  cast.time<-as.data.frame(cast(melt.time,Group.1~Group.2 ))
  
  #Set the NAs to 0, if bird was not present on one of the resources
  cast.time[is.na(cast.time)]<- 0
  selectivity<-cbind(cast.time,cast.time$H/(cast.time$H+cast.time$L))
  colnames(selectivity)<-c("Species","Time_High","Time_Low","Selectivity")
  
  #return output
  return(selectivity)}
