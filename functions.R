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

#Birds per hour visit rate
bph<-function(x){
bird_hour<-table(droplevels(x$Species),hours(x$Time.Begin))
p<-apply(bird_hour,1,mean)
return(data.frame(Species=names(p),bph=p))}

#Average time between visits for each species, function takes in the trial for each date and elevation

time_feed<-function(x){
  
  #split list by species
  x.s<-split(x,list(x$Species),drop=TRUE)
  
  #repeat across all species
  sp.out<-sapply(x.s,function(y){
    
    #create an empty vector
    out<-vector()
    
    #for each row subtract the time between feeding events
    
    for(j in 1:nrow(y)-1){
      
      timeS<-y[j,"Time.End"]
      timeE<-y[j+1,"Time.End"]
      
      #find difference in time
      diff<-timeE - timeS
      
      #return time in terms of mintunes
      out[j]<- minutes(diff) + seconds(diff)/60
    }
    
    #add a NA for the last position, there is no last value
    out<-c(out,NA)
    
    #find mean value for time between feeding
    return(mean(out[is.finite(out)],na.rm=TRUE))
  })
  return(data.frame(Species=names(sp.out),Time_Feed=sp.out))
}
         
 
#average feeding time
avgF<-function(x){
  T<-aggregate(x$Time_Feeder_Obs,list(x$Species),mean,na.rm=TRUE)
  colnames(T)<-c("Species","avgF")
  T$avgF<-minutes(T$avgF)*60 + seconds(T$avgF)
  return(T)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
