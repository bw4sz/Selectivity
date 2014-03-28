###########Behavioral descriptors  of hummingbird foraging.

require(ggplot2)
require(reshape)
require(chron)

#Set working directory
droppath<-"C:/Users/Jorge/Dropbox/"

vid<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",sep=""),row.names=1)

#get videos only?
vid<-vid[!is.na(vid$Video),]

#diversity of plants by each species
head(vid)

#flower by species matrix
a<-as.data.frame.array(table(vid$Iplant_Double,vid$Hummingbird))
#turn to presence

a<-as.matrix(a)

#number of flower species utilized
diversity<-sapply(colnames(a),function(x){
  b<-a[,colnames(a) %in% x]
  b[b>1]<-1
  out<-names(b[which(b==1)])
  length(out[!out %in% ""])
})

#average visits feeding at resources
visitsFlower<-sapply(colnames(a),function(x){
  b<-a[,colnames(a) %in% x]
  out<-mean(b[which(b>0)])
})

#merge factors
visitsFlower<-melt(visitsFlower)
colnames(visitsFlower)<-c("Visits")

diversity<-melt(diversity)
colnames(diversity)<-c("Flowers")

dat<-merge(visitsFlower,diversity,by="row.names")

ggplot(dat,aes(x=Visits,Flowers)) + geom_point() + geom_text(aes(label=Row.names),size=3.5)

#time between visits
vid$TimeSame<-NA

#create time column
vid$Time<-chron::times(vid$Time)

split.vid<-split(vid,list(vid$ID,vid$Video,vid$Hummingbird),drop=TRUE)

TimeS<-rbind.fill(lapply(split.vid,function(y){
  if(nrow(y)>1){
    for(x in 2:nrow(y)){
      ti<-y[x,]$Time - y[x-1,]$Time 
      y[x,"TimeSame"]<-minutes(ti)+seconds(ti)/60
    }}
  return(y)
}))

#aggregate time since feeding for each species
sp.Time<-aggregate(TimeS$TimeSame,list(vid$Hummingbird),function(x) {
  mean(x[is.finite(x)],na.rm=TRUE)
})

dat<-merge(dat,sp.Time,by.x="Row.names",by.y="Group.1")

colnames(dat)<-c("Species","Visits","Flowers","TimeSame")

rownames(dat)<-dat$Species

#Quality of resources
#read in data, see Nectar.R in maquipucuna repo.

nectar<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/nectarmean.csv",sep=""),row.names=1)

##average flower quality
flowerQ<-sapply(colnames(a),function(x){
  
  #get column for that species
  b<-a[,colnames(a) %in% x]
  #just get the presence records
  listb<-b[b>0]
  
  #create a dataframe
  flowerframe<-data.frame(Iplant_Double=names(listb),Visits=listb)
  
  #merge brix column
  flowerframeN<-merge(flowerframe,nectar)
  
  #weighted average
  avgBR<-weighted.mean(flowerframeN$Brix,weights=flowerframe$Visits,na.rm=TRUE)
  return(avgBR)
})

dat<-merge(dat,data.frame(flowerQ),by.x="Species",by.y="row.names")

rownames(dat)<-dat$Species

#multivariate analysis
#remove none complete rows
pca.dat<-dat[complete.cases(dat),]

biplot(prcomp(pca.dat[,-c(1)],scale=TRUE))

