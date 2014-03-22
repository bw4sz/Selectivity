#Selectivity Experiments

################################################################
#Step 1 - Set up data environment and load in data
################################################################

#load in packages
require(ggplot2)
require(chron)
require(reshape)
require(plotKML)
require(maptools)

#set gitpath
gitpath<-"C:/Users/Ben/Documents/Selectivity/"

#source functions
source(paste(gitpath,"functions.R",sep=""))

#Set working directory
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)

##Read in data
dat<-read.csv(paste(droppath,"Thesis//Maquipucuna_SantaLucia/Data2013/csv/CompetitionFeeders.csv",sep=""))

#Make all entries capital
dat$Sex<-toupper(dat$Sex)

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv",row.names=1)

dat<-dat[,1:12]

#####################################
#Step 2 - Data Cleaning and Sampling
#####################################

#Which dates need to be matched?
vid_totals_date<-aggregate(dat$Video,list(dat$Elevation,dat$Treatment,dat$Date,dat$Replicate),function(x) nlevels(droplevels(x)))
vid_totals_date<-cast(vid_totals_date,Group.1 + Group.3 + Group.4~Group.2)

colnames(vid_totals_date)<-c("Elevation","Date","Replicate(O_R)","High Feeder","Low Feeder")

print(vid_totals_date)

#no unknown species
dat<-dat[!dat$Species %in% "UKWN",]

#Create time columns
dat$Time.End<-times(dat$Time.End)
dat$Time.Begin<-times(dat$Time.Begin)

#Find time difference 
dat$Time_Feeder_Obs<-dat$Time.End - dat$Time.Begin

#Get any rownumbers that are negative, these need to be fixed. 
dat[which(dat$Time_Feeder_Obs < 0),]

#average visits per hour
S_H<-table(hours(dat$Time.Begin),dat$Species)

#################################
#Species Presence and Time
##################################

#Create overall date stamp
dat$Time_Stamp<-as.POSIXct(chron(dates=as.character(dat$Date),dat$Time.Begin))

#Time and species occurence, facetted by elevation
ggplot(dat,aes(x=strptime(dat$Time.Begin,"%H:%M"),fill=Species)) + geom_histogram(position="dodge") + facet_wrap(~Elevation)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/TimeofDayElevation.svg",height=11,width=8,dpi=300)

#Overall Month_Day and Elevation
ggplot(dat,aes(y=factor(Elevation),x=dat$Time_Stamp,col=Species)) + geom_point(size=3) + scale_x_datetime() + facet_wrap(~Species)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=11,width=8,dpi=300)

#Species richness and identity at each elevation
sp_matrixHL<-(table(dat$Species,dat$Elevation,dat$Treatment) >= 1) * 1

#View species at each elevation and treatment
m.sp_m<-melt(sp_matrixHL)
colnames(m.sp_m)<-c("Species","Elevation","Treatment","Presence")

#turn 0's to NA's just for plotting
m.sp_m[m.sp_m$Presence==0,"Presence"]<-NA

#richness across feeders
rangefeed<-ggplot(m.sp_m,aes(y=Species,x=factor(Elevation),fill=as.factor(Presence)))+ geom_tile() + theme_bw() + scale_fill_discrete(na.value="white")
rangefeed + labs(fill="Present",x="Elevation")
ggsave(paste(gitpath,"Figures/RangeExtentFeeders.svg",sep=""),dpi=300,height=8,width=11)

#Total Time per species
Total_Time_Species<-aggregate(dat$Time_Feeder_Obs,by=list(dat$Species),sum,na.rm=TRUE) 
colnames(Total_Time_Species)<-c("Species","TotalTime")
Total_Time_Species$Time<-minutes(Total_Time_Species$TotalTime)+seconds(Total_Time_Species$TotalTime)/60

ggplot(Total_Time_Species,aes(Species,Time)) + geom_bar() + theme_bw() + ylab("Minutes on Feeders") + theme(axis.text.x=element_text(angle=-90,vjust=-.1))

#mean time feeding bout
Mean_Time_Species<-aggregate(dat$Time_Feeder_Obs,by=list(dat$Species),mean,na.rm=TRUE) 
colnames(Mean_Time_Species)<-c("Species","Mean_Time")
ggplot(Mean_Time_Species,aes(Species,seconds(Mean_Time))) + geom_bar()  + theme_bw() + theme(axis.text.x=element_text(angle=-90)) + ylab("Average Seconds Feeding")

ggplot(dat,aes(x=seconds(Time_Feeder_Obs))) + geom_histogram()  + theme_bw()
ggsave(paste(gitpath,"Figures/Feedingtime.svg",sep=""),dpi=300,height=8,width=11)

#############################################
#Selectivity Analysis
#############################################

#Create a species list for each video

####Match each trial together, trials are done on the same day at the same elevation


#Split data into a list, with each compenent being one trial pair
Trials<-split(dat, list(dat$Elevation,dat$Date,dat$Replicate),drop=TRUE)

#Get number of levels per trial
levels.trial<-lapply(Trials,function(x) nlevels(factor(x$Treatment)))

#Only use trials that have a high and low, ie levels=2
complete.trial<- Trials[levels.trial ==2]

#time since feeding events
order.trials<-rbind.fill(lapply(1:length(complete.trial),function(g){
  #within feeding events
  tr<-complete.trial[[g]]
  
  order.x<-tr[order(tr$Time.Begin),]
  order.x$TimeSince<-NULL
  
  for(x in 2:nrow(order.x)){
    if(!order.x[x,"Species"] == order.x[x-1,"Species"]){
      ti<-order.x[x,]$Time.Begin - order.x[x-1,]$Time.End 
      order.x[x,"TimeSince"]<-minutes(ti)+seconds(ti)/60
    } else (order.x[x,"TimeSince"]<-NA)
  }
  
  order.trials$Mass_diff<-NULL
  for(x in 2:nrow(order.x)){
    a<-order.x[x,"Species"]
    b<-order.x[x-1,"Species"]
    
    massA<-hum.morph[hum.morph$English %in% a, "Mass"]
    massB<-hum.morph[hum.morph$English %in% b, "Mass"]
    
    massDiff<-massB - massA
    if(length(massDiff) == 0){order.x[x,"Mass_diff"]<-NA} else{
      order.x[x,"Mass_diff"]<-massDiff
    }
  }
  return(order.x)
}))

ggplot(order.trials,aes(x=Mass_diff,y=as.numeric(Treatment)-1)) + geom_point() + geom_smooth(family="binomial",method="glm",aes(weight=minutes(Time_Feeder_Obs) + seconds(Time_Feeder_Obs)))

####Within trial metrics per species

Tdata<-lapply(complete.trial,function(x){
  
  #caluclate selectivity
  a<-selective(x)
  
  #calculate birds per hour
  b<-bph(x)
  
  #Average time between feeding
  k<-time_feed(x)
  
  #average duration of feeding bout
  d<-avgF(x)
  
  #total visits
  tss<-data.frame(table(droplevels(x$Species)))
  colnames(tss)<-c("Species","N")
  
  #visits of other species
  visitsOthers<-sapply(levels(factor(x$Species)),function(g){
    sum(tss[!tss$Species %in% g,"N"])
  })
  
  visitsOthers<-data.frame(Species=names(visitsOthers),visitsOthers)
  
  tss<-data.frame(table(droplevels(x$Species)))
  colnames(tss)<-c("Species","N")
  
  
  #merge data
  dat.trials<-merge(merge(merge(merge(merge(a,b),d),k),tss),visitsOthers)
  Elevation=unique(x$Elevation)
  Date=unique(x$Date)
  Trichness<-length(a[minutes(times(a$Time_High + a$Time_Low)) > 1,]$Species)
  Replicate=unique(x$Replicate)
  
  #Total visits
  Tvisits<-nrow(x)
  
  out<-data.frame(dat.trials,Elevation,Date,Replicate,Richness=Trichness,Tvisits)
  return(out)})

#Bind dataset to a dataframe
selective.matrix<-rbind.fill(Tdata)

#Create times argument
selective.matrix$Time_High<-times(selective.matrix$Time_High)
selective.matrix$Time_Low<-times(selective.matrix$Time_Low)
selective.matrix$Total_Time<-selective.matrix$Time_High + selective.matrix$Time_Low

#add total minutes feeding as a weight
selective.matrix$Minutes_High<-minutes(selective.matrix$Time_High) + seconds(selective.matrix$Time_High)/60
selective.matrix$Minutes_Low<-minutes(selective.matrix$Time_Low) + seconds(selective.matrix$Time_Low)/60
selective.matrix$Minutes_Total<-selective.matrix$Minutes_Low+selective.matrix$Minutes_High

#Add month column
selective.matrix$MonthA<-format(as.POSIXct(selective.matrix$Date,format="%m/%d/%Y"),"%b")

#selective.matrix<-selective.matrix[selective.matrix$Minutes_Total > 1,]

#Optimal foraging says that individuals should occupy patches at a rate equal to their quality
sH<-sum(selective.matrix$Minutes_High)
sL<-sum(selective.matrix$Minutes_Low)

#Optimal foraging says its should be three
print(sH/sL)

#Competition keeps birds from occupying higher quality patch

#Effect of increasing richness on selectivity
ggplot(selective.matrix,aes(x=Richness,y=Selectivity)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + scale_x_continuous(breaks=seq(0,9,1))

#By species
ggplot(selective.matrix,aes(x=Richness,y=Selectivity)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + scale_x_continuous(breaks=seq(0,9,1)) + facet_wrap(~Species)

#Total visits does not effect selectivity, just amount of total feeding
ggplot(selective.matrix,aes(x=N,y=Minutes_High)) + geom_point() + stat_smooth(method="lm")
ggplot(selective.matrix,aes(x=N,y=Minutes_Low)) + geom_point() + stat_smooth(method="lm")
ggplot(selective.matrix,aes(x=N,y=Selectivity)) + geom_point() + stat_smooth(method="lm")

#Effect of increasing time on the high value resource on number of other species.
ggplot(selective.matrix,aes(x=visitsOthers,y=Selectivity)) + geom_point() + stat_smooth(method="lm") 

#pairs plot
require(GGally)
ggpairs(selective.matrix[,c("Selectivity","bph","avgF","Time_Feed","N","visitsOthers")])


ggplot(selective.matrix,aes(x=Minutes_Total,y=Selectivity)) + geom_point()

#######################################
#Elevation and Selectivity Analysis
#######################################

#unweighted
p<-ggplot(selective.matrix[],aes(x=as.numeric(Elevation),Selectivity,col=Species)) + geom_point(size=3) + facet_wrap(~Species) + stat_smooth(method="glm",aes(group=1),family="binomial")
p
ggsave(paste(gitpath,"Figures//Selectivity_Elevation_Unweighted.svg",sep=""),height=8,width=15)

#weighted
p<-ggplot(selective.matrix,aes(x=as.numeric(Elevation),Selectivity,col=Species,size=Minutes_Total)) + geom_point() + facet_wrap(~Species)
p  + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + theme_bw() + xlab("Elevation") + scale_x_continuous(breaks=as.numeric(levels(factor(dat$Elevation))))
ggsave(paste(gitpath,"Figures//Selectivity_Elevation_Unweighted.svg",sep=""),height=8,width=15)

#multimodal?
p<-ggplot(selective.matrix,aes(x=as.numeric(Elevation),Selectivity,col=Species,size=Minutes_Total)) + geom_point() + facet_wrap(~Species)
p  + stat_smooth(method="glm",formula=(y~(poly(x,2))),family="binomial",aes(weight=Minutes_Total)) + theme_bw() + xlab("Elevation") + scale_x_continuous(breaks=as.numeric(levels(factor(dat$Elevation))))

#######################################
#Morphology and Selectivity Analysis
#######################################
#PCA of trait space

# Hum Standardized variables, what to do about NA's?
#Standard the matrix to correct for different units by subtracting the means and dividing by sd
zscore <- apply(hum.morph[,c("Bill","Mass","WingChord","Tarsus_Length","Nail_Length","Wing_Loading")], 2, function(x){
  y<-(x - mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  return(y)
})
rownames(zscore)<-hum.morph$English

#Need to figure out what to do about Na's, we could use closely related species?
trait_pc<-prcomp(na.omit(zscore))

#View Biplot of PC Space
biplot(trait_pc)

#distance in PCA space

d.pca<-as.matrix(dist(trait_pc$x))

#bind loadings 1 and 2 to dataframe
hum_load<-trait_pc$x[,c("PC1","PC2")]
rownames(hum_load)<-rownames(zscore)

#merge data
hum.morph<-merge(hum.morph,hum_load,by.x="English",by.y="row.names")

#merge PCA and original data with selectivity
selective.matrix<-merge(selective.matrix,hum.morph[,!colnames(hum.morph) %in% "Species"],by.x="Species",by.y="English")

#Weighted average of selectivity
ws<-sapply(split(selective.matrix,selective.matrix$Species),function(x){
  weighted.mean(x$Selectivity,x$Total_Time)
})

selective.matrix<-merge(selective.matrix,data.frame(weighted.selectivity=ws),by.x="Species",by.y="row.names")

#average metrics across species
avgStat<-aggregate(selective.matrix[,c("avgF","bph","Time_Feed","N.x")],by=list(selective.matrix$Species),mean,na.rm=TRUE)

colnames(avgStat)[1]<-"Species"

#merge with weighted selectivity
avgStat<-merge(ws,avgStat,by.x="row.names",by.y="Species")
colnames(avgStat)<-c("Species","W.Selectivity","avgF","bph","Time_Feed","N")
rownames(avgStat)<-avgStat$Species

#take out any NA's
avgStat<-avgStat[complete.cases(avgStat),]
pcaStat<-prcomp(avgStat[,-1],scale=TRUE)
biplot(pcaStat)

## Write selectivity tables to file
write.csv(selective.matrix,paste(droppath,"Thesis//Maquipucuna_SantaLucia/Results/Selectivity/Selectivity_Elevation.csv",sep=""))

#Selectivity Descriptors for each species
ggplot(selective.matrix,aes(x=Species,Selectivity)) + geom_boxplot() + theme(axis.text.x=element_text(angle=-90,vjust=-.1))

#Facet by elevation
ggplot(selective.matrix,aes(x=Species,Selectivity,col=as.factor(Elevation))) + geom_boxplot() + theme(axis.text.x=element_text(angle=-90,vjust=-.1)) + facet_wrap(~Elevation,nrow=2)

#Facet by elevation, color by replicate
ggplot(selective.matrix,aes(x=Species,Selectivity,col=Replicate)) + geom_boxplot() + theme(axis.text.x=element_text(angle=-90,vjust=-.1)) + facet_grid(MonthA~Elevation,scale="free_x")
ggsave(paste(gitpath,"Figures/FullTrial.svg",sep=""),dpi=300,height=10,width=10)

#aggregate by species
wss<-aggregate(selective.matrix$weighted.selectivity,by=list(selective.matrix$Species,selective.matrix$PC1,selective.matrix$PC2),mean)
colnames(wss)<-c("Species","PC1","PC2","weighted.selectivity")

biplot(trait_pc)

ggplot(wss,aes(x=PC2,y=weighted.selectivity)) + geom_point() + geom_smooth(method="glm",family="binomial") + geom_text(aes(label=Species),size=2)
ggplot(wss,aes(x=PC1,y=weighted.selectivity)) + geom_point() + geom_smooth(method="glm",family="binomial") + geom_text(aes(label=Species),size=2)

p<-ggplot(selective.matrix,aes(x=PC1,y=Selectivity,size=Minutes_Total,label=Species)) + geom_point() + stat_smooth(method="glm",link="binomial",aes(weight=Minutes_Total))
p

p<-ggplot(selective.matrix,aes(x=PC2,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",link="binomial",aes(weight=Minutes_Total,group=1))
p


#redundancy analysis and behavioral descriptors


###########################################################################
#P
#
###########################################################################
#########################################################
#Selectivity as function of distance to nearest range edge
##########################################################

#Bring in transect data, see Maquipucuna Repo
transect<-read.csv(paste(droppath,"Thesis//Maquipucuna_SantaLucia//Results//HummingbirdTransects//HumTransectRows.csv",sep=""),row.names=1)

#just get summer transect data
head(transect)

levels(transect$Hummingbird.Species)[levels(transect$Hummingbird.Species) %in% c("violet-tailed Slyph","VIolet-tailed Slyph","Violet-tailed Slyph")]<-"Violet-tailed Sylph"
levels(transect$Hummingbird.Species)[levels(transect$Hummingbird.Species) %in% c("Booted Racketail")]<-"Booted Racket-tail"

transect<-transect[transect$Month %in% c(6,7,8),]
dim(transect)

#Range extent for each species
ggplot(transect,aes(x=ele,fill=Hummingbird.Species)) + geom_histogram() + facet_wrap(~Hummingbird.Species)

rangeLim<-aggregate(transect$ele,list(transect$Hummingbird.Species),range,na.rm=TRUE)
rangeLim<-data.frame(rangeLim$Group.1,rangeLim$x)
colnames(rangeLim)<-c("Species","Min","Max")

ggplot(rangeLim,aes(x=Species,ymin=Min,ymax=Max)) + geom_linerange() + coord_flip()

#Distance to range edge for each datapoint
head(dat)

distU<-apply(selective.matrix,1,function(x){
  
  #If species doesn't exist in transect, skip
  if(sum(rangeLim$Species %in% x["Species"])==0) {
    r<-NA
    names(r)<-NA
    return(r)
  }
  
  maxR<-abs(as.numeric(x["Elevation"]) - as.numeric(rangeLim[rangeLim$Species %in% x["Species"],]$Max))
  minR<-abs(as.numeric(x["Elevation"]) - as.numeric(rangeLim[rangeLim$Species %in% x["Species"],]$Min))
  
  
  if(maxR < minR) {return(data.frame(UP="Dist_Upper",RDist=maxR))}
  if(minR < maxR) {return(data.frame(UP="Dist_Lower",RDist=minR))}
  
})

sapply(distU,nrow)
selective.matrix<-data.frame(selective.matrix,rbind.fill(distU))

#Selectivity and distance to range edge.
rangeplot<-ggplot(selective.matrix,aes(RDist,Selectivity,col=Species,size=Minutes_Total)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1)) 
rangeplot + facet_wrap(nrow=2,~Species)

#####################################################
#Selectivity as a function of difference in body size
#####################################################

#Species list for each trial.
sp.lists<-sapply(complete.trial,function(x){
  levels(droplevels(x$Species))
})

#get body size lists for each trial
mass.lists<-lapply(sp.lists,function(x){
  hum.morph[hum.morph$English %in% x, "Mass"]
})


#irect

#For each row in the selectivity matrix, get the difference to the largest species

#get the species with the max body size at the feeder during that trial
selective.matrix$MassD<-sapply(1:nrow(selective.matrix),function(y){
  
  x<-selective.matrix[y,]
  
  #find the index in the list
  index<-paste(paste(x["Elevation"],x[["Date"]],sep="."),x[["Replicate"]],sep=".")
  
  mass_T<-mass.lists[names(mass.lists) %in% index]
  
  
  #weighted mass difference, the mass to all species
  #multiple total time feeding by mass
  
  #get trial, with all species not included in the focal y row
  trialT<-selective.matrix[selective.matrix$Elevation %in% x[["Elevation"]]& selective.matrix$Date %in% x[["Date"]] & selective.matrix$Replicate %in% x[["Replicate"]] & !selective.matrix$Species==x[["Species"]] ,]  
  
  #create the mass "environment" ie mean mass difference between the focal species and all competitors, weighted by time feeding.
  #first get mass difference by each species and their time
  diff<-x$Mass - trialT$Mass
  
  diff<-diff[is.finite(diff)]
  #mean weighted differences
  weight.diff<-weighted.mean(diff,trialT$Minutes_Total,na.rm=TRUE)
  
  return(weight.diff)})

massplot<-ggplot(selective.matrix,aes(x=MassD,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(group=1))
massplot + facet_wrap(~Elevation)

###Difference in weight between the last bird that fed and the high or low value feeder



#################Repeat for PC1######################
#####################################################
#Selectivity as a function of difference in body size
#####################################################

#get body size lists for each trial
PC1.lists<-lapply(sp.lists,function(x){
  hum.morph[hum.morph$English %in% x, "PC1"]
})

#For each row in the selectivity matrix, get the difference to the largest species

#get the species with the max body size at the feeder during that trial
selective.matrix$PC1D<-sapply(1:nrow(selective.matrix),function(y){
  
  x<-selective.matrix[y,]
  
  #find the index in the list
  index<-paste(paste(x["Elevation"],x[["Date"]],sep="."),x[["Replicate"]],sep=".")
  
  mass_T<-PC1.lists[names(PC1.lists) %in% index]
  
  #weighted mass difference, the mass to all species
  #multiple total time feeding by mass
  
  #get trial, with all species not included in the focal y row
  trialT<-selective.matrix[selective.matrix$Elevation %in% x[["Elevation"]]& selective.matrix$Date %in% x[["Date"]] & selective.matrix$Replicate %in% x[["Replicate"]] & !selective.matrix$Species==x[["Species"]] ,]  
  
  #create the mass "environment" ie mean mass difference between the focal species and all competitors, weighted by time feeding.
  #first get mass difference by each species and their time
  diff<-x$PC1 - trialT$PC1
  
  diff<-diff[is.finite(diff)]
  #mean weighted differences
  weight.diff<-weighted.mean(diff,trialT$Minutes_Total,na.rm=TRUE)
  
  return(weight.diff)})

PC1D<-ggplot(selective.matrix,aes(x=PC1D,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1))
PC1D + facet_wrap(~Elevation)


#################Repeat for PC2######################
#####################################################
#Selectivity as a function of difference in body size
#####################################################

#get body size lists for each trial
PC2.lists<-lapply(sp.lists,function(x){
  hum.morph[hum.morph$English %in% x, "PC2"]
})


#For each row in the selectivity matrix, get the difference to the largest species

#get the species with the max body size at the feeder during that trial
selective.matrix$PC2D<-sapply(1:nrow(selective.matrix),function(y){
  
  x<-selective.matrix[y,]
  
  #find the index in the list
  index<-paste(paste(x["Elevation"],x[["Date"]],sep="."),x[["Replicate"]],sep=".")
  
  mass_T<-PC2.lists[names(PC2.lists) %in% index]
  
  #weighted mass difference, the mass to all species
  #multiple total time feeding by mass
  
  #get trial, with all species not included in the focal y row
  trialT<-selective.matrix[selective.matrix$Elevation %in% x[["Elevation"]]& selective.matrix$Date %in% x[["Date"]] & selective.matrix$Replicate %in% x[["Replicate"]] & !selective.matrix$Species==x[["Species"]] ,]  
  
  #create the mass "environment" ie mean mass difference between the focal species and all competitors, weighted by time feeding.
  #first get mass difference by each species and their time
  diff<-x$PC2 - trialT$PC2
  
  diff<-diff[is.finite(diff)]
  #mean weighted differences
  weight.diff<-weighted.mean(diff,trialT$Minutes_Total,na.rm=TRUE)
  
  return(weight.diff)})

PC2D<-ggplot(selective.matrix,aes(x=PC2D,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1))
PC2D + facet_grid(MonthA~Elevation)

#################Multivariate distance######################
#####################################################
#Selectivity as a function of difference in body size
#####################################################


#For each row in the selectivity matrix, get the difference to the largest species

#get the species with the max body size at the feeder during that trial
selective.matrix$MultD<-sapply(1:nrow(selective.matrix),function(y){
  
  x<-selective.matrix[y,]
  
  #weighted mass difference, the mass to all species
  #multiple total time feeding by mass
  
  #get trial, with all species not included in the focal y row
  trialT<-selective.matrix[selective.matrix$Elevation %in% x[["Elevation"]]& selective.matrix$Date %in% x[["Date"]] & selective.matrix$Replicate %in% x[["Replicate"]] & !selective.matrix$Species==x[["Species"]] ,]  
  
  #get mean multivariate distance
  PCdiff<-d.pca[as.character(x[["Species"]]),as.character(trialT[["Species"]])]
  
  PCdiff<-PCdiff[is.finite(PCdiff)]
  #mean weighted differences
  weight.diffW<-weighted.mean(PCdiff,trialT$Minutes_Total,na.rm=TRUE)
  weight.diff<-mean(PCdiff,na.rm=TRUE)
  
  return(weight.diffW)})

MultD<-ggplot(data=selective.matrix,aes(x=MultD,y=Selectivity,size=Minutes_Total,col=Species)) + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1)) + geom_point()
MultD + facet_wrap(~Species)
#####################################################
#Selectivity and Available Resource
#####################################################

#read in flower totals from FlowerTransects.R
fltransects<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/CleanedHolgerTransect.csv",sep=""),row.names=1)

#just summer data
fltransects<-droplevels(fltransects[fltransects$month %in% c(6,7,8),])

fltransects$GPS_ID<-as.numeric(as.character(fltransects$GPS_ID))

#Bring in video data
vid<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",sep=""),row.names=1)

#just get videos from the summer months
vid<-vid[vid$Month %in% c(6,7,8),]

#match with spatial infodim
gps<-readShapePoints(paste(droppath,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp",sep=""))@data

gps<-droplevels(gps[gps$MonthID %in% c(6,7,8),])

gps$GPS_ID<-as.numeric(as.character(gps$GPS_ID))

#GPSID + month
gps$mergeID<-paste(gps$GPS_ID,gps$MonthID,sep="_")

fltransects$mergeID<-paste(fltransects$GPS_ID,fltransects$month,sep="_")

table(fltransects$mergeID %in% gps$mergeID)
table(fltransects$GPS_ID %in% gps$GPS_ID)

fltransects<-merge(fltransects,gps,by="mergeID")

#where elevation failed, take the midpoint of the transect

fltransects[is.na(fltransects$ele),"ele"]<-apply(fltransects[is.na(fltransects$ele),c("Elevation.Begin","Elevation.End")],1,mean)

#Split videos into species lists
vid.s<-split(vid,list(vid$Hummingbird))

selective.matrix$fl_s<-sapply(1:nrow(selective.matrix),function(g){
  
  #grab row
  x<-selective.matrix[g,]
  
  #Get the hummingbird index
  flIndex<-vid.s[[as.character(x$Species)]]
  
  #Which species does the bird feed on
  fl.sp<-flIndex$Iplant_Double
  
  #dates within two weeks either side. 
  
  dateL<-abs(difftime(as.POSIXlt(x[["Date"]],format="%m/%d/%Y"),  as.POSIXlt(fltransects$Date,format="%m/%d/%Y"),units="weeks")) <= 2
  
  flower.month<-fltransects[fltransects$Iplant_Double %in% fl.sp & dateL & (fltransects$ele > x[["Elevation"]]-100 &fltransects$ele < x[["Elevation"]]+ 100) ,]
  if(nrow(flower.month)==0) return(NA)
  
  mean.fl<-aggregate(flower.month$Total_Flowers,list(flower.month$Transect.ID),mean,na.rm=TRUE)
  
  return(mean(mean.fl$x,na.rm=TRUE))})


keep<-names(which(table(selective.matrix$Species) > 3))

resourceplotS<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=fl_s,y=Selectivity,col=factor(Elevation))) + geom_point() +  stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1))
resourceplotS+ facet_wrap(~Species) 

resourceplot<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=fl_s,y=Minutes_Total,col=factor(Elevation))) + geom_point() + stat_smooth(method="lm",aes(group=1))
resourceplot + facet_wrap(~Species,scales="free")

####PLot all three together
jpeg("Thesis/Selectivity/HypothesisPlot.jpeg",res=300,height=5,width=20,units="in")
multiplot(resourceplot,massplot,rangeplot,cols=3)
dev.off()

# ############Quick look at temperature and elevation
# require(scales)
# ggplot(dat,aes(x=chron(Time.Begin),y=Temp,col=factor(Elevation))) + geom_smooth(se=FALSE) + scale_x_chron(format="%H")



