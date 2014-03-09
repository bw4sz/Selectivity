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

#I think i the weighted regression is weird, just take out data where the bird fed less than 

# plot(ecdf(selective.matrix$Minutes_Total))
# 
# ecdf(selective.matrix$Minutes_Total) (1)
# 
# hist(selective.matrix$Minutes_Total,breaks=seq(0,40,.5))
# hist(selective.matrix$Minutes_High,breaks=seq(0,40,.5))
# hist(selective.matrix$Minutes_Low,breaks=seq(0,40,.5))

#Take out birds feeding less than 1min over the 6hours
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

#Effects of increasing visits on selectivity
ggplot(selective.matrix,aes(x=Tvisits,y=Time_High)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total))

#Effect of increasing time on the high value resource on number of other species.
ggplot(selective.matrix,aes(x=visitsOthers,y=Selectivity)) + geom_point() + stat_smooth(method="lm") 

#pairs plot
require(GGally)
ggpairs(selective.matrix[,c("Selectivity","bph","avgF","Time_Feed","N","visitsOthers")])

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

#aggregate by species
wss<-aggregate(selective.matrix$weighted.selectivity,by=list(selective.matrix$Species,selective.matrix$PC1,selective.matrix$PC2),mean)
colnames(wss)<-c("Species","PC1","PC2","weighted.selectivity")

ggplot(selective.matrix[selective.matrix$Elevation==1500,],aes(x=Species,y=Selectivity,col=Date)) + geom_point(size=6) + facet_wrap(~Replicate)

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

massplot<-ggplot(selective.matrix,aes(x=MassD,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1))
massplot + facet_wrap(~Elevation)

#####################################################
#Selectivity and Available Resource
#####################################################

#read in flower totals from FlowerTransects.R
fltransects<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/CleanedHolgerTransect.csv",sep=""),row.names=1)

#just summer data
fltransects<-fltransects[fltransects$month %in% c(6,7,8),]

#Bring in video data
vid<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",sep=""),row.names=1)

#just get videos from the summer months
vid<-vid[vid$Month %in% c(6,7,8),]

#match with spatial info
gps<-readShapePoints(paste(droppath,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp",sep=""))@data

gps<-droplevels(gps[gps$MonthID %in% c(6,7,8),])

gps$nameNumeric<-as.numeric(gps$name)

mergdat<-merge(fltransects,gpx.dat,by.y="name",by.x="GPS_ID")

missingD<-fltransects$GPS_ID[!fltransects$GPS_ID %in% mergdat$GPS_ID]

readsh(gps,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp")

head(fltransects[is.na(fltransects$GPS_ID),])
#Split videos into species lists
vid.s<-split(vid,list(vid$Hummingbird))

selective.matrix$fl_s<-sapply(1:nrow(selective.matrix),function(g){
x<-selective.matrix[g,]

#Get the hummingbird index
flIndex<-vid.s[[x$Species]]

#Which species does the bird feed on
fl.sp<-flIndex$Iplant_Double

#How many of those flowers are within the elevation gradient at that month?
#within the 400m gradient

if(x[["Elevation"]]==1700){
  
  flower.month<-fltransects[fltransects$Iplant_Double %in% fl.sp & fltransects$month %in% which(month.abb==x$MonthA) & (fltransects$Elevation.End %in% x$Elevation),]
  
}

if(!x[["Elevation"]] == 1700){
  
  flower.month<-fltransects[fltransects$Iplant_Double %in% fl.sp & fltransects$month %in% which(month.abb==x$MonthA) & (fltransects$Elevation.Begin %in% x$Elevation|fltransects$Elevation.End %in% x$Elevation),]
  
}


#if elevation is 1700, we don't want to grab the transects from above, since they aren't on the same side.

#aggregate on a per transect basis
if(nrow(flower.month)== 0){return(0)}

mean.fl<-aggregate(flower.month$Total_Flowers,list(flower.month$Transect.ID),mean)

return(mean(mean.fl$x,na.rm=TRUE))})

resourceplotS<-ggplot(selective.matrix,aes(x=fl_s,y=Selectivity,col=factor(Elevation))) + geom_point() +  stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1))
resourceplotS+ facet_wrap(~Species,scales="free") 

resourceplot<-ggplot(selective.matrix,aes(x=fl_s,y=Minutes_Total,col=factor(Elevation))) + geom_point() + stat_smooth(method="lm",aes(group=1))
resourceplot + facet_wrap(~Species,scales="free")

####PLot all three together
jpeg("Thesis/Selectivity/HypothesisPlot.jpeg",res=300,height=5,width=20,units="in")
multiplot(resourceplot,massplot,rangeplot,cols=3)
dev.off()

# ############Quick look at temperature and elevation
# require(scales)
# ggplot(dat,aes(x=chron(Time.Begin),y=Temp,col=factor(Elevation))) + geom_smooth(se=FALSE) + scale_x_chron(format="%H")



