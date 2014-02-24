
#Step 1 - Set up data environment and load in data
################################################################
#load in packages
require(ggplot2)
require(chron)
require(reshape)

#set gitpath
gitpath<-"C:/Users/Office653-1//Documents/GitHub/Selectivity/"

#source functions
source(paste(gitpath,"functions.R",sep=""))

#Set working directory
droppath<-"C:/Users/office653-1/Dropbox/"
setwd(droppath)

##Read in data
dat<-read.csv(paste(droppath,"Thesis//Maquipucuna_SantaLucia/Data2013/csv/CompetitionFeeders.csv",sep=""))

dat$Sex<-toupper(dat$Sex)

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv",row.names=1)


#####################################
#Step 2 - Data Cleaning and Sampling
#####################################

#take out the unknown species
dat<-dat[!dat$Species %in% "UKWN",]

#Which dates need to be matched?
vid_totals_date<-aggregate(dat$Video,list(dat$Elevation,dat$Treatment,dat$Date,dat$Replicate),function(x) nlevels(droplevels(x)))
vid_totals_date<-cast(vid_totals_date,Group.1 + Group.3 + Group.4~Group.2)

print(vid_totals_date)

#Species richness and identity at each elevation
sp_matrixHL<-(table(dat$Species,dat$Elevation,dat$Treatment) >= 1) * 1

#View species at each elevation and treatment
m.sp_m<-melt(sp_matrixHL)
colnames(m.sp_m)<-c("Species","Elevation","Treatment","Presence")

#turn 0's to NA's just for plotting
m.sp_m[m.sp_m$Presence==0,"Presence"]<-NA

#richness across feeders
p<-ggplot(m.sp_m,aes(y=Species,x=factor(Elevation),fill=as.factor(Presence)))+ geom_tile() + theme_bw() + scale_fill_discrete(na.value="white")
p + labs(fill="Present",x="Elevation")
ggsave(paste(gitpath,"Figures/RangeExtentFeeders.svg",sep=""),dpi=300,height=8,width=11)

#####################
#Time on the feeders
####################

#Create time columns
dat$Time.End<-times(dat$Time.End)
dat$Time.Begin<-times(dat$Time.Begin)

#Find time difference 
dat$Time_Feeder_Obs<-dat$Time.End - dat$Time.Begin

#Get any rownumbers that are negative, these need to be fixed. 
dat[which(dat$Time_Feeder_Obs < 0),]

#Total Time per species
Total_Time_Species<-aggregate(dat$Time_Feeder_Obs,by=list(dat$Species),sum,na.rm=TRUE) 
colnames(Total_Time_Species)<-c("Species","TotalTime")
Total_Time_Species$Time<-minutes(Total_Time_Species$TotalTime)+seconds(Total_Time_Species$TotalTime)/60
ggplot(Total_Time_Species,aes(Species,Time)) + geom_bar() + theme_bw() + ylab("Minutes on Feeders") + theme(axis.text.x=element_text(angle=-90,vjust=-.1))

#mean time feeding bout
Mean_Time_Species<-aggregate(dat$Time_Feeder_Obs,by=list(dat$Species),mean,na.rm=TRUE) 
colnames(Mean_Time_Species)<-c("Species","Mean_Time")
ggplot(Mean_Time_Species,aes(Species,Mean_Time)) + geom_bar() + theme(axis.text.x=element_text(angle=-90))

#Average time feeding for all species, and each species
ggplot(dat,aes(x=seconds(Time_Feeder_Obs))) + geom_histogram()
ggsave(paste(gitpath,"Figures/Feedingtime.svg",sep=""),dpi=300,height=8,width=11)

#average visits per hour
S_H<-table(hours(dat$Time.Begin),dat$Species)

#Create a species list for each video

####Match each trial together, trials are done on the same day at the same elevation
#Split data into a list, with each compenent being one trial pair
Trials<-split(dat, list(dat$Elevation,dat$Date,dat$Replicate),drop=TRUE)

#####Just for data clarity remove any trials that down have high and low value data entered
#Get number of levels per trial
levels.trial<-lapply(Trials,function(x) nlevels(factor(x$Treatment)))

#Only use trials that have a high and low, ie levels=2
complete.trial<- Trials[levels.trial ==2]

#We want to compute for each trial, the number of seconds feeding, the selectivity for each species, time between feeding bouts, total number of feeding seconds.

####Within trial metrics per species

Tdata<-lapply(complete.trial,function(x){
  a<-selective(x)
  b<-bph(x)
  d<-avgF(x)
  dat.trials<-merge(merge(a,b),d)
  Elevation=unique(x$Elevation)
  Date=unique(x$Date)
  Replicate=unique(x$Replicate)
  #Richness at that feeder
  Trichness<-length(levels(droplevels(x$Species)))
  #Total visits
  Tvisits<-nrow(x)
  out<-data.frame(dat.trials,Elevation,Date,Replicate,Richness=Trichness,Tvisits)
return(out)})


selective.matrix<-rbind.fill(Tdata)

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

plot(ecdf(selective.matrix$Minutes_Total))

ecdf(selective.matrix$Minutes_Total) (.5)

hist(selective.matrix$Minutes_Total,breaks=seq(0,40,.5))
hist(selective.matrix$Minutes_High,breaks=seq(0,40,.5))
hist(selective.matrix$Minutes_Low,breaks=seq(0,40,.5))

#Take out birds feeding less than 1min over the 6hours
selective.matrix<-selective.matrix[selective.matrix$Minutes_Total > 1,]


##Descriptive stats complete


###############Optimal Foraging

#Birds should prefer high value resources
ggplot(selective.matrix,aes(x=Minutes_High,Minutes_Low)) + geom_point() + geom_abline()

#Optimal foraging says that individuals should occupy patches at a rate equal to their quality
sH<-sum(selective.matrix$Minutes_High)
sL<-sum(selective.matrix$Minutes_Low)

#Optimal foraging says its should be three
print(sH/sL)

#Competition keeps birds from occupying higher quality patch

#Effect of increasing richness on selectivity
ggplot(selective.matrix,aes(x=Richness,y=Selectivity)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + scale_x_continuous(breaks=seq(0,9,1))

#Effects of increasing visits on selectivity
ggplot(selective.matrix,aes(x=Tvisits,y=Selectivity)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + scale



#####################################
#Correlations among data
#####################################

#pairs plot
ggpairs(selective.matrix[,c("Selectivity","bph","avgF","Elevation","Minutes_High","Minutes_Low","Minutes_Total","MonthA")])

##########################
#merge with morphology
##########################
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

hum.morph<-merge(hum.morph,hum_load,by.x="English",by.y="row.names")

#merge PCA and original data with selectivity
selective.matrix<-merge(selective.matrix,hum.morph[,!colnames(hum.morph) %in% "Species"],by.x="Species",by.y="English")

#Weighted average of selectivity
ws<-sapply(split(selective.matrix,selective.matrix$Species),function(x){
  weighted.mean(x$Selectivity,x$Total_Time)
})

selective.matrix<-merge(selective.matrix,data.frame(weighted.selectivity=ws),by.x="Species",by.y="row.names")

#average metrics
#Weighted average of selectivity
ws<-sapply(split(selective.matrix,selective.matrix$Species),function(x){
  weighted.mean(x$Selectivity,x$Total_Time)
})

#average metrics across species
avgStat<-aggregate(selective.matrix[,c("avgF","bph")],by=list(selective.matrix$Species),mean,na.rm=TRUE)
colnames(avgStat)<-c("Species","avgF")

#merge with weighted selectivity
avgStat<-merge(ws,avgStat,by.x="row.names",by.y="Species")
colnames(avgStat)<-c("Species","W.Selectivity","avgF","bph")
rownames(avgStat)<-avgStat$Species

pcaStat<-prcomp(avgStat[,-1],scale=TRUE)
biplot(pcaStat)

########################################
#Plotting Selectivity across elevation
########################################

#get only species that have atleast 2min
sumT<-aggregate(selective.matrix$Minutes_Total,list(selective.matrix$Species),sum)
speciesSkip<-sumT[sumT$x < 2,]$Group.1

#unweighted
p<-ggplot(selective.matrix[!selective.matrix$Species %in% speciesSkip, ],aes(x=as.numeric(Elevation),Selectivity,col=Species)) + geom_point(size=3) + facet_wrap(~Species) + stat_smooth(method="glm",aes(group=1))
p + ylim(0,1)
ggsave(paste(gitpath,"Figures//Selectivity_Elevation_Unweighted.svg",sep=""),height=8,width=15)

#unweighted with a threshold
p<-ggplot(selective.matrix[!selective.matrix$Species %in% speciesSkip & selective.matrix$Minutes_Total > 1, ],aes(x=as.numeric(Elevation),Selectivity,col=Species)) + geom_point(size=3) + facet_wrap(~Species) + stat_smooth(method="glm",family="binomial",aes(group=1))
p + ylim(0,1)
ggsave(paste(gitpath,"Figures//Selectivity_Elevation_Unweighted.svg",sep=""),height=8,width=15)

#weighted
p<-ggplot(selective.matrix[!selective.matrix$Species %in% speciesSkip & selective.matrix$Minutes_Total > 1,],aes(x=as.numeric(Elevation),Selectivity,col=Species,size=Minutes_Total)) + geom_point() + facet_wrap(~Species)
p  + stat_smooth(method="glm",family="binomial") + theme_bw() + xlab("Elevation") + scale_x_continuous(breaks=as.numeric(levels(factor(dat$Elevation))))
ggsave(paste(gitpath,"Figures//Selectivity_Elevation_Unweighted.svg",sep=""),height=8,width=15)

# #weighted and time
p<-ggplot(selective.matrix[!selective.matrix$Species %in% speciesSkip,],aes(x=as.numeric(Elevation),Selectivity,col=MonthA,size=Minutes_Total)) + geom_point() + facet_wrap(~Species)
p
p  + geom_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + theme_bw() + xlab("Elevation") + stat_smooth()

## Write selectivity tables to file
write.csv(selective.matrix,paste(droppath,"Thesis//Maquipucuna_SantaLucia/Results/Selectivity/Selectivity_Elevation.csv",sep=""))

##Split out by date?
head(selective.matrix)

#######################################
#Selectivity, Phylogeny and Morphology
#######################################

#Selectivity Descriptors for each species
ggplot(selective.matrix[!selective.matrix$Species %in% speciesSkip,],aes(x=Species,Selectivity)) + geom_boxplot() + theme(axis.text.x=element_text(angle=-90,vjust=-.1))

#aggregate
wss<-aggregate(selective.matrix$weighted.selectivity,by=list(selective.matrix$Species,selective.matrix$PC1,selective.matrix$PC2),mean)
colnames(wss)<-c("Species","PC1","PC2","weighted.selectivity")

ggplot(wss,aes(x=PC2,y=weighted.selectivity)) + geom_point() + geom_smooth(method="glm",family="binomial") + geom_text(aes(label=Species),size=2)
ggplot(wss,aes(x=PC1,y=weighted.selectivity)) + geom_point() + geom_smooth(method="glm",family="binomial") + geom_text(aes(label=Species),size=2)

p<-ggplot(selective.matrix,aes(x=PC1,y=Selectivity,size=Minutes_Total,label=Species)) + geom_point() + stat_smooth(method="glm",link="binomial",aes(weight=Minutes_Total))
p

p<-ggplot(selective.matrix,aes(x=PC2,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",link="binomial",aes(weight=Minutes_Total,group=1))
p

##########################################
#Selectivity as a function of body size
###################################

p<-ggplot(selective.matrix,aes(x=Mass,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",link="binomial",aes(weight=Minutes_Total,group=1))
p


##########################################
#Compare Selectivity to Available Resource, incomplete, needs to be adjusted to per day
##########################################

#read in flower totals from FlowerTransects.R
read.csv(paste(droppath,"FlowerTransects/FlowerTotals.csv"))

#TH

#Create a transect R column
selective.matrix$Elev<-paste(selective.matrix$Elevation,(as.numeric(selective.matrix$Elevation) + 200),sep="_")

#Create a month column
selective.matrix$Elev

#Merge
selective.fl<-merge(fl.totals,selective.matrix)

#For now, aggregate across months?
ggplot(selective.fl,aes(x=as.numeric(TotalFlowers),Selectivity,col=Species,size=Minutes_Total)) + geom_point() + facet_wrap(~Species)

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

ggplot(dat,aes(x=dat$Time_Stamp,fill=Species)) + geom_histogram(position="dodge") + facet_wrap(~Elevation)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/TimeofDayElevation.svg",height=11,width=8,dpi=300)

