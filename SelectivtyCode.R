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
require(doSNOW)
require(vegan)
require(plyr)
require(rms)

#set gitpath
gitpath<-"C:/Users/Ben/Documents/Selectivity/"

#source functions
source(paste(gitpath,"functions.R",sep=""))

#Set working directory
droppath<-"C:/Users/Ben/Dropbox/"

#load data if needed
load("Selectivity.RData")

#Read in data
dat<-read.csv(paste(droppath,"Thesis//Maquipucuna_SantaLucia/Data2013/csv/CompetitionFeeders.csv",sep=""))

#Make all entries capital
dat$Sex<-toupper(dat$Sex)

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv",sep=""),row.names=1)

dat<-dat[,1:12]

#flower transect data
#read in flower totals from FlowerTransects.R
fltransects<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/CleanedHolgerTransect.csv",sep=""),row.names=1)

#Bring in video data
vid<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",sep=""),row.names=1)

#####################################
#Step 2 - Data Cleaning and Sampling
#####################################

#Which dates need to be matched?
vid_totals_date<-aggregate(dat$Video,list(dat$Elevation,dat$Treatment,dat$Date,dat$Replicate),function(x) nlevels(droplevels(x)))
vid_totals_date<-cast(vid_totals_date,Group.1 + Group.3 + Group.4~Group.2)

colnames(vid_totals_date)<-c("Elevation","Date","Replicate(O_R)","High Feeder","Low Feeder")

print(vid_totals_date)

###General stats on sampling

#Number of days
paste("Number of days",nrow(vid_totals_date),sep=": ")

#no unknown species
dat<-dat[!dat$Species %in% "UKWN",]

#One misspelling of green crowned brilliant
levels(dat$Species)[levels(dat$Species) %in% "Green-crowned Brillaint"]<-"Green-crowned Brilliant"

#Create time columns
dat$Time.End<-chron::times(dat$Time.End)
dat$Time.Begin<-chron::times(dat$Time.Begin)

#Find time difference 
dat$Time_Feeder_Obs<-dat$Time.End - dat$Time.Begin

#Get any rownumbers that are negative, these need to be fixed. 
dat[which(dat$Time_Feeder_Obs < 0),]

#average visits per hour
S_H<-table(hours(dat$Time.Begin),dat$Species)

##################################
#Species Presence and Time
##################################

#Create overall date stamp
dat$Time_Stamp<-as.POSIXct(chron(dates=as.character(dat$Date),dat$Time.Begin))

# #Time and species occurence, facetted by elevation
# ggplot(dat,aes(x=strptime(dat$Time.Begin,"%H:%M"),fill=Species)) + geom_histogram(position="dodge") + facet_wrap(~Elevation)
# ggsave("Thesis//Maquipucuna_SantaLucia/Results/TimeofDayElevation.svg",height=11,width=8,dpi=300)
# 
# #Overall Month_Day and Elevation
# ggplot(dat,aes(y=factor(Elevation),x=dat$Time_Stamp,col=Species)) + geom_point(size=3) + scale_x_datetime() + facet_wrap(~Species)
# ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=11,width=8,dpi=300)

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

#Total Time per species per elevation
Total_Time_Species<-aggregate(dat$Time_Feeder_Obs,by=list(dat$Species,dat$Elevation),sum,na.rm=TRUE) 
colnames(Total_Time_Species)<-c("Species","Elevation","TotalTime")
Total_Time_Species$Time<-minutes(Total_Time_Species$TotalTime)+seconds(Total_Time_Species$TotalTime)/60

p<-ggplot(Total_Time_Species,aes(y=Species,as.factor(Elevation),fill=Time)) + geom_tile() + theme_bw() + ylab("Minutes on Feeders") + theme(axis.text.x=element_text(angle=-90,vjust=-.1)) + scale_fill_continuous(low="gray90",high="gray10")
p+labs(x="Elevation",y="Species",fill="Minutes Feeding")
ggsave(paste(gitpath,"Figures/TimeFeeding.pdf",sep=""),dpi=300,height=8,width=11)
ggsave(paste(gitpath,"Figures/TimeFeeding.jpg",sep=""),dpi=300,height=8,width=11)

#mean time feeding bout
Mean_Time_Species<-aggregate(dat$Time_Feeder_Obs,by=list(dat$Species),mean,na.rm=TRUE) 
colnames(Mean_Time_Species)<-c("Species","Mean_Time")

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
complete.trials<-lapply(1:length(complete.trial),function(g){
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
  
  order.x$Mass_diff<-NULL
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
})

order.trials<-rbind.fill(complete.trials)

#Mass of most recent bird and feeder choice
ggplot(order.trials,aes(x=Mass_diff,y=as.numeric(Treatment)-1)) + geom_point() + geom_smooth(family="binomial",method="glm",aes(weight=minutes(Time_Feeder_Obs) + seconds(Time_Feeder_Obs))) + facet_wrap(~Species)

#Average time since feeding
ggplot(order.trials,aes(x=Species,TimeSince)) + geom_boxplot() + theme(axis.text.x=element_text(angle=-90))

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
  

  #time since last species visit. 
    
  #merge data
  dat.trials<-merge(merge(merge(merge(merge(a,b),d),k),tss),visitsOthers)
  Elevation=unique(x$Elevation)
  Date=unique(x$Date)
  Trichness<-length(a[minutes(chron::times(a$Time_High + a$Time_Low)) > 1,]$Species)
  Replicate=unique(x$Replicate)
  
  #Total visits
  Tvisits<-nrow(x)
  
  out<-data.frame(dat.trials,Elevation,Date,Replicate,Richness=Trichness,Tvisits)
  return(out)})

#Bind dataset to a dataframe
selective.matrix<-rbind.fill(Tdata)

#Create times argument
selective.matrix$Time_High<-chron::times(selective.matrix$Time_High)
selective.matrix$Time_Low<-chron::times(selective.matrix$Time_Low)
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

#set minimal number of datapoints. 
keep<-names(which(table(selective.matrix$Species) > 3))

#Effect of increasing richness on selectivity
ggplot(selective.matrix,aes(x=Richness,y=Selectivity)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + scale_x_continuous(breaks=seq(0,9,1))

#By species
ggplot(selective.matrix,aes(x=Richness,y=Selectivity)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + scale_x_continuous(breaks=seq(0,9,1)) + facet_wrap(~Species)

#Total visits does not effect selectivity, just amount of total feeding
ggplot(selective.matrix,aes(x=N,y=Minutes_High)) + geom_point() + stat_smooth(method="lm")
ggplot(selective.matrix,aes(x=N,y=Minutes_Low)) + geom_point() + stat_smooth(method="lm")
ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=N,y=Selectivity)) + geom_point() + ylim(0,1) + stat_smooth(method="lm") + facet_wrap(~Species,scales="free_x")
ggsave(paste(gitpath,"Figures//Selectivity_Abundance.svg",sep=""),height=8,width=15)

#Effect of increasing time on the high value resource on number of other species.
ggplot(selective.matrix,aes(x=visitsOthers,y=Selectivity)) + geom_point() + stat_smooth(method="lm") 

#pairs plot
require(GGally)
#ggpairs(selective.matrix[,c("Selectivity","bph","avgF","Time_Feed","N","visitsOthers")])

ggplot(selective.matrix,aes(x=Minutes_Total,y=Selectivity)) + geom_point()

#######################################
#Elevation and Selectivity Analysis
#######################################

#unweighted
p<-ggplot(selective.matrix[],aes(x=as.numeric(Elevation),Selectivity)) + geom_point(size=3) + facet_wrap(~Species) + stat_smooth(method="glm",aes(group=1),family="binomial")
p + theme_bw()

ggsave(paste(gitpath,"Figures//Selectivity_Elevation_Unweighted.svg",sep=""),height=8,width=15)

#weighted
p<-ggplot(selective.matrix,aes(x=as.numeric(Elevation),Selectivity,col=Species,size=Minutes_Total)) + geom_point() + facet_wrap(~Species)
p  + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + theme_bw() + xlab("Elevation") + scale_x_continuous(breaks=as.numeric(levels(factor(dat$Elevation))))
ggsave(paste(gitpath,"Figures//Selectivity_Elevation_Unweighted.svg",sep=""),height=8,width=15)

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
trait_pc<-prcomp(zscore)

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
avgStat<-aggregate(selective.matrix[,c("bph")],by=list(selective.matrix$Species),mean,na.rm=TRUE)

colnames(avgStat)[1]<-"Species"

#merge with weighted selectivity
avgStat<-merge(ws,avgStat,by.x="row.names",by.y="Species")
colnames(avgStat)<-c("Species","Weighted.Selectivity","Visits.per.hour")
rownames(avgStat)<-avgStat$Species

#take out any NA's
avgStat<-avgStat[complete.cases(avgStat),]

#take out species without enough data
avgStat<-avgStat[avgStat$Species %in% keep,]

pcaStat<-prcomp(avgStat[,-c(1)],scale=TRUE)
biplot(pcaStat)

## Write selectivity tables to file
#write.csv(selective.matrix,paste(droppath,"Thesis//Maquipucuna_SantaLucia/Results/Selectivity/Selectivity_Elevation.csv",sep=""))

#Selectivity Descriptors for each species
ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=Species,Selectivity)) + geom_boxplot() + theme(axis.text.x=element_text(angle=-90,vjust=-.1))

#Facet by elevation
ggplot(selective.matrix,aes(x=Species,Selectivity,col=as.factor(Elevation))) + geom_boxplot() + theme(axis.text.x=element_text(angle=-90,vjust=-.1)) + facet_wrap(~Elevation,nrow=2)

#Facet by elevation, color by replicate
ggplot(selective.matrix,aes(x=Species,Selectivity,col=Replicate)) + geom_boxplot() + theme_bw() + theme(axis.text.x=element_text(angle=-90,vjust=-.1)) + facet_grid(MonthA~Elevation,scale="free_x") + geom_point()
ggsave(paste(gitpath,"Figures/FullTrial.svg",sep=""),dpi=300,height=6,width=10)
ggsave(paste(gitpath,"Figures/FullTrial.jpeg",sep=""),dpi=300,height=7,width=11)

#aggregate by species
wss<-aggregate(selective.matrix$weighted.selectivity,by=list(selective.matrix$Species,selective.matrix$PC1,selective.matrix$PC2),mean)
colnames(wss)<-c("Species","PC1","PC2","weighted.selectivity")

pdf("Figures/morphPCA.pdf")
biplot(prcomp(trait.f[rownames(trait.f) %in% keep,],scale=TRUE))
dev.off()

jpeg("Figures/morphPCA.jpeg")
biplot(prcomp(trait.f[rownames(trait.f) %in% keep,],scale=TRUE))
dev.off()


##cca 

#get the metrics from the feeders
feeder<-avgStat[,-c(1)]
#traits<-hum.morph[,c("Bill","Mass","WingChord","Tarsus_Length","Nail_Length","Wing_Loading")]

traits<-hum.morph[,c("Bill","Mass","WingChord","Nail_Length","Foot_Extension")]
rownames(traits)<-hum.morph$English

trait.f<-traits[rownames(traits) %in% rownames(feeder),]

#same order?
trait.fs<-trait.f[sort(rownames(feeder)),]

fe<-as.data.frame(feeder$Weighted.Selectivity)
rownames(fe)<-rownames(feeder)
pdf("Figures/RDA.pdf")
plot(rd.out<-rda(X=fe,Y=trait.fs),scaling=3)
dev.off()
summary(rd.out)

jpeg("Figures/RDA.jpg",res=300,width=7,height=7,units="in")
plot(rd.out<-rda(X=feeder[,],Y=trait.fs[],scale=TRUE,),scaling=1)
dev.off()
summary(rd.out)
#break into species and elevation

# selective.agg<-aggregate(selective.matrix[,c("Selectivity","bph","Time_Feed")],list(selective.matrix$Elevation,selective.matrix$Species),mean,na.rm=TRUE)
# selective.split<-split(selective.agg,list(selective.agg$Group.1))
# 
# feeder<-selective.split[[2]]
# 
# rownames(feeder)<-feeder$Group.2
# trait.f<-traits[rownames(traits) %in% rownames(feeder),]
# 
# #same order?
# trait.fs<-trait.f[sort(rownames(feeder)),]
# plot(rda(feeder[,-c(1,2)],trait.fs))


##############
ggplot(wss,aes(x=PC2,y=weighted.selectivity)) + geom_point() + geom_smooth(method="glm",family="binomial") + geom_text(aes(label=Species),size=2)
ggplot(wss,aes(x=PC1,y=weighted.selectivity)) + geom_point() + geom_smooth(method="glm",family="binomial") + geom_text(aes(label=Species),size=2)

p<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=PC1,y=Selectivity,size=Minutes_Total,label=Species)) + geom_point() + stat_smooth(method="glm",link="binomial",aes(weight=Minutes_Total))
p

p<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=PC2,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",link="binomial",aes(weight=Minutes_Total,group=1))
p

p<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=Mass,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",link="binomial",aes(weight=Minutes_Total,group=1))

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
  if(sum(rangeLim$Species %in% x[["Species"]])==0) {
    r<-NA

    return(data.frame(UP=NA, RDist=r))
  }
  
  maxR<-abs(as.numeric(x["Elevation"]) - as.numeric(rangeLim[rangeLim$Species %in% x["Species"],]$Max))
  minR<-abs(as.numeric(x["Elevation"]) - as.numeric(rangeLim[rangeLim$Species %in% x["Species"],]$Min))
  
  
  if(maxR < minR) {return(data.frame(UP="Dist_Upper",RDist=maxR))}
  if(minR < maxR) {return(data.frame(UP="Dist_Lower",RDist=minR))}
  if(minR == maxR){data.frame(UP=NA, RDist=NA)}
})

selective.matrix<-data.frame(selective.matrix,rbind.fill(distU))

selective.matrix$UP
#Selectivity and distance to range edge.
rangeplot<-ggplot(selective.matrix[selective.matrix$Species %in% keep & !is.na(selective.matrix$UP),],aes(RDist,Selectivity,col=Species,size=Minutes_Total,shape=UP)) + geom_point() 
rangeplot<-rangeplot + stat_smooth(aes(weight=Minutes_Total),method="glm",family="binomial",se=FALSE)
rangeplot + facet_wrap(~UP)

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

massplot<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=MassD,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(group=1))
massplot + facet_wrap(~Species,scale="free_x") 

############
#Selectivity and moving window of mass difference
############


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

massplot<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=MassD,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1),legend=FALSE)
massplot + facet_wrap(~Species,scales="free_x",nrow=2) + theme_bw()
ggsave(paste(gitpath,"Figures/Mass_Selectivity.jpeg",sep=""),height=8,width=12,units="in",dpi=300)


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

PC1D<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=PC1D,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1))
PC1D + facet_wrap(~Species,scales="free_x")


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

PC2D<-ggplot(selective.matrix[selective.matrix$Species %in% keep,],aes(x=PC2D,y=Selectivity,size=Minutes_Total,label=Species,col=Species)) + geom_point() + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=1))
PC2D + facet_wrap(~Species,scales="free_x")

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

MultD<-ggplot(data=selective.matrix[selective.matrix$Species %in% keep,],aes(x=MultD,y=Selectivity,size=Minutes_Total,col=Species)) + stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total)) + geom_point()
MultD + facet_wrap(~Species,scales="free_x")
#####################################################
#Selectivity and Available Resource
#####################################################

#just summer data
fltransects<-droplevels(fltransects[fltransects$month %in% c(6,7,8),])

fltransects$GPS_ID<-as.numeric(as.character(fltransects$GPS_ID))

#just get videos from the summer months
vid<-vid[vid$Month %in% c(6,7,8),]

#match with spatial infodim
gps<-readShapePoints(paste(droppath,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp",sep=""))@data

gps<-droplevels(gps[gps$MonthID %in% c(6,7,8),])

gps$GPS_ID<-as.numeric(as.character(gps$GPS_ID))

#GPSID + month
gps$mergeID<-paste(gps$GPS_ID,gps$MonthID,sep="_")

#remove duplicated
gps_d<-gps[duplicated(gps,fromLast=TRUE),]

fltransects$mergeID<-paste(fltransects$GPS_ID,fltransects$month,sep="_")

table(fltransects$mergeID %in% gps$mergeID)
table(fltransects$GPS_ID %in% gps$GPS_ID)

fltransects<-merge(fltransects,gps_d,by="mergeID",all.x=TRUE)

#where elevation failed, take the midpoint of the transect
fltransects[is.na(fltransects$ele),"ele"]<-apply(fltransects[is.na(fltransects$ele),c("Elevation.Begin","Elevation.End")],1,mean)

#Split videos into species lists
vid.s<-split(vid,list(vid$Hummingbird))

cl<-makeCluster(4,"SOCK")
registerDoSNOW(cl)
selective.matrix[,c("fl_all", "fl_s")]<-foreach(g=1:nrow(selective.matrix),.combine=rbind) %do%{
  
  #grab row
  x<-selective.matrix[g,]
  
  #Get the hummingbird index
  flIndex<-vid.s[[as.character(x$Species)]]
  
  #Which species does the bird feed on
  fl.sp<-flIndex$Iplant_Double
  
  #dates within two weeks either side. 
  dateL<-abs(difftime(as.POSIXlt(x[["Date"]],format="%m/%d/%Y"),  as.POSIXlt(fltransects$Date,format="%m/%d/%Y"),units="weeks")) <= 2.5
  
  #and within 200m of the elevation
  flower.month<-fltransects[dateL & (fltransects$ele > x[["Elevation"]]-100 &fltransects$ele < x[["Elevation"]]+ 100) ,]
  
  if(nrow(flower.month)==0) return(NA)
  
  mean.fl_all<-mean(aggregate(flower.month$Total_Flowers,list(flower.month$Transect.ID),sum,na.rm=TRUE)$x)
    
  flower_sp<-flower.month[flower.month$Iplant_Double %in% fl.sp,]
  
  if(nrow(flower_sp)==0) return(NA)
  
  mean.fl<-mean(aggregate(flower_sp$Total_Flowers,list(flower_sp$Transect.ID),sum,na.rm=TRUE)$x)
  
  return(c(mean.fl_all,mean.fl))}
stopCluster(cl)

flowerplot<-melt(id.var=c("Species","Selectivity","Elevation","Minutes_Total","bph"),measure.vars=c("fl_s","fl_all"),data=selective.matrix)

resourceplotS<-ggplot(flowerplot[flowerplot$Species %in% keep,],aes(x=value,linetype=variable,y=Selectivity,col=variable)) + geom_point() +  stat_smooth(method="glm",family="binomial",aes(weight=Minutes_Total,group=variable),se=FALSE)
resourceplotS+ facet_wrap(~Species,scales="free_x") + labs(x="Available Resources") + theme_bw()

ggsave(paste(gitpath,"Figures/Selectivity_Resources.jpeg",sep=""),height=7,width=11,units="in",dpi=300)

resourceplot<-ggplot(flowerplot[flowerplot$Species %in% keep,],aes(x=value,y=bph,col=variable,linetype=variable)) + geom_point() + stat_smooth(method="lm",aes(group=variable),size=1,se=FALSE)
resourceplot + facet_wrap(~Species,scales="free",nrow=2) + xlab("Available Resources (# of Flowers)") + ylab("Visits/hour") + labs("Elevation") + theme_bw() + scale_color_discrete(name="Available Resources",labels=c("Species Only","All")) + scale_linetype_discrete(guide=FALSE)
ggsave(paste(gitpath,"Figures/BPH_Resources.jpeg",sep=""),height=5,width=17,units="in",dpi=300)

##############Chisqaure anova#################

selective.matrix$MassD[!is.finite(selective.matrix$MassD)]<-0
selective.matrix$MultD[!is.finite(selective.matrix$MultD)]<-0

mod_data<-selective.matrix[selective.matrix$Species %in% keep & complete.cases(selective.matrix[,c("Species","Mass","MassD","Elevation","RDist","MultD","fl_all","fl_s")]), ]

missing<-selective.matrix[!complete.cases(selective.matrix[,c("Species","Mass","MassD","Elevation","MultD","fl_all","fl_s")]), ]

##Compute model fits

#Intercept only
m_intercept<-glm(data=mod_data,Selectivity~1,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

#By Species
m_species<-glm(data=mod_data,Selectivity~Species,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

#By Mass
m_mass<-glm(data=mod_data,Selectivity~Mass:Species,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

#By Difference in Mass
m_massD<-glm(data=mod_data,Selectivity~MassD:Species,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

#By multivariate distance
m_multD<-glm(data=mod_data,Selectivity~MultD:Species,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

#By Distance to range edge
m_Elevation<-glm(data=mod_data,Selectivity~RDist:Species,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

#By available resources, all
m_Allresources<-glm(data=mod_data,Selectivity~fl_all:Species,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

#By available resources, by species
m_Speciesresources<-glm(data=mod_data,Selectivity~fl_s:Species,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

m_MorphResources<-glm(data=mod_data,Selectivity~fl_s:Species + Mass:Species,family="binomial",weights=mod_data$Minutes_Total,na.action="na.fail")

#rsquared for best model


1-m_Allresources$deviance/m_Allresources$null.deviance

mods<-list(m_intercept,m_species,m_mass,m_massD,m_multD,m_Elevation,m_Allresources,m_Speciesresources)



#Significance of terms
lapply(mods,anova,test="Chisq")

anova(m_intercept,m_Speciesresources,m_Allresources,test=c("Chisq"))

at<-anova(m_intercept,m_Allresources,m_Speciesresources,test=c("Chisq"))
at<-anova(m_intercept,m_species,m_mass,m_massD,m_multD,m_Allresources,m_Speciesresources,test=c("Chisq"))

at<-anova(m_Allresources,m_Speciesresources,test=c("Chisq"))

summary(at)
at

#######AIC##################
#Just use species for which we have complete info to compare model fits


missing<-selective.matrix[!selective.matrix$Species %in% keep & complete.cases(selective.matrix[,c("Species","Mass","MassD","Elevation","MultD","fl_all")]), ]

nrow(selective.matrix)

nrow(mod_data)

aictable<-AIC(m_intercept,m_species,m_mass,m_massD,m_multD,m_Allresources,m_Speciesresources,m_MorphResources)
aictable[order(aictable$AIC),]

capture.output(aictable,file="Figures/AIC.xls")


#write table to file
capture.output(at,file="Figures/Anova.xls")

#############################
#Multimodel inference
#############################
require(MuMIn)

modelA<-model.avg(m_intercept,m_MorphResources,m_species,m_mass,m_Elevation,m_massD,m_multD,m_Allresources,m_Speciesresources)

#Get psuedoR2
mods<-list(m_intercept,m_MorphResources,m_species,m_mass,m_Elevation,m_massD,m_multD,m_Allresources,m_Speciesresources)
R2<-lapply(mods,function(x){
  1-x$deviance/x$null.deviance
})

sumAIC<-round(modelA$summary,2)

###############Needs to be hand ordered if rerun, no match yet.
sumAIC$PsuedoR2<-round(unlist(R2),2)[c(2,9,5,7,3,4,8,6,1)]

sumAIC
capture.output(sumAIC,file="Figures/AIC.xls")

anova(m_Speciesresources,m_MorphResources,test="Chisq")

save.image("Selectivity.RData")