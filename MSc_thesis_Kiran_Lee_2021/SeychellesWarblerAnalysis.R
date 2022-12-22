### territory quality timeline
setwd("~/Desktop/Seychelles Warblers/Data")


## load stuff- so i actually do the manual way where i go to "Files", click on the file and select import dataset
YearlyTQData <- read_excel("YearlyTQ.xlsx")
TQ <- read_excel("TQ.xlsx")
TQ <- read_excel("BirdIDSexYear.xlsx")
TQ <- read_excel("FullestSexRatioData.xlsx")
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyverse)
library(ggpubr)
library(lme4)
library(readxl)
str(YearlyTQData)
str(Data)
str(TQ)

BirdIDSexYear<- read_excel("BirdIDSexYear.xlsx")
FullestSexRatioData <- read_excel("FullestSexRatioData.xlsx")


## subset data removing weird stuff
Data <- subset(FullestSexRatioData, !is.na(Year))
Data <- subset(Data, !is.na(Sex))
Data <- subset(Data, !is.na(TerritoryQuality))
Data <- subset(Data, pParentage>0.5) #choose chicks who's parentage is known with higher than half probability
Data$MumID[Data$MumID == "-998"] <- NA

#what is the mean and se for parentage assignment?
mean(Data$pParentage)
sd(Data$pParentage, na.rm=TRUE) /  
  sqrt(length(Data$pParentage[!is.na(Data$pParentage)]))

#maybe don't subset this
Data<-subset(Data, !is.na(MumID) & !is.na(BreedGroupID))
Data$BreedGroupID<- as.factor(Data$BreedGroupID)

#create gelman transformed territory quality column


Data$GelmanTerritoryQuality<-(log(Data$TerritoryQuality)-(mean(log(Data$TerritoryQuality))))/(2*sd(log(Data$TerritoryQuality)))

TData$GelmanTerritoryQuality<-(log(TData$TerritoryQuality)-(mean(log(TData$TerritoryQuality))))/(2*sd(log(TData$TerritoryQuality)))

TData$GelmanTerritoryQuality<-(TData$MeanTQ-(mean(Data$TerritoryQuality)))/(2*sd(Data$TerritoryQuality))

#potentially dont use
fn<-function(x) ((log(x))-mean(log(x))/2*sd(log(x)))

##create dataframe with summary stats for each year on numbers daughters and males and territory quality 
DataGroupStat<-ddply(Data, .(Year),summarise,
                   MeanTQ= mean(TerritoryQuality),
                   RangeTQ=max(TerritoryQuality)-min(TerritoryQuality),
                   sdTQ= sd(TerritoryQuality),
                   OffspringSexRatio= mean(Sex), # mean offspring sex ratio had in that year
                   cvTQ= sdTQ/MeanTQ,
                   nDaughters= length(Sex[Sex=="0"]), # number daughters had in that year
                   nSons= length(Sex[Sex=="1"])) # number sons had in that year

#check this works by creating scatter plot of sex ratio over territory quality
ggplot(data=DataGroupStat, aes(x=MeanTQ, y=(nSons/(nSons+nDaughters))))+
  geom_point()+
  geom_text(aes(label=Year),hjust=0, vjust=0) 




### load territory quality dataset (using territory quality information not just found in the chick pedigree sex data)
# subset tq data
TQ<-rename(TQ, c("PeriodYear"="Year"))
TQ<-rename(TQ, c("TQcorrected"="TerritoryQuality"))
TQ<-subset(TQ,  !is.na(TerritoryQuality))
TQ$logTerritoryQuality<-log(TQ$TerritoryQuality)

TQ$GelmanTerritoryQuality<-(TQ$logTerritoryQuality)-(mean(TQ$logTerritoryQuality))/(2*sd(TQ$logTerritoryQuality))

Data$GelmanTerritoryQuality<-(log(Data$TerritoryQuality)-(mean(log(Data$TerritoryQuality))))/(2*sd(log(Data$TerritoryQuality)))

#how has territory quality changed over time? between years 1990-1999, territory quality was  better and more varied within a year.
ggplot(data=TQ, aes(x=factor(Year), y=TerritoryQuality))+
  geom_boxplot()

#subset and create dataframe for stable territories
TQStableTerritories<-TQ[TQ$TerritoryID %in% names(which(table(TQ$TerritoryID) > 2)), ]

#create dataframe with summary stats for each year on territory quality
TQgroupstata<-ddply(TQ, .(Year),summarise,
        MeanTQ= mean(TerritoryQuality),
        RangeTQ=max(TerritoryQuality)-min(TerritoryQuality),
        sdTQ= sd(TerritoryQuality),
        cvTQ= sdTQ/MeanTQ)

#for each territory, what is the spread of its quality?
ggplot(data=subset(TQStableTerritories, TerritoryQuality<700000), aes(x=reorder(as.factor(TerritoryID),TerritoryQuality,na.rm = TRUE), y=TerritoryQuality))+
       geom_boxplot()+
  scale_x_discrete(breaks=unique(TQ$TerritoryID))

#histogram to see spread of gelman territory quality. looks good
ggplot(data=Data, aes(x=GelmanTerritoryQuality))+
  geom_histogram()

#fix this: why isnt gelman territory quality working in TQ?
hist(TQ$GelmanTerritoryQuality)

ggplot(data=TQ, aes(x=GelmanTerritoryQuality))+
  geom_histogram(position="identity",alpha=0.1, bins=0.1)+
  geom_density()

#Can probably remove: create dataframe of population sex ratio for each year (using sex information of entire population not just found in the chick pedigree sex data)
YearlySRgroupstat<-ddply(BirdIDSexYear, .(Year),summarise,
                         PopulationSexRatio= length(Sex[Sex=="1"])/(length(Sex[Sex=="1"])+length(Sex[Sex=="0"])),
                         nFem= length(Sex[Sex=="0"]),
                         nMale= length(Sex[Sex=="1"])
)


#create ultimate yearly dataframe merging YearlySRgroupstat (yearly sex ratio data) with DataGroupStat (yearly territory quality data) with slope data
#create slope column with values produced from models below
TQgroupstata<-merge(YearlySRgroupstat, TQgroupstata, by="Year")
TQgroupstata<-merge(x = TQgroupstata , y = DataGroupStat[ , c("Year", "OffspringSexRatio")], by = "Year", all.x=TRUE)
TQgroupstata<-merge(x = TQgroupstata , y = DataGroupStat[ , c("Year", "nDaughters")], by = "Year", all.x=TRUE)
TQgroupstata<-merge(x = TQgroupstata , y = DataGroupStat[ , c("Year", "nSons")], by = "Year", all.x=TRUE)
TQgroupstata <- subset(TQgroupstata, Year!= 1990 & Year!= 2019)


#create dataframe with summary stats per territory
TerritoryIDGroupStat<-ddply(Data, .(TerritoryID),summarise,
                   MeanTQbetweenyears= mean(TerritoryQuality),
                   RangeTQbetweenyears=max(TerritoryQuality)-min(TerritoryQuality),
                   sdTQbetweenyears= sd(TerritoryQuality),
                   SexRatiobetweenyears= mean(Sex),
                   cvTQbetweenyears= sdTQbetweenyears/MeanTQbetweenyears)


#add cvTQbetweenyears column
Data<-merge(x = Data , y = TerritoryIDGroupStat[ , c("TerritoryID", "cvTQbetweenyears")], by = "TerritoryID", all.x=TRUE)

#add column of how variable a territory's quality is
TData<-merge(x = TData , y = TerritoryIDGroupStat[ , c("TerritoryID", "cvTQbetweenyears")], by = "TerritoryID", all.x=TRUE)


#add column of how variable territory qualities are within a year
TData<-merge(x = TData , y = TQgroupstata[ , c("Year", "cvTQ")], by = "Year", all.x=TRUE)

TData$BreedGroupID<- as.factor(TData$BreedGroupID)


#what is the ratio of helpers male and female? 0.25 female
BirdIDSexYear<-BirdIDSexYear %>% filter(!is.na(Sex))
Statusgroupstat<-ddply(BirdIDSexYear, .(Status),summarise,
                       SexRatio= mean(Sex)
                       
) 

##investigating whether mothers in variable territories change their offspring sex ratio according to territory quality

##merge fieldperiod start and end dates into Data
#first create dataframe with summary stats per fieldperiod
FieldPeriodIDGroupStat<-ddply(BirdIDSexYear, .(FieldPeriodID, PeriodEnd, PeriodStart),summarise,
                              
                              nFem= length(Sex[Sex=="0"]),
                              nMale= length(Sex[Sex=="1"]))
FieldPeriodIDGroupStat$PeriodLength<-FieldPeriodIDGroupStat$PeriodEnd-FieldPeriodIDGroupStat$PeriodStart

Data<-merge(x = Data , y = FieldPeriodIDGroupStat [ , c("FieldPeriodID", "PeriodStart")], by = "FieldPeriodID", all.x=TRUE)
Data<-merge(x = Data , y = FieldPeriodIDGroupStat [ , c("FieldPeriodID", "PeriodEnd")], by = "FieldPeriodID", all.x=TRUE)
Data<-merge(x = Data , y = FieldPeriodIDGroupStat [ , c("FieldPeriodID", "PeriodLength")], by = "FieldPeriodID", all.x=TRUE)

TData<-merge(x = TData , y = FieldPeriodIDGroupStat [ , c("FieldPeriodID", "PeriodLength")], by = "FieldPeriodID", all.x=TRUE)

#what is the distribution in breed group size?
BreedGroupIDGroupStat<-ddply(Data, .(BreedGroupID, TerritoryID), summarise,
                             Offspring=mean(nOffspringBreedGroup),
                             Adults=mean(nAdults))
BreedGroupIDGroupStat$GroupSize<-BreedGroupIDGroupStat$Offspring+BreedGroupIDGroupStat$Adults
hist(BreedGroupIDGroupStat$GroupSize)
mean(BreedGroupIDGroupStat$GroupSize)

##investigating whether females indeed become helpers on the same territory the next year
#create the following columns in Data: "YearLater","Status after YearLater","Territory after YearLater"
Data$YearLater<-Data$Year+1
Data$TwoYearsLater<-Data$Year+2
Data$ThreeYearsLater<-Data$Year+3

#merge status column in BirdIDSexYear if the Year matches YearLater and if BirdID matches ChickID
TrialMergeData = merge(Data, BirdIDSexYear, by.x=c("YearLater", "ChickID"), by.y=c("Year", "BirdID"))
TrialMergeData2 = merge(Data, BirdIDSexYear, by.x=c("TwoYearsLater", "ChickID"), by.y=c("Year","BirdID"))
TrialMergeData3 = merge(Data, BirdIDSexYear, by.x=c("ThreeYearsLater", "ChickID"), by.y=c("Year","BirdID"))

MumClutchSize<-data.frame(Mumgroupstat$MumID, Mumgroupstat$BreedGroupID, Mumgroupstat$ClutchSize, Mumgroupstat$Year)
names(MumClutchSize) <- gsub(x = names(MumClutchSize),
                               pattern = "\\Mumgroupstat.",
                               replacement = "")
TrialMergeDataKnownMums$MumID<-as.factor(TrialMergeDataKnownMums$MumID)
TrialMergeDataKnownMums<-TrialMergeDataKnownMums %>% filter(!is.na(MumID))

TrialMergeDataKnownMums<-subset(TrialMergeDataKnownMums, !is.na(MumID))
View(TrialData)
TrialData=merge(Data, Mumgroupstat, by.x=c("MumID", "Year"), by.y=c("MumID", "Year")) #dont need this potentially?





#add in clutch size data in which the chick was born. HERE I NEED TO DISCOUNT CHICKS WITH NA MUMS BECAUSE THERE ARE MANY NA MUMS
TrialMergeData$MumID<-as.factor(TrialMergeData$MumID)
TrialMergeData2$MumID<-as.factor(TrialMergeData2$MumID)
TrialMergeData3$MumID<-as.factor(TrialMergeData3$MumID)
TrialMergeData$BreedGroupID<-TrialMergeData$BreedGroupID.x
TrialMergeData$BreedGroupID<-as.factor(TrialMergeData$BreedGroupID)
TrialMergeData2$BreedGroupID<-TrialMergeData2$BreedGroupID.x
TrialMergeData2$BreedGroupID<-as.factor(TrialMergeData2$BreedGroupID)
TrialMergeData3$BreedGroupID<-TrialMergeData3$BreedGroupID.x
TrialMergeData3$BreedGroupID<-as.factor(TrialMergeData3$BreedGroupID)
TrialMergeData$Year<-as.factor(TrialMergeData$Year)
TrialMergeData2$Year<-as.factor(TrialMergeData2$Year)
TrialMergeData3$Year<-as.factor(TrialMergeData3$Year)
Mumgroupstat$MumID<-as.factor(Mumgroupstat$MumID)
Mumgroupstat$BreedGroupID<-as.factor(Mumgroupstat$BreedGroupID)

TrialMergeData<-left_join(TrialMergeData, Mumgroupstat, by=c('MumID'='MumID', 'Year'='Year', 'BreedGroupID'='BreedGroupID'))
TrialMergeData2<-left_join(TrialMergeData2, Mumgroupstat, by=c('MumID'='MumID', 'Year'='Year', 'BreedGroupID'='BreedGroupID'))
TrialMergeData3<-left_join(TrialMergeData3, Mumgroupstat, by=c('MumID'='MumID', 'Year'='Year', 'BreedGroupID'='BreedGroupID'))



#
TrialMergeData$MumID[TrialMergeData$MumID == "NA"] <- NA
TrialMergeData$DadID[TrialMergeData$DadID == -998] <- NA
TrialMergeData<-subset(TrialMergeData, !is.na(MumID))
TrialMergeData$RankedStatus<-recode(TrialMergeData$Status, "CH"=1,"BrF" = 1, "BrM" = 1, "H"=2, "OFL" = 3, "FL" = 4, "ABX" = 5, "AB" = 6, "FLOAT" = 7, "SEEN2" = 8, "SEEN1" = 9, "TBRF" = 10, "TBRM" = 10, "U" = 11, "SBR" = 12, "B"=13,"NSA"=14, "NS"=15)

#select chicks whose rank the next year is "highest" if the same chick occurs twice
TrialMergeData<-TrialMergeData %>%
  group_by(ChickID, BreedGroupID, FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, nAdults.x) %>%
  slice(which.min(RankedStatus))



#
TrialMergeData2$MumID[TrialMergeData2$MumID == "NA"] <- NA
TrialMergeData2$DadID[TrialMergeData2$DadID == -998] <- NA
TrialMergeData2<-subset(TrialMergeData2, !is.na(MumID))
TrialMergeData2$RankedStatus<-recode(TrialMergeData2$Status, "CH"=1,"BrF" = 1, "BrM" = 1, "H"=2, "OFL" = 3, "FL" = 4, "ABX" = 5, "AB" = 6, "FLOAT" = 7, "SEEN2" = 8, "SEEN1" = 9, "TBRF" = 10, "TBRM" = 10, "U" = 11, "SBR" = 12, "B"=13,"NSA"=14, "NS"=15)

#select chicks whose rank the next year is "highest" if the same chick occurs twice
TrialMergeData2<-TrialMergeData2 %>%
  group_by(ChickID, BreedGroupID, FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, nAdults.x) %>%
  slice(which.min(RankedStatus))


#
TrialMergeData3$MumID[TrialMergeData3$MumID == "NA"] <- NA
TrialMergeData3$DadID[TrialMergeData3$DadID == -998] <- NA
TrialMergeData3<-subset(TrialMergeData3, !is.na(MumID))
TrialMergeData3$RankedStatus<-recode(TrialMergeData3$Status, "CH"=1,"BrF" = 1, "BrM" = 1, "H"=2, "OFL" = 3, "FL" = 4, "ABX" = 5, "AB" = 6, "FLOAT" = 7, "SEEN2" = 8, "SEEN1" = 9, "TBRF" = 10, "TBRM" = 10, "U" = 11, "SBR" = 12, "B"=13,"NSA"=14, "NS"=15)

#select chicks whose rank the next year is "highest" if the same chick occurs twice
TrialMergeData3<-TrialMergeData3 %>%
  group_by(ChickID, BreedGroupID, FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, nAdults.x) %>%
  slice(which.min(RankedStatus))


#merge dataframes for all 3 years into one


#what is the fate of female and male chicks 1 year later? no observable difference between sons and daughters
table(TrialMergeData$Status, TrialMergeData$Sex.x, TrialMergeData$DispersalStatus)
table(TrialMergeData2$Status, TrialMergeData2$Sex.x, TrialMergeData2$DispersalStatus) 
table(TrialMergeData3$Status, TrialMergeData3$Sex.x, TrialMergeData3$DispersalStatus) 

HelpersSex<-as.data.frame(table(BirdIDSexYear$Status, BirdIDSexYear$Sex))
HelpersSex
ggplot(data=subset(BirdIDSexYear,Status=="H"), aes(x=as.factor(Sex)))+
  geom_bar(aes(fill=as.factor(Sex)))

MumStatus$StatusBrF<-revalue(MumStatus$Status, c(ABX="NonBrF",H="NonBrF",AB="NonBrF",NSA="NonBrF",SEEN1="NonBrF",SEEN2="NonBrF",NS="NonBrF",OFL="NonBrF",SBR="NonBrF",TBRF="NonBrF"))
table(MumStatus$StatusBrF)

ggplot(data=subset(MumStatus), aes(x=as.factor(Status)))+
  geom_bar(aes(fill=as.factor(Sex)))

ggplot(data=subset(MumStatus), aes(x=as.factor(StatusBrF)))+
  geom_bar(aes(fill=as.factor(Sex)))

#add a column for whether they remain on territory or not

TrialMergeData$DispersalStatus<-if_else( TrialMergeData$TerritoryID.x==TrialMergeData$TerritoryID.y, 0, 1)
TrialMergeData2$DispersalStatus<-if_else( TrialMergeData2$TerritoryID.x==TrialMergeData2$TerritoryID.y, 0, 1)
TrialMergeData3$DispersalStatus<-if_else( TrialMergeData3$TerritoryID.x==TrialMergeData3$TerritoryID.y, 0, 1)


#plot status of chicks subsequenct years as bar graphs REALLY MESSY SHOULD REVIEW
#create YearsLater column
TrialMergeData$YearsLater<-(TrialMergeData$YearLater/TrialMergeData$YearLater)
TrialMergeData2$YearsLater<-(TrialMergeData2$YearLater/TrialMergeData2$YearLater)+1
TrialMergeData3$YearsLater<-(TrialMergeData3$YearLater/TrialMergeData3$YearLater)+2
TrialMergeData$DispersalStatusOneYear<-TrialMergeData$DispersalStatus
TrialMergeData2$DispersalStatusTwoYears<-TrialMergeData2$DispersalStatus
TrialMergeData3$DispersalStatusThreeYears<-TrialMergeData3$DispersalStatus
TrialMergeData$BreedingStatusOneYear<-TrialMergeData$Status
TrialMergeData2$BreedingStatusTwoYears<-TrialMergeData2$Status
TrialMergeData3$BreedingStatusThreeYears<-TrialMergeData3$Status

TrialMergeData$GTQx<-TrialMergeData$GelmanTerritoryQuality.x
TrialMergeData$GTQy<-TrialMergeData$GelmanTerritoryQuality.y

TrialMergeData$TerritoryQualityDifference<-(1.01*(TrialMergeData$GTQy)-((TrialMergeData$GTQx)))
TrialMergeData$TerritoryQualityDifference<-TrialMergeData$TerritoryQualityDifference-(TrialMergeData$GelmanTerritoryQuality.x)

TrialMergeData2$TerritoryQualityDifference<-TrialMergeData2$GelmanTerritoryQuality.y-TrialMergeData2$GelmanTerritoryQuality.x
TrialMergeData3$TerritoryQualityDifference<-TrialMergeData3$GelmanTerritoryQuality.y-TrialMergeData3$GelmanTerritoryQuality.x

ZeroYearLater<-TrialData
ZeroYearLater$BreedingStatus<-"CH"
ZeroYearLater$DispersalStatus<-0
ZeroYearLater$YearsLater<-0
ZeroYearLater<-data.frame(ZeroYearLater$ChickID,ZeroYearLater$MumID,ZeroYearLater$Sex, ZeroYearLater$Status, ZeroYearLater$DispersalStatus,ZeroYearLater$YearsLater, ZeroYearLater$nOffspringBreedGroup,ZeroYearLater$ClutchSize, ZeroYearLater$nAdults.x,ZeroYearLater$BreedGroupID)
OneYearLater<-data.frame(TrialMergeData$ChickID,TrialMergeData$MumID,TrialMergeData$DadID, TrialMergeData$Sex.x, TrialMergeData$Status,TrialMergeData$DispersalStatus, TrialMergeData$YearsLater, TrialMergeData$nOffspringBreedGroup, TrialMergeData$ClutchSize,TrialMergeData$nAllAB.x,TrialMergeData$nAdults.x,TrialMergeData$BreedGroupID,TrialMergeData$TerritoryID.x,TrialMergeData$GelmanTerritoryQuality.x,TrialMergeData$TerritoryID.y,TrialMergeData$FieldPeriodID.y,TrialMergeData$FieldPeriodID.x,TrialMergeData$YearLater)
TwoYearLater<-data.frame(TrialMergeData2$ChickID,TrialMergeData2$MumID,TrialMergeData2$DadID, TrialMergeData2$Sex.x, TrialMergeData2$Status,TrialMergeData2$DispersalStatus, TrialMergeData2$YearsLater, TrialMergeData2$nOffspringBreedGroup, TrialMergeData2$ClutchSize, TrialMergeData2$nAllAB.x,TrialMergeData2$nAdults.x,TrialMergeData2$BreedGroupID,TrialMergeData2$TerritoryID.x,TrialMergeData2$GelmanTerritoryQuality.x,TrialMergeData2$TerritoryID.y,TrialMergeData2$FieldPeriodID.y,TrialMergeData2$FieldPeriodID.x,TrialMergeData2$YearLater)
ThreeYearLater<-data.frame(TrialMergeData3$ChickID,TrialMergeData3$MumID,TrialMergeData3$DadID, TrialMergeData3$Sex.x, TrialMergeData3$Status,TrialMergeData3$DispersalStatus, TrialMergeData3$YearsLater, TrialMergeData3$nOffspringBreedGroup, TrialMergeData3$ClutchSize,TrialMergeData3$nAllAB.x, TrialMergeData3$nAdults.x,TrialMergeData3$BreedGroupID,TrialMergeData3$TerritoryID.x,TrialMergeData3$GelmanTerritoryQuality.x,TrialMergeData3$TerritoryID.y,TrialMergeData3$FieldPeriodID.y,TrialMergeData3$FieldPeriodID.x,TrialMergeData3$YearLater)

nrow(TrialMergeData$ChickID)

names(ZeroYearLater) <- gsub(x = names(ZeroYearLater),
                            pattern = "\\.",
                            replacement = " ")
names(OneYearLater) <- gsub(x = names(OneYearLater),
                            pattern = "\\.",
                            replacement = " ")
names(TwoYearLater) <- gsub(x = names(TwoYearLater),
                            pattern = "\\.",
                            replacement = " ")
names(ThreeYearLater) <- gsub(x = names(ThreeYearLater),
                            pattern = "\\.",
                            replacement = " ")

ZeroYearLater<-rename(ZeroYearLater, c("ZeroYearLater ChickID"="ChickID",
                                       "ZeroYearLater MumID"="MumID",
                                       "ZeroYearLater Sex"="Sex",
                                     "ZeroYearLater BreedingStatus"="BreedingStatus",
                                     "ZeroYearLater DispersalStatus"="DispersalStatus",
                                     "ZeroYearLater YearsLater"="YearsLater",
                                     "ZeroYearLater nOffspringBreedGroup"="nOffspringBreedGroup",
                                     "ZeroYearLater ClutchSize"="ClutchSize",
                                     "ZeroYearLater nAdults x"="nAdults"))
OneYearLater<-rename(OneYearLater, c("TrialMergeData ChickID"="ChickID",
                                     "TrialMergeData MumID"="MumID",
                                     "TrialMergeData DadID"="DadID",
                                     "TrialMergeData Sex x"="Sex",
                     "TrialMergeData Status"="BreedingStatus",
                     "TrialMergeData DispersalStatus"="DispersalStatus",
                     "TrialMergeData YearsLater"="YearsLater",
                     "TrialMergeData nOffspringBreedGroup"="nOffspringBreedGroup",
                     "TrialMergeData ClutchSize"="ClutchSize",
                     "TrialMergeData nAllAB x"="nAllABx",
                     "TrialMergeData nAdults x"="nAdults",
                     "TrialMergeData BreedGroupID"="BreedGroupID",
                     "TrialMergeData TerritoryID x"="TerritoryIDx",
                     "TrialMergeData GelmanTerritoryQuality x"="GelmanTerritoryQuality",
                     "TrialMergeData TerritoryID y"="TerritoryIDy",
                     "TrialMergeData FieldPeriodID y"="FieldPeriody",
                     "TrialMergeData FieldPeriodID x"="FieldPeriodx",
                     "TrialMergeData YearLater"="YearLater"))
TwoYearLater<-rename(TwoYearLater, c("TrialMergeData2 ChickID"="ChickID",
                                     "TrialMergeData2 MumID"="MumID",
                                     "TrialMergeData2 DadID"="DadID",
                                     "TrialMergeData2 Sex x"="Sex",
                                     "TrialMergeData2 Status"="BreedingStatus",
                                     "TrialMergeData2 DispersalStatus"="DispersalStatus",
                                     "TrialMergeData2 YearsLater"="YearsLater",
                                     "TrialMergeData2 nOffspringBreedGroup"="nOffspringBreedGroup",
                                     "TrialMergeData2 ClutchSize"="ClutchSize",
                                     "TrialMergeData2 BreedGroupID"="BreedGroupID",
                                     "TrialMergeData2 TerritoryID x"="TerritoryIDx",
                                     "TrialMergeData2 GelmanTerritoryQuality x"="GelmanTerritoryQuality",
                                     "TrialMergeData2 TerritoryID y"="TerritoryIDy",
                                     "TrialMergeData2 FieldPeriodID y"="FieldPeriody",
                                     "TrialMergeData2 FieldPeriodID x"="FieldPeriodx",
                                     "TrialMergeData2 nAllAB x"="nAllABx",
                                     "TrialMergeData2 nAdults x"="nAdults",
                                     "TrialMergeData2 YearLater"="YearLater"))
ThreeYearLater<-rename(ThreeYearLater, c("TrialMergeData3 ChickID"="ChickID",
                                         "TrialMergeData3 MumID"="MumID",
                                         "TrialMergeData3 DadID"="DadID",
                                     "TrialMergeData3 Sex x"="Sex",
                                     "TrialMergeData3 Status"="BreedingStatus",
                                     "TrialMergeData3 DispersalStatus"="DispersalStatus",
                                     "TrialMergeData3 YearsLater"="YearsLater",
                                     "TrialMergeData3 nOffspringBreedGroup"="nOffspringBreedGroup",
                                     "TrialMergeData3 ClutchSize"="ClutchSize",
                                     "TrialMergeData3 BreedGroupID"="BreedGroupID",
                                     "TrialMergeData3 TerritoryID x"="TerritoryIDx",
                                     "TrialMergeData3 GelmanTerritoryQuality x"="GelmanTerritoryQuality",
                                     "TrialMergeData3 TerritoryID y"="TerritoryIDy",
                                     "TrialMergeData3 FieldPeriodID y"="FieldPeriody",
                                     "TrialMergeData3 FieldPeriodID x"="FieldPeriodx",
                                     "TrialMergeData3 nAllAB x"="nAllABx",
                                     "TrialMergeData3 nAdults x"="nAdults",
                                     "TrialMergeData3 YearLater"="YearLater"))


              
AllYearLater<-bind_rows(ZeroYearLater,OneYearLater, TwoYearLater, ThreeYearLater)
AllYearLater<-bind_rows(OneYearLater, TwoYearLater, ThreeYearLater)

AllYearLater$RankedStatus<-AllYearLater$BreedingStatus
AllYearLater$RankedStatus<-recode(AllYearLater$RankedStatus, "CH"=1,"BrF" = 1, "BrM" = 1, "H"=2, "OFL" = 3, "FL" = 4, "ABX" = 5, "AB" = 6, "FLOAT" = 7, "SEEN2" = 8, "SEEN1" = 9, "TBRF" = 10, "TBRM" = 10, "U" = 11, "SBR" = 12, "B"=13,"NSA"=14, "NS"=15)
AllYearLater$BreedGroupSize<-AllYearLater$nAdults+AllYearLater$nOffspringBreedGroup
AllYearLater<-subset(AllYearLater, RankedStatus<14) #remove chicks listed as "NS (not seen) and NSA (not seen known to be alive)



G<-ggplot(data=subset(AllYearLater,BreedingStatus=="H"), aes(x=as.factor(Sex)))+
  facet_wrap(vars(as.factor(YearsLater)))+
  geom_bar(aes(fill=as.factor(DispersalStatus)))
G

G<-ggplot(data=subset(AllYearLater), aes(x=as.factor(Sex)))+
  facet_wrap(vars(as.factor(YearsLater)),ncol=4)+
  geom_bar(aes(fill=as.factor(DispersalStatus)))


table(AllYearLater$YearsLater, AllYearLater$BreedingStatus)

ggplot(data=subset(AllYearLater), aes(x=YearsLater, y=DispersalStatus, color=as.factor(Sex),group=as.factor(Sex)))+geom_jitter(width=0.3,height=0.05)+geom_smooth(method='lm')

ggplot(data=subset(AllYearLater, ClutchSize>1), aes(x=YearsLater, y=DispersalStatus, color=as.factor(Sex),group=as.factor(Sex)))+geom_jitter(width=0.3,height=0.05, )+geom_smooth(method='lm',se=F)+stat_summary(
  geom = "point",
  fun = "mean",
  col = "black",
  size = 3,
  shape = 24,
  fill = "red"
)
ggplot(data=subset(AllYearLater, YearsLater!=0), aes(x=YearsLater, y=DispersalStatus, color=as.factor(Sex),group=as.factor(Sex)))+geom_jitter(width=0.3,height=0.05)+geom_smooth(method='lm', se=F)
ggplot(data=subset(AllYearLater, YearsLater!=0 & ClutchSize>1), aes(x=YearsLater, y=DispersalStatus, color=as.factor(Sex),group=as.factor(Sex))) +geom_jitter(width=0.3,height=0.05)+geom_smooth(method='lm', se=F)+ ylim(0,1)+stat_summary(fun = mean, 
                                                                                                                                                                                                                                                         geom = "point",
                                                                                                                                                                                                                                                         aes(group = Sex),
                                                                                                                                                                                                                                                         shape = 95, size = 12, show.legend = F)
ggplot(data=subset(AllYearLater, YearsLater!=0 ), aes(x=YearsLater, y=DispersalStatus, color=as.factor(nOffspringBreedGroup),group=as.factor(nOffspringBreedGroup))) + 
  geom_jitter(width=0.3,height=0.05)+ 
  geom_smooth(method='lm', se=F)+ ylim(0,1) + 
  labs(x= "years later", y="dispersal status",color = "offspring born") +
  theme(legend.position = c(0.8, 0.38),
          legend.direction = "vertical")+
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(2)),
        legend.title = element_text(size = rel(2)))+
  theme(legend.background=element_blank())

ggplot(data=subset(AllYearLater, YearsLater!=0 & nOffspringBreedGroup>1), aes(x=YearsLater, y=DispersalStatus, color=as.factor(Sex),group=as.factor(Sex)))+geom_jitter(width=0.3,height=0.05)+geom_smooth(method='lm')

ggplot(data=subset(AllYearLater,BreedingStatus!="NS"|BreedingStatus!="NSA"& YearsLater!=0), aes(x=YearsLater, y=DispersalStatus, color=as.factor(Sex),group=as.factor(Sex)))+geom_jitter(width=0.3,height=0.05)+geom_smooth(method='lm')

table(AllYearLater$nOffspringBreedGroup)

AllYearLater$Sex<-as.factor(AllYearLater$Sex)
AllYearLater$DispersalStatus<-as.factor(AllYearLater$DispersalStatus)
AllYearLater$DadID<-as.factor(AllYearLater$DadID)
AllYearLater$ChickID<-as.factor(AllYearLater$ChickID)

AllYearLater$TerritoryIDx<-as.factor(AllYearLater$TerritoryIDx)
AllYearLater$FieldPeriodx<-as.factor(AllYearLater$FieldPeriodx)


m1<-glmer(DispersalStatus~Sex+GelmanTerritoryQuality+nOffspringBreedGroup+nAdults+(1|TerritoryIDx)+(1|MumID)+(1|DadID), data=subset(AllYearLater,YearsLater==1), family = binomial)
m1_sc <- update(m1,data=dfs)
ss <- getME(m1_sc,c("theta","fixef"))
m2 <- update(m1_sc,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
tt <- getME(m1_sc,"theta")
ll <- getME(m1_sc,"lower")
min(tt[ll==0])
M <- update(m1_sc,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
summary(M)
isSingular(m2, tol = 1e-4)
check_singularity(m2, tolerance = 1e-05)

summary(m)
m1<-glm(DispersalStatus~Sex+nOffspringBreedGroup+nAllABx, data=subset(AllYearLater, YearsLater==3), family = binomial)
m1<-glm(DispersalStatus~Sex+nOffspringBreedGroup+YearsLater+GelmanTerritoryQuality+nAllABx, data=subset(AllYearLater), family = binomial)
summary(m1)

m2<-glmer(DispersalStatus~Sex+GelmanTerritoryQuality+nOffspringBreedGroup*YearsLater+nAdults+(1|BreedGroupID)+(1|FieldPeriodx)+(1|TerritoryIDx)+(1|MumID)+(1|DadID), data=AllYearLater, family = binomial)
summary(m2)
length(getME(m2,"theta"))
length(fixef(m2))
numcols <- grep("^c\\.",names(AllYearLater))
dfs <- AllYearLater
dfs[,numcols] <- scale(dfs[,numcols])
m2_sc <- update(m2,data=dfs)
ss <- getME(m2_sc,c("theta","fixef"))
m2 <- update(m2_sc,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
tt <- getME(m2_sc,"theta")
ll <- getME(m2_sc,"lower")
min(tt[ll==0])
m3 <- update(m2_sc,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
summary(m3)
isSingular(m2, tol = 1e-4)
check_singularity(m2, tolerance = 1e-05)


table(AllYearLater$YearsLater, AllYearLater$DispersalStatus)
453/(453+398)
556/(556+91)
468/508

table(AllYearLater$YearsLater, AllYearLater$Dominant,AllYearLater$Sex)
241/(241+28)
198/(198+41)

ggplot(data=subset(AllYearLater, YearsLater!=0), aes(x=YearsLater, y=DispersalStatus, color=as.factor(nOffspringBreedGroup),group=as.factor(nOffspringBreedGroup))) + 
  geom_jitter(width=0.3,height=0.05)+ 
  geom_smooth(method='lm', se=F)+ ylim(0,1) + 
  labs(x= "yearsLater", y="dispersal status",color = "offspring born") +
  theme(legend.position = c(0.8, 0.38),
        legend.direction = "vertical")+
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text =element_text(size = rel(1)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)))+
  theme(legend.background=element_blank())

AllYearLater$nOffspringBreedGroupMerged<-AllYearLater$nOffspringBreedGroup
AllYearLater$nOffspringBreedGroupMerged[AllYearLater$nOffspringBreedGroupMerged>2]<-"3+"
AllYearLater$nOffspringBreedGroupMerged<-as.numeric(AllYearLater$nOffspringBreedGroupMerged)
AllYearLater$DispersalStatus<-as.numeric(AllYearLater$DispersalStatus)


ggplot(data=subset(AllYearLater, YearsLater!=0), aes(x=factor(nOffspringBreedGroupMerged), y=(DispersalStatus), color=factor(YearsLater))) + 
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="point",line="dotted", size=3)+
  stat_summary(aes(group=1, color=factor(YearsLater)),fun.data=mean_sdl, mult=1, 
               geom="line")+
  geom_jitter(alpha=0.25,width=0.3,height=0.05)+ 
  geom_smooth(method='lm', se=F)+ ylim(-0.15,1) + 
  labs(x= "offspring born", y="dispersal status",color = "years later") +
  theme(legend.direction = "vertical")+
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text =element_text(size = rel(1)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)))+
  theme(legend.background=element_blank())+
  annotate("text", size=5,x = 1, y = -0.15, label = "n=1145")+
  annotate("text",size=5, x = 2, y = -0.15, label = "n=561")+
  annotate("text", size=5, x = 3, y = -0.15, label = "n=300")
  

table(AllYearLater$nOffspringBreedGroupMerged)



ggplot(data=subset(AllYearLater, YearsLater==1), aes(x=factor(nOffspringBreedGroupMerged), y=(DispersalStatus), color=factor(YearsLater))) + 
  stat_summary(aes(group=7),fun.data = "mean_sdl", geom = "smooth")+
  geom_jitter(alpha=0.25,width=0.3,height=0.05)+ 
  geom_smooth(method='lm', se=F)+ ylim(0,1) + 
  labs(x= "offspring born", y="dispersal status",color = "years later") +
  theme(legend.position = c(0.8, 0.38),
        legend.direction = "vertical")+
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text =element_text(size = rel(1)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)))+
  theme(legend.background=element_blank())

ggplot(data=subset(AllYearLater, nOffspringBreedGroup==8), aes(x=GelmanTerritoryQuality, y=DispersalStatus)) + 
  geom_jitter(width=0.3,height=0.05)+ 
  geom_smooth(method='lm', se=F)+ 
  labs(x= "TerritoryQuality", y="Dispersal",color = "Dispersal") +
  theme(legend.position = c(0.8, 0.38),
        legend.direction = "vertical")+
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text =element_text(size = rel(1)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)))+
  theme(legend.background=element_blank())
 anova(m1,test="Chi")
drop1(m2, test="Chi")

require(plyr)
f1<-DispersalStatus~Sex+nOffspringBreedGroup+GelmanTerritoryQuality+nAllABx+YearsLater
f2<-DispersalStatus~Sex*YearsLater+nOffspringBreedGroup+GelmanTerritoryQuality+nAllABx
f3<-DispersalStatus~Sex+nOffspringBreedGroup*YearsLater+GelmanTerritoryQuality+nAllABx
f4<-DispersalStatus~Sex+nOffspringBreedGroup+GelmanTerritoryQuality*YearsLater+nAllABx
f5<-DispersalStatus~Sex+nOffspringBreedGroup+GelmanTerritoryQuality+nAllABx*YearsLater
f6<-DispersalStatus~Sex*nOffspringBreedGroup+GelmanTerritoryQuality+nAllABx+YearsLater
f7<-DispersalStatus~Sex+YearsLater+nOffspringBreedGroup*GelmanTerritoryQuality+nAllABx
f8<-DispersalStatus~Sex+YearsLater+GelmanTerritoryQuality+nAllABx*nOffspringBreedGroup
f9<-DispersalStatus~Sex*GelmanTerritoryQuality+nOffspringBreedGroup+nAllABx+YearsLater
f10<-DispersalStatus~Sex+nOffspringBreedGroup+nAllABx*GelmanTerritoryQuality+YearsLater


formulae <- list(f1, f2, f3, f4, f5,f6,f7,f8,f9,f10)
Dispersal.glms <- llply(formulae, function(f) glm(f, family=binomial, data=AllYearLater))
names(Dispersal.glms) <- formulae
ldply(Dispersal.glms, AIC)

#any sex differences in breed status of chicks surviving the next year? yes male chicks tend to become dominant the next year. chicks in better territories tend to become dominant.
AllYearLater$Dominant<-AllYearLater$RankedStatus
AllYearLater$Dominant[AllYearLater$RankedStatus==1] <- 1
AllYearLater$Dominant[AllYearLater$RankedStatus>1] <- 0
m<-glmer(Dominant ~ Sex + GelmanTerritoryQuality+ nOffspringBreedGroup +nAllABx+ (1|TerritoryIDx) +(1|FieldPeriodx) + (1|MumID) +(1|DadID), data=AllYearLater, family=binomial)
summary(m)

ggplot(data=subset(AllYearLater, YearsLater!=0), aes(x=YearsLater, y=Dominant, color=as.factor(Sex),group=as.factor(Sex))) +geom_jitter(width=0.3,height=0.05)+geom_smooth(method=lm, se=F)+ ylim(0,1)+stat_summary(fun = mean, 
                                                                                                                                                                                                                                              geom = "point",
                                                                                                                                                                                                                                              aes(group = Sex),
                                                                                                                                                                                                                                              shape = 95, size = 12, show.legend = F)



##maybe dont need the following anymore:
OneYearLater<-OneYearLater %>% 
  rename_at(vars(starts_with('TrialMergeData')), funs(paste0('', .)))

FinalMergeData <- do.call("rbind", list(TrialMergeData, TrialMergeData2, TrialMergeData3))

FinalMergeData = merge(TrialMergeData, TrialMergeData2, by.x=c("ChickID"), by.y=c("ChickID"))
FinalMergeData2 = merge(FinalMergeData, TrialMergeData3, by.x=c("ChickID"), by.y=c("ChickID"))


ggplot(data=FinalMergeData, aes(x=factor(Sex), y=value,fill = factor(Dispersal), fill=factor(Dispersal)))+ facet_grid(~ YearsLater)+
  geom_col(position = "dodge")

ggplot(data=FinalMergeData, aes(x=factor(Sex), y=value,fill = factor(Sex), fill=factor(Dispersal)))+ facet_grid(~ YearsLater)+
  geom_col(position = "dodge")

ggplot(data=FinalMergeData, aes(x=factor(YearsLater), y=HelperChicks))+
  geom_col(position="dodge")

HelperChicks<-c(14,27,16,38,9,10,16,14,1,7,19,10)
YearsLater<-c("1","1","1","1","2","2","2","2","3","3","3","3")
Sex<-c(1,1,0,0,1,1,0,0,1,1,0,0)
Dispersal<-c(1,0,1,0,1,0,1,0,1,0,1,0)

FinalMergeData<-data.frame(HelperChicks,YearsLater,Sex,Dispersal,stringsAsFactors = FALSE)

install.packages("reshape")

FinalMergeData <- melt(FinalMergeData, id=c("YearsLater","Sex","Dispersal"))
library(reshape2) # for melt

melted <- melt(FinalMergeData, "YearsLater")

melted$cat <- ''
melted[melted$variable == 'Sex',]$cat <- "first"
melted[melted$variable != 'Sex',]$cat <- "second"

ggplot(melted, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ YearsLater)
##


#subset data by breedgroup to not pseudoreplicate
TData<-ddply(Data, .(Year, BreedGroupID, TerritoryID, nHelpers, nAB, nABX, nAllAB,nHelpersABX, nAdults,cvTQbetweenyears,nOffspringBreedGroup, FieldPeriodID),summarise, #This subset is important to not pseudoreplicate females offspring sex ratios. It takes the mean sex ratio when clutch size is bigger than 1.
             SexRatio= mean(Sex),
             TerritoryQuality= mean(TerritoryQuality),
             GelmanTerritoryQuality=mean(GelmanTerritoryQuality),
             nFem= length(Sex[Sex=="0"]), # number females per Year and territory ID
             nMale= length(Sex[Sex=="1"]),# number of males per year and territory ID
             nMums=n_distinct(MumID, na.rm = FALSE)
             
) 




#add column of how variable territory qualities are within a year
TData<-merge(x = TData , y = TQgroupstata[ , c("Year", "cvTQ")], by = "Year", all.x=TRUE)

#make breedgroupid a factor
TData$BreedGroupID<- as.factor(TData$BreedGroupID)

#add a column for subordinate non-helpers
TData$NonHelpers<-TData$nAdults - TData$nHelpersABX

#add in the dominant mum and dad id for each breed group
DominantMaleIDSexYear<-subset(BirdIDSexYear, Status=="BrM")
DominantMaleIDSexYear$MaleID<-DominantMaleIDSexYear$BirdID

DominantFemaleIDSexYear<-subset(BirdIDSexYear, Status=="BrF")
DominantFemaleIDSexYear$FemaleID<-DominantFemaleIDSexYear$BirdID


TData<-merge(x = TData, y = DominantMaleIDSexYear[ , c("BreedGroupID", "MaleID")], by = "BreedGroupID", all.x=TRUE)
TData<-merge(x = TData, y = DominantFemaleIDSexYear[ , c("BreedGroupID", "FemaleID")], by = "BreedGroupID", all.x=TRUE)


#mum group stat. My dataset contains 995 mums and 1034 dads.
Mumgroupstat<-ddply(Data, .(MumID,BreedGroupID,GelmanTerritoryQuality,nHelpers, nAB, nABX, nAllAB,nHelpersABX, nAdults,Year,cvTQbetweenyears,PeriodLength),summarise,
                          OffspringSexRatio= mean(Sex),
                          nFem= length(Sex[Sex=="0"]), # number females per treatment group
                          nMale= length(Sex[Sex=="1"]))

#how many unique breeding events? 396 (when not including -998 who are NA mums)
length(unique(unlist(Mumgroupstat[c("MumID")])))

Mumgroupstat$ClutchSize<-Mumgroupstat$nFem+Mumgroupstat$nMale

#how many offspring in this?
sum(Mumgroupstat)

#how many offspring can a father sire in a year?
Dadgroupstat<-ddply(Data, .(DadID,Year,FieldPeriodID),summarise,
                    OffspringSexRatio= mean(Sex),
                    nFem= length(Sex[Sex=="0"]), # number females per treatment group
                    nMale= length(Sex[Sex=="1"]))

Dadgroupstat$ClutchSize<-Dadgroupstat$nFem+Dadgroupstat$nMale
Dadgroupstat$Year<-as.factor(Dadgroupstat$Year)
BirdIDSexYear$Year<-as.factor(BirdIDSexYear$Year)
Dadgroupstat<-left_join(Dadgroupstat, BirdIDSexYear, by=c('DadID'='BirdID', 'Year'='Year', 'FieldPeriodID'='FieldPeriodID'))
table(Dadgroupstat$Status)
835/(835+1+15+7+5+4+1+35+3+1+1)

ggplot(Dadgroupstat, aes(x=ClutchSize, y=OffspringSexRatio)) + geom_jitter(width=0.4,height=0.01) + 
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  )+ geom_hline(yintercept=mean(Dadgroupstat$OffspringSexRatio), linetype="dashed", 
                color = "red", size=0.5)+ scale_y_continuous(breaks=seq(0,1,0.1)) + labs(x = "Number of offspring sired by father", y = "Offspring sex ratio")

m<-glmer(cbind(nMale,nFem)~ClutchSize  +(1|DadID), data=Dadgroupstat, family=binomial)
summary(m)
drop1(m, test="Chi")

ggplot(Mumgroupstat, aes(x=nOffspring)) + geom_histogram(bins = 4)

ggplot(Mumgroupstat, aes(x=nOffspring, y=SexRatio)) + geom_jitter() +   stat_summary(aes(y = SexRatio), fun.y=mean, colour="red", geom="line")

ggplot(Mumgroupstat, aes(x=ClutchSize, y=OffspringSexRatio)) + geom_jitter(width=0.4,height=0.01) +  geom_smooth(method = 'lm', se=T) +
stat_summary(
  geom = "point",
  fun.y = "mean",
  col = "blue",
  size = 5,
  shape = 4,
  fill = "red"
) + 
  labs(x= "Offspring produced", y="Offspring sex ratio") +
   theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(2)))

ggplot(Mumstatusgroupstat, aes(x=ClutchSize, y=OffspringSexRatio)) + geom_jitter(width=0.4,height=0.01) + 
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  )
ggplot(Mumgroupstat, aes(x=ClutchSize, y=GelmanTerritoryQuality,)) + geom_jitter(width=0.3,height=0.01) + geom_smooth(method = 'lm', se=F) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  )

ggplot(Mumgroupstat, aes(x=nHelpersABX, y=ClutchSize,)) + geom_jitter(width=0.3,height=0.01) + geom_smooth(method = 'lm', se=F) + 
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  )


m<-glmer(cbind(nMale,nFem)~ClutchSize + nAllAB + GelmanTerritoryQuality + cvTQbetweenyears + (1|MumID), data=Mumgroupstat, family=binomial)
summary(m)
drop1(m, test="Chi")
#troubeshooting model
ss <- getME(m,c("theta","fixef"))
m2 <- update(m,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(m2)
m3 <- update(m,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
summary(m3)



Mumstatusgroupstat<-ddply(MumStatus, .(MumID,BreedGroupID,TerritoryID,GelmanTerritoryQuality,FieldPeriodID,nHelpers, nAB, nABX, nAllAB, nHelpersABX, nAdults,Year,cvTQbetweenyears,Status),summarise,
                    OffspringSexRatio= mean(Sex),
                    nFem= length(Sex[Sex=="0"]), # number females per treatment group
                    nMale= length(Sex[Sex=="1"]))

Mumstatusgroupstat$ClutchSize<-Mumstatusgroupstat$nFem+Mumstatusgroupstat$nMale
Mumstatusgroupstat<-merge(x = Mumstatusgroupstat , y = FieldPeriodIDGroupStat [ , c("FieldPeriodID", "PeriodLength")], by = "FieldPeriodID", all.x=TRUE)

#how many unique breeding events? 397
length(unique(unlist(Mumstatusgroupstat[c("MumID")])))

ggplot(Mumgroupstat, aes(x=nOffspring)) + geom_histogram(bins = 4)

ggplot(Mumgroupstat, aes(x=nOffspring, y=SexRatio)) + geom_jitter() +   stat_summary(aes(y = SexRatio), fun.y=mean, colour="red", geom="line")

theme_set(theme_bw())


ggplot(subset(Mumstatusgroupstat,Status=="BrF"), aes(x=ClutchSize, y=OffspringSexRatio)) + geom_jitter(width=0.4,height=0.01)+geom_smooth(method = 'lm') + 
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  ) + geom_hline(yintercept=mean(Mumstatusgroupstat$OffspringSexRatio[Mumstatusgroupstat$Status=="BrF"]), linetype="dashed", 
                  color = "red", size=0.5)+ scale_y_continuous(breaks=seq(0,1,0.1)) + labs(x = "Dominant mother offspring produced", y = "Offspring sex ratio")+
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)))

ggplot(subset(Mumstatusgroupstat,Status!="BrF"), aes(x=ClutchSize, y=OffspringSexRatio)) + geom_jitter(width=0.4,height=0.01) + 
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  ) + geom_hline(yintercept=mean(Mumstatusgroupstat$OffspringSexRatio[Mumstatusgroupstat$Status!="BrF"]), linetype="dashed", 
                 color = "red", size=0.5) + scale_y_continuous(breaks=seq(0,1,0.1)) + labs(x = "Subordinate mother offspring produced", y = "Offspring sex ratio")+
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)))
Mumstatusgroupstat$ClutchSizeMerged<-Mumstatusgroupstat$ClutchSize
Mumstatusgroupstat$ClutchSizeMerged[Mumstatusgroupstat$ClutchSizeMerged>2]<-"3+"
lables <- c("1" = "Dominant", "0" = "Subordinate")

ggplot(subset(Mumstatusgroupstat), aes(x=ClutchSizeMerged, y=OffspringSexRatio,color=Dominant, group=Dominant)) +
  geom_jitter(width=0.4,height=0.01)+
  geom_smooth(method = lm, se=F) + 
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 21,
    fill = "black"
  ) + 
  facet_grid(col=vars(Dominant), labeller=labeller(Dominant = lables))+
  geom_hline(data=filter(Mumstatusgroupstat, Status=="BrF"), 
          aes(yintercept=mean(Mumstatusgroupstat$OffspringSexRatio[Mumstatusgroupstat$Status=="BrF"])), 
          linetype="dashed", 
          size=1, width=0.5, 
          colour="#00CCCC") + 
  geom_hline(data=filter(Mumstatusgroupstat, Status!="BrF"), 
          aes(yintercept=mean(Mumstatusgroupstat$OffspringSexRatio[Mumstatusgroupstat$Status!="BrF"])),
          linetype="dashed", 
          size=1, width=0.5,
          colour="#FF3333") + 
  labs(x = "offspring produced", y = "offspring sex ratio")+
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)),
        legend.title =element_text(size = rel(1.5)),
        legend.text =element_text(size = rel(1.5) ),
        strip.background = element_blank(),
        strip.text.x = element_blank())+ 
  scale_color_discrete(name = "mother status",breaks=c("0", "1"), labels = c("subordinate", "dominant"))+
  geom_text(size=5,data = dat_text, aes(x = x,  y = y, label = lables), show.legend = FALSE)

dat_text <- data.frame(
  Dominant = c("1", "1", "1", "0","0","0"),
  lables = c( "n=577","n=95","n=13","n=93", "n=10", "n=4"),
  x     = c(1, 2,3,1,2,3),
  y     = c(-0.1, -0.1, -0.1,-0.1,-0.1,-0.1)
)

p + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label)
)

table(Mumstatusgroupstat$ClutchSizeMerged, Mumstatusgroupstat$Dominant)

yintercept=mean(Mumstatusgroupstat$OffspringSexRatio[Mumstatusgroupstat$Status=="BrF"]), linetype="dashed", 
color = "blue", size=0.5, width=0.5)+ scale_y_continuous(breaks=seq(0,1,0.1))

ggplot(iris, aes(Sepal.Length, Petal.Length)) + facet_wrap(~Species, scales="free") + geom_point() + 
  geom_vline(data=filter(iris, Species=="setosa"), aes(xintercept=5), colour="pink") + 
  geom_vline(data=filter(iris, Species=="versicolor"), aes(xintercept=6), colour="blue") + 
  geom_hline(data=filter(iris, Species=="virginica"), aes(yintercept=6), colour="green") 

ggplot(subset(Mumstatusgroupstat), aes(x=ClutchSizeMerged, y=OffspringSexRatio,color=Dominant, group=Dominant)) +
  geom_jitter(width=0.4,height=0.01)+
  geom_smooth(method = lm, se=F) + 
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 21,
    fill = "red"
  ) + 
  facet_grid(col=vars(Dominant))+
  labs(x = "offspring produced", y = "offspring sex ratio")+
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)))

ggplot(subset(Mumgroupstat), aes(x=ClutchSize, y=GelmanTerritoryQuality)) + geom_jitter(width=0.4,height=0.01) + 
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  ) + geom_hline(yintercept=mean(Mumstatusgroupstat$GelmanTerritoryQuality), linetype="dashed", 
                 color = "red", size=0.5)  + labs(x = "ClutchSize", y = "Transformed Territory Quality") + geom_smooth() +
  theme(axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
          axis.text =element_text(size = rel(1.5)))

ggplot(subset(Mumstatusgroupstat, Dominant==0), aes(x=as.integer(nHelpers), y=ClutchSize)) + 
  labs(x = "Number of helpers", y = "Number of offspring produced")+
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 21,
    fill = "red"
  ) +
  scale_x_continuous(breaks=c(0,1,2))+
  geom_jitter(alpha = .25, width = .2, height = 0.1) +
  geom_smooth(method = 'lm') + 
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)))

Mumstatusgroupstat$Dominant<-Mumstatusgroupstat$Status
Mumstatusgroupstat$Dominant[Mumstatusgroupstat$Status!="BrF"] <- 0
Mumstatusgroupstat$Dominant[Mumstatusgroupstat$Status=="BrF"] <- 1
Mumstatusgroupstat

m<-glmer(cbind(nMale,nFem)~ClutchSize+Dominant+GelmanTerritoryQuality +nHelpers + cvTQbetweenyears + (1|MumID) + (1|FieldPeriodID) + (1|TerritoryID), data=subset(Mumstatusgroupstat, !is.na(cvTQbetweenyears)), family=binomial)
summary(m)
drop1(m, test="Chi")
ss <- getME(m,c("theta","fixef"))
m2 <- update(m,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(m2)
m3 <- update(m,start=ss,control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5)))
summary(m3)

### Graphs ###
#what is the mean and se for clutch size?
mean(Mumgroupstat$ClutchSize)
median(Mumgroupstat$ClutchSize)
sd(Mumgroupstat$ClutchSize, na.rm=TRUE) /  
  sqrt(length(Mumgroupstat$ClutchSize[!is.na(Mumgroupstat$ClutchSize)]))

table(Mumgroupstat$ClutchSize)

#what is the mean and se for number of offspring in breed group?
mean(TData$nOffspringBreedGroup)
median(TData$nOffspringBreedGroup)

sd(TData$nOffspringBreedGroup, na.rm=TRUE) /  
  sqrt(length(TData$nOffspringBreedGroup[!is.na(TData$nOffspringBreedGroup)]))

#what is the mean and se for dominant and subordinate number of offspring produced?
mean(Data$pParentage)
sd(Data$pParentage, na.rm=TRUE) /  
  sqrt(length(Data$pParentage[!is.na(Data$pParentage)]))

#what is the mean and se field period length?
mean(FieldPeriodIDGroupStat$PeriodLength [FieldPeriodIDGroupStat$FieldPeriodID>20])
sd(FieldPeriodIDGroupStat$PeriodLength [FieldPeriodIDGroupStat$FieldPeriodID>20], na.rm=TRUE) /  
  sqrt(length(FieldPeriodIDGroupStat$PeriodLength [FieldPeriodIDGroupStat$FieldPeriodID>20][!is.na(FieldPeriodIDGroupStat$PeriodLength [FieldPeriodIDGroupStat$FieldPeriodID>20])]))

#what is the mean and se for offspring sired by father?
Dadgroupstat$ClutchSize<-Dadgroupstat$nFem+Dadgroupstat$nMale
mean(Dadgroupstat$ClutchSize[!is.na(Dadgroupstat$ClutchSize)])
sd(Dadgroupstat$ClutchSize, na.rm=TRUE) /  
  sqrt(length(Dadgroupstat$ClutchSize[!is.na(Dadgroupstat$ClutchSize)]))

#what is the mean offspring sex ratio for dom and sub mothers?
mean(Mumstatusgroupstat$OffspringSexRatio[Mumstatusgroupstat$Dominant==1])
mean(Mumstatusgroupstat$OffspringSexRatio[Mumstatusgroupstat$Dominant==0])
sum(Mumstatusgroupstat$nFem[Mumstatusgroupstat$Dominant==1])
sum(Mumstatusgroupstat$nMale[Mumstatusgroupstat$Dominant==1])

sum(Mumstatusgroupstat$nFem [Mumstatusgroupstat$Dominant==0])
sum(Mumstatusgroupstat$nMale [Mumstatusgroupstat$Dominant==0])


chisq.test(x=c(78,48),p=c(.5,.5))
binom.test(x=78, n=126, p = 1/2,
           alternative = "two.sided")

#First create a binomial line of best fit
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

#No relationship between territory quality and sex
Data$Year<-as.factor(Data$Year)
ggplot(data=subset(Data, nHelpers==0 & GelmanTerritoryQuality<5 &nOffspringBreedGroup==1), aes(x=GelmanTerritoryQuality, y=Sex, color=factor(Year), group=Year))+
  geom_point()+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)

ggplot(data=subset(MumStatus, MumStatus=="H" & GelmanTerritoryQuality<5), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)

ggplot(data=subset(MumStatus, MumStatus=="BrF" & GelmanTerritoryQuality<5), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)

ggplot(data=subset(Mumgroupstat, nHelpers==0 & GelmanTerritoryQuality<5 & ClutchSize==1), aes(x=GelmanTerritoryQuality, y=OffspringSexRatio, color=factor(Year), group=Year))+
  geom_point()+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)

ggplot(data=subset(Mumgroupstat), aes(x=ClutchSize, y=OffspringSexRatio))+
  geom_jitter()+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)

ggplot(data=subset(Mumgroupstat), aes(x=ClutchSize, y=GelmanTerritoryQuality))+
  geom_jitter()+
  geom_smooth()+
  ylim(0,1)

ggplot(data=subset(Mumgroupstat), aes(x=nAllAB, y=OffspringSexRatio))+
  geom_jitter()+
  geom_smooth(method='lm')+
  ylim(0,1)

#Interestingly, when subsetting for before 2000 (when Jan published his results, we do replicate his negative tq/sexratio relationship somewhat)
ggplot(data=subset(Data, NumberHelpers==0 & Year<2000), aes(x=GelmanTerritoryQuality, y=Sex, color=factor(Year), group=Year))+
  geom_point()+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)


#Larger clutches are more male-biased
ggplot(data=subset(Data, ClutchSize<5), aes(x=ClutchSize, y=Sex, color=factor(Year), group=Year))+
  geom_jitter(width = 0.05, height = 0.05)+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)

ggplot(data=subset(Data), aes(x=ClutchSize, y=Sex, color=, group=Year))+
  geom_jitter(width = 0.05, height = 0.05)+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)

ggplot(data=subset(TData), aes(x=ClutchSize, y=GelmanTerritoryQuality, color=Year, group=Year))+
  geom_jitter(width = 0.05, height = 0.05)+
 geom_smooth(alpha=0.0)


#Mothers in territories of better quality have larger clutches
ggplot(data=subset(Data, NumberHelpers==0 & GelmanTerritoryQuality<5), aes(x=factor(ClutchSize), y=GelmanTerritoryQuality))+
  geom_boxplot()
#Better quality territories have larger clutches visually
ggplot(data=TData, aes(x=factor(ClutchSize) , y=GelmanTerritoryQuality))+
  geom_boxplot()

#Territories with more helpers have larger clutches visually
ggplot(data=subset(TData), aes(x=factor(nHelpers), y=nOffspringBreedGroup))+
 geom_violin() +
  stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red")+
  geom_jitter(alpha = .25, width = .2, height = 0.1) +
  ylim(1,7)+ 
  labs(x= "number of helping subordinates", y="number of offspring born")+
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)))+
  annotate("text", size=5,y = 0.5, x = 1, label = "n=6")+
  annotate("text",size=5, y = 0.5, x = 2, label = "n=10")+
  annotate("text", size=5, y = 0.5, x = 3, label = "n=32")+
  annotate("text", size=5,y = 0.5, x =4, label = "n=83")+
  ylim(0.5,7)




ggplot(data=subset(Mumstatusgroupstat, Dominant==1), aes(x=factor(nHelpersABX), y=ClutchSize))+
  geom_violin() +
  ylim(1,3)+ 
  labs(x= "Number of subordinates", y="Number of offspring born")+
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text =element_text(size = rel(1)))

geom_violin()#The number of adult subordinates does not affect the sex ratio of the breed group
ggplot(data=TData, aes(x=(nAllAB), y=SexRatio))+
  geom_jitter(width = 0.07, height = 0.02)+
  geom_point()+
  geom_smooth(method='lm')


ggplot(data=TData, aes(x=factor(NumberHelpers), y=ClutchSize, color=Year, group=Year))+
  geom_jitter(width = 0.07, height = 0.07)+
  geom_point()


#But is it in fact longer field periods have larger clutches because mothers have two clutches a year? No (yay!)
ggplot(data=subset(TData), aes(x=factor(ClutchSize), y=PeriodLength))+
  geom_boxplot()

#No observable relationship between territory quality and number of helpers
ggplot(data=subset(TData), aes(x=factor(NumberHelpers), y=GelmanTerritoryQuality))+
  geom_boxplot()+
  geom_smooth()


ggplot(data=TData, aes(x=ClutchSize, y=HelpersABX))+
  geom_point()+
  geom_jitter(width=0.1, height=0.1)

TData$Year<-as.factor(TData$Year)
TData$TerritoryID<-as.factor(TData$TerritoryID)

TData$DadID[TData$DadID == "-998"] <- NA
TData$MumID<-as.factor(TData$MumID)
TData$DadID<-as.factor(TData$DadID)

mm<-glmer(nOffspringBreedGroup ~ GelmanTerritoryQuality +nHelpers + NonHelpers +PeriodLength + (1|TerritoryID) + (1|FieldPeriodID)+ (1|MaleID)+ (1|FemaleID), data=TData, family=poisson)
summary(mm)
drop1(mm, test="Chisq")

mmm<-glmer(nAdults~GelmanTerritoryQuality + (1|FieldPeriodID) + (1|TerritoryID) + (1|MaleID) + (1|FemaleID), data=TData, family=poisson)
summary(mmm)
drop1(mmm, test="Chisq")
Mumgroupstat$MumID<-as.factor(Mumgroupstat$MumID)

Mumstatusgroupstat$NonHelpers<-Mumstatusgroupstat$nAdults - Mumstatusgroupstat$nHelpersABX
Mumstatusgroupstat$Dominant<-as.factor(Mumstatusgroupstat$Dominant)
mmm<-glm(ClutchSize ~ GelmanTerritoryQuality+ Dominant*nHelpers +NonHelpers+ PeriodLength, data=Mumstatusgroupstat, family=poisson)
mmm<-glmer(ClutchSize ~ GelmanTerritoryQuality+nHelpers+Dominant +  NonHelpers+ PeriodLength + (1|TerritoryID) + (1|MumID), data=subset(Mumstatusgroupstat), family=poisson)

summary(mmm)
mmm<-glm(ClutchSize ~ GelmanTerritoryQuality+ nAllAB + PeriodLength, data=Mumgroupstat, family=poisson)
mm<-glmer(ClutchSize ~ GelmanTerritoryQuality+ nAllAB + PeriodLength + (1|BreedGroupID), data=Mumgroupstat, family=poisson)
summary(mm)

# what does the data look like for dominants and subordinates in terms of their clutch size?
ggplot(data = subset(Mumstatusgroupstat), aes(factor(Dominant), ClutchSize)) +
  geom_boxplot()+geom_jitter()


anova(mmm,mm)
drop1(mm, test="Chisq")


## Investigating whether helpers produce more sons
#Load, then subset PedigreePlusStatus
## subset data removing weird stuff
MumStatus <- subset(PedigreePlusStatus, !is.na(Year))
MumStatus <- subset(MumStatus, !is.na(Sex))
MumStatus <- subset(MumStatus, !is.na(TerritoryQuality))
MumStatus <- subset(MumStatus, pParentage>0.5) #choose chicks who's parentage is known with higher than half probability
MumStatus$MumID <- as.character(MumStatus$MumID)
MumStatus$MumID[MumStatus$MumID == "-998"] <- "NA"
MumStatus$MumID<- as.factor(MumStatus$MumID)
MumStatus<-subset(MumStatus, !is.na(MumID) & !is.na(BreedGroupID))
MumStatus$TerritoryQuality<-as.numeric(gsub(",", ".", MumStatus$TerritoryQuality))
MumStatus$GelmanTerritoryQuality<-(log(MumStatus$TerritoryQuality)-(mean(log(MumStatus$TerritoryQuality))))/(2*sd(log(MumStatus$TerritoryQuality)))
#add cvTQbetweenyears column
MumStatus<-merge(x = MumStatus , y = TerritoryIDGroupStat[ , c("TerritoryID", "cvTQbetweenyears")], by = "TerritoryID", all.x=TRUE)
#create column of all additional birds (helpers + additional birds + additional birds that might help)
MumStatus<-merge(x = MumStatus , y = Data[ , c("BreedGroupID", "HelpersABX")], by = "BreedGroupID", all.x=TRUE)



MumStatusgroupstat<-ddply(MumStatus, .(MumStatus),summarise,
                       SexRatio= mean(Sex),
                       nFem= length(Sex[Sex=="0"]), # number females per treatment group
                       nMale= length(Sex[Sex=="1"])
                       
) 


### Results ###


m1<-glmer(ClutchSize ~ NumberHelpers + GelmanTerritoryQuality + cvTQbetweenyears + (1|Year), data=subset(TData, !is.na(TData$cvTQbetweenyears)), family="poisson")

m1<-glmer(ClutchSize ~ 1+(1|Year), data=subset(TData), family="poisson")
summary(m1)
drop1(m1, test="Chisq")




#better quality territories have more helpers and potential helpers (additional birds)??
ggplot(data=TData, aes(x=factor(GelmanTerritoryQuality) , y=nOffspringBreedGroup))+
  geom_boxplot()

ggplot(data=TData, aes(x=factor(nAdults) , y=GelmanTerritoryQuality))+
  geom_violin() + coord_flip() +
  stat_summary(fun.y=mean, mult=1, 
               geom="pointrange", color="red")+
  geom_jitter(alpha = .25, width = .2, height = 0.1)+
  labs(x= "number of subordinates", y="territory quality")+
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)))+
  annotate("text", size=5,y = -1.9, x = 7, label = "n=6")+
  annotate("text",size=5, y = -1.9, x = 6, label = "n=10")+
  annotate("text", size=5, y = -1.9, x = 5, label = "n=32")+
  annotate("text", size=5,y = -1.9, x =4, label = "n=83")+
  annotate("text",size=5, y = -1.9, x = 3, label = "n=203")+
  annotate("text", size=5, y = -1.9, x = 2, label = "n=295")+
  annotate("text", size=5, y = -1.9, x = 1, label = "n=234")+ ylim(-2.01,2.01)

table(TData$nAdults)

ggplot(data=subset(TData), aes(x=factor(nHelpers), y=nOffspringBreedGroup))+
  geom_violin() +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="red")+
  geom_jitter(alpha = .25, width = .2, height = 0.1) +
  ylim(1,4)+ 
  labs(x= "Number of helping subordinates", y="Number of offspring born")+
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(2)),
        axis.text =element_text(size = rel(1.5)))



#variable territories have larger clutches??
ggplot(data=TData, aes(x=factor(ClutchSize) , y=cvTQbetweenyears))+
  geom_boxplot()


## model  whether sex ratio is determined by territory quality
Data$Year<-as.factor(Data$Year)
m<-glm(Sex ~  GelmanTerritoryQuality+ ClutchSize + Year + cvTQbetweenyears+ nHelpersABX, data=(subset(Data, ClutchSize<5)), family=binomial)
summary(m)
drop1(m, test="Chisq")

m<-glm(Sex ~  GelmanTerritoryQuality + factor(Year)+ cvTQbetweenyears+ NumberHelpers + ClutchSize + MumStatus, data=MumStatus, family=binomial)
summary(m)
drop1(m, test="Chisq")

m<-glm(Sex ~  GelmanTerritoryQuality + cvTQbetweenyears+ nHelpers + ClutchSize + Dominant, data=subset(NAremovedmumstatusgroupstat), family=binomial)
summary(m)
drop1(m, test="Chisq")

#check na
sapply(Mumstatusgroupstat, function(x) sum(is.na(x)))

#remove na in cvTQbetweenyears
NAremovedmumstatusgroupstat<-subset(Mumstatusgroupstat, !is.na(cvTQbetweenyears))
sapply(NAremovedmumstatusgroupstat, function(x) sum(is.na(x)))

##plotting for inidividual years the territory quality/ sex ratio relationship

m96<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==1996), family=binomial)
summary(m96)

m97<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==1997), family=binomial)
summary(m97)

m98<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==1998), family=binomial)
summary(m98)

m99<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==1999), family=binomial)
summary(m99)

m00<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2000), family=binomial)
summary(m00)

m01<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2001), family=binomial)
summary(m01)

m02<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2002), family=binomial)
summary(m02)

m03<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2003), family=binomial)
summary(m03)

m04<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2004), family=binomial)
summary(m04)

m05<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2005), family=binomial)
summary(m05)

m06<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2006), family=binomial)
summary(m06)

m07<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2007), family=binomial)
summary(m07)

m08<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2008), family=binomial)
summary(m08)

m09<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2009), family=binomial)
summary(m09)

m10<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2010), family=binomial)
summary(m10)

m11<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2011), family=binomial)
summary(m11)

m12<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2012), family=binomial)
summary(m12)

m13<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2013), family=binomial)
summary(m13)


m14<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2014), family=binomial)
summary(m14)

m15<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2015), family=binomial)
summary(m15)

m16<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2016), family=binomial)
summary(m16)

m17<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2017), family=binomial)
summary(m17)

m18<-glm(Sex ~ GelmanTerritoryQuality + AllAB + ClutchSize, data=subset(Data, Year==2018), family=binomial)
summary(m18)

modelslopes<-c(-0.8888, -1.2982, -0.69365, 0.1917, 0.50857, -2.031, 0.3210, 0.6166, -0.75134, -0.4117, -0.8239, 0.79073, 0.62022, 0.5042, -0.6920, -0.35407, 0.079972, 0.1422, 0.66047)
TQgroupstata$Slopes<-modelslopes

ggplot(data=TQgroupstata, aes(x=cvTQ, y=Slopes))+
  geom_point()

ggplot(data=TQgroupstata, aes(x=MeanTQ, y=Slopes))+
  geom_point()

ggplot(data=TQGroupStat, aes(x=PopulationSexRatio, y=Slopes))+
  geom_point()

m<-lm(Slopes~cvTQ + MeanTQ + (nMale/nMale/nFem), data=TQgroupstata)
summary(m)
drop1(m)

# create models to determine the slope (gradient). use subsetted data with no helpers for this. use Gelman transformed territory quality.
m96<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears + (1|MumID) + (1|BreedGroupID), data=subset(Data, Year==1996 & NumberHelpers==0), family=binomial) #doublecheck this is correct with mirjam
summary(m96)

m97<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==1997 & NumberHelpers==0 ), family=binomial)
summary(m97)

m98<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==1998 & NumberHelpers==0 ), family=binomial)
summary(m98)

m99<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==1999 & NumberHelpers==0 ), family=binomial)
summary(m99)

m00<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2000 & NumberHelpers==0 ), family=binomial)
summary(m00)

m01<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2001 & NumberHelpers==0 ), family=binomial)
summary(m01)

m02<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2002 & NumberHelpers==0 ), family=binomial)
summary(m02)

m03<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2003 & NumberHelpers==0 ), family=binomial)
summary(m03)

m04<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2004 & NumberHelpers==0 ), family=binomial)
summary(m04)

m05<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2005 & NumberHelpers==0 ), family=binomial)
summary(m05)

m06<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2006 & NumberHelpers==0 ), family=binomial)
summary(m06)

m07<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2007 & NumberHelpers==0 ), family=binomial)
summary(m07)

m08<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2008 & NumberHelpers==0 ), family=binomial)
summary(m08)

m09<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2009 & NumberHelpers==0 ), family=binomial)
summary(m09)

m10<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2010 & NumberHelpers==0 ), family=binomial)
summary(m10)

m11<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2011 & NumberHelpers==0 ), family=binomial)
summary(m11)

m12<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2012 & NumberHelpers==0 ), family=binomial)
summary(m12)

m13<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2013 & NumberHelpers==0 ), family=binomial)
summary(m13)


m14<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2014 & NumberHelpers==0 ), family=binomial)
summary(m14)

m15<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2015 & NumberHelpers==0 ), family=binomial)
summary(m15)

m16<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2016 & NumberHelpers==0 ), family=binomial)
summary(m16)

m17<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  (1|MumID) + (1|BreedGroupID) , data=subset(Data, Year==2017 & NumberHelpers==0 ), family=binomial)
summary(m17)

m18<-glmer(Sex ~ GelmanTerritoryQuality + ClutchSize + cvTQbetweenyears +  + ClutchSize + (1|MumID) + (1|BreedGroupID), data=subset(Data, Year==2018 & NumberHelpers==0), family=binomial)
summary(m18)

#and the slope for breeding pairs with helpers
m96<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==1996 & NumberHelpers==1), family=binomial) #doublecheck this is correct with mirjam
summary(m96)

m97<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==1997 & NumberHelpers==1), family=binomial)
summary(m97)

m98<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==1998 & NumberHelpers==1), family=binomial)
summary(m98)

m99<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==1999 & NumberHelpers==1), family=binomial)
summary(m99)

m00<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2000 & NumberHelpers==1), family=binomial)
summary(m00)

m01<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2001 & NumberHelpers==1), family=binomial)
summary(m01)

m02<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2002 & NumberHelpers==1), family=binomial)
summary(m02)

m03<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2003 & NumberHelpers==1), family=binomial)
summary(m03)

m04<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2004 & NumberHelpers==1), family=binomial)
summary(m04)

m05<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2005 & NumberHelpers==1), family=binomial)
summary(m05)

m06<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2006 & NumberHelpers==1), family=binomial)
summary(m06)

m07<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2007 & NumberHelpers==1), family=binomial)
summary(m07)

m08<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2008 & NumberHelpers==1), family=binomial)
summary(m08)

m09<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2009 & NumberHelpers==1), family=binomial)
summary(m09)

m10<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2010 & NumberHelpers==1), family=binomial)
summary(m10)

m11<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2011 & NumberHelpers==1), family=binomial)
summary(m11)

m12<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2012 & NumberHelpers==1), family=binomial)
summary(m12)

m13<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2013 & NumberHelpers==1), family=binomial)
summary(m13)


m14<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2014 & NumberHelpers==1), family=binomial)
summary(m14)

m15<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2015 & NumberHelpers==1), family=binomial)
summary(m15)

m16<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2016 & NumberHelpers==1), family=binomial)
summary(m16)

m17<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2017 & NumberHelpers==1), family=binomial)
summary(m17)

m18<-glmer(Sex ~ GelmanTerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2018 & NumberHelpers==1), family=binomial)
summary(m18)

#And for clutch size is 1

m96<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID), data=subset(Data, Year==1996 & NumberHelpers==0 & ClutchSize==1), family=binomial)
summary(m96)

m97<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID), data=subset(Data, Year==1997 & NumberHelpers==0 & ClutchSize==1), family=binomial)
summary(m97)

m98<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID), data=subset(Data, Year==1998 & NumberHelpers==0 & ClutchSize==1), family=binomial)
summary(m98)

m99<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID), data=subset(Data, Year==1999 & NumberHelpers==0 & ClutchSize==1), family=binomial)
summary(m99)

m00<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2000 & NumberHelpers==0), family=binomial)
summary(m00)

m01<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2001 & NumberHelpers==0), family=binomial)
summary(m01)

m02<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2002 & NumberHelpers==0), family=binomial)
summary(m02)

m03<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2003 & NumberHelpers==0), family=binomial)
summary(m03)

m04<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2004 & NumberHelpers==0), family=binomial)
summary(m04)

m05<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2005 & NumberHelpers==0), family=binomial)
summary(m05)

m06<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2006 & NumberHelpers==0), family=binomial)
summary(m06)

m07<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2007 & NumberHelpers==0), family=binomial)
summary(m07)

m08<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2008 & NumberHelpers==0), family=binomial)
summary(m08)

m09<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2009 & NumberHelpers==0), family=binomial)
summary(m09)

m10<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2010 & NumberHelpers==0), family=binomial)
summary(m10)

m11<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2011 & NumberHelpers==0), family=binomial)
summary(m11)

m12<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2012 & NumberHelpers==0), family=binomial)
summary(m12)

m13<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2013 & NumberHelpers==0), family=binomial)
summary(m13)


m14<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2014 & NumberHelpers==0), family=binomial)
summary(m14)

m15<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2015 & NumberHelpers==0), family=binomial)
summary(m15)

m16<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2016 & NumberHelpers==0), family=binomial)
summary(m16)

m17<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2017 & NumberHelpers==0), family=binomial)
summary(m17)

m18<-glm(Sex ~ TerritoryQuality + (1|MumID) + (1|BreedGroupID) + (1|ClutchSize), data=subset(Data, Year==2018 & NumberHelpers==0), family=binomial)
summary(m18)



#how does variation in territory quality within the year affect the slope of the territory quality/sex ratio relationship?
ggplot(data=TQgroupstata, aes(x=cvTQ, y=Slope))+
  geom_point()+
  geom_text(aes(label=Year),hjust=0, vjust=0)+
  geom_smooth(method="lm")

mod<-glm(Slope ~ cvTQ , data=TQgroupstata)
summary(mod)

 ggplot(data=TQgroupstata, aes(x=RangeTQ, y=Slope))+
  geom_point()+
  geom_text(aes(label=Year),hjust=0, vjust=0)+
  geom_smooth(method="lm")

ggplot(data=TQgroupstata, aes(x=MeanTQ, y=Slope))+
  geom_point()+
  geom_text(aes(label=Year),hjust=0, vjust=0)+
  geom_smooth(method="lm")

ggplot(data=TQgroupstata, aes(x=Year, y=Slope))+
  geom_point()+
  geom_text(aes(label=Year),hjust=0, vjust=0, size=7)

#no relationship between population sex ratio and offspring sex ratio. populaiton sex ratio seems female-biased
ggplot(data=TQgroupstata, aes(x=PopulationSexRatio, y=OffspringSexRatio))+
  geom_point()+
  geom_text(aes(label=Year),hjust=0, vjust=0)+
  geom_smooth(method="lm") 
mod<-lm(cbind(nSons/nDaughters) ~ cbind(nMale/nFem) , data=TQgroupstata)
summary(mod) 
anova(mod)

#How many chicks does each mum have per year?
MumIDYearGroupStat<-ddply(subset(Data, !is.na(MumID)), .(MumID,Year),summarise,
                          nChicks= length(ChickID))
MumIDYearGroupStat$MumID<-as.numeric(MumIDYearGroupStat$MumID, Header=TRUE)
MumIDYearGroupStat<-na.omit(MumIDYearGroupStat) 

hist(MumIDYearGroupStat$nChicks) 

ggplot(data=MumIDYearGroupStat, aes(x=Year, y=len, fill=supp)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
            color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

#### beneath this point is code written at an earlier date that may not be relevant now or needs review ####
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
install.packages("tidymodels")

## subset for each year. don't actually think i use this
Data96<-subset(Data, Year=="1996")
Data97<-subset(Data, Year=="1997")
Data98<-subset(Data, Year=="1998")
Data99<-subset(Data, Year=="1999")
Data00<-subset(Data, Year=="2000")
Data01<-subset(Data, Year=="2001")
Data02<-subset(Data, Year=="2002")
Data03<-subset(Data, Year=="2003")
Data04<-subset(Data, Year=="2004")
Data05<-subset(Data, Year=="2005")
Data06<-subset(Data, Year=="2006")
Data07<-subset(Data, Year=="2007")
Data08<-subset(Data, Year=="2008")
Data09<-subset(Data, Year=="2009")
Data10<-subset(Data, Year=="2010")
Data11<-subset(Data, Year=="2011")
Data12<-subset(Data, Year=="2012")
Data13<-subset(Data, Year=="2013")
Data14<-subset(Data, Year=="2014")
Data15<-subset(Data, Year=="2015")
Data16<-subset(Data, Year=="2016")
Data17<-subset(Data, Year=="2017")
Data18<-subset(Data, Year=="2018")

### the following script is from the start of the project when i tried to replicate Jan's 1997 nature paper and split territory quality into quartiles (low, medium, high)
# divide territory quality into groups. Low is 1, High is 3
Data$quartile <- ntile(Data$TerritoryQuality, 10) 
Data96$quartile <- ntile(Data96$TerritoryQuality, 3) 
Data97$quartile <- ntile(Data97$TerritoryQuality, 3) 
Data98$quartile <- ntile(Data98$TerritoryQuality, 3) 
Data99$quartile <- ntile(Data99$TerritoryQuality, 3) 
Data00$quartile <- ntile(Data00$TerritoryQuality, 3) 
Data01$quartile <- ntile(Data01$TerritoryQuality, 3) 
Data02$quartile <- ntile(Data02$TerritoryQuality, 3) 
Data03$quartile <- ntile(Data03$TerritoryQuality, 3) 
Data04$quartile <- ntile(Data04$TerritoryQuality, 3) 
Data05$quartile <- ntile(Data05$TerritoryQuality, 3) 
Data06$quartile <- ntile(Data06$TerritoryQuality, 3) 
Data07$quartile <- ntile(Data07$TerritoryQuality, 3) 
Data08$quartile <- ntile(Data08$TerritoryQuality, 3) 
Data09$quartile <- ntile(Data09$TerritoryQuality, 3) 
Data10$quartile <- ntile(Data10$TerritoryQuality, 3) 
Data11$quartile <- ntile(Data11$TerritoryQuality, 3) 
Data12$quartile <- ntile(Data12$TerritoryQuality, 3) 
Data13$quartile <- ntile(Data13$TerritoryQuality, 3) 
Data14$quartile <- ntile(Data14$TerritoryQuality, 3) 
Data15$quartile <- ntile(Data15$TerritoryQuality, 3) 
Data16$quartile <- ntile(Data16$TerritoryQuality, 3) 
Data17$quartile <- ntile(Data17$TerritoryQuality, 3) 
Data18$quartile <- ntile(Data18$TerritoryQuality, 3) 



#Group data by territory quality groups
DataGroupStat<-ddply(Data, .(quartile),summarise,
                   MeanTQ= mean(TerritoryQuality),
                   nFem= length(Sex[Sex=="0"]), # number females per treatment group
                   nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat96<-ddply(Data96, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat97<-ddply(Data97, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat98<-ddply(Data98, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat99<-ddply(Data99, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat00<-ddply(Data00, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat01<-ddply(Data01, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat02<-ddply(Data02, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat03<-ddply(Data03, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat04<-ddply(Data04, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat05<-ddply(Data05, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat06<-ddply(Data06, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat07<-ddply(Data07, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat08<-ddply(Data08, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat09<-ddply(Data09, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat10<-ddply(Data10, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat11<-ddply(Data11, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat12<-ddply(Data12, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat13<-ddply(Data13, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat14<-ddply(Data14, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat15<-ddply(Data15, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat16<-ddply(Data16, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group
DataGroupStat17<-ddply(Data17, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group

DataGroupStat18<-ddply(Data18, .(quartile),summarise,
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])
) # number of males per treatment group


# scatter of territory quality and sex ratio
ggplot(data=DataGroupStat, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() # scatter plot sex ratio over territory quality groups

plot(DataGroupStat96$quartile,DataGroupStat96$nMale/(DataGroupStat96$nMale+DataGroupStat96$nFem),
     main="TQ with SR",
     xlab="TQ",
     ylab="SR")

plot96<-ggplot(data=DataGroupStat96, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point()+
  geom_text(aes(label=nFem+nMale),hjust=0, vjust=0) #scatter plot sex ratio over years

plot97<-ggplot(data=DataGroupStat97, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot98<-ggplot(data=DataGroupStat98, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot99<-ggplot(data=DataGroupStat99, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot00<-ggplot(data=DataGroupStat00, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot01<-ggplot(data=DataGroupStat01, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot02<-ggplot(data=DataGroupStat02, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot03<-ggplot(data=DataGroupStat03, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot04<-ggplot(data=DataGroupStat04, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot05<-ggplot(data=DataGroupStat05, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot06<-ggplot(data=DataGroupStat06, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot07<-ggplot(data=DataGroupStat07, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot08<-ggplot(data=DataGroupStat08, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot09<-ggplot(data=DataGroupStat09, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot10<-ggplot(data=DataGroupStat10, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot11<-ggplot(data=DataGroupStat11, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot12<-ggplot(data=DataGroupStat12, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot13<-ggplot(data=DataGroupStat13, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot14<-ggplot(data=DataGroupStat14, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot15<-ggplot(data=DataGroupStat15, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot16<-ggplot(data=DataGroupStat16, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot17<-ggplot(data=DataGroupStat17, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years
plot18<-ggplot(data=DataGroupStat18, aes(x=quartile, y=(nMale/(nMale+nFem))))+
  geom_point() #Scatter plot sex ratio over years

# SR by TQ plots according to variation in TQ within the year
ggarrange(plot18, plot96,plot11,plot17,plot98,plot16,plot10,plot09, plot14,plot13,plot15,plot04,plot03,plot99,plot12,plot07,plot97,plot06 + rremove("x.text"), 
          labels = c("18", "96", "11","17", "98", "16","10", "09","14","13","15","04","03","99","12","07","97","06"),
          ncol = 7, nrow = 3)

ggarrange(plot96, plot98,plot10,plot11,plot16,plot17,plot18 + rremove("x.text"), 
          labels = c("96", "98", "10","11", "16", "17","18"),
          ncol = 4, nrow = 2)

ggarrange(plot03,plot04,plot09,plot13,plot14,plot15 + rremove("x.text"), 
          labels = c("03", "04", "09","13", "14", "15"),
          ncol = 3, nrow = 2)

ggarrange(plot97,plot99,plot06,plot07,plot08,plot12 + rremove("x.text"), 
          labels = c("97", "99", "06","07", "08", "12"),
          ncol = 3, nrow = 2)

# creating dataframe summarising statistics for each year (this is a repeat of DataGroupStat, i dont know why i did this)
Yeargroupstat<-ddply(Data, .(Year),summarise,
                     SexRatio= mean(Sex),
                     MeanTQ= mean(TerritoryQuality),
                     nFem= length(Sex[Sex=="0"]), # number females per treatment group
                     nMale= length(Sex[Sex=="1"])# number of males per treatment group
)


### under review: Include only territories that occur at least three times as these are stable
TerritoryIDFreq<-as.data.frame(table(TQ$TerritoryID))

 TerritoryIDFreq<-TerritoryIDFreq %>%
  rename(TerritoryID= Var1)

TQ<-merge(TQ, TerritoryIDFreq, by="TerritoryID")

StableTerritories<-subset(TQmerged, FreqTerritoryID>2)

summary(TQ)
table(TQ$TerritoryID)
table(TQSinglesRemoved$TerritoryID)
length(unique(TQ$TerritoryID))
length(unique(StableTerritories$TerritoryID))
length(unique(TQ$Year))

# CV territory quality

MeanTQ<-TQSinglesRemoved %>% 
  group_by(TerritoryID) %>% 
  summarize(Mean = mean(TQcorrected))

sdTQ<-TQSinglesRemoved %>% 
  group_by(TerritoryID) %>% 
  summarize(SD = sd(TQcorrected))

cvTQ<-as.data.frame(cbind(MeanTQ$TerritoryID, sdTQ$SD/MeanTQ$Mean)) 
cvTQ<-cvTQ %>%
  rename(CV=V2, 
         TerritoryID=V1)

TQvariance<-as.data.frame(cbind(MeanTQ,sdTQ$SD,cvTQ$CV))

TQmerged<-merge(TQSinglesRemoved, MeanTQ, by="TerritoryID")
TQmerged<-merge(TQmerged, sdTQ, by="TerritoryID")
TQmerged<-merge(TQmerged, cvTQ, by="TerritoryID")

# Scatter of CV of territories -DOUBLE CHECK THE TERRITOY IDS OVER 400
ggplot(data=TQvariance, aes(x=TerritoryID, y=cvTQ$CV))+
  geom_point()

ggplot(data=Data, aes(x=TerritoryID, y=TerritoryQuality))+
  geom_point()


    
#which mums occur in multiple years? looks like a third of mums only breed once. this means MumID does not show enough variance to be a random effect.
MumIDGroupStat<-ddply(Data, .(MumID),summarise,
                      nOccurences= length(MumID))



#create column ranking years by variability in territory quality REMOVE????
TerritoryIDGroupStat$TerritoryVariability <- ntile(TerritoryIDGroupStat$cvTQ, 2) #split territory IDs into top most variable or bottom most variable

#merge column ranking years by variability in territory quality with full dataset REMOVE????
TData<-merge(x = Data , y = TerritoryIDGroupStat[ , c("TerritoryID", "TerritoryVariability")], by = "TerritoryID", all.x=TRUE)

# old graphs
ggplot(data=subset(TData,cvovercvranked==1 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvovercvranked==2 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvovercvranked==3 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


ggplot(data=subset(TData,cvTQwithinyearranked==1 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQwithinyearranked==2 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQwithinyearranked==3 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

#investigating whether mothers in highly variable territories change their sex ratio based on territory quality differently
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==1996), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==1996), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==1997), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==1997), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==1998), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==1998), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==1999), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==1999), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2000), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2000), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2001), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2001), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2002), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2002), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2003), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2003), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2004), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2004), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2005), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2005), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2006), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2006), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2007), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2007), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2008), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2008), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2009), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2009), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2010), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2010), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2011), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2011), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2012), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2012), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2013), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2013), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2014), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2014), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2015), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2015), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)
ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2016), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2016), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2017), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2017), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==1 & NumberHelpers==0 & Year==2018), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData,cvTQbetweenyearsranked==2 & NumberHelpers==0 & Year==2018), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

# Sites that do not vary between years show more negative slope in territoryquality/sexratio relationship than those which vary between years 
PlotVariable<-ggplot(data=subset(TData,Variability==1 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

PlotVariable

PlotUnVariable<-ggplot(data=subset(TData,Variability==2 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

PlotUnVariable

# Testing my TQ / cvTQ metric
ggplot(data=subset(TData,NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot1996<-ggplot(data=subset(TData, Year==1996 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1997<-ggplot(data=subset(TData, Year==1997 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1998<-ggplot(data=subset(TData, Year==1998 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1999<-ggplot(data=subset(TData, Year==1999 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2000<-ggplot(data=subset(TData, Year==2000 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2001<-ggplot(data=subset(TData, Year==2001 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2002<-ggplot(data=subset(TData, Year==2002 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2003<-ggplot(data=subset(TData, Year==2003 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2004<-ggplot(data=subset(TData, Year==2004 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2005<-ggplot(data=subset(TData, Year==2005 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2006<-ggplot(data=subset(TData, Year==2006 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2007<-ggplot(data=subset(TData, Year==2007 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2008<-ggplot(data=subset(TData, Year==2008 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2009<-ggplot(data=subset(TData, Year==2009 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2010<-ggplot(data=subset(TData, Year==2010 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2011<-ggplot(data=subset(TData, Year==2011 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2012<-ggplot(data=subset(TData, Year==2012 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2013<-ggplot(data=subset(TData, Year==2013 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2014<-ggplot(data=subset(TData, Year==2014 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2015<-ggplot(data=subset(TData, Year==2015 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2016<-ggplot(data=subset(TData, Year==2016 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2017<-ggplot(data=subset(TData, Year==2017 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2018<-ggplot(data=subset(TData, Year==2018 & NumberHelpers==0), aes(x=TQcvTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


#Plots for number helpers=0

Plot1996<-ggplot(data=subset(TData, Year==1996 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1997<-ggplot(data=subset(TData, Year==1997 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1998<-ggplot(data=subset(TData, Year==1998 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1999<-ggplot(data=subset(TData, Year==1999 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2000<-ggplot(data=subset(TData, Year==2000 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2001<-ggplot(data=subset(TData, Year==2001 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2002<-ggplot(data=subset(TData, Year==2002 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2003<-ggplot(data=subset(TData, Year==2003 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2004<-ggplot(data=subset(TData, Year==2004 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2005<-ggplot(data=subset(TData, Year==2005 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2006<-ggplot(data=subset(TData, Year==2006 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2007<-ggplot(data=subset(TData, Year==2007 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2008<-ggplot(data=subset(TData, Year==2008 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2009<-ggplot(data=subset(TData, Year==2009 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2010<-ggplot(data=subset(TData, Year==2010 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2011<-ggplot(data=subset(TData, Year==2011 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2012<-ggplot(data=subset(TData, Year==2012 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2013<-ggplot(data=subset(TData, Year==2013 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2014<-ggplot(data=subset(TData, Year==2014 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2015<-ggplot(data=subset(TData, Year==2015 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2016<-ggplot(data=subset(TData, Year==2016 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2017<-ggplot(data=subset(TData, Year==2017 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2018<-ggplot(data=subset(TData, Year==2018 & NumberHelpers==0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

#Plots for number helpers=0 using gelman transformed

Plot1996<-ggplot(data=subset(Data, Year==1996 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1997<-ggplot(data=subset(Data, Year==1997 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1998<-ggplot(data=subset(Data, Year==1998 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1999<-ggplot(data=subset(Data, Year==1999 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2000<-ggplot(data=subset(Data, Year==2000 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2001<-ggplot(data=subset(Data, Year==2001 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2002<-ggplot(data=subset(Data, Year==2002 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2003<-ggplot(data=subset(Data, Year==2003 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2004<-ggplot(data=subset(Data, Year==2004 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2005<-ggplot(data=subset(Data, Year==2005 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2006<-ggplot(data=subset(Data, Year==2006 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2007<-ggplot(data=subset(Data, Year==2007 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2008<-ggplot(data=subset(Data, Year==2008 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2009<-ggplot(data=subset(Data, Year==2009 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2010<-ggplot(data=subset(Data, Year==2010 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2011<-ggplot(data=subset(Data, Year==2011 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2012<-ggplot(data=subset(Data, Year==2012 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2013<-ggplot(data=subset(Data, Year==2013 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2014<-ggplot(data=subset(Data, Year==2014 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2015<-ggplot(data=subset(Data, Year==2015 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2016<-ggplot(data=subset(Data, Year==2016 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2017<-ggplot(data=subset(Data, Year==2017 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2018<-ggplot(data=subset(Data, Year==2018 & NumberHelpers==0 & NumberAB==0 & NumberABX==0), aes(x=GelmanTerritoryQuality, y=Sex))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

#Plots for number helpers>0
ggplot(data=subset(TData, NumberHelpers>0 & GelmanMeanTQ<4), aes(x=GelmanMeanTQ, y=SexRatio, color=Year, group=Year))+
  geom_point()+
  binomial_smooth(alpha=0.0)+
  ylim(0,1)

Plot1996Helpers<-ggplot(data=subset(TData, Year==1996 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot1997Helpers<-ggplot(data=subset(TData, Year==1997 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1998Helpers<-ggplot(data=subset(TData, Year==1998 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1999Helpers<-ggplot(data=subset(TData, Year==1999 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2000Helpers<-ggplot(data=subset(TData, Year==2000 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2001Helpers<-ggplot(data=subset(TData, Year==2001 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2002Helpers<-ggplot(data=subset(TData, Year==2002 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2003Helpers<-ggplot(data=subset(TData, Year==2003 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2004Helpers<-ggplot(data=subset(TData, Year==2004 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2005Helpers<-ggplot(data=subset(TData, Year==2005 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2006Helpers<-ggplot(data=subset(TData, Year==2006 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2007Helpers<-ggplot(data=subset(TData, Year==2007 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2008Helpers<-ggplot(data=subset(TData, Year==2008 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2009Helpers<-ggplot(data=subset(TData, Year==2009 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2010Helpers<-ggplot(data=subset(TData, Year==2010 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2011Helpers<-ggplot(data=subset(TData, Year==2011 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2012Helpers<-ggplot(data=subset(TData, Year==2012 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2013Helpers<-ggplot(data=subset(TData, Year==2013 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2014Helpers<-ggplot(data=subset(TData, Year==2014 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2015Helpers<-ggplot(data=subset(TData, Year==2015 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2016Helpers<-ggplot(data=subset(TData, Year==2016 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2017Helpers<-ggplot(data=subset(TData, Year==2017 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2018Helpers<-ggplot(data=subset(TData, Year==2018 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

#Plots for number helpers>0 using gelman transformed
ggplot(data=subset(TData,MeanTQ<400000 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot1996Helpers<-ggplot(data=subset(TData, Year==1996 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot1997Helpers<-ggplot(data=subset(TData, Year==1997 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1998Helpers<-ggplot(data=subset(TData, Year==1998 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1999Helpers<-ggplot(data=subset(TData, Year==1999 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2000Helpers<-ggplot(data=subset(TData, Year==2000 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2001Helpers<-ggplot(data=subset(TData, Year==2001 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2002Helpers<-ggplot(data=subset(TData, Year==2002 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2003Helpers<-ggplot(data=subset(TData, Year==2003 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2004Helpers<-ggplot(data=subset(TData, Year==2004 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2005Helpers<-ggplot(data=subset(TData, Year==2005 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2006Helpers<-ggplot(data=subset(TData, Year==2006 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2007Helpers<-ggplot(data=subset(TData, Year==2007 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2008Helpers<-ggplot(data=subset(TData, Year==2008 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2009Helpers<-ggplot(data=subset(TData, Year==2009 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2010Helpers<-ggplot(data=subset(TData, Year==2010 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2011Helpers<-ggplot(data=subset(TData, Year==2011 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2012Helpers<-ggplot(data=subset(TData, Year==2012 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2013Helpers<-ggplot(data=subset(TData, Year==2013 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2014Helpers<-ggplot(data=subset(TData, Year==2014 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2015Helpers<-ggplot(data=subset(TData, Year==2015 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2016Helpers<-ggplot(data=subset(TData, Year==2016 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2017Helpers<-ggplot(data=subset(TData, Year==2017 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2018Helpers<-ggplot(data=subset(TData, Year==2018 & NumberHelpers>0), aes(x=GelmanMeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

#Plots for number helpers=1
ggplot(data=subset(TData,MeanTQ<400000 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot1996SingleHelper<-ggplot(data=subset(TData, Year==1996 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot1997SingleHelper<-ggplot(data=subset(TData, Year==1997 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1998SingleHelper<-ggplot(data=subset(TData, Year==1998 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1999SingleHelper<-ggplot(data=subset(TData, Year==1999 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2000SingleHelper<-ggplot(data=subset(TData, Year==2000 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2001SingleHelper<-ggplot(data=subset(TData, Year==2001 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2002SingleHelper<-ggplot(data=subset(TData, Year==2002 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2003SingleHelper<-ggplot(data=subset(TData, Year==2003 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2004SingleHelper<-ggplot(data=subset(TData, Year==2004 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2005SingleHelper<-ggplot(data=subset(TData, Year==2005 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2006SingleHelper<-ggplot(data=subset(TData, Year==2006 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2007SingleHelper<-ggplot(data=subset(TData, Year==2007 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2008SingleHelper<-ggplot(data=subset(TData, Year==2008 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2009SingleHelper<-ggplot(data=subset(TData, Year==2009 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2010SingleHelper<-ggplot(data=subset(TData, Year==2010 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2011SingleHelper<-ggplot(data=subset(TData, Year==2011 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2012SingleHelper<-ggplot(data=subset(TData, Year==2012 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2013SingleHelper<-ggplot(data=subset(TData, Year==2013 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2014SingleHelper<-ggplot(data=subset(TData, Year==2014 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2015SingleHelper<-ggplot(data=subset(TData, Year==2015 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2016SingleHelper<-ggplot(data=subset(TData, Year==2016 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2017SingleHelper<-ggplot(data=subset(TData, Year==2017 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2018SingleHelper<-ggplot(data=subset(TData, Year==2018 & NumberHelpers==1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

#Plots for number helpers>1
ggplot(data=subset(TData,MeanTQ<400000 & NumberHelpers>0), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot1996TwoThreeHelpers<-ggplot(data=subset(TData, Year==1996 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot1997TwoThreeHelpers<-ggplot(data=subset(TData, Year==1997 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1998TwoThreeHelpers<-ggplot(data=subset(TData, Year==1998 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)


Plot1999TwoThreeHelpers<-ggplot(data=subset(TData, Year==1999 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2000TwoThreeHelpers<-ggplot(data=subset(TData, Year==2000 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2001TwoThreeHelpers<-ggplot(data=subset(TData, Year==2001 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2002TwoThreeHelpers<-ggplot(data=subset(TData, Year==2002 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2003TwoThreeHelpers<-ggplot(data=subset(TData, Year==2003 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2004TwoThreeHelpers<-ggplot(data=subset(TData, Year==2004 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2005TwoThreeHelpers<-ggplot(data=subset(TData, Year==2005 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2006TwoThreeHelpers<-ggplot(data=subset(TData, Year==2006 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2007TwoThreeHelpers<-ggplot(data=subset(TData, Year==2007 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2008TwoThreeHelpers<-ggplot(data=subset(TData, Year==2008 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2009TwoThreeHelpers<-ggplot(data=subset(TData, Year==2009 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2010TwoThreeHelpers<-ggplot(data=subset(TData, Year==2010 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2011TwoThreeHelpers<-ggplot(data=subset(TData, Year==2011 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2012TwoThreeHelpers<-ggplot(data=subset(TData, Year==2012 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2013TwoThreeHelpers<-ggplot(data=subset(TData, Year==2013 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2014TwoThreeHelpers<-ggplot(data=subset(TData, Year==2014 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2015TwoThreeHelpers<-ggplot(data=subset(TData, Year==2015 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2016TwoThreeHelpers<-ggplot(data=subset(TData, Year==2016 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2017TwoThreeHelpers<-ggplot(data=subset(TData, Year==2017 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

Plot2018TwoThreeHelpers<-ggplot(data=subset(TData, Year==2018 & NumberHelpers>1), aes(x=MeanTQ, y=SexRatio))+
  geom_point()+
  binomial_smooth()+
  ylim(0,1)

#no relationship between the number of helpers at nest and the sex ratio of the offspring
ggplot(data=TData, aes(x=NumberHelpers, y=SexRatio))+
  geom_jitter(width = 0.05, height = 0.05)+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData, NumberHelpers>0), aes(x=NumberHelpers, y=SexRatio))+
  geom_jitter(width = 0.05, height = 0.05)+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData, NumberHelpers>0 & NumberHelpers<3), aes(x=NumberHelpers, y=SexRatio))+
  geom_jitter(width = 0.05, height = 0.05)+
  binomial_smooth()+
  ylim(0,1)

ggplot(data=subset(TData, NumberHelpers<2), aes(x=NumberHelpers, y=SexRatio))+
  geom_jitter(width = 0.05, height = 0.05)+
  binomial_smooth()+
  ylim(0,1)

mod<-glm(Sex ~ NumberHelpers, data=subset(Data, NumberHelpers>0 & NumberHelpers<4), family=binomial)
summary(mod)

#Scatters of TQ by SR grouped for TQ variability (CV) within the year: low, medium, high
ggarrange(Plot1996, Plot1998,Plot2010,Plot2011,Plot2016,Plot2017,Plot2018 + rremove("x.text"), 
          labels = c("96", "98", "10","11", "16", "17","18"),
          ncol = 4, nrow = 2) #Low CV years

ggarrange(Plot2003,Plot2004,Plot2009,Plot2013,Plot2014,Plot2015 + rremove("x.text"), 
          labels = c("03", "04", "09","13", "14", "15"),
          ncol = 3, nrow = 2) #Medium CV years

ggarrange(Plot1997,Plot1999,Plot2006,Plot2007,Plot2008,Plot2012 + rremove("x.text"), 
          labels = c("97", "99", "06","07", "08", "12"),
          ncol = 3, nrow = 2) #High CV years

#Scatters of TQ by SR grouped for mean TQ within the year: low, medium, high
ggarrange(Plot2003, Plot2004,Plot2006,Plot2007,Plot2008,Plot2009,Plot2012 + rremove("x.text"), 
          labels = c("'03", "'04", "'06","'07", "'08", "'09","'12"),
          ncol = 4, nrow = 2) #Low quality years

ggarrange(Plot2010,Plot2013,Plot2014,Plot2015,Plot2016,Plot2018 + rremove("x.text"), 
          labels = c("'10", "'13", "'14","'15", "'16", "'18"),
          ncol = 3, nrow = 2) #Medium quality years

ggarrange(Plot1996,Plot1997,Plot1998,Plot1999,Plot2011,Plot2017 + rremove("x.text"), 
          labels = c("'96", "'97", "'98","'99", "'11", "'17"),
          ncol = 3, nrow = 2) #High quality years

ggarrange(Plot1997, Plot2008,Plot2007,Plot1998,Plot1999,Plot2011,Plot1996,Plot2004, Plot2006,Plot2018,Plot2015,Plot2016,Plot2003,Plot2013,Plot2010,Plot2009,Plot2014,Plot2017 + rremove("x.text"), 
          labels = c("97", "08", "07","98", "99", "11","96", "04","06","18","15","16","03","13","10","09","14","17","12"),
          ncol = 7, nrow = 3) #according to CV

ggarrange(Plot2012, Plot2004,Plot2007,Plot2015,Plot2014,Plot2008,Plot2003,Plot2010, Plot2006,Plot2009,Plot2016,Plot2018,Plot2013,Plot2017,Plot2011,Plot1999,Plot1996,Plot1997 + rremove("x.text"), 
          labels = c("12", "04", "07","15", "14", "08","03", "10","06","09","16","18","13","17","11","99","96","97"),
          ncol = 7, nrow = 3) #according to range

ggarrange(Plot2012, Plot2017,Plot2014,Plot2009,Plot2010,Plot2013,Plot2003,Plot2016, Plot2015,Plot2018,Plot2006,Plot2004,Plot1996,Plot2011,Plot1999,Plot1998,Plot2007,Plot2008,Plot1997 + rremove("x.text"), 
          labels = c("12", "17", "14","09", "10", "13","03", "16","15","18","06","04","96","11","99","98","07","08","97"),
          ncol = 7, nrow = 3) #by year with no helpers
        

ggarrange(Plot1996Helpers, Plot1997Helpers,Plot1998Helpers,Plot1999Helpers,Plot2000Helpers,Plot2001Helpers,Plot2002Helpers,Plot2003Helpers, Plot2004Helpers,Plot2005Helpers,Plot2006Helpers,Plot2007Helpers,Plot2008Helpers,Plot2009Helpers,Plot2010Helpers,Plot2011Helpers,Plot2012Helpers,Plot2013Helpers,Plot2014Helpers,Plot2015Helpers,Plot2016Helpers,Plot2017Helpers,Plot2018Helpers + rremove("x.text"), 
          labels = c("96", "97", "98","99", "00", "01","02", "03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18"),
          ncol = 7, nrow = 4) #by year with helpers

ggarrange(Plot1996SingleHelper, Plot1997SingleHelper,Plot1998SingleHelper,Plot1999SingleHelper,Plot2000SingleHelper,Plot2001SingleHelper,Plot2002SingleHelper,Plot2003SingleHelper, Plot2004SingleHelper,Plot2005SingleHelper,Plot2006SingleHelper,Plot2007SingleHelper,Plot2008SingleHelper,Plot2009SingleHelper,Plot2010SingleHelper,Plot2011SingleHelper,Plot2012SingleHelper,Plot2013SingleHelper,Plot2014SingleHelper,Plot2015SingleHelper,Plot2016SingleHelper,Plot2017SingleHelper,Plot2018SingleHelper + rremove("x.text"), 
          labels = c("96", "97", "98","99", "00", "01","02", "03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18"),
          ncol = 7, nrow = 4) #by year with 1 helper

ggarrange(Plot1996TwoThreeHelpers, Plot1997TwoThreeHelpers,Plot1998TwoThreeHelpers,Plot1999TwoThreeHelpers,Plot2000TwoThreeHelpers,Plot2001TwoThreeHelpers,Plot2002TwoThreeHelpers,Plot2003TwoThreeHelpers, Plot2004TwoThreeHelpers,Plot2005TwoThreeHelpers,Plot2006TwoThreeHelpers,Plot2007TwoThreeHelpers,Plot2008TwoThreeHelpers,Plot2009TwoThreeHelpers,Plot2010TwoThreeHelpers,Plot2011TwoThreeHelpers,Plot2012TwoThreeHelpers,Plot2013TwoThreeHelpers,Plot2014TwoThreeHelpers,Plot2015TwoThreeHelpers,Plot2016TwoThreeHelpers,Plot2017TwoThreeHelpers,Plot2018TwoThreeHelpers + rremove("x.text"), 
          labels = c("96", "97", "98","99", "00", "01","02", "03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18"),
          ncol = 7, nrow = 4) #by year with 2 or 3 helpers



table(AllYearLater$ClutchSize)

TrialMergeData<-merge(TrialMergeData, Mumgroupstat, by=c("MumID", "Year"), all.x  = TRUE)


ZeroYearLater<-merge(x = ZeroYearLater , y = Mumgroupstat [ , c("MumID", "ClutchSize")], by = "MumID", all.x=TRUE)
TrialMergeData<-merge(x = TrialMergeData , y = Mumgroupstat [ , c("MumID", "ClutchSize")], by = "MumID", all.x=TRUE)
TrialMergeData2<-merge(x = TrialMergeData2 , y = Mumgroupstat [ , c("MumID", "ClutchSize")], by = "MumID", all.x=TRUE)
TrialMergeData3<-merge(x = TrialMergeData3 , y = Mumgroupstat [ , c("MumID", "ClutchSize")], by = "MumID", all.x=TRUE)


#trying to investigate dispersal status of chicks that become parents in following years
Chicksbecomemums = merge(Data, Mumgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "MumID"))
TMD = merge(TMD, Dadgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "MumID"))

Chicksbecomedads= merge(Data, Dadgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "DadID"))





MumTMDv2= merge(TrialMergeData, Mumgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "MumID"))
DadTMDv2= merge(TrialMergeData, Dadgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "DadID"))

MumTMD2v2= merge(TrialMergeData2, Mumgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "MumID"))
DadTMD2v2= merge(TrialMergeData2, Dadgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "DadID"))
BrYearLater<-subset(TrialMergeData, Status=="BrM"|Status=="BrF")
BrYearLater<-subset(AllYearLater, YearsLater==1)
BrYearLater<-subset(BrYearLater, BreedingStatus=="BrM"|BreedingStatus=="BrF")
Chicksbecomingmums= merge(BrYearLater, Mumgroupstat, by.x=c("YearsLater", "ChickID"), by.y=c("Year", "MumID"))
Chicksbecomingdads= merge(BrYearLater, Dadgroupstat, by.x=c("YearsLater", "ChickID"), by.y=c("Year", "DadID"))


NewOneYearLater<-data.frame(TrialMergeDataParentsKnown$ChickID,TrialMergeDataParentsKnown$MumID, TrialMergeDataParentsKnown$Sex.x, TrialMergeDataParentsKnown$Status,TrialMergeDataParentsKnown$DispersalStatus, TrialMergeDataParentsKnown$YearLater)
names(NewOneYearLater) <- gsub(x = names(NewOneYearLater),
                               pattern = "\\.",
                               replacement = " ")
NewOneYearLater<-rename(NewOneYearLater, c("TrialMergeDataParentsKnown ChickID"="ChickID",
                                           "TrialMergeDataParentsKnown MumID"="MumID",
                                           "TrialMergeDataParentsKnown Sex x"="Sex",
                                           "TrialMergeDataParentsKnown Status"="BreedingStatus",
                                           "TrialMergeDataParentsKnown DispersalStatus"="DispersalStatus",
                                           "TrialMergeDataParentsKnown YearLater"="YearLater"))
TrialMergeDataParentsKnown$DispersalStatus<-if_else( TrialMergeDataParentsKnown$TerritoryID.x==TrialMergeDataParentsKnown$TerritoryID.y, 0, 1)

Newchicksbecomemums= merge(OneYearLater, Mumgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "MumID"))
Newchicksbecomedads= merge(OneYearLater, Dadgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "DadID"))
Newchicksbecomemumstwoyearlater= merge(TwoYearLater, Mumgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "MumID"))
Newchicksbecomedadstwoyearlater= merge(TwoYearLater, Dadgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "DadID"))

# no longer needed?


AllYearLater<-AllYearLater %>%
  group_by(ChickID, Sex, YearsLater, nOffspringBreedGroup, ClutchSize, nAdults) %>%
  slice(which.min(RankedStatus))

Chickssurvivingoneyear<-subset(AllYearLater, YearsLater==1)
Chickssurvivingtwoyears<-subset(AllYearLater, YearsLater==2)

Chicksbecomemumsnextyear = merge(Chickssurvivingoneyear, Mumgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "MumID"))
Chicksbecomemumsnextyear = merge(Chickssurvivingtwoyears, Mumgroupstat, by.x=c("YearLater", "ChickID"), by.y=c("Year", "MumID"))

#select chicks whose field period the next year is highest (latest record), if the same chick occurs twice and is the same rank
TrialMergeData<-TrialMergeData %>%
  group_by(ChickID, BreedGroupID, BreedGroupID.y,FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.max(FieldPeriodID.y))

#select chicks whose breed group the next year is highest (latest record) if the same chick occurs twice and is the same rank and is recorded in the same field period
TrialMergeData<-TrialMergeData %>%
  group_by(ChickID, BreedGroupID, FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.max(BreedGroupID.y))
#select chicks whose field period the next year is highest (latest record), if the same chick occurs twice and is the same rank
TrialMergeData2<-TrialMergeData2 %>%
  group_by(ChickID, BreedGroupID, BreedGroupID.y,FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.max(FieldPeriodID.y))

#select chicks whose breed group the next year is highest (latest record) if the same chick occurs twice and is the same rank and is recorded in the same field period
TrialMergeData2<-TrialMergeData2 %>%
  group_by(ChickID, BreedGroupID, FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.max(BreedGroupID.y))
#select chicks whose field period the next year is highest (latest record), if the same chick occurs twice and is the same rank
TrialMergeData3<-TrialMergeData3 %>%
  group_by(ChickID, BreedGroupID, FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, nAdults.x) %>%
  slice(which.min(RankedStatus)))

#select chicks whose breed group the next year is highest (latest record) if the same chick occurs twice and is the same rank and is recorded in the same field period
TrialMergeData3<-TrialMergeData3 %>%
  group_by(ChickID, BreedGroupID, FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.max(BreedGroupID.y))

TrialMergeDataParentsKnown<-TrialMergeData
TrialMergeDataParentsKnown$MumID[TrialMergeDataParentsKnown$MumID == "NA"] <- NA
TrialMergeDataParentsKnown$DadID[TrialMergeDataParentsKnown$DadID == -998] <- NA
TrialMergeDataParentsKnown<-subset(TrialMergeDataParentsKnown, !is.na(MumID))
TrialMergeDataParentsKnown<-subset(TrialMergeDataParentsKnown, !is.na(DadID))
TrialMergeDataParentsKnown$RankedStatus<-recode(TrialMergeDataParentsKnown$Status, "CH"=1,"BrF" = 1, "BrM" = 1, "H"=2, "OFL" = 3, "FL" = 4, "ABX" = 5, "AB" = 6, "FLOAT" = 7, "SEEN2" = 8, "SEEN1" = 9, "TBRF" = 10, "TBRM" = 10, "U" = 11, "SBR" = 12, "B"=13,"NSA"=14, "NS"=15)

#select chicks whose rank the next year is "highest" if the same chick occurs twice
TrialMergeDataParentsKnown<-TrialMergeDataParentsKnown %>%
  group_by(ChickID, BreedGroupID, BreedGroupID.y, FieldPeriodID.x, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.min(RankedStatus))

#select chicks whose field period the next year is highest (latest record), if the same chick occurs twice and is the same rank
TrialMergeDataParentsKnown<-TrialMergeDataParentsKnown %>%
  group_by(ChickID, BreedGroupID, BreedGroupID.y,FieldPeriodID.x, FieldPeriodID.y, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.max(FieldPeriodID.y))

#select chicks whose breed group the next year is highest (latest record) if the same chick occurs twice and is the same rank and is recorded in the same field period
TrialMergeDataParentsKnown<-TrialMergeDataParentsKnown %>%
  group_by(ChickID, BreedGroupID, BreedGroupID.y,FieldPeriodID.x, FieldPeriodID.y, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.max(BreedGroupID.y))

TrialMergeDataParentsKnown<-TrialMergeDataParentsKnown %>%
  group_by(ChickID, BreedGroupID, BreedGroupID.y,FieldPeriodID.x, FieldPeriodID.y, Sex.x, YearLater, nOffspringBreedGroup, ClutchSize, nAdults.x) %>%
  slice(which.max(BreedGroupID))

# observations in other territories
ObservedOneYearLater<-data.frame(TrialMergeData$ChickID,TrialMergeData$MumID, TrialMergeData$Sex.x, TrialMergeData$Status,TrialMergeData$DispersalStatus, TrialMergeData$YearsLater, TrialMergeData$nOffspringBreedGroup,TrialMergeData$nAllAB,TrialMergeData$nAdults,TrialMergeData$BreedGroupID,TrialMergeData$TerritoryID.x,TrialMergeData$GelmanTerritoryQuality,TrialMergeData$TerritoryID.y,TrialMergeData$FieldPeriodID.y,TrialMergeData$YearLater)
ObservedTwoYearLater<-data.frame(TrialMergeData2$ChickID,TrialMergeData2$MumID, TrialMergeData2$Sex.x, TrialMergeData2$Status,TrialMergeData2$DispersalStatus, TrialMergeData2$YearsLater, TrialMergeData2$nOffspringBreedGroup, TrialMergeData2$nAllAB,TrialMergeData2$nAdults,TrialMergeData2$BreedGroupID,TrialMergeData2$TerritoryID.x,TrialMergeData2$GelmanTerritoryQuality,TrialMergeData2$TerritoryID.y,TrialMergeData2$FieldPeriodID.y,TrialMergeData2$YearLater)
ObservedThreeYearLater<-data.frame(TrialMergeData3$ChickID,TrialMergeData3$MumID, TrialMergeData3$Sex.x, TrialMergeData3$Status,TrialMergeData3$DispersalStatus, TrialMergeData3$YearsLater, TrialMergeData3$nOffspringBreedGroup, TrialMergeData3$nAllAB, TrialMergeData3$nAdults,TrialMergeData3$BreedGroupID,TrialMergeData3$TerritoryID.x,TrialMergeData3$GelmanTerritoryQuality,TrialMergeData3$TerritoryID.y,TrialMergeData3$FieldPeriodID.y,TrialMergeData3$YearLater)

nrow(TrialMergeData$ChickID)

names(ZeroYearLater) <- gsub(x = names(ZeroYearLater),
                             pattern = "\\.",
                             replacement = " ")
names(ObservedOneYearLater) <- gsub(x = names(ObservedOneYearLater),
                                    pattern = "\\.",
                                    replacement = " ")
names(ObservedTwoYearLater) <- gsub(x = names(ObservedTwoYearLater),
                                    pattern = "\\.",
                                    replacement = " ")
names(ObservedThreeYearLater) <- gsub(x = names(ObservedThreeYearLater),
                                      pattern = "\\.",
                                      replacement = " ")

ZeroYearLater<-rename(ZeroYearLater, c("ZeroYearLater ChickID"="ChickID",
                                       "ZeroYearLater MumID"="MumID",
                                       "ZeroYearLater Sex"="Sex",
                                       "ZeroYearLater BreedingStatus"="BreedingStatus",
                                       "ZeroYearLater DispersalStatus"="DispersalStatus",
                                       "ZeroYearLater YearsLater"="YearsLater",
                                       "ZeroYearLater nOffspringBreedGroup"="nOffspringBreedGroup",
                                       "ZeroYearLater ClutchSize"="ClutchSize",
                                       "ZeroYearLater nAdults x"="nAdults"))
ObservedOneYearLater<-rename(ObservedOneYearLater, c("TrialMergeData ChickID"="ChickID",
                                                     "TrialMergeData MumID"="MumID",
                                                     "TrialMergeData Sex x"="Sex",
                                                     "TrialMergeData Status"="BreedingStatus",
                                                     "TrialMergeData DispersalStatus"="DispersalStatus",
                                                     "TrialMergeData YearsLater"="YearsLater",
                                                     "TrialMergeData nOffspringBreedGroup"="nOffspringBreedGroup",
                                                     
                                                     "TrialMergeData nAllAB"="nAllAB",
                                                     "TrialMergeData nAdults"="nAdults",
                                                     "TrialMergeData BreedGroupID"="BreedGroupID",
                                                     "TrialMergeData TerritoryID x"="TerritoryIDx",
                                                     "TrialMergeData GelmanTerritoryQuality"="GelmanTerritoryQuality",
                                                     "TrialMergeData TerritoryID y"="TerritoryIDy",
                                                     "TrialMergeData FieldPeriodID y"="FieldPeriody",
                                                     "TrialMergeData YearLater"="YearLater"))
ObservedTwoYearLater<-rename(ObservedTwoYearLater, c("TrialMergeData2 ChickID"="ChickID",
                                                     "TrialMergeData2 MumID"="MumID",
                                                     "TrialMergeData2 Sex x"="Sex",
                                                     "TrialMergeData2 Status"="BreedingStatus",
                                                     "TrialMergeData2 DispersalStatus"="DispersalStatus",
                                                     "TrialMergeData2 YearsLater"="YearsLater",
                                                     "TrialMergeData2 nOffspringBreedGroup"="nOffspringBreedGroup",
                                                     
                                                     "TrialMergeData2 BreedGroupID"="BreedGroupID",
                                                     "TrialMergeData2 TerritoryID x"="TerritoryIDx",
                                                     "TrialMergeData2 GelmanTerritoryQuality"="GelmanTerritoryQuality",
                                                     "TrialMergeData2 TerritoryID y"="TerritoryIDy",
                                                     "TrialMergeData2 FieldPeriodID y"="FieldPeriody",
                                                     "TrialMergeData2 nAllAB"="nAllAB",
                                                     "TrialMergeData2 nAdults"="nAdults",
                                                     "TrialMergeData2 YearLater"="YearLater"))
ObservedThreeYearLater<-rename(ObservedThreeYearLater, c("TrialMergeData3 ChickID"="ChickID",
                                                         "TrialMergeData3 MumID"="MumID",
                                                         "TrialMergeData3 Sex x"="Sex",
                                                         "TrialMergeData3 Status"="BreedingStatus",
                                                         "TrialMergeData3 DispersalStatus"="DispersalStatus",
                                                         "TrialMergeData3 YearsLater"="YearsLater",
                                                         "TrialMergeData3 nOffspringBreedGroup"="nOffspringBreedGroup",
                                                         
                                                         "TrialMergeData3 BreedGroupID"="BreedGroupID",
                                                         "TrialMergeData3 TerritoryID x"="TerritoryIDx",
                                                         "TrialMergeData3 GelmanTerritoryQuality"="GelmanTerritoryQuality",
                                                         "TrialMergeData3 TerritoryID y"="TerritoryIDy",
                                                         "TrialMergeData3 FieldPeriodID y"="FieldPeriody",
                                                         "TrialMergeData3 nAllAB"="nAllAB",
                                                         "TrialMergeData3 nAdults"="nAdults",
                                                         "TrialMergeData3 YearLater"="YearLater"))



ObservedAllYearLater<-bind_rows(ObservedOneYearLater, ObservedTwoYearLater, ObservedThreeYearLater)
ObservedAllYearLater$RankedStatus<-ObservedAllYearLater$BreedingStatus
ObservedAllYearLater$RankedStatus<-recode(ObservedAllYearLater$RankedStatus, "BrF" = 1, "BrM" = 1,"CH"=2, "H"=3, "OFL" = 4, "FL" = 5, "ABX" = 6, "AB" = 7, "FLOAT" = 8, "SEEN2" = 9, "SEEN1" = 10, "TBRF" = 11, "TBRM" = 11, "U" = 12, "SBR" = 13, "B"=14,"NSA"=15, "NS"=16)
ObservedAllYearLater$BreedGroupSize<-ObservedAllYearLater$nAdults+ObservedAllYearLater$nOffspringBreedGroup


ggplot(data=subset(ObservedAllYearLater, YearsLater!=0 & RankedStatus>6 & RankedStatus<11), aes(x=YearsLater, y=DispersalStatus, color=as.factor(Sex),group=as.factor(Sex))) +geom_jitter(width=0.3,height=0.05)+geom_smooth(method='lm', se=F)+ ylim(0,1)+stat_summary(fun = mean, 
                                                                                                                                                                                                                                                                        geom = "point",
                                                                                                                                                                                                                                                                        aes(group = Sex),
                                                                                                                                                                                                                                                                        shape = 95, size = 12, show.legend = F)

table(ObservedOneYearLater$BreedingStatus, ObservedOneYearLater$Sex)

#when i tried to investigate how many offspring can a father sire in a year depending on status?
DadStatus <- subset(PedigreePlusStatus, !is.na(Year))
DadStatus <- subset(DadStatus, !is.na(Sex))
DadStatus <- subset(DadStatus, !is.na(TerritoryQuality))
DadStatus <- subset(DadStatus, pParentage>0.5) #choose chicks who's parentage is known with higher than half probability
DadStatus$DadID <- as.character(DadStatus$DadID)
DadStatus$DadID[DadStatus$DadID == "-998"] <- "NA"
DadStatus$DadID<- as.factor(DadStatus$DadID)
DadStatus<-subset(DadStatus, !is.na(DadID) & !is.na(BreedGroupID) & !is.na(ClutchSize))
DadStatus$TerritoryQuality<-as.numeric(gsub(",", ".", DadStatus$TerritoryQuality))
DadStatus$GelmanTerritoryQuality<-(log(DadStatus$TerritoryQuality)-(mean(log(DadStatus$TerritoryQuality))))/(2*sd(log(DadStatus$TerritoryQuality)))
#add cvTQbetweenyears column
DadStatus<-merge(x = DadStatus , y = TerritoryIDGroupStat[ , c("TerritoryID", "cvTQbetweenyears")], by = "TerritoryID", all.x=TRUE)
#create column of all additional birds (helpers + additional birds + additional birds that might help)
DadStatus<-merge(x = DadStatus , y = Data[ , c("BreedGroupID", "nHelpersABX")], by = "BreedGroupID", all.x=TRUE)



DadStatusgroupstat<-ddply(DadStatus, .(DadStatus),summarise,
                          SexRatio= mean(Sex),
                          nFem= length(Sex[Sex=="0"]), # number females per treatment group
                          nMale= length(Sex[Sex=="1"])
                          
) 

Dadstatusgroupstat<-ddply(DadStatus, .(DadID,BreedGroupID,GelmanTerritoryQuality,FieldPeriodID,nHelpers, nAB, nABX, nAllAB, nHelpersABX.x, nAdults,Year,cvTQbetweenyears,Status),summarise,
                          OffspringSexRatio= mean(Sex),
                          nFem= length(Sex[Sex=="0"]), # number females per treatment group
                          nMale= length(Sex[Sex=="1"]))


Dadstatusgroupstat<-ddply(Data, .(DadID,BreedGroupID,GelmanTerritoryQuality,nHelpers, nAB, nABX, nAllAB,nHelpersABX, nAdults,Year,cvTQbetweenyears),summarise,
                          OffspringSexRatio= mean(Sex),
                          nFem= length(Sex[Sex=="0"]), # number females per treatment group
                          nMale= length(Sex[Sex=="1"]))

DadStatusgroupstat=merge(Dadstatusgroupstat, BirdIDSexYear,by.x=c("BreedGroupID", "DadID"), by.y=c("BreedGroupID","BirdID"))

table(Mumstatusgroupstat$ClutchSize, Mumstatusgroupstat$Dominant)

TrialMergeData<-left_join(TrialMergeData, TQ, by=c('TerritoryID'='TerritoryID', 'FieldPeriodID'='FieldPeriodID'))

(0.967*1)-(0.166*1)+(0.007*1)
(0.967*3)-(0.166*9)+(0.007*27)
(0.967*4)-(0.166*16)+(0.007*64)
(0.967*10)-(0.166*100)+(0.007*1000)
