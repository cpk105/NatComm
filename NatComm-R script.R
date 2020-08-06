#Load library
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(RColorBrewer)
library(fields)
#library(drc)
library(nlme)
#library(devtools)

#Import data
IC1_raw<-read.csv("IC1_raw.csv")
#Sort by alphabetical order
IC1_raw_sorted<-IC1_raw[order(IC1_raw$Citation),]

#Create dataframe
pervious_stat<-data.frame(Statistics=c("N","Mean","SD","Min.","Max"),All=rep(999,times=5),Experimental=rep(999,times=5),Modelling=rep(999,times=5),All=rep(999,times=5),Experimental=rep(999,times=5),Modelling=rep(999,times=5))

#Convert factor to numeric and characters to factors
IC1_raw$irri_rate<-as.character(IC1_raw$irri_rate)
IC1_raw$irri_rate<-as.numeric(IC1_raw$irri_rate)
IC1_raw$delta_Tmean<-as.character(IC1_raw$delta_Tmean)
IC1_raw$delta_Tmean<-as.numeric(IC1_raw$delta_Tmean)
IC1_raw$delta_Ttransmax<-as.character(IC1_raw$delta_Ttransmax)
IC1_raw$delta_Ttransmax<-as.numeric(IC1_raw$delta_Ttransmax)
IC1_raw$Approach<-as.factor(IC1_raw$Approach)
IC1_raw$Environment<-as.factor(IC1_raw$Environment)

IC1_raw_sorted$irri_rate<-as.character(IC1_raw_sorted$irri_rate)
IC1_raw_sorted$irri_rate<-as.numeric(IC1_raw_sorted$irri_rate)
IC1_raw_sorted$delta_Tmean<-as.character(IC1_raw_sorted$delta_Tmean)
IC1_raw_sorted$delta_Tmean<-as.numeric(IC1_raw_sorted$delta_Tmean)
IC1_raw_sorted$delta_Ttransmax<-as.character(IC1_raw_sorted$delta_Ttransmax)
IC1_raw_sorted$delta_Ttransmax<-as.numeric(IC1_raw_sorted$delta_Ttransmax)
IC1_raw_sorted$Approach<-as.factor(IC1_raw_sorted$Approach)
IC1_raw_sorted$Environment<-as.factor(IC1_raw_sorted$Environment)

#Calculate cooling statistics (pervious)
#Exclude studies
#IC1_raw_excluded<-IC1_raw[-c(19,20,14,25,27,33,8,26),]
IC1_raw_excluded<-IC1_raw_sorted[-c(3,5,10,11,13,18,19,21,22,23,24,25,26,28:31),] 
pervious_stat[1,2:7]<-c(length(IC1_raw_excluded$delta_Tmean[!is.na(IC1_raw_excluded$delta_Tmean)]),length(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Experimental"&!is.na(IC1_raw_excluded$delta_Tmean)]),length(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Modelling"&!is.na(IC1_raw_excluded$delta_Tmean)]),length(IC1_raw_excluded$delta_Ttransmax[!is.na(IC1_raw_excluded$delta_Ttransmax)]),length(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Experimental"&!is.na(IC1_raw_excluded$delta_Ttransmax)]),length(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Modelling"&!is.na(IC1_raw_excluded$delta_Ttransmax)]))
pervious_stat[2:5,2]<-c(mean(IC1_raw_excluded$delta_Tmean,na.rm=T),sd(IC1_raw_excluded$delta_Tmean,na.rm=T),min(IC1_raw_excluded$delta_Tmean,na.rm=T),max(IC1_raw_excluded$delta_Tmean,na.rm=T))
pervious_stat[2:5,3]<-c(mean(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Experimental"],na.rm=T),sd(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Experimental"],na.rm=T),min(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Experimental"],na.rm=T),max(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Experimental"],na.rm=T))
pervious_stat[2:5,4]<-c(mean(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Modelling"],na.rm=T),sd(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Modelling"],na.rm=T),min(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Modelling"],na.rm=T),max(IC1_raw_excluded$delta_Tmean[IC1_raw_excluded$Approach=="Modelling"],na.rm=T))
pervious_stat[2:5,5]<-c(mean(IC1_raw_excluded$delta_Ttransmax,na.rm=T),sd(IC1_raw_excluded$delta_Ttransmax,na.rm=T),min(IC1_raw_excluded$delta_Ttransmax,na.rm=T),max(IC1_raw_excluded$delta_Ttransmax,na.rm=T))
pervious_stat[2:5,6]<-c(mean(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Experimental"],na.rm=T),sd(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Experimental"],na.rm=T),min(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Experimental"],na.rm=T),max(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Experimental"],na.rm=T))
pervious_stat[2:5,7]<-c(mean(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Modelling"],na.rm=T),sd(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Modelling"],na.rm=T),min(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Modelling"],na.rm=T),max(IC1_raw_excluded$delta_Ttransmax[IC1_raw_excluded$Approach=="Modelling"],na.rm=T))

write.csv(pervious_stat,"C:/Users/Cheung Pui Kwan/Desktop/Temp/pervious_stat.csv")


#Import .tif 
Adegoke2003_LW<-raster("Adegoke2003_Nebraska_LWnet.tif")
Adegoke2003_RP<-raster("Adegoke2003_Nebraska_RP.tif")
Adegoke2003_SH<-raster("Adegoke2003_Nebraska_SH.tif")
Adegoke2003_SM<-raster("Adegoke2003_Nebraska_SM.tif")
Adegoke2003_SW<-raster("Adegoke2003_Nebraska_SWnet.tif")
Adegoke2003_Ta<-raster("Adegoke2003_Nebraska_Ta.tif")
Adegoke2003_v<-raster("Adegoke2003_Nebraska_v.tif")


Alter2015_1LW<-raster("Alter2015_Gezira_LWnet_Jul.tif")
Alter2015_1RP<-raster("Alter2015_Gezira_RP_Jul.tif")
Alter2015_1SH<-raster("Alter2015_Gezira_SH_Jul.tif")
Alter2015_1SM<-raster("Alter2015_Gezira_SM_Jul.tif")
Alter2015_1SW<-raster("Alter2015_Gezira_SWnet_Jul.tif")
Alter2015_1Ta<-raster("Alter2015_Gezira_Ta_Jul.tif")
Alter2015_1v<-raster("Alter2015_Gezira_v_Jul.tif")
Alter2015_2LW<-raster("Alter2015_Gezira_LWnet_Aug.tif")
Alter2015_2RP<-raster("Alter2015_Gezira_RP_Aug.tif")
Alter2015_2SH<-raster("Alter2015_Gezira_SH_Aug.tif")
Alter2015_2SM<-raster("Alter2015_Gezira_SM_Aug.tif")
Alter2015_2SW<-raster("Alter2015_Gezira_SWnet_Aug.tif")
Alter2015_2Ta<-raster("Alter2015_Gezira_Ta_Aug.tif")
Alter2015_2v<-raster("Alter2015_Gezira_v_Aug.tif")



Bonan2000_LW<-raster("Bonan2000_Boulder_LWnet.tif")
Bonan2000_RP<-raster("Bonan2000_Boulder_RP.tif")
Bonan2000_SH<-raster("Bonan2000_Boulder_SH.tif")
Bonan2000_SM<-raster("Bonan2000_Boulder_SM.tif")
Bonan2000_SW<-raster("Bonan2000_Boulder_SWnet.tif")
Bonan2000_Ta<-raster("Bonan2000_Boulder_Ta.tif")
Bonan2000_v<-raster("Bonan2000_Boulder_v.tif")



Broadbent2018_LW<-raster("Broadbent2018_Mawson_LWnet.tif")
Broadbent2018_RP<-raster("Broadbent2018_Mawson_RP.tif")
Broadbent2018_SH<-raster("Broadbent2018_Mawson_SH.tif")
Broadbent2018_SM<-raster("Broadbent2018_Mawson_SM.tif")
Broadbent2018_SW<-raster("Broadbent2018_Mawson_SWnet.tif")
Broadbent2018_Ta<-raster("Broadbent2018_Mawson_Ta.tif")
Broadbent2018_v<-raster("Broadbent2018_Mawson_v.tif")


Broadbent2019_LW<-raster("Broadbent2019_Mawson_LWnet.tif")
Broadbent2019_RP<-raster("Broadbent2019_Mawson_RP.tif")
Broadbent2019_SH<-raster("Broadbent2019_Mawson_SH.tif")
Broadbent2019_SM<-raster("Broadbent2019_Mawson_SM.tif")
Broadbent2019_SW<-raster("Broadbent2019_Mawson_SWnet.tif")
Broadbent2019_Ta<-raster("Broadbent2019_Mawson_Ta.tif")
Broadbent2019_v<-raster("Broadbent2019_Mawson_v.tif")


Chen2017_LW<-raster("Chen2017_YellowRiver_LWnet.tif")
Chen2017_RP<-raster("Chen2017_YellowRiver_RP.tif")
Chen2017_SH<-raster("Chen2017_YellowRiver_SH.tif")
Chen2017_SM<-raster("Chen2017_YellowRiver_SM.tif")
Chen2017_SW<-raster("Chen2017_YellowRiver_SWnet.tif")
Chen2017_Ta<-raster("Chen2017_YellowRiver_Ta.tif")
Chen2017_v<-raster("Chen2017_YellowRiver_v.tif")


Chen2018_1LW<-raster("Chen2018_Mead_LWnet_May.tif")
Chen2018_1RP<-raster("Chen2018_Mead_RP_May.tif")
Chen2018_1SH<-raster("Chen2018_Mead_SH_May.tif")
Chen2018_1SM<-raster("Chen2018_Mead_SM_May.tif")
Chen2018_1SW<-raster("Chen2018_Mead_SWnet_May.tif")
Chen2018_1Ta<-raster("Chen2018_Mead_Ta_May.tif")
Chen2018_1v<-raster("Chen2018_Mead_v_May.tif")
Chen2018_2LW<-raster("Chen2018_Mead_LWnet_Jun.tif")
Chen2018_2RP<-raster("Chen2018_Mead_RP_Jun.tif")
Chen2018_2SH<-raster("Chen2018_Mead_SH_Jun.tif")
Chen2018_2SM<-raster("Chen2018_Mead_SM_Jun.tif")
Chen2018_2SW<-raster("Chen2018_Mead_SWnet_Jun.tif")
Chen2018_2Ta<-raster("Chen2018_Mead_Ta_Jun.tif")
Chen2018_2v<-raster("Chen2018_Mead_v_Jun.tif")
Chen2018_3LW<-raster("Chen2018_Mead_LWnet_Jul.tif")
Chen2018_3RP<-raster("Chen2018_Mead_RP_Jul.tif")
Chen2018_3SH<-raster("Chen2018_Mead_SH_Jul.tif")
Chen2018_3SM<-raster("Chen2018_Mead_SM_Jul.tif")
Chen2018_3SW<-raster("Chen2018_Mead_SWnet_Jul.tif")
Chen2018_3Ta<-raster("Chen2018_Mead_Ta_Jul.tif")
Chen2018_3v<-raster("Chen2018_Mead_v_Jul.tif")
Chen2018_4LW<-raster("Chen2018_Mead_LWnet_Aug.tif")
Chen2018_4RP<-raster("Chen2018_Mead_RP_Aug.tif")
Chen2018_4SH<-raster("Chen2018_Mead_SH_Aug.tif")
Chen2018_4SM<-raster("Chen2018_Mead_SM_Aug.tif")
Chen2018_4SW<-raster("Chen2018_Mead_SWnet_Aug.tif")
Chen2018_4Ta<-raster("Chen2018_Mead_Ta_Aug.tif")
Chen2018_4v<-raster("Chen2018_Mead_v_Aug.tif")
Chen2018_5LW<-raster("Chen2018_Mead_LWnet_Sep.tif")
Chen2018_5RP<-raster("Chen2018_Mead_RP_Sep.tif")
Chen2018_5SH<-raster("Chen2018_Mead_SH_Sep.tif")
Chen2018_5SM<-raster("Chen2018_Mead_SM_Sep.tif")
Chen2018_5SW<-raster("Chen2018_Mead_SWnet_Sep.tif")
Chen2018_5Ta<-raster("Chen2018_Mead_Ta_Sep.tif")
Chen2018_5v<-raster("Chen2018_Mead_v_Sep.tif")



Cook2014_LW<-raster("Cook2014_Global_LWnet.tif")
Cook2014_RP<-raster("Cook2014_Global_RP.tif")
Cook2014_SH<-raster("Cook2014_Global_SH.tif")
Cook2014_SM<-raster("Cook2014_Global_SM.tif")
Cook2014_SW<-raster("Cook2014_Global_SWnet.tif")
Cook2014_Ta<-raster("Cook2014_Global_Ta.tif")
Cook2014_v<-raster("Cook2014_Global_v.tif")


Daniel2018_LW<-raster("Daniel2018_Paris_LWnet.tif")
Daniel2018_RP<-raster("Daniel2018_Paris_RP.tif")
Daniel2018_SH<-raster("Daniel2018_Paris_SH.tif")
Daniel2018_SM<-raster("Daniel2018_Paris_SM.tif")
Daniel2018_SW<-raster("Daniel2018_Paris_SWnet.tif")
Daniel2018_Ta<-raster("Daniel2018_Paris_Ta.tif")
Daniel2018_v<-raster("Daniel2018_Paris_v.tif")


Geerts2006_LW<-raster("Geerts2006_MCMIA_LWnet.tif")
Geerts2006_RP<-raster("Geerts2006_MCMIA_RP.tif")
Geerts2006_SH<-raster("Geerts2006_MCMIA_SH.tif")
Geerts2006_SM<-raster("Geerts2006_MCMIA_SM.tif")
Geerts2006_SW<-raster("Geerts2006_MCMIA_SWnet.tif")
Geerts2006_Ta<-raster("Geerts2006_MCMIA_Ta.tif")
Geerts2006_v<-raster("Geerts2006_MCMIA_v.tif")


Grossman_Clarke2010_1LW<-raster("Grossman-Clarke2010_Phoenix_LWnet_2003.tif")
Grossman_Clarke2010_1RP<-raster("Grossman-Clarke2010_Phoenix_RP_2003.tif")
Grossman_Clarke2010_1SH<-raster("Grossman-Clarke2010_Phoenix_SH_2003.tif")
Grossman_Clarke2010_1SM<-raster("Grossman-Clarke2010_Phoenix_SM_2003.tif")
Grossman_Clarke2010_1SW<-raster("Grossman-Clarke2010_Phoenix_SWnet_2003.tif")
Grossman_Clarke2010_1Ta<-raster("Grossman-Clarke2010_Phoenix_Ta_2003.tif")
Grossman_Clarke2010_1v<-raster("Grossman-Clarke2010_Phoenix_v_2003.tif")
Grossman_Clarke2010_2LW<-raster("Grossman-Clarke2010_Phoenix_LWnet_2005to2007Jun.tif")
Grossman_Clarke2010_2RP<-raster("Grossman-Clarke2010_Phoenix_RP_2005to2007Jun.tif")
Grossman_Clarke2010_2SH<-raster("Grossman-Clarke2010_Phoenix_SH_2005to2007Jun.tif")
Grossman_Clarke2010_2SM<-raster("Grossman-Clarke2010_Phoenix_SM_2005to2007Jun.tif")
Grossman_Clarke2010_2SW<-raster("Grossman-Clarke2010_Phoenix_SWnet_2005to2007Jun.tif")
Grossman_Clarke2010_2Ta<-raster("Grossman-Clarke2010_Phoenix_Ta_2005to2007Jun.tif")
Grossman_Clarke2010_2v<-raster("Grossman-Clarke2010_Phoenix_v_2005to2007Jun.tif")
Grossman_Clarke2010_3LW<-raster("Grossman-Clarke2010_Phoenix_LWnet_2005to2007Jul.tif")
Grossman_Clarke2010_3RP<-raster("Grossman-Clarke2010_Phoenix_RP_2005to2007Jul.tif")
Grossman_Clarke2010_3SH<-raster("Grossman-Clarke2010_Phoenix_SH_2005to2007Jul.tif")
Grossman_Clarke2010_3SM<-raster("Grossman-Clarke2010_Phoenix_SM_2005to2007Jul.tif")
Grossman_Clarke2010_3SW<-raster("Grossman-Clarke2010_Phoenix_SWnet_2005to2007Jul.tif")
Grossman_Clarke2010_3Ta<-raster("Grossman-Clarke2010_Phoenix_Ta_2005to2007Jul.tif")
Grossman_Clarke2010_3v<-raster("Grossman-Clarke2010_Phoenix_v_2005to2007Jul.tif")





Hancock2015_LW<-raster("Hancock2015_Toowoomba_LWnet.tif")
Hancock2015_RP<-raster("Hancock2015_Toowoomba_RP.tif")
Hancock2015_SH<-raster("Hancock2015_Toowoomba_SH.tif")
Hancock2015_SM<-raster("Hancock2015_Toowoomba_SM.tif")
Hancock2015_SW<-raster("Hancock2015_Toowoomba_SWnet.tif")
Hancock2015_Ta<-raster("Hancock2015_Toowoomba_Ta.tif")
Hancock2015_v<-raster("Hancock2015_Toowoomba_v.tif")


Harding2012_1LW<-raster("Harding2012_LWnet_1983.tif")
Harding2012_1RP<-raster("Harding2012_RP_1983.tif")
Harding2012_1SH<-raster("Harding2012_SH_1983.tif")
Harding2012_1SM<-raster("Harding2012_SM_1983.tif")
Harding2012_1SW<-raster("Harding2012_SWnet_1983.tif")
Harding2012_1Ta<-raster("Harding2012_Ta_1983.tif")
Harding2012_1v<-raster("Harding2012_v_1983.tif")
Harding2012_2LW<-raster("Harding2012_LWnet_1985.tif")
Harding2012_2RP<-raster("Harding2012_RP_1985.tif")
Harding2012_2SH<-raster("Harding2012_SH_1985.tif")
Harding2012_2SM<-raster("Harding2012_SM_1985.tif")
Harding2012_2SW<-raster("Harding2012_SWnet_1985.tif")
Harding2012_2Ta<-raster("Harding2012_Ta_1985.tif")
Harding2012_2v<-raster("Harding2012_v_1985.tif")
Harding2012_3LW<-raster("Harding2012_LWnet_1988.tif")
Harding2012_3RP<-raster("Harding2012_RP_1988.tif")
Harding2012_3SH<-raster("Harding2012_SH_1988.tif")
Harding2012_3SM<-raster("Harding2012_SM_1988.tif")
Harding2012_3SW<-raster("Harding2012_SWnet_1988.tif")
Harding2012_3Ta<-raster("Harding2012_Ta_1988.tif")
Harding2012_3v<-raster("Harding2012_v_1988.tif")
Harding2012_4LW<-raster("Harding2012_LWnet_1990.tif")
Harding2012_4RP<-raster("Harding2012_RP_1990.tif")
Harding2012_4SH<-raster("Harding2012_SH_1990.tif")
Harding2012_4SM<-raster("Harding2012_SM_1990.tif")
Harding2012_4SW<-raster("Harding2012_SWnet_1990.tif")
Harding2012_4Ta<-raster("Harding2012_Ta_1990.tif")
Harding2012_4v<-raster("Harding2012_v_1990.tif")
Harding2012_5LW<-raster("Harding2012_LWnet_1993.tif")
Harding2012_5RP<-raster("Harding2012_RP_1993.tif")
Harding2012_5SH<-raster("Harding2012_SH_1993.tif")
Harding2012_5SM<-raster("Harding2012_SM_1993.tif")
Harding2012_5SW<-raster("Harding2012_SWnet_1993.tif")
Harding2012_5Ta<-raster("Harding2012_Ta_1993.tif")
Harding2012_5v<-raster("Harding2012_v_1993.tif")
Harding2012_6LW<-raster("Harding2012_LWnet_1997.tif")
Harding2012_6RP<-raster("Harding2012_RP_1997.tif")
Harding2012_6SH<-raster("Harding2012_SH_1997.tif")
Harding2012_6SM<-raster("Harding2012_SM_1997.tif")
Harding2012_6SW<-raster("Harding2012_SWnet_1997.tif")
Harding2012_6Ta<-raster("Harding2012_Ta_1997.tif")
Harding2012_6v<-raster("Harding2012_v_1997.tif")
Harding2012_7LW<-raster("Harding2012_LWnet_2000.tif")
Harding2012_7RP<-raster("Harding2012_RP_2000.tif")
Harding2012_7SH<-raster("Harding2012_SH_2000.tif")
Harding2012_7SM<-raster("Harding2012_SM_2000.tif")
Harding2012_7SW<-raster("Harding2012_SWnet_2000.tif")
Harding2012_7Ta<-raster("Harding2012_Ta_2000.tif")
Harding2012_7v<-raster("Harding2012_v_2000.tif")
Harding2012_8LW<-raster("Harding2012_LWnet_2007.tif")
Harding2012_8RP<-raster("Harding2012_RP_2007.tif")
Harding2012_8SH<-raster("Harding2012_SH_2007.tif")
Harding2012_8SM<-raster("Harding2012_SM_2007.tif")
Harding2012_8SW<-raster("Harding2012_SWnet_2007.tif")
Harding2012_8Ta<-raster("Harding2012_Ta_2007.tif")
Harding2012_8v<-raster("Harding2012_v_2007.tif")
Harding2012_9LW<-raster("Harding2012_LWnet_2008.tif")
Harding2012_9RP<-raster("Harding2012_RP_2008.tif")
Harding2012_9SH<-raster("Harding2012_SH_2008.tif")
Harding2012_9SM<-raster("Harding2012_SM_2008.tif")
Harding2012_9SW<-raster("Harding2012_SWnet_2008.tif")
Harding2012_9Ta<-raster("Harding2012_Ta_2008.tif")
Harding2012_9v<-raster("Harding2012_v_2008.tif")

Huber2014_LW<-raster("Huber2014_LWnet.tif")
Huber2014_RP<-raster("Huber2014_RP.tif")
Huber2014_SH<-raster("Huber2014_SH.tif")
Huber2014_SM<-raster("Huber2014_SM.tif")
Huber2014_SW<-raster("Huber2014_SWnet.tif")
Huber2014_Ta<-raster("Huber2014_Ta.tif")
Huber2014_v<-raster("Huber2014_v.tif")


Iglesias2002_1LW<-raster("Iglesias2002_LWnet_Aug.tif")
Iglesias2002_1RP<-raster("Iglesias2002_RP_Aug.tif")
Iglesias2002_1SH<-raster("Iglesias2002_SH_Aug.tif")
Iglesias2002_1SM<-raster("Iglesias2002_SM_Aug.tif")
Iglesias2002_1SW<-raster("Iglesias2002_SWnet_Aug.tif")
Iglesias2002_1Ta<-raster("Iglesias2002_Ta_Aug.tif")
Iglesias2002_1v<-raster("Iglesias2002_v_Aug.tif")
Iglesias2002_2LW<-raster("Iglesias2002_LWnet_Sep.tif")
Iglesias2002_2RP<-raster("Iglesias2002_RP_Sep.tif")
Iglesias2002_2SH<-raster("Iglesias2002_SH_Sep.tif")
Iglesias2002_2SM<-raster("Iglesias2002_SM_Sep.tif")
Iglesias2002_2SW<-raster("Iglesias2002_SWnet_Sep.tif")
Iglesias2002_2Ta<-raster("Iglesias2002_Ta_Sep.tif")
Iglesias2002_2v<-raster("Iglesias2002_v_Sep.tif")




Iglesias2005_1LW<-raster("Iglesias2005_LWnet_Jul.tif")
Iglesias2005_1RP<-raster("Iglesias2005_RP_Jul.tif")
Iglesias2005_1SH<-raster("Iglesias2005_SH_Jul.tif")
Iglesias2005_1SM<-raster("Iglesias2005_SM_Jul.tif")
Iglesias2005_1SW<-raster("Iglesias2005_SWnet_Jul.tif")
Iglesias2005_1Ta<-raster("Iglesias2005_Ta_Jul.tif")
Iglesias2005_1v<-raster("Iglesias2005_v_Jul.tif")
Iglesias2005_2LW<-raster("Iglesias2005_LWnet_Aug.tif")
Iglesias2005_2RP<-raster("Iglesias2005_RP_Aug.tif")
Iglesias2005_2SH<-raster("Iglesias2005_SH_Aug.tif")
Iglesias2005_2SM<-raster("Iglesias2005_SM_Aug.tif")
Iglesias2005_2SW<-raster("Iglesias2005_SWnet_Aug.tif")
Iglesias2005_2Ta<-raster("Iglesias2005_Ta_Aug.tif")
Iglesias2005_2v<-raster("Iglesias2005_v_Aug.tif")


Kanamaru2008_LW<-raster("Kanamaru2008_LWnet.tif")
Kanamaru2008_RP<-raster("Kanamaru2008_RP.tif")
Kanamaru2008_SH<-raster("Kanamaru2008_SH.tif")
Kanamaru2008_SM<-raster("Kanamaru2008_SM.tif")
Kanamaru2008_SW<-raster("Kanamaru2008_SWnet.tif")
Kanamaru2008_Ta<-raster("Kanamaru2008_Ta.tif")
Kanamaru2008_v<-raster("Kanamaru2008_v.tif")


Kohl1974_LW<-raster("Kohl1974_LWnet.tif")
Kohl1974_RP<-raster("Kohl1974_RP.tif")
Kohl1974_SH<-raster("Kohl1974_SH.tif")
Kohl1974_SM<-raster("Kohl1974_SM.tif")
Kohl1974_SW<-raster("Kohl1974_SWnet.tif")
Kohl1974_Ta<-raster("Kohl1974_Ta.tif")
Kohl1974_v<-raster("Kohl1974_v.tif")


Lakatos2010_LW<-raster("Lakatos2010_LWnet.tif")
Lakatos2010_RP<-raster("Lakatos2010_RP.tif")
Lakatos2010_SH<-raster("Lakatos2010_SH.tif")
Lakatos2010_SM<-raster("Lakatos2010_SM.tif")
Lakatos2010_SW<-raster("Lakatos2010_SWnet.tif")
Lakatos2010_Ta<-raster("Lakatos2010_Ta.tif")
Lakatos2010_v<-raster("Lakatos2010_v.tif")



Lakatos2012_LW<-raster("Lakatos2012_LWnet.tif")
Lakatos2012_RP<-raster("Lakatos2012_RP.tif")
Lakatos2012_SH<-raster("Lakatos2012_SH.tif")
Lakatos2012_SM<-raster("Lakatos2012_SM.tif")
Lakatos2012_SW<-raster("Lakatos2012_SWnet.tif")
Lakatos2012_Ta<-raster("Lakatos2012_Ta.tif")
Lakatos2012_v<-raster("Lakatos2012_v.tif")



Nainanayake2004_1LW<-raster("Nainanayake2004_LWnet_2004Decto2005Mar.tif")
Nainanayake2004_1RP<-raster("Nainanayake2004_RP_2004Decto2005Mar.tif")
Nainanayake2004_1SH<-raster("Nainanayake2004_SH_2004Decto2005Mar.tif")
Nainanayake2004_1SM<-raster("Nainanayake2004_SM_2004Decto2005Mar.tif")
Nainanayake2004_1SW<-raster("Nainanayake2004_SWnet_2004Decto2005Mar.tif")
Nainanayake2004_1Ta<-raster("Nainanayake2004_Ta_2004Decto2005Mar.tif")
Nainanayake2004_1v<-raster("Nainanayake2004_v_2004Decto2005Mar.tif")
Nainanayake2004_2LW<-raster("Nainanayake2004_LWnet_2004JultoSep.tif")
Nainanayake2004_2RP<-raster("Nainanayake2004_RP_2004JultoSep.tif")
Nainanayake2004_2SH<-raster("Nainanayake2004_SH_2004JultoSep.tif")
Nainanayake2004_2SM<-raster("Nainanayake2004_SM_2004JultoSep.tif")
Nainanayake2004_2SW<-raster("Nainanayake2004_SWnet_2004JultoSep.tif")
Nainanayake2004_2Ta<-raster("Nainanayake2004_Ta_2004JultoSep.tif")
Nainanayake2004_2v<-raster("Nainanayake2004_v_2004JultoSep.tif")
Nainanayake2004_3LW<-raster("Nainanayake2004_LWnet_2005JultoOct.tif")
Nainanayake2004_3RP<-raster("Nainanayake2004_RP_2005JultoOct.tif")
Nainanayake2004_3SH<-raster("Nainanayake2004_SH_2005JultoOct.tif")
Nainanayake2004_3SM<-raster("Nainanayake2004_SM_2005JultoOct.tif")
Nainanayake2004_3SW<-raster("Nainanayake2004_SWnet_2005JultoOct.tif")
Nainanayake2004_3Ta<-raster("Nainanayake2004_Ta_2005JultoOct.tif")
Nainanayake2004_3v<-raster("Nainanayake2004_v_2005JultoOct.tif")




Puma2010_LW<-raster("Puma2010_Global_LWnet.tif")
Puma2010_RP<-raster("Puma2010_Global_RP.tif")
Puma2010_SH<-raster("Puma2010_Global_SH.tif")
Puma2010_SM<-raster("Puma2010_Global_SM.tif")
Puma2010_SW<-raster("Puma2010_Global_SWnet.tif")
Puma2010_Ta<-raster("Puma2010_Global_Ta.tif")
Puma2010_v<-raster("Puma2010_Global_v.tif")


Sacks2009_LW<-raster("Sacks2009_Global_LWnet.tif")
Sacks2009_RP<-raster("Sacks2009_Global_RP.tif")
Sacks2009_SH<-raster("Sacks2009_Global_SH.tif")
Sacks2009_SM<-raster("Sacks2009_Global_SM.tif")
Sacks2009_SW<-raster("Sacks2009_Global_SWnet.tif")
Sacks2009_Ta<-raster("Sacks2009_Global_Ta.tif")
Sacks2009_v<-raster("Sacks2009_Global_v.tif")


Sorooshian2011_LW<-raster("Sorooshian2011_LWnet.tif")
Sorooshian2011_RP<-raster("Sorooshian2011_RP.tif")
Sorooshian2011_SH<-raster("Sorooshian2011_SH.tif")
Sorooshian2011_SM<-raster("Sorooshian2011_SM.tif")
Sorooshian2011_SW<-raster("Sorooshian2011_SWnet.tif")
Sorooshian2011_Ta<-raster("Sorooshian2011_Ta.tif")
Sorooshian2011_v<-raster("Sorooshian2011_v.tif")


Sugimoto2019_LW<-raster("Sugimoto2019_LWnet.tif")
Sugimoto2019_RP<-raster("Sugimoto2019_RP.tif")
Sugimoto2019_SH<-raster("Sugimoto2019_SH.tif")
Sugimoto2019_SM<-raster("Sugimoto2019_SM.tif")
Sugimoto2019_SW<-raster("Sugimoto2019_SWnet.tif")
Sugimoto2019_Ta<-raster("Sugimoto2019_Ta.tif")
Sugimoto2019_v<-raster("Sugimoto2019_v.tif")


Thiery2017_LW<-raster("Thiery2017_Global_LWnet.tif")
Thiery2017_RP<-raster("Thiery2017_Global_RP.tif")
Thiery2017_SH<-raster("Thiery2017_Global_SH.tif")
Thiery2017_SM<-raster("Thiery2017_Global_SM.tif")
Thiery2017_SW<-raster("Thiery2017_Global_SWnet.tif")
Thiery2017_Ta<-raster("Thiery2017_Global_Ta.tif")
Thiery2017_v<-raster("Thiery2017_Global_v.tif")


Thompson1993_LW<-raster("Thompson1993_LWnet.tif")
Thompson1993_RP<-raster("Thompson1993_RP.tif")
Thompson1993_SH<-raster("Thompson1993_SH.tif")
Thompson1993_SM<-raster("Thompson1993_SM.tif")
Thompson1993_SW<-raster("Thompson1993_SWnet.tif")
Thompson1993_Ta<-raster("Thompson1993_Ta.tif")
Thompson1993_v<-raster("Thompson1993_v.tif")


Vahmani2016_LW<-raster("Vahmani2016_LWnet.tif")
Vahmani2016_RP<-raster("Vahmani2016_RP.tif")
Vahmani2016_SH<-raster("Vahmani2016_SH.tif")
Vahmani2016_SM<-raster("Vahmani2016_SM.tif")
Vahmani2016_SW<-raster("Vahmani2016_SWnet.tif")
Vahmani2016_Ta<-raster("Vahmani2016_Ta.tif")
Vahmani2016_v<-raster("Vahmani2016_v.tif")


Wen2012_LW<-raster("Wen2012_LWnet.tif")
Wen2012_RP<-raster("Wen2012_RP.tif")
Wen2012_SH<-raster("Wen2012_SH.tif")
Wen2012_SM<-raster("Wen2012_SM.tif")
Wen2012_SW<-raster("Wen2012_SWnet.tif")
Wen2012_Ta<-raster("Wen2012_Ta.tif")
Wen2012_v<-raster("Wen2012_v.tif")


Yang2015_LW<-raster("Yang2015_LWnet.tif")
Yang2015_RP<-raster("Yang2015_RP.tif")
Yang2015_SH<-raster("Yang2015_SH.tif")
Yang2015_SM<-raster("Yang2015_SM.tif")
Yang2015_SW<-raster("Yang2015_SWnet.tif")
Yang2015_Ta<-raster("Yang2015_Ta.tif")
Yang2015_v<-raster("Yang2015_v.tif")


Yang2016_LW<-raster("Yang2016_LWnet.tif")
Yang2016_RP<-raster("Yang2016_RP.tif")
Yang2016_SH<-raster("Yang2016_SH.tif")
Yang2016_SM<-raster("Yang2016_SM.tif")
Yang2016_SW<-raster("Yang2016_SWnet.tif")
Yang2016_Ta<-raster("Yang2016_Ta.tif")
Yang2016_v<-raster("Yang2016_v.tif")


Yang2017_1LW<-raster("Yang2017_LWnet_2002.tif")
Yang2017_1RP<-raster("Yang2017_RP_2002.tif")
Yang2017_1SH<-raster("Yang2017_SH_2002.tif")
Yang2017_1SM<-raster("Yang2017_SM_2002.tif")
Yang2017_1SW<-raster("Yang2017_SWnet_2002.tif")
Yang2017_1Ta<-raster("Yang2017_Ta_2002.tif")
Yang2017_1v<-raster("Yang2017_v_2002.tif")
Yang2017_2LW<-raster("Yang2017_LWnet_2005to2007.tif")
Yang2017_2RP<-raster("Yang2017_RP_2005to2007.tif")
Yang2017_2SH<-raster("Yang2017_SH_2005to2007.tif")
Yang2017_2SM<-raster("Yang2017_SM_2005to2007.tif")
Yang2017_2SW<-raster("Yang2017_SWnet_2005to2007.tif")
Yang2017_2Ta<-raster("Yang2017_Ta_2005to2007.tif")
Yang2017_2v<-raster("Yang2017_v_2005to2007.tif")
Yang2017_3LW<-raster("Yang2017_LWnet_2010.tif")
Yang2017_3RP<-raster("Yang2017_RP_2010.tif")
Yang2017_3SH<-raster("Yang2017_SH_2010.tif")
Yang2017_3SM<-raster("Yang2017_SM_2010.tif")
Yang2017_3SW<-raster("Yang2017_SWnet_2010.tif")
Yang2017_3Ta<-raster("Yang2017_Ta_2010.tif")
Yang2017_3v<-raster("Yang2017_v_2010.tif")
Yang2017_4LW<-raster("Yang2017_LWnet_2013.tif")
Yang2017_4RP<-raster("Yang2017_RP_2013.tif")
Yang2017_4SH<-raster("Yang2017_SH_2013.tif")
Yang2017_4SM<-raster("Yang2017_SM_2013.tif")
Yang2017_4SW<-raster("Yang2017_SWnet_2013.tif")
Yang2017_4Ta<-raster("Yang2017_Ta_2013.tif")
Yang2017_4v<-raster("Yang2017_v_2013.tif")



Zou2014_LW<-raster("Zou2014_LWnet.tif")
Zou2014_RP<-raster("Zou2014_RP.tif")
Zou2014_SH<-raster("Zou2014_SH.tif")
Zou2014_SM<-raster("Zou2014_SM.tif")
Zou2014_SW<-raster("Zou2014_SWnet.tif")
Zou2014_Ta<-raster("Zou2014_Ta.tif")
Zou2014_v<-raster("Zou2014_v.tif")

Chen2017.2_LW<-raster("Chen2017(2)_YellowRiver_LWnet.tif")
Chen2017.2_RP<-raster("Chen2017(2)_YellowRiver_RP.tif")
Chen2017.2_SH<-raster("Chen2017(2)_YellowRiver_SH.tif")
Chen2017.2_SM<-raster("Chen2017(2)_YellowRiver_SM.tif")
Chen2017.2_SW<-raster("Chen2017(2)_YellowRiver_SWnet.tif")
Chen2017.2_Ta<-raster("Chen2017(2)_YellowRiver_Ta.tif")
Chen2017.2_v<-raster("Chen2017(2)_YellowRiver_v.tif")

Chen2017.3_LW<-raster("Chen2017(3)_YellowRiver_LWnet.tif")
Chen2017.3_RP<-raster("Chen2017(3)_YellowRiver_RP.tif")
Chen2017.3_SH<-raster("Chen2017(3)_YellowRiver_SH.tif")
Chen2017.3_SM<-raster("Chen2017(3)_YellowRiver_SM.tif")
Chen2017.3_SW<-raster("Chen2017(3)_YellowRiver_SWnet.tif")
Chen2017.3_Ta<-raster("Chen2017(3)_YellowRiver_Ta.tif")
Chen2017.3_v<-raster("Chen2017(3)_YellowRiver_v.tif")

Huber2014.2_LW<-raster("Huber2014(2)_LWnet.tif")
Huber2014.2_RP<-raster("Huber2014(2)_RP.tif")
Huber2014.2_SH<-raster("Huber2014(2)_SH.tif")
Huber2014.2_SM<-raster("Huber2014(2)_SM.tif")
Huber2014.2_SW<-raster("Huber2014(2)_SWnet.tif")
Huber2014.2_Ta<-raster("Huber2014(2)_Ta.tif")
Huber2014.2_v<-raster("Huber2014(2)_v.tif")

Valmassoi2020_LW<-raster("Valmassoi2020_LWnet.tif")
Valmassoi2020_RP<-raster("Valmassoi2020_RP.tif")
Valmassoi2020_SH<-raster("Valmassoi2020_SH.tif")
Valmassoi2020_SM<-raster("Valmassoi2020_SM.tif")
Valmassoi2020_SW<-raster("Valmassoi2020_SWnet.tif")
Valmassoi2020_Ta<-raster("Valmassoi2020_Ta.tif")
Valmassoi2020_v<-raster("Valmassoi2020_v.tif")


#Define extent without Antarctica and crop raster by extent for global studies
global_land_extent_no_antar<-extent(-180.3125, 179.8811, -57.8, 90.25)
Cook2014_LW <-crop(Cook2014_LW,global_land_extent_no_antar)
Cook2014_SH <-crop(Cook2014_SH,global_land_extent_no_antar)
Cook2014_SW <-crop(Cook2014_SW,global_land_extent_no_antar)
Cook2014_Ta <-crop(Cook2014_Ta,global_land_extent_no_antar)
Cook2014_v <-crop(Cook2014_v,global_land_extent_no_antar)
Cook2014_RP <-crop(Cook2014_RP,global_land_extent_no_antar)
Cook2014_SM <-crop(Cook2014_SM,global_land_extent_no_antar)

Puma2010_LW <-crop(Puma2010_LW,global_land_extent_no_antar)
Puma2010_SH <-crop(Puma2010_SH,global_land_extent_no_antar)
Puma2010_SW <-crop(Puma2010_SW,global_land_extent_no_antar)
Puma2010_Ta <-crop(Puma2010_Ta,global_land_extent_no_antar)
Puma2010_v <-crop(Puma2010_v,global_land_extent_no_antar)
Puma2010_RP <-crop(Puma2010_RP,global_land_extent_no_antar)
Puma2010_SM <-crop(Puma2010_SM,global_land_extent_no_antar)

Sacks2009_LW <-crop(Sacks2009_LW,global_land_extent_no_antar)
Sacks2009_SH <-crop(Sacks2009_SH,global_land_extent_no_antar)
Sacks2009_SW <-crop(Sacks2009_SW,global_land_extent_no_antar)
Sacks2009_Ta <-crop(Sacks2009_Ta,global_land_extent_no_antar)
Sacks2009_v <-crop(Sacks2009_v,global_land_extent_no_antar)
Sacks2009_RP <-crop(Sacks2009_RP,global_land_extent_no_antar)
Sacks2009_SM <-crop(Sacks2009_SM,global_land_extent_no_antar)

Thiery2017_LW <-crop(Thiery2017_LW,global_land_extent_no_antar)
Thiery2017_SH <-crop(Thiery2017_SH,global_land_extent_no_antar)
Thiery2017_SW <-crop(Thiery2017_SW,global_land_extent_no_antar)
Thiery2017_Ta <-crop(Thiery2017_Ta,global_land_extent_no_antar)
Thiery2017_v <-crop(Thiery2017_v,global_land_extent_no_antar)
Thiery2017_RP <-crop(Thiery2017_RP,global_land_extent_no_antar)
Thiery2017_SM <-crop(Thiery2017_SM,global_land_extent_no_antar)


#Calculate mean of .tif and store in data frame
Giovanni_data<-data.frame(Citation=IC1_raw$Citation,LWnet=999, RP=999,SH=999,SM=999,SWnet=999,Ta=999,v=999)

expand<-c(1,2,1,1,1,1,5,1,1,1,3,1,9,1,2,2,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,4,1,1,1,1,1)

#create an expanded data frame to store data that will be aggregated into the main data frame because some studies have more than 1 set of parameters
Giovanni_data_expanded<-data.frame(Citation=rep(999,times=sum(expand)),LWnet=999,RP=999,SH=999,SM=999,SWnet=999,Ta=999,v=999)

#Name the first column of the expanded data frame for aggregation in the next step
i=1
j=1
k=1
while(i<38){
  Giovanni_data_expanded[k:j,]<-Giovanni_data[rep(i,times=expand[i]),]
  k=j+1
  i=i+1
  j=sum(expand[1:i])
}

#Fill the expanded data frame

Giovanni_data_expanded[1,2]<-cellStats(Adegoke2003_LW,stat="mean")
Giovanni_data_expanded[1,3]<-cellStats(Adegoke2003_RP,stat="mean")
Giovanni_data_expanded[1,4]<-cellStats(Adegoke2003_SH,stat="mean")
Giovanni_data_expanded[1,5]<-cellStats(Adegoke2003_SM,stat="mean")
Giovanni_data_expanded[1,6]<-cellStats(Adegoke2003_SW,stat="mean")
Giovanni_data_expanded[1,7]<-cellStats(Adegoke2003_Ta,stat="mean")
Giovanni_data_expanded[1,8]<-cellStats(Adegoke2003_v,stat="mean")
Giovanni_data_expanded[2,2]<-cellStats(Alter2015_1LW,stat="mean")
Giovanni_data_expanded[2,3]<-cellStats(Alter2015_1RP,stat="mean")
Giovanni_data_expanded[2,4]<-cellStats(Alter2015_1SH,stat="mean")
Giovanni_data_expanded[2,5]<-cellStats(Alter2015_1SM,stat="mean")
Giovanni_data_expanded[2,6]<-cellStats(Alter2015_1SW,stat="mean")
Giovanni_data_expanded[2,7]<-cellStats(Alter2015_1Ta,stat="mean")
Giovanni_data_expanded[2,8]<-cellStats(Alter2015_1v,stat="mean")
Giovanni_data_expanded[3,2]<-cellStats(Alter2015_2LW,stat="mean")
Giovanni_data_expanded[3,3]<-cellStats(Alter2015_2RP,stat="mean")
Giovanni_data_expanded[3,4]<-cellStats(Alter2015_2SH,stat="mean")
Giovanni_data_expanded[3,5]<-cellStats(Alter2015_2SM,stat="mean")
Giovanni_data_expanded[3,6]<-cellStats(Alter2015_2SW,stat="mean")
Giovanni_data_expanded[3,7]<-cellStats(Alter2015_2Ta,stat="mean")
Giovanni_data_expanded[3,8]<-cellStats(Alter2015_2v,stat="mean")
Giovanni_data_expanded[4,2]<-cellStats(Bonan2000_LW,stat="mean")
Giovanni_data_expanded[4,3]<-cellStats(Bonan2000_RP,stat="mean")
Giovanni_data_expanded[4,4]<-cellStats(Bonan2000_SH,stat="mean")
Giovanni_data_expanded[4,5]<-cellStats(Bonan2000_SM,stat="mean")
Giovanni_data_expanded[4,6]<-cellStats(Bonan2000_SW,stat="mean")
Giovanni_data_expanded[4,7]<-cellStats(Bonan2000_Ta,stat="mean")
Giovanni_data_expanded[4,8]<-cellStats(Bonan2000_v,stat="mean")
Giovanni_data_expanded[5,2]<-cellStats(Broadbent2018_LW,stat="mean")
Giovanni_data_expanded[5,3]<-cellStats(Broadbent2018_RP,stat="mean")
Giovanni_data_expanded[5,4]<-cellStats(Broadbent2018_SH,stat="mean")
Giovanni_data_expanded[5,5]<-cellStats(Broadbent2018_SM,stat="mean")
Giovanni_data_expanded[5,6]<-cellStats(Broadbent2018_SW,stat="mean")
Giovanni_data_expanded[5,7]<-cellStats(Broadbent2018_Ta,stat="mean")
Giovanni_data_expanded[5,8]<-cellStats(Broadbent2018_v,stat="mean")
Giovanni_data_expanded[6,2]<-cellStats(Broadbent2019_LW,stat="mean")
Giovanni_data_expanded[6,3]<-cellStats(Broadbent2019_RP,stat="mean")
Giovanni_data_expanded[6,4]<-cellStats(Broadbent2019_SH,stat="mean")
Giovanni_data_expanded[6,5]<-cellStats(Broadbent2019_SM,stat="mean")
Giovanni_data_expanded[6,6]<-cellStats(Broadbent2019_SW,stat="mean")
Giovanni_data_expanded[6,7]<-cellStats(Broadbent2019_Ta,stat="mean")
Giovanni_data_expanded[6,8]<-cellStats(Broadbent2019_v,stat="mean")
Giovanni_data_expanded[7,2]<-cellStats(Chen2017_LW,stat="mean")
Giovanni_data_expanded[7,3]<-cellStats(Chen2017_RP,stat="mean")
Giovanni_data_expanded[7,4]<-cellStats(Chen2017_SH,stat="mean")
Giovanni_data_expanded[7,5]<-cellStats(Chen2017_SM,stat="mean")
Giovanni_data_expanded[7,6]<-cellStats(Chen2017_SW,stat="mean")
Giovanni_data_expanded[7,7]<-cellStats(Chen2017_Ta,stat="mean")
Giovanni_data_expanded[7,8]<-cellStats(Chen2017_v,stat="mean")
Giovanni_data_expanded[8,2]<-cellStats(Chen2018_1LW,stat="mean")
Giovanni_data_expanded[8,3]<-cellStats(Chen2018_1RP,stat="mean")
Giovanni_data_expanded[8,4]<-cellStats(Chen2018_1SH,stat="mean")
Giovanni_data_expanded[8,5]<-cellStats(Chen2018_1SM,stat="mean")
Giovanni_data_expanded[8,6]<-cellStats(Chen2018_1SW,stat="mean")
Giovanni_data_expanded[8,7]<-cellStats(Chen2018_1Ta,stat="mean")
Giovanni_data_expanded[8,8]<-cellStats(Chen2018_1v,stat="mean")
Giovanni_data_expanded[9,2]<-cellStats(Chen2018_2LW,stat="mean")
Giovanni_data_expanded[9,3]<-cellStats(Chen2018_2RP,stat="mean")
Giovanni_data_expanded[9,4]<-cellStats(Chen2018_2SH,stat="mean")
Giovanni_data_expanded[9,5]<-cellStats(Chen2018_2SM,stat="mean")
Giovanni_data_expanded[9,6]<-cellStats(Chen2018_2SW,stat="mean")
Giovanni_data_expanded[9,7]<-cellStats(Chen2018_2Ta,stat="mean")
Giovanni_data_expanded[9,8]<-cellStats(Chen2018_2v,stat="mean")
Giovanni_data_expanded[10,2]<-cellStats(Chen2018_3LW,stat="mean")
Giovanni_data_expanded[10,3]<-cellStats(Chen2018_3RP,stat="mean")
Giovanni_data_expanded[10,4]<-cellStats(Chen2018_3SH,stat="mean")
Giovanni_data_expanded[10,5]<-cellStats(Chen2018_3SM,stat="mean")
Giovanni_data_expanded[10,6]<-cellStats(Chen2018_3SW,stat="mean")
Giovanni_data_expanded[10,7]<-cellStats(Chen2018_3Ta,stat="mean")
Giovanni_data_expanded[10,8]<-cellStats(Chen2018_3v,stat="mean")
Giovanni_data_expanded[11,2]<-cellStats(Chen2018_4LW,stat="mean")
Giovanni_data_expanded[11,3]<-cellStats(Chen2018_4RP,stat="mean")
Giovanni_data_expanded[11,4]<-cellStats(Chen2018_4SH,stat="mean")
Giovanni_data_expanded[11,5]<-cellStats(Chen2018_4SM,stat="mean")
Giovanni_data_expanded[11,6]<-cellStats(Chen2018_4SW,stat="mean")
Giovanni_data_expanded[11,7]<-cellStats(Chen2018_4Ta,stat="mean")
Giovanni_data_expanded[11,8]<-cellStats(Chen2018_4v,stat="mean")
Giovanni_data_expanded[12,2]<-cellStats(Chen2018_5LW,stat="mean")
Giovanni_data_expanded[12,3]<-cellStats(Chen2018_5RP,stat="mean")
Giovanni_data_expanded[12,4]<-cellStats(Chen2018_5SH,stat="mean")
Giovanni_data_expanded[12,5]<-cellStats(Chen2018_5SM,stat="mean")
Giovanni_data_expanded[12,6]<-cellStats(Chen2018_5SW,stat="mean")
Giovanni_data_expanded[12,7]<-cellStats(Chen2018_5Ta,stat="mean")
Giovanni_data_expanded[12,8]<-cellStats(Chen2018_5v,stat="mean")
Giovanni_data_expanded[13,2]<-cellStats(Cook2014_LW,stat="mean")
Giovanni_data_expanded[13,3]<-cellStats(Cook2014_RP,stat="mean")
Giovanni_data_expanded[13,4]<-cellStats(Cook2014_SH,stat="mean")
Giovanni_data_expanded[13,5]<-cellStats(Cook2014_SM,stat="mean")
Giovanni_data_expanded[13,6]<-cellStats(Cook2014_SW,stat="mean")
Giovanni_data_expanded[13,7]<-cellStats(Cook2014_Ta,stat="mean")
Giovanni_data_expanded[13,8]<-cellStats(Cook2014_v,stat="mean")
Giovanni_data_expanded[14,2]<-cellStats(Daniel2018_LW,stat="mean")
Giovanni_data_expanded[14,3]<-cellStats(Daniel2018_RP,stat="mean")
Giovanni_data_expanded[14,4]<-cellStats(Daniel2018_SH,stat="mean")
Giovanni_data_expanded[14,5]<-cellStats(Daniel2018_SM,stat="mean")
Giovanni_data_expanded[14,6]<-cellStats(Daniel2018_SW,stat="mean")
Giovanni_data_expanded[14,7]<-cellStats(Daniel2018_Ta,stat="mean")
Giovanni_data_expanded[14,8]<-cellStats(Daniel2018_v,stat="mean")
Giovanni_data_expanded[15,2]<-cellStats(Geerts2006_LW,stat="mean")
Giovanni_data_expanded[15,3]<-cellStats(Geerts2006_RP,stat="mean")
Giovanni_data_expanded[15,4]<-cellStats(Geerts2006_SH,stat="mean")
Giovanni_data_expanded[15,5]<-cellStats(Geerts2006_SM,stat="mean")
Giovanni_data_expanded[15,6]<-cellStats(Geerts2006_SW,stat="mean")
Giovanni_data_expanded[15,7]<-cellStats(Geerts2006_Ta,stat="mean")
Giovanni_data_expanded[15,8]<-cellStats(Geerts2006_v,stat="mean")
Giovanni_data_expanded[16,2]<-cellStats(Grossman_Clarke2010_1LW,stat="mean")
Giovanni_data_expanded[16,3]<-cellStats(Grossman_Clarke2010_1RP,stat="mean")
Giovanni_data_expanded[16,4]<-cellStats(Grossman_Clarke2010_1SH,stat="mean")
Giovanni_data_expanded[16,5]<-cellStats(Grossman_Clarke2010_1SM,stat="mean")
Giovanni_data_expanded[16,6]<-cellStats(Grossman_Clarke2010_1SW,stat="mean")
Giovanni_data_expanded[16,7]<-cellStats(Grossman_Clarke2010_1Ta,stat="mean")
Giovanni_data_expanded[16,8]<-cellStats(Grossman_Clarke2010_1v,stat="mean")
Giovanni_data_expanded[17,2]<-cellStats(Grossman_Clarke2010_2LW,stat="mean")
Giovanni_data_expanded[17,3]<-cellStats(Grossman_Clarke2010_2RP,stat="mean")
Giovanni_data_expanded[17,4]<-cellStats(Grossman_Clarke2010_2SH,stat="mean")
Giovanni_data_expanded[17,5]<-cellStats(Grossman_Clarke2010_2SM,stat="mean")
Giovanni_data_expanded[17,6]<-cellStats(Grossman_Clarke2010_2SW,stat="mean")
Giovanni_data_expanded[17,7]<-cellStats(Grossman_Clarke2010_2Ta,stat="mean")
Giovanni_data_expanded[17,8]<-cellStats(Grossman_Clarke2010_2v,stat="mean")
Giovanni_data_expanded[18,2]<-cellStats(Grossman_Clarke2010_3LW,stat="mean")
Giovanni_data_expanded[18,3]<-cellStats(Grossman_Clarke2010_3RP,stat="mean")
Giovanni_data_expanded[18,4]<-cellStats(Grossman_Clarke2010_3SH,stat="mean")
Giovanni_data_expanded[18,5]<-cellStats(Grossman_Clarke2010_3SM,stat="mean")
Giovanni_data_expanded[18,6]<-cellStats(Grossman_Clarke2010_3SW,stat="mean")
Giovanni_data_expanded[18,7]<-cellStats(Grossman_Clarke2010_3Ta,stat="mean")
Giovanni_data_expanded[18,8]<-cellStats(Grossman_Clarke2010_3v,stat="mean")
Giovanni_data_expanded[19,2]<-cellStats(Hancock2015_LW,stat="mean")
Giovanni_data_expanded[19,3]<-cellStats(Hancock2015_RP,stat="mean")
Giovanni_data_expanded[19,4]<-cellStats(Hancock2015_SH,stat="mean")
Giovanni_data_expanded[19,5]<-cellStats(Hancock2015_SM,stat="mean")
Giovanni_data_expanded[19,6]<-cellStats(Hancock2015_SW,stat="mean")
Giovanni_data_expanded[19,7]<-cellStats(Hancock2015_Ta,stat="mean")
Giovanni_data_expanded[19,8]<-cellStats(Hancock2015_v,stat="mean")
Giovanni_data_expanded[20,2]<-cellStats(Harding2012_1LW,stat="mean")
Giovanni_data_expanded[20,3]<-cellStats(Harding2012_1RP,stat="mean")
Giovanni_data_expanded[20,4]<-cellStats(Harding2012_1SH,stat="mean")
Giovanni_data_expanded[20,5]<-cellStats(Harding2012_1SM,stat="mean")
Giovanni_data_expanded[20,6]<-cellStats(Harding2012_1SW,stat="mean")
Giovanni_data_expanded[20,7]<-cellStats(Harding2012_1Ta,stat="mean")
Giovanni_data_expanded[20,8]<-cellStats(Harding2012_1v,stat="mean")
Giovanni_data_expanded[21,2]<-cellStats(Harding2012_2LW,stat="mean")
Giovanni_data_expanded[21,3]<-cellStats(Harding2012_2RP,stat="mean")
Giovanni_data_expanded[21,4]<-cellStats(Harding2012_2SH,stat="mean")
Giovanni_data_expanded[21,5]<-cellStats(Harding2012_2SM,stat="mean")
Giovanni_data_expanded[21,6]<-cellStats(Harding2012_2SW,stat="mean")
Giovanni_data_expanded[21,7]<-cellStats(Harding2012_2Ta,stat="mean")
Giovanni_data_expanded[21,8]<-cellStats(Harding2012_2v,stat="mean")
Giovanni_data_expanded[22,2]<-cellStats(Harding2012_3LW,stat="mean")
Giovanni_data_expanded[22,3]<-cellStats(Harding2012_3RP,stat="mean")
Giovanni_data_expanded[22,4]<-cellStats(Harding2012_3SH,stat="mean")
Giovanni_data_expanded[22,5]<-cellStats(Harding2012_3SM,stat="mean")
Giovanni_data_expanded[22,6]<-cellStats(Harding2012_3SW,stat="mean")
Giovanni_data_expanded[22,7]<-cellStats(Harding2012_3Ta,stat="mean")
Giovanni_data_expanded[22,8]<-cellStats(Harding2012_3v,stat="mean")
Giovanni_data_expanded[23,2]<-cellStats(Harding2012_4LW,stat="mean")
Giovanni_data_expanded[23,3]<-cellStats(Harding2012_4RP,stat="mean")
Giovanni_data_expanded[23,4]<-cellStats(Harding2012_4SH,stat="mean")
Giovanni_data_expanded[23,5]<-cellStats(Harding2012_4SM,stat="mean")
Giovanni_data_expanded[23,6]<-cellStats(Harding2012_4SW,stat="mean")
Giovanni_data_expanded[23,7]<-cellStats(Harding2012_4Ta,stat="mean")
Giovanni_data_expanded[23,8]<-cellStats(Harding2012_4v,stat="mean")
Giovanni_data_expanded[24,2]<-cellStats(Harding2012_5LW,stat="mean")
Giovanni_data_expanded[24,3]<-cellStats(Harding2012_5RP,stat="mean")
Giovanni_data_expanded[24,4]<-cellStats(Harding2012_5SH,stat="mean")
Giovanni_data_expanded[24,5]<-cellStats(Harding2012_5SM,stat="mean")
Giovanni_data_expanded[24,6]<-cellStats(Harding2012_5SW,stat="mean")
Giovanni_data_expanded[24,7]<-cellStats(Harding2012_5Ta,stat="mean")
Giovanni_data_expanded[24,8]<-cellStats(Harding2012_5v,stat="mean")
Giovanni_data_expanded[25,2]<-cellStats(Harding2012_6LW,stat="mean")
Giovanni_data_expanded[25,3]<-cellStats(Harding2012_6RP,stat="mean")
Giovanni_data_expanded[25,4]<-cellStats(Harding2012_6SH,stat="mean")
Giovanni_data_expanded[25,5]<-cellStats(Harding2012_6SM,stat="mean")
Giovanni_data_expanded[25,6]<-cellStats(Harding2012_6SW,stat="mean")
Giovanni_data_expanded[25,7]<-cellStats(Harding2012_6Ta,stat="mean")
Giovanni_data_expanded[25,8]<-cellStats(Harding2012_6v,stat="mean")
Giovanni_data_expanded[26,2]<-cellStats(Harding2012_7LW,stat="mean")
Giovanni_data_expanded[26,3]<-cellStats(Harding2012_7RP,stat="mean")
Giovanni_data_expanded[26,4]<-cellStats(Harding2012_7SH,stat="mean")
Giovanni_data_expanded[26,5]<-cellStats(Harding2012_7SM,stat="mean")
Giovanni_data_expanded[26,6]<-cellStats(Harding2012_7SW,stat="mean")
Giovanni_data_expanded[26,7]<-cellStats(Harding2012_7Ta,stat="mean")
Giovanni_data_expanded[26,8]<-cellStats(Harding2012_7v,stat="mean")
Giovanni_data_expanded[27,2]<-cellStats(Harding2012_8LW,stat="mean")
Giovanni_data_expanded[27,3]<-cellStats(Harding2012_8RP,stat="mean")
Giovanni_data_expanded[27,4]<-cellStats(Harding2012_8SH,stat="mean")
Giovanni_data_expanded[27,5]<-cellStats(Harding2012_8SM,stat="mean")
Giovanni_data_expanded[27,6]<-cellStats(Harding2012_8SW,stat="mean")
Giovanni_data_expanded[27,7]<-cellStats(Harding2012_8Ta,stat="mean")
Giovanni_data_expanded[27,8]<-cellStats(Harding2012_8v,stat="mean")
Giovanni_data_expanded[28,2]<-cellStats(Harding2012_9LW,stat="mean")
Giovanni_data_expanded[28,3]<-cellStats(Harding2012_9RP,stat="mean")
Giovanni_data_expanded[28,4]<-cellStats(Harding2012_9SH,stat="mean")
Giovanni_data_expanded[28,5]<-cellStats(Harding2012_9SM,stat="mean")
Giovanni_data_expanded[28,6]<-cellStats(Harding2012_9SW,stat="mean")
Giovanni_data_expanded[28,7]<-cellStats(Harding2012_9Ta,stat="mean")
Giovanni_data_expanded[28,8]<-cellStats(Harding2012_9v,stat="mean")
Giovanni_data_expanded[29,2]<-cellStats(Huber2014_LW,stat="mean")
Giovanni_data_expanded[29,3]<-cellStats(Huber2014_RP,stat="mean")
Giovanni_data_expanded[29,4]<-cellStats(Huber2014_SH,stat="mean")
Giovanni_data_expanded[29,5]<-cellStats(Huber2014_SM,stat="mean")
Giovanni_data_expanded[29,6]<-cellStats(Huber2014_SW,stat="mean")
Giovanni_data_expanded[29,7]<-cellStats(Huber2014_Ta,stat="mean")
Giovanni_data_expanded[29,8]<-cellStats(Huber2014_v,stat="mean")
Giovanni_data_expanded[30,2]<-cellStats(Iglesias2002_1LW,stat="mean")
Giovanni_data_expanded[30,3]<-cellStats(Iglesias2002_1RP,stat="mean")
Giovanni_data_expanded[30,4]<-cellStats(Iglesias2002_1SH,stat="mean")
Giovanni_data_expanded[30,5]<-cellStats(Iglesias2002_1SM,stat="mean")
Giovanni_data_expanded[30,6]<-cellStats(Iglesias2002_1SW,stat="mean")
Giovanni_data_expanded[30,7]<-cellStats(Iglesias2002_1Ta,stat="mean")
Giovanni_data_expanded[30,8]<-cellStats(Iglesias2002_1v,stat="mean")
Giovanni_data_expanded[31,2]<-cellStats(Iglesias2002_2LW,stat="mean")
Giovanni_data_expanded[31,3]<-cellStats(Iglesias2002_2RP,stat="mean")
Giovanni_data_expanded[31,4]<-cellStats(Iglesias2002_2SH,stat="mean")
Giovanni_data_expanded[31,5]<-cellStats(Iglesias2002_2SM,stat="mean")
Giovanni_data_expanded[31,6]<-cellStats(Iglesias2002_2SW,stat="mean")
Giovanni_data_expanded[31,7]<-cellStats(Iglesias2002_2Ta,stat="mean")
Giovanni_data_expanded[31,8]<-cellStats(Iglesias2002_2v,stat="mean")
Giovanni_data_expanded[32,2]<-cellStats(Iglesias2005_1LW,stat="mean")
Giovanni_data_expanded[32,3]<-cellStats(Iglesias2005_1RP,stat="mean")
Giovanni_data_expanded[32,4]<-cellStats(Iglesias2005_1SH,stat="mean")
Giovanni_data_expanded[32,5]<-cellStats(Iglesias2005_1SM,stat="mean")
Giovanni_data_expanded[32,6]<-cellStats(Iglesias2005_1SW,stat="mean")
Giovanni_data_expanded[32,7]<-cellStats(Iglesias2005_1Ta,stat="mean")
Giovanni_data_expanded[32,8]<-cellStats(Iglesias2005_1v,stat="mean")
Giovanni_data_expanded[33,2]<-cellStats(Iglesias2005_2LW,stat="mean")
Giovanni_data_expanded[33,3]<-cellStats(Iglesias2005_2RP,stat="mean")
Giovanni_data_expanded[33,4]<-cellStats(Iglesias2005_2SH,stat="mean")
Giovanni_data_expanded[33,5]<-cellStats(Iglesias2005_2SM,stat="mean")
Giovanni_data_expanded[33,6]<-cellStats(Iglesias2005_2SW,stat="mean")
Giovanni_data_expanded[33,7]<-cellStats(Iglesias2005_2Ta,stat="mean")
Giovanni_data_expanded[33,8]<-cellStats(Iglesias2005_2v,stat="mean")
Giovanni_data_expanded[34,2]<-cellStats(Kanamaru2008_LW,stat="mean")
Giovanni_data_expanded[34,3]<-cellStats(Kanamaru2008_RP,stat="mean")
Giovanni_data_expanded[34,4]<-cellStats(Kanamaru2008_SH,stat="mean")
Giovanni_data_expanded[34,5]<-cellStats(Kanamaru2008_SM,stat="mean")
Giovanni_data_expanded[34,6]<-cellStats(Kanamaru2008_SW,stat="mean")
Giovanni_data_expanded[34,7]<-cellStats(Kanamaru2008_Ta,stat="mean")
Giovanni_data_expanded[34,8]<-cellStats(Kanamaru2008_v,stat="mean")
Giovanni_data_expanded[35,2]<-cellStats(Kohl1974_LW,stat="mean")
Giovanni_data_expanded[35,3]<-cellStats(Kohl1974_RP,stat="mean")
Giovanni_data_expanded[35,4]<-cellStats(Kohl1974_SH,stat="mean")
Giovanni_data_expanded[35,5]<-cellStats(Kohl1974_SM,stat="mean")
Giovanni_data_expanded[35,6]<-cellStats(Kohl1974_SW,stat="mean")
Giovanni_data_expanded[35,7]<-cellStats(Kohl1974_Ta,stat="mean")
Giovanni_data_expanded[35,8]<-cellStats(Kohl1974_v,stat="mean")
Giovanni_data_expanded[36,2]<-cellStats(Lakatos2010_LW,stat="mean")
Giovanni_data_expanded[36,3]<-cellStats(Lakatos2010_RP,stat="mean")
Giovanni_data_expanded[36,4]<-cellStats(Lakatos2010_SH,stat="mean")
Giovanni_data_expanded[36,5]<-cellStats(Lakatos2010_SM,stat="mean")
Giovanni_data_expanded[36,6]<-cellStats(Lakatos2010_SW,stat="mean")
Giovanni_data_expanded[36,7]<-cellStats(Lakatos2010_Ta,stat="mean")
Giovanni_data_expanded[36,8]<-cellStats(Lakatos2010_v,stat="mean")
Giovanni_data_expanded[37,2]<-cellStats(Lakatos2012_LW,stat="mean")
Giovanni_data_expanded[37,3]<-cellStats(Lakatos2012_RP,stat="mean")
Giovanni_data_expanded[37,4]<-cellStats(Lakatos2012_SH,stat="mean")
Giovanni_data_expanded[37,5]<-cellStats(Lakatos2012_SM,stat="mean")
Giovanni_data_expanded[37,6]<-cellStats(Lakatos2012_SW,stat="mean")
Giovanni_data_expanded[37,7]<-cellStats(Lakatos2012_Ta,stat="mean")
Giovanni_data_expanded[37,8]<-cellStats(Lakatos2012_v,stat="mean")
Giovanni_data_expanded[38,2]<-cellStats(Nainanayake2004_1LW,stat="mean")
Giovanni_data_expanded[38,3]<-cellStats(Nainanayake2004_1RP,stat="mean")
Giovanni_data_expanded[38,4]<-cellStats(Nainanayake2004_1SH,stat="mean")
Giovanni_data_expanded[38,5]<-cellStats(Nainanayake2004_1SM,stat="mean")
Giovanni_data_expanded[38,6]<-cellStats(Nainanayake2004_1SW,stat="mean")
Giovanni_data_expanded[38,7]<-cellStats(Nainanayake2004_1Ta,stat="mean")
Giovanni_data_expanded[38,8]<-cellStats(Nainanayake2004_1v,stat="mean")
Giovanni_data_expanded[39,2]<-cellStats(Nainanayake2004_2LW,stat="mean")
Giovanni_data_expanded[39,3]<-cellStats(Nainanayake2004_2RP,stat="mean")
Giovanni_data_expanded[39,4]<-cellStats(Nainanayake2004_2SH,stat="mean")
Giovanni_data_expanded[39,5]<-cellStats(Nainanayake2004_2SM,stat="mean")
Giovanni_data_expanded[39,6]<-cellStats(Nainanayake2004_2SW,stat="mean")
Giovanni_data_expanded[39,7]<-cellStats(Nainanayake2004_2Ta,stat="mean")
Giovanni_data_expanded[39,8]<-cellStats(Nainanayake2004_2v,stat="mean")
Giovanni_data_expanded[40,2]<-cellStats(Nainanayake2004_3LW,stat="mean")
Giovanni_data_expanded[40,3]<-cellStats(Nainanayake2004_3RP,stat="mean")
Giovanni_data_expanded[40,4]<-cellStats(Nainanayake2004_3SH,stat="mean")
Giovanni_data_expanded[40,5]<-cellStats(Nainanayake2004_3SM,stat="mean")
Giovanni_data_expanded[40,6]<-cellStats(Nainanayake2004_3SW,stat="mean")
Giovanni_data_expanded[40,7]<-cellStats(Nainanayake2004_3Ta,stat="mean")
Giovanni_data_expanded[40,8]<-cellStats(Nainanayake2004_3v,stat="mean")
Giovanni_data_expanded[41,2]<-cellStats(Puma2010_LW,stat="mean")
Giovanni_data_expanded[41,3]<-cellStats(Puma2010_RP,stat="mean")
Giovanni_data_expanded[41,4]<-cellStats(Puma2010_SH,stat="mean")
Giovanni_data_expanded[41,5]<-cellStats(Puma2010_SM,stat="mean")
Giovanni_data_expanded[41,6]<-cellStats(Puma2010_SW,stat="mean")
Giovanni_data_expanded[41,7]<-cellStats(Puma2010_Ta,stat="mean")
Giovanni_data_expanded[41,8]<-cellStats(Puma2010_v,stat="mean")
Giovanni_data_expanded[42,2]<-cellStats(Sacks2009_LW,stat="mean")
Giovanni_data_expanded[42,3]<-cellStats(Sacks2009_RP,stat="mean")
Giovanni_data_expanded[42,4]<-cellStats(Sacks2009_SH,stat="mean")
Giovanni_data_expanded[42,5]<-cellStats(Sacks2009_SM,stat="mean")
Giovanni_data_expanded[42,6]<-cellStats(Sacks2009_SW,stat="mean")
Giovanni_data_expanded[42,7]<-cellStats(Sacks2009_Ta,stat="mean")
Giovanni_data_expanded[42,8]<-cellStats(Sacks2009_v,stat="mean")
Giovanni_data_expanded[43,2]<-cellStats(Sorooshian2011_LW,stat="mean")
Giovanni_data_expanded[43,3]<-cellStats(Sorooshian2011_RP,stat="mean")
Giovanni_data_expanded[43,4]<-cellStats(Sorooshian2011_SH,stat="mean")
Giovanni_data_expanded[43,5]<-cellStats(Sorooshian2011_SM,stat="mean")
Giovanni_data_expanded[43,6]<-cellStats(Sorooshian2011_SW,stat="mean")
Giovanni_data_expanded[43,7]<-cellStats(Sorooshian2011_Ta,stat="mean")
Giovanni_data_expanded[43,8]<-cellStats(Sorooshian2011_v,stat="mean")
Giovanni_data_expanded[44,2]<-cellStats(Sugimoto2019_LW,stat="mean")
Giovanni_data_expanded[44,3]<-cellStats(Sugimoto2019_RP,stat="mean")
Giovanni_data_expanded[44,4]<-cellStats(Sugimoto2019_SH,stat="mean")
Giovanni_data_expanded[44,5]<-cellStats(Sugimoto2019_SM,stat="mean")
Giovanni_data_expanded[44,6]<-cellStats(Sugimoto2019_SW,stat="mean")
Giovanni_data_expanded[44,7]<-cellStats(Sugimoto2019_Ta,stat="mean")
Giovanni_data_expanded[44,8]<-cellStats(Sugimoto2019_v,stat="mean")
Giovanni_data_expanded[45,2]<-cellStats(Thiery2017_LW,stat="mean")
Giovanni_data_expanded[45,3]<-cellStats(Thiery2017_RP,stat="mean")
Giovanni_data_expanded[45,4]<-cellStats(Thiery2017_SH,stat="mean")
Giovanni_data_expanded[45,5]<-cellStats(Thiery2017_SM,stat="mean")
Giovanni_data_expanded[45,6]<-cellStats(Thiery2017_SW,stat="mean")
Giovanni_data_expanded[45,7]<-cellStats(Thiery2017_Ta,stat="mean")
Giovanni_data_expanded[45,8]<-cellStats(Thiery2017_v,stat="mean")
Giovanni_data_expanded[46,2]<-cellStats(Thompson1993_LW,stat="mean")
Giovanni_data_expanded[46,3]<-cellStats(Thompson1993_RP,stat="mean")
Giovanni_data_expanded[46,4]<-cellStats(Thompson1993_SH,stat="mean")
Giovanni_data_expanded[46,5]<-cellStats(Thompson1993_SM,stat="mean")
Giovanni_data_expanded[46,6]<-cellStats(Thompson1993_SW,stat="mean")
Giovanni_data_expanded[46,7]<-cellStats(Thompson1993_Ta,stat="mean")
Giovanni_data_expanded[46,8]<-cellStats(Thompson1993_v,stat="mean")
Giovanni_data_expanded[47,2]<-cellStats(Vahmani2016_LW,stat="mean")
Giovanni_data_expanded[47,3]<-cellStats(Vahmani2016_RP,stat="mean")
Giovanni_data_expanded[47,4]<-cellStats(Vahmani2016_SH,stat="mean")
Giovanni_data_expanded[47,5]<-cellStats(Vahmani2016_SM,stat="mean")
Giovanni_data_expanded[47,6]<-cellStats(Vahmani2016_SW,stat="mean")
Giovanni_data_expanded[47,7]<-cellStats(Vahmani2016_Ta,stat="mean")
Giovanni_data_expanded[47,8]<-cellStats(Vahmani2016_v,stat="mean")
Giovanni_data_expanded[48,2]<-cellStats(Wen2012_LW,stat="mean")
Giovanni_data_expanded[48,3]<-cellStats(Wen2012_RP,stat="mean")
Giovanni_data_expanded[48,4]<-cellStats(Wen2012_SH,stat="mean")
Giovanni_data_expanded[48,5]<-cellStats(Wen2012_SM,stat="mean")
Giovanni_data_expanded[48,6]<-cellStats(Wen2012_SW,stat="mean")
Giovanni_data_expanded[48,7]<-cellStats(Wen2012_Ta,stat="mean")
Giovanni_data_expanded[48,8]<-cellStats(Wen2012_v,stat="mean")
Giovanni_data_expanded[49,2]<-cellStats(Yang2015_LW,stat="mean")
Giovanni_data_expanded[49,3]<-cellStats(Yang2015_RP,stat="mean")
Giovanni_data_expanded[49,4]<-cellStats(Yang2015_SH,stat="mean")
Giovanni_data_expanded[49,5]<-cellStats(Yang2015_SM,stat="mean")
Giovanni_data_expanded[49,6]<-cellStats(Yang2015_SW,stat="mean")
Giovanni_data_expanded[49,7]<-cellStats(Yang2015_Ta,stat="mean")
Giovanni_data_expanded[49,8]<-cellStats(Yang2015_v,stat="mean")
Giovanni_data_expanded[50,2]<-cellStats(Yang2016_LW,stat="mean")
Giovanni_data_expanded[50,3]<-cellStats(Yang2016_RP,stat="mean")
Giovanni_data_expanded[50,4]<-cellStats(Yang2016_SH,stat="mean")
Giovanni_data_expanded[50,5]<-cellStats(Yang2016_SM,stat="mean")
Giovanni_data_expanded[50,6]<-cellStats(Yang2016_SW,stat="mean")
Giovanni_data_expanded[50,7]<-cellStats(Yang2016_Ta,stat="mean")
Giovanni_data_expanded[50,8]<-cellStats(Yang2016_v,stat="mean")
Giovanni_data_expanded[51,2]<-cellStats(Yang2017_1LW,stat="mean")
Giovanni_data_expanded[51,3]<-cellStats(Yang2017_1RP,stat="mean")
Giovanni_data_expanded[51,4]<-cellStats(Yang2017_1SH,stat="mean")
Giovanni_data_expanded[51,5]<-cellStats(Yang2017_1SM,stat="mean")
Giovanni_data_expanded[51,6]<-cellStats(Yang2017_1SW,stat="mean")
Giovanni_data_expanded[51,7]<-cellStats(Yang2017_1Ta,stat="mean")
Giovanni_data_expanded[51,8]<-cellStats(Yang2017_1v,stat="mean")
Giovanni_data_expanded[52,2]<-cellStats(Yang2017_2LW,stat="mean")
Giovanni_data_expanded[52,3]<-cellStats(Yang2017_2RP,stat="mean")
Giovanni_data_expanded[52,4]<-cellStats(Yang2017_2SH,stat="mean")
Giovanni_data_expanded[52,5]<-cellStats(Yang2017_2SM,stat="mean")
Giovanni_data_expanded[52,6]<-cellStats(Yang2017_2SW,stat="mean")
Giovanni_data_expanded[52,7]<-cellStats(Yang2017_2Ta,stat="mean")
Giovanni_data_expanded[52,8]<-cellStats(Yang2017_2v,stat="mean")
Giovanni_data_expanded[53,2]<-cellStats(Yang2017_3LW,stat="mean")
Giovanni_data_expanded[53,3]<-cellStats(Yang2017_3RP,stat="mean")
Giovanni_data_expanded[53,4]<-cellStats(Yang2017_3SH,stat="mean")
Giovanni_data_expanded[53,5]<-cellStats(Yang2017_3SM,stat="mean")
Giovanni_data_expanded[53,6]<-cellStats(Yang2017_3SW,stat="mean")
Giovanni_data_expanded[53,7]<-cellStats(Yang2017_3Ta,stat="mean")
Giovanni_data_expanded[53,8]<-cellStats(Yang2017_3v,stat="mean")
Giovanni_data_expanded[54,2]<-cellStats(Yang2017_4LW,stat="mean")
Giovanni_data_expanded[54,3]<-cellStats(Yang2017_4RP,stat="mean")
Giovanni_data_expanded[54,4]<-cellStats(Yang2017_4SH,stat="mean")
Giovanni_data_expanded[54,5]<-cellStats(Yang2017_4SM,stat="mean")
Giovanni_data_expanded[54,6]<-cellStats(Yang2017_4SW,stat="mean")
Giovanni_data_expanded[54,7]<-cellStats(Yang2017_4Ta,stat="mean")
Giovanni_data_expanded[54,8]<-cellStats(Yang2017_4v,stat="mean")
Giovanni_data_expanded[55,2]<-cellStats(Zou2014_LW,stat="mean")
Giovanni_data_expanded[55,3]<-cellStats(Zou2014_RP,stat="mean")
Giovanni_data_expanded[55,4]<-cellStats(Zou2014_SH,stat="mean")
Giovanni_data_expanded[55,5]<-cellStats(Zou2014_SM,stat="mean")
Giovanni_data_expanded[55,6]<-cellStats(Zou2014_SW,stat="mean")
Giovanni_data_expanded[55,7]<-cellStats(Zou2014_Ta,stat="mean")
Giovanni_data_expanded[55,8]<-cellStats(Zou2014_v,stat="mean")
Giovanni_data_expanded[56,2]<-cellStats(Chen2017.2_LW,stat="mean")
Giovanni_data_expanded[56,3]<-cellStats(Chen2017.2_RP,stat="mean")
Giovanni_data_expanded[56,4]<-cellStats(Chen2017.2_SH,stat="mean")
Giovanni_data_expanded[56,5]<-cellStats(Chen2017.2_SM,stat="mean")
Giovanni_data_expanded[56,6]<-cellStats(Chen2017.2_SW,stat="mean")
Giovanni_data_expanded[56,7]<-cellStats(Chen2017.2_Ta,stat="mean")
Giovanni_data_expanded[56,8]<-cellStats(Chen2017.2_v,stat="mean")
Giovanni_data_expanded[57,2]<-cellStats(Chen2017.3_LW,stat="mean")
Giovanni_data_expanded[57,3]<-cellStats(Chen2017.3_RP,stat="mean")
Giovanni_data_expanded[57,4]<-cellStats(Chen2017.3_SH,stat="mean")
Giovanni_data_expanded[57,5]<-cellStats(Chen2017.3_SM,stat="mean")
Giovanni_data_expanded[57,6]<-cellStats(Chen2017.3_SW,stat="mean")
Giovanni_data_expanded[57,7]<-cellStats(Chen2017.3_Ta,stat="mean")
Giovanni_data_expanded[57,8]<-cellStats(Chen2017.3_v,stat="mean")
Giovanni_data_expanded[58,2]<-cellStats(Huber2014.2_LW,stat="mean")
Giovanni_data_expanded[58,3]<-cellStats(Huber2014.2_RP,stat="mean")
Giovanni_data_expanded[58,4]<-cellStats(Huber2014.2_SH,stat="mean")
Giovanni_data_expanded[58,5]<-cellStats(Huber2014.2_SM,stat="mean")
Giovanni_data_expanded[58,6]<-cellStats(Huber2014.2_SW,stat="mean")
Giovanni_data_expanded[58,7]<-cellStats(Huber2014.2_Ta,stat="mean")
Giovanni_data_expanded[58,8]<-cellStats(Huber2014.2_v,stat="mean")
Giovanni_data_expanded[59,2]<-cellStats(Valmassoi2020_LW,stat="mean")
Giovanni_data_expanded[59,3]<-cellStats(Valmassoi2020_RP,stat="mean")
Giovanni_data_expanded[59,4]<-cellStats(Valmassoi2020_SH,stat="mean")
Giovanni_data_expanded[59,5]<-cellStats(Valmassoi2020_SM,stat="mean")
Giovanni_data_expanded[59,6]<-cellStats(Valmassoi2020_SW,stat="mean")
Giovanni_data_expanded[59,7]<-cellStats(Valmassoi2020_Ta,stat="mean")
Giovanni_data_expanded[59,8]<-cellStats(Valmassoi2020_v,stat="mean")



#Check whether the data look normal
summary(Giovanni_data_expanded[,2])
summary(Giovanni_data_expanded[,3])
summary(Giovanni_data_expanded[,4])
summary(Giovanni_data_expanded[,5])
summary(Giovanni_data_expanded[,6])
summary(Giovanni_data_expanded[,7])
summary(Giovanni_data_expanded[,8])


#Aggreate expanded data frame to main data frame
Giovanni_data<-as.data.frame(aggregate(cbind(LWnet,RP,SH,SM,SWnet,Ta,v)~Citation,data=Giovanni_data_expanded,FUN=mean))

#Sum LWnet and SWnet to get Qnet
Giovanni_data[,ncol(Giovanni_data)+1]<-Giovanni_data$LWnet+Giovanni_data$SWnet
colnames(Giovanni_data)[ncol(Giovanni_data)]<-"Qnet"

#Copy a data frame and use data reported in papers to replace Giovanni data
#Giovanni_data_replaced<-Giovanni_data
#Giovanni_data_replaced$SH[c(10,18,32)]<-c(8.06,4.41,7.31)
#Giovanni_data_replaced$Ta[c(4,7,10,17,19,20,25,28,30,32)]<-c(31.38,23.587,25.4,23.92,12.72,10.27,29,18.84,24.83,26.76)
#Giovanni_data_replaced$v[c(18)]<-c(3.7)



#Combine cooling data with background climate and other data
Giovanni_data[,(ncol(Giovanni_data)+1):(ncol(Giovanni_data)+3)]<-IC1_raw_sorted[,12:14]
Giovanni_data<-cbind(Giovanni_data,IC1_raw_sorted[,1:11])
rownames(Giovanni_data)<-1:37

#Regression
#Exclude studies
#Giovanni_data_excluded <-Giovanni_data[-c(19,20,8,22,23,26,14,25,27,33,28),]
Giovanni_data_excluded <-Giovanni_data[-c(3,5,10,11,13,18,19,21,22,23,24,25,26,28:31,16),]
Giovanni_data_excluded$RP_monthly<-Giovanni_data_excluded$RP*24*60*60*30

#Check available samples
Giovanni_data_excluded$Citation[!is.na(Giovanni_data_excluded$delta_Tmean)==TRUE]

#Check colinearity
cor(Giovanni_data_excluded[,c(3:5,7:9)])

lm_Tmean_weather<- lm(delta_Tmean~Ta+SH+v+Qnet+RP,data=Giovanni_data_excluded)
summary(lm_Tmean_weather)
lm_Tmean_weather_step<-step(lm_Tmean_weather,direction="both",trace=F)
summary(lm_Tmean_weather_step)




lm_Tmean_irri<- lm(delta_Tmean~ irri_rate,data=Giovanni_data_excluded)
summary(lm_Tmean_irri)

lm_Tmean_weather_irri<- lm(delta_Tmean~Ta+SH+v+Qnet+RP+irri_rate,data=Giovanni_data_excluded)
summary(lm_Tmean_weather_irri)
lm_Tmean_weather_irri_step<-step(lm_Tmean_weather_irri,direction="both",trace=F)
summary(lm_Tmean_weather_irri_step)

plot(Giovanni_data_excluded$Ta,Giovanni_data_excluded$delta_Tmean,xlab="Ta (oC)",ylab="delta Tmean (oC)",xlim=c(280,310))
abline(h=0)
text(Giovanni_data_excluded$Ta,Giovanni_data_excluded$delta_Tmean,labels=rownames(Giovanni_data_excluded),adj=c(-0.5,0))

plot(Giovanni_data_excluded$SH,Giovanni_data_excluded$delta_Tmean,xlab="Specific humidity (g/kg)",ylab="delta Tmean (oC)",xlim=c(0,0.02))
abline(h=0)
text(Giovanni_data_excluded$SH,Giovanni_data_excluded$delta_Tmean,labels=rownames(Giovanni_data_excluded),adj=c(-0.5,0))

plot(Giovanni_data_excluded$v,Giovanni_data_excluded$delta_Tmean,xlab="Wind speed (m/s)",ylab="delta Tmean (oC)")
abline(h=0)
text(Giovanni_data_excluded$v,Giovanni_data_excluded$delta_Tmean,labels=rownames(Giovanni_data_excluded),adj=c(-0.5,0))

plot(Giovanni_data_excluded$Qnet,Giovanni_data_excluded$delta_Tmean,xlab="Net radiation (W/m2)",ylab="delta Tmean (oC)",xlim=c(50,250))
abline(h=0)
text(Giovanni_data_excluded$Qnet,Giovanni_data_excluded$delta_Tmean,labels=rownames(Giovanni_data_excluded),adj=c(-1,0))

plot(Giovanni_data_excluded$RP_monthly,Giovanni_data_excluded$delta_Tmean,xlab="Rainfall (mm/month)",ylab="delta Tmean (oC)",xlim=c(0,200))
abline(h=0)
text(Giovanni_data_excluded$RP_monthly,Giovanni_data_excluded$delta_Tmean,labels=rownames(Giovanni_data_excluded),adj=c(-1,0))

plot(Giovanni_data_excluded$SM,Giovanni_data_excluded$delta_Tmean,xlab="Soil moisture (0-10 cm) (kg/m^2)",ylab="delta Tmean (oC)",xlim=c(0,40))
abline(h=0)
text(Giovanni_data_excluded$SM,Giovanni_data_excluded$delta_Tmean,labels=rownames(Giovanni_data_excluded),adj=c(-1,0))

plot(Giovanni_data_excluded$irri_rate,Giovanni_data_excluded$delta_Tmean,xlab="Daily irrigation rate (mm)",ylab="delta Tmean (oC)",xlim=c(0,45))
abline(h=0)
text(Giovanni_data_excluded$irri_rate,Giovanni_data_excluded$delta_Tmean,labels=rownames(Giovanni_data_excluded),adj=c(-0.5,0))


lm_Ttransmax_weather<- lm(delta_Ttransmax~Ta+SH+v+Qnet+RP,data=Giovanni_data_excluded)
summary(lm_Ttransmax_weather)
lm_Ttransmax_weather_step<-step(lm_Ttransmax_weather,direction="both",trace=F)
summary(lm_Ttransmax_weather_step)


plot(Giovanni_data_excluded$Ta,Giovanni_data_excluded$delta_Ttransmax,xlab="Ta (oC)",ylab="delta Ttransmax (oC)",ylim=c(-11,1))
abline(h=0)
text(Giovanni_data_excluded$Ta,Giovanni_data_excluded$delta_Ttransmax,labels=rownames(Giovanni_data_excluded),adj=c(-0.5,0))

plot(Giovanni_data_excluded$SH,Giovanni_data_excluded$delta_Ttransmax,xlab="Specific humidity (g/kg)",ylab="delta Ttransmax (oC)" ,ylim=c(-11,1))
abline(h=0)
text(Giovanni_data_excluded$SH,Giovanni_data_excluded$delta_Ttransmax,labels=rownames(Giovanni_data_excluded),adj=c(-0.5,0))

plot(Giovanni_data_excluded$v,Giovanni_data_excluded$delta_Ttransmax,xlab="Wind speed (m/s)",ylab="delta Ttransmax (oC)" ,ylim=c(-11,1))
abline(h=0)
text(Giovanni_data_excluded$v,Giovanni_data_excluded$delta_Ttransmax,labels=rownames(Giovanni_data_excluded),adj=c(-0.5,0))

plot(Giovanni_data_excluded$Qnet,Giovanni_data_excluded$delta_Ttransmax,xlab="Net radiation (W/m2)",ylab="delta Ttransmax (oC)",ylim=c(-11,1))
abline(h=0)
text(Giovanni_data_excluded$Qnet,Giovanni_data_excluded$delta_Ttransmax,labels=rownames(Giovanni_data_excluded),adj=c(-2,0))

plot(Giovanni_data_excluded$RP,Giovanni_data_excluded$delta_Transmax,xlab="Rainfall (mm/day)",ylab="delta Transmax (oC)" ,ylim=c(-11,1))
abline(h=0)
text(Giovanni_data_excluded$RP,Giovanni_data_excluded$delta_Transmax,labels=rownames(Giovanni_data_excluded),adj=c(-2,0))

plot(Giovanni_data_excluded$SM,Giovanni_data_excluded$delta_Transmax,xlab="Soil moisture (0-10 cm) (kg/m^2)",ylab="delta Transmax (oC)" ,ylim=c(-11,1))
abline(h=0)
text(Giovanni_data_excluded$SM,Giovanni_data_excluded$delta_Transmax,labels=rownames(Giovanni_data_excluded),adj=c(-2,0))



library(relaimpo)

relaimpo_Tmean_weather_step<-calc.relimp(lm_Tmean_weather_step)
relaimpo_Tmean_weather_step

relaimpo_Tmean_weather_irri_step <-calc.relimp(lm_Tmean_weather_irri_step)
relaimpo_Tmean_weather_irri_step

relaimpo_Ttransmax_weather_step <-calc.relimp(lm_Ttransmax_weather_step)
relaimpo_Ttransmax_weather_step


#change specific humidity and air temperature units from kg/kg to g/kg and from K to oC, and rain precipitation from kg/m2/s to daily total
Giovanni_data_excluded_scaled<- Giovanni_data_excluded
Giovanni_data_excluded_scaled[,3]<-Giovanni_data_excluded[,3]*60*60*24
Giovanni_data_excluded_scaled[,4]<-Giovanni_data_excluded[,4]*1000
Giovanni_data_excluded_scaled[,7]<-Giovanni_data_excluded[,7]-273.15

write.csv(Giovanni_data_excluded_scaled,"C:/Users/Cheung Pui Kwan/Desktop/temp/IC1_data.csv")

#Export regression results
regression_table<-data.frame(Variable=c("(Intercept)","Ta","Rainfall"),Unit=c("NA","oC","10 mm/month"),Estimate=999,SE=999,t=999,p=999,R2=999)
regression_table[,3:6]<-coef(summary(lm_Tmean_weather_step))
regression_table[,7]<-c("NA",unname(relaimpo_Tmean_weather_step$lmg))
regression_table[3,3:4]<-regression_table[3,3:4]*10/(60*60*24*30)
write.csv(regression_table,"C:/Users/Cheung Pui Kwan/Desktop/temp/regression_table.csv")

#Plot regression
Giovanni_data_excluded$Climate_broad<-substring(Giovanni_data_excluded$Climate.classification,1,1)
#Ta
par(bg="white",mfrow=c(1,1), mgp=c(2.5,0.5,0))
Giovanni_data_excluded$Ta_degC<-Giovanni_data_excluded$Ta-273.15
plot(Giovanni_data_excluded$Ta_degC[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="B"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="B"],pch=21,bg=rgb(1,0,0),xlab="",ylab="",las=1,cex=1.5,xlim=c(15,31),ylim=c(-3.5,1.5))
abline(h=0,lty=2)
mtext(side=1,"Near surface mean air temperature (\u00B0C)",font=2,line=1.5)
mtext(side=2,"Irrigation-induced change in mean air temperature (\u00B0C)",font=2,line=1.5,las=3)
points(Giovanni_data_excluded$Ta_degC[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="B"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="B"],pch=24,bg=rgb(1,0,0),cex=1.5)
points(Giovanni_data_excluded$Ta_degC[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="C"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="C"],pch=21,bg=rgb(1,1,0),cex=1.5)
points(Giovanni_data_excluded$Ta_degC[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="C"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="C"],pch=24,bg=rgb(1,1,0),cex=1.5)
points(Giovanni_data_excluded$Ta_degC[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="D"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="D"],pch=21,bg=rgb(1,0,1),cex=1.5)
points(Giovanni_data_excluded$Ta_degC[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="D"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="D"],pch=24,bg=rgb(1,0,1),cex=1.5)
points(Giovanni_data_excluded$Ta_degC[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="G"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="G"],pch=21,bg=1,cex=1.5)
points(Giovanni_data_excluded$Ta_degC[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="G"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="G"],pch=24,bg=1,cex=1.5)
abline(lm(Giovanni_data_excluded$delta_Tmean~Giovanni_data_excluded$Ta_degC),col="black",lwd=2)
title("a",adj=0)
legend(20,1.4,legend=c("Modelling","Experimental"),pch=c(1,2),bty="n",cex=1.2)
legend(25.5,1.5,legend=c("B: Arid","C: Temperate","D: Continental"),pch=15,col=c(rgb(1,0,0),rgb(1,1,0),rgb(1,0,1)),bty="n",cex=1.2)
#95% CI
x<-Giovanni_data_excluded$Ta_degC
lm_Tmean_Ta_degC<-lm(Giovanni_data_excluded$delta_Tmean~x)
newx<-seq(9,31)
prd<-predict(lm_Tmean_Ta_degC,newdata=data.frame(x=newx),interval=c("confidence"),level=0.95,type="response")
lines(newx,prd[,2],lty=3)
lines(newx,prd[,3],lty=3)

#Qnet
plot(Giovanni_data_excluded$Qnet[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="B"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="B"],pch=21,bg=rgb(1,0,0),xlab="",ylab="",las=1,cex=1.5,xlim=c(70,170),ylim=c(-3.5,1.5))
abline(h=0,lty=2)
mtext(side=1, expression(bold("Study-period near surface mean net radiation (W/m"^"2"*")")),font=2,line=1.5)
mtext(side=2,"Iirrigation-induced change in air temperature (\u00B0C)",font=2,line=1.5,las=3)
points(Giovanni_data_excluded$Qnet[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="B"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="B"],pch=24,bg=rgb(1,0,0),cex=1.5)
points(Giovanni_data_excluded$Qnet[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="C"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="C"],pch=21,bg=rgb(1,1,0),cex=1.5)
points(Giovanni_data_excluded$Qnet[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="C"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="C"],pch=24,bg=rgb(1,1,0),cex=1.5)
points(Giovanni_data_excluded$Qnet[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="D"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="D"],pch=21,bg=rgb(1,0,1),cex=1.5)
points(Giovanni_data_excluded$Qnet[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="D"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="D"],pch=24,bg=rgb(1,0,1),cex=1.5)
points(Giovanni_data_excluded$Qnet[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="G"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="G"], pch=21,bg=1,cex=1.5)
points(Giovanni_data_excluded$Qnet[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="G"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="G"],pch=24,bg=1,cex=1.5)
abline(lm(Giovanni_data_excluded$delta_Tmean~Giovanni_data_excluded$Qnet),bg="black",lwd=2)
title("b",adj=0)
x<-Giovanni_data_excluded$Qnet
lm_Tmean_Qnet<-lm(Giovanni_data_excluded$delta_Tmean~x)
newx<-seq(70,170)
prd<-predict(lm_Tmean_Qnet,newdata=data.frame(x=newx),interval=c("confidence"),level=0.95,type="response")
lines(newx,prd[,2],lty=3)
lines(newx,prd[,3],lty=3)

#RP
Giovanni_data_excluded$RP_monthly<-Giovanni_data_excluded$RP*24*60*60*30
plot(Giovanni_data_excluded$RP_monthly[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="B"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="B"],pch=21,bg=rgb(1,0,0),xlab="",ylab="",las=1,cex=1.5,xlim=c(0,150),ylim=c(-3.5,1.5))
abline(h=0,lty=2)
mtext(side=1, "Mean rainfall (mm/month)", font=2,line=1.5)
mtext(side=2,"Irrigation-induced change in mean air temperature (\u00B0C)",font=2,line=1.5,las=3)
points(Giovanni_data_excluded$RP_monthly[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="B"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="B"],pch=24,bg=rgb(1,0,0),cex=1.5)
points(Giovanni_data_excluded$RP_monthly[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="C"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="C"],pch=21,bg=rgb(1,1,0),cex=1.5)
points(Giovanni_data_excluded$RP_monthly[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="C"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="C"],pch=24,bg=rgb(1,1,0),cex=1.5)
points(Giovanni_data_excluded$RP_monthly[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="D"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="D"],pch=21,bg=rgb(1,0,1),cex=1.5)
points(Giovanni_data_excluded$RP_monthly[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="D"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="D"],pch=24,bg=rgb(1,0,1),cex=1.5)
points(Giovanni_data_excluded$RP_monthly[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="G"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Modelling"&Giovanni_data_excluded$Climate_broad=="G"],pch=21,bg=1,cex=1.5)
points(Giovanni_data_excluded$RP_monthly[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="G"],Giovanni_data_excluded$delta_Tmean[Giovanni_data_excluded$Approach=="Experimental"&Giovanni_data_excluded$Climate_broad=="G"],pch=24,bg=1,cex=1.5)
abline(lm(Giovanni_data_excluded$delta_Tmean~Giovanni_data_excluded$RP_monthly),col="black",lwd=2)
title("b",adj=0)
x<-Giovanni_data_excluded$RP_monthly
lm_Tmean_RP_monthly<-lm(Giovanni_data_excluded$delta_Tmean~x)
newx<-seq(0,150)
prd<-predict(lm_Tmean_RP_monthly,newdata=data.frame(x=newx),interval=c("confidence"),level=0.95,type="response")
lines(newx,prd[,2],lty=3)
lines(newx,prd[,3],lty=3)

#Map of cooling potential
Global_LW_JJA<-raster("Global_LWnet_JJA.tif")
Global_RP_JJA<-raster("Global_RP_JJA.tif")
Global_SH_JJA<-raster("Global_SH_JJA.tif")
Global_SM_JJA<-raster("Global_SM_JJA.tif")
Global_SW_JJA<-raster("Global_SWnet_JJA.tif")
Global_Ta_JJA<-raster("Global_Ta_JJA.tif")
Global_v_JJA<-raster("Global_v_JJA.tif")

Global_LW_DJF<-raster("Global_LWnet_DJF.tif")
Global_RP_DJF<-raster("Global_RP_DJF.tif")
Global_SH_DJF<-raster("Global_SH_DJF.tif")
Global_SM_DJF<-raster("Global_SM_DJF.tif")
Global_SW_DJF<-raster("Global_SWnet_DJF.tif")
Global_Ta_DJF<-raster("Global_Ta_DJF.tif")
Global_v_DJF<-raster("Global_v_DJF.tif")

Global_Qnet_JJA<-Global_LW_JJA+Global_SW_JJA
Global_Qnet_DJF<-Global_LW_DJF+Global_SW_DJF

Global_delta_Tmean_JJA<-(unname(lm_Tmean_weather_step$coefficients[1])+
                           unname(lm_Tmean_weather_step$coefficients[2])*Global_Ta_JJA+
                           
                           unname(lm_Tmean_weather_step$coefficients[3])*Global_RP_JJA
)

plot(Global_delta_Tmean_JJA)

Global_delta_Tmean_DJF<-(unname(lm_Tmean_weather_step$coefficients[1])+
                           unname(lm_Tmean_weather_step$coefficients[2])*Global_Ta_DJF+
                           
                           unname(lm_Tmean_weather_step$coefficients[3])*Global_RP_DJF
)

plot(Global_delta_Tmean_DJF)

#####
#Crop delta_Tmean by hemisphere and merge
extent(Global_delta_Tmean_DJF)
Global_delta_Tmean_DJF_southern<-crop(Global_delta_Tmean_DJF,extent(-180,180,-60,0))
extent(Global_delta_Tmean_JJA)
Global_delta_Tmean_JJA_northern<-crop(Global_delta_Tmean_JJA,extent(-180,180,0,90))
Global_delta_Tmean_JJA_DJF<-merge(Global_delta_Tmean_DJF_southern,Global_delta_Tmean_JJA_northern)

#Create a full map
Global_delta_Tmean_JJA_DJF_full<-Global_delta_Tmean_JJA_DJF
#####

Global_delta_Tmean_JJA<-(unname(lm_Tmean_weather_step$coefficients[1])+
                           unname(lm_Tmean_weather_step$coefficients[2])*Global_Ta_JJA+
                           
                           unname(lm_Tmean_weather_step$coefficients[3])*Global_RP_JJA
)

plot(Global_delta_Tmean_JJA)

Global_delta_Tmean_DJF<-(unname(lm_Tmean_weather_step$coefficients[1])+
                           unname(lm_Tmean_weather_step$coefficients[2])*Global_Ta_DJF+
                           
                           unname(lm_Tmean_weather_step$coefficients[3])*Global_RP_DJF
)

plot(Global_delta_Tmean_DJF)

#Create raster world map for masking
Global_delta_Tmean_JJA_land<-Global_delta_Tmean_JJA
Global_delta_Tmean_JJA_land[!is.na(Global_delta_Tmean_JJA_land)]<-999
Global_delta_Tmean_JJA_land
Global_delta_Tmean_DJF_land<-Global_delta_Tmean_DJF
Global_delta_Tmean_DJF_land[!is.na(Global_delta_Tmean_DJF_land)]<-999
Global_delta_Tmean_DJF_land

#Exclude locations with Ta,v and RP outside the data range
#Global_Ta_JJA[Global_Ta_JJA>max(Giovanni_data_excluded$Ta)|Global_Ta_JJA<min(Giovanni_data_excluded$Ta)]<-NA
#Global_v_JJA[Global_v_JJA>max(Giovanni_data_excluded$v)|Global_v_JJA<min(Giovanni_data_excluded$v)]<-NA
#Global_RP_JJA[Global_RP_JJA>max(Giovanni_data_excluded$RP)|Global_RP_JJA<min(Giovanni_data_excluded$RP)]<-NA
#Global_Ta_DJF[Global_Ta_DJF>max(Giovanni_data_excluded$Ta)|Global_Ta_DJF<min(Giovanni_data_excluded$Ta)]<-NA
#Global_v_DJF[Global_v_DJF>max(Giovanni_data_excluded$v)|Global_v_DJF<min(Giovanni_data_excluded$v)]<-NA
#Global_RP_DJF[Global_RP_DJF>max(Giovanni_data_excluded$RP)|Global_RP_DJF<min(Giovanni_data_excluded$RP)]<-NA


Global_delta_Tmean_JJA<-(unname(lm_Tmean_weather_step$coefficients[1])+
                           unname(lm_Tmean_weather_step$coefficients[2])*Global_Ta_JJA+
                           
                           unname(lm_Tmean_weather_step$coefficients[3])*Global_RP_JJA
)

plot(Global_delta_Tmean_JJA)

Global_delta_Tmean_DJF<-(unname(lm_Tmean_weather_step$coefficients[1])+
                           unname(lm_Tmean_weather_step$coefficients[2])*Global_Ta_DJF+
                           
                           unname(lm_Tmean_weather_step$coefficients[3])*Global_RP_DJF
)

plot(Global_delta_Tmean_DJF)




#Import city data
#Input cities data
cities_point<-readOGR("ne_10m_populated_places.shp")


#Check whether the selected 100 cities are available in shapefile

#Input the name of the 100 cities
cities_100_raw<-read.csv("Cities_100_raw.csv")

#Remove space
cities_100_raw[,1]<-trimws(cities_100_raw[,1],which="right")
cities_100_raw[,2]<-trimws(cities_100_raw[,2],which="right")
cities_100_raw[,3]<-trimws(cities_100_raw[,3],which="right")
cities_100_raw[,4]<-trimws(cities_100_raw[,4],which="right")

#Check availability
cities_100_raw$City_availability<-999
i=1
while(i<101){
  if(length(cities_point$MIN_AREAKM[cities_point$NAME==cities_100_raw$City[i]])>0){
    cities_100_raw$City_availability[i]<-"Y"
    cities_100_raw$City_name_in_data[i]<-cities_100_raw$City[i]
  } else {
    cities_100_raw$City_availability[i]<-"N"
  }
  i=i+1
}

#Check and save the name and row number of missing city
missing<-which(cities_100_raw$City_availability=="N")
missing_city<-cities_100_raw[which(cities_100_raw$City_availability=="N"),1]
missing_country<-cities_100_raw[which(cities_100_raw$City_availability=="N"),2]

missing_city

#Save city name of the data for missing cities
cities_point$NAME[cities_point$ADM0NAME==missing_country[1]]
cities_100_raw$City_name_in_data[missing[1]]<-cities_point$NAME[cities_point$ADM0NAME==missing_country[1]][12]

cities_point$NAME[cities_point$ADM0NAME==missing_country[2]]
cities_100_raw$City_name_in_data[missing[2]]<-cities_point$NAME[cities_point$ADM0NAME==missing_country[2]][247]

cities_point$NAME[cities_point$ADM0NAME==missing_country[3]]
cities_100_raw$City_name_in_data[missing[3]]<-cities_point$NAME[cities_point$ADM0NAME==missing_country[3]][27]

#manual input of unknown city area
cities_100_raw$Radius.km.<-as.numeric(as.character(cities_100_raw$Radius.km.))
cities_100_raw$Area.km2.<-as.numeric(as.character(cities_100_raw$Area.km2.))

cities_100_raw$Area.km2.[cities_100_raw$City=="Cairns"]<-254.3
cities_100_raw$Area.km2.[cities_100_raw$City=="Davao"]<-129
cities_100_raw$Area.km2.[cities_100_raw$City=="Harstad"]<-11.15
cities_100_raw$Area.km2.[cities_100_raw$City=="Heihe"]<-2*4
cities_100_raw$Area.km2.[cities_100_raw$City=="Islamabad"]<-220
cities_100_raw$Area.km2.[cities_100_raw$City=="Moroni"]<-30
cities_100_raw$Area.km2.[cities_100_raw$City=="Paramaribo"]<-182
cities_100_raw$Area.km2.[cities_100_raw$City=="Pasto"]<-26.4
cities_100_raw$Area.km2.[cities_100_raw$City=="Kiev"]<-762
cities_100_raw$Area.km2.[cities_100_raw$City=="Nukus"]<-222
cities_100_raw$Area.km2.[cities_100_raw$City=="Sivas"]<-7*4
cities_100_raw$Area.km2.[cities_100_raw$City=="Suva"]<-10*5
cities_100_raw$Area.km2.[cities_100_raw$City=="Tallinn"]<-159
cities_100_raw$Area.km2.[cities_100_raw$City=="Tel Aviv-Yafo"]<-577
cities_100_raw$Area.km2.[cities_100_raw$City=="Thimphu"]<-26.1
cities_100_raw$Area.km2.[cities_100_raw$City=="Washington, D.C."]<-5281
cities_100_raw$Area.km2.[cities_100_raw$City=="Turpan"]<-10*10/2
cities_100_raw$Area.km2.[cities_100_raw$City=="Walvis Bay"]<-10*4
cities_100_raw$Area.km2.[cities_100_raw$City=="North Platte"]<-5*5
#cities_100_raw$Area.km2.[cities_100_raw$City=="Devonport"]<-3*3
cities_100_raw$Area.km2.[cities_100_raw$City=="Arusha"]<-10*10
cities_100_raw$Area.km2.[cities_100_raw$City=="Nicosia"]<-111
#cities_100_raw$Area.km2.[cities_100_raw$City=="Las Palmas"]<-100.55

#Calculate radius
cities_100_raw$Radius.km.<-((cities_100_raw$Area.km2./pi)^0.5)

#Crop and mask raster by shapefile and calculate delta Tmean
#Note: crop function is used for cropping the extent of a raster (xmin, ymin, xmax, ymax)
#Note: mask function is used for masking the raster values outside the shapefile 
#Note: the extent of the shapefile and the raster has to be the same to be able to use the mask function

#Example 1
test<-readOGR("wrreg_lamb.shp")
plot(test)
summary(test)
test1<-crop(Sacks2009_LW,test)
proj4string(Sacks2009_LW)
test<-spTransform(test,proj4string(Sacks2009_LW))
plot(Sacks2009_LW)
plot(test,add=T)
test2<-mask(crop(Sacks2009_LW,extent(test)),test)
plot(test2)

#Example 2
cities_100_point_test<-subset(cities_point,NAME==c("Hong Kong","Tokyo"))
cities_100_area_test<-gBuffer(cities_100_point,byid=T,width=c(4,2))
cities_100_area_test<-spTransform(cities_100_area,proj4string(Sacks2009_LW))
plot(Sacks2009_LW)
plot(cities_100_area_test,add=T)
cities_100_area_LW<-mask(crop(Sacks2009_LW,extent(cities_100_area_test)),cities_100_area_test)


#Rectify the country name before subsetting using both city and country name
same_name<-character(length = 100)
i=1
while(i<101){
  print(cities_point$MAX_AREAKM[cities_point$NAME==cities_100_raw$City_name_in_data[i]&cities_point$ADM0NAME==cities_100_raw$Country_Region[i]])
  i=i+1
}
#Identify the row number by saving 
i=1
while(i<101){  if(is.numeric(cities_point$MAX_AREAKM[cities_point$NAME==cities_100_raw$City_name_in_data[i]&cities_point$ADM0NAME==cities_100_raw$Country_Region[i]])){
  same_name[i]<-(cities_point$MAX_AREAKM[cities_point$NAME==cities_100_raw$City_name_in_data[i]&cities_point$ADM0NAME==cities_100_raw$Country_Region[i]])    
}
  else{same_name[i]<-"NA"}
  i=i+1
}
#Rectifying the country name
cities_point$ADM0NAME[cities_point$NAME=="Hong Kong"]
cities_100_raw$Country_Region[cities_100_raw$City=="Hong Kong"]<-cities_point$ADM0NAME[cities_point$NAME=="Hong Kong"]

#Check again to see if using city and country name together can get one output of area for all 100 cities
i=1
while(i<101){
  print(cities_point$MAX_AREAKM[cities_point$NAME==cities_100_raw$City_name_in_data[i]&cities_point$ADM0NAME==cities_100_raw$Country_Region[i]])
  i=i+1
}

#Extract the 100 cities from the data
i=1
cities_100_point<-cities_point[cities_point$NAME==cities_100_raw$City_name_in_data[i]&cities_point$ADM0NAME==cities_100_raw$Country_Region[i],]
i=2
while(i<101){
  temp<-cities_point[cities_point$NAME==cities_100_raw$City_name_in_data[i]&cities_point$ADM0NAME==cities_100_raw$Country_Region[i],]
  cities_100_point<-rbind(cities_100_point,temp)
  i=i+1
}

#Create a subset of cities point that have a dark background colour in the delta Tmean map
cities_100_point_white<-cities_100_point[cities_100_point$ADM0NAME==c("Iraq")|cities_100_point$ADM0NAME==c("Pakistan")|cities_100_point$ADM0NAME==c("Nepal")|cities_100_point$ADM0NAME==c("Saudi Arabia")|cities_100_point$ADM0NAME==c("United Arab Emirates")|cities_100_point$ADM0NAME==c("Qatar")|cities_100_point$ADM0NAME==c("India")|cities_100_point$ADM0NAME==c("Uzbekistan"),]

library(rgeos)
#create buffer around 100 cities
cities_100_area<-gBuffer(cities_100_point,byid=T,width=cities_100_raw$Radius.km./100)
cities_100_area<-spTransform(cities_100_area,proj4string(Global_delta_Tmean_JJA))
plot(Global_delta_Tmean_JJA)
plot(cities_100_area,add=T)


#Use extract to get the delta_Tmean and save in two columns
cities_100_raw$delta_Tmean_JJA<-as.vector(extract(Global_delta_Tmean_JJA,cities_100_area,method="bilinear",na.rm=T,fun=mean,weights=T,normalizeWeights=F))
cities_100_raw$delta_Tmean_DJF<-as.vector(extract(Global_delta_Tmean_DJF,cities_100_area,method="bilinear",na.rm=T,fun=mean,weights=T,normalizeWeights=F))

#Add a column to determine location (southern or northern hemisphere) 
i=1
while(i<101){
  if(cities_point$LATITUDE[cities_point$NAME==cities_100_raw$City_name_in_data[i]&cities_point$ADM0NAME==cities_100_raw$Country_Region[i]]>0){
    cities_100_raw$Hemisphere[i]<-"Northern"
  }
  else{
    cities_100_raw$Hemisphere[i]<-"Southern"
  }
  i=i+1
}

#Add a column to save JJA or DJF delta_Tmean depending on location
i=1
while(i<101){
  if(cities_100_raw$Hemisphere[i]=="Northern"){
    cities_100_raw$delta_Tmean[i]<-cities_100_raw$delta_Tmean_JJA[i]
  }
  else{
    cities_100_raw$delta_Tmean[i]<-cities_100_raw$delta_Tmean_DJF[i]
  }
  i=i+1
}

#Set cells with cities_100_raw$delta_Tmean=0 to NA because they were not covered in the data range
cities_100_raw$delta_Tmean[cities_100_raw$delta_Tmean==0]<-NA

#Add broad climate classification
cities_100_raw$Climate_broad<-substring(cities_100_raw$Climate,1,1)
cities_100_raw$Climate_broad[cities_100_raw$Climate_broad=="A"]<-"A: Tropical"
cities_100_raw$Climate_broad[cities_100_raw$Climate_broad=="B"]<-"B: Arid"
cities_100_raw$Climate_broad[cities_100_raw$Climate_broad=="C"]<-"C: Temperate"
cities_100_raw$Climate_broad[cities_100_raw$Climate_broad=="D"]<-"D: Continental"

#Export to csv
write.csv(cities_100_raw,"C:/Users/Cheung Pui Kwan/Desktop/Temp/cities_100_delta_Tmean.csv")

#Crop and merge hemispheres
#Crop delta_Tmean by hemisphere and merge
extent(Global_delta_Tmean_DJF)
Global_delta_Tmean_DJF_southern<-crop(Global_delta_Tmean_DJF,extent(-180,180,-60,0))
extent(Global_delta_Tmean_JJA)
Global_delta_Tmean_JJA_northern<-crop(Global_delta_Tmean_JJA,extent(-180,180,0,90))
Global_delta_Tmean_JJA_DJF<-merge(Global_delta_Tmean_DJF_southern,Global_delta_Tmean_JJA_northern)

# Crop delta_Tmean_full by hemisphere and merge
Global_delta_Tmean_JJA_DJF_full<-crop(Global_delta_Tmean_JJA_DJF_full,extent(-180,180,-60,90))

#Crop delta_Tmean_land by hemisphere and merge
extent(Global_delta_Tmean_DJF_land)
Global_delta_Tmean_DJF_land_southern<-crop(Global_delta_Tmean_DJF_land,extent(-180,180,-60,0))
extent(Global_delta_Tmean_JJA_land)
Global_delta_Tmean_JJA_land_northern<-crop(Global_delta_Tmean_JJA_land,extent(-180,180,0,90))
Global_delta_Tmean_JJA_DJF_land<-merge(Global_delta_Tmean_DJF_land_southern,Global_delta_Tmean_JJA_land_northern)


#Crop Ta by hemisphere and merge
extent(Global_Ta_DJF)
Global_Ta_DJF_southern<-crop(Global_Ta_DJF,extent(-180,180,-60,0))
extent(Global_Ta_JJA)
Global_Ta_JJA_northern<-crop(Global_Ta_JJA,extent(-180,180,0,90))
Global_Ta_JJA_DJF<-merge(Global_Ta_DJF_southern,Global_Ta_JJA_northern)



#Apply necessary scale
Global_Ta_JJA_DJF_plot<- Global_Ta_JJA_DJF-273.15

#Crop SH by hemisphere and merge
extent(Global_SH_DJF)
Global_SH_DJF_southern<-crop(Global_SH_DJF,extent(-180,180,-60,0))
extent(Global_SH_JJA)
Global_SH_JJA_northern<-crop(Global_SH_JJA,extent(-180,180,0,90))
Global_SH_JJA_DJF<-merge(Global_SH_DJF_southern,Global_SH_JJA_northern)

#Apply necessary scale
Global_SH_JJA_DJF_plot<- Global_SH_JJA_DJF

#Crop v by hemisphere and merge
extent(Global_v_DJF)
Global_v_DJF_southern<-crop(Global_v_DJF,extent(-180,180,-60,0))
extent(Global_v_JJA)
Global_v_JJA_northern<-crop(Global_v_JJA,extent(-180,180,0,90))
Global_v_JJA_DJF<-merge(Global_v_DJF_southern,Global_v_JJA_northern)

#Apply necessary scale
Global_v_JJA_DJF_plot<- Global_v_JJA_DJF

#Crop Qnet by hemisphere and merge
extent(Global_Qnet_DJF)
Global_Qnet_DJF_southern<-crop(Global_Qnet_DJF,extent(-180,180,-60,0))
extent(Global_Qnet_JJA)
Global_Qnet_JJA_northern<-crop(Global_Qnet_JJA,extent(-180,180,0,90))
Global_Qnet_JJA_DJF<-merge(Global_Qnet_DJF_southern,Global_Qnet_JJA_northern)

#Apply necessary scale
Global_Qnet_JJA_DJF_plot<- Global_Qnet_JJA_DJF

#Crop RP by hemisphere and merge
extent(Global_RP_DJF)
Global_RP_DJF_southern<-crop(Global_RP_DJF,extent(-180,180,-60,0))
extent(Global_RP_JJA)
Global_RP_JJA_northern<-crop(Global_RP_JJA,extent(-180,180,0,90))
Global_RP_JJA_DJF<-merge(Global_RP_DJF_southern,Global_RP_JJA_northern)

#Apply necessary scale
Global_RP_JJA_DJF_plot<- Global_RP_JJA_DJF*60*60*24*30



#Create grey-out areas
Global_delta_Tmean_JJA_DJF_exclusion<-mask(Global_delta_Tmean_JJA_DJF_land,Global_delta_Tmean_JJA_DJF,inverse=T)
plot(Global_delta_Tmean_JJA_DJF_exclusion)


#Plot Koppen-Geiger
library(fields)
Koppen_Geiger<-raster("world_koppen_tif_export.tif")
Koppen_Geiger_AE<-Koppen_Geiger
Koppen_Geiger_AE[Koppen_Geiger_AE>3&Koppen_Geiger_AE<29]<-NA
plot(Koppen_Geiger_AE)
Koppen_Geiger_AE<-crop(Koppen_Geiger_AE,extent(-180,180,-60,90))
Koppen_Geiger_AE_resampled<-resample(Koppen_Geiger_AE,Global_delta_Tmean_JJA_DJF_full)
Koppen_Geiger_AE_resampled[Koppen_Geiger_AE_resampled>0]<-999
Koppen_Geiger_AE_poly<-rasterToPolygons(Koppen_Geiger_AE_resampled,dissolve=T,na.rm=T)
plot(Koppen_Geiger_AE_poly,density=10)

#Create a separte legend
barplot(c(1,1,1,1),legend.text=c("Tropical","Arid","Temperate","Continental"),col=c(rgb(0,0,255/255),rgb(255/255,0,0),rgb(255/255,255/255,0),rgb(255/255,0,255/255)),args.legend = list(bty = "n", x = "top", ncol = 4,cex=1.5,pt.lwd=2),ylim=c(0,2))

#Global delta Tmean map
#Set a colour ramp
Red3White1Blue6<-c("#0000CC","#0000FF","#CCCCFF","#3333FF","#6666FF","#9999FF","#FFFFFF","#FFCCCC","#FF9999","#FF6666")
#Import country boundary
country_boundary<-readOGR("ne_50m_admin_0_countries_lakes.shp")
country_boundary<-crop(country_boundary,extent(-180,180,-60,80))

#Exclude Greenland because of abnormal soil moisture level
#greenland_area<-readOGR("GRL_adm0.shp")
#Global_delta_Tmean_JJA_DJF_noGreenland<-mask(Global_delta_Tmean_JJA_DJF,greenland_area,inverse=T,col="grey")
#plot(Global_delta_Tmean_JJA_DJF_noGreenland)

#apply necessary scale
Global_delta_Tmean_JJA_DJF_plot<-Global_delta_Tmean_JJA_DJF

#Check value range
Global_delta_Tmean_JJA_DJF_plot
minV<-minValue(Global_delta_Tmean_JJA_DJF_plot)
maxV<-maxValue(Global_delta_Tmean_JJA_DJF_plot)

#Check required colour numbers
length(c(round(minV,digits=1),seq(-2.5,1,0.5),round(maxV,digits=1)))

#Set colour ramps
#blues<-colorRampPalette(c(rev(brewer.pal(9,"Blues"),#abd9e9)))
#reds<-colorRampPalette(c(brewer.pal(9,"Reds")))
#blues_col<-rev(c("#e0f3f8", "#abd9e9", "#74add1", "#4575b4")) #4 blues
#reds_col<-c("#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026") #6 reds
reds_col<-c("#ffffbf", "#fdae61", "#d73027")

#Plot (delta_Tmean,J JA+DJF), save in Rstudio, grey out out-of-range data
library(fields)
par(mgp=c(1.5,0.3,0),mar=c(3.1, 4.1, 3.1, 2.1),las=1)
image.plot(Global_delta_Tmean_JJA_DJF_plot,ylim=c(-60,80),zlim=c(minV,maxV),xlab="",ylab="",nlevel=9,col=c(rev(brewer.pal(6,"Blues")),reds_col),legend.shrink=1,legend.width=1.3,horizontal=T,tck=0,axes=F,axis.args=list(tck=0,mgp=c(3,0.25,0)),breaks=c(minV,seq(-2.5,1,0.5),maxV),lab.breaks=c(round(minV,digits=1),seq(-2.5,1,0.5),round(maxV,digits=1)-0.1))
axis(side=1,at=c(-120,-60,0,60,120),labels=c("-120","-60","0","60","120"),tck=0.02)
axis(side=2,at=c(-60,-30,0,30,60),labels = c("-60","-30","0","30","60"),tck=0.02)
mtext(side=1,line=5.6,"Estimated irrigation-induced change in mean air temperature (\u00B0C)",font=2)
mtext(side=1,line=1.3,"Longitude (degree)",font=2)
mtext(side=2,line=1.3,"Laditude (degree)",font=2,las=3)
box()
plot(Global_delta_Tmean_JJA_DJF_exclusion,add=T,col="grey90",axes=F,legend=F)
plot(country_boundary,add=T,border="grey70",ylim=c(-60,80))
#plot(cities_100_point,add=T,pch=4,cex=1.2)
axis(side=3,at=c(-120,-60,0,60,120),labels=c("","","","",""),tck=0.02)
axis(side=4,at=c(-60,-30,0,30,60),labels=c("","","","",""),tck=0.02,pos=180)
abline(h=0,col="white",lwd=1)
abline(h=0.5)
abline(h=-0.5)
text("JJA",x=-170,y=5,font=2)
text("DJF",x=-170,y=-5,font=2)           
legend(-170,-35,legend=c("No data"),pch=c(22),pt.bg="grey90",bty="n",pt.cex=1.6,cex=1.2)




#Mask the tropical and polar
Global_delta_Tmean_JJA_DJF_excludeAE<-mask(Global_delta_Tmean_JJA_DJF_full,Koppen_Geiger_AE_resampled, inverse=T)
plot(Global_delta_Tmean_JJA_DJF_excludeAE)

#apply necessary scale
Global_delta_Tmean_JJA_DJF_excludeAE_plot<-Global_delta_Tmean_JJA_DJF_excludeAE

#Check value range
Global_delta_Tmean_JJA_DJF_excludeAE_plot 
minV<-minValue(Global_delta_Tmean_JJA_DJF_excludeAE_plot)
maxV<-maxValue(Global_delta_Tmean_JJA_DJF_excludeAE_plot)

#Check required colour numbers
length(c(round(minV,digits=1),seq(-2.5,3,0.5),round(maxV,digits=1)))
c(round(minV,digits=1),seq(-2.5,3,0.5),round(maxV,digits=1))

#Set colour ramps
blues<-colorRampPalette(c(rev(brewer.pal(9,"Blues"))))
reds<-colorRampPalette(c(brewer.pal(9,"Reds")))
#blues_col<-rev(c("#e0f3f8", "#abd9e9", "#74add1", "#4575b4")) #4 blues
#reds_col5<-c("#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027")#5 reds
#reds_col4<-c("#ffffbf", "#fee090", "#fdae61", "#f46d43")#4 reds
#reds_col<-c("#ffffbf", "#fdae61", "#d73027")

#Plot (delta_Tmean,J JA+DJF) save in Rstudio, grey out tropical and polar
library(fields)
par(mgp=c(1.5,0.3,0),mar=c(3.1, 4.1, 3.1, 2.1),las=1)
image.plot(Global_delta_Tmean_JJA_DJF_excludeAE,ylim=c(-60,80),zlim=c(minV,maxV),xlab="",ylab="" ,col=c("#053061",rev(brewer.pal(11,"RdYlBu"))),legend.shrink=1,legend.width=1.3,horizontal=T,tck=0,axes=F,axis.args=list(tck=0,mgp=c(3,0.25,0)),breaks=c(seq(-3,3,0.5)),lab.breaks=c(seq(-3,2.5,0.5),">3"))
axis(side=1,at=c(-120,-60,0,60,120),labels=c("-120","-60","0","60","120"),tck=0.02)
axis(side=2,at=c(-60,-30,0,30,60),labels = c("-60","-30","0","30","60"),tck=0.02)
mtext(side=1,line=5.6,"Estimated irrigation-induced change in mean air temperature (\u00B0C)",font=2)
mtext(side=1,line=1.3,"Longitude (degree)",font=2)
mtext(side=2,line=1.3,"Laditude (degree)",font=2,las=3)
box()
plot(country_boundary,add=T,border="grey70",ylim=c(-60,80))
plot(Koppen_Geiger_AE_poly,col="grey90", border="grey70",add=T,axes=F,lwd=0.5)
#plot(Koppen_Geiger_AE_poly,density=10,angle=45,border="grey70",add=T,axes=F,lwd=0.5)
#plot(Koppen_Geiger_AE_poly,density=10,angle=135,border="grey70",add=T,axes=F,lwd=0.5)
plot(cities_100_point,add=T,pch=4,cex=1)
plot(cities_100_point_white,add=T,pch=4,cex=1,col="white")
axis(side=3,at=c(-120,-60,0,60,120),labels=c("","","","",""),tck=0.02)
axis(side=4,at=c(-60,-30,0,30,60),labels=c("","","","",""),tck=0.02,pos=180)
abline(h=0,col="white",lwd=1)
abline(h=0.5)
abline(h=-0.5)
text("JJA",x=-170,y=5,font=2)
text("DJF",x=-170,y=-5,font=2)           
legend(-170,-35,legend=c("No data"),pch=c(22),pt.bg="grey90",bty="n",pt.cex=1.6,cex=1.2)
#legend(-170,-35,legend=c(""),pch=c(7),pt.bg="grey90",bty="n",pt.cex=1.6,cex=1.2)
legend(-170,-25,legend=c("City"),pch=c(4),pt.bg=c("black"),bty="n",pt.cex=c(1),cex=1.1)
#Global Ta map
#Mask the tropical and polar
Global_Ta_JJA_DJF_excludeAE<-mask(Global_Ta_JJA_DJF_plot,Koppen_Geiger_AE_resampled, inverse=T)
plot(Global_Ta_JJA_DJF_excludeAE)

#apply necessary scale
Global_Ta_JJA_DJF_excludeAE_plot<-Global_Ta_JJA_DJF_excludeAE

#Check value range
Global_Ta_JJA_DJF_excludeAE_plot 
minV<-minValue(Global_Ta_JJA_DJF_excludeAE_plot)
maxV<-maxValue(Global_Ta_JJA_DJF_excludeAE_plot)

#Check required colour numbers
length(c(round(minV,digits=1),seq(0,35,5),round(maxV,digits=1)))
(c(round(minV,digits=1),seq(0,35,5),round(maxV,digits=1)))

#Set colour
yellow_red<-colorRampPalette(brewer.pal(9,"YlOrRd"))
#blues_col<-rev(c("#e0f3f8", "#abd9e9", "#74add1"))

#Plot (Ta), save in Rstudio
library(RColorBrewer)
library(fields)
par(mgp=c(1.5,0.3,0),mar=c(3.1, 4.1, 3.1, 2.1),las=1)
image.plot(Global_Ta_JJA_DJF_excludeAE_plot,ylim=c(-60,80),zlim=c(minV,maxV),xlab="",ylab="",col=c(yellow_red(8)),legend.shrink=1,legend.width=1.3,horizontal=T,tck=0,axes=F,axis.args=list(tck=0,mgp=c(3,0.25,0)),breaks=c(seq(0,40,5)),lab.breaks=c(seq(0,40,5)))
axis(side=1,at=c(-120,-60,0,60,120),labels=c("-120","-60","0","60","120"),tck=0.02)
axis(side=2,at=c(-60,-30,0,30,60),labels = c("-60","-30","0","30","60"),tck=0.02)
mtext(side=1,line=5.6,"Near surface mean air temperature (\u00B0C)",font=2)
mtext(side=1,line=1.3,"Longitude (degree)",font=2)
mtext(side=2,line=1.3,"Laditude (degree)",font=2,las=3)
box()
plot(country_boundary,add=T,border="grey70",ylim=c(-60,80))
#plot(Global_delta_Tmean_JJA_DJF_exclusion,add=T,col="grey90",axes=F,legend=F)
plot(Koppen_Geiger_AE_poly,col="grey90", border="grey70",add=T,axes=F,lwd=0.5)
#plot(Koppen_Geiger_AE_poly,density=10,angle=45,border="grey70",add=T,axes=F,lwd=0.5)
#plot(Koppen_Geiger_AE_poly,density=10,angle=135,border="grey70",add=T,axes=F,lwd=0.5)
#plot(cities_100_point,add=T,pch=4,cex=1.2)
axis(side=3,at=c(-120,-60,0,60,120),labels=c("","","","",""),tck=0.02)
axis(side=4,at=c(-60,-30,0,30,60),labels=c("","","","",""),tck=0.02,pos=180)
abline(h=0,col="white",lwd=1)
abline(h=0.5)
abline(h=-0.5)
text("JJA",x=-170,y=5,font=2)
text("DJF",x=-170,y=-5,font=2)           
legend(-170,-35,legend=c("No data"),pch=c(22),pt.bg="grey90",bty="n",pt.cex=1.6,cex=1.2)
title("a",adj=0)
#Global RP map
#Mask the tropical and polar
Global_RP_JJA_DJF_excludeAE<-mask(Global_RP_JJA_DJF_plot,Koppen_Geiger_AE_resampled, inverse=T)
plot(Global_RP_JJA_DJF_excludeAE)

#apply necessary scale
Global_RP_JJA_DJF_excludeAE_plot<-Global_RP_JJA_DJF_excludeAE

#Check value range
Global_RP_JJA_DJF_excludeAE_plot 
minV<-minValue(Global_RP_JJA_DJF_excludeAE_plot)
maxV<-maxValue(Global_RP_JJA_DJF_excludeAE_plot)

#Check required colour numbers
length(c(round(minV,digits=1),seq(50,400,50),round(maxV,digits=1)))
(c(round(minV,digits=1),seq(50,400,50),round(maxV,digits=1)))

#Set colour
blues<-colorRampPalette(brewer.pal(9,"Blues"))

#Plot (RP), save in Rstudio
library(RColorBrewer)
library(fields)
par(mgp=c(1.5,0.3,0),mar=c(3.1, 4.1, 3.1, 2.1),las=1)
image.plot(Global_RP_JJA_DJF_excludeAE_plot,ylim=c(-60,80),zlim=c(minV,maxV),xlab="",ylab="",col=c(blues(9)),legend.shrink=1,legend.width=1.3,horizontal=T,tck=0,axes=F,axis.args=list(tck=0,mgp=c(3,0.25,0)),breaks=c(seq(0,450,50)),lab.breaks=c(seq(0,450,50)))
axis(side=1,at=c(-120,-60,0,60,120),labels=c("-120","-60","0","60","120"),tck=0.02)
axis(side=2,at=c(-60,-30,0,30,60),labels = c("-60","-30","0","30","60"),tck=0.02)
mtext(side=1,line=5.6,"Mean rainfall (mm/month)",font=2)
mtext(side=1,line=1.3,"Longitude (degree)",font=2)
mtext(side=2,line=1.3,"Laditude (degree)",font=2,las=3)
box()
plot(country_boundary,add=T,border="grey70",ylim=c(-60,80))
#plot(Global_delta_Tmean_JJA_DJF_exclusion,add=T,col="grey90",axes=F,legend=F)
plot(Koppen_Geiger_AE_poly,col="grey90", border="grey70",add=T,axes=F,lwd=0.5)
#plot(Koppen_Geiger_AE_poly,density=10,angle=45,border="grey70",add=T,axes=F,lwd=0.5)
#plot(Koppen_Geiger_AE_poly,density=10,angle=135,border="grey70",add=T,axes=F,lwd=0.5)
#plot(cities_100_point,add=T,pch=4,cex=1.2)
axis(side=3,at=c(-120,-60,0,60,120),labels=c("","","","",""),tck=0.02)
axis(side=4,at=c(-60,-30,0,30,60),labels=c("","","","",""),tck=0.02,pos=180)
abline(h=0,col="white",lwd=1)
abline(h=0.5)
abline(h=-0.5)
text("JJA",x=-170,y=5,font=2)
text("DJF",x=-170,y=-5,font=2)           
legend(-170,-35,legend=c("No data"),pch=c(22),pt.bg="grey90",bty="n",pt.cex=1.6,cex=1.2)
title("b",adj=0)
#Area irrigated as percentage of area equipped for irrigation
irri_map<-raster("gmia_v5_aai_pct_aei.asc")
par(mgp=c(1.5,0.3,0),mar=c(3.1, 4.1, 3.1, 2.1),las=1)
image.plot(irri_map,ylim=c(-60,80),zlim=c(0,100),xlab="",ylab="",col=c(Greens(10)),legend.shrink=1,legend.width=1.3,horizontal=T,tck=0,axes=F,axis.args=list(tck=0,mgp=c(3,0.25,0)),breaks=c(seq(0,100,10)),lab.breaks=c(seq(0,100,10)))
axis(side=1,at=c(-120,-60,0,60,120),labels=c("-120","-60","0","60","120"),tck=0.02)
axis(side=2,at=c(-60,-30,0,30,60),labels = c("-60","-30","0","30","60"),tck=0.02)
mtext(side=1,line=5.6,"Irrigated area expressed as area equipped for irrigation (%)",font=2)
mtext(side=1,line=1.3,"Longitude (degree)",font=2)
mtext(side=2,line=1.3,"Laditude (degree)",font=2,las=3)
box()
plot(country_boundary,add=T,border="grey70",ylim=c(-60,80))
axis(side=3,at=c(-120,-60,0,60,120),labels=c("","","","",""),tck=0.02)
axis(side=4,at=c(-60,-30,0,30,60),labels=c("","","","",""),tck=0.02,pos=180)
#Boxplot

#Exclude delta_Tmean=NA
cities_100_raw_noNA<-cities_100_raw[!is.na(cities_100_raw$delta_Tmean),]

#Check sample size
summary(as.factor(cities_100_raw_noNA$Climate_broad))

#Boxplot by broad climate
cities_100_raw_noNA$Climate_broad<-as.factor(cities_100_raw_noNA$Climate_broad)
BCD_means<-tapply(cities_100_raw_noNA$delta_Tmean,cities_100_raw_noNA$Climate_broad,mean)
BCD_boxplot<-boxplot(cities_100_raw_noNA$delta_Tmean~cities_100_raw_noNA$Climate_broad,axes=F,ylim=c(-3,2),xlab="",ylab="", col=c(rgb(0.95,0,0),rgb(0.95,0.95,0),rgb(0.95,0,0.95)))
par(mgp=c(3,1,0),mar=c(5.1, 4.1, 4.1, 2.1))
boxplot(cities_100_raw_noNA$delta_Tmean~cities_100_raw_noNA$Climate_broad,axes=F,ylim=c(-3,2),xlab="",ylab="", col=c(rgb(0.95,0,0),rgb(0.95,0.95,0),rgb(0.95,0,0.95)))
points(BCD_means,col="black",pch=16)
mtext(side=1,"Koppen-Geiger climate classification",font=2,line=2.5)
mtext(side=2,"Estimated irrigation-induced change in air temperature (\u00B0C)",font=2,line=2.5,las=3)
axis(side=1,at=c(1,2,3),labels=c("B: Arid","C: Temperate","D: Continental"))
axis(side=2,at=c(seq(-6,5,1)),labels=c(seq(-6,5,1)),las=1)
axis(side=3,at=c(1,2,3),labels=c("N=27","N=49","N=24"),tck=0,line=-2.5,col="white")
box()

#Table by individual climate
cooling_by_KGCC<-data.frame()
KGCC<-names(summary(as.factor(cities_100_raw_noNA$Climate[!is.na(cities_100_raw_noNA$delta_Tmean)])))

i=1
while(i<(length(KGCC)+1)){
  cooling_by_KGCC[1:5,i]<-c(length(cities_100_raw_noNA$delta_Tmean[cities_100_raw_noNA$Climate==KGCC[i]]),unname(summary(cities_100_raw_noNA$delta_Tmean[cities_100_raw_noNA$Climate==KGCC[i]]))[c(1,4,5)],sd(cities_100_raw_noNA$delta_Tmean[cities_100_raw_noNA$Climate==KGCC[i]]))
  i=i+1
}
cooling_by_KGCC<-cooling_by_KGCC[c(1,3,5,2,4),]
row.names(cooling_by_KGCC)<-c("N","Mean","SD","Min.","Max")
cities_100_raw_noNA$Climate_code_name<-paste(cities_100_raw_noNA$Climate,": ",cities_100_raw_noNA$name)
colnames(cooling_by_KGCC)<-names(summary(as.factor(cities_100_raw_noNA$Climate_code_name)))
cooling_by_KGCC<-data.frame(t(cooling_by_KGCC))
write.csv(cooling_by_KGCC,"C:/Users/Cheung Pui Kwan/Desktop/Temp/cooling_by_KGCC.csv")
write.csv(cities_100_raw_noNA,"C:/Users/Cheung Pui Kwan/Desktop/Temp/cities_100_delta_Tmean.csv")

