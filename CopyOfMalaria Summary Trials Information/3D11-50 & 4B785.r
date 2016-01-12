##########################################
##
##  Data ## Oocysts
##
##
##########################################
d114b7=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 50and 4B7 85\\mosquito.txt",header=T)

d114b7$OocPrev<-ifelse(d114b7$Oocyst==0,0,1)
summary(d114b7)

###Oocyst intensity mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7$Oocyst[d114b7$Round==1 & d114b7$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7$Oocyst[d114b7$Round==1 & d114b7$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(d114b7$Oocyst[d114b7$Round==1 & d114b7$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7$Oocyst[d114b7$Round==1 & d114b7$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(d114b7$Oocyst[d114b7$Round==1 & d114b7$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7$Oocyst[d114b7$Round==1 & d114b7$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(d114b7$Oocyst[d114b7$Round==1 & d114b7$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7$Oocyst[d114b7$Round==1 & d114b7$Bites==10])
###Oocyst prevalence mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7$OocPrev[d114b7$Round==1 & d114b7$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7$OocPrev[d114b7$Round==1 & d114b7$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7$OocPrev[d114b7$Round==1 & d114b7$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7$OocPrev[d114b7$Round==1 & d114b7$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7$OocPrev[d114b7$Round==1 & d114b7$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7$OocPrev[d114b7$Round==1 & d114b7$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7$OocPrev[d114b7$Round==1 & d114b7$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7$OocPrev[d114b7$Round==1 & d114b7$Bites==10])
##########################################
##
##  Data ## BloodStage and Sporozoites
##
##
##########################################

d114b7b<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 50and 4B7 85\\mouse.txt",header=T)
d114b7b$bloodstage<-ifelse(d114b7b$Parasitemia==0,0,1)
summary(d114b7b)


###Bloodstage mean and CI95%
##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7b$bloodstage[d114b7b$Round==1 & d114b7b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7b$bloodstage[d114b7b$Round==1 & d114b7b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7b$bloodstage[d114b7b$Round==1 & d114b7b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7b$bloodstage[d114b7b$Round==1 & d114b7b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7b$bloodstage[d114b7b$Round==1 & d114b7b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7b$bloodstage[d114b7b$Round==1 & d114b7b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d114b7b$bloodstage[d114b7b$Round==1 & d114b7b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d114b7b$bloodstage[d114b7b$Round==1 & d114b7b$Bites==10])


###Sporozoites mean and CI95%
##create prevalence
d114b7b$sp1<-ifelse(d114b7b$Sporozoite1==0,0,1)
d114b7b$sp2<-ifelse(d114b7b$Sporozoite2==0,0,1)
d114b7b$sp3<-ifelse(d114b7b$Sporozoite3==0,0,1)
d114b7b$sp4<-ifelse(d114b7b$Sporozoite4==0,0,1)
d114b7b$sp5<-ifelse(d114b7b$Sporozoite5==0,0,1)
d114b7b$sp6<-ifelse(d114b7b$Sporozoite6==0,0,1)
d114b7b$sp7<-ifelse(d114b7b$Sporozoite7==0,0,1)
d114b7b$sp8<-ifelse(d114b7b$Sporozoite8==0,0,1)
d114b7b$sp9<-ifelse(d114b7b$Sporozoite9==0,0,1)
d114b7b$sp10<-ifelse(d114b7b$Sporozoite10==0,0,1)


sp1a<-d114b7b$sp1[d114b7b$Round==1 & d114b7b$Bites==1]

sp2a<-c(d114b7b$sp1[d114b7b$Round==1 & d114b7b$Bites==2],d114b7b$sp2[d114b7b$Round==1 & d114b7b$Bites==2])

sp5a<-c(d114b7b$sp1[d114b7b$Round==1 & d114b7b$Bites==5],d114b7b$sp2[d114b7b$Round==1 & d114b7b$Bites==5],
        d114b7b$sp3[d114b7b$Round==1 & d114b7b$Bites==5],d114b7b$sp4[d114b7b$Round==1 & d114b7b$Bites==5],
        d114b7b$sp5[d114b7b$Round==1 & d114b7b$Bites==5])

sp10a<-c(d114b7b$sp1[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$sp2[d114b7b$Round==1 & d114b7b$Bites==10],
         d114b7b$sp3[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$sp4[d114b7b$Round==1 & d114b7b$Bites==10],
         d114b7b$sp5[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$sp6[d114b7b$Round==1 & d114b7b$Bites==10],
         d114b7b$sp7[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$sp8[d114b7b$Round==1 & d114b7b$Bites==10],
         d114b7b$sp9[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$sp10[d114b7b$Round==1 & d114b7b$Bites==10])


##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp1a)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp2a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp5a)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp10a,na.rm=T)


###Sporozoite intensity
Sporozoite1a<-d114b7b$Sporozoite1[d114b7b$Round==1 & d114b7b$Bites==1]

Sporozoite2a<-c(d114b7b$Sporozoite1[d114b7b$Round==1 & d114b7b$Bites==2],d114b7b$Sporozoite2[d114b7b$Round==1 & d114b7b$Bites==2])

Sporozoite5a<-c(d114b7b$Sporozoite1[d114b7b$Round==1 & d114b7b$Bites==5],d114b7b$Sporozoite2[d114b7b$Round==1 & d114b7b$Bites==5],
                d114b7b$Sporozoite3[d114b7b$Round==1 & d114b7b$Bites==5],d114b7b$Sporozoite4[d114b7b$Round==1 & d114b7b$Bites==5],
                d114b7b$Sporozoite5[d114b7b$Round==1 & d114b7b$Bites==5])

Sporozoite10a<-c(d114b7b$Sporozoite1[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$Sporozoite2[d114b7b$Round==1 & d114b7b$Bites==10],
                 d114b7b$Sporozoite3[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$Sporozoite4[d114b7b$Round==1 & d114b7b$Bites==10],
                 d114b7b$Sporozoite5[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$Sporozoite6[d114b7b$Round==1 & d114b7b$Bites==10],
                 d114b7b$Sporozoite7[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$Sporozoite8[d114b7b$Round==1 & d114b7b$Bites==10],
                 d114b7b$Sporozoite9[d114b7b$Round==1 & d114b7b$Bites==10],d114b7b$Sporozoite10[d114b7b$Round==1 & d114b7b$Bites==10])

##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite1a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite2a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite5a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite5a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite10a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite10a,na.rm=T)


rm(list = ls())
require(bbmle)
require(hacks)
require(VGAM)
require(emdbook)
require(lme4)
require(boot)
library(reshape)
library("glmmADMB")
require(binom)
require(RColorBrewer)

is.mosi<-0
n.rounds=1
n.bites=4
t.bites<-c(1,2,5,10)


par(mfcol=c(4,5),mar= c(1, 4, 1, 2))

is.mosi<-0

##Controls: mouse read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=T)
##Controls: mosquito read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocysts.txt",header=T)

##4B7_85%
##Bloodstage
prevH.round.1<-matrix(ncol=4,nrow=4,rep(0,4))
prevH.lower.1<-matrix(ncol=4,nrow=4,rep(0,4))
prevH.upper.1<-matrix(ncol=4,nrow=4,rep(0,4))

for(mb in 1:4){
  
  hh=rbind(prevH.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevH.lower.1[mb,])
  ciU<-rbind(prevH.upper.1[mb,])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100, 
                col = c("dodgerblue4"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100 , col = mybarcol, lwd = 1.5)
  
}

##Oocyst prevalence
prevMO.round.1<-matrix(ncol=3,nrow=4,rep(0,4))
prevMO.lower.1<-matrix(ncol=3,nrow=4,rep(0,4))
prevMO.upper.1<-matrix(ncol=3,nrow=4,rep(0,4))

for(mb in 1:4){
  
  hh=rbind(prevMO.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMO.lower.1[mb,])
  ciU<-rbind(prevMO.upper.1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100, 
                col = c("forestgreen"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100 , col = mybarcol, lwd = 1.5)
  
}
##Oocyst intensity
prevMOI.round.1<-matrix(ncol=3,nrow=4,rep(0,4))
bootMOI.lower1<-matrix(ncol=3,nrow=4,rep(0,4))
bootMOI.upper1<-matrix(ncol=3,nrow=4,rep(0,4))

for(mb in 1:4){
  
  hh=rbind(prevMOI.round.1[mb,])
  hh.max<-apply(hh, 2, max)+5
  ciL<-rbind(bootMOI.lower1[mb,])
  ciU<-rbind(bootMOI.upper1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100,
                col = c("firebrick3"),
                ylim= c(0,45),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100 , col = mybarcol, lwd = 1.5)
  #text(colMeans(mp)+0.2,hh.max,  labels =less.than.001[mb,], col = "red")
  
  #mtext(side = 1, at = colMeans(mp), line = 2,
  #      text = paste("efficacy", formatC(colMeans(hh))), col = "red")
  
}

##Sporozoite prevalence
prevMOS.round.1<-matrix(ncol=3,nrow=4,c(0,0.1,0.20,0.1632653,rep(0,8)))
prevMOS.lower.1<-matrix(ncol=3,nrow=4,c(0,0.0,0.04,0.0612245,rep(0,8)))
prevMOS.upper.1<-matrix(ncol=3,nrow=4,c(0,0.3,0.36,0.2708333,rep(0,8)))

for(mb in 1:4){
  
  hh=rbind(prevMOS.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMOS.lower.1[mb,])
  ciU<-rbind(prevMOS.upper.1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100, 
                col = c("orange"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100 , col = mybarcol, lwd = 1.5)
  
}

##Sporozoite intensity
MOSI.round.1<-matrix(ncol=3, nrow = 4,c(0,0.2,0.28,0.244898,rep(0,8)))
bootMOSI.lower1<-matrix(ncol=3,nrow=4,c(0,0.0,0.08,0.1,rep(0,8)))
bootMOSI.upper1<-matrix(ncol=3,nrow=4,c(0,0.6,0.52,0.42,rep(0,8)))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}
