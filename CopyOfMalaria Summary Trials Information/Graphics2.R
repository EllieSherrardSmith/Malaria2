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
n.rounds=4
n.bites=4
t.bites<-c(1,2,5,10)


par(mfcol=c(4,5),mar= c(3, 4, 1, 2))

is.mosi<-0

##Controls: mouse read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=T)
##Controls: mosquito read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocysts.txt",header=T)

##4B7_85%
##Bloodstage
prevH.round.1<-matrix(ncol=4,nrow=4,rep(0,16))
prevH.lower.1<-matrix(ncol=4,nrow=4,rep(0,16))
prevH.upper.1<-matrix(ncol=4,nrow=4,rep(0,16))

for(mb in 1:4){
  
  hh=rbind(prevH.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevH.lower.1[mb,])
  ciU<-rbind(prevH.upper.1[mb,])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh, 
                col = c("dodgerblue4"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  
}

##Oocyst prevalence
prevMO.round.1<-matrix(ncol=3,nrow=4,c(0,0,4,10,rep(0,8)))
prevMO.lower.1<-matrix(ncol=3,nrow=4,c(0,0,0,2,rep(0,8)))
prevMO.upper.1<-matrix(ncol=3,nrow=4,c(0,0,10,20,rep(0,8)))

for(mb in 1:4){
  
  hh=rbind(prevMO.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMO.lower.1[mb,])
  ciU<-rbind(prevMO.upper.1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh, 
                col = c("forestgreen"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  
}
##Oocyst intensity
prevMOI.round.1<-matrix(ncol=3,nrow=4,c(0,0,0.06,0.18,rep(0,8)))
bootMOI.lower1<-matrix(ncol=3,nrow=4,c(0,0,0,0.04,rep(0,8)))
bootMOI.upper1<-matrix(ncol=3,nrow=4,c(0,0,0.16,0.36,rep(0,8)))

for(mb in 1:4){
  
  hh=rbind(prevMOI.round.1[mb,])
  hh.max<-apply(hh, 2, max)+5
  ciL<-rbind(bootMOI.lower1[mb,])
  ciU<-rbind(bootMOI.upper1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  
  mybarcol <- "gray20"
  mp <- barplot(hh,
                col = c("firebrick3"),
                ylim= c(0,45),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  #text(colMeans(mp)+0.2,hh.max,  labels =less.than.001[mb,], col = "red")
  
  #mtext(side = 1, at = colMeans(mp), line = 2,
  #      text = paste("efficacy", formatC(colMeans(hh))), col = "red")
  
}

##Sporozoite prevalence
prevMOS.round.1<-matrix(ncol=3,nrow=4,c(20,11,4,10,rep(0,8)))
prevMOS.lower.1<-matrix(ncol=3,nrow=4,c(0,0,0,2,rep(0,8)))
prevMOS.upper.1<-matrix(ncol=3,nrow=4,c(60,33,12,20,rep(0,8)))

for(mb in 1:4){
  
  hh=rbind(prevMOS.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMOS.lower.1[mb,])
  ciU<-rbind(prevMOS.upper.1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh, 
                col = c("orange"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  
}
##Sporozoite intensity
MOSI.round.1<-matrix(ncol=3,nrow=4,c(0.2,0.11,0.04,0.16,rep(0,8)))
bootMOSI.lower1<-matrix(ncol=3,nrow=4,c(0,0,0,0.04,rep(0,8)))
bootMOSI.upper1<-matrix(ncol=3,nrow=4,c(0.6,0.33,0.12,0.32,rep(0,8)))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}


###########################################################
###########################################################

is.mosi<-0
n.rounds=4
n.bites=4
t.bites<-c(1,2,5,10)


par(mfcol=c(4,5),mar= c(3, 4, 1, 2))

is.mosi<-0

##ATV_85%
##Bloodstage
prevH.round.1<-matrix(ncol=4,nrow=4,c(0,20,40,0,rep(0,12)))
prevH.lower.1<-matrix(ncol=4,nrow=4,c(0,60,80,0,rep(0,12)))
prevH.upper.1<-matrix(ncol=4,nrow=4,rep(0,16))

for(mb in 1:4){
  
  hh=rbind(prevH.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevH.lower.1[mb,])
  ciU<-rbind(prevH.upper.1[mb,])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh, 
                col = c("dodgerblue4"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  
}

##Oocyst prevalence
prevMO.round.1<-matrix(ncol=3,nrow=4,c(0,8,2,0,rep(0,8)))
prevMO.lower.1<-matrix(ncol=3,nrow=4,c(0,2,0,0,rep(0,8)))
prevMO.upper.1<-matrix(ncol=3,nrow=4,c(0,16,6,0,rep(0,8)))

for(mb in 1:4){
  
  hh=rbind(prevMO.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMO.lower.1[mb,])
  ciU<-rbind(prevMO.upper.1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh, 
                col = c("forestgreen"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  
}
##Oocyst intensity
prevMOI.round.1<-matrix(ncol=3,nrow=4,c(0,1.82,0.16,0,rep(0,8)))
bootMOI.lower1<-matrix(ncol=3,nrow=4,c(0,2,0,0,rep(0,8)))
bootMOI.upper1<-matrix(ncol=3,nrow=4,c(0,4.4,0.48,0,rep(0,8)))

for(mb in 1:4){
  
  hh=rbind(prevMOI.round.1[mb,])
  hh.max<-apply(hh, 2, max)+5
  ciL<-rbind(bootMOI.lower1[mb,])
  ciU<-rbind(bootMOI.upper1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  
  mybarcol <- "gray20"
  mp <- barplot(hh,
                col = c("firebrick3"),
                ylim= c(0,45),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  #text(colMeans(mp)+0.2,hh.max,  labels =less.than.001[mb,], col = "red")
  
  #mtext(side = 1, at = colMeans(mp), line = 2,
  #      text = paste("efficacy", formatC(colMeans(hh))), col = "red")
  
}

##Sporozoite prevalence
prevMOS.round.1<-matrix(ncol=3,nrow=4,c(0,10,13,2,rep(0,8)))
prevMOS.lower.1<-matrix(ncol=3,nrow=4,c(0,0,0,0,rep(0,8)))
prevMOS.upper.1<-matrix(ncol=3,nrow=4,c(0,30,29,6,rep(0,8)))

for(mb in 1:4){
  
  hh=rbind(prevMOS.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMOS.lower.1[mb,])
  ciU<-rbind(prevMOS.upper.1[mb,])
  colnames(hh)<-seq(1,3,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh, 
                col = c("orange"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  
}
##Sporozoite intensity
MOSI.round.1<-matrix(ncol=3,nrow=4,c(0,0.2,0.26,0.02,rep(0,8)))
bootMOSI.lower1<-matrix(ncol=3,nrow=4,c(0,0,0,0.04,rep(0,8)))
bootMOSI.upper1<-matrix(ncol=3,nrow=4,c(0,0.6,0.56,0.06,rep(0,8)))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}
