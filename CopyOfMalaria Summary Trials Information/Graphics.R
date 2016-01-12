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
data.mosi=read.table("E:\\IMPERIAL Nov 2014\\ATV-25 Example\\mosquito.txt",header=TRUE)
data.mouse=read.table("E:\\IMPERIAL Nov 2014\\ATV-25 Example\\mouse.txt",header=TRUE)
data.all<-if(is.mosi==1) data.mosi else data.mouse
Parasite<-if(is.mosi==0) data.all$Parasitemia else data.all$Oocyst
prev<-as.factor(ifelse(Parasite>0,1,0))
Treatment<-as.factor(data.all$Treatment)
Bites<-as.factor(abs(data.all$Bites))
Rond<-as.factor(data.all$Round)
my.id<-seq(1,length(Rond),1)
inf.data<-data.frame(my.id,prev,Parasite,Treatment,Bites,Rond)
summary(inf.data)


prevH.round.1<-matrix(ncol=4,nrow=4)
prevH.lower.1<-matrix(ncol=4,nrow=4)
prevH.upper.1<-matrix(ncol=4,nrow=4)

for(mb in 1:4){
  results.table1<-binom.confint(x = as.numeric(table(prev[Treatment==1&Bites==t.bites[mb]],Rond[Treatment==1&Bites==t.bites[mb]])[2,]), 
                                n = as.numeric(table(Treatment[Bites==t.bites[mb]],Rond[Bites==t.bites[mb]])[1,]),methods="wilson")
  
  
  prevH.round.1[mb,]<-as.numeric(results.table1[,4])*100
  
  prevH.lower.1[mb,]<-as.numeric(results.table1[,5])*100
  
  prevH.upper.1[mb,]<-as.numeric(results.table1[,6])*100
}


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



is.mosi<-1
data.mosi=read.table("Q:/1.Papers/Malaria/TBV Group/MVI Mouse/ATV25%/mosquito.txt",header=TRUE)
data.mouse=read.table("Q:/1.Papers/Malaria/TBV Group/MVI Mouse/ATV25%/mouse.txt",header=TRUE)
data.all<-if(is.mosi==1) data.mosi else data.mouse
Parasite<-if(is.mosi==0) data.all$Parasitemia else data.all$Oocyst
prev<-as.factor(ifelse(Parasite>0,1,0))
Treatment<-as.factor(data.all$Treatment)
Bites<-as.factor(abs(data.all$Bites))
Rond<-as.factor(data.all$Round)
my.id<-seq(1,length(Rond),1)
inf.data<-data.frame(my.id,prev,Parasite,Treatment,Bites,Rond)
summary(inf.data)

prevH.round.1<-matrix(ncol=3,nrow=4)
prevH.lower.1<-matrix(ncol=3,nrow=4)
prevH.upper.1<-matrix(ncol=3,nrow=4)


for(mb in 1:4){
  results.table1<-binom.confint(x = as.numeric(table(prev[Treatment==1&Bites==t.bites[mb]],Rond[Treatment==1&Bites==t.bites[mb]])[2,]), 
                                n = as.numeric(table(Treatment[Bites==t.bites[mb]],Rond[Bites==t.bites[mb]])[1,]),methods="wilson")
  
  prevH.round.1[mb,]<-as.numeric(results.table1[,4])*100
  
  prevH.lower.1[mb,]<-as.numeric(results.table1[,5])*100
  
  prevH.upper.1[mb,]<-as.numeric(results.table1[,6])*100
}


for(mb in 1:4){
  
  hh=rbind(prevH.round.1[mb,])
  #hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevH.lower.1[mb,])
  ciU<-rbind(prevH.upper.1[mb,])
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
  #text(colMeans(mp)+0.2,hh.max,  labels =less.than.001[mb,], col = "red")
  
  #stopifnot(dim(mp) == dim(hh))# corresponding matrices
  #mtext(side = 1, at = colMeans(mp), line = 2,
  #      text = paste("efficacy", formatC(colMeans(hh))), col = "red")
  
}



less.than.001[5,2:5]="**"


prevH.round.1<-matrix(ncol=3,nrow=4)
boot.lower1<-matrix(ncol=3,nrow=4)
boot.upper1<-matrix(ncol=3,nrow=4)
n.boots<-1000
boot.fun1<-numeric(n.boots)

for(mb in 1:4){
  for(mr in 1:3){
    model.values1<-Parasite[Treatment==1&Bites==t.bites[mb]&Rond==mr] 
    for(b in 1:n.boots){
      boot.fun1[b]<-mean(sample(model.values1,size=length(model.values1),replace = TRUE))}
    boot.lower1[mb,mr]<-sort(boot.fun1)[0.025*n.boots]
    boot.upper1[mb,mr]<-sort(boot.fun1)[0.975*n.boots]
    
    prevH.round.1[mb,mr]<-mean(model.values1)
  }}



for(mb in 1:4){
  
  hh=rbind(prevH.round.1[mb,])
  hh.max<-apply(hh, 2, max)+5
  ciL<-rbind(boot.lower1[mb,])
  ciU<-rbind(boot.upper1[mb,])
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







##sporozoite score
summary(data.mouse)

sporo.data<-data.mouse[6:15]
s.bites<-data.mouse$Bites
s.round<-data.mouse$Round

sporo.mean<-matrix(ncol=n.rounds-1,nrow=n.bites)
sporo.mean.l<-matrix(ncol=n.rounds-1,nrow=n.bites)
sporo.mean.u<-matrix(ncol=n.rounds-1,nrow=n.bites)
sporo.prev<-matrix(ncol=n.rounds-1,nrow=n.bites)
sporo.prev.l<-matrix(ncol=n.rounds-1,nrow=n.bites)
sporo.prev.u<-matrix(ncol=n.rounds-1,nrow=n.bites)



for(nb in 1:4){
  for(mb in 1:3){
    sporos<-as.numeric(na.omit(as.numeric(unlist(sporo.data[s.bites==t.bites[nb]&s.round==mb,]))))
    prev.sporos<-ifelse(sporos>0,1,0)
    sporo.mean[nb,mb]<-mean(sporos)
    sporo.prev[nb,mb]<-mean(prev.sporos)
    
    my.boots<-numeric(n.boots)
    for(b in 1:n.boots){
      my.boots[b]<-mean(sample(sporos,length(sporos), replace = TRUE))}
    
    sporo.mean.l[nb,mb]<-as.numeric(quantile(my.boots,prob=c(0.025)))
    sporo.mean.u[nb,mb]<-as.numeric(quantile(my.boots,prob=c(0.975)))
    
    for(b in 1:n.boots){
      my.boots[b]<-mean(sample(prev.sporos,length(sporos), replace = TRUE))}
    
    sporo.prev.l[nb,mb]<-as.numeric(quantile(my.boots,prob=c(0.025)))
    sporo.prev.u[nb,mb]<-as.numeric(quantile(my.boots,prob=c(0.975)))
  }
}


for(nb in 1:4){
  
  colnames(sporo.prev)<-c(1,2,3)
  mp <- barplot(sporo.prev[nb,]*100,col = "orange",cex.names = 1.4,ylim=c(0,100))
  
  par(las=1,col.axis="black")
  
  segments(mp, sporo.prev.l[nb,]*100, mp, sporo.prev.u[nb,]*100 , col = mybarcol, lwd = 1.5)
  
}

for(nb in 1:4){
  
  colnames(sporo.mean)<-c(1,2,3)
  mp <- barplot(sporo.mean[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, sporo.mean.l[nb,], mp, sporo.mean.u[nb,] , col = mybarcol, lwd = 1.5)
  
}
