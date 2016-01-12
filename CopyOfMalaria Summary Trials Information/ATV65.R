##########################################
##
##  Data ## Oocysts
##
##
##########################################

atv65<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\ATV-65\\mosquito.txt",header=TRUE)
atv65$OocPrev<-ifelse(atv65$Oocyst==0,0,1)
summary(atv65)

###Oocyst intensity mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65$Oocyst[atv65$Round==1 & atv65$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65$Oocyst[atv65$Round==1 & atv65$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv65$Oocyst[atv65$Round==1 & atv65$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65$Oocyst[atv65$Round==1 & atv65$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv65$Oocyst[atv65$Round==1 & atv65$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65$Oocyst[atv65$Round==1 & atv65$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv65$Oocyst[atv65$Round==1 & atv65$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65$Oocyst[atv65$Round==1 & atv65$Bites==10])

###Oocyst prevalence mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65$OocPrev[atv65$Round==1 & atv65$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65$OocPrev[atv65$Round==1 & atv65$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65$OocPrev[atv65$Round==1 & atv65$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65$OocPrev[atv65$Round==1 & atv65$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65$OocPrev[atv65$Round==1 & atv65$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65$OocPrev[atv65$Round==1 & atv65$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65$OocPrev[atv65$Round==1 & atv65$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65$OocPrev[atv65$Round==1 & atv65$Bites==10])


##########################################
##
##  Data ## BloodStage and Sporozoites
##
##
##########################################

atv65b<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\ATV-65\\mouse.txt",header=TRUE)
atv65b$bloodstage<-ifelse(atv65b$Parasitemia==0,0,1)
summary(atv65b)


###Bloodstage mean and CI95%
##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65b$bloodstage[atv65b$Round==1 & atv65b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65b$bloodstage[atv65b$Round==1 & atv65b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65b$bloodstage[atv65b$Round==1 & atv65b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65b$bloodstage[atv65b$Round==1 & atv65b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65b$bloodstage[atv65b$Round==1 & atv65b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65b$bloodstage[atv65b$Round==1 & atv65b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv65b$bloodstage[atv65b$Round==1 & atv65b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv65b$bloodstage[atv65b$Round==1 & atv65b$Bites==10])


###Sporozoites mean and CI95%
##create prevalence
atv65b$sp1<-ifelse(atv65b$Sporozoite1==0,0,1)
atv65b$sp2<-ifelse(atv65b$Sporozoite2==0,0,1)
atv65b$sp3<-ifelse(atv65b$Sporozoite3==0,0,1)
atv65b$sp4<-ifelse(atv65b$Sporozoite4==0,0,1)
atv65b$sp5<-ifelse(atv65b$Sporozoite5==0,0,1)
atv65b$sp6<-ifelse(atv65b$Sporozoite6==0,0,1)
atv65b$sp7<-ifelse(atv65b$Sporozoite7==0,0,1)
atv65b$sp8<-ifelse(atv65b$Sporozoite8==0,0,1)
atv65b$sp9<-ifelse(atv65b$Sporozoite9==0,0,1)
atv65b$sp10<-ifelse(atv65b$Sporozoite10==0,0,1)


sp1a<-atv65b$sp1[atv65b$Round==1 & atv65b$Bites==1]

sp2a<-c(atv65b$sp1[atv65b$Round==1 & atv65b$Bites==2],atv65b$sp2[atv65b$Round==1 & atv65b$Bites==2])

sp5a<-c(atv65b$sp1[atv65b$Round==1 & atv65b$Bites==5],atv65b$sp2[atv65b$Round==1 & atv65b$Bites==5],
        atv65b$sp3[atv65b$Round==1 & atv65b$Bites==5],atv65b$sp4[atv65b$Round==1 & atv65b$Bites==5],
        atv65b$sp5[atv65b$Round==1 & atv65b$Bites==5])

sp10a<-c(atv65b$sp1[atv65b$Round==1 & atv65b$Bites==10],atv65b$sp2[atv65b$Round==1 & atv65b$Bites==10],
         atv65b$sp3[atv65b$Round==1 & atv65b$Bites==10],atv65b$sp4[atv65b$Round==1 & atv65b$Bites==10],
         atv65b$sp5[atv65b$Round==1 & atv65b$Bites==10],atv65b$sp6[atv65b$Round==1 & atv65b$Bites==10],
         atv65b$sp7[atv65b$Round==1 & atv65b$Bites==10],atv65b$sp8[atv65b$Round==1 & atv65b$Bites==10],
         atv65b$sp9[atv65b$Round==1 & atv65b$Bites==10],atv65b$sp10[atv65b$Round==1 & atv65b$Bites==10])

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
Sporozoite1a<-atv65b$Sporozoite1[atv65b$Round==1 & atv65b$Bites==1]

Sporozoite2a<-c(atv65b$Sporozoite1[atv65b$Round==1 & atv65b$Bites==2],atv65b$Sporozoite2[atv65b$Round==1 & atv65b$Bites==2])

Sporozoite5a<-c(atv65b$Sporozoite1[atv65b$Round==1 & atv65b$Bites==5],atv65b$Sporozoite2[atv65b$Round==1 & atv65b$Bites==5],
                atv65b$Sporozoite3[atv65b$Round==1 & atv65b$Bites==5],atv65b$Sporozoite4[atv65b$Round==1 & atv65b$Bites==5],
                atv65b$Sporozoite5[atv65b$Round==1 & atv65b$Bites==5])

Sporozoite10a<-c(atv65b$Sporozoite1[atv65b$Round==1 & atv65b$Bites==10],atv65b$Sporozoite2[atv65b$Round==1 & atv65b$Bites==10],
                 atv65b$Sporozoite3[atv65b$Round==1 & atv65b$Bites==10],atv65b$Sporozoite4[atv65b$Round==1 & atv65b$Bites==10],
                 atv65b$Sporozoite5[atv65b$Round==1 & atv65b$Bites==10],atv65b$Sporozoite6[atv65b$Round==1 & atv65b$Bites==10],
                 atv65b$Sporozoite7[atv65b$Round==1 & atv65b$Bites==10],atv65b$Sporozoite8[atv65b$Round==1 & atv65b$Bites==10],
                 atv65b$Sporozoite9[atv65b$Round==1 & atv65b$Bites==10],atv65b$Sporozoite10[atv65b$Round==1 & atv65b$Bites==10])

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


is.mosi<-0
n.rounds=4
n.bites=4
t.bites<-c(1,2,5,10)


par(mfcol=c(4,5),mar= c(3, 4, 1, 2))

is.mosi<-0

##Controls: mouse read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=T)
##Controls: mosquito read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocysts.txt",header=T)

##ATV_65%
##Bloodstage
prevH.round.1<-matrix(ncol=4,nrow=4,c(0,0,0.2,0,rep(0,12)))
prevH.lower.1<-matrix(ncol=4,nrow=4,c(0,0,0,0,rep(0,12)))
prevH.upper.1<-matrix(ncol=4,nrow=4,c(0,0,0.6,0,rep(0,12)))

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
prevMO.round.1<-matrix(ncol=3,nrow=4,c(0,0,0.2,0,rep(0,8)))
prevMO.lower.1<-matrix(ncol=3,nrow=4,c(0,0,0,0,rep(0,8)))
prevMO.upper.1<-matrix(ncol=3,nrow=4,c(0,0,0.6,0,rep(0,8)))

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
prevMOI.round.1<-matrix(ncol=3,nrow=4,c(0,0,0.4653465,0,rep(0,8)))
bootMOI.lower1<-matrix(ncol=3,nrow=4,c(0,0,0.04950495,0,rep(0,8)))
bootMOI.upper1<-matrix(ncol=3,nrow=4,c(0,0,1.05940594,0,rep(0,8)))

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
prevMOS.round.1<-matrix(ncol=3,nrow=4,rep(0,12))
prevMOS.lower.1<-matrix(ncol=3,nrow=4,rep(0,12))
prevMOS.upper.1<-matrix(ncol=3,nrow=4,rep(0,12))

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
MOSI.round.1<-matrix(ncol=3,nrow=4,rep(0,12))
bootMOSI.lower1<-matrix(ncol=3,nrow=4,rep(0,12))
bootMOSI.upper1<-matrix(ncol=3,nrow=4,rep(0,12))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}


