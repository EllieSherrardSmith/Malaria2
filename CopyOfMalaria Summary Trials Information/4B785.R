##########################################
##
##  Data ## Oocysts
##
##
##########################################

b7_85<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\4B7-85\\mosquito.txt",header=TRUE)
b7_85$OocPrev<-ifelse(b7_85$Oocyst==0,0,1)
summary(b7_85)

###Oocyst intensity mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85$Oocyst[b7_85$Round==1 & b7_85$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85$Oocyst[b7_85$Round==1 & b7_85$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(b7_85$Oocyst[b7_85$Round==1 & b7_85$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85$Oocyst[b7_85$Round==1 & b7_85$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(b7_85$Oocyst[b7_85$Round==1 & b7_85$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85$Oocyst[b7_85$Round==1 & b7_85$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(b7_85$Oocyst[b7_85$Round==1 & b7_85$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85$Oocyst[b7_85$Round==1 & b7_85$Bites==10])


###Oocyst prevalence mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85$OocPrev[b7_85$Round==1 & b7_85$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85$OocPrev[b7_85$Round==1 & b7_85$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85$OocPrev[b7_85$Round==1 & b7_85$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85$OocPrev[b7_85$Round==1 & b7_85$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85$OocPrev[b7_85$Round==1 & b7_85$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85$OocPrev[b7_85$Round==1 & b7_85$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85$OocPrev[b7_85$Round==1 & b7_85$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85$OocPrev[b7_85$Round==1 & b7_85$Bites==10])


##########################################
##
##  Data ## BloodStage and Sporozoites
##
##
##########################################

b7_85b<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\4B7-85\\mouse.txt",header=TRUE)
b7_85b$bloodstage<-ifelse(b7_85b$Parasitemia==0,0,1)
summary(b7_85b)


###Bloodstage mean and CI95%
##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85b$bloodstage[b7_85b$Round==1 & b7_85b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85b$bloodstage[b7_85b$Round==1 & b7_85b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85b$bloodstage[b7_85b$Round==1 & b7_85b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85b$bloodstage[b7_85b$Round==1 & b7_85b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85b$bloodstage[b7_85b$Round==1 & b7_85b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85b$bloodstage[b7_85b$Round==1 & b7_85b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(b7_85b$bloodstage[b7_85b$Round==1 & b7_85b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(b7_85b$bloodstage[b7_85b$Round==1 & b7_85b$Bites==10])



###Sporozoites mean and CI95%
##create prevalence
b7_85b$sp1<-ifelse(b7_85b$Sporozoite1==0,0,1)
b7_85b$sp2<-ifelse(b7_85b$Sporozoite2==0,0,1)
b7_85b$sp3<-ifelse(b7_85b$Sporozoite3==0,0,1)
b7_85b$sp4<-ifelse(b7_85b$Sporozoite4==0,0,1)
b7_85b$sp5<-ifelse(b7_85b$Sporozoite5==0,0,1)
b7_85b$sp6<-ifelse(b7_85b$Sporozoite6==0,0,1)
b7_85b$sp7<-ifelse(b7_85b$Sporozoite7==0,0,1)
b7_85b$sp8<-ifelse(b7_85b$Sporozoite8==0,0,1)
b7_85b$sp9<-ifelse(b7_85b$Sporozoite9==0,0,1)
b7_85b$sp10<-ifelse(b7_85b$Sporozoite10==0,0,1)


sp1a<-b7_85b$sp1[b7_85b$Round==1 & b7_85b$Bites==1]

sp2a<-c(b7_85b$sp1[b7_85b$Round==1 & b7_85b$Bites==2],b7_85b$sp2[b7_85b$Round==1 & b7_85b$Bites==2])

sp5a<-c(b7_85b$sp1[b7_85b$Round==1 & b7_85b$Bites==5],b7_85b$sp2[b7_85b$Round==1 & b7_85b$Bites==5],
        b7_85b$sp3[b7_85b$Round==1 & b7_85b$Bites==5],b7_85b$sp4[b7_85b$Round==1 & b7_85b$Bites==5],
        b7_85b$sp5[b7_85b$Round==1 & b7_85b$Bites==5])

sp10a<-c(b7_85b$sp1[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp2[b7_85b$Round==1 & b7_85b$Bites==10],
         b7_85b$sp3[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp4[b7_85b$Round==1 & b7_85b$Bites==10],
         b7_85b$sp5[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp6[b7_85b$Round==1 & b7_85b$Bites==10],
         b7_85b$sp7[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp8[b7_85b$Round==1 & b7_85b$Bites==10],
         b7_85b$sp9[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp10[b7_85b$Round==1 & b7_85b$Bites==10])

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
mean(sp5a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp10a,na.rm=T)


###Sporozoite intensity
Sporozoite1a<-b7_85b$Sporozoite1[b7_85b$Round==1 & b7_85b$Bites==1]

Sporozoite2a<-c(b7_85b$Sporozoite1[b7_85b$Round==1 & b7_85b$Bites==2],b7_85b$Sporozoite2[b7_85b$Round==1 & b7_85b$Bites==2])

Sporozoite5a<-c(b7_85b$Sporozoite1[b7_85b$Round==1 & b7_85b$Bites==5],b7_85b$Sporozoite2[b7_85b$Round==1 & b7_85b$Bites==5],
                b7_85b$Sporozoite3[b7_85b$Round==1 & b7_85b$Bites==5],b7_85b$Sporozoite4[b7_85b$Round==1 & b7_85b$Bites==5],
                b7_85b$Sporozoite5[b7_85b$Round==1 & b7_85b$Bites==5])

Sporozoite10a<-c(b7_85b$Sporozoite1[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite2[b7_85b$Round==1 & b7_85b$Bites==10],
                 b7_85b$Sporozoite3[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite4[b7_85b$Round==1 & b7_85b$Bites==10],
                 b7_85b$Sporozoite5[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite6[b7_85b$Round==1 & b7_85b$Bites==10],
                 b7_85b$Sporozoite7[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite8[b7_85b$Round==1 & b7_85b$Bites==10],
                 b7_85b$Sporozoite9[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite10[b7_85b$Round==1 & b7_85b$Bites==10])


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
  mp <- barplot(hh*100, 
                col = c("dodgerblue4"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100 , col = mybarcol, lwd = 1.5)
  
}

##Oocyst prevalence
prevMO.round.1<-matrix(ncol=3,nrow=4,c(0,0,0.04,0.10,rep(0,8)))
prevMO.lower.1<-matrix(ncol=3,nrow=4,c(0,0,0.00,0.02,rep(0,8)))
prevMO.upper.1<-matrix(ncol=3,nrow=4,c(0,0,0.10,0.18,rep(0,8)))

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
prevMOS.round.1<-matrix(ncol=3,nrow=4,c(0.2,0.111,0.04,0.10,rep(0,8)))
prevMOS.lower.1<-matrix(ncol=3,nrow=4,c(0.0,0.000,0.00,0.02,rep(0,8)))
prevMOS.upper.1<-matrix(ncol=3,nrow=4,c(0.6,0.333,0.12,0.18,rep(0,8)))

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
MOSI.round.1<-matrix(ncol=3, nrow = 4,c(0.2,0.1111,0.04,0.16,rep(0,8)))
bootMOSI.lower1<-matrix(ncol=3,nrow=4,c(0.0,0.0000,0.00,0.04,rep(0,8)))
bootMOSI.upper1<-matrix(ncol=3,nrow=4,c(0.6,0.3333,0.12,0.32,rep(0,8)))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}

