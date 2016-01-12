
conb<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse3AddingCombinations.txt",header=TRUE)
conb$bloodstage<-ifelse(conb$Parasitemia==0,0,1)
summary(conb)


###Bloodstage mean and CI95%
##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==1 & conb$Bites==1],replace=T),na.rm=T)
prevH.lower.1[1,1]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[1,1]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[1,1]<-mean(conb$bloodstage[conb$Round==1 & conb$Bites==1],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==1 & conb$Bites==2],replace=T),na.rm=T)
prevH.lower.1[2,1]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[2,1]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[2,1]<-mean(conb$bloodstage[conb$Round==1 & conb$Bites==2],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==1 & conb$Bites==3],replace=T),na.rm=T)
prevH.lower.1[3,1]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[3,1]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[3,1]<-mean(conb$bloodstage[conb$Round==1 & conb$Bites==3],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==1 & conb$Bites==4],replace=T),na.rm=T)
prevH.lower.1[4,1]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[4,1]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[4,1]<-mean(conb$bloodstage[conb$Round==1 & conb$Bites==4],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==1 & conb$Bites==5],replace=T),na.rm=T)
prevH.lower.1[5,1]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[5,1]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[5,1]<-mean(conb$bloodstage[conb$Round==1 & conb$Bites==5],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==1 & conb$Bites==10],replace=T),na.rm=T)
prevH.lower.1[6,1]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[6,1]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[6,1]<-mean(conb$bloodstage[conb$Round==1 & conb$Bites==10],na.rm=TRUE)

##Round 2

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==2 & conb$Bites==1],replace=T),na.rm=T)
prevH.lower.1[1,2]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[1,2]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[1,2]<-mean(conb$bloodstage[conb$Round==2 & conb$Bites==1],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==2 & conb$Bites==2],replace=T),na.rm=T)
prevH.lower.1[2,2]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[2,2]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[2,2]<-mean(conb$bloodstage[conb$Round==2 & conb$Bites==2],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==2 & conb$Bites==3],replace=T),na.rm=T)
prevH.lower.1[3,2]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[3,2]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[3,2]<-mean(conb$bloodstage[conb$Round==2 & conb$Bites==3],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==2 & conb$Bites==4],replace=T),na.rm=T)
prevH.lower.1[4,2]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[4,2]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[4,2]<-mean(conb$bloodstage[conb$Round==2 & conb$Bites==4],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==2 & conb$Bites==5],replace=T),na.rm=T)
prevH.lower.1[5,2]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[5,2]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[5,2]<-mean(conb$bloodstage[conb$Round==2 & conb$Bites==5],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==2 & conb$Bites==10],replace=T),na.rm=T)
prevH.lower.1[6,2]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[6,2]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[6,2]<-mean(conb$bloodstage[conb$Round==2 & conb$Bites==10],na.rm=TRUE)

##Round 3

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==3 & conb$Bites==1],replace=T),na.rm=T)
prevH.lower.1[1,3]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[1,3]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[1,3]<-mean(conb$bloodstage[conb$Round==3 & conb$Bites==1],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==3 & conb$Bites==2],replace=T),na.rm=T)
prevH.lower.1[2,3]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[2,3]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[2,3]<-mean(conb$bloodstage[conb$Round==3 & conb$Bites==2],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==3 & conb$Bites==3],replace=T),na.rm=T)
prevH.lower.1[3,3]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[3,3]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[3,3]<-mean(conb$bloodstage[conb$Round==3 & conb$Bites==3],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==3 & conb$Bites==4],replace=T),na.rm=T)
prevH.lower.1[4,3]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[4,3]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[4,3]<-mean(conb$bloodstage[conb$Round==3 & conb$Bites==4],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==3 & conb$Bites==5],replace=T),na.rm=T)
prevH.lower.1[5,3]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[5,3]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[5,3]<-mean(conb$bloodstage[conb$Round==3 & conb$Bites==5],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==3 & conb$Bites==10],replace=T),na.rm=T)
prevH.lower.1[6,3]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[6,3]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[6,3]<-mean(conb$bloodstage[conb$Round==3 & conb$Bites==10],na.rm=TRUE)

##Round 4

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==4 & conb$Bites==1],replace=T),na.rm=T)
prevH.lower.1[1,4]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[1,4]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[1,4]<-mean(conb$bloodstage[conb$Round==4 & conb$Bites==1],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==4 & conb$Bites==2],replace=T),na.rm=T)
prevH.lower.1[2,4]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[2,4]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[2,4]<-mean(conb$bloodstage[conb$Round==4 & conb$Bites==2],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==4 & conb$Bites==3],replace=T),na.rm=T)
prevH.lower.1[3,4]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[3,4]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[3,4]<-mean(conb$bloodstage[conb$Round==4 & conb$Bites==3],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==4 & conb$Bites==4],replace=T),na.rm=T)
prevH.lower.1[4,4]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[4,4]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[4,4]<-mean(conb$bloodstage[conb$Round==4 & conb$Bites==4],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==4 & conb$Bites==5],replace=T),na.rm=T)
prevH.lower.1[5,4]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[5,4]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[5,4]<-mean(conb$bloodstage[conb$Round==4 & conb$Bites==5],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(conb$bloodstage[conb$Round==4 & conb$Bites==10],replace=T),na.rm=T)
prevH.lower.1[6,4]<-quantile(a,0.025,na.rm=TRUE)
prevH.upper.1[6,4]<-quantile(a,0.975,na.rm=TRUE)
prevH.round.1[6,4]<-mean(conb$bloodstage[conb$Round==4 & conb$Bites==10],na.rm=TRUE)


Controls2<-TV25mbrsA<-ATV50mbrs<-ATV65mbrs<-ATV85mbrs<-numeric(4)
##ATV_25%
##Bloodstage
prevH.round.ATV25<-matrix(ncol=4,nrow=4,c(0.4,1,0.8,1,0,1,1,1,0.2,1,1,1,0,0.2,1,1))
for (i in 1:4) ATV25mbrs[i] <- mean(prevH.round.ATV25[i,])
prevH.lower.ATV25<-matrix(ncol=4,nrow=4,c(0.0,1,0.4,1,0,1,1,1,0.0,1,1,1,0,0,1,1))
prevH.upper.ATV25<-matrix(ncol=4,nrow=4,c(0.8,1,1,1,0,1,1,1,0.6,1,1,1,0,0.6,1,1))

##ATV_50%
##Bloodstage
prevH.round.ATV50<-matrix(ncol=4,nrow=4,c(0.8,1,1,1,0.4,0.2,0.2,1,
                                      0,0.2,0.2,0.4,0,0.2,0.4,0.4))
for (i in 1:4) ATV50mbrs[i] <- mean(prevH.round.ATV50[i,])
prevH.lower.ATV50<-matrix(ncol=4,nrow=4,c(0.4,1,1,1,0,0.2,0.2,1,
                                      0,0,0,0,0,0,0,0))
prevH.upper.ATV50<-matrix(ncol=4,nrow=4,c(1,1,1,1,0.8,1,1,1,
                                      0,0.6,0.6,0.8,0,0.6,0.8,0.8))

##ATV_65%
##Bloodstage
prevH.round.ATV65<-matrix(ncol=4,nrow=4,c(0,0,0.2,0,rep(0,12)))
for (i in 1:4) ATV65mbrs[i] <- mean(prevH.round.ATV65[i,])
prevH.lower.ATV65<-matrix(ncol=4,nrow=4,c(0,0,0,0,rep(0,12)))
prevH.upper.ATV65<-matrix(ncol=4,nrow=4,c(0,0,0.6,0,rep(0,12)))

##ATV_85%
##Bloodstage
prevH.round.ATV85<-matrix(ncol=4,nrow=4,c(0,0.2,0.4,0,rep(0,12)))
for (i in 1:4) ATV85mbrs[i] <- mean(prevH.round.ATV85[i,])
prevH.lower.ATV85<-matrix(ncol=4,nrow=4,c(0,0.0,0.0,0,rep(0,12)))
prevH.upper.ATV85<-matrix(ncol=4,nrow=4,c(0,0.6,0.8,0,rep(0,12)))

##Controls
prevH.round.con <- prevH.round.1
prevH.lower.con <- prevH.lower.1
prevH.upper.con <- prevH.upper.1

mbr<-c(1,2,5,6)
for (i in 1:4) Controls2[i] <- mean(prevH.round.con[mbr[i],],na.rm=TRUE)
########################################################
##
##Effect sizes for each intervention for each round 
##
############################################################

EffectSizeATV25<-(Controls2-ATV25mbrs)/Controls2
EffectSizeATV50<-(Controls2-ATV50mbrs)/Controls2
EffectSizeATV65<-(Controls2-ATV65mbrs)/Controls2
EffectSizeATV85<-(Controls2-ATV85mbrs)/Controls2

Bites<- c(1,2,5,10)


DaysElimATV25<-c(3,4,4,4)
DaysElimATV50<-c(2,4,4,4)
DaysElimATV65<-c(1,1,2,1)
DaysElimATV85<-c(1,2,2,1)

data<-data.frame(Bites,
                 EffectSizeATV25,EffectSizeATV50,EffectSizeATV65,EffectSizeATV85,
                 DaysElimATV25,DaysElimATV50,DaysElimATV65,DaysElimATV85)

write.csv(data,"C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ContourPlotData.csv")
