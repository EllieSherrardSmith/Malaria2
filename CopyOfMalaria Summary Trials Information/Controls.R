##########################################
##
##  Data ## Oocysts
##
##
##########################################

con<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mosquito.txt",header=TRUE)
con$OocPrev<-ifelse(con$Oocyst==0,0,1)

summary(con)
con<-subset(con,DATASOURCE=="NEW")

prevH.round.1<-matrix(ncol=4,nrow=6)
prevH.lower.1<-matrix(ncol=4,nrow=6)
prevH.upper.1<-matrix(ncol=4,nrow=6)

prevMO.round.1<-matrix(ncol=4,nrow=6)
prevMO.lower.1<-matrix(ncol=4,nrow=6)
prevMO.upper.1<-matrix(ncol=4,nrow=6)

prevMOI.round.1<-matrix(ncol=4,nrow=6)
prevMOI.lower1<-matrix(ncol=4,nrow=6)
prevMOI.upper1<-matrix(ncol=4,nrow=6)

prevMOS.round.1<-matrix(ncol=4,nrow=6)
prevMOS.lower.1<-matrix(ncol=4,nrow=6)
prevMOS.upper.1<-matrix(ncol=4,nrow=6)

MOSI.round.1<-matrix(ncol=4,nrow=6)
bootMOSI.lower1<-matrix(ncol=4,nrow=6)
bootMOSI.upper1<-matrix(ncol=4,nrow=6)

###Oocyst intensity mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==1 & con$Bites==1],replace=T),na.rm=T)
prevMOI.lower1[1,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[1,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[1,1]<-mean(con$Oocyst[con$Round==1 & con$Bites==1],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==1 & con$Bites==2],replace=T),na.rm=T)
prevMOI.lower1[2,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[2,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[2,1]<-mean(con$Oocyst[con$Round==1 & con$Bites==2],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==1 & con$Bites==3],replace=T),na.rm=T)
prevMOI.lower1[3,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[3,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[3,1]<-mean(con$Oocyst[con$Round==1 & con$Bites==3],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==1 & con$Bites==4],replace=T),na.rm=T)
prevMOI.lower1[4,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[4,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[4,1]<-mean(con$Oocyst[con$Round==1 & con$Bites==4],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==1 & con$Bites==5],replace=T),na.rm=T)
prevMOI.lower1[5,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[5,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[5,1]<-mean(con$Oocyst[con$Round==1 & con$Bites==5],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==1 & con$Bites==10],replace=T),na.rm=T)
prevMOI.lower1[6,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[6,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[6,1]<-mean(con$Oocyst[con$Round==1 & con$Bites==10],na.rm=TRUE)

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==2 & con$Bites==1],replace=T),na.rm=T)
prevMOI.lower1[1,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[1,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[1,2]<-mean(con$Oocyst[con$Round==2 & con$Bites==1],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==2 & con$Bites==2],replace=T),na.rm=T)
prevMOI.lower1[2,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[2,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[2,2]<-mean(con$Oocyst[con$Round==2 & con$Bites==2],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==2 & con$Bites==3],replace=T),na.rm=T)
prevMOI.lower1[3,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[3,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[3,2]<-mean(con$Oocyst[con$Round==2 & con$Bites==3],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==2 & con$Bites==4],replace=T),na.rm=T)
prevMOI.lower1[4,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[4,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[4,2]<-mean(con$Oocyst[con$Round==2 & con$Bites==4],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==2 & con$Bites==5],replace=T),na.rm=T)
prevMOI.lower1[5,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[5,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[5,2]<-mean(con$Oocyst[con$Round==2 & con$Bites==5],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==2 & con$Bites==10],replace=T),na.rm=T)
prevMOI.lower1[6,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[6,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[6,2]<-mean(con$Oocyst[con$Round==2 & con$Bites==10],na.rm=TRUE)

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==3 & con$Bites==1],replace=T),na.rm=T)
prevMOI.lower1[1,3]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[1,3]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[1,3]<-mean(con$Oocyst[con$Round==3 & con$Bites==1],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==3 & con$Bites==2],replace=T),na.rm=T)
prevMOI.lower1[2,3]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[2,3]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[2,3]<-mean(con$Oocyst[con$Round==3 & con$Bites==2],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==3 & con$Bites==3],replace=T),na.rm=T)
prevMOI.lower1[3,3]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[3,3]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[3,3]<-mean(con$Oocyst[con$Round==3 & con$Bites==3],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==3 & con$Bites==4],replace=T),na.rm=T)
prevMOI.lower1[4,3]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[4,3]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[4,3]<-mean(con$Oocyst[con$Round==3 & con$Bites==4],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==3 & con$Bites==5],replace=T),na.rm=T)
prevMOI.lower1[5,3]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[5,3]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[5,3]<-mean(con$Oocyst[con$Round==3 & con$Bites==5],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==3 & con$Bites==10],replace=T),na.rm=T)
prevMOI.lower1[6,3]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[6,3]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[6,3]<-mean(con$Oocyst[con$Round==3 & con$Bites==10],na.rm=TRUE)

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==4 & con$Bites==1],replace=T),na.rm=T)
prevMOI.lower1[1,4]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[1,4]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[1,4]<-mean(con$Oocyst[con$Round==4 & con$Bites==1],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==4 & con$Bites==2],replace=T),na.rm=T)
prevMOI.lower1[2,4]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[2,4]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[2,4]<-mean(con$Oocyst[con$Round==4 & con$Bites==2],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==4 & con$Bites==3],replace=T),na.rm=T)
prevMOI.lower1[3,4]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[3,4]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[3,4]<-mean(con$Oocyst[con$Round==4 & con$Bites==3],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==4 & con$Bites==4],replace=T),na.rm=T)
prevMOI.lower1[4,4]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[4,4]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[4,4]<-mean(con$Oocyst[con$Round==4 & con$Bites==4],na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==4 & con$Bites==5],replace=T),na.rm=T)
prevMOI.lower1[5,4]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[5,4]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[5,4]<-mean(con$Oocyst[con$Round==4 & con$Bites==5],na.rm=TRUE)


for(i in 1:10000) a[i] <- mean(sample(con$Oocyst[con$Round==4 & con$Bites==10],replace=T),na.rm=T)
prevMOI.lower1[6,4]<-quantile(a,0.025,na.rm=TRUE)
prevMOI.upper1[6,4]<-quantile(a,0.975,na.rm=TRUE)
prevMOI.round.1[6,4]<-mean(con$Oocyst[con$Round==4 & con$Bites==10],na.rm=TRUE)



###Oocyst prevalence mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==1 & con$Bites==1],replace=T),na.rm=T)
prevMO.lower.1[1,1]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[1,1]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[1,1]<-mean(con$OocPrev[con$Round==1 & con$Bites==1],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==1 & con$Bites==2],replace=T),na.rm=T)
prevMO.lower.1[2,1]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[2,1]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[2,1]<-mean(con$OocPrev[con$Round==1 & con$Bites==2],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==1 & con$Bites==3],replace=T),na.rm=T)
prevMO.lower.1[3,1]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[3,1]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[3,1]<-mean(con$OocPrev[con$Round==1 & con$Bites==3],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==1 & con$Bites==4],replace=T),na.rm=T)
prevMO.lower.1[4,1]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[4,1]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[4,1]<-mean(con$OocPrev[con$Round==1 & con$Bites==4],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==1 & con$Bites==5],replace=T),na.rm=T)
prevMO.lower.1[5,1]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[5,1]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[5,1]<-mean(con$OocPrev[con$Round==1 & con$Bites==5],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==1 & con$Bites==10],replace=T),na.rm=T)
prevMO.lower.1[6,1]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[6,1]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[6,1]<-mean(con$OocPrev[con$Round==1 & con$Bites==10],na.rm=TRUE)

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==2 & con$Bites==1],replace=T),na.rm=T)
prevMO.lower.1[1,2]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[1,2]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[1,2]<-mean(con$OocPrev[con$Round==2 & con$Bites==1],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==2 & con$Bites==2],replace=T),na.rm=T)
prevMO.lower.1[2,2]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[2,2]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[2,2]<-mean(con$OocPrev[con$Round==2 & con$Bites==2],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==2 & con$Bites==3],replace=T),na.rm=T)
prevMO.lower.1[3,2]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[3,2]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[3,2]<-mean(con$OocPrev[con$Round==2 & con$Bites==3],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==2 & con$Bites==4],replace=T),na.rm=T)
prevMO.lower.1[4,2]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[4,2]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[4,2]<-mean(con$OocPrev[con$Round==2 & con$Bites==4],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==2 & con$Bites==5],replace=T),na.rm=T)
prevMO.lower.1[5,2]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[5,2]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[5,2]<-mean(con$OocPrev[con$Round==2 & con$Bites==5],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==2 & con$Bites==10],replace=T),na.rm=T)
prevMO.lower.1[6,2]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[6,2]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[6,2]<-mean(con$OocPrev[con$Round==2 & con$Bites==10],na.rm=TRUE)

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==3 & con$Bites==1],replace=T),na.rm=T)
prevMO.lower.1[1,3]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[1,3]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[1,3]<-mean(con$OocPrev[con$Round==3 & con$Bites==1],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==3 & con$Bites==2],replace=T),na.rm=T)
prevMO.lower.1[2,3]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[2,3]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[2,3]<-mean(con$OocPrev[con$Round==3 & con$Bites==2],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==3 & con$Bites==3],replace=T),na.rm=T)
prevMO.lower.1[3,3]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[3,3]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[3,3]<-mean(con$OocPrev[con$Round==3 & con$Bites==3],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==3 & con$Bites==4],replace=T),na.rm=T)
prevMO.lower.1[4,3]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[4,3]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[4,3]<-mean(con$OocPrev[con$Round==3 & con$Bites==4],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==3 & con$Bites==5],replace=T),na.rm=T)
prevMO.lower.1[5,3]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[5,3]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[5,3]<-mean(con$OocPrev[con$Round==3 & con$Bites==5],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==3 & con$Bites==10],replace=T),na.rm=T)
prevMO.lower.1[6,3]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[6,3]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[6,3]<-mean(con$OocPrev[con$Round==3 & con$Bites==10],na.rm=TRUE)

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==4 & con$Bites==1],replace=T),na.rm=T)
prevMO.lower.1[1,4]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[1,4]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[1,4]<-mean(con$OocPrev[con$Round==4 & con$Bites==1],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==4 & con$Bites==2],replace=T),na.rm=T)
prevMO.lower.1[2,4]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[2,4]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[2,4]<-mean(con$OocPrev[con$Round==4 & con$Bites==2],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==4 & con$Bites==3],replace=T),na.rm=T)
prevMO.lower.1[3,4]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[3,4]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[3,4]<-mean(con$OocPrev[con$Round==4 & con$Bites==3],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==4 & con$Bites==4],replace=T),na.rm=T)
prevMO.lower.1[4,4]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[4,4]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[4,4]<-mean(con$OocPrev[con$Round==4 & con$Bites==4],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==4 & con$Bites==5],replace=T),na.rm=T)
prevMO.lower.1[5,4]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[5,4]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[5,4]<-mean(con$OocPrev[con$Round==4 & con$Bites==5],na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(con$OocPrev[con$Round==4 & con$Bites==10],replace=T),na.rm=T)
prevMO.lower.1[6,4]<-quantile(a,0.025,na.rm=TRUE)
prevMO.upper.1[6,4]<-quantile(a,0.975,na.rm=TRUE)
prevMO.round.1[6,4]<-mean(con$OocPrev[con$Round==4 & con$Bites==10],na.rm=TRUE)

##########################################
##
##  Data ## BloodStage and Sporozoites
##
##
##########################################

conb<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse3AddingCombinations.txt",header=TRUE)
conb$bloodstage<-ifelse(conb$Parasitemia==0,0,1)
summary(conb)

conb<-subset(conb,DATASOURCE=="NEW")
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

###Sporozoites mean and CI95%
##create prevalence
conb$sp1<-ifelse(conb$Sporozoite1==0,0,1)
conb$sp2<-ifelse(conb$Sporozoite2==0,0,1)
conb$sp3<-ifelse(conb$Sporozoite3==0,0,1)
conb$sp4<-ifelse(conb$Sporozoite4==0,0,1)
conb$sp5<-ifelse(conb$Sporozoite5==0,0,1)
conb$sp6<-ifelse(conb$Sporozoite6==0,0,1)
conb$sp7<-ifelse(conb$Sporozoite7==0,0,1)
conb$sp8<-ifelse(conb$Sporozoite8==0,0,1)
conb$sp9<-ifelse(conb$Sporozoite9==0,0,1)
conb$sp10<-ifelse(conb$Sporozoite10==0,0,1)


sp1a<-conb$sp1[conb$Round==1 & conb$Bites==1]

sp2a<-c(conb$sp1[conb$Round==1 & conb$Bites==2],conb$sp2[conb$Round==1 & conb$Bites==2])

sp3a<-c(conb$sp1[conb$Round==1 & conb$Bites==3],conb$sp2[conb$Round==1 & conb$Bites==3],
        conb$sp3[conb$Round==1 & conb$Bites==3])

sp4a<-c(conb$sp1[conb$Round==1 & conb$Bites==4],conb$sp2[conb$Round==1 & conb$Bites==4],
        conb$sp3[conb$Round==1 & conb$Bites==4],conb$sp4[conb$Round==1 & conb$Bites==4])
        
sp5a<-c(conb$sp1[conb$Round==1 & conb$Bites==5],conb$sp2[conb$Round==1 & conb$Bites==5],
        conb$sp3[conb$Round==1 & conb$Bites==5],conb$sp4[conb$Round==1 & conb$Bites==5],
        conb$sp5[conb$Round==1 & conb$Bites==5])

sp10a<-c(conb$sp1[conb$Round==1 & conb$Bites==10],conb$sp2[conb$Round==1 & conb$Bites==10],
        conb$sp3[conb$Round==1 & conb$Bites==10],conb$sp4[conb$Round==1 & conb$Bites==10],
        conb$sp5[conb$Round==1 & conb$Bites==10],conb$sp6[conb$Round==1 & conb$Bites==10],
        conb$sp7[conb$Round==1 & conb$Bites==10],conb$sp8[conb$Round==1 & conb$Bites==10],
        conb$sp9[conb$Round==1 & conb$Bites==10],conb$sp10[conb$Round==1 & conb$Bites==10])


sp1b<-conb$sp1[conb$Round==2 & conb$Bites==1]

sp2b<-c(conb$sp1[conb$Round==2 & conb$Bites==2],conb$sp2[conb$Round==2 & conb$Bites==2])

sp3b<-c(conb$sp1[conb$Round==2 & conb$Bites==3],conb$sp2[conb$Round==2 & conb$Bites==3],
         conb$sp3[conb$Round==2 & conb$Bites==3])
         
sp4b<-c(conb$sp1[conb$Round==2 & conb$Bites==4],conb$sp2[conb$Round==2 & conb$Bites==4],
                  conb$sp3[conb$Round==2 & conb$Bites==4],conb$sp3[conb$Round==2 & conb$Bites==4])
        
sp5b<-c(conb$sp1[conb$Round==2 & conb$Bites==5],conb$sp2[conb$Round==2 & conb$Bites==5],
        conb$sp3[conb$Round==2 & conb$Bites==5],conb$sp4[conb$Round==2 & conb$Bites==5],
        conb$sp5[conb$Round==2 & conb$Bites==5])

sp10b<-c(conb$sp1[conb$Round==2 & conb$Bites==10],conb$sp2[conb$Round==2 & conb$Bites==10],
        conb$sp3[conb$Round==2 & conb$Bites==10],conb$sp4[conb$Round==2 & conb$Bites==10],
        conb$sp5[conb$Round==2 & conb$Bites==10],conb$sp6[conb$Round==2 & conb$Bites==10],
        conb$sp7[conb$Round==2 & conb$Bites==10],conb$sp8[conb$Round==2 & conb$Bites==10],
        conb$sp9[conb$Round==2 & conb$Bites==10],conb$sp10[conb$Round==2 & conb$Bites==10])

sp1c<-conb$sp1[conb$Round==3 & conb$Bites==1]

sp2c<-c(conb$sp1[conb$Round==3 & conb$Bites==2],conb$sp2[conb$Round==3 & conb$Bites==2])

sp3c<-c(conb$sp1[conb$Round==3 & conb$Bites==3],conb$sp2[conb$Round==3 & conb$Bites==3],
        conb$sp3[conb$Round==3 & conb$Bites==3])

sp4c<-c(conb$sp1[conb$Round==3 & conb$Bites==4],conb$sp2[conb$Round==3 & conb$Bites==4],
        conb$sp3[conb$Round==3 & conb$Bites==4],conb$sp3[conb$Round==3 & conb$Bites==4])

sp5c<-c(conb$sp1[conb$Round==3 & conb$Bites==5],conb$sp2[conb$Round==3 & conb$Bites==5],
        conb$sp3[conb$Round==3 & conb$Bites==5],conb$sp4[conb$Round==3 & conb$Bites==5],
        conb$sp5[conb$Round==3 & conb$Bites==5])

sp10c<-c(conb$sp1[conb$Round==3 & conb$Bites==10],conb$sp2[conb$Round==3 & conb$Bites==10],
        conb$sp3[conb$Round==3 & conb$Bites==10],conb$sp4[conb$Round==3 & conb$Bites==10],
        conb$sp5[conb$Round==3 & conb$Bites==10],conb$sp6[conb$Round==3 & conb$Bites==10],
        conb$sp7[conb$Round==3 & conb$Bites==10],conb$sp8[conb$Round==3 & conb$Bites==10],
        conb$sp9[conb$Round==3 & conb$Bites==10],conb$sp10[conb$Round==3 & conb$Bites==10])

sp1d<-conb$sp1[conb$Round==4 & conb$Bites==1]

sp2d<-c(conb$sp1[conb$Round==4 & conb$Bites==2],conb$sp2[conb$Round==4 & conb$Bites==2])

sp3d<-c(conb$sp1[conb$Round==4 & conb$Bites==3],conb$sp2[conb$Round==4 & conb$Bites==3],
        conb$sp3[conb$Round==4 & conb$Bites==3])

sp4d<-c(conb$sp1[conb$Round==4 & conb$Bites==4],conb$sp2[conb$Round==4 & conb$Bites==4],
        conb$sp3[conb$Round==4 & conb$Bites==4],conb$sp3[conb$Round==4 & conb$Bites==4])

sp5d<-c(conb$sp1[conb$Round==4 & conb$Bites==5],conb$sp2[conb$Round==4 & conb$Bites==5],
        conb$sp3[conb$Round==4 & conb$Bites==5],conb$sp4[conb$Round==4 & conb$Bites==5],
        conb$sp5[conb$Round==4 & conb$Bites==5])

sp10d<-c(conb$sp1[conb$Round==4 & conb$Bites==10],conb$sp2[conb$Round==4 & conb$Bites==10],
        conb$sp3[conb$Round==4 & conb$Bites==10],conb$sp4[conb$Round==4 & conb$Bites==10],
        conb$sp5[conb$Round==4 & conb$Bites==10],conb$sp6[conb$Round==4 & conb$Bites==10],
        conb$sp7[conb$Round==4 & conb$Bites==10],conb$sp8[conb$Round==4 & conb$Bites==10],
        conb$sp9[conb$Round==4 & conb$Bites==10],conb$sp10[conb$Round==4 & conb$Bites==10])

##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1a,replace=T),na.rm=T)
prevMOS.lower.1[1,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[1,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[1,1]<-mean(sp1a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2a,replace=T),na.rm=T)
prevMOS.lower.1[2,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[2,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[2,1]<-mean(sp2a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp3a,replace=T),na.rm=T)
prevMOS.lower.1[3,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[3,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[3,1]<-mean(sp3a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp4a,replace=T),na.rm=T)
prevMOS.lower.1[4,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[4,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[4,1]<-mean(sp4a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5a,replace=T),na.rm=T)
prevMOS.lower.1[5,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[5,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[5,1]<-mean(sp5a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10a,replace=T),na.rm=T)
prevMOS.lower.1[6,1]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[6,1]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[6,1]<-mean(sp10a,na.rm=T)

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1b,replace=T),na.rm=T)
prevMOS.lower.1[1,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[1,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[1,2]<-mean(sp1b,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2b,replace=T),na.rm=T)
prevMOS.lower.1[2,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[2,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[2,2]<-mean(sp2b,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp3b,replace=T),na.rm=T)
prevMOS.lower.1[3,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[3,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[3,2]<-mean(sp3b,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp4b,replace=T),na.rm=T)
prevMOS.lower.1[4,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[4,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[4,2]<-mean(sp4b,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5b,replace=T),na.rm=T)
prevMOS.lower.1[5,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[5,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[5,2]<-mean(sp5b,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10b,replace=T),na.rm=T)
prevMOS.lower.1[6,2]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[6,2]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[6,2]<-mean(sp10b,na.rm=TRUE)

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1c,replace=T),na.rm=T)
prevMOS.lower.1[1,3]<-quantile(a,0.025,na.rm=TRUE)
prevMOS.upper.1[1,3]<-quantile(a,0.975,na.rm=TRUE)
prevMOS.round.1[1,3]<-mean(sp1c,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2c,replace=T),na.rm=T)
prevMOS.lower.1[2,3]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[2,3]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[2,3]<-mean(sp2c,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp3c,replace=T),na.rm=T)
prevMOS.lower.1[3,3]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[3,3]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[3,3]<-mean(sp3c,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp4c,replace=T),na.rm=T)
prevMOS.lower.1[4,3]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[4,3]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[4,3]<-mean(sp4c,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5c,replace=T),na.rm=T)
prevMOS.lower.1[5,3]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[5,3]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[5,3]<-mean(sp5c,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10c,replace=T),na.rm=T)
prevMOS.lower.1[6,3]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[6,3]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[6,3]<-mean(sp10c,na.rm=T)

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1d,replace=T),na.rm=T)
prevMOS.lower.1[1,4]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[1,4]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[1,4]<-mean(sp1d,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2d,replace=T),na.rm=T)
prevMOS.lower.1[2,4]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[2,4]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[2,4]<-mean(sp2d,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp3d,replace=T),na.rm=T)
prevMOS.lower.1[3,4]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[3,4]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[3,4]<-mean(sp3d,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp4d,replace=T),na.rm=T)
prevMOS.lower.1[4,4]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[4,4]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[4,4]<-mean(sp4d,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5d,replace=T),na.rm=T)
prevMOS.lower.1[5,4]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[5,4]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[5,4]<-mean(sp5d,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10d,replace=T),na.rm=T)
prevMOS.lower.1[6,4]<-quantile(a,0.025,na.rm=T)
prevMOS.upper.1[6,4]<-quantile(a,0.975,na.rm=T)
prevMOS.round.1[6,4]<-mean(sp10d,na.rm=T)

###Sporozoite intensity
Sporozoite1a<-conb$Sporozoite1[conb$Round==1 & conb$Bites==1]

Sporozoite2a<-c(conb$Sporozoite1[conb$Round==1 & conb$Bites==2],conb$Sporozoite2[conb$Round==1 & conb$Bites==2])

Sporozoite3a<-c(conb$Sporozoite1[conb$Round==1 & conb$Bites==3],conb$Sporozoite2[conb$Round==1 & conb$Bites==3],
                conb$Sporozoite3[conb$Round==1 & conb$Bites==3])
                
Sporozoite4a<-c(conb$Sporozoite1[conb$Round==1 & conb$Bites==4],conb$Sporozoite2[conb$Round==1 & conb$Bites==4],
                conb$Sporozoite3[conb$Round==1 & conb$Bites==4],conb$Sporozoite4[conb$Round==1 & conb$Bites==4])
                 
Sporozoite5a<-c(conb$Sporozoite1[conb$Round==1 & conb$Bites==5],conb$Sporozoite2[conb$Round==1 & conb$Bites==5],
                conb$Sporozoite3[conb$Round==1 & conb$Bites==5],conb$Sporozoite4[conb$Round==1 & conb$Bites==5],
                conb$Sporozoite5[conb$Round==1 & conb$Bites==5])

Sporozoite10a<-c(conb$Sporozoite1[conb$Round==1 & conb$Bites==10],conb$Sporozoite2[conb$Round==1 & conb$Bites==10],
                conb$Sporozoite3[conb$Round==1 & conb$Bites==10],conb$Sporozoite4[conb$Round==1 & conb$Bites==10],
                conb$Sporozoite5[conb$Round==1 & conb$Bites==10],conb$Sporozoite6[conb$Round==1 & conb$Bites==10],
                conb$Sporozoite7[conb$Round==1 & conb$Bites==10],conb$Sporozoite8[conb$Round==1 & conb$Bites==10],
                conb$Sporozoite9[conb$Round==1 & conb$Bites==10],conb$Sporozoite10[conb$Round==1 & conb$Bites==10])

Sporozoite1b<-conb$Sporozoite1[conb$Round==2 & conb$Bites==1]

Sporozoite2b<-c(conb$Sporozoite1[conb$Round==2 & conb$Bites==2],conb$Sporozoite2[conb$Round==2 & conb$Bites==2])

Sporozoite3b<-c(conb$Sporozoite1[conb$Round==2 & conb$Bites==3],conb$Sporozoite2[conb$Round==2 & conb$Bites==3],
                conb$Sporozoite3[conb$Round==2 & conb$Bites==3])

Sporozoite4b<-c(conb$Sporozoite1[conb$Round==2 & conb$Bites==4],conb$Sporozoite2[conb$Round==2 & conb$Bites==4],
                conb$Sporozoite3[conb$Round==2 & conb$Bites==4],conb$Sporozoite4[conb$Round==2 & conb$Bites==4])

Sporozoite5b<-c(conb$Sporozoite1[conb$Round==2 & conb$Bites==5],conb$Sporozoite2[conb$Round==2 & conb$Bites==5],
                conb$Sporozoite3[conb$Round==2 & conb$Bites==5],conb$Sporozoite4[conb$Round==2 & conb$Bites==5],
                conb$Sporozoite5[conb$Round==2 & conb$Bites==5])

Sporozoite10b<-c(conb$Sporozoite1[conb$Round==2 & conb$Bites==10],conb$Sporozoite2[conb$Round==2 & conb$Bites==10],
                conb$Sporozoite3[conb$Round==2 & conb$Bites==10],conb$Sporozoite4[conb$Round==2 & conb$Bites==10],
                conb$Sporozoite5[conb$Round==2 & conb$Bites==10],conb$Sporozoite6[conb$Round==2 & conb$Bites==10],
                conb$Sporozoite7[conb$Round==2 & conb$Bites==10],conb$Sporozoite8[conb$Round==2 & conb$Bites==10],
                conb$Sporozoite9[conb$Round==2 & conb$Bites==10],conb$Sporozoite10[conb$Round==2 & conb$Bites==10])


Sporozoite1c<-conb$Sporozoite1[conb$Round==3 & conb$Bites==1]

Sporozoite2c<-c(conb$Sporozoite1[conb$Round==3 & conb$Bites==2],conb$Sporozoite2[conb$Round==3 & conb$Bites==2])

Sporozoite3c<-c(conb$Sporozoite1[conb$Round==3 & conb$Bites==3],conb$Sporozoite2[conb$Round==3 & conb$Bites==3],
                conb$Sporozoite3[conb$Round==3 & conb$Bites==3])

Sporozoite4c<-c(conb$Sporozoite1[conb$Round==3 & conb$Bites==4],conb$Sporozoite2[conb$Round==3 & conb$Bites==4],
                conb$Sporozoite3[conb$Round==3 & conb$Bites==4],conb$Sporozoite4[conb$Round==3 & conb$Bites==4])

Sporozoite5c<-c(conb$Sporozoite1[conb$Round==3 & conb$Bites==5],conb$Sporozoite2[conb$Round==3 & conb$Bites==5],
                conb$Sporozoite3[conb$Round==3 & conb$Bites==5],conb$Sporozoite4[conb$Round==3 & conb$Bites==5],
                conb$Sporozoite5[conb$Round==3 & conb$Bites==5])

Sporozoite10c<-c(conb$Sporozoite1[conb$Round==3 & conb$Bites==10],conb$Sporozoite2[conb$Round==3 & conb$Bites==10],
                conb$Sporozoite3[conb$Round==3 & conb$Bites==10],conb$Sporozoite4[conb$Round==3 & conb$Bites==10],
                conb$Sporozoite5[conb$Round==3 & conb$Bites==10],conb$Sporozoite6[conb$Round==3 & conb$Bites==10],
                conb$Sporozoite7[conb$Round==3 & conb$Bites==10],conb$Sporozoite8[conb$Round==3 & conb$Bites==10],
                conb$Sporozoite9[conb$Round==3 & conb$Bites==10],conb$Sporozoite10[conb$Round==3 & conb$Bites==10])


Sporozoite1d<-conb$Sporozoite1[conb$Round==4 & conb$Bites==1]

Sporozoite2d<-c(conb$Sporozoite1[conb$Round==4 & conb$Bites==2],conb$Sporozoite2[conb$Round==4 & conb$Bites==2])

Sporozoite3d<-c(conb$Sporozoite1[conb$Round==4 & conb$Bites==3],conb$Sporozoite2[conb$Round==4 & conb$Bites==3],
                conb$Sporozoite3[conb$Round==4 & conb$Bites==3])

Sporozoite4d<-c(conb$Sporozoite1[conb$Round==4 & conb$Bites==4],conb$Sporozoite2[conb$Round==4 & conb$Bites==4],
                conb$Sporozoite3[conb$Round==4 & conb$Bites==4],conb$Sporozoite4[conb$Round==4 & conb$Bites==4])

Sporozoite5d<-c(conb$Sporozoite1[conb$Round==4 & conb$Bites==5],conb$Sporozoite2[conb$Round==4 & conb$Bites==5],
                conb$Sporozoite3[conb$Round==4 & conb$Bites==5],conb$Sporozoite4[conb$Round==4 & conb$Bites==5],
                conb$Sporozoite5[conb$Round==4 & conb$Bites==5])

Sporozoite10d<-c(conb$Sporozoite1[conb$Round==4 & conb$Bites==10],conb$Sporozoite2[conb$Round==4 & conb$Bites==10],
                conb$Sporozoite3[conb$Round==4 & conb$Bites==10],conb$Sporozoite4[conb$Round==4 & conb$Bites==10],
                conb$Sporozoite5[conb$Round==4 & conb$Bites==10],conb$Sporozoite6[conb$Round==4 & conb$Bites==10],
                conb$Sporozoite7[conb$Round==4 & conb$Bites==10],conb$Sporozoite8[conb$Round==4 & conb$Bites==10],
                conb$Sporozoite9[conb$Round==4 & conb$Bites==10],conb$Sporozoite10[conb$Round==4 & conb$Bites==10])


##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1a,replace=T),na.rm=T)
bootMOSI.lower1[1,1]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[1,1]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[1,1]<-mean(Sporozoite1a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2a,replace=T),na.rm=T)
bootMOSI.lower1[2,1]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[2,1]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[2,1]<-mean(Sporozoite2a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite3a,replace=T),na.rm=T)
bootMOSI.lower1[3,1]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[3,1]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[3,1]<-mean(Sporozoite3a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite4a,replace=T),na.rm=T)
bootMOSI.lower1[4,1]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[4,1]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[4,1]<-mean(Sporozoite4a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite5a,replace=T),na.rm=T)
bootMOSI.lower1[5,1]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[5,1]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[5,1]<-mean(Sporozoite5a,na.rm=T)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite10a,replace=T),na.rm=T)
bootMOSI.lower1[6,1]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[6,1]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[6,1]<-mean(Sporozoite10a,na.rm=T)

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1b,replace=T),na.rm=T)
bootMOSI.lower1[1,2]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[1,2]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[1,2]<-mean(Sporozoite1b,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2b,replace=T),na.rm=T)
bootMOSI.lower1[2,2]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[2,2]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[2,2]<-mean(Sporozoite2b,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite3b,replace=T),na.rm=T)
bootMOSI.lower1[3,2]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[3,2]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[3,2]<-mean(Sporozoite3b,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite4b,replace=T),na.rm=T)
bootMOSI.lower1[4,2]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[4,2]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[4,2]<-mean(Sporozoite4b,na.rm=TRUE)
a<-numeric(10000)

for(i in 1:10000) a[i] <- mean(sample(Sporozoite5b,replace=T),na.rm=T)
bootMOSI.lower1[5,2]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[5,2]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[5,2]<-mean(Sporozoite5b,na.rm=TRUE)

for(i in 1:10000) a[i] <- mean(sample(Sporozoite10b,replace=T),na.rm=T)
bootMOSI.lower1[6,2]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[6,2]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[6,2]<-mean(Sporozoite10b,na.rm=TRUE)



##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1c,replace=T),na.rm=T)
bootMOSI.lower1[1,3]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[1,3]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[1,3]<-mean(Sporozoite1c,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2c,replace=T),na.rm=T)
bootMOSI.lower1[2,3]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[2,3]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[2,3]<-mean(Sporozoite2c,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite3c,replace=T),na.rm=T)
bootMOSI.lower1[3,3]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[3,3]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[3,3]<-mean(Sporozoite3c,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite4c,replace=T),na.rm=T)
bootMOSI.lower1[4,3]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[4,3]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[4,3]<-mean(Sporozoite4c,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite5c,replace=T),na.rm=T)
bootMOSI.lower1[5,3]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[5,3]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[5,3]<-mean(Sporozoite5c,na.rm=T)
a<-numeric(10000)

for(i in 1:10000) a[i] <- mean(sample(Sporozoite10c,replace=T),na.rm=T)
bootMOSI.lower1[6,3]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[6,3]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[6,3]<-mean(Sporozoite10c,na.rm=T)

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1d,replace=T),na.rm=T)
bootMOSI.lower1[1,4]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[1,4]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[1,4]<-mean(Sporozoite1d,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2d,replace=T),na.rm=T)
bootMOSI.lower1[2,4]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[2,4]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[2,4]<-mean(Sporozoite2d,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite3d,replace=T),na.rm=T)
bootMOSI.lower1[3,4]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[3,4]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[3,4]<-mean(Sporozoite3d,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite4d,replace=T),na.rm=T)
bootMOSI.lower1[4,4]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[4,4]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[4,4]<-mean(Sporozoite4d,na.rm=TRUE)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite5d,replace=T),na.rm=T)
bootMOSI.lower1[5,4]<-quantile(a,0.025,na.rm=TRUE)
bootMOSI.upper1[5,4]<-quantile(a,0.975,na.rm=TRUE)
MOSI.round.1[5,4]<-mean(Sporozoite5d,na.rm=TRUE)

bootMOSI.lower1[6,4]<-bootMOSI.upper1[6,4]<-MOSI.round.1[6,4]<-NA


###########
## Graphs
###########

par(mfcol=c(4,5),mar= c(3, 4, 1, 2))

##Bloodstage

bit<-c(1,2,5,6)
for(mb in 1:4){
  
  hh=rbind(prevH.round.1[bit[mb],])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevH.lower.1[bit[mb],])
  ciU<-rbind(prevH.upper.1[bit[mb],])
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

for(mb in 1:4){
  
  hh=rbind(prevMO.round.1[bit[mb],])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMO.lower.1[bit[mb],])
  ciU<-rbind(prevMO.upper.1[bit[mb],])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100, 
                col = c("forestgreen"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100, col = mybarcol, lwd = 1.5)
  
}
##Oocyst intensity

for(mb in 1:4){
  
  hh=rbind(prevMOI.round.1[bit[mb],])
  hh.max<-apply(hh, 2, max)+5
  ciL<-rbind(prevMOI.lower1[bit[mb],])
  ciU<-rbind(prevMOI.upper1[bit[mb],])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  
  mybarcol <- "gray20"
  mp <- barplot(hh,
                col = c("firebrick3"),
                ylim= c(0,45),
                        main = "NEW CONTROLS", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  #text(colMeans(mp)+0.2,hh.max,  labels =less.than.001[mb,], col = "red")
  
  #mtext(side = 1, at = colMeans(mp), line = 2,
  #      text = paste("efficacy", formatC(colMeans(hh))), col = "red")
  
}

##Sporozoite prevalence

for(mb in 1:4){
  
  hh=rbind(prevMOS.round.1[bit[mb],])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMOS.lower.1[bit[mb],])
  ciU<-rbind(prevMOS.upper.1[bit[mb],])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100, 
                col = c("orange"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100, col = mybarcol, lwd = 1.5)
  
}
##Sporozoite intensity
for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3,4)
  mp <- barplot(MOSI.round.1[bit[nb],],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[bit[nb],], mp, bootMOSI.upper1[bit[nb],] , col = mybarcol, lwd = 1.5)
  
}

