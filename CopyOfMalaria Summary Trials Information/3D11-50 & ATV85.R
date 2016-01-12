##########################################
##
##  Data ## Oocysts
##
##
##########################################

d11atv <- read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\3d11 35ATV-85\\mosquito.txt",header=TRUE)
d11atv$OocPrev<-ifelse(d11atv$Oocyst==0,0,1)
summary(d11atv)

###Oocyst intensity mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==1 & d11atv$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==1 & d11atv$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==1 & d11atv$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==1 & d11atv$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==1 & d11atv$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==1 & d11atv$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==1 & d11atv$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==1 & d11atv$Bites==10])

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==2 & d11atv$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==2 & d11atv$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==2 & d11atv$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==2 & d11atv$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==2 & d11atv$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==2 & d11atv$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==2 & d11atv$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==2 & d11atv$Bites==10])

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==3 & d11atv$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==3 & d11atv$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==3 & d11atv$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==3 & d11atv$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==3 & d11atv$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==3 & d11atv$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==3 & d11atv$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==3 & d11atv$Bites==10])

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==4 & d11atv$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==4 & d11atv$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==4 & d11atv$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==4 & d11atv$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==4 & d11atv$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==4 & d11atv$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(d11atv$Oocyst[d11atv$Round==4 & d11atv$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$Oocyst[d11atv$Round==4 & d11atv$Bites==10])

###Oocyst prevalence mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==1 & d11atv$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==1 & d11atv$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==1 & d11atv$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==1 & d11atv$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==1 & d11atv$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==1 & d11atv$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==1 & d11atv$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==1 & d11atv$Bites==10])

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==2 & d11atv$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==2 & d11atv$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==2 & d11atv$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==2 & d11atv$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==2 & d11atv$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==2 & d11atv$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==2 & d11atv$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==2 & d11atv$Bites==10])

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==3 & d11atv$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==3 & d11atv$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==3 & d11atv$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==3 & d11atv$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==3 & d11atv$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==3 & d11atv$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==3 & d11atv$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==3 & d11atv$Bites==10])

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==4 & d11atv$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==4 & d11atv$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==4 & d11atv$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==4 & d11atv$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==4 & d11atv$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==4 & d11atv$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atv$OocPrev[d11atv$Round==4 & d11atv$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atv$OocPrev[d11atv$Round==4 & d11atv$Bites==10])


##########################################
##
##  Data ## BloodStage and Sporozoites
##
##
##########################################

d11atvb<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\3d11 35ATV-85\\mouse.txt",header=TRUE)
d11atvb$bloodstage<-ifelse(d11atvb$Parasitemia==0,0,1)
summary(d11atvb)


###Bloodstage mean and CI95%
##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==1 & d11atvb$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==1 & d11atvb$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==1 & d11atvb$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==1 & d11atvb$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==1 & d11atvb$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==1 & d11atvb$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==1 & d11atvb$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==1 & d11atvb$Bites==10])

##Round 2

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==2 & d11atvb$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==2 & d11atvb$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==2 & d11atvb$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==2 & d11atvb$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==2 & d11atvb$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==2 & d11atvb$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==2 & d11atvb$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==2 & d11atvb$Bites==10])

##Round 3

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==3 & d11atvb$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==3 & d11atvb$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==3 & d11atvb$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==3 & d11atvb$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==3 & d11atvb$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==3 & d11atvb$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(d11atvb$bloodstage[d11atvb$Round==3 & d11atvb$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(d11atvb$bloodstage[d11atvb$Round==3 & d11atvb$Bites==10])

###Sporozoites mean and CI95%
##create prevalence
d11atvb$sp1<-ifelse(d11atvb$Sporozoite1==0,0,1)
d11atvb$sp2<-ifelse(d11atvb$Sporozoite2==0,0,1)
d11atvb$sp3<-ifelse(d11atvb$Sporozoite3==0,0,1)
d11atvb$sp4<-ifelse(d11atvb$Sporozoite4==0,0,1)
d11atvb$sp5<-ifelse(d11atvb$Sporozoite5==0,0,1)
d11atvb$sp6<-ifelse(d11atvb$Sporozoite6==0,0,1)
d11atvb$sp7<-ifelse(d11atvb$Sporozoite7==0,0,1)
d11atvb$sp8<-ifelse(d11atvb$Sporozoite8==0,0,1)
d11atvb$sp9<-ifelse(d11atvb$Sporozoite9==0,0,1)
d11atvb$sp10<-ifelse(d11atvb$Sporozoite10==0,0,1)


sp1a<-d11atvb$sp1[d11atvb$Round==1 & d11atvb$Bites==1]

sp2a<-c(d11atvb$sp1[d11atvb$Round==1 & d11atvb$Bites==2],d11atvb$sp2[d11atvb$Round==1 & d11atvb$Bites==2])

sp5a<-c(d11atvb$sp1[d11atvb$Round==1 & d11atvb$Bites==5],d11atvb$sp2[d11atvb$Round==1 & d11atvb$Bites==5],
        d11atvb$sp3[d11atvb$Round==1 & d11atvb$Bites==5],d11atvb$sp4[d11atvb$Round==1 & d11atvb$Bites==5],
        d11atvb$sp5[d11atvb$Round==1 & d11atvb$Bites==5])

sp10a<-c(d11atvb$sp1[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$sp2[d11atvb$Round==1 & d11atvb$Bites==10],
         d11atvb$sp3[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$sp4[d11atvb$Round==1 & d11atvb$Bites==10],
         d11atvb$sp5[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$sp6[d11atvb$Round==1 & d11atvb$Bites==10],
         d11atvb$sp7[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$sp8[d11atvb$Round==1 & d11atvb$Bites==10],
         d11atvb$sp9[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$sp10[d11atvb$Round==1 & d11atvb$Bites==10])

sp1b<-d11atvb$sp1[d11atvb$Round==2 & d11atvb$Bites==1]

sp2b<-c(d11atvb$sp1[d11atvb$Round==2 & d11atvb$Bites==2],d11atvb$sp2[d11atvb$Round==2 & d11atvb$Bites==2])

sp5b<-c(d11atvb$sp1[d11atvb$Round==2 & d11atvb$Bites==5],d11atvb$sp2[d11atvb$Round==2 & d11atvb$Bites==5],
        d11atvb$sp3[d11atvb$Round==2 & d11atvb$Bites==5],d11atvb$sp4[d11atvb$Round==2 & d11atvb$Bites==5],
        d11atvb$sp5[d11atvb$Round==2 & d11atvb$Bites==5])

sp10b<-c(d11atvb$sp1[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$sp2[d11atvb$Round==2 & d11atvb$Bites==10],
         d11atvb$sp3[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$sp4[d11atvb$Round==2 & d11atvb$Bites==10],
         d11atvb$sp5[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$sp6[d11atvb$Round==2 & d11atvb$Bites==10],
         d11atvb$sp7[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$sp8[d11atvb$Round==2 & d11atvb$Bites==10],
         d11atvb$sp9[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$sp10[d11atvb$Round==2 & d11atvb$Bites==10])

sp1c<-d11atvb$sp1[d11atvb$Round==3 & d11atvb$Bites==1]

sp2c<-c(d11atvb$sp1[d11atvb$Round==3 & d11atvb$Bites==2],d11atvb$sp2[d11atvb$Round==3 & d11atvb$Bites==2])

sp5c<-c(d11atvb$sp1[d11atvb$Round==3 & d11atvb$Bites==5],d11atvb$sp2[d11atvb$Round==3 & d11atvb$Bites==5],
        d11atvb$sp3[d11atvb$Round==3 & d11atvb$Bites==5],d11atvb$sp4[d11atvb$Round==3 & d11atvb$Bites==5],
        d11atvb$sp5[d11atvb$Round==3 & d11atvb$Bites==5])

sp10c<-c(d11atvb$sp1[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$sp2[d11atvb$Round==3 & d11atvb$Bites==10],
         d11atvb$sp3[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$sp4[d11atvb$Round==3 & d11atvb$Bites==10],
         d11atvb$sp5[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$sp6[d11atvb$Round==3 & d11atvb$Bites==10],
         d11atvb$sp7[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$sp8[d11atvb$Round==3 & d11atvb$Bites==10],
         d11atvb$sp9[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$sp10[d11atvb$Round==3 & d11atvb$Bites==10])

sp1d<-d11atvb$sp1[d11atvb$Round==4 & d11atvb$Bites==1]

sp2d<-c(d11atvb$sp1[d11atvb$Round==4 & d11atvb$Bites==2],d11atvb$sp2[d11atvb$Round==4 & d11atvb$Bites==2])

sp5d<-c(d11atvb$sp1[d11atvb$Round==4 & d11atvb$Bites==5],d11atvb$sp2[d11atvb$Round==4 & d11atvb$Bites==5],
        d11atvb$sp3[d11atvb$Round==4 & d11atvb$Bites==5],d11atvb$sp4[d11atvb$Round==4 & d11atvb$Bites==5],
        d11atvb$sp5[d11atvb$Round==4 & d11atvb$Bites==5])

sp10d<-c(d11atvb$sp1[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$sp2[d11atvb$Round==4 & d11atvb$Bites==10],
         d11atvb$sp3[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$sp4[d11atvb$Round==4 & d11atvb$Bites==10],
         d11atvb$sp5[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$sp6[d11atvb$Round==4 & d11atvb$Bites==10],
         d11atvb$sp7[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$sp8[d11atvb$Round==4 & d11atvb$Bites==10],
         d11atvb$sp9[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$sp10[d11atvb$Round==4 & d11atvb$Bites==10])

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


##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1b,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp1b)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2b,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp2b)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5b,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp5b)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10b,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp10b,na.rm=T)


##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1c,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp1c)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2c,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp2c)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5c,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp5c)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10c,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp10c,na.rm=T)

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1d,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp1d)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2d,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp2d)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5d,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp5d)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10d,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp10d)

###Sporozoite intensity
Sporozoite1a<-d11atvb$Sporozoite1[d11atvb$Round==1 & d11atvb$Bites==1]

Sporozoite2a<-c(d11atvb$Sporozoite1[d11atvb$Round==1 & d11atvb$Bites==2],d11atvb$Sporozoite2[d11atvb$Round==1 & d11atvb$Bites==2])

Sporozoite5a<-c(d11atvb$Sporozoite1[d11atvb$Round==1 & d11atvb$Bites==5],d11atvb$Sporozoite2[d11atvb$Round==1 & d11atvb$Bites==5],
                d11atvb$Sporozoite3[d11atvb$Round==1 & d11atvb$Bites==5],d11atvb$Sporozoite4[d11atvb$Round==1 & d11atvb$Bites==5],
                d11atvb$Sporozoite5[d11atvb$Round==1 & d11atvb$Bites==5])

Sporozoite10a<-c(d11atvb$Sporozoite1[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$Sporozoite2[d11atvb$Round==1 & d11atvb$Bites==10],
                 d11atvb$Sporozoite3[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$Sporozoite4[d11atvb$Round==1 & d11atvb$Bites==10],
                 d11atvb$Sporozoite5[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$Sporozoite6[d11atvb$Round==1 & d11atvb$Bites==10],
                 d11atvb$Sporozoite7[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$Sporozoite8[d11atvb$Round==1 & d11atvb$Bites==10],
                 d11atvb$Sporozoite9[d11atvb$Round==1 & d11atvb$Bites==10],d11atvb$Sporozoite10[d11atvb$Round==1 & d11atvb$Bites==10])

Sporozoite1b<-d11atvb$Sporozoite1[d11atvb$Round==2 & d11atvb$Bites==1]

Sporozoite2b<-c(d11atvb$Sporozoite1[d11atvb$Round==2 & d11atvb$Bites==2],d11atvb$Sporozoite2[d11atvb$Round==2 & d11atvb$Bites==2])

Sporozoite5b<-c(d11atvb$Sporozoite1[d11atvb$Round==2 & d11atvb$Bites==5],d11atvb$Sporozoite2[d11atvb$Round==2 & d11atvb$Bites==5],
                d11atvb$Sporozoite3[d11atvb$Round==2 & d11atvb$Bites==5],d11atvb$Sporozoite4[d11atvb$Round==2 & d11atvb$Bites==5],
                d11atvb$Sporozoite5[d11atvb$Round==2 & d11atvb$Bites==5])

Sporozoite10b<-c(d11atvb$Sporozoite1[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$Sporozoite2[d11atvb$Round==2 & d11atvb$Bites==10],
                 d11atvb$Sporozoite3[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$Sporozoite4[d11atvb$Round==2 & d11atvb$Bites==10],
                 d11atvb$Sporozoite5[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$Sporozoite6[d11atvb$Round==2 & d11atvb$Bites==10],
                 d11atvb$Sporozoite7[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$Sporozoite8[d11atvb$Round==2 & d11atvb$Bites==10],
                 d11atvb$Sporozoite9[d11atvb$Round==2 & d11atvb$Bites==10],d11atvb$Sporozoite10[d11atvb$Round==2 & d11atvb$Bites==10])

Sporozoite1c<-d11atvb$Sporozoite1[d11atvb$Round==3 & d11atvb$Bites==1]

Sporozoite2c<-c(d11atvb$Sporozoite1[d11atvb$Round==3 & d11atvb$Bites==2],d11atvb$Sporozoite2[d11atvb$Round==3 & d11atvb$Bites==2])

Sporozoite5c<-c(d11atvb$Sporozoite1[d11atvb$Round==3 & d11atvb$Bites==5],d11atvb$Sporozoite2[d11atvb$Round==3 & d11atvb$Bites==5],
                d11atvb$Sporozoite3[d11atvb$Round==3 & d11atvb$Bites==5],d11atvb$Sporozoite4[d11atvb$Round==3 & d11atvb$Bites==5],
                d11atvb$Sporozoite5[d11atvb$Round==3 & d11atvb$Bites==5])

Sporozoite10c<-c(d11atvb$Sporozoite1[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$Sporozoite2[d11atvb$Round==3 & d11atvb$Bites==10],
                 d11atvb$Sporozoite3[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$Sporozoite4[d11atvb$Round==3 & d11atvb$Bites==10],
                 d11atvb$Sporozoite5[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$Sporozoite6[d11atvb$Round==3 & d11atvb$Bites==10],
                 d11atvb$Sporozoite7[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$Sporozoite8[d11atvb$Round==3 & d11atvb$Bites==10],
                 d11atvb$Sporozoite9[d11atvb$Round==3 & d11atvb$Bites==10],d11atvb$Sporozoite10[d11atvb$Round==3 & d11atvb$Bites==10])

Sporozoite1d<-d11atvb$Sporozoite1[d11atvb$Round==4 & d11atvb$Bites==1]

Sporozoite2d<-c(d11atvb$Sporozoite1[d11atvb$Round==4 & d11atvb$Bites==2],d11atvb$Sporozoite2[d11atvb$Round==4 & d11atvb$Bites==2])

Sporozoite5d<-c(d11atvb$Sporozoite1[d11atvb$Round==4 & d11atvb$Bites==5],d11atvb$Sporozoite2[d11atvb$Round==4 & d11atvb$Bites==5],
                d11atvb$Sporozoite3[d11atvb$Round==4 & d11atvb$Bites==5],d11atvb$Sporozoite4[d11atvb$Round==4 & d11atvb$Bites==5],
                d11atvb$Sporozoite5[d11atvb$Round==4 & d11atvb$Bites==5])

Sporozoite10d<-c(d11atvb$Sporozoite1[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$Sporozoite2[d11atvb$Round==4 & d11atvb$Bites==10],
                 d11atvb$Sporozoite3[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$Sporozoite4[d11atvb$Round==4 & d11atvb$Bites==10],
                 d11atvb$Sporozoite5[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$Sporozoite6[d11atvb$Round==4 & d11atvb$Bites==10],
                 d11atvb$Sporozoite7[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$Sporozoite8[d11atvb$Round==4 & d11atvb$Bites==10],
                 d11atvb$Sporozoite9[d11atvb$Round==4 & d11atvb$Bites==10],d11atvb$Sporozoite10[d11atvb$Round==4 & d11atvb$Bites==10])

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


##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1b,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite1b)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2b,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite2b)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite5b,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite5b)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite10b,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite10b,na.rm=T)


##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1c,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite1c)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2c,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite2c)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite5c,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite5c)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite10c,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite10c,na.rm=T)



##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1d,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite1d)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2d,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite2d)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite5d,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite5d)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite10d,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite10d,na.rm=T)


is.mosi<-0
n.rounds=4
n.bites=4
t.bites<-c(1,2,5,10)


par(mfcol=c(4,5),mar= c(3, 4, 1, 2))

is.mosi<-0

##Controls: mouse read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=T)
##Controls: mosquito read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocysts.txt",header=T)

##3D11 & ATV_85%
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
prevMO.round.1<-matrix(ncol=3,nrow=4,c(rep(0,12)))
prevMO.lower.1<-matrix(ncol=3,nrow=4,c(rep(0,12)))
prevMO.upper.1<-matrix(ncol=3,nrow=4,c(rep(0,12)))

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
prevMOI.round.1<-matrix(ncol=3,nrow=4,rep(0,12))
bootMOI.lower1<-matrix(ncol=3,nrow=4,rep(0,12))
bootMOI.upper1<-matrix(ncol=3,nrow=4,rep(0,12))

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