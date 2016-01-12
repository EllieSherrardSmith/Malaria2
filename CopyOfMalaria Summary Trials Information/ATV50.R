##########################################
##
##  Data ## Oocysts
##
##
##########################################

atv50<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\ATV-50 Graph\\Mosquito.txt",header=TRUE)
atv50$OocPrev<-ifelse(atv50$Oocyst==0,0,1)
summary(atv50)

###Oocyst intensity mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==1 & atv50$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==1 & atv50$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==1 & atv50$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==1 & atv50$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==1 & atv50$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==1 & atv50$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==1 & atv50$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==1 & atv50$Bites==10])

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==2 & atv50$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==2 & atv50$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==2 & atv50$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==2 & atv50$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==2 & atv50$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==2 & atv50$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==2 & atv50$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==2 & atv50$Bites==10])

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==3 & atv50$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==3 & atv50$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==3 & atv50$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==3 & atv50$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==3 & atv50$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==3 & atv50$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==3 & atv50$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==3 & atv50$Bites==10])

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==4 & atv50$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==4 & atv50$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==4 & atv50$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==4 & atv50$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==4 & atv50$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==4 & atv50$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv50$Oocyst[atv50$Round==4 & atv50$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$Oocyst[atv50$Round==4 & atv50$Bites==10])

###Oocyst prevalence mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==1 & atv50$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==1 & atv50$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==1 & atv50$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==1 & atv50$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==1 & atv50$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==1 & atv50$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==1 & atv50$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==1 & atv50$Bites==10])

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==2 & atv50$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==2 & atv50$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==2 & atv50$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==2 & atv50$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==2 & atv50$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==2 & atv50$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==2 & atv50$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==2 & atv50$Bites==10])

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==3 & atv50$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==3 & atv50$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==3 & atv50$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==3 & atv50$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==3 & atv50$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==3 & atv50$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==3 & atv50$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==3 & atv50$Bites==10])

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==4 & atv50$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==4 & atv50$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==4 & atv50$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==4 & atv50$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==4 & atv50$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==4 & atv50$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50$OocPrev[atv50$Round==4 & atv50$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50$OocPrev[atv50$Round==4 & atv50$Bites==10])


##########################################
##
##  Data ## BloodStage and Sporozoites
##
##
##########################################

atv50b<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\ATV-50 Graph\\Mouse.txt",header=TRUE)
atv50b$bloodstage<-ifelse(atv50b$Parasitemia==0,0,1)
summary(atv50b)


###Bloodstage mean and CI95%
##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==1 & atv50b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==1 & atv50b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==1 & atv50b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==1 & atv50b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==1 & atv50b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==1 & atv50b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==1 & atv50b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==1 & atv50b$Bites==10])

##Round 2

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==2 & atv50b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==2 & atv50b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==2 & atv50b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==2 & atv50b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==2 & atv50b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==2 & atv50b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==2 & atv50b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==2 & atv50b$Bites==10])

##Round 3

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==3 & atv50b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==3 & atv50b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==3 & atv50b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==3 & atv50b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==3 & atv50b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==3 & atv50b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==3 & atv50b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==3 & atv50b$Bites==10])

##Round 4

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==4 & atv50b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==4 & atv50b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==4 & atv50b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==4 & atv50b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==4 & atv50b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==4 & atv50b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv50b$bloodstage[atv50b$Round==4 & atv50b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv50b$bloodstage[atv50b$Round==4 & atv50b$Bites==10])

###Sporozoites mean and CI95%
##create prevalence
atv50b$sp1<-ifelse(atv50b$Sporozoite1==0,0,1)
atv50b$sp2<-ifelse(atv50b$Sporozoite2==0,0,1)
atv50b$sp3<-ifelse(atv50b$Sporozoite3==0,0,1)
atv50b$sp4<-ifelse(atv50b$Sporozoite4==0,0,1)
atv50b$sp5<-ifelse(atv50b$Sporozoite5==0,0,1)
atv50b$sp6<-ifelse(atv50b$Sporozoite6==0,0,1)
atv50b$sp7<-ifelse(atv50b$Sporozoite7==0,0,1)
atv50b$sp8<-ifelse(atv50b$Sporozoite8==0,0,1)
atv50b$sp9<-ifelse(atv50b$Sporozoite9==0,0,1)
atv50b$sp10<-ifelse(atv50b$Sporozoite10==0,0,1)


sp1a<-atv50b$sp1[atv50b$Round==1 & atv50b$Bites==1]

sp2a<-c(atv50b$sp1[atv50b$Round==1 & atv50b$Bites==2],atv50b$sp2[atv50b$Round==1 & atv50b$Bites==2])

sp5a<-c(atv50b$sp1[atv50b$Round==1 & atv50b$Bites==5],atv50b$sp2[atv50b$Round==1 & atv50b$Bites==5],
        atv50b$sp3[atv50b$Round==1 & atv50b$Bites==5],atv50b$sp4[atv50b$Round==1 & atv50b$Bites==5],
        atv50b$sp5[atv50b$Round==1 & atv50b$Bites==5])

sp10a<-c(atv50b$sp1[atv50b$Round==1 & atv50b$Bites==10],atv50b$sp2[atv50b$Round==1 & atv50b$Bites==10],
         atv50b$sp3[atv50b$Round==1 & atv50b$Bites==10],atv50b$sp4[atv50b$Round==1 & atv50b$Bites==10],
         atv50b$sp5[atv50b$Round==1 & atv50b$Bites==10],atv50b$sp6[atv50b$Round==1 & atv50b$Bites==10],
         atv50b$sp7[atv50b$Round==1 & atv50b$Bites==10],atv50b$sp8[atv50b$Round==1 & atv50b$Bites==10],
         atv50b$sp9[atv50b$Round==1 & atv50b$Bites==10],atv50b$sp10[atv50b$Round==1 & atv50b$Bites==10])

sp1b<-atv50b$sp1[atv50b$Round==2 & atv50b$Bites==1]

sp2b<-c(atv50b$sp1[atv50b$Round==2 & atv50b$Bites==2],atv50b$sp2[atv50b$Round==2 & atv50b$Bites==2])

sp5b<-c(atv50b$sp1[atv50b$Round==2 & atv50b$Bites==5],atv50b$sp2[atv50b$Round==2 & atv50b$Bites==5],
        atv50b$sp3[atv50b$Round==2 & atv50b$Bites==5],atv50b$sp4[atv50b$Round==2 & atv50b$Bites==5],
        atv50b$sp5[atv50b$Round==2 & atv50b$Bites==5])

sp10b<-c(atv50b$sp1[atv50b$Round==2 & atv50b$Bites==10],atv50b$sp2[atv50b$Round==2 & atv50b$Bites==10],
         atv50b$sp3[atv50b$Round==2 & atv50b$Bites==10],atv50b$sp4[atv50b$Round==2 & atv50b$Bites==10],
         atv50b$sp5[atv50b$Round==2 & atv50b$Bites==10],atv50b$sp6[atv50b$Round==2 & atv50b$Bites==10],
         atv50b$sp7[atv50b$Round==2 & atv50b$Bites==10],atv50b$sp8[atv50b$Round==2 & atv50b$Bites==10],
         atv50b$sp9[atv50b$Round==2 & atv50b$Bites==10],atv50b$sp10[atv50b$Round==2 & atv50b$Bites==10])

sp1c<-atv50b$sp1[atv50b$Round==3 & atv50b$Bites==1]

sp2c<-c(atv50b$sp1[atv50b$Round==3 & atv50b$Bites==2],atv50b$sp2[atv50b$Round==3 & atv50b$Bites==2])

sp5c<-c(atv50b$sp1[atv50b$Round==3 & atv50b$Bites==5],atv50b$sp2[atv50b$Round==3 & atv50b$Bites==5],
        atv50b$sp3[atv50b$Round==3 & atv50b$Bites==5],atv50b$sp4[atv50b$Round==3 & atv50b$Bites==5],
        atv50b$sp5[atv50b$Round==3 & atv50b$Bites==5])

sp10c<-c(atv50b$sp1[atv50b$Round==3 & atv50b$Bites==10],atv50b$sp2[atv50b$Round==3 & atv50b$Bites==10],
         atv50b$sp3[atv50b$Round==3 & atv50b$Bites==10],atv50b$sp4[atv50b$Round==3 & atv50b$Bites==10],
         atv50b$sp5[atv50b$Round==3 & atv50b$Bites==10],atv50b$sp6[atv50b$Round==3 & atv50b$Bites==10],
         atv50b$sp7[atv50b$Round==3 & atv50b$Bites==10],atv50b$sp8[atv50b$Round==3 & atv50b$Bites==10],
         atv50b$sp9[atv50b$Round==3 & atv50b$Bites==10],atv50b$sp10[atv50b$Round==3 & atv50b$Bites==10])

sp1d<-atv50b$sp1[atv50b$Round==4 & atv50b$Bites==1]

sp2d<-c(atv50b$sp1[atv50b$Round==4 & atv50b$Bites==2],atv50b$sp2[atv50b$Round==4 & atv50b$Bites==2])

sp5d<-c(atv50b$sp1[atv50b$Round==4 & atv50b$Bites==5],atv50b$sp2[atv50b$Round==4 & atv50b$Bites==5],
        atv50b$sp3[atv50b$Round==4 & atv50b$Bites==5],atv50b$sp4[atv50b$Round==4 & atv50b$Bites==5],
        atv50b$sp5[atv50b$Round==4 & atv50b$Bites==5])

sp10d<-c(atv50b$sp1[atv50b$Round==4 & atv50b$Bites==10],atv50b$sp2[atv50b$Round==4 & atv50b$Bites==10],
         atv50b$sp3[atv50b$Round==4 & atv50b$Bites==10],atv50b$sp4[atv50b$Round==4 & atv50b$Bites==10],
         atv50b$sp5[atv50b$Round==4 & atv50b$Bites==10],atv50b$sp6[atv50b$Round==4 & atv50b$Bites==10],
         atv50b$sp7[atv50b$Round==4 & atv50b$Bites==10],atv50b$sp8[atv50b$Round==4 & atv50b$Bites==10],
         atv50b$sp9[atv50b$Round==4 & atv50b$Bites==10],atv50b$sp10[atv50b$Round==4 & atv50b$Bites==10])

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
Sporozoite1a<-atv50b$Sporozoite1[atv50b$Round==1 & atv50b$Bites==1]

Sporozoite2a<-c(atv50b$Sporozoite1[atv50b$Round==1 & atv50b$Bites==2],atv50b$Sporozoite2[atv50b$Round==1 & atv50b$Bites==2])

Sporozoite5a<-c(atv50b$Sporozoite1[atv50b$Round==1 & atv50b$Bites==5],atv50b$Sporozoite2[atv50b$Round==1 & atv50b$Bites==5],
                atv50b$Sporozoite3[atv50b$Round==1 & atv50b$Bites==5],atv50b$Sporozoite4[atv50b$Round==1 & atv50b$Bites==5],
                atv50b$Sporozoite5[atv50b$Round==1 & atv50b$Bites==5])

Sporozoite10a<-c(atv50b$Sporozoite1[atv50b$Round==1 & atv50b$Bites==10],atv50b$Sporozoite2[atv50b$Round==1 & atv50b$Bites==10],
                 atv50b$Sporozoite3[atv50b$Round==1 & atv50b$Bites==10],atv50b$Sporozoite4[atv50b$Round==1 & atv50b$Bites==10],
                 atv50b$Sporozoite5[atv50b$Round==1 & atv50b$Bites==10],atv50b$Sporozoite6[atv50b$Round==1 & atv50b$Bites==10],
                 atv50b$Sporozoite7[atv50b$Round==1 & atv50b$Bites==10],atv50b$Sporozoite8[atv50b$Round==1 & atv50b$Bites==10],
                 atv50b$Sporozoite9[atv50b$Round==1 & atv50b$Bites==10],atv50b$Sporozoite10[atv50b$Round==1 & atv50b$Bites==10])

Sporozoite1b<-atv50b$Sporozoite1[atv50b$Round==2 & atv50b$Bites==1]

Sporozoite2b<-c(atv50b$Sporozoite1[atv50b$Round==2 & atv50b$Bites==2],atv50b$Sporozoite2[atv50b$Round==2 & atv50b$Bites==2])

Sporozoite5b<-c(atv50b$Sporozoite1[atv50b$Round==2 & atv50b$Bites==5],atv50b$Sporozoite2[atv50b$Round==2 & atv50b$Bites==5],
                atv50b$Sporozoite3[atv50b$Round==2 & atv50b$Bites==5],atv50b$Sporozoite4[atv50b$Round==2 & atv50b$Bites==5],
                atv50b$Sporozoite5[atv50b$Round==2 & atv50b$Bites==5])

Sporozoite10b<-c(atv50b$Sporozoite1[atv50b$Round==2 & atv50b$Bites==10],atv50b$Sporozoite2[atv50b$Round==2 & atv50b$Bites==10],
                 atv50b$Sporozoite3[atv50b$Round==2 & atv50b$Bites==10],atv50b$Sporozoite4[atv50b$Round==2 & atv50b$Bites==10],
                 atv50b$Sporozoite5[atv50b$Round==2 & atv50b$Bites==10],atv50b$Sporozoite6[atv50b$Round==2 & atv50b$Bites==10],
                 atv50b$Sporozoite7[atv50b$Round==2 & atv50b$Bites==10],atv50b$Sporozoite8[atv50b$Round==2 & atv50b$Bites==10],
                 atv50b$Sporozoite9[atv50b$Round==2 & atv50b$Bites==10],atv50b$Sporozoite10[atv50b$Round==2 & atv50b$Bites==10])

Sporozoite1c<-atv50b$Sporozoite1[atv50b$Round==3 & atv50b$Bites==1]

Sporozoite2c<-c(atv50b$Sporozoite1[atv50b$Round==3 & atv50b$Bites==2],atv50b$Sporozoite2[atv50b$Round==3 & atv50b$Bites==2])

Sporozoite5c<-c(atv50b$Sporozoite1[atv50b$Round==3 & atv50b$Bites==5],atv50b$Sporozoite2[atv50b$Round==3 & atv50b$Bites==5],
                atv50b$Sporozoite3[atv50b$Round==3 & atv50b$Bites==5],atv50b$Sporozoite4[atv50b$Round==3 & atv50b$Bites==5],
                atv50b$Sporozoite5[atv50b$Round==3 & atv50b$Bites==5])

Sporozoite10c<-c(atv50b$Sporozoite1[atv50b$Round==3 & atv50b$Bites==10],atv50b$Sporozoite2[atv50b$Round==3 & atv50b$Bites==10],
                 atv50b$Sporozoite3[atv50b$Round==3 & atv50b$Bites==10],atv50b$Sporozoite4[atv50b$Round==3 & atv50b$Bites==10],
                 atv50b$Sporozoite5[atv50b$Round==3 & atv50b$Bites==10],atv50b$Sporozoite6[atv50b$Round==3 & atv50b$Bites==10],
                 atv50b$Sporozoite7[atv50b$Round==3 & atv50b$Bites==10],atv50b$Sporozoite8[atv50b$Round==3 & atv50b$Bites==10],
                 atv50b$Sporozoite9[atv50b$Round==3 & atv50b$Bites==10],atv50b$Sporozoite10[atv50b$Round==3 & atv50b$Bites==10])

Sporozoite1d<-atv50b$Sporozoite1[atv50b$Round==4 & atv50b$Bites==1]

Sporozoite2d<-c(atv50b$Sporozoite1[atv50b$Round==4 & atv50b$Bites==2],atv50b$Sporozoite2[atv50b$Round==4 & atv50b$Bites==2])

Sporozoite5d<-c(atv50b$Sporozoite1[atv50b$Round==4 & atv50b$Bites==5],atv50b$Sporozoite2[atv50b$Round==4 & atv50b$Bites==5],
        atv50b$Sporozoite3[atv50b$Round==4 & atv50b$Bites==5],atv50b$Sporozoite4[atv50b$Round==4 & atv50b$Bites==5],
        atv50b$Sporozoite5[atv50b$Round==4 & atv50b$Bites==5])

Sporozoite10d<-c(atv50b$Sporozoite1[atv50b$Round==4 & atv50b$Bites==10],atv50b$Sporozoite2[atv50b$Round==4 & atv50b$Bites==10],
         atv50b$Sporozoite3[atv50b$Round==4 & atv50b$Bites==10],atv50b$Sporozoite4[atv50b$Round==4 & atv50b$Bites==10],
         atv50b$Sporozoite5[atv50b$Round==4 & atv50b$Bites==10],atv50b$Sporozoite6[atv50b$Round==4 & atv50b$Bites==10],
         atv50b$Sporozoite7[atv50b$Round==4 & atv50b$Bites==10],atv50b$Sporozoite8[atv50b$Round==4 & atv50b$Bites==10],
         atv50b$Sporozoite9[atv50b$Round==4 & atv50b$Bites==10],atv50b$Sporozoite10[atv50b$Round==4 & atv50b$Bites==10])

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

##ATV_50%
##Bloodstage
prevH.round.1<-matrix(ncol=4,nrow=4,c(0.8,1,1,1,0.4,0.2,0.2,1,
                                      0,0.2,0.2,0.4,0,0.2,0.4,0.4))
prevH.lower.1<-matrix(ncol=4,nrow=4,c(0.4,1,1,1,0,0.2,0.2,1,
                                      0,0,0,0,0,0,0,0))
prevH.upper.1<-matrix(ncol=4,nrow=4,c(1,1,1,1,0.8,1,1,1,
                                      0,0.6,0.6,0.8,0,0.6,0.8,0.8))

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
  segments(mp, ciL*100, mp, ciU*100, col = mybarcol, lwd = 1.5)
  
}

##Oocyst prevalence
prevMO.round.1<-matrix(ncol=4,nrow=4,c(0.3529412,0.4901961,0.5294118,0.7254902,0.08,0.18,0.4,0.54,
                                       0,0.08,0.16,0.32,0,0,0.2,0.28))
prevMO.lower.1<-matrix(ncol=4,nrow=4,c(0.2156863,0.3529412,0.3921569,0.6078431,0.02,0.08,0.26,0.40,
                                       0,0.02,0.06,0.20,0,0,0.10,0.16))
prevMO.upper.1<-matrix(ncol=4,nrow=4,c(0.4901961,0.6274510,0.6666667,0.8431373,0.16,0.30,0.54,0.68,
                                       0,0.16,0.26,0.46,0,0,0.32,0.40))

for(mb in 1:4){
  
  hh=rbind(prevMO.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMO.lower.1[mb,])
  ciU<-rbind(prevMO.upper.1[mb,])
  colnames(hh)<-seq(1,4,1)
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
prevMOI.round.1<-matrix(ncol=4,nrow=4,c(3.196078,9.196078,9.509804,24.68627,1.5,1.64,3.72,6.54,
                                        0,1.1,3.22,3.02,0,0,3.66,3.6))
bootMOI.lower1<-matrix(ncol=4,nrow=4,c(1.529412,4.960784,5.235294,16.25490,0.02,0.34,1.94,3.86,
                                       0,0.06,0.4,1.46,0,0,1.24,1.36))
bootMOI.upper1<-matrix(ncol=4,nrow=4,c(5.117647,14.117647,14.451471,34.47059,3.76,3.52,5.84,9.80,
                                       0,2.64,7.14,4.86,0,0,6.66,6.54))

for(mb in 1:4){
  
  hh=rbind(prevMOI.round.1[mb,])
  hh.max<-apply(hh, 2, max)+5
  ciL<-rbind(bootMOI.lower1[mb,])
  ciU<-rbind(bootMOI.upper1[mb,])
  colnames(hh)<-seq(1,4,1)
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
prevMOS.round.1<-matrix(ncol=3,nrow=4,c(0.4,0.555556,0.56,0.4897959,0.2,0.2,0.28,0.2653061,
                                        0,0.2,0.28,0.3265306))
prevMOS.lower.1<-matrix(ncol=3,nrow=4,c(0,0.222222,0.36,0.3469388,0,0,0.12,0.142857,
                                        0,0,0.12,0.2))
prevMOS.upper.1<-matrix(ncol=3,nrow=4,c(0.8,0.888889,0.76,0.6304348,0.6,0.5,0.48,0.3958333,
                                        0,0.5,0.48,0.46))

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
MOSI.round.1<-matrix(ncol=3,nrow=4,c(0.6,1,1.08,0.9387755,0.2,0.4,0.64,0.6938776,
                                     0,0.4,0.56,0.9183673))
bootMOSI.lower1<-matrix(ncol=3,nrow=4,c(0,0.3333,0.64,0.6326531,0,0,0.24,0.36,
                                        0,0,0.2,0.5416667))
bootMOSI.upper1<-matrix(ncol=3,nrow=4,c(1.4,1.7,1.56,1.2653061,0.6,1,1.08,1.06,
                                        0,1,0.96,1.3265306))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}
