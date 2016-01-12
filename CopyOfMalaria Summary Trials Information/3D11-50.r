##########################################
##
##  Data ## Oocysts
##
##
##########################################

t3D11<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3D11-50\\mosquito.txt",header=TRUE)
t3D11$OocPrev<-ifelse(t3D11$Oocyst==0,0,1)
summary(t3D11)

###Oocyst intensity mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==1 & t3D11$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==1 & t3D11$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==1 & t3D11$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==1 & t3D11$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==1 & t3D11$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==1 & t3D11$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==1 & t3D11$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==1 & t3D11$Bites==10])

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==2 & t3D11$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==2 & t3D11$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==2 & t3D11$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==2 & t3D11$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==2 & t3D11$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==2 & t3D11$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==2 & t3D11$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==2 & t3D11$Bites==10])

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==3 & t3D11$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==3 & t3D11$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==3 & t3D11$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==3 & t3D11$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==3 & t3D11$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==3 & t3D11$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==3 & t3D11$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==3 & t3D11$Bites==10])

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==4 & t3D11$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==4 & t3D11$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==4 & t3D11$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==4 & t3D11$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==4 & t3D11$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==4 & t3D11$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(t3D11$Oocyst[t3D11$Round==4 & t3D11$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$Oocyst[t3D11$Round==4 & t3D11$Bites==10])


###Oocyst prevalence mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==1 & t3D11$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==1 & t3D11$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==1 & t3D11$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==1 & t3D11$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==1 & t3D11$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==1 & t3D11$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==1 & t3D11$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==1 & t3D11$Bites==10])

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==2 & t3D11$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==2 & t3D11$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==2 & t3D11$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==2 & t3D11$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==2 & t3D11$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==2 & t3D11$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==2 & t3D11$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==2 & t3D11$Bites==10])

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==3 & t3D11$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==3 & t3D11$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==3 & t3D11$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==3 & t3D11$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==3 & t3D11$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==3 & t3D11$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==3 & t3D11$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==3 & t3D11$Bites==10])

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==4 & t3D11$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==4 & t3D11$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==4 & t3D11$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==4 & t3D11$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==4 & t3D11$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==4 & t3D11$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11$OocPrev[t3D11$Round==4 & t3D11$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11$OocPrev[t3D11$Round==4 & t3D11$Bites==10])

##########################################
##
##  Data ## BloodStage and Sporozoites
##
##
##########################################

t3D11b<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3D11-50\\mouse.txt",header=TRUE)
t3D11b$bloodstage<-ifelse(t3D11b$Parasitemia==0,0,1)
summary(t3D11b)


###Bloodstage mean and CI95%
##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==1 & t3D11b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==1 & t3D11b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==1 & t3D11b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==1 & t3D11b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==1 & t3D11b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==1 & t3D11b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==1 & t3D11b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==1 & t3D11b$Bites==10])

##Round 2

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==2 & t3D11b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==2 & t3D11b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==2 & t3D11b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==2 & t3D11b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==2 & t3D11b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==2 & t3D11b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==2 & t3D11b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==2 & t3D11b$Bites==10])

##Round 3

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==3 & t3D11b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==3 & t3D11b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==3 & t3D11b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==3 & t3D11b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==3 & t3D11b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==3 & t3D11b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==3 & t3D11b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==3 & t3D11b$Bites==10])

##Round 4

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==4 & t3D11b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==4 & t3D11b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==4 & t3D11b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==4 & t3D11b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==4 & t3D11b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==4 & t3D11b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(t3D11b$bloodstage[t3D11b$Round==4 & t3D11b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(t3D11b$bloodstage[t3D11b$Round==4 & t3D11b$Bites==10])

###Sporozoites mean and CI95%
##create prevalence
t3D11b$sp1<-ifelse(t3D11b$Sporozoite1==0,0,1)
t3D11b$sp2<-ifelse(t3D11b$Sporozoite2==0,0,1)
t3D11b$sp3<-ifelse(t3D11b$Sporozoite3==0,0,1)
t3D11b$sp4<-ifelse(t3D11b$Sporozoite4==0,0,1)
t3D11b$sp5<-ifelse(t3D11b$Sporozoite5==0,0,1)
t3D11b$sp6<-ifelse(t3D11b$Sporozoite6==0,0,1)
t3D11b$sp7<-ifelse(t3D11b$Sporozoite7==0,0,1)
t3D11b$sp8<-ifelse(t3D11b$Sporozoite8==0,0,1)
t3D11b$sp9<-ifelse(t3D11b$Sporozoite9==0,0,1)
t3D11b$sp10<-ifelse(t3D11b$Sporozoite10==0,0,1)


sp1a<-t3D11b$sp1[t3D11b$Round==1 & t3D11b$Bites==1]

sp2a<-c(t3D11b$sp1[t3D11b$Round==1 & t3D11b$Bites==2],t3D11b$sp2[t3D11b$Round==1 & t3D11b$Bites==2])

sp5a<-c(t3D11b$sp1[t3D11b$Round==1 & t3D11b$Bites==5],t3D11b$sp2[t3D11b$Round==1 & t3D11b$Bites==5],
        t3D11b$sp3[t3D11b$Round==1 & t3D11b$Bites==5],t3D11b$sp4[t3D11b$Round==1 & t3D11b$Bites==5],
        t3D11b$sp5[t3D11b$Round==1 & t3D11b$Bites==5])

sp10a<-c(t3D11b$sp1[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$sp2[t3D11b$Round==1 & t3D11b$Bites==10],
         t3D11b$sp3[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$sp4[t3D11b$Round==1 & t3D11b$Bites==10],
         t3D11b$sp5[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$sp6[t3D11b$Round==1 & t3D11b$Bites==10],
         t3D11b$sp7[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$sp8[t3D11b$Round==1 & t3D11b$Bites==10],
         t3D11b$sp9[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$sp10[t3D11b$Round==1 & t3D11b$Bites==10])

sp1b<-t3D11b$sp1[t3D11b$Round==2 & t3D11b$Bites==1]

sp2b<-c(t3D11b$sp1[t3D11b$Round==2 & t3D11b$Bites==2],t3D11b$sp2[t3D11b$Round==2 & t3D11b$Bites==2])

sp5b<-c(t3D11b$sp1[t3D11b$Round==2 & t3D11b$Bites==5],t3D11b$sp2[t3D11b$Round==2 & t3D11b$Bites==5],
        t3D11b$sp3[t3D11b$Round==2 & t3D11b$Bites==5],t3D11b$sp4[t3D11b$Round==2 & t3D11b$Bites==5],
        t3D11b$sp5[t3D11b$Round==2 & t3D11b$Bites==5])

sp10b<-c(t3D11b$sp1[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$sp2[t3D11b$Round==2 & t3D11b$Bites==10],
         t3D11b$sp3[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$sp4[t3D11b$Round==2 & t3D11b$Bites==10],
         t3D11b$sp5[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$sp6[t3D11b$Round==2 & t3D11b$Bites==10],
         t3D11b$sp7[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$sp8[t3D11b$Round==2 & t3D11b$Bites==10],
         t3D11b$sp9[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$sp10[t3D11b$Round==2 & t3D11b$Bites==10])

sp1c<-t3D11b$sp1[t3D11b$Round==3 & t3D11b$Bites==1]

sp2c<-c(t3D11b$sp1[t3D11b$Round==3 & t3D11b$Bites==2],t3D11b$sp2[t3D11b$Round==3 & t3D11b$Bites==2])

sp5c<-c(t3D11b$sp1[t3D11b$Round==3 & t3D11b$Bites==5],t3D11b$sp2[t3D11b$Round==3 & t3D11b$Bites==5],
        t3D11b$sp3[t3D11b$Round==3 & t3D11b$Bites==5],t3D11b$sp4[t3D11b$Round==3 & t3D11b$Bites==5],
        t3D11b$sp5[t3D11b$Round==3 & t3D11b$Bites==5])

sp10c<-c(t3D11b$sp1[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$sp2[t3D11b$Round==3 & t3D11b$Bites==10],
         t3D11b$sp3[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$sp4[t3D11b$Round==3 & t3D11b$Bites==10],
         t3D11b$sp5[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$sp6[t3D11b$Round==3 & t3D11b$Bites==10],
         t3D11b$sp7[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$sp8[t3D11b$Round==3 & t3D11b$Bites==10],
         t3D11b$sp9[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$sp10[t3D11b$Round==3 & t3D11b$Bites==10])

sp1d<-t3D11b$sp1[t3D11b$Round==4 & t3D11b$Bites==1]

sp2d<-c(t3D11b$sp1[t3D11b$Round==4 & t3D11b$Bites==2],t3D11b$sp2[t3D11b$Round==4 & t3D11b$Bites==2])

sp5d<-c(t3D11b$sp1[t3D11b$Round==4 & t3D11b$Bites==5],t3D11b$sp2[t3D11b$Round==4 & t3D11b$Bites==5],
        t3D11b$sp3[t3D11b$Round==4 & t3D11b$Bites==5],t3D11b$sp4[t3D11b$Round==4 & t3D11b$Bites==5],
        t3D11b$sp5[t3D11b$Round==4 & t3D11b$Bites==5])

sp10d<-c(t3D11b$sp1[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$sp2[t3D11b$Round==4 & t3D11b$Bites==10],
         t3D11b$sp3[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$sp4[t3D11b$Round==4 & t3D11b$Bites==10],
         t3D11b$sp5[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$sp6[t3D11b$Round==4 & t3D11b$Bites==10],
         t3D11b$sp7[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$sp8[t3D11b$Round==4 & t3D11b$Bites==10],
         t3D11b$sp9[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$sp10[t3D11b$Round==4 & t3D11b$Bites==10])

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
Sporozoite1a<-t3D11b$Sporozoite1[t3D11b$Round==1 & t3D11b$Bites==1]

Sporozoite2a<-c(t3D11b$Sporozoite1[t3D11b$Round==1 & t3D11b$Bites==2],t3D11b$Sporozoite2[t3D11b$Round==1 & t3D11b$Bites==2])

Sporozoite5a<-c(t3D11b$Sporozoite1[t3D11b$Round==1 & t3D11b$Bites==5],t3D11b$Sporozoite2[t3D11b$Round==1 & t3D11b$Bites==5],
                t3D11b$Sporozoite3[t3D11b$Round==1 & t3D11b$Bites==5],t3D11b$Sporozoite4[t3D11b$Round==1 & t3D11b$Bites==5],
                t3D11b$Sporozoite5[t3D11b$Round==1 & t3D11b$Bites==5])

Sporozoite10a<-c(t3D11b$Sporozoite1[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$Sporozoite2[t3D11b$Round==1 & t3D11b$Bites==10],
                 t3D11b$Sporozoite3[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$Sporozoite4[t3D11b$Round==1 & t3D11b$Bites==10],
                 t3D11b$Sporozoite5[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$Sporozoite6[t3D11b$Round==1 & t3D11b$Bites==10],
                 t3D11b$Sporozoite7[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$Sporozoite8[t3D11b$Round==1 & t3D11b$Bites==10],
                 t3D11b$Sporozoite9[t3D11b$Round==1 & t3D11b$Bites==10],t3D11b$Sporozoite10[t3D11b$Round==1 & t3D11b$Bites==10])

Sporozoite1b<-t3D11b$Sporozoite1[t3D11b$Round==2 & t3D11b$Bites==1]

Sporozoite2b<-c(t3D11b$Sporozoite1[t3D11b$Round==2 & t3D11b$Bites==2],t3D11b$Sporozoite2[t3D11b$Round==2 & t3D11b$Bites==2])

Sporozoite5b<-c(t3D11b$Sporozoite1[t3D11b$Round==2 & t3D11b$Bites==5],t3D11b$Sporozoite2[t3D11b$Round==2 & t3D11b$Bites==5],
                t3D11b$Sporozoite3[t3D11b$Round==2 & t3D11b$Bites==5],t3D11b$Sporozoite4[t3D11b$Round==2 & t3D11b$Bites==5],
                t3D11b$Sporozoite5[t3D11b$Round==2 & t3D11b$Bites==5])

Sporozoite10b<-c(t3D11b$Sporozoite1[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$Sporozoite2[t3D11b$Round==2 & t3D11b$Bites==10],
                 t3D11b$Sporozoite3[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$Sporozoite4[t3D11b$Round==2 & t3D11b$Bites==10],
                 t3D11b$Sporozoite5[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$Sporozoite6[t3D11b$Round==2 & t3D11b$Bites==10],
                 t3D11b$Sporozoite7[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$Sporozoite8[t3D11b$Round==2 & t3D11b$Bites==10],
                 t3D11b$Sporozoite9[t3D11b$Round==2 & t3D11b$Bites==10],t3D11b$Sporozoite10[t3D11b$Round==2 & t3D11b$Bites==10])

Sporozoite1c<-t3D11b$Sporozoite1[t3D11b$Round==3 & t3D11b$Bites==1]

Sporozoite2c<-c(t3D11b$Sporozoite1[t3D11b$Round==3 & t3D11b$Bites==2],t3D11b$Sporozoite2[t3D11b$Round==3 & t3D11b$Bites==2])

Sporozoite5c<-c(t3D11b$Sporozoite1[t3D11b$Round==3 & t3D11b$Bites==5],t3D11b$Sporozoite2[t3D11b$Round==3 & t3D11b$Bites==5],
                t3D11b$Sporozoite3[t3D11b$Round==3 & t3D11b$Bites==5],t3D11b$Sporozoite4[t3D11b$Round==3 & t3D11b$Bites==5],
                t3D11b$Sporozoite5[t3D11b$Round==3 & t3D11b$Bites==5])

Sporozoite10c<-c(t3D11b$Sporozoite1[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$Sporozoite2[t3D11b$Round==3 & t3D11b$Bites==10],
                 t3D11b$Sporozoite3[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$Sporozoite4[t3D11b$Round==3 & t3D11b$Bites==10],
                 t3D11b$Sporozoite5[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$Sporozoite6[t3D11b$Round==3 & t3D11b$Bites==10],
                 t3D11b$Sporozoite7[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$Sporozoite8[t3D11b$Round==3 & t3D11b$Bites==10],
                 t3D11b$Sporozoite9[t3D11b$Round==3 & t3D11b$Bites==10],t3D11b$Sporozoite10[t3D11b$Round==3 & t3D11b$Bites==10])

Sporozoite1d<-t3D11b$Sporozoite1[t3D11b$Round==4 & t3D11b$Bites==1]

Sporozoite2d<-c(t3D11b$Sporozoite1[t3D11b$Round==4 & t3D11b$Bites==2],t3D11b$Sporozoite2[t3D11b$Round==4 & t3D11b$Bites==2])

Sporozoite5d<-c(t3D11b$Sporozoite1[t3D11b$Round==4 & t3D11b$Bites==5],t3D11b$Sporozoite2[t3D11b$Round==4 & t3D11b$Bites==5],
                t3D11b$Sporozoite3[t3D11b$Round==4 & t3D11b$Bites==5],t3D11b$Sporozoite4[t3D11b$Round==4 & t3D11b$Bites==5],
                t3D11b$Sporozoite5[t3D11b$Round==4 & t3D11b$Bites==5])

Sporozoite10d<-c(t3D11b$Sporozoite1[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$Sporozoite2[t3D11b$Round==4 & t3D11b$Bites==10],
                 t3D11b$Sporozoite3[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$Sporozoite4[t3D11b$Round==4 & t3D11b$Bites==10],
                 t3D11b$Sporozoite5[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$Sporozoite6[t3D11b$Round==4 & t3D11b$Bites==10],
                 t3D11b$Sporozoite7[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$Sporozoite8[t3D11b$Round==4 & t3D11b$Bites==10],
                 t3D11b$Sporozoite9[t3D11b$Round==4 & t3D11b$Bites==10],t3D11b$Sporozoite10[t3D11b$Round==4 & t3D11b$Bites==10])

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

##ATV_85%
##Bloodstage
prevH.round.1<-matrix(ncol=4,nrow=4,c(0.2,0,0.2,0.6,0.2,0,0.8,0.8,
                                      0.2,0,1,1,0,0,1,1))
prevH.lower.1<-matrix(ncol=4,nrow=4,c(0,0,0,0.2,0,0,0.4,0.4,
                                      0,0,1,1,0,0,1,1))
prevH.upper.1<-matrix(ncol=4,nrow=4,c(0.6,0,0.6,1,0.6,0,1,1,
                                      0.6,0,1,1,0,0,1,1))

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
prevMO.round.1<-matrix(ncol=4,nrow=4,c(0.08,0,0.1372549,0.4117647,0.12,0,0.78,0.7,
                                       0,0,0.92,0.98,0,0,0.76,0.86))
prevMO.lower.1<-matrix(ncol=4,nrow=4,c(0.02,0,0.05882353,0.2745098,0.04,0,0.66,0.56,
                                       0,0,0.84,0.94,0,0,0.64,0.76))
prevMO.upper.1<-matrix(ncol=4,nrow=4,c(0.16,0,0.23529412,0.5490196,0.22,0,0.88,0.82,
                                       0,0,0.98,1,0,0,0.88,0.94))

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
prevMOI.round.1<-matrix(ncol=4,nrow=4,c(10.04,0,13.09804,36.39216,5.56,0,63.08,57.08,
                                        0,0,45.26,52.56,0,0,30.96,45.62))
bootMOI.lower1<-matrix(ncol=4,nrow=4,c(1.48,0,4.509804,21.88186,1.02,0,44.8390,39.8800,
                                       0,0,36.58,43.60,0,0,21.3395,35.3))
bootMOI.upper1<-matrix(ncol=4,nrow=4,c(21.0205,0,23.196078,52.52941,11.64,0,83.3615,76.2805,
                                       0,0,54.38,61.98,0,0,42.4,57.0805))

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
prevMOS.round.1<-matrix(ncol=4,nrow=4,c(0.4,0.5,0.3181818,0.4081633,0.2,0,0.28,0.58,
                                        0.2,0,0.6,0.7142857,0,0,0.83333,0.8541667))
prevMOS.lower.1<-matrix(ncol=4,nrow=4,c(0,0.2,0.1304348,0.2708333,0,0,0.12,0.44,
                                        0,0,0.4,0.5833333,0,0,0.72,0.66667))
prevMOS.upper.1<-matrix(ncol=4,nrow=4,c(0.8,0.8,0.5217391,0.5434783,0.6,0,0.48,0.72,
                                        0.6,0,0.8,0.8367347,0,0,1,0.8979592))

for(mb in 1:4){
  
  hh=rbind(prevMOS.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMOS.lower.1[mb,])
  ciU<-rbind(prevMOS.upper.1[mb,])
  colnames(hh)<-seq(1,4,1)
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
MOSI.round.1<-matrix(ncol=4,nrow=4,c(0.8,0.8,0.7727273,0.877551,0.4,0,0.88,1.6,
                                     0.6,0,1.68,1.979592,0,0,2.4,2.145833))
bootMOSI.lower1<-matrix(ncol=4,nrow=4,c(0,0.3,0.2727273,0.56,0,0,0.36,1.18,
                                        0,0,1.12,1.56,0,0,2,1.744564))
bootMOSI.upper1<-matrix(ncol=4,nrow=4,c(2,1.4,1.3478261,1.22449,1.2,0,1.48,2.02,
                                        1.8,0,2.24,2.387755,0,0,3.125,2.541667))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3,4)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}



