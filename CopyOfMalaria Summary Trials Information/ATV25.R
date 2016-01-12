##########################################
##
##  Data ## Oocysts
##
##
##########################################

atv25<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mosquito.txt",header=TRUE)
atv25$OocPrev<-ifelse(atv25$Oocyst==0,0,1)
summary(atv25)

###Oocyst intensity mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==1 & atv25$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==1 & atv25$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==1 & atv25$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==1 & atv25$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==1 & atv25$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==1 & atv25$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==1 & atv25$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==1 & atv25$Bites==10])

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==2 & atv25$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==2 & atv25$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==2 & atv25$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==2 & atv25$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==2 & atv25$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==2 & atv25$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==2 & atv25$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==2 & atv25$Bites==10])

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==3 & atv25$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==3 & atv25$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==3 & atv25$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==3 & atv25$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==3 & atv25$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==3 & atv25$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==3 & atv25$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==3 & atv25$Bites==10])

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==4 & atv25$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==4 & atv25$Bites==1])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==4 & atv25$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==4 & atv25$Bites==2])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==4 & atv25$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==4 & atv25$Bites==5])

for(i in 1:10000) a[i] <- mean(sample(atv25$Oocyst[atv25$Round==4 & atv25$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$Oocyst[atv25$Round==4 & atv25$Bites==10])

###Oocyst prevalence mean and CI95%
##Round 1
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==1 & atv25$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==1 & atv25$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==1 & atv25$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==1 & atv25$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==1 & atv25$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==1 & atv25$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==1 & atv25$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==1 & atv25$Bites==10])

##Round 2
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==2 & atv25$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==2 & atv25$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==2 & atv25$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==2 & atv25$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==2 & atv25$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==2 & atv25$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==2 & atv25$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==2 & atv25$Bites==10])

##Round 3
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==3 & atv25$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==3 & atv25$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==3 & atv25$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==3 & atv25$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==3 & atv25$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==3 & atv25$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==3 & atv25$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==3 & atv25$Bites==10])

##Round 4
a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==4 & atv25$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==4 & atv25$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==4 & atv25$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==4 & atv25$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==4 & atv25$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==4 & atv25$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25$OocPrev[atv25$Round==4 & atv25$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25$OocPrev[atv25$Round==4 & atv25$Bites==10])


##########################################
##
##  Data ## BloodStage and Sporozoites
##
##
##########################################

atv25b<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mouse.txt",header=TRUE)
atv25b$bloodstage<-ifelse(atv25b$Parasitemia==0,0,1)
summary(atv25b)


###Bloodstage mean and CI95%
##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==1 & atv25b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==1 & atv25b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==1 & atv25b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==1 & atv25b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==1 & atv25b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==1 & atv25b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==1 & atv25b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==1 & atv25b$Bites==10])

##Round 2

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==2 & atv25b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==2 & atv25b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==2 & atv25b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==2 & atv25b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==2 & atv25b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==2 & atv25b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==2 & atv25b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==2 & atv25b$Bites==10])
##Round 3

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==3 & atv25b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==3 & atv25b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==3 & atv25b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==3 & atv25b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==3 & atv25b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==3 & atv25b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==3 & atv25b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==3 & atv25b$Bites==10])

##Round 4

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==4 & atv25b$Bites==1],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==4 & atv25b$Bites==1])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==4 & atv25b$Bites==2],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==4 & atv25b$Bites==2])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==4 & atv25b$Bites==5],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==4 & atv25b$Bites==5])

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(atv25b$bloodstage[atv25b$Round==4 & atv25b$Bites==10],replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(atv25b$bloodstage[atv25b$Round==4 & atv25b$Bites==10])

###Sporozoites mean and CI95%
##create prevalence
atv25b$sp1<-ifelse(atv25b$Sporozoite1==0,0,1)
atv25b$sp2<-ifelse(atv25b$Sporozoite2==0,0,1)
atv25b$sp3<-ifelse(atv25b$Sporozoite3==0,0,1)
atv25b$sp4<-ifelse(atv25b$Sporozoite4==0,0,1)
atv25b$sp5<-ifelse(atv25b$Sporozoite5==0,0,1)
atv25b$sp6<-ifelse(atv25b$Sporozoite6==0,0,1)
atv25b$sp7<-ifelse(atv25b$Sporozoite7==0,0,1)
atv25b$sp8<-ifelse(atv25b$Sporozoite8==0,0,1)
atv25b$sp9<-ifelse(atv25b$Sporozoite9==0,0,1)
atv25b$sp10<-ifelse(atv25b$Sporozoite10==0,0,1)


sp1a<-atv25b$sp1[atv25b$Round==1 & atv25b$Bites==1]

sp2a<-c(atv25b$sp1[atv25b$Round==1 & atv25b$Bites==2],atv25b$sp2[atv25b$Round==1 & atv25b$Bites==2])

sp5a<-c(atv25b$sp1[atv25b$Round==1 & atv25b$Bites==5],atv25b$sp2[atv25b$Round==1 & atv25b$Bites==5],
        atv25b$sp3[atv25b$Round==1 & atv25b$Bites==5],atv25b$sp4[atv25b$Round==1 & atv25b$Bites==5],
        atv25b$sp5[atv25b$Round==1 & atv25b$Bites==5])

sp10a<-c(atv25b$sp1[atv25b$Round==1 & atv25b$Bites==10],atv25b$sp2[atv25b$Round==1 & atv25b$Bites==10],
         atv25b$sp3[atv25b$Round==1 & atv25b$Bites==10],atv25b$sp4[atv25b$Round==1 & atv25b$Bites==10],
         atv25b$sp5[atv25b$Round==1 & atv25b$Bites==10],atv25b$sp6[atv25b$Round==1 & atv25b$Bites==10],
         atv25b$sp7[atv25b$Round==1 & atv25b$Bites==10],atv25b$sp8[atv25b$Round==1 & atv25b$Bites==10],
         atv25b$sp9[atv25b$Round==1 & atv25b$Bites==10],atv25b$sp10[atv25b$Round==1 & atv25b$Bites==10])

sp1b<-atv25b$sp1[atv25b$Round==2 & atv25b$Bites==1]

sp2b<-c(atv25b$sp1[atv25b$Round==2 & atv25b$Bites==2],atv25b$sp2[atv25b$Round==2 & atv25b$Bites==2])

sp5b<-c(atv25b$sp1[atv25b$Round==2 & atv25b$Bites==5],atv25b$sp2[atv25b$Round==2 & atv25b$Bites==5],
        atv25b$sp3[atv25b$Round==2 & atv25b$Bites==5],atv25b$sp4[atv25b$Round==2 & atv25b$Bites==5],
        atv25b$sp5[atv25b$Round==2 & atv25b$Bites==5])

sp10b<-c(atv25b$sp1[atv25b$Round==2 & atv25b$Bites==10],atv25b$sp2[atv25b$Round==2 & atv25b$Bites==10],
         atv25b$sp3[atv25b$Round==2 & atv25b$Bites==10],atv25b$sp4[atv25b$Round==2 & atv25b$Bites==10],
         atv25b$sp5[atv25b$Round==2 & atv25b$Bites==10],atv25b$sp6[atv25b$Round==2 & atv25b$Bites==10],
         atv25b$sp7[atv25b$Round==2 & atv25b$Bites==10],atv25b$sp8[atv25b$Round==2 & atv25b$Bites==10],
         atv25b$sp9[atv25b$Round==2 & atv25b$Bites==10],atv25b$sp10[atv25b$Round==2 & atv25b$Bites==10])

sp1c<-atv25b$sp1[atv25b$Round==3 & atv25b$Bites==1]

sp2c<-c(atv25b$sp1[atv25b$Round==3 & atv25b$Bites==2],atv25b$sp2[atv25b$Round==3 & atv25b$Bites==2])

sp5c<-c(atv25b$sp1[atv25b$Round==3 & atv25b$Bites==5],atv25b$sp2[atv25b$Round==3 & atv25b$Bites==5],
        atv25b$sp3[atv25b$Round==3 & atv25b$Bites==5],atv25b$sp4[atv25b$Round==3 & atv25b$Bites==5],
        atv25b$sp5[atv25b$Round==3 & atv25b$Bites==5])

sp10c<-c(atv25b$sp1[atv25b$Round==3 & atv25b$Bites==10],atv25b$sp2[atv25b$Round==3 & atv25b$Bites==10],
         atv25b$sp3[atv25b$Round==3 & atv25b$Bites==10],atv25b$sp4[atv25b$Round==3 & atv25b$Bites==10],
         atv25b$sp5[atv25b$Round==3 & atv25b$Bites==10],atv25b$sp6[atv25b$Round==3 & atv25b$Bites==10],
         atv25b$sp7[atv25b$Round==3 & atv25b$Bites==10],atv25b$sp8[atv25b$Round==3 & atv25b$Bites==10],
         atv25b$sp9[atv25b$Round==3 & atv25b$Bites==10],atv25b$sp10[atv25b$Round==3 & atv25b$Bites==10])

##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp1a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp1a)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp2a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp2a)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp5a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp5a)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(sp10a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(sp10a)


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
mean(sp10c)


###Sporozoite intensity
Sporozoite1a<-atv25b$Sporozoite1[atv25b$Round==1 & atv25b$Bites==1]

Sporozoite2a<-c(atv25b$Sporozoite1[atv25b$Round==1 & atv25b$Bites==2],atv25b$Sporozoite2[atv25b$Round==1 & atv25b$Bites==2])

Sporozoite5a<-c(atv25b$Sporozoite1[atv25b$Round==1 & atv25b$Bites==5],atv25b$Sporozoite2[atv25b$Round==1 & atv25b$Bites==5],
        atv25b$Sporozoite3[atv25b$Round==1 & atv25b$Bites==5],atv25b$Sporozoite4[atv25b$Round==1 & atv25b$Bites==5],
        atv25b$Sporozoite5[atv25b$Round==1 & atv25b$Bites==5])

Sporozoite10a<-c(atv25b$Sporozoite1[atv25b$Round==1 & atv25b$Bites==10],atv25b$Sporozoite2[atv25b$Round==1 & atv25b$Bites==10],
         atv25b$Sporozoite3[atv25b$Round==1 & atv25b$Bites==10],atv25b$Sporozoite4[atv25b$Round==1 & atv25b$Bites==10],
         atv25b$Sporozoite5[atv25b$Round==1 & atv25b$Bites==10],atv25b$Sporozoite6[atv25b$Round==1 & atv25b$Bites==10],
         atv25b$Sporozoite7[atv25b$Round==1 & atv25b$Bites==10],atv25b$Sporozoite8[atv25b$Round==1 & atv25b$Bites==10],
         atv25b$Sporozoite9[atv25b$Round==1 & atv25b$Bites==10],atv25b$Sporozoite10[atv25b$Round==1 & atv25b$Bites==10])

Sporozoite1b<-atv25b$Sporozoite1[atv25b$Round==2 & atv25b$Bites==1]

Sporozoite2b<-c(atv25b$Sporozoite1[atv25b$Round==2 & atv25b$Bites==2],atv25b$Sporozoite2[atv25b$Round==2 & atv25b$Bites==2])

Sporozoite5b<-c(atv25b$Sporozoite1[atv25b$Round==2 & atv25b$Bites==5],atv25b$Sporozoite2[atv25b$Round==2 & atv25b$Bites==5],
        atv25b$Sporozoite3[atv25b$Round==2 & atv25b$Bites==5],atv25b$Sporozoite4[atv25b$Round==2 & atv25b$Bites==5],
        atv25b$Sporozoite5[atv25b$Round==2 & atv25b$Bites==5])

Sporozoite10b<-c(atv25b$Sporozoite1[atv25b$Round==2 & atv25b$Bites==10],atv25b$Sporozoite2[atv25b$Round==2 & atv25b$Bites==10],
         atv25b$Sporozoite3[atv25b$Round==2 & atv25b$Bites==10],atv25b$Sporozoite4[atv25b$Round==2 & atv25b$Bites==10],
         atv25b$Sporozoite5[atv25b$Round==2 & atv25b$Bites==10],atv25b$Sporozoite6[atv25b$Round==2 & atv25b$Bites==10],
         atv25b$Sporozoite7[atv25b$Round==2 & atv25b$Bites==10],atv25b$Sporozoite8[atv25b$Round==2 & atv25b$Bites==10],
         atv25b$Sporozoite9[atv25b$Round==2 & atv25b$Bites==10],atv25b$Sporozoite10[atv25b$Round==2 & atv25b$Bites==10])

Sporozoite1c<-atv25b$Sporozoite1[atv25b$Round==3 & atv25b$Bites==1]

Sporozoite2c<-c(atv25b$Sporozoite1[atv25b$Round==3 & atv25b$Bites==2],atv25b$Sporozoite2[atv25b$Round==3 & atv25b$Bites==2])

Sporozoite5c<-c(atv25b$Sporozoite1[atv25b$Round==3 & atv25b$Bites==5],atv25b$Sporozoite2[atv25b$Round==3 & atv25b$Bites==5],
        atv25b$Sporozoite3[atv25b$Round==3 & atv25b$Bites==5],atv25b$Sporozoite4[atv25b$Round==3 & atv25b$Bites==5],
        atv25b$Sporozoite5[atv25b$Round==3 & atv25b$Bites==5])

Sporozoite10c<-c(atv25b$Sporozoite1[atv25b$Round==3 & atv25b$Bites==10],atv25b$Sporozoite2[atv25b$Round==3 & atv25b$Bites==10],
         atv25b$Sporozoite3[atv25b$Round==3 & atv25b$Bites==10],atv25b$Sporozoite4[atv25b$Round==3 & atv25b$Bites==10],
         atv25b$Sporozoite5[atv25b$Round==3 & atv25b$Bites==10],atv25b$Sporozoite6[atv25b$Round==3 & atv25b$Bites==10],
         atv25b$Sporozoite7[atv25b$Round==3 & atv25b$Bites==10],atv25b$Sporozoite8[atv25b$Round==3 & atv25b$Bites==10],
         atv25b$Sporozoite9[atv25b$Round==3 & atv25b$Bites==10],atv25b$Sporozoite10[atv25b$Round==3 & atv25b$Bites==10])

##Round 1

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite1a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite1a)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite2a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite2a)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite5a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite5a)

a<-numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(Sporozoite10a,replace=T),na.rm=T)
quantile(a,c(0.025,0.975))
mean(Sporozoite10a)


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
mean(Sporozoite10c)


is.mosi<-0
n.rounds=4
n.bites=4
t.bites<-c(1,2,5,10)


par(mfcol=c(4,5),mar= c(3, 4, 1, 2))

is.mosi<-0

##Controls: mouse read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=T)
##Controls: mosquito read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocysts.txt",header=T)

##ATV_25%
##Bloodstage
prevH.round.1<-matrix(ncol=4,nrow=4,c(0.4,1,0.8,1,0,1,1,1,0.2,1,1,1,0,0.2,1,1))
prevH.lower.1<-matrix(ncol=4,nrow=4,c(0.0,1,0.4,1,0,1,1,1,0.0,1,1,1,0,0,1,1))
prevH.upper.1<-matrix(ncol=4,nrow=4,c(0.8,1,1,1,0,1,1,1,0.6,1,1,1,0,0.6,1,1))

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
prevMO.round.1<-matrix(ncol=3,nrow=4,c(0.2,0.74,0.5,0.72,0.08,0.82,0.92,0.82,0,0.44,0.7,0.62))
prevMO.lower.1<-matrix(ncol=3,nrow=4,c(0.1,0.62,0.36,0.58,0.02,0.7,0.84,0.72,0,0.3,0.58,0.48))
prevMO.upper.1<-matrix(ncol=3,nrow=4,c(0.32,0.86,0.64,0.84,0.16,0.92,0.98,0.92,0,0.58,0.82,0.76))

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
prevMOI.round.1<-matrix(ncol=3,nrow=4,c(1.6,20.58,6.78,28.5,0.12,30.56,34.9,41.28,
                                        0,15.68,23.7,21.4))
bootMOI.lower1<-matrix(ncol=3,nrow=4,c(0.36,14.6,3.64,19.72,0.02,23.62,27.3395,31.0595,
                                       0,8.2,15.9,12.4))
bootMOI.upper1<-matrix(ncol=3,nrow=4,c(3.58,26.8,10.48,37.8605,0.26,37.50,42.8005,51.9805,
                                       0,24.3205,32.2205,32.2005))

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
prevMOS.round.1<-matrix(ncol=3,nrow=4,c(0.4,0.7,0.48,0.52,0,0.9,0.68,0.6734694,
                                        0,0.5,0.72,0.58))
prevMOS.lower.1<-matrix(ncol=3,nrow=4,c(0.0,0.4,0.28,0.38,0,0.7,0.48,0.5416667,
                                        0,0.2,0.52,0.44))
prevMOS.upper.1<-matrix(ncol=3,nrow=4,c(0.8,1.0,0.68,0.66,0,1,0.84,0.8,
                                        0,0.8,0.88,0.72))

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
MOSI.round.1<-matrix(ncol=3,nrow=4,c(1,1.9,0.92,1.38,0,2.4,1.8,1.57,0,1.1,1.92,1.38))
bootMOSI.lower1<-matrix(ncol=3,nrow=4,c(0,1,0.52,0.98,0,1.6,1.24,1.2,0,0.4,1.32,1))
bootMOSI.upper1<-matrix(ncol=3,nrow=4,c(2.2,2.8,1.32,1.80,0,3.1,2.36,1.9375,0,1.9,2.48,1.78))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}
