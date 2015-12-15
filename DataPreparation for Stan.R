###***************Add the initial parasitemia in different mice

library(nlme)
library(lme4)
library(rstan)
library(MASS)
library(boot)
library(coda)
library(R2OpenBUGS)
library(ggplot2)
library("Rlab")
library(contrast)

##########################################################################
## 
##  ##         ##       ########      ##     
##  ###      ##  ##        ##       ##  ##   
##  ## #    ##    ##       ##      ##    ##   
##  ##  #   ########       ##      ########  
##  ##  #   ##    ##       ##     ##     ##  
##  ## #   ##      ##      ##    ##       ##
##  ###   ##        ##     ##   ##         ##
##  
##########################################################################
## 1. Preparation
##

##1.1 Controls complete sample

##1.1 Oocysts 
con<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mosquito_includingdec2015controls.txt",header=TRUE)
con$OocPrev<-ifelse(con$Oocyst==0,0,1)

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocystsC<-c(sample(con$Oocyst[con$Bites == 1 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 1 & con$Round == 2],45),
            sample(con$Oocyst[con$Bites == 1 & con$Round == 3],45),sample(con$Oocyst[con$Bites == 1 & con$Round == 4],45),
            sample(con$Oocyst[con$Bites == 2 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 2 & con$Round == 2],45),
            sample(con$Oocyst[con$Bites == 2 & con$Round == 3],45),sample(con$Oocyst[con$Bites == 2 & con$Round == 4],45),
            sample(con$Oocyst[con$Bites == 5 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 5 & con$Round == 2],45),
            sample(con$Oocyst[con$Bites == 5 & con$Round == 3],45),sample(con$Oocyst[con$Bites == 5 & con$Round == 4],45),
            sample(con$Oocyst[con$Bites == 10 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 10 & con$Round == 2],45),
            sample(con$Oocyst[con$Bites == 10 & con$Round == 3],45),sample(con$Oocyst[con$Bites == 10 & con$Round == 4],45))

length(oocystsC)
prevoocC<-ifelse(oocystsC==0,0,1)
prevooc1<-c(sum(sum(prevoocC[1:45])/45,sum(prevoocC[46:90])/45,sum(prevoocC[91:135])/45,sum(prevoocC[136:180])/45)/4,
            sum(sum(prevoocC[181:225])/45,sum(prevoocC[226:270])/45,sum(prevoocC[271:315])/45,sum(prevoocC[316:360])/45)/4,
            sum(sum(prevoocC[361:405])/45,sum(prevoocC[406:450])/45,sum(prevoocC[451:495])/45,sum(prevoocC[496:540])/45)/4,
            sum(sum(prevoocC[541:585])/45,sum(prevoocC[586:630])/45,sum(prevoocC[631:675])/45)/3)


meanoocystsC<-c(mean(con$Oocyst[con$Bites == 1 & con$Round == 1]),mean(con$Oocyst[con$Bites == 1 & con$Round == 2]),
            mean(con$Oocyst[con$Bites == 1 & con$Round == 3]),mean(con$Oocyst[con$Bites == 1 & con$Round == 4]),
            mean(con$Oocyst[con$Bites == 2 & con$Round == 1]),mean(con$Oocyst[con$Bites == 2 & con$Round == 2]),
            mean(con$Oocyst[con$Bites == 2 & con$Round == 3]),mean(con$Oocyst[con$Bites == 2 & con$Round == 4]),
            mean(con$Oocyst[con$Bites == 5 & con$Round == 1]),mean(con$Oocyst[con$Bites == 5 & con$Round == 2]),
            mean(con$Oocyst[con$Bites == 5 & con$Round == 3]),mean(con$Oocyst[con$Bites == 5 & con$Round == 4]),
            mean(con$Oocyst[con$Bites == 10 & con$Round == 1]),mean(con$Oocyst[con$Bites == 10 & con$Round == 2]),
            mean(con$Oocyst[con$Bites == 10 & con$Round == 3]))


##1.1 Sporozoites, parasitemia and Gametocytemia 
spors<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse_includingdec2015controls.txt",header=TRUE)
spors$Treatment<- ifelse(spors$Treatment == 0, "CONTROL","1")
spors2<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse_includingdec2015controls.txt",header=TRUE)##Or do mouse3AddingCombinations

##MEAN PARASITEMIA IN MICE
parasitC<-cbind(c(19.5,22.4,16.8,17.1,21.8),
  sample(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "1" & spors$Round == 1],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "1" & spors$Round == 2],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "1" & spors$Round == 3],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "1" & spors$Round == 4],5,replace=FALSE),
  c(19.5,22.4,16.8,17.1,21.8),
  sample(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "1" & spors$Round == 1],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "1" & spors$Round == 2],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "1" & spors$Round == 3],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "1" & spors$Round == 4],5,replace=FALSE),
  c(19.5,22.4,16.8,17.1,21.8),
  sample(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "1" & spors$Round == 1],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "1" & spors$Round == 2],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "1" & spors$Round == 3],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "1" & spors$Round == 4],5,replace=FALSE),
  c(19.5,22.4,16.8,17.1,21.8),
  sample(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "1" & spors$Round == 1],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "1" & spors$Round == 2],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "1" & spors$Round == 3],5,replace=FALSE),
  sample(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "1" & spors$Round == 4],5,replace=FALSE)
  )

parasitC_REAL<-cbind(
  c(spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 1],rep(0,20)),
  c(spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 2],rep(0,20)),
  c(spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 3],rep(0,20)),
  c(spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 4],rep(0,20)),
  spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 1],
  c(spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 2],rep(0,20)),
  c(spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 3],rep(0,20)),
  c(spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 4],rep(0,20)),
  spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 1],
  c(spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 2],rep(0,20)),
  c(spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 3],rep(0,20)),
  c(spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 4],rep(0,20)),
  c(spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 1],rep(0,7)),
  c(spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 2],rep(0,25)),
  c(spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 3],rep(0,25)),
  c(spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 3],rep(0,20)),)


newparaC_REAL<-t(parasitC_REAL)

plot(c(0,mean(spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL"]),
mean(spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL"]),
mean(spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL"]),
mean(spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL"]))~c(0,1,2,5,10),
  ylim=c(0,10),xlim=c(0,10),
  ylab="Parasitemia (%)",xlab="Number of Bites")
lines(c(0,mean(spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL"]),
       mean(spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL"]),
       mean(spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL"]),
       mean(spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL"]))~c(0,1,2,5,10))

parasitCvector<-c(
  spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 1],
  spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 2],
  spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 3],
  spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 1],
  spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 2],
  spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 3],
  spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 1],
  spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 2],
  spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 3],
  spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 1],
  spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 2],
  spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 3])
parasitCvectorNAMES<-c(rep("ConB1G1",10),rep("ConB1G2",10),rep("ConB1G3",10),
                       rep("ConB2G1",30),rep("ConB2G2",10),rep("ConB2G3",10),
                       rep("ConB5G1",30),rep("ConB5G2",10),rep("ConB5G3",10),
                       rep("ConB10G1",23),rep("ConB10G2",5),rep("ConB10G3",5))
parasitMean<-data.frame(parasitCvector,parasitCvectorNAMES)
parasitMean2<-subset(parasitMean,parasitCvectorNAMES!="ConB10G1" & parasitCvectorNAMES!="ConB10G2" & parasitCvectorNAMES!="ConB10G3")
te<-as.vector(tapply(parasitMean2$parasitCvector,parasitMean2$parasitCvectorNAMES,mean))
contPARAmean<-c(te[4:12])

parasitemC<-parasitemUPPC<-parasitemLOWC<-numeric(15)
for (i in 1:15){
  parasitemC[i]<-sum(parasitC[,i])/5
  
 # a<-numeric(10000)
#  for (j in 1:10000){ 
    
 #   a[j]<-mean(sample(parasitC[,i],4))
  #  parasitemUPPC[i]<-quantile(a,0.975)
   # parasitemLOWC[i]<-quantile(a,0.025)
  #}
}

##MEAN Gametocytemia IN MICE
gametC<-cbind(
  sample(spors2$Gametocytemia[spors2$Bites==1 & spors2$Round == 1],5),
  sample(spors2$Gametocytemia[spors2$Bites==1 & spors2$Round == 2],5),
  sample(spors2$Gametocytemia[spors2$Bites==1 & spors2$Round == 3],5),
  sample(spors2$Gametocytemia[spors2$Bites==1 & spors2$Round == 4],5),
  
  sample(spors2$Gametocytemia[spors2$Bites==2 & spors2$Round == 1],5),
  sample(spors2$Gametocytemia[spors2$Bites==2 & spors2$Round == 2],5),
  sample(spors2$Gametocytemia[spors2$Bites==2 & spors2$Round == 3],5),
  sample(spors2$Gametocytemia[spors2$Bites==2 & spors2$Round == 4],5),
  
  sample(spors2$Gametocytemia[spors2$Bites==5 & spors2$Round == 1],5),
  sample(spors2$Gametocytemia[spors2$Bites==5 & spors2$Round == 2],5),
  sample(spors2$Gametocytemia[spors2$Bites==5 & spors2$Round == 3],5),
  sample(spors2$Gametocytemia[spors2$Bites==5 & spors2$Round == 4],5),
  
  sample(spors2$Gametocytemia[spors2$Bites==10 & spors2$Round == 1],5),
  sample(spors2$Gametocytemia[spors2$Bites==10 & spors2$Round == 2],5),
  sample(spors2$Gametocytemia[spors2$Bites==10 & spors2$Round == 3],5))

gametCvector<-c(
  spors2$Gametocytemia[spors2$Bites==1 & spors2$Round == 1],
  spors2$Gametocytemia[spors2$Bites==1 & spors2$Round == 2],
  spors2$Gametocytemia[spors2$Bites==1 & spors2$Round == 3],
  #spors2$Gametocytemia[spors2$Bites==1 & spors2$Round == 4],
  spors2$Gametocytemia[spors2$Bites==2 & spors2$Round == 1],
  spors2$Gametocytemia[spors2$Bites==2 & spors2$Round == 2],
  spors2$Gametocytemia[spors2$Bites==2 & spors2$Round == 3],
  #spors2$Gametocytemia[spors2$Bites==2 & spors2$Round == 4],
  spors2$Gametocytemia[spors2$Bites==5 & spors2$Round == 1],
  spors2$Gametocytemia[spors2$Bites==5 & spors2$Round == 2],
  spors2$Gametocytemia[spors2$Bites==5 & spors2$Round == 3],
  #spors2$Gametocytemia[spors2$Bites==5 & spors2$Round == 4],
  spors2$Gametocytemia[spors2$Bites==10 & spors2$Round == 1],
  spors2$Gametocytemia[spors2$Bites==10 & spors2$Round == 2],
  spors2$Gametocytemia[spors2$Bites==10 & spors2$Round == 3])
gametCvectorNAMES<-c(rep("ConB1G1",10),rep("ConB1G2",10),rep("ConB1G3",10),
                       rep("ConB2G1",30),rep("ConB2G2",10),rep("ConB2G3",10),
                       rep("ConB5G1",30),rep("ConB5G2",10),rep("ConB5G3",10),
                       rep("ConB10G1",23),rep("ConB10G2",5),rep("ConB10G3",5))
gametMean<-data.frame(gametCvector,gametCvectorNAMES)
gametMean2<-subset(gametMean,gametCvectorNAMES!="ConB10G1" & gametCvectorNAMES!="ConB10G2" & gametCvectorNAMES!="ConB10G3")
te<-as.vector(tapply(gametMean2$gametCvector,gametMean2$gametCvectorNAMES,mean))
contGAMETmean<-c(te[4:12])

gametoC<-gametoUPPC<-gametoLOWC<-numeric(15)
for (i in 1:15){
  gametoC[i]<-sum(gametC[,i])/5
  #a<-numeric(10000)
  #for (j in 1:10000){ 
    
  #  a[j]<-mean(sample(gametC[,i],4))
  #  gametoUPPC[i]<-quantile(a,0.975)
  #  gametoLOWC[i]<-quantile(a,0.025)
  #}
}

##PREVALENCE IN MICE
spors2$prevBS<-ifelse(spors2$Parasitemia > 0 | spors2$Gametocytemia > 0, 1, 0)
spcount<-expand.grid(seq(1,218,1))
for(i in 1:218){
for (j in 7:16){
spcount[i,j-6]<-ifelse(is.na(spors2[i,j])==FALSE,1,0)}}
for (i in 1:length(spors2$prevBS)){
  spors2$sporoCount[i]<-sum(spcount[i,])}
for (i in 1:length(spors2$prevBS)){
spors2$meanpermouse[i]<-sum(spors2[i,7:16],na.rm=T)/spors2$sporoCount[i]
}

PREV_C<-cbind(
  sample(spors2$prevBS[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 1],5),
  sample(spors2$prevBS[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 2],5),
  sample(spors2$prevBS[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 3],5),
  sample(spors2$prevBS[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 4],5),
  sample(spors2$prevBS[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 1],5),
  sample(spors2$prevBS[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 2],5),
  sample(spors2$prevBS[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 3],5),
  sample(spors2$prevBS[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 4],5),
  sample(spors2$prevBS[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 1],5),
  sample(spors2$prevBS[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 2],5),
  sample(spors2$prevBS[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 3],5),
  sample(spors2$prevBS[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 4],5),
  sample(spors2$prevBS[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 1],5),
  sample(spors2$prevBS[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 2],5),
  sample(spors2$prevBS[spors2$Bites==10 & spors2$Treatment == "CONTROL" & spors2$Round == 3],5))

PREV_Cmean<-c(
  mean(spors2$prevBS[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 1]),
  mean(spors2$prevBS[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 2]),
  mean(spors2$prevBS[spors2$Bites==1 & spors2$Treatment == "CONTROL" & spors2$Round == 3]),
  mean(spors2$prevBS[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 1]),
  mean(spors2$prevBS[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 2]),
  mean(spors2$prevBS[spors2$Bites==2 & spors2$Treatment == "CONTROL" & spors2$Round == 3]),
  mean(spors2$prevBS[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 1]),
  mean(spors2$prevBS[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 2]),
  mean(spors2$prevBS[spors2$Bites==5 & spors2$Treatment == "CONTROL" & spors2$Round == 3]))

###Sporozoite intensity
Sporozoite1a<-spors2$Sporozoite1[spors2$Round==1 & spors2$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(spors2$Sporozoite1[spors2$Round==1 & spors2$Bites==2],spors2$Sporozoite2[spors2$Round==1 & spors2$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(spors2$Sporozoite1[spors2$Round==1 & spors2$Bites==5],spors2$Sporozoite2[spors2$Round==1 & spors2$Bites==5],
                spors2$Sporozoite3[spors2$Round==1 & spors2$Bites==5],spors2$Sporozoite4[spors2$Round==1 & spors2$Bites==5],
                spors2$Sporozoite5[spors2$Round==1 & spors2$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(spors2$Sporozoite1[spors2$Round==1 & spors2$Bites==10],spors2$Sporozoite2[spors2$Round==1 & spors2$Bites==10],
                 spors2$Sporozoite3[spors2$Round==1 & spors2$Bites==10],spors2$Sporozoite4[spors2$Round==1 & spors2$Bites==10],
                 spors2$Sporozoite5[spors2$Round==1 & spors2$Bites==10],spors2$Sporozoite6[spors2$Round==1 & spors2$Bites==10],
                 spors2$Sporozoite7[spors2$Round==1 & spors2$Bites==10],spors2$Sporozoite8[spors2$Round==1 & spors2$Bites==10],
                 spors2$Sporozoite9[spors2$Round==1 & spors2$Bites==10],spors2$Sporozoite10[spors2$Round==1 & spors2$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
          length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
          length(Sporozoite10a[Sporozoite10a==4]))


Sporozoite1b<-spors2$Sporozoite1[spors2$Round==2 & spors2$Bites==1]
spb1r2<-c(length(Sporozoite1b[Sporozoite1b==0]),length(Sporozoite1b[Sporozoite1b==1]),
          length(Sporozoite1b[Sporozoite1b==2]),length(Sporozoite1b[Sporozoite1b==3]),
          length(Sporozoite1b[Sporozoite1b==4]))

Sporozoite2b<-c(spors2$Sporozoite1[spors2$Round==2 & spors2$Bites==2],spors2$Sporozoite2[spors2$Round==2 & spors2$Bites==2])
spb2r2<-c(length(Sporozoite2b[Sporozoite2b==0]),length(Sporozoite2b[Sporozoite2b==1]),
          length(Sporozoite2b[Sporozoite2b==2]),length(Sporozoite2b[Sporozoite2b==3]),
          length(Sporozoite2b[Sporozoite2b==4]))

Sporozoite5b<-c(spors2$Sporozoite1[spors2$Round==2 & spors2$Bites==5],spors2$Sporozoite2[spors2$Round==2 & spors2$Bites==5],
                spors2$Sporozoite3[spors2$Round==2 & spors2$Bites==5],spors2$Sporozoite4[spors2$Round==2 & spors2$Bites==5],
                spors2$Sporozoite5[spors2$Round==2 & spors2$Bites==5])
spb5r2<-c(length(Sporozoite5b[Sporozoite5b==0]),length(Sporozoite5b[Sporozoite5b==1]),
          length(Sporozoite5b[Sporozoite5b==2]),length(Sporozoite5b[Sporozoite5b==3]),
          length(Sporozoite5b[Sporozoite5b==4]))

Sporozoite10b<-c(spors2$Sporozoite1[spors2$Round==2 & spors2$Bites==10],spors2$Sporozoite2[spors2$Round==2 & spors2$Bites==10],
                 spors2$Sporozoite3[spors2$Round==2 & spors2$Bites==10],spors2$Sporozoite4[spors2$Round==2 & spors2$Bites==10],
                 spors2$Sporozoite5[spors2$Round==2 & spors2$Bites==10],spors2$Sporozoite6[spors2$Round==2 & spors2$Bites==10],
                 spors2$Sporozoite7[spors2$Round==2 & spors2$Bites==10],spors2$Sporozoite8[spors2$Round==2 & spors2$Bites==10],
                 spors2$Sporozoite9[spors2$Round==2 & spors2$Bites==10],spors2$Sporozoite10[spors2$Round==2 & spors2$Bites==10])
spb10r2<-c(length(Sporozoite10b[Sporozoite10b==0]),length(Sporozoite10b[Sporozoite10b==1]),
           length(Sporozoite10b[Sporozoite10b==2]),length(Sporozoite10b[Sporozoite10b==3]),
           length(Sporozoite10b[Sporozoite10b==4]))


Sporozoite1c<-spors2$Sporozoite1[spors2$Round==3 & spors2$Bites==1]
spb1r3<-c(length(Sporozoite1c[Sporozoite1c==0]),length(Sporozoite1c[Sporozoite1c==1]),
          length(Sporozoite1c[Sporozoite1c==2]),length(Sporozoite1c[Sporozoite1c==3]),
          length(Sporozoite1c[Sporozoite1c==4]))

Sporozoite2c<-c(spors2$Sporozoite1[spors2$Round==3 & spors2$Bites==2],spors2$Sporozoite2[spors2$Round==3 & spors2$Bites==2])
spb2r3<-c(length(Sporozoite2c[Sporozoite2c==0]),length(Sporozoite2c[Sporozoite2c==1]),
          length(Sporozoite2c[Sporozoite2c==2]),length(Sporozoite2c[Sporozoite2c==3]),
          length(Sporozoite2c[Sporozoite2c==4]))

Sporozoite5c<-c(spors2$Sporozoite1[spors2$Round==3 & spors2$Bites==5],spors2$Sporozoite2[spors2$Round==3 & spors2$Bites==5],
                spors2$Sporozoite3[spors2$Round==3 & spors2$Bites==5],spors2$Sporozoite4[spors2$Round==3 & spors2$Bites==5],
                spors2$Sporozoite5[spors2$Round==3 & spors2$Bites==5])
spb5r3<-c(length(Sporozoite5c[Sporozoite5c==0]),length(Sporozoite5c[Sporozoite5c==1]),
          length(Sporozoite5c[Sporozoite5c==2]),length(Sporozoite5c[Sporozoite5c==3]),
          length(Sporozoite5c[Sporozoite5c==4]))

Sporozoite10c<-c(spors2$Sporozoite1[spors2$Round==3 & spors2$Bites==10],spors2$Sporozoite2[spors2$Round==3 & spors2$Bites==10],
                 spors2$Sporozoite3[spors2$Round==3 & spors2$Bites==10],spors2$Sporozoite4[spors2$Round==3 & spors2$Bites==10],
                 spors2$Sporozoite5[spors2$Round==3 & spors2$Bites==10],spors2$Sporozoite6[spors2$Round==3 & spors2$Bites==10],
                 spors2$Sporozoite7[spors2$Round==3 & spors2$Bites==10],spors2$Sporozoite8[spors2$Round==3 & spors2$Bites==10],
                 spors2$Sporozoite9[spors2$Round==3 & spors2$Bites==10],spors2$Sporozoite10[spors2$Round==3 & spors2$Bites==10])
spb10r3<-c(length(Sporozoite10c[Sporozoite10c==0]),length(Sporozoite10c[Sporozoite10c==1]),
           length(Sporozoite10c[Sporozoite10c==2]),length(Sporozoite10c[Sporozoite10c==3]),
           length(Sporozoite10c[Sporozoite10c==4]))


Sporozoite1d<-spors2$Sporozoite1[spors2$Round==4 & spors2$Bites==1]
spb1r4<-c(length(Sporozoite1d[Sporozoite1d==0]),length(Sporozoite1d[Sporozoite1d==1]),
          length(Sporozoite1d[Sporozoite1d==2]),length(Sporozoite1d[Sporozoite1d==3]),
          length(Sporozoite1d[Sporozoite1d==4]))

Sporozoite2d<-c(spors2$Sporozoite1[spors2$Round==4 & spors2$Bites==2],spors2$Sporozoite2[spors2$Round==4 & spors2$Bites==2])
spb2r4<-c(length(Sporozoite2d[Sporozoite2d==0]),length(Sporozoite2d[Sporozoite2d==1]),
          length(Sporozoite2d[Sporozoite2d==2]),length(Sporozoite2d[Sporozoite2d==3]),
          length(Sporozoite2d[Sporozoite2d==4]))

Sporozoite5d<-c(spors2$Sporozoite1[spors2$Round==4 & spors2$Bites==5],spors2$Sporozoite2[spors2$Round==4 & spors2$Bites==5],
                spors2$Sporozoite3[spors2$Round==4 & spors2$Bites==5],spors2$Sporozoite4[spors2$Round==4 & spors2$Bites==5],
                spors2$Sporozoite5[spors2$Round==4 & spors2$Bites==5])
spb5r4<-c(length(Sporozoite5d[Sporozoite5d==0]),length(Sporozoite5d[Sporozoite5d==1]),
          length(Sporozoite5d[Sporozoite5d==2]),length(Sporozoite5d[Sporozoite5d==3]),
          length(Sporozoite5d[Sporozoite5d==4]))

Sporozoite10d<-c(spors2$Sporozoite1[spors2$Round==4 & spors2$Bites==10],spors2$Sporozoite2[spors2$Round==4 & spors2$Bites==10],
                spors2$Sporozoite3[spors2$Round==4 & spors2$Bites==10],spors2$Sporozoite4[spors2$Round==4 & spors2$Bites==10],
                spors2$Sporozoite5[spors2$Round==4 & spors2$Bites==10],spors2$Sporozoite6[spors2$Round==4 & spors2$Bites==10],
                spors2$Sporozoite7[spors2$Round==4 & spors2$Bites==10],spors2$Sporozoite8[spors2$Round==4 & spors2$Bites==10],
                spors2$Sporozoite9[spors2$Round==4 & spors2$Bites==10],spors2$Sporozoite10[spors2$Round==4 & spors2$Bites==10])
spb10r4<-c(length(Sporozoite10d[Sporozoite10d==0]),length(Sporozoite10d[Sporozoite10d==1]),
          length(Sporozoite10d[Sporozoite10d==2]),length(Sporozoite10d[Sporozoite10d==3]),
          length(Sporozoite10d[Sporozoite10d==4]))

spors2_C<-rbind(spb1r1,spb1r2,spb1r3,spb1r4,
               spb2r1,spb2r2,spb2r3,spb2r4,
               spb5r1,spb5r2,spb5r3,spb5r4,
               spb10r1,spb10r2,spb10r3,spb10r4)

MEANsp<-c(
  mean(Sporozoite1a,na.rm=T),mean(Sporozoite1b,na.rm=T),mean(Sporozoite1c,na.rm=T),
  mean(Sporozoite2a,na.rm=T),mean(Sporozoite2b,na.rm=T),mean(Sporozoite2c,na.rm=T),
  mean(Sporozoite5a,na.rm=T),mean(Sporozoite5b,na.rm=T),mean(Sporozoite5c,na.rm=T),
  mean(Sporozoite10a,na.rm=T),mean(Sporozoite10b,na.rm=T),mean(Sporozoite10c,na.rm=T))

MEANsp2<-c(
  mean(c(Sporozoite1a,Sporozoite1b,Sporozoite1c),na.rm=T),
  mean(c(Sporozoite2a,Sporozoite2b,Sporozoite2c),na.rm=T),
  mean(c(Sporozoite5a,Sporozoite5b,Sporozoite5c),na.rm=T),
  mean(c(Sporozoite10a,Sporozoite10b,Sporozoite10c),na.rm=T))

plot(c(0,mean(spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL"]),
       mean(spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL"]),
       mean(spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL"]),
       mean(spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL"]))~c(0,MEANsp2),
     ylab="Parasitemia (%)", xlab="Sporozoite Score",ylim=c(0,15),xlim=c(0,3))
abline(lm(c(0,mean(spors2$Parasitemia[spors2$Bites==1 & spors2$Treatment == "CONTROL"]),
   mean(spors2$Parasitemia[spors2$Bites==2 & spors2$Treatment == "CONTROL"]),
   mean(spors2$Parasitemia[spors2$Bites==5 & spors2$Treatment == "CONTROL"]),
   mean(spors2$Parasitemia[spors2$Bites==10 & spors2$Treatment == "CONTROL"]))~c(0,MEANsp2)+0),lty=1)

##################################
## 1.2 ATV-25%

##1.2 Oocysts 
ATV25<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mosquito.txt",header=TRUE)
ATV25$OocPrev<-ifelse(ATV25$Oocyst==0,0,1)

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_ATV25<-c(sample(ATV25$Oocyst[ATV25$Bites == 0 & ATV25$Round == 0],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 1],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 2],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 3],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 0 & ATV25$Round == 0],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 1],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 2],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 3],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 0 & ATV25$Round == 0],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 1],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 2],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 3],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 0 & ATV25$Round == 0],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 10 & ATV25$Round == 1],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 10 & ATV25$Round == 2],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 10 & ATV25$Round == 3],45))

length(oocysts_ATV25)
prevooc<-ifelse(oocysts_ATV25==0,0,1)
prevoocATV25<-ifelse(oocysts_ATV25==0,0,1)
prevooc1ATV<-c(sum(sum(prevooc[1:45])/45,sum(prevooc[46:90])/45,sum(prevooc[91:135])/45)/3,
            sum(sum(prevooc[136:180])/45,sum(prevooc[181:225])/45,sum(prevooc[226:270])/45)/3,
            sum(sum(prevooc[271:315])/45,sum(prevooc[316:360])/45,sum(prevooc[361:405])/45)/3,
            sum(sum(prevooc[406:450])/45,sum(prevooc[451:495])/45,sum(prevooc[496:540])/45)/3)


meanoocysts_ATV25<-c(mean(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 1]),mean(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 2]),
                mean(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 3]),
                mean(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 1]),mean(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 2]),
                mean(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 3]),
                mean(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 1]),mean(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 2]),
                mean(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 3]),
                mean(ATV25$Oocyst[ATV25$Bites == 10 & ATV25$Round == 1]),mean(ATV25$Oocyst[ATV25$Bites == 10 & ATV25$Round == 2]),
                mean(ATV25$Oocyst[ATV25$Bites == 10 & ATV25$Round == 3]))


##1.2 Sporozoites, parasitemia and Gametocytemia 
sporsATV25<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mouse.txt",header=TRUE)

##MEAN PARASITEMIA IN MICE
parasitATV25<-cbind(
  sporsATV25$Parasitemia[sporsATV25$Bites==1 & sporsATV25$Round == 0],
  sporsATV25$Parasitemia[sporsATV25$Bites==1 & sporsATV25$Round == 1],
  sporsATV25$Parasitemia[sporsATV25$Bites==1 & sporsATV25$Round == 2],
  sporsATV25$Parasitemia[sporsATV25$Bites==1 & sporsATV25$Round == 3],
  sporsATV25$Parasitemia[sporsATV25$Bites==1 & sporsATV25$Round == 4],
  sporsATV25$Parasitemia[sporsATV25$Bites==2 & sporsATV25$Round == 0],
  sporsATV25$Parasitemia[sporsATV25$Bites==2 & sporsATV25$Round == 1],
  sporsATV25$Parasitemia[sporsATV25$Bites==2 & sporsATV25$Round == 2],
  sporsATV25$Parasitemia[sporsATV25$Bites==2 & sporsATV25$Round == 3],
  sporsATV25$Parasitemia[sporsATV25$Bites==2 & sporsATV25$Round == 4],
  sporsATV25$Parasitemia[sporsATV25$Bites==5 & sporsATV25$Round == 0],
  sporsATV25$Parasitemia[sporsATV25$Bites==5 & sporsATV25$Round == 1],
  sporsATV25$Parasitemia[sporsATV25$Bites==5 & sporsATV25$Round == 2],
  sporsATV25$Parasitemia[sporsATV25$Bites==5 & sporsATV25$Round == 3],
  sporsATV25$Parasitemia[sporsATV25$Bites==5 & sporsATV25$Round == 4],
  sporsATV25$Parasitemia[sporsATV25$Bites==10 & sporsATV25$Round == 0],
  sporsATV25$Parasitemia[sporsATV25$Bites==10 & sporsATV25$Round == 1],
  sporsATV25$Parasitemia[sporsATV25$Bites==10 & sporsATV25$Round == 2],
  sporsATV25$Parasitemia[sporsATV25$Bites==10 & sporsATV25$Round == 3],
  sporsATV25$Parasitemia[sporsATV25$Bites==10 & sporsATV25$Round == 4])
newparasitATV25<-t(parasitATV25)

lines(c(mean(sporsATV25$Parasitemia[sporsATV25$Bites==1]),
        mean(sporsATV25$Parasitemia[sporsATV25$Bites==2]),
        mean(sporsATV25$Parasitemia[sporsATV25$Bites==5]),
        mean(sporsATV25$Parasitemia[sporsATV25$Bites==10]))~c(1,2,5,10),lty=2)
points(c(mean(sporsATV25$Parasitemia[sporsATV25$Bites==1]),
        mean(sporsATV25$Parasitemia[sporsATV25$Bites==2]),
        mean(sporsATV25$Parasitemia[sporsATV25$Bites==5]),
        mean(sporsATV25$Parasitemia[sporsATV25$Bites==10]))~c(1,2,5,10),lty=2)
parasitATV25vector<-c(
  sporsATV25$Parasitemia[sporsATV25$Bites==1 & sporsATV25$Round == 1],
  sporsATV25$Parasitemia[sporsATV25$Bites==1 & sporsATV25$Round == 2],
  sporsATV25$Parasitemia[sporsATV25$Bites==1 & sporsATV25$Round == 3],
  sporsATV25$Parasitemia[sporsATV25$Bites==2 & sporsATV25$Round == 1],
  sporsATV25$Parasitemia[sporsATV25$Bites==2 & sporsATV25$Round == 2],
  sporsATV25$Parasitemia[sporsATV25$Bites==2 & sporsATV25$Round == 3],
  sporsATV25$Parasitemia[sporsATV25$Bites==5 & sporsATV25$Round == 1],
  sporsATV25$Parasitemia[sporsATV25$Bites==5 & sporsATV25$Round == 2],
  sporsATV25$Parasitemia[sporsATV25$Bites==5 & sporsATV25$Round == 3],
  sporsATV25$Parasitemia[sporsATV25$Bites==10 & sporsATV25$Round == 1],
  sporsATV25$Parasitemia[sporsATV25$Bites==10 & sporsATV25$Round == 2],
  sporsATV25$Parasitemia[sporsATV25$Bites==10 & sporsATV25$Round == 3])
parasitATV25vectorNAMES<-c(rep("ConB1G1",5),rep("ConB1G2",5),rep("ConB1G3",5),
                       rep("ConB2G1",5),rep("ConB2G2",5),rep("ConB2G3",5),
                       rep("ConB5G1",5),rep("ConB5G2",5),rep("ConB5G3",5),
                       rep("ConB10G1",5),rep("ConB10G2",5),rep("ConB10G3",5))
parasitMeanATV25<-data.frame(parasitATV25vector,parasitATV25vectorNAMES)
te<-as.vector(tapply(parasitMeanATV25$parasitATV25vector,parasitMeanATV25$parasitATV25vectorNAMES,mean))
ATV25PARAmean<-c(te[4:12],te[1:3])


parasitemATV25<-parasitemUPPATV25<-parasitemLOWATV25<-numeric(12)
for (i in 1:12){
  parasitemATV25[i]<-sum(parasitATV25[,i])/5
  
  # a<-numeric(10000)
  #  for (j in 1:10000){ 
  
  #   a[j]<-mean(sample(parasitATV25[,i],4))
  #  parasitemUPPATV25[i]<-quantile(a,0.975)
  # parasitemLOWATV25[i]<-quantile(a,0.025)
  #}
}

##MEAN Gametocytemia IN MICE
gametATV25<-cbind(
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==1 & sporsATV25$Round == 1],5),
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==1 & sporsATV25$Round == 2],5),
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==1 & sporsATV25$Round == 3],5),
  
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==2 & sporsATV25$Round == 1],5),
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==2 & sporsATV25$Round == 2],5),
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==2 & sporsATV25$Round == 3],5),
  
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==5 & sporsATV25$Round == 1],5),
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==5 & sporsATV25$Round == 2],5),
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==5 & sporsATV25$Round == 3],5),
  
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==10 & sporsATV25$Round == 1],5),
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==10 & sporsATV25$Round == 2],5),
  sample(sporsATV25$Gametocytemia[sporsATV25$Bites==10 & sporsATV25$Round == 3],5))

gametATV25vector<-c(
  sporsATV25$Gametocytemia[sporsATV25$Bites==1 & sporsATV25$Round == 1],
  sporsATV25$Gametocytemia[sporsATV25$Bites==1 & sporsATV25$Round == 2],
  sporsATV25$Gametocytemia[sporsATV25$Bites==1 & sporsATV25$Round == 3],
  sporsATV25$Gametocytemia[sporsATV25$Bites==2 & sporsATV25$Round == 1],
  sporsATV25$Gametocytemia[sporsATV25$Bites==2 & sporsATV25$Round == 2],
  sporsATV25$Gametocytemia[sporsATV25$Bites==2 & sporsATV25$Round == 3],
  sporsATV25$Gametocytemia[sporsATV25$Bites==5 & sporsATV25$Round == 1],
  sporsATV25$Gametocytemia[sporsATV25$Bites==5 & sporsATV25$Round == 2],
  sporsATV25$Gametocytemia[sporsATV25$Bites==5 & sporsATV25$Round == 3],
  sporsATV25$Gametocytemia[sporsATV25$Bites==10 & sporsATV25$Round == 1],
  sporsATV25$Gametocytemia[sporsATV25$Bites==10 & sporsATV25$Round == 2],
  sporsATV25$Gametocytemia[sporsATV25$Bites==10 & sporsATV25$Round == 3])
gamATV25vectorNAMES<-c(rep("ConB1G1",5),rep("ConB1G2",5),rep("ConB1G3",5),
                           rep("ConB2G1",5),rep("ConB2G2",5),rep("ConB2G3",5),
                           rep("ConB5G1",5),rep("ConB5G2",5),rep("ConB5G3",5),
                           rep("ConB10G1",5),rep("ConB10G2",5),rep("ConB10G3",5))
gamMeanATV25<-data.frame(gametATV25vector,gamATV25vectorNAMES)
te<-as.vector(tapply(gamMeanATV25$gametATV25vector,gamMeanATV25$gamATV25vectorNAMES,mean))
ATV25GAMmean<-c(te[4:12],te[1:3])

gametoATV25<-gametoUPPATV25<-gametoLOWATV25<-numeric(12)
for (i in 1:12){
  gametoATV25[i]<-sum(gametATV25[,i])/5
  #a<-numeric(10000)
  #for (j in 1:10000){ 
  
  #  a[j]<-mean(sample(gametATV25[,i],4))
  #  gametoUPPATV25[i]<-quantile(a,0.975)
  #  gametoLOWATV25[i]<-quantile(a,0.025)
  #}
}

##PREVALENCE IN MICE
sporsATV25$prevBS<-ifelse(sporsATV25$Parasitemia > 0 | sporsATV25$Gametocytemia > 0, 1, 0)
spcount<-expand.grid(seq(1,nrow(sporsATV25),1))
for(i in 1:nrow(sporsATV25)){
  for (j in 6:15){
    spcount[i,j-5]<-ifelse(is.na(sporsATV25[i,j])==FALSE,1,0)}}
for (i in 1:length(sporsATV25$prevBS)){
  sporsATV25$sporoCount[i]<-sum(spcount[i,])}
for (i in 1:nrow(sporsATV25)){
  sporsATV25$meanpermouse[i]<-sum(sporsATV25[i,6:15],na.rm=T)/sporsATV25$sporoCount[i]
}

PREV_ATV25<-cbind(
  sample(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 1],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 2],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 3],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 1],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 2],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 3],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 1],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 2],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 3],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==10 & sporsATV25$Round == 1],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==10 & sporsATV25$Round == 2],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==10 & sporsATV25$Round == 3],5))

PREV_ATV25Mean<-c(
  mean(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 1]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 2]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 3]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 1]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 2]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 3]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 1]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 2]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 3]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==10 & sporsATV25$Round == 1]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==10 & sporsATV25$Round == 2]),
  mean(sporsATV25$prevBS[sporsATV25$Bites==10 & sporsATV25$Round == 3]))

###Sporozoite intensity
Sporozoite1z<-sporsATV25$Sporozoite1[sporsATV25$Round==0 & sporsATV25$Bites==1]
spb1r0<-c(length(Sporozoite1z[Sporozoite1z==0]),length(Sporozoite1z[Sporozoite1z==1]),
          length(Sporozoite1z[Sporozoite1z==2]),length(Sporozoite1z[Sporozoite1z==3]),
          length(Sporozoite1z[Sporozoite1z==4]))

Sporozoite2z<-c(sporsATV25$Sporozoite1[sporsATV25$Round==0 & sporsATV25$Bites==2],sporsATV25$Sporozoite2[sporsATV25$Round==0 & sporsATV25$Bites==2])
spb2r0<-c(length(Sporozoite2z[Sporozoite2z==0]),length(Sporozoite2z[Sporozoite2z==1]),
          length(Sporozoite2z[Sporozoite2z==2]),length(Sporozoite2z[Sporozoite2z==3]),
          length(Sporozoite2z[Sporozoite2z==4]))

Sporozoite5z<-c(sporsATV25$Sporozoite1[sporsATV25$Round==0 & sporsATV25$Bites==5],sporsATV25$Sporozoite2[sporsATV25$Round==0 & sporsATV25$Bites==5],
                sporsATV25$Sporozoite3[sporsATV25$Round==0 & sporsATV25$Bites==5],sporsATV25$Sporozoite4[sporsATV25$Round==0 & sporsATV25$Bites==5],
                sporsATV25$Sporozoite5[sporsATV25$Round==0 & sporsATV25$Bites==5])
spb5r0<-c(length(Sporozoite5z[Sporozoite5z==0]),length(Sporozoite5z[Sporozoite5z==1]),
          length(Sporozoite5z[Sporozoite5z==2]),length(Sporozoite5z[Sporozoite5z==3]),
          length(Sporozoite5z[Sporozoite5z==4]))

Sporozoite10z<-c(sporsATV25$Sporozoite1[sporsATV25$Round==0 & sporsATV25$Bites==10],sporsATV25$Sporozoite2[sporsATV25$Round==0 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite3[sporsATV25$Round==0 & sporsATV25$Bites==10],sporsATV25$Sporozoite4[sporsATV25$Round==0 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite5[sporsATV25$Round==0 & sporsATV25$Bites==10],sporsATV25$Sporozoite6[sporsATV25$Round==0 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite7[sporsATV25$Round==0 & sporsATV25$Bites==10],sporsATV25$Sporozoite8[sporsATV25$Round==0 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite9[sporsATV25$Round==0 & sporsATV25$Bites==10],sporsATV25$Sporozoite10[sporsATV25$Round==0 & sporsATV25$Bites==10])
spb10r0<-c(length(Sporozoite10z[Sporozoite10z==0]),length(Sporozoite10z[Sporozoite10z==1]),
           length(Sporozoite10z[Sporozoite10z==2]),length(Sporozoite10z[Sporozoite10z==3]),
           length(Sporozoite10z[Sporozoite10z==4]))
##
Sporozoite1a<-sporsATV25$Sporozoite1[sporsATV25$Round==1 & sporsATV25$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(sporsATV25$Sporozoite1[sporsATV25$Round==1 & sporsATV25$Bites==2],sporsATV25$Sporozoite2[sporsATV25$Round==1 & sporsATV25$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(sporsATV25$Sporozoite1[sporsATV25$Round==1 & sporsATV25$Bites==5],sporsATV25$Sporozoite2[sporsATV25$Round==1 & sporsATV25$Bites==5],
                sporsATV25$Sporozoite3[sporsATV25$Round==1 & sporsATV25$Bites==5],sporsATV25$Sporozoite4[sporsATV25$Round==1 & sporsATV25$Bites==5],
                sporsATV25$Sporozoite5[sporsATV25$Round==1 & sporsATV25$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(sporsATV25$Sporozoite1[sporsATV25$Round==1 & sporsATV25$Bites==10],sporsATV25$Sporozoite2[sporsATV25$Round==1 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite3[sporsATV25$Round==1 & sporsATV25$Bites==10],sporsATV25$Sporozoite4[sporsATV25$Round==1 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite5[sporsATV25$Round==1 & sporsATV25$Bites==10],sporsATV25$Sporozoite6[sporsATV25$Round==1 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite7[sporsATV25$Round==1 & sporsATV25$Bites==10],sporsATV25$Sporozoite8[sporsATV25$Round==1 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite9[sporsATV25$Round==1 & sporsATV25$Bites==10],sporsATV25$Sporozoite10[sporsATV25$Round==1 & sporsATV25$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
           length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
           length(Sporozoite10a[Sporozoite10a==4]))


Sporozoite1b<-sporsATV25$Sporozoite1[sporsATV25$Round==2 & sporsATV25$Bites==1]
spb1r2<-c(length(Sporozoite1b[Sporozoite1b==0]),length(Sporozoite1b[Sporozoite1b==1]),
          length(Sporozoite1b[Sporozoite1b==2]),length(Sporozoite1b[Sporozoite1b==3]),
          length(Sporozoite1b[Sporozoite1b==4]))

Sporozoite2b<-c(sporsATV25$Sporozoite1[sporsATV25$Round==2 & sporsATV25$Bites==2],sporsATV25$Sporozoite2[sporsATV25$Round==2 & sporsATV25$Bites==2])
spb2r2<-c(length(Sporozoite2b[Sporozoite2b==0]),length(Sporozoite2b[Sporozoite2b==1]),
          length(Sporozoite2b[Sporozoite2b==2]),length(Sporozoite2b[Sporozoite2b==3]),
          length(Sporozoite2b[Sporozoite2b==4]))

Sporozoite5b<-c(sporsATV25$Sporozoite1[sporsATV25$Round==2 & sporsATV25$Bites==5],sporsATV25$Sporozoite2[sporsATV25$Round==2 & sporsATV25$Bites==5],
                sporsATV25$Sporozoite3[sporsATV25$Round==2 & sporsATV25$Bites==5],sporsATV25$Sporozoite4[sporsATV25$Round==2 & sporsATV25$Bites==5],
                sporsATV25$Sporozoite5[sporsATV25$Round==2 & sporsATV25$Bites==5])
spb5r2<-c(length(Sporozoite5b[Sporozoite5b==0]),length(Sporozoite5b[Sporozoite5b==1]),
          length(Sporozoite5b[Sporozoite5b==2]),length(Sporozoite5b[Sporozoite5b==3]),
          length(Sporozoite5b[Sporozoite5b==4]))

Sporozoite10b<-c(sporsATV25$Sporozoite1[sporsATV25$Round==2 & sporsATV25$Bites==10],sporsATV25$Sporozoite2[sporsATV25$Round==2 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite3[sporsATV25$Round==2 & sporsATV25$Bites==10],sporsATV25$Sporozoite4[sporsATV25$Round==2 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite5[sporsATV25$Round==2 & sporsATV25$Bites==10],sporsATV25$Sporozoite6[sporsATV25$Round==2 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite7[sporsATV25$Round==2 & sporsATV25$Bites==10],sporsATV25$Sporozoite8[sporsATV25$Round==2 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite9[sporsATV25$Round==2 & sporsATV25$Bites==10],sporsATV25$Sporozoite10[sporsATV25$Round==2 & sporsATV25$Bites==10])
spb10r2<-c(length(Sporozoite10b[Sporozoite10b==0]),length(Sporozoite10b[Sporozoite10b==1]),
           length(Sporozoite10b[Sporozoite10b==2]),length(Sporozoite10b[Sporozoite10b==3]),
           length(Sporozoite10b[Sporozoite10b==4]))


Sporozoite1c<-sporsATV25$Sporozoite1[sporsATV25$Round==3 & sporsATV25$Bites==1]
spb1r3<-c(length(Sporozoite1c[Sporozoite1c==0]),length(Sporozoite1c[Sporozoite1c==1]),
          length(Sporozoite1c[Sporozoite1c==2]),length(Sporozoite1c[Sporozoite1c==3]),
          length(Sporozoite1c[Sporozoite1c==4]))

Sporozoite2c<-c(sporsATV25$Sporozoite1[sporsATV25$Round==3 & sporsATV25$Bites==2],sporsATV25$Sporozoite2[sporsATV25$Round==3 & sporsATV25$Bites==2])
spb2r3<-c(length(Sporozoite2c[Sporozoite2c==0]),length(Sporozoite2c[Sporozoite2c==1]),
          length(Sporozoite2c[Sporozoite2c==2]),length(Sporozoite2c[Sporozoite2c==3]),
          length(Sporozoite2c[Sporozoite2c==4]))

Sporozoite5c<-c(sporsATV25$Sporozoite1[sporsATV25$Round==3 & sporsATV25$Bites==5],sporsATV25$Sporozoite2[sporsATV25$Round==3 & sporsATV25$Bites==5],
                sporsATV25$Sporozoite3[sporsATV25$Round==3 & sporsATV25$Bites==5],sporsATV25$Sporozoite4[sporsATV25$Round==3 & sporsATV25$Bites==5],
                sporsATV25$Sporozoite5[sporsATV25$Round==3 & sporsATV25$Bites==5])
spb5r3<-c(length(Sporozoite5c[Sporozoite5c==0]),length(Sporozoite5c[Sporozoite5c==1]),
          length(Sporozoite5c[Sporozoite5c==2]),length(Sporozoite5c[Sporozoite5c==3]),
          length(Sporozoite5c[Sporozoite5c==4]))

Sporozoite10c<-c(sporsATV25$Sporozoite1[sporsATV25$Round==3 & sporsATV25$Bites==10],sporsATV25$Sporozoite2[sporsATV25$Round==3 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite3[sporsATV25$Round==3 & sporsATV25$Bites==10],sporsATV25$Sporozoite4[sporsATV25$Round==3 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite5[sporsATV25$Round==3 & sporsATV25$Bites==10],sporsATV25$Sporozoite6[sporsATV25$Round==3 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite7[sporsATV25$Round==3 & sporsATV25$Bites==10],sporsATV25$Sporozoite8[sporsATV25$Round==3 & sporsATV25$Bites==10],
                 sporsATV25$Sporozoite9[sporsATV25$Round==3 & sporsATV25$Bites==10],sporsATV25$Sporozoite10[sporsATV25$Round==3 & sporsATV25$Bites==10])
spb10r3<-c(length(Sporozoite10c[Sporozoite10c==0]),length(Sporozoite10c[Sporozoite10c==1]),
           length(Sporozoite10c[Sporozoite10c==2]),length(Sporozoite10c[Sporozoite10c==3]),
           length(Sporozoite10c[Sporozoite10c==4]))



bsporsATV25<-rbind(spb1r0,spb1r1,spb1r2,spb1r3,
                   spb2r0,spb2r1,spb2r2,spb2r3,
                   spb5r0,spb5r1,spb5r2,spb5r3,
                   spb10r0,spb10r1,spb10r2,spb10r3)

MEANspATV25<-c(
  mean(Sporozoite1a,na.rm=T),mean(Sporozoite1b,na.rm=T),mean(Sporozoite1c,na.rm=T),
  mean(Sporozoite2a,na.rm=T),mean(Sporozoite2b,na.rm=T),mean(Sporozoite2c,na.rm=T),
  mean(Sporozoite5a,na.rm=T),mean(Sporozoite5b,na.rm=T),mean(Sporozoite5c,na.rm=T),
  mean(Sporozoite10a,na.rm=T),mean(Sporozoite10b,na.rm=T),mean(Sporozoite10c,na.rm=T))

MEANspATV25_2<-c(
  mean(c(Sporozoite1a,Sporozoite1b,Sporozoite1c),na.rm=T),
  mean(c(Sporozoite2a,Sporozoite2b,Sporozoite2c),na.rm=T),
  mean(c(Sporozoite5a,Sporozoite5b,Sporozoite5c),na.rm=T),
  mean(c(Sporozoite10a,Sporozoite10b,Sporozoite10c),na.rm=T))

sporsATV25<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mouse.txt",header=TRUE)

points(c(0,mean(sporsATV25$Parasitemia[sporsATV25$Bites==1]),
       mean(sporsATV25$Parasitemia[sporsATV25$Bites==2]),
       mean(sporsATV25$Parasitemia[sporsATV25$Bites==5]),
       mean(sporsATV25$Parasitemia[sporsATV25$Bites==10]))~c(0,MEANspATV25_2),
     ylab="Parasitemia (%)", xlab="Sporozoite Score",pch=15)
abline(lm(c(0,mean(sporsATV25$Parasitemia[sporsATV25$Bites==1]),
            mean(sporsATV25$Parasitemia[sporsATV25$Bites==2]),
            mean(sporsATV25$Parasitemia[sporsATV25$Bites==5]),
            mean(sporsATV25$Parasitemia[sporsATV25$Bites==10]))~c(0,MEANspATV25_2)+0),lty=2)


##################################
## 1.3 ATV-50%

##1.3 Oocysts 
ATV50<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-50 Graph\\mosquito.txt",header=TRUE)
ATV50$OocPrev<-ifelse(ATV50$Oocyst==0,0,1)
head(ATV50)
##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_ATV50<-c(sample(ATV50$Oocyst[ATV50$Bites == 0 & ATV50$Round == 0],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 1],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 2],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 3],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 0 & ATV50$Round == 0],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 1],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 2],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 3],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 0 & ATV50$Round == 0],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 1],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 2],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 3],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 0 & ATV50$Round == 0],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 10 & ATV50$Round == 1],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 10 & ATV50$Round == 2],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 10 & ATV50$Round == 3],45))

length(oocysts_ATV50)
prevooc<-ifelse(oocysts_ATV50==0,0,1)
prevoocATV50<-ifelse(oocysts_ATV50==0,0,1)
prevooc1ATV50<-c(sum(sum(prevooc[1:45])/45,sum(prevooc[46:90])/45,sum(prevooc[91:135])/45)/3,
               sum(sum(prevooc[136:180])/45,sum(prevooc[181:225])/45,sum(prevooc[226:270])/45)/3,
               sum(sum(prevooc[271:315])/45,sum(prevooc[316:360])/45,sum(prevooc[361:405])/45)/3,
               sum(sum(prevooc[406:450])/45,sum(prevooc[451:495])/45,sum(prevooc[496:540])/45)/3)


meanoocysts_ATV50<-c(mean(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 1]),mean(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 2]),
                     mean(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 3]),
                     mean(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 1]),mean(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 2]),
                     mean(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 3]),
                     mean(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 1]),mean(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 2]),
                     mean(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 3]),
                     mean(ATV50$Oocyst[ATV50$Bites == 10 & ATV50$Round == 1]),mean(ATV50$Oocyst[ATV50$Bites == 10 & ATV50$Round == 2]),
                     mean(ATV50$Oocyst[ATV50$Bites == 10 & ATV50$Round == 3]))


##1.3 Sporozoites, parasitemia and Gametocytemia 
sporsATV50<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-50 Graph\\mouse.txt",header=TRUE)
head(sporsATV50)
##MEAN PARASITEMIA IN MICE
parasitATV50<-cbind(
  sporsATV50$Parasitemia[sporsATV50$Bites==1 & sporsATV50$Round == 0],
  sporsATV50$Parasitemia[sporsATV50$Bites==1 & sporsATV50$Round == 1],
  sporsATV50$Parasitemia[sporsATV50$Bites==1 & sporsATV50$Round == 2],
  sporsATV50$Parasitemia[sporsATV50$Bites==1 & sporsATV50$Round == 3],
  sporsATV50$Parasitemia[sporsATV50$Bites==1 & sporsATV50$Round == 4],
  sporsATV50$Parasitemia[sporsATV50$Bites==2 & sporsATV50$Round == 0],
  sporsATV50$Parasitemia[sporsATV50$Bites==2 & sporsATV50$Round == 1],
  sporsATV50$Parasitemia[sporsATV50$Bites==2 & sporsATV50$Round == 2],
  sporsATV50$Parasitemia[sporsATV50$Bites==2 & sporsATV50$Round == 3],
  sporsATV50$Parasitemia[sporsATV50$Bites==2 & sporsATV50$Round == 4],
  sporsATV50$Parasitemia[sporsATV50$Bites==5 & sporsATV50$Round == 0],
  sporsATV50$Parasitemia[sporsATV50$Bites==5 & sporsATV50$Round == 1],
  sporsATV50$Parasitemia[sporsATV50$Bites==5 & sporsATV50$Round == 2],
  sporsATV50$Parasitemia[sporsATV50$Bites==5 & sporsATV50$Round == 3],
  sporsATV50$Parasitemia[sporsATV50$Bites==5 & sporsATV50$Round == 4],
  sporsATV50$Parasitemia[sporsATV50$Bites==10 & sporsATV50$Round == 0],
  sporsATV50$Parasitemia[sporsATV50$Bites==10 & sporsATV50$Round == 1],
  sporsATV50$Parasitemia[sporsATV50$Bites==10 & sporsATV50$Round == 2],
  sporsATV50$Parasitemia[sporsATV50$Bites==10 & sporsATV50$Round == 3],
  sporsATV50$Parasitemia[sporsATV50$Bites==10 & sporsATV50$Round == 4])

newparasitATV50<-t(parasitATV50)

lines(c(0,mean(sporsATV50$Parasitemia[sporsATV50$Bites==1]),
         mean(sporsATV50$Parasitemia[sporsATV50$Bites==2]),
         mean(sporsATV50$Parasitemia[sporsATV50$Bites==5]),
         mean(sporsATV50$Parasitemia[sporsATV50$Bites==10]))~c(0,1,2,5,10),lty=3)
points(c(0,mean(sporsATV50$Parasitemia[sporsATV50$Bites==1]),
        mean(sporsATV50$Parasitemia[sporsATV50$Bites==2]),
        mean(sporsATV50$Parasitemia[sporsATV50$Bites==5]),
        mean(sporsATV50$Parasitemia[sporsATV50$Bites==10]))~c(0,1,2,5,10),lty=3)


parasitATV50vector<-c(
  sporsATV50$Parasitemia[sporsATV50$Bites==1 & sporsATV50$Round == 1],
  sporsATV50$Parasitemia[sporsATV50$Bites==1 & sporsATV50$Round == 2],
  sporsATV50$Parasitemia[sporsATV50$Bites==1 & sporsATV50$Round == 3],
  sporsATV50$Parasitemia[sporsATV50$Bites==2 & sporsATV50$Round == 1],
  sporsATV50$Parasitemia[sporsATV50$Bites==2 & sporsATV50$Round == 2],
  sporsATV50$Parasitemia[sporsATV50$Bites==2 & sporsATV50$Round == 3],
  sporsATV50$Parasitemia[sporsATV50$Bites==5 & sporsATV50$Round == 1],
  sporsATV50$Parasitemia[sporsATV50$Bites==5 & sporsATV50$Round == 2],
  sporsATV50$Parasitemia[sporsATV50$Bites==5 & sporsATV50$Round == 3],
  sporsATV50$Parasitemia[sporsATV50$Bites==10 & sporsATV50$Round == 1],
  sporsATV50$Parasitemia[sporsATV50$Bites==10 & sporsATV50$Round == 2],
  sporsATV50$Parasitemia[sporsATV50$Bites==10 & sporsATV50$Round == 3])
parasitATV50vectorNAMES<-c(rep("ConB1G1",5),rep("ConB1G2",5),rep("ConB1G3",5),
                           rep("ConB2G1",5),rep("ConB2G2",5),rep("ConB2G3",5),
                           rep("ConB5G1",5),rep("ConB5G2",5),rep("ConB5G3",5),
                           rep("ConB10G1",5),rep("ConB10G2",5),rep("ConB10G3",5))
parasitMeanATV50<-data.frame(parasitATV50vector,parasitATV50vectorNAMES)
te<-as.vector(tapply(parasitMeanATV50$parasitATV50vector,parasitMeanATV50$parasitATV50vectorNAMES,mean))
ATV50PARAmean<-c(te[4:12],te[1:3])

parasitemATV50<-parasitemUPPATV50<-parasitemLOWATV50<-numeric(12)
for (i in 1:12){
  parasitemATV50[i]<-sum(parasitATV50[,i])/5
  
  # a<-numeric(10000)
  #  for (j in 1:10000){ 
  
  #   a[j]<-mean(sample(parasitATV50[,i],4))
  #  parasitemUPPATV50[i]<-quantile(a,0.975)
  # parasitemLOWATV50[i]<-quantile(a,0.025)
  #}
}

##MEAN Gametocytemia IN MICE
gametATV50<-cbind(
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==1 & sporsATV50$Round == 1],5),
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==1 & sporsATV50$Round == 2],5),
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==1 & sporsATV50$Round == 3],5),
  
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==2 & sporsATV50$Round == 1],5),
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==2 & sporsATV50$Round == 2],5),
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==2 & sporsATV50$Round == 3],5),
  
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==5 & sporsATV50$Round == 1],5),
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==5 & sporsATV50$Round == 2],5),
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==5 & sporsATV50$Round == 3],5),
  
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==10 & sporsATV50$Round == 1],5),
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==10 & sporsATV50$Round == 2],5),
  sample(sporsATV50$Gametocytemia[sporsATV50$Bites==10 & sporsATV50$Round == 3],5))

gametATV50vector<-c(
  sporsATV50$Gametocytemia[sporsATV50$Bites==1 & sporsATV50$Round == 1],
  sporsATV50$Gametocytemia[sporsATV50$Bites==1 & sporsATV50$Round == 2],
  sporsATV50$Gametocytemia[sporsATV50$Bites==1 & sporsATV50$Round == 3],
  sporsATV50$Gametocytemia[sporsATV50$Bites==2 & sporsATV50$Round == 1],
  sporsATV50$Gametocytemia[sporsATV50$Bites==2 & sporsATV50$Round == 2],
  sporsATV50$Gametocytemia[sporsATV50$Bites==2 & sporsATV50$Round == 3],
  sporsATV50$Gametocytemia[sporsATV50$Bites==5 & sporsATV50$Round == 1],
  sporsATV50$Gametocytemia[sporsATV50$Bites==5 & sporsATV50$Round == 2],
  sporsATV50$Gametocytemia[sporsATV50$Bites==5 & sporsATV50$Round == 3],
  sporsATV50$Gametocytemia[sporsATV50$Bites==10 & sporsATV50$Round == 1],
  sporsATV50$Gametocytemia[sporsATV50$Bites==10 & sporsATV50$Round == 2],
  sporsATV50$Gametocytemia[sporsATV50$Bites==10 & sporsATV50$Round == 3])
gamATV50vectorNAMES<-c(rep("ConB1G1",5),rep("ConB1G2",5),rep("ConB1G3",5),
                       rep("ConB2G1",5),rep("ConB2G2",5),rep("ConB2G3",5),
                       rep("ConB5G1",5),rep("ConB5G2",5),rep("ConB5G3",5),
                       rep("ConB10G1",5),rep("ConB10G2",5),rep("ConB10G3",5))
gamMeanATV50<-data.frame(gametATV50vector,gamATV50vectorNAMES)
te<-as.vector(tapply(gamMeanATV50$gametATV50vector,gamMeanATV50$gamATV50vectorNAMES,mean))
ATV50GAMmean<-c(te[4:12],te[1:3])

gametoATV50<-gametoUPPATV50<-gametoLOWATV50<-numeric(12)
for (i in 1:12){
  gametoATV50[i]<-sum(gametATV50[,i])/5
  #a<-numeric(10000)
  #for (j in 1:10000){ 
  
  #  a[j]<-mean(sample(gametATV50[,i],4))
  #  gametoUPPATV50[i]<-quantile(a,0.975)
  #  gametoLOWATV50[i]<-quantile(a,0.025)
  #}
}

##PREVALENCE IN MICE
sporsATV50$prevBS<-ifelse(sporsATV50$Parasitemia > 0 | sporsATV50$Gametocytemia > 0, 1, 0)
spcount<-expand.grid(seq(1,nrow(sporsATV50),1))
for(i in 1:nrow(sporsATV50)){
  for (j in 6:15){
    spcount[i,j-5]<-ifelse(is.na(sporsATV50[i,j])==FALSE,1,0)}}
for (i in 1:length(sporsATV50$prevBS)){
  sporsATV50$sporoCount[i]<-sum(spcount[i,])}
for (i in 1:nrow(sporsATV50)){
  sporsATV50$meanpermouse[i]<-sum(sporsATV50[i,6:15],na.rm=T)/sporsATV50$sporoCount[i]
}

PREV_ATV50<-cbind(
  sample(sporsATV50$prevBS[sporsATV50$Bites==1 & sporsATV50$Round == 1],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==1 & sporsATV50$Round == 2],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==1 & sporsATV50$Round == 3],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==2 & sporsATV50$Round == 1],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==2 & sporsATV50$Round == 2],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==2 & sporsATV50$Round == 3],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==5 & sporsATV50$Round == 1],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==5 & sporsATV50$Round == 2],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==5 & sporsATV50$Round == 3],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==10 & sporsATV50$Round == 1],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==10 & sporsATV50$Round == 2],5),
  sample(sporsATV50$prevBS[sporsATV50$Bites==10 & sporsATV50$Round == 3],5))

PREV_ATV50mean<-c(
  mean(sporsATV50$prevBS[sporsATV50$Bites==1 & sporsATV50$Round == 1]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==1 & sporsATV50$Round == 2]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==1 & sporsATV50$Round == 3]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==2 & sporsATV50$Round == 1]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==2 & sporsATV50$Round == 2]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==2 & sporsATV50$Round == 3]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==5 & sporsATV50$Round == 1]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==5 & sporsATV50$Round == 2]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==5 & sporsATV50$Round == 3]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==10 & sporsATV50$Round == 1]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==10 & sporsATV50$Round == 2]),
  mean(sporsATV50$prevBS[sporsATV50$Bites==10 & sporsATV50$Round == 3]))

###Sporozoite intensity
Sporozoite1Z<-sporsATV50$Sporozoite1[sporsATV50$Round==0 & sporsATV50$Bites==1]
spb1r0<-c(length(Sporozoite1Z[Sporozoite1Z==0]),length(Sporozoite1Z[Sporozoite1Z==1]),
          length(Sporozoite1Z[Sporozoite1Z==2]),length(Sporozoite1Z[Sporozoite1Z==3]),
          length(Sporozoite1Z[Sporozoite1Z==4]))

Sporozoite2Z<-c(sporsATV50$Sporozoite1[sporsATV50$Round==0 & sporsATV50$Bites==2],sporsATV50$Sporozoite2[sporsATV50$Round==0 & sporsATV50$Bites==2])
spb2r0<-c(length(Sporozoite2Z[Sporozoite2Z==0]),length(Sporozoite2Z[Sporozoite2Z==1]),
          length(Sporozoite2Z[Sporozoite2Z==2]),length(Sporozoite2Z[Sporozoite2Z==3]),
          length(Sporozoite2Z[Sporozoite2Z==4]))

Sporozoite5Z<-c(sporsATV50$Sporozoite1[sporsATV50$Round==0 & sporsATV50$Bites==5],sporsATV50$Sporozoite2[sporsATV50$Round==0 & sporsATV50$Bites==5],
                sporsATV50$Sporozoite3[sporsATV50$Round==0 & sporsATV50$Bites==5],sporsATV50$Sporozoite4[sporsATV50$Round==0 & sporsATV50$Bites==5],
                sporsATV50$Sporozoite5[sporsATV50$Round==0 & sporsATV50$Bites==5])
spb5r0<-c(length(Sporozoite5Z[Sporozoite5Z==0]),length(Sporozoite5Z[Sporozoite5Z==1]),
          length(Sporozoite5Z[Sporozoite5Z==2]),length(Sporozoite5Z[Sporozoite5Z==3]),
          length(Sporozoite5Z[Sporozoite5Z==4]))

Sporozoite10Z<-c(sporsATV50$Sporozoite1[sporsATV50$Round==0 & sporsATV50$Bites==10],sporsATV50$Sporozoite2[sporsATV50$Round==0 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite3[sporsATV50$Round==0 & sporsATV50$Bites==10],sporsATV50$Sporozoite4[sporsATV50$Round==0 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite5[sporsATV50$Round==0 & sporsATV50$Bites==10],sporsATV50$Sporozoite6[sporsATV50$Round==0 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite7[sporsATV50$Round==0 & sporsATV50$Bites==10],sporsATV50$Sporozoite8[sporsATV50$Round==0 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite9[sporsATV50$Round==0 & sporsATV50$Bites==10],sporsATV50$Sporozoite10[sporsATV50$Round==0 & sporsATV50$Bites==10])
spb10r0<-c(length(Sporozoite10Z[Sporozoite10Z==0]),length(Sporozoite10Z[Sporozoite10Z==1]),
           length(Sporozoite10Z[Sporozoite10Z==2]),length(Sporozoite10Z[Sporozoite10Z==3]),
           length(Sporozoite10Z[Sporozoite10Z==4]))

##
Sporozoite1a<-sporsATV50$Sporozoite1[sporsATV50$Round==1 & sporsATV50$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(sporsATV50$Sporozoite1[sporsATV50$Round==1 & sporsATV50$Bites==2],sporsATV50$Sporozoite2[sporsATV50$Round==1 & sporsATV50$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(sporsATV50$Sporozoite1[sporsATV50$Round==1 & sporsATV50$Bites==5],sporsATV50$Sporozoite2[sporsATV50$Round==1 & sporsATV50$Bites==5],
                sporsATV50$Sporozoite3[sporsATV50$Round==1 & sporsATV50$Bites==5],sporsATV50$Sporozoite4[sporsATV50$Round==1 & sporsATV50$Bites==5],
                sporsATV50$Sporozoite5[sporsATV50$Round==1 & sporsATV50$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(sporsATV50$Sporozoite1[sporsATV50$Round==1 & sporsATV50$Bites==10],sporsATV50$Sporozoite2[sporsATV50$Round==1 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite3[sporsATV50$Round==1 & sporsATV50$Bites==10],sporsATV50$Sporozoite4[sporsATV50$Round==1 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite5[sporsATV50$Round==1 & sporsATV50$Bites==10],sporsATV50$Sporozoite6[sporsATV50$Round==1 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite7[sporsATV50$Round==1 & sporsATV50$Bites==10],sporsATV50$Sporozoite8[sporsATV50$Round==1 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite9[sporsATV50$Round==1 & sporsATV50$Bites==10],sporsATV50$Sporozoite10[sporsATV50$Round==1 & sporsATV50$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
           length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
           length(Sporozoite10a[Sporozoite10a==4]))


Sporozoite1b<-sporsATV50$Sporozoite1[sporsATV50$Round==2 & sporsATV50$Bites==1]
spb1r2<-c(length(Sporozoite1b[Sporozoite1b==0]),length(Sporozoite1b[Sporozoite1b==1]),
          length(Sporozoite1b[Sporozoite1b==2]),length(Sporozoite1b[Sporozoite1b==3]),
          length(Sporozoite1b[Sporozoite1b==4]))

Sporozoite2b<-c(sporsATV50$Sporozoite1[sporsATV50$Round==2 & sporsATV50$Bites==2],sporsATV50$Sporozoite2[sporsATV50$Round==2 & sporsATV50$Bites==2])
spb2r2<-c(length(Sporozoite2b[Sporozoite2b==0]),length(Sporozoite2b[Sporozoite2b==1]),
          length(Sporozoite2b[Sporozoite2b==2]),length(Sporozoite2b[Sporozoite2b==3]),
          length(Sporozoite2b[Sporozoite2b==4]))

Sporozoite5b<-c(sporsATV50$Sporozoite1[sporsATV50$Round==2 & sporsATV50$Bites==5],sporsATV50$Sporozoite2[sporsATV50$Round==2 & sporsATV50$Bites==5],
                sporsATV50$Sporozoite3[sporsATV50$Round==2 & sporsATV50$Bites==5],sporsATV50$Sporozoite4[sporsATV50$Round==2 & sporsATV50$Bites==5],
                sporsATV50$Sporozoite5[sporsATV50$Round==2 & sporsATV50$Bites==5])
spb5r2<-c(length(Sporozoite5b[Sporozoite5b==0]),length(Sporozoite5b[Sporozoite5b==1]),
          length(Sporozoite5b[Sporozoite5b==2]),length(Sporozoite5b[Sporozoite5b==3]),
          length(Sporozoite5b[Sporozoite5b==4]))

Sporozoite10b<-c(sporsATV50$Sporozoite1[sporsATV50$Round==2 & sporsATV50$Bites==10],sporsATV50$Sporozoite2[sporsATV50$Round==2 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite3[sporsATV50$Round==2 & sporsATV50$Bites==10],sporsATV50$Sporozoite4[sporsATV50$Round==2 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite5[sporsATV50$Round==2 & sporsATV50$Bites==10],sporsATV50$Sporozoite6[sporsATV50$Round==2 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite7[sporsATV50$Round==2 & sporsATV50$Bites==10],sporsATV50$Sporozoite8[sporsATV50$Round==2 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite9[sporsATV50$Round==2 & sporsATV50$Bites==10],sporsATV50$Sporozoite10[sporsATV50$Round==2 & sporsATV50$Bites==10])
spb10r2<-c(length(Sporozoite10b[Sporozoite10b==0]),length(Sporozoite10b[Sporozoite10b==1]),
           length(Sporozoite10b[Sporozoite10b==2]),length(Sporozoite10b[Sporozoite10b==3]),
           length(Sporozoite10b[Sporozoite10b==4]))


Sporozoite1c<-sporsATV50$Sporozoite1[sporsATV50$Round==3 & sporsATV50$Bites==1]
spb1r3<-c(length(Sporozoite1c[Sporozoite1c==0]),length(Sporozoite1c[Sporozoite1c==1]),
          length(Sporozoite1c[Sporozoite1c==2]),length(Sporozoite1c[Sporozoite1c==3]),
          length(Sporozoite1c[Sporozoite1c==4]))

Sporozoite2c<-c(sporsATV50$Sporozoite1[sporsATV50$Round==3 & sporsATV50$Bites==2],sporsATV50$Sporozoite2[sporsATV50$Round==3 & sporsATV50$Bites==2])
spb2r3<-c(length(Sporozoite2c[Sporozoite2c==0]),length(Sporozoite2c[Sporozoite2c==1]),
          length(Sporozoite2c[Sporozoite2c==2]),length(Sporozoite2c[Sporozoite2c==3]),
          length(Sporozoite2c[Sporozoite2c==4]))

Sporozoite5c<-c(sporsATV50$Sporozoite1[sporsATV50$Round==3 & sporsATV50$Bites==5],sporsATV50$Sporozoite2[sporsATV50$Round==3 & sporsATV50$Bites==5],
                sporsATV50$Sporozoite3[sporsATV50$Round==3 & sporsATV50$Bites==5],sporsATV50$Sporozoite4[sporsATV50$Round==3 & sporsATV50$Bites==5],
                sporsATV50$Sporozoite5[sporsATV50$Round==3 & sporsATV50$Bites==5])
spb5r3<-c(length(Sporozoite5c[Sporozoite5c==0]),length(Sporozoite5c[Sporozoite5c==1]),
          length(Sporozoite5c[Sporozoite5c==2]),length(Sporozoite5c[Sporozoite5c==3]),
          length(Sporozoite5c[Sporozoite5c==4]))

Sporozoite10c<-c(sporsATV50$Sporozoite1[sporsATV50$Round==3 & sporsATV50$Bites==10],sporsATV50$Sporozoite2[sporsATV50$Round==3 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite3[sporsATV50$Round==3 & sporsATV50$Bites==10],sporsATV50$Sporozoite4[sporsATV50$Round==3 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite5[sporsATV50$Round==3 & sporsATV50$Bites==10],sporsATV50$Sporozoite6[sporsATV50$Round==3 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite7[sporsATV50$Round==3 & sporsATV50$Bites==10],sporsATV50$Sporozoite8[sporsATV50$Round==3 & sporsATV50$Bites==10],
                 sporsATV50$Sporozoite9[sporsATV50$Round==3 & sporsATV50$Bites==10],sporsATV50$Sporozoite10[sporsATV50$Round==3 & sporsATV50$Bites==10])
spb10r3<-c(length(Sporozoite10c[Sporozoite10c==0]),length(Sporozoite10c[Sporozoite10c==1]),
           length(Sporozoite10c[Sporozoite10c==2]),length(Sporozoite10c[Sporozoite10c==3]),
           length(Sporozoite10c[Sporozoite10c==4]))


bsporsATV50<-rbind(spb1r0,spb1r1,spb1r2,spb1r3,
                   spb2r0,spb2r1,spb2r2,spb2r3,
                   spb5r0,spb5r1,spb5r2,spb5r3,
                   spb10r0,spb10r1,spb10r2,spb10r3)

MEANspATV50<-c(
  mean(Sporozoite1a,na.rm=T),mean(Sporozoite1b,na.rm=T),mean(Sporozoite1c,na.rm=T),
  mean(Sporozoite2a,na.rm=T),mean(Sporozoite2b,na.rm=T),mean(Sporozoite2c,na.rm=T),
  mean(Sporozoite5a,na.rm=T),mean(Sporozoite5b,na.rm=T),mean(Sporozoite5c,na.rm=T),
  mean(Sporozoite10a,na.rm=T),mean(Sporozoite10b,na.rm=T),mean(Sporozoite10c,na.rm=T))

MEANspATV50_2<-c(
  mean(c(Sporozoite1a,Sporozoite1b,Sporozoite1c),na.rm=T),
  mean(c(Sporozoite2a,Sporozoite2b,Sporozoite2c),na.rm=T),
  mean(c(Sporozoite5a,Sporozoite5b,Sporozoite5c),na.rm=T),
  mean(c(Sporozoite10a,Sporozoite10b,Sporozoite10c),na.rm=T))


points(c(0,mean(sporsATV50$Parasitemia[sporsATV50$Bites==1]),
         mean(sporsATV50$Parasitemia[sporsATV50$Bites==2]),
         mean(sporsATV50$Parasitemia[sporsATV50$Bites==5]),
         mean(sporsATV50$Parasitemia[sporsATV50$Bites==10]))~c(0,MEANspATV50_2),
       ylab="Parasitemia (%)", xlab="Sporozoite Score",pch=17)
abline(lm(c(0,mean(sporsATV50$Parasitemia[sporsATV50$Bites==1]),
            mean(sporsATV50$Parasitemia[sporsATV50$Bites==2]),
            mean(sporsATV50$Parasitemia[sporsATV50$Bites==5]),
            mean(sporsATV50$Parasitemia[sporsATV50$Bites==10]))~c(0,MEANspATV50_2)+0),lty=3)

##################################
## 1.4 ATV-85%

##1.4 Oocysts 
ATV85<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-85\\mosquito.txt",header=TRUE)
ATV85$OocPrev<-ifelse(ATV85$Oocyst==0,0,1)
head(ATV85)
##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_ATV85<-c(sample(ATV85$Oocyst[ATV85$Bites == 0 & ATV85$Round == 0],45),sample(ATV85$Oocyst[ATV85$Bites == 1 & ATV85$Round == 1],45),rep(0,45*2),
                 sample(ATV85$Oocyst[ATV85$Bites == 0 & ATV85$Round == 0],45),sample(ATV85$Oocyst[ATV85$Bites == 2 & ATV85$Round == 1],45),rep(0,45*2),
                 sample(ATV85$Oocyst[ATV85$Bites == 0 & ATV85$Round == 0],45),sample(ATV85$Oocyst[ATV85$Bites == 5 & ATV85$Round == 1],45),rep(0,45*2),
                 sample(ATV85$Oocyst[ATV85$Bites == 0 & ATV85$Round == 0],45),sample(ATV85$Oocyst[ATV85$Bites == 10 & ATV85$Round == 1],45),rep(0,45*2))

length(oocysts_ATV85)
prevooc<-ifelse(oocysts_ATV85==0,0,1)
prevoocATV85<-ifelse(oocysts_ATV85==0,0,1)
prevooc1ATV85<-c(sum(prevooc[1:45])/45,
                 sum(prevooc[46:90])/45,
                 sum(prevooc[91:135])/45,
                 sum(prevooc[136:180])/45)


meanoocysts_ATV85<-c(mean(ATV85$Oocyst[ATV85$Bites == 1 & ATV85$Round == 1]),
                     mean(ATV85$Oocyst[ATV85$Bites == 2 & ATV85$Round == 1]),
                     mean(ATV85$Oocyst[ATV85$Bites == 5 & ATV85$Round == 1]),
                     mean(ATV85$Oocyst[ATV85$Bites == 10 & ATV85$Round == 1]))


##1.4 Sporozoites, parasitemia and Gametocytemia 
sporsATV85<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-85\\mouse.txt",header=TRUE)
summary(sporsATV85)

##MEAN PARASITEMIA IN MICE
parasitATV85<-cbind(
  sporsATV85$Parasitemia[sporsATV85$Bites==1 & sporsATV85$Round == 0],
  sporsATV85$Parasitemia[sporsATV85$Bites==1 & sporsATV85$Round == 1],rep(0,5),rep(0,5),rep(0,5),
    sporsATV85$Parasitemia[sporsATV85$Bites==2 & sporsATV85$Round == 0],
  sporsATV85$Parasitemia[sporsATV85$Bites==2 & sporsATV85$Round == 1],rep(0,5),rep(0,5),rep(0,5),
    sporsATV85$Parasitemia[sporsATV85$Bites==5 & sporsATV85$Round == 0],
  sporsATV85$Parasitemia[sporsATV85$Bites==5 & sporsATV85$Round == 1],rep(0,5),rep(0,5),rep(0,5),
    sporsATV85$Parasitemia[sporsATV85$Bites==10 & sporsATV85$Round == 0],
  sporsATV85$Parasitemia[sporsATV85$Bites==10 & sporsATV85$Round == 1],rep(0,5),rep(0,5),rep(0,5))

newparasitATV85<-t(parasitATV85)

parasitemATV85<-parasitemUPPATV85<-parasitemLOWATV85<-numeric(4)
for (i in 1:4){
  parasitemATV85[i]<-sum(parasitATV85[,i])/5
  
  # a<-numeric(10000)
  #  for (j in 1:10000){ 
  
  #   a[j]<-mean(sample(parasitATV85[,i],4))
  #  parasitemUPPATV85[i]<-quantile(a,0.975)
  # parasitemLOWATV85[i]<-quantile(a,0.025)
  #}
}

##MEAN Gametocytemia IN MICE
gametATV85<-cbind(
  sample(sporsATV85$Gametocytemia[sporsATV85$Bites==1 & sporsATV85$Round == 1],5),  
  sample(sporsATV85$Gametocytemia[sporsATV85$Bites==2 & sporsATV85$Round == 1],5),
  sample(sporsATV85$Gametocytemia[sporsATV85$Bites==5 & sporsATV85$Round == 1],5),
  sample(sporsATV85$Gametocytemia[sporsATV85$Bites==10 & sporsATV85$Round == 1],5))
gametoATV85<-gametoUPPATV85<-gametoLOWATV85<-numeric(4)
for (i in 1:4){
  gametoATV85[i]<-sum(gametATV85[,i])/5
  #a<-numeric(10000)
  #for (j in 1:10000){ 
  
  #  a[j]<-mean(sample(gametATV85[,i],4))
  #  gametoUPPATV85[i]<-quantile(a,0.975)
  #  gametoLOWATV85[i]<-quantile(a,0.025)
  #}
}

##PREVALENCE IN MICE
sporsATV85$prevBS<-ifelse(sporsATV85$Parasitemia > 0 | sporsATV85$Gametocytemia > 0, 1, 0)

PREV_ATV85<-cbind(
  sample(sporsATV85$prevBS[sporsATV85$Bites==1 & sporsATV85$Round == 1],5),
  sample(sporsATV85$prevBS[sporsATV85$Bites==2 & sporsATV85$Round == 1],5),
  sample(sporsATV85$prevBS[sporsATV85$Bites==5 & sporsATV85$Round == 1],5),
  sample(sporsATV85$prevBS[sporsATV85$Bites==10 & sporsATV85$Round == 1],5))

###Sporozoite intensity
Sporozoite1z<-sporsATV85$Sporozoite1[sporsATV85$Round==0 & sporsATV85$Bites==1]
spb1r0<-c(length(Sporozoite1z[Sporozoite1z==0]),length(Sporozoite1z[Sporozoite1z==1]),
          length(Sporozoite1z[Sporozoite1z==2]),length(Sporozoite1z[Sporozoite1z==3]),
          length(Sporozoite1z[Sporozoite1z==4]))

Sporozoite2z<-c(sporsATV85$Sporozoite1[sporsATV85$Round==0 & sporsATV85$Bites==2],sporsATV85$Sporozoite2[sporsATV85$Round==0 & sporsATV85$Bites==2])
spb2r0<-c(length(Sporozoite2z[Sporozoite2z==0]),length(Sporozoite2z[Sporozoite2z==1]),
          length(Sporozoite2z[Sporozoite2z==2]),length(Sporozoite2z[Sporozoite2z==3]),
          length(Sporozoite2z[Sporozoite2z==4]))

Sporozoite5z<-c(sporsATV85$Sporozoite1[sporsATV85$Round==0 & sporsATV85$Bites==5],sporsATV85$Sporozoite2[sporsATV85$Round==0 & sporsATV85$Bites==5],
                sporsATV85$Sporozoite3[sporsATV85$Round==0 & sporsATV85$Bites==5],sporsATV85$Sporozoite4[sporsATV85$Round==0 & sporsATV85$Bites==5],
                sporsATV85$Sporozoite5[sporsATV85$Round==0 & sporsATV85$Bites==5])
spb5r0<-c(length(Sporozoite5z[Sporozoite5z==0]),length(Sporozoite5z[Sporozoite5z==1]),
          length(Sporozoite5z[Sporozoite5z==2]),length(Sporozoite5z[Sporozoite5z==3]),
          length(Sporozoite5z[Sporozoite5z==4]))

Sporozoite10z<-c(sporsATV85$Sporozoite1[sporsATV85$Round==0 & sporsATV85$Bites==10],sporsATV85$Sporozoite2[sporsATV85$Round==0 & sporsATV85$Bites==10],
                 sporsATV85$Sporozoite3[sporsATV85$Round==0 & sporsATV85$Bites==10],sporsATV85$Sporozoite4[sporsATV85$Round==0 & sporsATV85$Bites==10],
                 sporsATV85$Sporozoite5[sporsATV85$Round==0 & sporsATV85$Bites==10],sporsATV85$Sporozoite6[sporsATV85$Round==0 & sporsATV85$Bites==10],
                 sporsATV85$Sporozoite7[sporsATV85$Round==0 & sporsATV85$Bites==10],sporsATV85$Sporozoite8[sporsATV85$Round==0 & sporsATV85$Bites==10],
                 sporsATV85$Sporozoite9[sporsATV85$Round==0 & sporsATV85$Bites==10],sporsATV85$Sporozoite10[sporsATV85$Round==0 & sporsATV85$Bites==10])
spb10r0<-c(length(Sporozoite10z[Sporozoite10z==0]),length(Sporozoite10z[Sporozoite10z==1]),
           length(Sporozoite10z[Sporozoite10z==2]),length(Sporozoite10z[Sporozoite10z==3]),
           length(Sporozoite10z[Sporozoite10z==4]))
##
Sporozoite1a<-sporsATV85$Sporozoite1[sporsATV85$Round==1 & sporsATV85$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(sporsATV85$Sporozoite1[sporsATV85$Round==1 & sporsATV85$Bites==2],sporsATV85$Sporozoite2[sporsATV85$Round==1 & sporsATV85$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(sporsATV85$Sporozoite1[sporsATV85$Round==1 & sporsATV85$Bites==5],sporsATV85$Sporozoite2[sporsATV85$Round==1 & sporsATV85$Bites==5],
                sporsATV85$Sporozoite3[sporsATV85$Round==1 & sporsATV85$Bites==5],sporsATV85$Sporozoite4[sporsATV85$Round==1 & sporsATV85$Bites==5],
                sporsATV85$Sporozoite5[sporsATV85$Round==1 & sporsATV85$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(sporsATV85$Sporozoite1[sporsATV85$Round==1 & sporsATV85$Bites==10],sporsATV85$Sporozoite2[sporsATV85$Round==1 & sporsATV85$Bites==10],
                 sporsATV85$Sporozoite3[sporsATV85$Round==1 & sporsATV85$Bites==10],sporsATV85$Sporozoite4[sporsATV85$Round==1 & sporsATV85$Bites==10],
                 sporsATV85$Sporozoite5[sporsATV85$Round==1 & sporsATV85$Bites==10],sporsATV85$Sporozoite6[sporsATV85$Round==1 & sporsATV85$Bites==10],
                 sporsATV85$Sporozoite7[sporsATV85$Round==1 & sporsATV85$Bites==10],sporsATV85$Sporozoite8[sporsATV85$Round==1 & sporsATV85$Bites==10],
                 sporsATV85$Sporozoite9[sporsATV85$Round==1 & sporsATV85$Bites==10],sporsATV85$Sporozoite10[sporsATV85$Round==1 & sporsATV85$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
           length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
           length(Sporozoite10a[Sporozoite10a==4]))


sporsATV85<-rbind(spb1r0,spb1r1,rep(0,5),rep(0,5),
                  spb2r0,spb2r1,rep(0,5),rep(0,5),
                  spb5r0,spb5r1,rep(0,5),rep(0,5),
                  spb10r0,spb10r1,rep(0,5),rep(0,5))

MEANspATV85<-c(mean(Sporozoite1a,na.rm=T),mean(Sporozoite2a,na.rm=T),
               mean(Sporozoite5a,na.rm=T),mean(Sporozoite10a,na.rm=T))

##################################
## 1.5 t4B785

##1.5 Oocysts 
t4B785<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\4B7-85\\mosquito.txt",header=TRUE)
t4B785$OocPrev<-ifelse(t4B785$Oocyst==0,0,1)
head(t4B785)
##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_t4B785<-c(sample(t4B785$Oocyst[t4B785$Bites == 0 & t4B785$Round == 0],45),sample(t4B785$Oocyst[t4B785$Bites == 1 & t4B785$Round == 1],45),rep(0,45*2),
                  sample(t4B785$Oocyst[t4B785$Bites == 0 & t4B785$Round == 0],45),sample(t4B785$Oocyst[t4B785$Bites == 2 & t4B785$Round == 1],45),rep(0,45*2),
                  sample(t4B785$Oocyst[t4B785$Bites == 0 & t4B785$Round == 0],45),sample(t4B785$Oocyst[t4B785$Bites == 5 & t4B785$Round == 1],45),rep(0,45*2),
                  sample(t4B785$Oocyst[t4B785$Bites == 0 & t4B785$Round == 0],45),sample(t4B785$Oocyst[t4B785$Bites == 10 & t4B785$Round == 1],45),rep(0,45*2))

length(oocysts_t4B785)
prevooc<-ifelse(oocysts_t4B785==0,0,1)
prevooc4B785<-ifelse(oocysts_t4B785==0,0,1)
prevooc1t4B785<-c(sum(prevooc[1:45])/45,
                 sum(prevooc[46:90])/45,
                 sum(prevooc[91:135])/45,
                 sum(prevooc[136:180])/45)


meanoocysts_t4B785<-c(mean(t4B785$Oocyst[t4B785$Bites == 1 & t4B785$Round == 1]),
                     mean(t4B785$Oocyst[t4B785$Bites == 2 & t4B785$Round == 1]),
                     mean(t4B785$Oocyst[t4B785$Bites == 5 & t4B785$Round == 1]),
                     mean(t4B785$Oocyst[t4B785$Bites == 10 & t4B785$Round == 1]))


##1.5 Sporozoites, parasitemia and Gametocytemia 
sporst4B785<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\4B7-85\\mouse.txt",header=TRUE)
summary(sporst4B785)

##MEAN PARASITEMIA IN MICE
parasitt4B785<-cbind(
  sporst4B785$Parasitemia[sporst4B785$Bites==1 & sporst4B785$Round == 0],
  sporst4B785$Parasitemia[sporst4B785$Bites==1 & sporst4B785$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst4B785$Parasitemia[sporst4B785$Bites==2 & sporst4B785$Round == 0],
  sporst4B785$Parasitemia[sporst4B785$Bites==2 & sporst4B785$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst4B785$Parasitemia[sporst4B785$Bites==5 & sporst4B785$Round == 0],
  sporst4B785$Parasitemia[sporst4B785$Bites==5 & sporst4B785$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst4B785$Parasitemia[sporst4B785$Bites==10 & sporst4B785$Round == 0],
  sporst4B785$Parasitemia[sporst4B785$Bites==10 & sporst4B785$Round == 1],rep(0,5),rep(0,5),rep(0,5))

newparasitt4B785<-t(parasitt4B785)

parasitemt4B785<-parasitemUPPt4B785<-parasitemLOWt4B785<-numeric(4)
for (i in 1:4){
  parasitemt4B785[i]<-sum(parasitt4B785[,i])/5
  
  # a<-numeric(10000)
  #  for (j in 1:10000){ 
  
  #   a[j]<-mean(sample(parasitt4B785[,i],4))
  #  parasitemUPPt4B785[i]<-quantile(a,0.975)
  # parasitemLOWt4B785[i]<-quantile(a,0.025)
  #}
}

##MEAN Gametocytemia IN MICE
gamett4B785<-cbind(
  sample(sporst4B785$Gametocytemia[sporst4B785$Bites==1 & sporst4B785$Round == 1],5),  
  sample(sporst4B785$Gametocytemia[sporst4B785$Bites==2 & sporst4B785$Round == 1],5),
  sample(sporst4B785$Gametocytemia[sporst4B785$Bites==5 & sporst4B785$Round == 1],5),
  sample(sporst4B785$Gametocytemia[sporst4B785$Bites==10 & sporst4B785$Round == 1],5))
gametot4B785<-gametoUPPt4B785<-gametoLOWt4B785<-numeric(4)
for (i in 1:4){
  gametot4B785[i]<-sum(gamett4B785[,i])/5
  #a<-numeric(10000)
  #for (j in 1:10000){ 
  
  #  a[j]<-mean(sample(gamett4B785[,i],4))
  #  gametoUPPt4B785[i]<-quantile(a,0.975)
  #  gametoLOWt4B785[i]<-quantile(a,0.025)
  #}
}

##PREVALENCE IN MICE
sporst4B785$prevBS<-ifelse(sporst4B785$Parasitemia > 0 | sporst4B785$Gametocytemia > 0, 1, 0)

PREV_t4B785<-cbind(
  sample(sporst4B785$prevBS[sporst4B785$Bites==1 & sporst4B785$Round == 1],5),
  sample(sporst4B785$prevBS[sporst4B785$Bites==2 & sporst4B785$Round == 1],5),
  sample(sporst4B785$prevBS[sporst4B785$Bites==5 & sporst4B785$Round == 1],5),
  sample(sporst4B785$prevBS[sporst4B785$Bites==10 & sporst4B785$Round == 1],5))

###Sporozoite intensity
Sporozoite1z<-sporst4B785$Sporozoite1[sporst4B785$Round==0 & sporst4B785$Bites==1]
spb1r0<-c(length(Sporozoite1z[Sporozoite1z==0]),length(Sporozoite1z[Sporozoite1z==1]),
          length(Sporozoite1z[Sporozoite1z==2]),length(Sporozoite1z[Sporozoite1z==3]),
          length(Sporozoite1z[Sporozoite1z==4]))

Sporozoite2z<-c(sporst4B785$Sporozoite1[sporst4B785$Round==0 & sporst4B785$Bites==2],sporst4B785$Sporozoite2[sporst4B785$Round==0 & sporst4B785$Bites==2])
spb2r0<-c(length(Sporozoite2z[Sporozoite2z==0]),length(Sporozoite2z[Sporozoite2z==1]),
          length(Sporozoite2z[Sporozoite2z==2]),length(Sporozoite2z[Sporozoite2z==3]),
          length(Sporozoite2z[Sporozoite2z==4]))

Sporozoite5z<-c(sporst4B785$Sporozoite1[sporst4B785$Round==0 & sporst4B785$Bites==5],sporst4B785$Sporozoite2[sporst4B785$Round==0 & sporst4B785$Bites==5],
                sporst4B785$Sporozoite3[sporst4B785$Round==0 & sporst4B785$Bites==5],sporst4B785$Sporozoite4[sporst4B785$Round==0 & sporst4B785$Bites==5],
                sporst4B785$Sporozoite5[sporst4B785$Round==0 & sporst4B785$Bites==5])
spb5r0<-c(length(Sporozoite5z[Sporozoite5z==0]),length(Sporozoite5z[Sporozoite5z==1]),
          length(Sporozoite5z[Sporozoite5z==2]),length(Sporozoite5z[Sporozoite5z==3]),
          length(Sporozoite5z[Sporozoite5z==4]))

Sporozoite10z<-c(sporst4B785$Sporozoite1[sporst4B785$Round==0 & sporst4B785$Bites==10],sporst4B785$Sporozoite2[sporst4B785$Round==0 & sporst4B785$Bites==10],
                 sporst4B785$Sporozoite3[sporst4B785$Round==0 & sporst4B785$Bites==10],sporst4B785$Sporozoite4[sporst4B785$Round==0 & sporst4B785$Bites==10],
                 sporst4B785$Sporozoite5[sporst4B785$Round==0 & sporst4B785$Bites==10],sporst4B785$Sporozoite6[sporst4B785$Round==0 & sporst4B785$Bites==10],
                 sporst4B785$Sporozoite7[sporst4B785$Round==0 & sporst4B785$Bites==10],sporst4B785$Sporozoite8[sporst4B785$Round==0 & sporst4B785$Bites==10],
                 sporst4B785$Sporozoite9[sporst4B785$Round==0 & sporst4B785$Bites==10],sporst4B785$Sporozoite10[sporst4B785$Round==0 & sporst4B785$Bites==10])
spb10r0<-c(length(Sporozoite10z[Sporozoite10z==0]),length(Sporozoite10z[Sporozoite10z==1]),
           length(Sporozoite10z[Sporozoite10z==2]),length(Sporozoite10z[Sporozoite10z==3]),
           length(Sporozoite10z[Sporozoite10z==4]))
##
Sporozoite1a<-sporst4B785$Sporozoite1[sporst4B785$Round==1 & sporst4B785$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(sporst4B785$Sporozoite1[sporst4B785$Round==1 & sporst4B785$Bites==2],sporst4B785$Sporozoite2[sporst4B785$Round==1 & sporst4B785$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(sporst4B785$Sporozoite1[sporst4B785$Round==1 & sporst4B785$Bites==5],sporst4B785$Sporozoite2[sporst4B785$Round==1 & sporst4B785$Bites==5],
                sporst4B785$Sporozoite3[sporst4B785$Round==1 & sporst4B785$Bites==5],sporst4B785$Sporozoite4[sporst4B785$Round==1 & sporst4B785$Bites==5],
                sporst4B785$Sporozoite5[sporst4B785$Round==1 & sporst4B785$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(sporst4B785$Sporozoite1[sporst4B785$Round==1 & sporst4B785$Bites==10],sporst4B785$Sporozoite2[sporst4B785$Round==1 & sporst4B785$Bites==10],
                 sporst4B785$Sporozoite3[sporst4B785$Round==1 & sporst4B785$Bites==10],sporst4B785$Sporozoite4[sporst4B785$Round==1 & sporst4B785$Bites==10],
                 sporst4B785$Sporozoite5[sporst4B785$Round==1 & sporst4B785$Bites==10],sporst4B785$Sporozoite6[sporst4B785$Round==1 & sporst4B785$Bites==10],
                 sporst4B785$Sporozoite7[sporst4B785$Round==1 & sporst4B785$Bites==10],sporst4B785$Sporozoite8[sporst4B785$Round==1 & sporst4B785$Bites==10],
                 sporst4B785$Sporozoite9[sporst4B785$Round==1 & sporst4B785$Bites==10],sporst4B785$Sporozoite10[sporst4B785$Round==1 & sporst4B785$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
           length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
           length(Sporozoite10a[Sporozoite10a==4]))


spors2t4B785<-rbind(spb1r0,spb1r1,rep(0,5),rep(0,5),
                   spb2r0,spb2r1,rep(0,5),rep(0,5),
                   spb5r0,spb5r1,rep(0,5),rep(0,5),
                   spb10r0,spb10r1,rep(0,5),rep(0,5))

MEANspt4B785<-c(mean(Sporozoite1a,na.rm=T),mean(Sporozoite2a,na.rm=T),
               mean(Sporozoite5a,na.rm=T),mean(Sporozoite10a,na.rm=T))



##################################
## 1.6 3D11 & ATV-85%

##1.6 Oocysts 
t3d11andATV85<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 35ATV-85\\mosquito.txt",header=TRUE)
t3d11andATV85$OocPrev<-ifelse(t3d11andATV85$Oocyst==0,0,1)
head(t3d11andATV85)
t3d11andATV85$Oocyst[t3d11andATV85$Bites == 0 & t3d11andATV85$Round == 0]

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_t3d11andATV85<-c(
  sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 0 & t3d11andATV85$Round == 0],45),sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 1 & t3d11andATV85$Round == 1],45),rep(0,45*2),
  sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 0 & t3d11andATV85$Round == 0],45),sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 2 & t3d11andATV85$Round == 1],45),rep(0,45*2),
  sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 0 & t3d11andATV85$Round == 0],45),sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 5 & t3d11andATV85$Round == 1],45),rep(0,45*2),
  sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 0 & t3d11andATV85$Round == 0],45),sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 10 & t3d11andATV85$Round == 1],45),rep(0,45*2))

length(oocysts_t3d11andATV85)
prevooc<-ifelse(oocysts_t3d11andATV85==0,0,1)
prevooc3d11andATV85<-ifelse(oocysts_t3d11andATV85==0,0,1)
prevooc13D11ANDatv85<-c(sum(prevooc[1:45])/45,
                        sum(prevooc[46:90])/45,
                        sum(prevooc[91:135])/45,
                        sum(prevooc[136:180])/45)


meanoocysts_t3d11andATV85<-c(mean(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 1 & t3d11andATV85$Round == 1]),
                     mean(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 2 & t3d11andATV85$Round == 1]),
                     mean(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 5 & t3d11andATV85$Round == 1]),
                     mean(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 10 & t3d11andATV85$Round == 1]))


##1.6 Sporozoites, parasitemia and Gametocytemia 
sporst3d11andATV85<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 35ATV-85\\mouse.txt",header=TRUE)

##MEAN PARASITEMIA IN MICE
parasitt3d11andATV85<-cbind(
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==1 & sporst3d11andATV85$Round == 0],
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==1 & sporst3d11andATV85$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==2 & sporst3d11andATV85$Round == 0],
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==2 & sporst3d11andATV85$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==5 & sporst3d11andATV85$Round == 0],
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==5 & sporst3d11andATV85$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==10 & sporst3d11andATV85$Round == 0],
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==10 & sporst3d11andATV85$Round == 1],rep(0,5),rep(0,5),rep(0,5))

newparasitt3d11andATV85<-t(parasitt3d11andATV85)

parasitemt3d11andATV85<-parasitemUPPt3d11andATV85<-parasitemLOWt3d11andATV85<-numeric(4)
for (i in 1:4){
  parasitemt3d11andATV85[i]<-sum(parasitt3d11andATV85[,i])/5
  
  # a<-numeric(10000)
  #  for (j in 1:10000){ 
  
  #   a[j]<-mean(sample(parasitt3d11andATV85[,i],4))
  #  parasitemUPPt3d11andATV85[i]<-quantile(a,0.975)
  # parasitemLOWt3d11andATV85[i]<-quantile(a,0.025)
  #}
}

##MEAN Gametocytemia IN MICE
gamett3d11andATV85<-cbind(
  sample(sporst3d11andATV85$Gametocytemia[sporst3d11andATV85$Bites==1 & sporst3d11andATV85$Round == 1],5),
  sample(sporst3d11andATV85$Gametocytemia[sporst3d11andATV85$Bites==2 & sporst3d11andATV85$Round == 1],5),
  sample(sporst3d11andATV85$Gametocytemia[sporst3d11andATV85$Bites==5 & sporst3d11andATV85$Round == 1],5),
  sample(sporst3d11andATV85$Gametocytemia[sporst3d11andATV85$Bites==10 & sporst3d11andATV85$Round == 1],5))
gametot3d11andATV85<-gametoUPPt3d11andATV85<-gametoLOWt3d11andATV85<-numeric(4)
for (i in 1:4){
  gametot3d11andATV85[i]<-sum(gamett3d11andATV85[,i])/5
  #a<-numeric(10000)
  #for (j in 1:10000){ 
  
  #  a[j]<-mean(sample(gamett3d11andATV85[,i],4))
  #  gametoUPPt3d11andATV85[i]<-quantile(a,0.975)
  #  gametoLOWt3d11andATV85[i]<-quantile(a,0.025)
  #}
}

##PREVALENCE IN MICE
sporst3d11andATV85$prevBS<-ifelse(sporst3d11andATV85$Parasitemia > 0 | sporst3d11andATV85$Gametocytemia > 0, 1, 0)

PREV_t3d11andATV85<-cbind(
  sample(sporst3d11andATV85$prevBS[sporst3d11andATV85$Bites==1 & sporst3d11andATV85$Round == 1],5),
  sample(sporst3d11andATV85$prevBS[sporst3d11andATV85$Bites==2 & sporst3d11andATV85$Round == 1],5),
  sample(sporst3d11andATV85$prevBS[sporst3d11andATV85$Bites==5 & sporst3d11andATV85$Round == 1],5),
  sample(sporst3d11andATV85$prevBS[sporst3d11andATV85$Bites==10 & sporst3d11andATV85$Round == 1],5))

###Sporozoite intensity
Sporozoite1a<-sporst3d11andATV85$Sporozoite1[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(sporst3d11andATV85$Sporozoite1[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==2],sporst3d11andATV85$Sporozoite2[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(sporst3d11andATV85$Sporozoite1[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==5],sporst3d11andATV85$Sporozoite2[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==5],
                sporst3d11andATV85$Sporozoite3[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==5],sporst3d11andATV85$Sporozoite4[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==5],
                sporst3d11andATV85$Sporozoite5[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(sporst3d11andATV85$Sporozoite1[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],sporst3d11andATV85$Sporozoite2[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],
                 sporst3d11andATV85$Sporozoite3[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],sporst3d11andATV85$Sporozoite4[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],
                 sporst3d11andATV85$Sporozoite5[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],sporst3d11andATV85$Sporozoite6[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],
                 sporst3d11andATV85$Sporozoite7[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],sporst3d11andATV85$Sporozoite8[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],
                 sporst3d11andATV85$Sporozoite9[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10],sporst3d11andATV85$Sporozoite10[sporst3d11andATV85$Round==1 & sporst3d11andATV85$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
           length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
           length(Sporozoite10a[Sporozoite10a==4]))

sporst3d11andATV85<-rbind(spb1r1,rep(0,5),rep(0,5),rep(0,5),
                          spb2r1,rep(0,5),rep(0,5),rep(0,5),
                          spb5r1,rep(0,5),rep(0,5),rep(0,5),
                          spb10r1,rep(0,5),rep(0,5),rep(0,5))

MEANspt3d11andATV85<-c(
  mean(Sporozoite1a,na.rm=T),
  mean(Sporozoite2a,na.rm=T),
  mean(Sporozoite5a,na.rm=T),
  mean(Sporozoite10a,na.rm=T))

##########################################
##1.7 Oocysts 3D11
T3d11<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3D11-50\\mosquito.txt",header=TRUE)
T3d11$OocPrev<-ifelse(T3d11$Oocyst==0,0,1)

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocystsT3d11<-c(sample(T3d11$Oocyst[T3d11$Bites == 1 & T3d11$Round == 1],45),sample(T3d11$Oocyst[T3d11$Bites == 1 & T3d11$Round == 2],45),
            sample(T3d11$Oocyst[T3d11$Bites == 1 & T3d11$Round == 3],45),sample(T3d11$Oocyst[T3d11$Bites == 1 & T3d11$Round == 4],45),
            sample(T3d11$Oocyst[T3d11$Bites == 2 & T3d11$Round == 1],45),sample(T3d11$Oocyst[T3d11$Bites == 2 & T3d11$Round == 2],45),
            sample(T3d11$Oocyst[T3d11$Bites == 2 & T3d11$Round == 3],45),sample(T3d11$Oocyst[T3d11$Bites == 2 & T3d11$Round == 4],45),
            sample(T3d11$Oocyst[T3d11$Bites == 5 & T3d11$Round == 1],45),sample(T3d11$Oocyst[T3d11$Bites == 5 & T3d11$Round == 2],45),
            sample(T3d11$Oocyst[T3d11$Bites == 5 & T3d11$Round == 3],45),sample(T3d11$Oocyst[T3d11$Bites == 5 & T3d11$Round == 4],45),
            sample(T3d11$Oocyst[T3d11$Bites == 10 & T3d11$Round == 1],45),sample(T3d11$Oocyst[T3d11$Bites == 10 & T3d11$Round == 2],45),
            sample(T3d11$Oocyst[T3d11$Bites == 10 & T3d11$Round == 3],45),sample(T3d11$Oocyst[T3d11$Bites == 10 & T3d11$Round == 4],45))

length(oocystsT3d11)
prevooc<-ifelse(oocystsT3d11==0,0,1)
prevooc3d11<-ifelse(oocystsT3d11==0,0,1)
prevooc1T3d11<-c(sum(sum(prevooc[1:45])/45,sum(prevooc[46:90])/45,sum(prevooc[91:135])/45,sum(prevooc[136:180])/45)/4,
            sum(sum(prevooc[181:225])/45,sum(prevooc[226:270])/45,sum(prevooc[271:315])/45,sum(prevooc[316:360])/45)/4,
            sum(sum(prevooc[361:405])/45,sum(prevooc[406:450])/45,sum(prevooc[451:495])/45,sum(prevooc[496:540])/45)/4,
            sum(sum(prevooc[541:585])/45,sum(prevooc[586:630])/45,sum(prevooc[631:675])/45)/3)


meanoocystsT3d11<-c(mean(T3d11$Oocyst[T3d11$Bites == 1 & T3d11$Round == 1]),mean(T3d11$Oocyst[T3d11$Bites == 1 & T3d11$Round == 2]),
                mean(T3d11$Oocyst[T3d11$Bites == 1 & T3d11$Round == 3]),mean(T3d11$Oocyst[T3d11$Bites == 1 & T3d11$Round == 4]),
                mean(T3d11$Oocyst[T3d11$Bites == 2 & T3d11$Round == 1]),mean(T3d11$Oocyst[T3d11$Bites == 2 & T3d11$Round == 2]),
                mean(T3d11$Oocyst[T3d11$Bites == 2 & T3d11$Round == 3]),mean(T3d11$Oocyst[T3d11$Bites == 2 & T3d11$Round == 4]),
                mean(T3d11$Oocyst[T3d11$Bites == 5 & T3d11$Round == 1]),mean(T3d11$Oocyst[T3d11$Bites == 5 & T3d11$Round == 2]),
                mean(T3d11$Oocyst[T3d11$Bites == 5 & T3d11$Round == 3]),mean(T3d11$Oocyst[T3d11$Bites == 5 & T3d11$Round == 4]),
                mean(T3d11$Oocyst[T3d11$Bites == 10 & T3d11$Round == 1]),mean(T3d11$Oocyst[T3d11$Bites == 10 & T3d11$Round == 2]),
                mean(T3d11$Oocyst[T3d11$Bites == 10 & T3d11$Round == 3]))


##1.7 Sporozoites, parasitemia and Gametocytemia 
T3d11<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3D11-50\\mouse.txt",header=TRUE)

##MEAN PARASITEMIA IN MICE
parasitT3d11<-cbind(
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 0],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 0],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 0],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 0],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 4])
newparasitT3d11<-t(parasitT3d11)

points(c(0,mean(T3d11$Parasitemia[T3d11$Bites==1]),
        mean(T3d11$Parasitemia[T3d11$Bites==2]),
        mean(T3d11$Parasitemia[T3d11$Bites==5]),
        mean(T3d11$Parasitemia[T3d11$Bites==10]))~c(0,1,2,5,10),lty=3,pch=20)
lines(c(0,mean(T3d11$Parasitemia[T3d11$Bites==1]),
         mean(T3d11$Parasitemia[T3d11$Bites==2]),
         mean(T3d11$Parasitemia[T3d11$Bites==5]),
         mean(T3d11$Parasitemia[T3d11$Bites==10]))~c(0,1,2,5,10),lty=3,pch=20)


parasitT3d11vector<-c(
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 3],
  #T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 3],
  #T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 3],
  #T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 3])
parasitT3D11vectorNAMES<-c(rep("ConB1G1",5),rep("ConB1G2",5),rep("ConB1G3",5),
                           rep("ConB2G1",5),rep("ConB2G2",5),rep("ConB2G3",5),
                           rep("ConB5G1",5),rep("ConB5G2",5),rep("ConB5G3",5),
                           rep("ConB10G1",5),rep("ConB10G2",5),rep("ConB10G3",5))
parasitMeanT3D11<-data.frame(parasitT3d11vector,parasitT3D11vectorNAMES)
te<-as.vector(tapply(parasitMeanT3D11$parasitT3d11vector,parasitMeanT3D11$parasitT3D11vectorNAMES,mean))
T3d11PARAmean<-c(te[4:12],te[1:3])

parasitemT3d11<-parasitemUPPT3d11<-parasitemLOWT3d11<-numeric(15)
for (i in 1:15){
  parasitemT3d11[i]<-sum(parasitT3d11[,i])/5
  
  # a<-numeric(10000)
  #  for (j in 1:10000){ 
  
  #   a[j]<-mean(sample(parasitT3d11[,i],4))
  #  parasitemUPPT3d11[i]<-quantile(a,0.975)
  # parasitemLOWT3d11[i]<-quantile(a,0.025)
  #}
}

##MEAN Gametocytemia IN MICE
gametT3d11<-cbind(
  sample(T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 1],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 2],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 3],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 4],5),
  
  sample(T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 1],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 2],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 3],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 4],5),
  
  sample(T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 1],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 2],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 3],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 4],5),
  
  sample(T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 1],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 2],5),
  sample(T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 3],5))

gametT3d11vector<-c(
  T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 1],
  T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 2],
  T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 3],
  #T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 4],
  T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 1],
  T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 2],
  T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 3],
  #T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 4],
  T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 1],
  T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 2],
  T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 3],
  #T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 4],
  T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 1],
  T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 2],
  T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 3])
gamT3D11vectorNAMES<-c(rep("ConB1G1",5),rep("ConB1G2",5),rep("ConB1G3",5),
                       rep("ConB2G1",5),rep("ConB2G2",5),rep("ConB2G3",5),
                       rep("ConB5G1",5),rep("ConB5G2",5),rep("ConB5G3",5),
                       rep("ConB10G1",5),rep("ConB10G2",5),rep("ConB10G3",5))
gamMeanT3D11<-data.frame(gametT3d11vector,gamT3D11vectorNAMES)
te<-as.vector(tapply(gamMeanT3D11$gametT3d11vector,gamMeanT3D11$gamT3D11vectorNAMES,mean))
T3D11GAMmean<-c(te[4:12],te[1:3])

gametoT3d11<-gametoUPPT3d11<-gametoLOWT3d11<-numeric(15)
for (i in 1:15){
  gametoT3d11[i]<-sum(gametT3d11[,i])/5
  #a<-numeric(10000)
  #for (j in 1:10000){ 
  
  #  a[j]<-mean(sample(gametC[,i],4))
  #  gametoUPPC[i]<-quantile(a,0.975)
  #  gametoLOWC[i]<-quantile(a,0.025)
  #}
}

##PREVALENCE IN MICE
T3d11$prevBS<-ifelse(T3d11$Parasitemia > 0 | T3d11$Gametocytemia > 0, 1, 0)
spcount<-expand.grid(seq(1,nrow(T3d11),1))
for(i in 1:nrow(T3d11)){
  for (j in 6:15){
    spcount[i,j-5]<-ifelse(is.na(T3d11[i,j])==FALSE,1,0)}}
for (i in 1:length(T3d11$prevBS)){
  T3d11$sporoCount[i]<-sum(spcount[i,])}
for (i in 1:nrow(T3d11)){
  T3d11$meanpermouse[i]<-sum(T3d11[i,6:15],na.rm=T)/T3d11$sporoCount[i]
}

PREV_T3d11<-cbind(
  sample(T3d11$prevBS[T3d11$Bites==1 & T3d11$Round == 1],5),
  sample(T3d11$prevBS[T3d11$Bites==1 & T3d11$Round == 2],5),
  sample(T3d11$prevBS[T3d11$Bites==1 & T3d11$Round == 3],5),
  sample(T3d11$prevBS[T3d11$Bites==1 & T3d11$Round == 4],5),
  sample(T3d11$prevBS[T3d11$Bites==2 & T3d11$Round == 1],5),
  sample(T3d11$prevBS[T3d11$Bites==2 & T3d11$Round == 2],5),
  sample(T3d11$prevBS[T3d11$Bites==2 & T3d11$Round == 3],5),
  sample(T3d11$prevBS[T3d11$Bites==2 & T3d11$Round == 4],5),
  sample(T3d11$prevBS[T3d11$Bites==5 & T3d11$Round == 1],5),
  sample(T3d11$prevBS[T3d11$Bites==5 & T3d11$Round == 2],5),
  sample(T3d11$prevBS[T3d11$Bites==5 & T3d11$Round == 3],5),
  sample(T3d11$prevBS[T3d11$Bites==5 & T3d11$Round == 4],5),
  sample(T3d11$prevBS[T3d11$Bites==10 & T3d11$Round == 1],5),
  sample(T3d11$prevBS[T3d11$Bites==10 & T3d11$Round == 2],5),
  sample(T3d11$prevBS[T3d11$Bites==10 & T3d11$Round == 3],5))

PREV_T3d11mean<-c(
  mean(T3d11$prevBS[T3d11$Bites==1 & T3d11$Round == 1]),
  mean(T3d11$prevBS[T3d11$Bites==1 & T3d11$Round == 2]),
  mean(T3d11$prevBS[T3d11$Bites==1 & T3d11$Round == 3]),
  mean(T3d11$prevBS[T3d11$Bites==2 & T3d11$Round == 1]),
  mean(T3d11$prevBS[T3d11$Bites==2 & T3d11$Round == 2]),
  mean(T3d11$prevBS[T3d11$Bites==2 & T3d11$Round == 3]),
  mean(T3d11$prevBS[T3d11$Bites==5 & T3d11$Round == 1]),
  mean(T3d11$prevBS[T3d11$Bites==5 & T3d11$Round == 2]),
  mean(T3d11$prevBS[T3d11$Bites==5 & T3d11$Round == 3]),
  mean(T3d11$prevBS[T3d11$Bites==10 & T3d11$Round == 1]),
  mean(T3d11$prevBS[T3d11$Bites==10 & T3d11$Round == 2]),
  mean(T3d11$prevBS[T3d11$Bites==10 & T3d11$Round == 3]))

###Sporozoite intensity
Sporozoite1z<-T3d11$Sporozoite1[T3d11$Round==0 & T3d11$Bites==1]
spb1r0<-c(length(Sporozoite1z[Sporozoite1z==0]),length(Sporozoite1z[Sporozoite1z==1]),
          length(Sporozoite1z[Sporozoite1z==2]),length(Sporozoite1z[Sporozoite1z==3]),
          length(Sporozoite1z[Sporozoite1z==4]))

Sporozoite2z<-c(T3d11$Sporozoite1[T3d11$Round==0 & T3d11$Bites==2],T3d11$Sporozoite2[T3d11$Round==0 & T3d11$Bites==2])
spb2r0<-c(length(Sporozoite2z[Sporozoite2z==0]),length(Sporozoite2z[Sporozoite2z==1]),
          length(Sporozoite2z[Sporozoite2z==2]),length(Sporozoite2z[Sporozoite2z==3]),
          length(Sporozoite2z[Sporozoite2z==4]))

Sporozoite5z<-c(T3d11$Sporozoite1[T3d11$Round==0 & T3d11$Bites==5],T3d11$Sporozoite2[T3d11$Round==0 & T3d11$Bites==5],
                T3d11$Sporozoite3[T3d11$Round==0 & T3d11$Bites==5],T3d11$Sporozoite4[T3d11$Round==0 & T3d11$Bites==5],
                T3d11$Sporozoite5[T3d11$Round==0 & T3d11$Bites==5])
spb5r0<-c(length(Sporozoite5z[Sporozoite5z==0]),length(Sporozoite5z[Sporozoite5z==1]),
          length(Sporozoite5z[Sporozoite5z==2]),length(Sporozoite5z[Sporozoite5z==3]),
          length(Sporozoite5z[Sporozoite5z==4]))

Sporozoite10z<-c(T3d11$Sporozoite1[T3d11$Round==0 & T3d11$Bites==10],T3d11$Sporozoite2[T3d11$Round==0 & T3d11$Bites==10],
                 T3d11$Sporozoite3[T3d11$Round==0 & T3d11$Bites==10],T3d11$Sporozoite4[T3d11$Round==0 & T3d11$Bites==10],
                 T3d11$Sporozoite5[T3d11$Round==0 & T3d11$Bites==10],T3d11$Sporozoite6[T3d11$Round==0 & T3d11$Bites==10],
                 T3d11$Sporozoite7[T3d11$Round==0 & T3d11$Bites==10],T3d11$Sporozoite8[T3d11$Round==0 & T3d11$Bites==10],
                 T3d11$Sporozoite9[T3d11$Round==0 & T3d11$Bites==10],T3d11$Sporozoite10[T3d11$Round==0 & T3d11$Bites==10])
spb10r0<-c(length(Sporozoite10z[Sporozoite10z==0]),length(Sporozoite10z[Sporozoite10z==1]),
           length(Sporozoite10z[Sporozoite10z==2]),length(Sporozoite10z[Sporozoite10z==3]),
           length(Sporozoite10z[Sporozoite10z==4]))
##
Sporozoite1a<-T3d11$Sporozoite1[T3d11$Round==1 & T3d11$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(T3d11$Sporozoite1[T3d11$Round==1 & T3d11$Bites==2],T3d11$Sporozoite2[T3d11$Round==1 & T3d11$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(T3d11$Sporozoite1[T3d11$Round==1 & T3d11$Bites==5],T3d11$Sporozoite2[T3d11$Round==1 & T3d11$Bites==5],
                T3d11$Sporozoite3[T3d11$Round==1 & T3d11$Bites==5],T3d11$Sporozoite4[T3d11$Round==1 & T3d11$Bites==5],
                T3d11$Sporozoite5[T3d11$Round==1 & T3d11$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(T3d11$Sporozoite1[T3d11$Round==1 & T3d11$Bites==10],T3d11$Sporozoite2[T3d11$Round==1 & T3d11$Bites==10],
                 T3d11$Sporozoite3[T3d11$Round==1 & T3d11$Bites==10],T3d11$Sporozoite4[T3d11$Round==1 & T3d11$Bites==10],
                 T3d11$Sporozoite5[T3d11$Round==1 & T3d11$Bites==10],T3d11$Sporozoite6[T3d11$Round==1 & T3d11$Bites==10],
                 T3d11$Sporozoite7[T3d11$Round==1 & T3d11$Bites==10],T3d11$Sporozoite8[T3d11$Round==1 & T3d11$Bites==10],
                 T3d11$Sporozoite9[T3d11$Round==1 & T3d11$Bites==10],T3d11$Sporozoite10[T3d11$Round==1 & T3d11$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
           length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
           length(Sporozoite10a[Sporozoite10a==4]))


Sporozoite1b<-T3d11$Sporozoite1[T3d11$Round==2 & T3d11$Bites==1]
spb1r2<-c(length(Sporozoite1b[Sporozoite1b==0]),length(Sporozoite1b[Sporozoite1b==1]),
          length(Sporozoite1b[Sporozoite1b==2]),length(Sporozoite1b[Sporozoite1b==3]),
          length(Sporozoite1b[Sporozoite1b==4]))

Sporozoite2b<-c(T3d11$Sporozoite1[T3d11$Round==2 & T3d11$Bites==2],T3d11$Sporozoite2[T3d11$Round==2 & T3d11$Bites==2])
spb2r2<-c(length(Sporozoite2b[Sporozoite2b==0]),length(Sporozoite2b[Sporozoite2b==1]),
          length(Sporozoite2b[Sporozoite2b==2]),length(Sporozoite2b[Sporozoite2b==3]),
          length(Sporozoite2b[Sporozoite2b==4]))

Sporozoite5b<-c(T3d11$Sporozoite1[T3d11$Round==2 & T3d11$Bites==5],T3d11$Sporozoite2[T3d11$Round==2 & T3d11$Bites==5],
                T3d11$Sporozoite3[T3d11$Round==2 & T3d11$Bites==5],T3d11$Sporozoite4[T3d11$Round==2 & T3d11$Bites==5],
                T3d11$Sporozoite5[T3d11$Round==2 & T3d11$Bites==5])
spb5r2<-c(length(Sporozoite5b[Sporozoite5b==0]),length(Sporozoite5b[Sporozoite5b==1]),
          length(Sporozoite5b[Sporozoite5b==2]),length(Sporozoite5b[Sporozoite5b==3]),
          length(Sporozoite5b[Sporozoite5b==4]))

Sporozoite10b<-c(T3d11$Sporozoite1[T3d11$Round==2 & T3d11$Bites==10],T3d11$Sporozoite2[T3d11$Round==2 & T3d11$Bites==10],
                 T3d11$Sporozoite3[T3d11$Round==2 & T3d11$Bites==10],T3d11$Sporozoite4[T3d11$Round==2 & T3d11$Bites==10],
                 T3d11$Sporozoite5[T3d11$Round==2 & T3d11$Bites==10],T3d11$Sporozoite6[T3d11$Round==2 & T3d11$Bites==10],
                 T3d11$Sporozoite7[T3d11$Round==2 & T3d11$Bites==10],T3d11$Sporozoite8[T3d11$Round==2 & T3d11$Bites==10],
                 T3d11$Sporozoite9[T3d11$Round==2 & T3d11$Bites==10],T3d11$Sporozoite10[T3d11$Round==2 & T3d11$Bites==10])
spb10r2<-c(length(Sporozoite10b[Sporozoite10b==0]),length(Sporozoite10b[Sporozoite10b==1]),
           length(Sporozoite10b[Sporozoite10b==2]),length(Sporozoite10b[Sporozoite10b==3]),
           length(Sporozoite10b[Sporozoite10b==4]))


Sporozoite1c<-T3d11$Sporozoite1[T3d11$Round==3 & T3d11$Bites==1]
spb1r3<-c(length(Sporozoite1c[Sporozoite1c==0]),length(Sporozoite1c[Sporozoite1c==1]),
          length(Sporozoite1c[Sporozoite1c==2]),length(Sporozoite1c[Sporozoite1c==3]),
          length(Sporozoite1c[Sporozoite1c==4]))

Sporozoite2c<-c(T3d11$Sporozoite1[T3d11$Round==3 & T3d11$Bites==2],T3d11$Sporozoite2[T3d11$Round==3 & T3d11$Bites==2])
spb2r3<-c(length(Sporozoite2c[Sporozoite2c==0]),length(Sporozoite2c[Sporozoite2c==1]),
          length(Sporozoite2c[Sporozoite2c==2]),length(Sporozoite2c[Sporozoite2c==3]),
          length(Sporozoite2c[Sporozoite2c==4]))

Sporozoite5c<-c(T3d11$Sporozoite1[T3d11$Round==3 & T3d11$Bites==5],T3d11$Sporozoite2[T3d11$Round==3 & T3d11$Bites==5],
                T3d11$Sporozoite3[T3d11$Round==3 & T3d11$Bites==5],T3d11$Sporozoite4[T3d11$Round==3 & T3d11$Bites==5],
                T3d11$Sporozoite5[T3d11$Round==3 & T3d11$Bites==5])
spb5r3<-c(length(Sporozoite5c[Sporozoite5c==0]),length(Sporozoite5c[Sporozoite5c==1]),
          length(Sporozoite5c[Sporozoite5c==2]),length(Sporozoite5c[Sporozoite5c==3]),
          length(Sporozoite5c[Sporozoite5c==4]))

Sporozoite10c<-c(T3d11$Sporozoite1[T3d11$Round==3 & T3d11$Bites==10],T3d11$Sporozoite2[T3d11$Round==3 & T3d11$Bites==10],
                 T3d11$Sporozoite3[T3d11$Round==3 & T3d11$Bites==10],T3d11$Sporozoite4[T3d11$Round==3 & T3d11$Bites==10],
                 T3d11$Sporozoite5[T3d11$Round==3 & T3d11$Bites==10],T3d11$Sporozoite6[T3d11$Round==3 & T3d11$Bites==10],
                 T3d11$Sporozoite7[T3d11$Round==3 & T3d11$Bites==10],T3d11$Sporozoite8[T3d11$Round==3 & T3d11$Bites==10],
                 T3d11$Sporozoite9[T3d11$Round==3 & T3d11$Bites==10],T3d11$Sporozoite10[T3d11$Round==3 & T3d11$Bites==10])
spb10r3<-c(length(Sporozoite10c[Sporozoite10c==0]),length(Sporozoite10c[Sporozoite10c==1]),
           length(Sporozoite10c[Sporozoite10c==2]),length(Sporozoite10c[Sporozoite10c==3]),
           length(Sporozoite10c[Sporozoite10c==4]))


Sporozoite1d<-T3d11$Sporozoite1[T3d11$Round==4 & T3d11$Bites==1]
spb1r4<-c(length(Sporozoite1d[Sporozoite1d==0]),length(Sporozoite1d[Sporozoite1d==1]),
          length(Sporozoite1d[Sporozoite1d==2]),length(Sporozoite1d[Sporozoite1d==3]),
          length(Sporozoite1d[Sporozoite1d==4]))

Sporozoite2d<-c(T3d11$Sporozoite1[T3d11$Round==4 & T3d11$Bites==2],T3d11$Sporozoite2[T3d11$Round==4 & T3d11$Bites==2])
spb2r4<-c(length(Sporozoite2d[Sporozoite2d==0]),length(Sporozoite2d[Sporozoite2d==1]),
          length(Sporozoite2d[Sporozoite2d==2]),length(Sporozoite2d[Sporozoite2d==3]),
          length(Sporozoite2d[Sporozoite2d==4]))

Sporozoite5d<-c(T3d11$Sporozoite1[T3d11$Round==4 & T3d11$Bites==5],T3d11$Sporozoite2[T3d11$Round==4 & T3d11$Bites==5],
                T3d11$Sporozoite3[T3d11$Round==4 & T3d11$Bites==5],T3d11$Sporozoite4[T3d11$Round==4 & T3d11$Bites==5],
                T3d11$Sporozoite5[T3d11$Round==4 & T3d11$Bites==5])
spb5r4<-c(length(Sporozoite5d[Sporozoite5d==0]),length(Sporozoite5d[Sporozoite5d==1]),
          length(Sporozoite5d[Sporozoite5d==2]),length(Sporozoite5d[Sporozoite5d==3]),
          length(Sporozoite5d[Sporozoite5d==4]))

Sporozoite10d<-c(T3d11$Sporozoite1[T3d11$Round==4 & T3d11$Bites==10],T3d11$Sporozoite2[T3d11$Round==4 & T3d11$Bites==10],
                T3d11$Sporozoite3[T3d11$Round==4 & T3d11$Bites==10],T3d11$Sporozoite4[T3d11$Round==4 & T3d11$Bites==10],
                T3d11$Sporozoite5[T3d11$Round==4 & T3d11$Bites==10],T3d11$Sporozoite6[T3d11$Round==4 & T3d11$Bites==10],
                T3d11$Sporozoite7[T3d11$Round==4 & T3d11$Bites==10],T3d11$Sporozoite8[T3d11$Round==4 & T3d11$Bites==10],
                T3d11$Sporozoite9[T3d11$Round==4 & T3d11$Bites==10],T3d11$Sporozoite10[T3d11$Round==4 & T3d11$Bites==10])
spb10r4<-c(length(Sporozoite10d[Sporozoite10d==0]),length(Sporozoite10d[Sporozoite10d==1]),
          length(Sporozoite10d[Sporozoite10d==2]),length(Sporozoite10d[Sporozoite10d==3]),
          length(Sporozoite10d[Sporozoite10d==4]))

T3d11_C<-rbind(spb1r0,spb1r1,spb1r2,spb1r3,
               spb2r0,spb2r1,spb2r2,spb2r3,
               spb5r0,spb5r1,spb5r2,spb5r3,
               spb10r0,spb10r1,spb10r2,spb10r3)

MEANspT3d11<-c(
  mean(Sporozoite1a,na.rm=T),mean(Sporozoite1b,na.rm=T),mean(Sporozoite1c,na.rm=T),#mean(Sporozoite1d,na.rm=T),
  mean(Sporozoite2a,na.rm=T),mean(Sporozoite2b,na.rm=T),mean(Sporozoite2c,na.rm=T),#mean(Sporozoite1d,na.rm=T),
  mean(Sporozoite5a,na.rm=T),mean(Sporozoite5b,na.rm=T),mean(Sporozoite5c,na.rm=T),#mean(Sporozoite1d,na.rm=T),
  mean(Sporozoite10a,na.rm=T),mean(Sporozoite10b,na.rm=T),mean(Sporozoite10c,na.rm=T))


MEANspT3d11_2<-c(
  mean(c(Sporozoite1a,Sporozoite1b,Sporozoite1c),na.rm=T),
  mean(c(Sporozoite2a,Sporozoite2b,Sporozoite2c),na.rm=T),
  mean(c(Sporozoite5a,Sporozoite5b,Sporozoite5c),na.rm=T),
  mean(c(Sporozoite10a,Sporozoite10b,Sporozoite10c),na.rm=T))

points(c(0,mean(T3d11$Parasitemia[T3d11$Bites==1]),
         mean(T3d11$Parasitemia[T3d11$Bites==2]),
         mean(T3d11$Parasitemia[T3d11$Bites==5]),
         mean(T3d11$Parasitemia[T3d11$Bites==10]))~c(0,MEANspT3d11_2),
       ylab="Parasitemia (%)", xlab="Sporozoite Score",pch=20)
abline(lm(c(0,mean(T3d11$Parasitemia[T3d11$Bites==1]),
            mean(T3d11$Parasitemia[T3d11$Bites==2]),
            mean(T3d11$Parasitemia[T3d11$Bites==5]),
            mean(T3d11$Parasitemia[T3d11$Bites==10]))~c(0,MEANspT3d11_2)+0),lty=4)

##################################
## 1.8 3D11 & 4B7-85%

##1.8 Oocysts 
t3d11and4B785<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 35ATV-85\\mosquito.txt",header=TRUE)
t3d11and4B785$OocPrev<-ifelse(t3d11and4B785$Oocyst==0,0,1)
head(t3d11and4B785)


##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_t3d11and4B785<-c(
  sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 0 & t3d11and4B785$Round == 0],45),sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 1 & t3d11and4B785$Round == 1],45),rep(0,45*2),
  sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 0 & t3d11and4B785$Round == 0],45),sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 2 & t3d11and4B785$Round == 1],45),rep(0,45*2),
  sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 0 & t3d11and4B785$Round == 0],45),sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 5 & t3d11and4B785$Round == 1],45),rep(0,45*2),
  sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 0 & t3d11and4B785$Round == 0],45),sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 10 & t3d11and4B785$Round == 1],45),rep(0,45*2))

length(oocysts_t3d11and4B785)
prevooc<-ifelse(oocysts_t3d11and4B785==0,0,1)
prevooc3d11and4b785<-ifelse(oocysts_t3d11and4B785==0,0,1)
prevooc13D11AND4b785<-c(sum(prevooc[1:45])/45,
                        sum(prevooc[46:90])/45,
                        sum(prevooc[91:135])/45,
                        sum(prevooc[136:180])/45)


meanoocysts_t3d11and4B785<-c(mean(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 1 & t3d11and4B785$Round == 2]),
                             mean(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 2 & t3d11and4B785$Round == 2]),
                             mean(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 5 & t3d11and4B785$Round == 2]),
                             mean(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 10 & t3d11andATV85$Round == 2]))


##1.8 Sporozoites, parasitemia and Gametocytemia 
sporst3d11and4B785<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 35ATV-85\\mouse.txt",header=TRUE)

##MEAN PARASITEMIA IN MICE
parasitt3d11and4B785<-cbind(
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==1 & sporst3d11and4B785$Round == 0],
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==1 & sporst3d11and4B785$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==2 & sporst3d11and4B785$Round == 0],
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==2 & sporst3d11and4B785$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==5 & sporst3d11and4B785$Round == 0],
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==5 & sporst3d11and4B785$Round == 1],rep(0,5),rep(0,5),rep(0,5),
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==10 & sporst3d11and4B785$Round == 0],
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==10 & sporst3d11and4B785$Round == 1],rep(0,5),rep(0,5),rep(0,5))

newparasitt3d11and4B785<-t(parasitt3d11and4B785)
parasitemt3d11and4B785<-parasitemUPPt3d11and4B785<-parasitemLOWt3d11and4B785<-numeric(4)
for (i in 1:4){
  parasitemt3d11and4B785[i]<-sum(parasitt3d11and4B785[,i])/5
  
  # a<-numeric(10000)
  #  for (j in 1:10000){ 
  
  #   a[j]<-mean(sample(parasitt3d11and4B785[,i],4))
  #  parasitemUPPt3d11and4B785[i]<-quantile(a,0.975)
  # parasitemLOWt3d11and4B785[i]<-quantile(a,0.025)
  #}
}

##MEAN Gametocytemia IN MICE
gamett3d11and4B785<-cbind(
  sample(sporst3d11and4B785$Gametocytemia[sporst3d11and4B785$Bites==1 & sporst3d11and4B785$Round == 1],5),
  sample(sporst3d11and4B785$Gametocytemia[sporst3d11and4B785$Bites==2 & sporst3d11and4B785$Round == 1],5),
  sample(sporst3d11and4B785$Gametocytemia[sporst3d11and4B785$Bites==5 & sporst3d11and4B785$Round == 1],5),
  sample(sporst3d11and4B785$Gametocytemia[sporst3d11and4B785$Bites==10 & sporst3d11and4B785$Round == 1],5))
gametot3d11and4B785<-gametoUPPt3d11and4B785<-gametoLOWt3d11and4B785<-numeric(4)
for (i in 1:4){
  gametot3d11and4B785[i]<-sum(gamett3d11and4B785[,i])/5
  #a<-numeric(10000)
  #for (j in 1:10000){ 
  
  #  a[j]<-mean(sample(gamett3d11and4B785[,i],4))
  #  gametoUPPt3d11and4B785[i]<-quantile(a,0.975)
  #  gametoLOWt3d11and4B785[i]<-quantile(a,0.025)
  #}
}

##PREVALENCE IN MICE
sporst3d11and4B785$prevBS<-ifelse(sporst3d11and4B785$Parasitemia > 0 | sporst3d11and4B785$Gametocytemia > 0, 1, 0)

PREV_t3d11and4B785<-cbind(
  sample(sporst3d11and4B785$prevBS[sporst3d11and4B785$Bites==1 & sporst3d11and4B785$Round == 1],5),
  sample(sporst3d11and4B785$prevBS[sporst3d11and4B785$Bites==2 & sporst3d11and4B785$Round == 1],5),
  sample(sporst3d11and4B785$prevBS[sporst3d11and4B785$Bites==5 & sporst3d11and4B785$Round == 1],5),
  sample(sporst3d11and4B785$prevBS[sporst3d11and4B785$Bites==10 & sporst3d11and4B785$Round == 1],5))

###Sporozoite intensity
Sporozoite1z<-sporst3d11and4B785$Sporozoite1[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==1]
spb1r0<-c(length(Sporozoite1z[Sporozoite1z==0]),length(Sporozoite1z[Sporozoite1z==1]),
          length(Sporozoite1z[Sporozoite1z==2]),length(Sporozoite1z[Sporozoite1z==3]),
          length(Sporozoite1z[Sporozoite1z==4]))

Sporozoite2z<-c(sporst3d11and4B785$Sporozoite1[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==2],sporst3d11and4B785$Sporozoite2[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==2])
spb2r0<-c(length(Sporozoite2z[Sporozoite2z==0]),length(Sporozoite2z[Sporozoite2z==1]),
          length(Sporozoite2z[Sporozoite2z==2]),length(Sporozoite2z[Sporozoite2z==3]),
          length(Sporozoite2z[Sporozoite2z==4]))

Sporozoite5z<-c(sporst3d11and4B785$Sporozoite1[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==5],sporst3d11and4B785$Sporozoite2[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==5],
                sporst3d11and4B785$Sporozoite3[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==5],sporst3d11and4B785$Sporozoite4[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==5],
                sporst3d11and4B785$Sporozoite5[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==5])
spb5r0<-c(length(Sporozoite5z[Sporozoite5z==0]),length(Sporozoite5z[Sporozoite5z==1]),
          length(Sporozoite5z[Sporozoite5z==2]),length(Sporozoite5z[Sporozoite5z==3]),
          length(Sporozoite5z[Sporozoite5z==4]))

Sporozoite10z<-c(sporst3d11and4B785$Sporozoite1[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite2[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],
                 sporst3d11and4B785$Sporozoite3[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite4[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],
                 sporst3d11and4B785$Sporozoite5[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite6[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],
                 sporst3d11and4B785$Sporozoite7[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite8[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],
                 sporst3d11and4B785$Sporozoite9[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite10[sporst3d11and4B785$Round==0 & sporst3d11and4B785$Bites==10])
spb10r0<-c(length(Sporozoite10z[Sporozoite10z==0]),length(Sporozoite10z[Sporozoite10z==1]),
           length(Sporozoite10z[Sporozoite10z==2]),length(Sporozoite10z[Sporozoite10z==3]),
           length(Sporozoite10z[Sporozoite10z==4]))
##
Sporozoite1a<-sporst3d11and4B785$Sporozoite1[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(sporst3d11and4B785$Sporozoite1[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==2],sporst3d11and4B785$Sporozoite2[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(sporst3d11and4B785$Sporozoite1[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==5],sporst3d11and4B785$Sporozoite2[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==5],
                sporst3d11and4B785$Sporozoite3[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==5],sporst3d11and4B785$Sporozoite4[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==5],
                sporst3d11and4B785$Sporozoite5[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(sporst3d11and4B785$Sporozoite1[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite2[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],
                 sporst3d11and4B785$Sporozoite3[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite4[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],
                 sporst3d11and4B785$Sporozoite5[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite6[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],
                 sporst3d11and4B785$Sporozoite7[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite8[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],
                 sporst3d11and4B785$Sporozoite9[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10],sporst3d11and4B785$Sporozoite10[sporst3d11and4B785$Round==1 & sporst3d11and4B785$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
           length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
           length(Sporozoite10a[Sporozoite10a==4]))

sporst3d11and4B785<-rbind(spb1r0,spb1r1,rep(0,5),rep(0,5),
                          spb1r0,spb2r1,rep(0,5),rep(0,5),
                          spb1r0,spb5r1,rep(0,5),rep(0,5),
                          spb1r0,spb10r1,rep(0,5),rep(0,5))

MEANspt3d11and4B785<-c(
  mean(Sporozoite1a,na.rm=T),
  mean(Sporozoite2a,na.rm=T),
  mean(Sporozoite5a,na.rm=T),
  mean(Sporozoite10a,na.rm=T))
#############################################################################
##
## Summary variables
##
#############################################################################
tempOOCYSTS<-c(meanoocystsC[1:3],meanoocystsC[5:7],meanoocystsC[9:11],  ##controls (bites 1,2,5 and G1-3)
               meanoocysts_ATV25,                                       ##ATV25 (bites 1,2,5,10 and G1-3) 
               meanoocysts_ATV50,                                       ##ATV50 (bites 1,2,5,10 and G1-3)
               meanoocystsT3d11[1:3],meanoocystsT3d11[5:7],meanoocystsT3d11[9:11],meanoocystsT3d11[13:15])   ##T3d11 (bites 1,2,5,10 and G1-3)
                                                                        ##other treatments so effective that data not used as zero)
tempSPORS<-c(MEANsp[1:9], ##dropping 10 bites for controls as useless at the moment...
             MEANspATV25,MEANspATV50,MEANspT3d11)

tempPARA<-c(contPARAmean,ATV25PARAmean,ATV50PARAmean,T3d11PARAmean)


tempGAMET<-c(contGAMETmean,ATV25GAMmean,ATV50GAMmean,T3D11GAMmean)

tempPREV<-c(PREV_Cmean,PREV_ATV25Mean,PREV_ATV50mean,PREV_T3d11mean)

##for mice
infstat<-c(spors2$prevBS,sporsATV25$prevBS,sporsATV50$prevBS,T3d11$prevBS)
meansporoscore<-c(spors2$meanpermouse,sporsATV25$meanpermouse,sporsATV50$meanpermouse,T3d11$meanpermouse)
mouseparasitemia<-c(spors2$Parasitemia,sporsATV25$Parasitemia,sporsATV50$Parasitemia,T3d11$Parasitemia)
mousegametocytemia<-c(spors2$Gametocytemia,sporsATV25$Gametocytemia,sporsATV50$Gametocytemia,T3d11$Gametocytemia)
###########################################################
## Does parasitemia correlate with gametocytemia?

parasitemiaALL<-c(parasitCvector,parasitATV25vector,parasitATV50vector,parasitT3d11vector)
gametocytemiaALL<-c(gametCvector,gametATV25vector,gametATV50vector,gametT3d11vector)

corr<-data.frame(parasitemiaALL,gametocytemiaALL)
par(las=2)
plot(corr$gametocytemiaALL~corr$parasitemiaALL,
     ylab="Percentage gametocytemia",
     xlab="Percentage parasitemia",xaxt="n")
abline(lm(gametocytemiaALL~parasitemiaALL))
par(las=1);axis(1,at=c(0,10,20,30))
summary(lm(gametocytemiaALL~parasitemiaALL+0))


##############################################################
##
## 2.1 Effect size on prevalence for each treatment
##
##OOCYSTS
EffOocATV25<-(prevooc1-prevooc1ATV)/prevooc1
EffOocATV50<-(prevooc1-prevooc1ATV50)/prevooc1
EffOocATV85<-(prevooc1-prevooc1ATV85)/prevooc1
EffOoc4B785<-(prevooc1-prevooc1t4B785)/prevooc1

EffOoc3d11<-(prevooc1-prevooc1T3d11)/prevooc1
EffOoc3D11ANDatv85<-(prevooc1-prevooc13D11ANDatv85)/prevooc1
EffOoc3D11AND4b785<-(prevooc1-prevooc13D11AND4b785)/prevooc1


##
##SPOROZOITES

spors_C_3ROUNDS<-rbind(spors_C[1:3,],spors_C[5:7,],spors_C[9:11,],spors_C[13:15,])
T3d11_C2<-rbind(T3d11_C[1:3,],T3d11_C[5:7,],T3d11_C[9:11,],T3d11_C[13:15,])

spors_Cprev<-spors_TATV25prev<-spors_TATV50prev<-spors_T3d11prev<-numeric(12)
for(i in 1:12){
  spors_Cprev[i]<-1-(spors_C_3ROUNDS[i,1]/sum(spors_C_3ROUNDS[i,]))
  spors_TATV25prev[i]<-1-(sporsATV25[i,1]/sum(sporsATV25[i,]))
  spors_TATV50prev[i]<-1-(sporsATV50[i,1]/sum(sporsATV50[i,]))
  spors_T3d11prev[i]<-1-(T3d11_C2[i,1]/sum(T3d11_C2[i,]))
}
effectSporsATV25<-(spors_Cprev-spors_TATV25prev)/spors_Cprev
effectSporsATV50<-(spors_Cprev-spors_TATV50prev)/spors_Cprev
effectSpors3d11<-(spors_Cprev-spors_T3d11prev)/spors_Cprev

spors_C_1ROUND<-rbind(spors_C[1,],spors_C[5,],spors_C[9,],spors_C[13,])
spors_Cprev2<-spors_TATV85prev<-spors_T4B785prev<-
  spors_T3d11andATV85prev<-spors_T3d11and4b785prev<-numeric(4)
for(i in 1:4){
  spors_Cprev2[i]<-1-(spors_C_1ROUND[i,1]/sum(spors_C_1ROUND[i,]))
  spors_TATV85prev[i]<-1-(sporsATV85[i,1]/sum(sporsATV85[i,]))
  spors_T4B785prev[i]<-1-(sporst4B785[i,1]/sum(sporst4B785[i,]))
  spors_T3d11andATV85prev[i]<-1-(sporst3d11andATV85[i,1]/sum(sporst3d11andATV85[i,]))
  spors_T3d11and4b785prev[i]<-1-(sporst3d11and4B785[i,1]/sum(sporst3d11and4B785[i,]))
}
effectSporsATV85<-(spors_Cprev2-spors_TATV85prev)/spors_Cprev2
effectSpors4B785<-(spors_Cprev2-spors_T4B785prev)/spors_Cprev2
effectSpors3d11andATV85prev<-(spors_Cprev2-spors_T3d11andATV85prev)/spors_Cprev2
effectSpors3d11and4b785prev<-(spors_Cprev2-spors_T3d11and4b785prev)/spors_Cprev2

############################################################
##
## 3. Prepare lists of data for stan model
##
##
#############################################################

###
### Organise the data for the initial and final parasitemia and translate to parasite density
##assume that an infection with 30% parasitemia results in death
parasitATV25[4,1] <- parasitATV25[4,6] <- parasitATV25[4,11]  <- parasitATV25[4,16]  <- 30
parasitATV50[3:5,1] <- parasitATV50[3:5,6] <- parasitATV50[3:5,11]  <- parasitATV50[3:5,16] <- 30
parasitt4B785[5,1] <- parasitt4B785[5,6] <- parasitt4B785[5,11] <- parasitt4B785[5,16] <- 30

parasitT3d11[3:5,1] <-  parasitT3d11[3:5,6] <- parasitT3d11[3:5,11] <- parasitT3d11[3:5,16] <- 30     
parasitt3d11andATV85[2:5,1] <- parasitt3d11andATV85[2:5,6] <- parasitt3d11andATV85[2:5,11] <- parasitt3d11andATV85[2:5,16] <- 30
parasitt3d11and4B785[2:5,1] <- parasitt3d11and4B785[2:5,6] <- parasitt3d11and4B785[2:5,11] <- parasitt3d11and4B785[2:5,16] <- 30

parasit2_C <- round(parasitC*1200)
parasit2_T_ATV25 <- round(parasitATV25*1200)
parasit2_T_ATV50 <- round(parasitATV50*1200)
parasit2_T_ATV85 <- round(parasitATV85*1200)
parasit2_T_4B785 <- round(parasitt4B785*1200)
parasit2_T_3D11_50 <- round(parasitT3d11*1200)
parasit2_T_ATV85_3D11_50 <- round(parasitt3d11andATV85*1200)
parasit2_T_4B785_3D11_50 <- round(parasitt3d11and4B785*1200)

para_intC <- structure(.Data =c( parasit2_C[,1:4],
                                 parasit2_C[,6:9],
                                 parasit2_C[,11:14],
                                 parasit2_C[,16:19]),.Dim=c(5,16))

para_intT_ATV25 <- structure(.Data =c(parasit2_T_ATV25[,1:4],
                                      parasit2_T_ATV25[,6:9],
                                      parasit2_T_ATV25[,11:14],
                                      parasit2_T_ATV25[,16:19]),.Dim=c(5,16))

para_intT_ATV50 <- structure(.Data =c(parasit2_T_ATV50[,1:4],
                                      parasit2_T_ATV50[,6:9],
                                      parasit2_T_ATV50[,11:14],
                                      parasit2_T_ATV50[,16:19]),.Dim=c(5,16))

para_intT_ATV85 <- structure(.Data =c(parasit2_T_ATV85[,1:4],
                                      parasit2_T_ATV85[,6:9],
                                      parasit2_T_ATV85[,11:14],
                                      parasit2_T_ATV85[,16:19]),.Dim=c(5,16))

para_intT_4B785 <- structure(.Data =c(parasit2_T_4B785[,1:4],
                                      parasit2_T_4B785[,6:9],
                                      parasit2_T_4B785[,11:14],
                                      parasit2_T_4B785[,16:19]),.Dim=c(5,16))

para_intT_3D11_50 <- structure(.Data =c(parasit2_T_3D11_50[,1:4],
                                      parasit2_T_3D11_50[,6:9],
                                      parasit2_T_3D11_50[,11:14],
                                      parasit2_T_3D11_50[,16:19]),.Dim=c(5,16))

para_intT_ATV85_3D11_50 <- structure(.Data =c(parasit2_T_ATV85_3D11_50[,1:4],
                                      parasit2_T_ATV85_3D11_50[,6:9],
                                      parasit2_T_ATV85_3D11_50[,11:14],
                                      parasit2_T_ATV85_3D11_50[,16:19]),.Dim=c(5,16))

para_intT_4B785_3D11_50 <- structure(.Data =c(parasit2_T_4B785_3D11_50[,1:4],
                                      parasit2_T_4B785_3D11_50[,6:9],
                                      parasit2_T_4B785_3D11_50[,11:14],
                                      parasit2_T_4B785_3D11_50[,16:19]),.Dim=c(5,16))


para_endC <- structure(.Data =c( parasit2_C[,2:5],
                                 parasit2_C[,7:10],
                                 parasit2_C[,12:15],
                                 parasit2_C[,17:20]),.Dim=c(5,16))

para_endT_ATV25 <- structure(.Data =c(parasit2_T_ATV25[,2:5],
                                      parasit2_T_ATV25[,7:10],
                                      parasit2_T_ATV25[,12:15],
                                      parasit2_T_ATV25[,17:20]),.Dim=c(5,16))

para_endT_ATV50 <- structure(.Data =c(parasit2_T_ATV50[,2:5],
                                      parasit2_T_ATV50[,7:10],
                                      parasit2_T_ATV50[,12:15],
                                      parasit2_T_ATV50[,17:20]),.Dim=c(5,16))

para_endT_ATV85 <- structure(.Data =c(parasit2_T_ATV85[,2:5],
                                      parasit2_T_ATV85[,7:10],
                                      parasit2_T_ATV85[,12:15],
                                      parasit2_T_ATV85[,17:20]),.Dim=c(5,16))

para_endT_4B785 <- structure(.Data =c(parasit2_T_4B785[,2:5],
                                      parasit2_T_4B785[,7:10],
                                      parasit2_T_4B785[,12:15],
                                      parasit2_T_4B785[,17:20]),.Dim=c(5,16))

para_endT_3D11_50 <- structure(.Data =c(parasit2_T_3D11_50[,2:5],
                                        parasit2_T_3D11_50[,7:10],
                                        parasit2_T_3D11_50[,12:15],
                                        parasit2_T_3D11_50[,17:20]),.Dim=c(5,16))

para_endT_ATV85_3D11_50 <- structure(.Data =c(parasit2_T_ATV85_3D11_50[,2:5],
                                              parasit2_T_ATV85_3D11_50[,7:10],
                                              parasit2_T_ATV85_3D11_50[,12:15],
                                              parasit2_T_ATV85_3D11_50[,17:20]),.Dim=c(5,16))

para_endT_4B785_3D11_50 <- structure(.Data =c(parasit2_T_4B785_3D11_50[,2:5],
                                              parasit2_T_4B785_3D11_50[,7:10],
                                              parasit2_T_4B785_3D11_50[,12:15],
                                              parasit2_T_4B785_3D11_50[,17:20]),.Dim=c(5,16))

ooc_count_C_temp = structure(.Data = c(oocystsC),.Dim=c(45,16)),   ## FROM THE NEW DATA
ooc_count_T_ATV25_temp = structure(.Data = c(oocysts_ATV25),.Dim=c(45,16))  
ooc_count_T_ATV50_temp = structure(.Data = c(oocysts_ATV50),.Dim=c(45,16))  
ooc_count_T_ATV85_temp = structure(.Data = c(oocysts_ATV85),.Dim=c(45,16))  
ooc_count_T_4B785_temp = structure(.Data = c(oocysts_t4B785),.Dim=c(45,16))
ooc_count_T_3D11_50_temp = structure(.Data = c(oocystsT3d11),.Dim=c(45,16))
ooc_count_T_ATV85_3D11_50_temp = structure(.Data = c(oocysts_t3d11andATV85),.Dim=c(45,16))  
ooc_count_T_4B785_3D11_50_temp = structure(.Data = c(oocysts_t3d11and4B785),.Dim=c(45,16)) 

Alldata <- list(
    N_bin = 5,
    bin_edge = c(0, 1, 10, 100, 1000, 1002),
    
    N_mice = 5,
    N_bite = 4,  ## 1, 2, 5 and 10 bites
    N_round = 4, ## 4 made up with zero's where necessary
    
    bite_C = c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
    bite_T_ATV25 = c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
    bite_T_ATV50 = c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
    bite_T_ATV85 = c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
    bite_T_4B785 = c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
    bite_T_3D11_50 = c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
    bite_T_ATV85_3D11_50 = c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
    bite_T_4B785_3D11_50 = c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
    
    round_C = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    round_T_ATV25 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    round_T_ATV50 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    round_T_ATV85 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    round_T_4B785 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    round_T_3D11_50 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    round_T_ATV85_3D11_50 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    round_T_4B785_3D11_50 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
    
    N_ooc = 45,

    N_C = 16,
    N_T_ATV25 = 16,
    N_T_ATV50 = 16,
    N_T_ATV85 = 16,
    N_T_4B785 = 16,
    N_T_3D11_50 = 16,
    N_T_ATV85_3D11_50 = 16,
    N_T_4B785_3D11_50 = 16,

    para_init_count_C = structure(.Data = c(para_intC),.Dim=c(16,5)), 
    para_init_count_T_ATV25 = structure(.Data = c(para_intT_ATV25),.Dim=c(16,5)),
    para_init_count_T_ATV50 = structure(.Data = c(para_intT_ATV50),.Dim=c(16,5)),
    para_init_count_T_ATV85 = structure(.Data = c(para_intT_ATV85),.Dim=c(16,5)),
    para_init_count_T_4B785 = structure(.Data = c(para_intT_4B785),.Dim=c(16,5)),
    para_init_count_T_3D11_50 = structure(.Data = c(para_intT_3D11_50),.Dim=c(16,5)),
    para_init_count_T_ATV85_3D11_50 = structure(.Data = c(para_intT_ATV85_3D11_50),.Dim=c(16,5)),
    para_init_count_T_4B785_3D11_50 = structure(.Data = c(para_intT_4B785_3D11_50),.Dim=c(16,5)),
    
    ooc_count_C = t(ooc_count_C_temp),   ## FROM THE NEW DATA
    ooc_count_T_ATV25 = t(ooc_count_T_ATV25_temp),  
    ooc_count_T_ATV50 = t(ooc_count_T_ATV50_temp),  
    ooc_count_T_ATV85 = t(ooc_count_T_ATV85_temp),  
    ooc_count_T_4B785 = t(ooc_count_T_4B785_temp),
    ooc_count_T_3D11_50 = t(ooc_count_T_3D11_50_temp),
    ooc_count_T_ATV85_3D11_50 = t(ooc_count_T_ATV85_3D11_50_temp),  
    ooc_count_T_4B785_3D11_50 = t(ooc_count_T_4B785_3D11_50_temp),  
    
    sporo_count_C = structure(.Data=spors2_C,.Dim=c(16,5)),
    sporo_count_T_ATV25 = structure(.Data=bsporsATV25,.Dim=c(16,5)),
    sporo_count_T_ATV50 = structure(.Data=bsporsATV50,.Dim=c(16,5)),
    sporo_count_T_ATV85 = structure(.Data=sporsATV85,.Dim=c(16,5)),
    sporo_count_T_4B785 = structure(.Data=spors2t4B785,.Dim=c(16,5)),
    sporo_count_T_3D11_50 = structure(.Data=T3d11_C,.Dim=c(16,5)),
    sporo_count_T_ATV85_3D11_50 = structure(.Data=sporst3d11andATV85,.Dim=c(16,5)),
    sporo_count_T_4B785_3D11_50 = structure(.Data=sporst3d11and4B785,.Dim=c(16,5)),
        
    para_end_count_C = structure(.Data = c(para_endC),.Dim=c(16,5)),
    para_end_count_T_ATV25 = structure(.Data = c(para_endT_ATV25),.Dim=c(16,5)),
    para_end_count_T_ATV50 = structure(.Data = c(para_endT_ATV50),.Dim=c(16,5)),
    para_end_count_T_ATV85 = structure(.Data = c(para_endT_ATV85),.Dim=c(16,5)),
    para_end_count_T_4B785 = structure(.Data = c(para_endT_4B785),.Dim=c(16,5)),
    para_end_count_T_3D11_50 = structure(.Data = c(para_endT_3D11_50),.Dim=c(16,5)),
    para_end_count_T_ATV85_3D11_50 = structure(.Data = c(para_endT_ATV85_3D11_50),.Dim=c(16,5)),
    para_end_count_T_4B785_3D11_50 = structure(.Data = c(para_endT_4B785_3D11_50),.Dim=c(16,5))
    
)

stan_rdump(ls(Alldata), "ElliedataMousetoMouse_Alldata_dec2015.R", envir=list2env(Alldata))

test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\TBI_testing_model\\mouse_to_mouse\\mouse_to_mouse_extended to fit to all experimental data.stan", data=Alldata,
              iter=1000, chains=4)

###############
##
## All tb compared
##

ooc_count_C_temp = structure(.Data = c(oocystsC),.Dim=c(45,16))   ## FROM THE NEW DATA
ooc_count_T_ATV25_temp = structure(.Data = c(oocysts_ATV25),.Dim=c(45,16))  
ooc_count_T_ATV50_temp = structure(.Data = c(oocysts_ATV50),.Dim=c(45,16))  
ooc_count_T_ATV85_temp = structure(.Data = c(oocysts_ATV85),.Dim=c(45,16))  
ooc_count_T_4B785_temp = structure(.Data = c(oocysts_t4B785),.Dim=c(45,16))

ooc_count_C = t(ooc_count_C_temp)
ooc_count_T_ATV25 = t(ooc_count_T_ATV25_temp)
ooc_count_T_ATV50 = t(ooc_count_T_ATV50_temp)
ooc_count_T_ATV85 = t(ooc_count_T_ATV85_temp)
ooc_count_T_4B785 = t(ooc_count_T_4B785_temp)

TB_data<- list(
  N_bin = 5,
  bin_edge = c(0, 1, 10, 100, 1000, 1002),
  
  N_mice = 5,
  N_bite = 4,  ## 1, 2, 5 and 10 bites
  N_round = 4, ## 4 made up with zero's where necessary
  
  bite_C = rep(c(1,2,3,4),each=4),#c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
  bite_T_ATV25 = rep(c(1,2,3,4),each=4),#c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
  bite_T_ATV50 = rep(c(1,2,3,4),each=4),#c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
  bite_T_ATV85 = rep(c(1,2,3,4),each=4),#c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
  bite_T_4B785 = rep(c(1,2,3,4),each=4),#c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
  
  round_C = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  round_T_ATV25 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  round_T_ATV50 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  round_T_ATV85 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  round_T_4B785 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  
  N_ooc = 45,
  
  N_C = 16,
  N_T_ATV25 = 16,
  N_T_ATV50 = 16,
  N_T_ATV85 = 16,
  N_T_4B785 = 16,
  
  para_init_count_C = t(para_intC), 
  para_init_count_T_ATV25 = t(para_intT_ATV25),
  para_init_count_T_ATV50 = t(para_intT_ATV50),
  para_init_count_T_ATV85 = t(para_intT_ATV85),
  para_init_count_T_4B785 = t(para_intT_4B785),
    
  ooc_count_C = ooc_count_C,   ## FROM THE NEW DATA
  ooc_count_T_ATV25 = ooc_count_T_ATV25,  
  ooc_count_T_ATV50 = ooc_count_T_ATV50,  
  ooc_count_T_ATV85 = ooc_count_T_ATV85,  
  ooc_count_T_4B785 = ooc_count_T_4B785,
    
  sporo_count_C = structure(.Data=spors2_C,.Dim=c(16,5)),
  sporo_count_T_ATV25 = structure(.Data=bsporsATV25,.Dim=c(16,5)),
  sporo_count_T_ATV50 = structure(.Data=bsporsATV50,.Dim=c(16,5)),
  sporo_count_T_ATV85 = structure(.Data=sporsATV85,.Dim=c(16,5)),
  sporo_count_T_4B785 = structure(.Data=spors2t4B785,.Dim=c(16,5)),
    
  para_end_count_C = t(para_endC),
  para_end_count_T_ATV25 = t(para_endT_ATV25),
  para_end_count_T_ATV50 = t(para_endT_ATV50),
  para_end_count_T_ATV85 = t(para_endT_ATV85),
  para_end_count_T_4B785 = t(para_endT_4B785)
    
)

stan_rdump(ls(TB_data), "TB_data.R", envir=list2env(TB_data))
##

##Below works for ATV25
datalisted <- list(
  N_bin = 5,
  bin_edge = c(0, 1, 10, 100, 1000, 1002),
  
  N_mice = 5,
  N_bite = 4,  ## 1, 2, 5 and 10 bites
  N_round = 4, ## 4 made up with zero's where necessary
  
  bite_C = rep(c(1,2,3,4),each=4),#c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
  bite_T = rep(c(1,2,3,4),each=4),#c(1, 1, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 10, 10, 10, 10),
  
  round_C = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  round_T = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  
  N_ooc = 45,
  
  N_C = 16,
  N_T = 16,
  
  para_init_count_C = t(para_intC), 
  para_init_count_T = t(para_intT_ATV85),
  
  ooc_count_C = ooc_count_C,   ## FROM THE NEW DATA
  ooc_count_T = ooc_count_T_ATV85, 
  
  sporo_count_C = structure(.Data=spors2_C,.Dim=c(16,5)),
  sporo_count_T = structure(.Data=bsporsATV85,.Dim=c(16,5)),
  
  para_end_count_C = t(para_endC),
  para_end_count_T = t(para_endT_ATV85)
  
)
test1ATV85 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\TBI_testing_model\\mouse_to_mouse\\mouse_to_mouse.stan", 
              data=datalisted, 
              sample_file = "atv85_output.csv", set_seed = 8675309,
              iter=1000, chains=4)

##write.csv(test1ATV85, file = "test1ATV85.csv")
print(test1ATV85)