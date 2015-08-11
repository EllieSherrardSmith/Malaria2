
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
con<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mosquito.txt",header=TRUE)
con$OocPrev<-ifelse(con$Oocyst==0,0,1)

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocystsC<-c(sample(con$Oocyst[con$Bites == 1 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 1 & con$Round == 2],45),
            sample(con$Oocyst[con$Bites == 1 & con$Round == 3],45),sample(con$Oocyst[con$Bites == 1 & con$Round == 4],45),
            sample(con$Oocyst[con$Bites == 2 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 2 & con$Round == 2],45),
            sample(con$Oocyst[con$Bites == 2 & con$Round == 3],45),sample(con$Oocyst[con$Bites == 2 & con$Round == 4],45),
            sample(con$Oocyst[con$Bites == 5 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 5 & con$Round == 2],45),
            sample(con$Oocyst[con$Bites == 5 & con$Round == 3],45),sample(con$Oocyst[con$Bites == 5 & con$Round == 4],45),
            sample(con$Oocyst[con$Bites == 10 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 10 & con$Round == 2],45),
            sample(con$Oocyst[con$Bites == 10 & con$Round == 3],45))

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
spors<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse3AddingCombinations.txt",header=TRUE)##Or do mouse3AddingCombinations

##MEAN PARASITEMIA IN MICE
parasitC<-cbind(
  sample(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 4],5),
  
  sample(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 4],5),

  sample(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 4],5),
  
  sample(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 3],5))

parasitC_REAL<-cbind(
  c(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 1],rep(0,20)),
  c(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 2],rep(0,20)),
  c(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 3],rep(0,20)),
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 1],
  c(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 2],rep(0,20)),
  c(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 3],rep(0,20)),
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 1],
  c(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 2],rep(0,20)),
  c(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 3],rep(0,20)),
  c(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 1],rep(0,7)),
  c(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 2],rep(0,25)),
  c(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 3],rep(0,25)))

newparaC_REAL<-t(parasitC_REAL)

plot(c(0,mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL"]),
mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL"]),
mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL"]),
mean(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL"]))~c(0,1,2,5,10),
  ylim=c(0,10),xlim=c(0,10),
  ylab="Parasitemia (%)",xlab="Number of Bites")
lines(c(0,mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL"]),
       mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL"]),
       mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL"]),
       mean(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL"]))~c(0,1,2,5,10))

parasitCvector<-c(
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 1],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 2],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 3],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 1],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 2],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 3],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 1],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 2],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 3],
  spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 1],
  spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 2],
  spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 3])

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
  sample(spors$Gametocytemia[spors$Bites==1 & spors$Round == 1],5),
  sample(spors$Gametocytemia[spors$Bites==1 & spors$Round == 2],5),
  sample(spors$Gametocytemia[spors$Bites==1 & spors$Round == 3],5),
  sample(spors$Gametocytemia[spors$Bites==1 & spors$Round == 4],5),
  
  sample(spors$Gametocytemia[spors$Bites==2 & spors$Round == 1],5),
  sample(spors$Gametocytemia[spors$Bites==2 & spors$Round == 2],5),
  sample(spors$Gametocytemia[spors$Bites==2 & spors$Round == 3],5),
  sample(spors$Gametocytemia[spors$Bites==2 & spors$Round == 4],5),
  
  sample(spors$Gametocytemia[spors$Bites==5 & spors$Round == 1],5),
  sample(spors$Gametocytemia[spors$Bites==5 & spors$Round == 2],5),
  sample(spors$Gametocytemia[spors$Bites==5 & spors$Round == 3],5),
  sample(spors$Gametocytemia[spors$Bites==5 & spors$Round == 4],5),
  
  sample(spors$Gametocytemia[spors$Bites==10 & spors$Round == 1],5),
  sample(spors$Gametocytemia[spors$Bites==10 & spors$Round == 2],5),
  sample(spors$Gametocytemia[spors$Bites==10 & spors$Round == 3],5))

gametCvector<-c(
  spors$Gametocytemia[spors$Bites==1 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==1 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==1 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==1 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==2 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==2 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==2 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==2 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==5 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==5 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==5 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==5 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==10 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==10 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==10 & spors$Round == 3])

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
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)

PREV_C<-cbind(
  sample(spors$prevBS[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$prevBS[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$prevBS[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(spors$prevBS[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 4],5),
  sample(spors$prevBS[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$prevBS[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$prevBS[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(spors$prevBS[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 4],5),
  sample(spors$prevBS[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$prevBS[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$prevBS[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(spors$prevBS[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 4],5),
  sample(spors$prevBS[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$prevBS[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$prevBS[spors$Bites==10 & spors$Treatment == "CONTROL" & spors$Round == 3],5))

###Sporozoite intensity
Sporozoite1a<-spors$Sporozoite1[spors$Round==1 & spors$Bites==1]
spb1r1<-c(length(Sporozoite1a[Sporozoite1a==0]),length(Sporozoite1a[Sporozoite1a==1]),
          length(Sporozoite1a[Sporozoite1a==2]),length(Sporozoite1a[Sporozoite1a==3]),
          length(Sporozoite1a[Sporozoite1a==4]))

Sporozoite2a<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==2],spors$Sporozoite2[spors$Round==1 & spors$Bites==2])
spb2r1<-c(length(Sporozoite2a[Sporozoite2a==0]),length(Sporozoite2a[Sporozoite2a==1]),
          length(Sporozoite2a[Sporozoite2a==2]),length(Sporozoite2a[Sporozoite2a==3]),
          length(Sporozoite2a[Sporozoite2a==4]))

Sporozoite5a<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==5],spors$Sporozoite2[spors$Round==1 & spors$Bites==5],
                spors$Sporozoite3[spors$Round==1 & spors$Bites==5],spors$Sporozoite4[spors$Round==1 & spors$Bites==5],
                spors$Sporozoite5[spors$Round==1 & spors$Bites==5])
spb5r1<-c(length(Sporozoite5a[Sporozoite5a==0]),length(Sporozoite5a[Sporozoite5a==1]),
          length(Sporozoite5a[Sporozoite5a==2]),length(Sporozoite5a[Sporozoite5a==3]),
          length(Sporozoite5a[Sporozoite5a==4]))

Sporozoite10a<-c(spors$Sporozoite1[spors$Round==1 & spors$Bites==10],spors$Sporozoite2[spors$Round==1 & spors$Bites==10],
                 spors$Sporozoite3[spors$Round==1 & spors$Bites==10],spors$Sporozoite4[spors$Round==1 & spors$Bites==10],
                 spors$Sporozoite5[spors$Round==1 & spors$Bites==10],spors$Sporozoite6[spors$Round==1 & spors$Bites==10],
                 spors$Sporozoite7[spors$Round==1 & spors$Bites==10],spors$Sporozoite8[spors$Round==1 & spors$Bites==10],
                 spors$Sporozoite9[spors$Round==1 & spors$Bites==10],spors$Sporozoite10[spors$Round==1 & spors$Bites==10])
spb10r1<-c(length(Sporozoite10a[Sporozoite10a==0]),length(Sporozoite10a[Sporozoite10a==1]),
          length(Sporozoite10a[Sporozoite10a==2]),length(Sporozoite10a[Sporozoite10a==3]),
          length(Sporozoite10a[Sporozoite10a==4]))


Sporozoite1b<-spors$Sporozoite1[spors$Round==2 & spors$Bites==1]
spb1r2<-c(length(Sporozoite1b[Sporozoite1b==0]),length(Sporozoite1b[Sporozoite1b==1]),
          length(Sporozoite1b[Sporozoite1b==2]),length(Sporozoite1b[Sporozoite1b==3]),
          length(Sporozoite1b[Sporozoite1b==4]))

Sporozoite2b<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==2],spors$Sporozoite2[spors$Round==2 & spors$Bites==2])
spb2r2<-c(length(Sporozoite2b[Sporozoite2b==0]),length(Sporozoite2b[Sporozoite2b==1]),
          length(Sporozoite2b[Sporozoite2b==2]),length(Sporozoite2b[Sporozoite2b==3]),
          length(Sporozoite2b[Sporozoite2b==4]))

Sporozoite5b<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==5],spors$Sporozoite2[spors$Round==2 & spors$Bites==5],
                spors$Sporozoite3[spors$Round==2 & spors$Bites==5],spors$Sporozoite4[spors$Round==2 & spors$Bites==5],
                spors$Sporozoite5[spors$Round==2 & spors$Bites==5])
spb5r2<-c(length(Sporozoite5b[Sporozoite5b==0]),length(Sporozoite5b[Sporozoite5b==1]),
          length(Sporozoite5b[Sporozoite5b==2]),length(Sporozoite5b[Sporozoite5b==3]),
          length(Sporozoite5b[Sporozoite5b==4]))

Sporozoite10b<-c(spors$Sporozoite1[spors$Round==2 & spors$Bites==10],spors$Sporozoite2[spors$Round==2 & spors$Bites==10],
                 spors$Sporozoite3[spors$Round==2 & spors$Bites==10],spors$Sporozoite4[spors$Round==2 & spors$Bites==10],
                 spors$Sporozoite5[spors$Round==2 & spors$Bites==10],spors$Sporozoite6[spors$Round==2 & spors$Bites==10],
                 spors$Sporozoite7[spors$Round==2 & spors$Bites==10],spors$Sporozoite8[spors$Round==2 & spors$Bites==10],
                 spors$Sporozoite9[spors$Round==2 & spors$Bites==10],spors$Sporozoite10[spors$Round==2 & spors$Bites==10])
spb10r2<-c(length(Sporozoite10b[Sporozoite10b==0]),length(Sporozoite10b[Sporozoite10b==1]),
           length(Sporozoite10b[Sporozoite10b==2]),length(Sporozoite10b[Sporozoite10b==3]),
           length(Sporozoite10b[Sporozoite10b==4]))


Sporozoite1c<-spors$Sporozoite1[spors$Round==3 & spors$Bites==1]
spb1r3<-c(length(Sporozoite1c[Sporozoite1c==0]),length(Sporozoite1c[Sporozoite1c==1]),
          length(Sporozoite1c[Sporozoite1c==2]),length(Sporozoite1c[Sporozoite1c==3]),
          length(Sporozoite1c[Sporozoite1c==4]))

Sporozoite2c<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==2],spors$Sporozoite2[spors$Round==3 & spors$Bites==2])
spb2r3<-c(length(Sporozoite2c[Sporozoite2c==0]),length(Sporozoite2c[Sporozoite2c==1]),
          length(Sporozoite2c[Sporozoite2c==2]),length(Sporozoite2c[Sporozoite2c==3]),
          length(Sporozoite2c[Sporozoite2c==4]))

Sporozoite5c<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==5],spors$Sporozoite2[spors$Round==3 & spors$Bites==5],
                spors$Sporozoite3[spors$Round==3 & spors$Bites==5],spors$Sporozoite4[spors$Round==3 & spors$Bites==5],
                spors$Sporozoite5[spors$Round==3 & spors$Bites==5])
spb5r3<-c(length(Sporozoite5c[Sporozoite5c==0]),length(Sporozoite5c[Sporozoite5c==1]),
          length(Sporozoite5c[Sporozoite5c==2]),length(Sporozoite5c[Sporozoite5c==3]),
          length(Sporozoite5c[Sporozoite5c==4]))

Sporozoite10c<-c(spors$Sporozoite1[spors$Round==3 & spors$Bites==10],spors$Sporozoite2[spors$Round==3 & spors$Bites==10],
                 spors$Sporozoite3[spors$Round==3 & spors$Bites==10],spors$Sporozoite4[spors$Round==3 & spors$Bites==10],
                 spors$Sporozoite5[spors$Round==3 & spors$Bites==10],spors$Sporozoite6[spors$Round==3 & spors$Bites==10],
                 spors$Sporozoite7[spors$Round==3 & spors$Bites==10],spors$Sporozoite8[spors$Round==3 & spors$Bites==10],
                 spors$Sporozoite9[spors$Round==3 & spors$Bites==10],spors$Sporozoite10[spors$Round==3 & spors$Bites==10])
spb10r3<-c(length(Sporozoite10c[Sporozoite10c==0]),length(Sporozoite10c[Sporozoite10c==1]),
           length(Sporozoite10c[Sporozoite10c==2]),length(Sporozoite10c[Sporozoite10c==3]),
           length(Sporozoite10c[Sporozoite10c==4]))


Sporozoite1d<-spors$Sporozoite1[spors$Round==4 & spors$Bites==1]
spb1r4<-c(length(Sporozoite1d[Sporozoite1d==0]),length(Sporozoite1d[Sporozoite1d==1]),
          length(Sporozoite1d[Sporozoite1d==2]),length(Sporozoite1d[Sporozoite1d==3]),
          length(Sporozoite1d[Sporozoite1d==4]))

Sporozoite2d<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==2],spors$Sporozoite2[spors$Round==4 & spors$Bites==2])
spb2r4<-c(length(Sporozoite2d[Sporozoite2d==0]),length(Sporozoite2d[Sporozoite2d==1]),
          length(Sporozoite2d[Sporozoite2d==2]),length(Sporozoite2d[Sporozoite2d==3]),
          length(Sporozoite2d[Sporozoite2d==4]))

Sporozoite5d<-c(spors$Sporozoite1[spors$Round==4 & spors$Bites==5],spors$Sporozoite2[spors$Round==4 & spors$Bites==5],
                spors$Sporozoite3[spors$Round==4 & spors$Bites==5],spors$Sporozoite4[spors$Round==4 & spors$Bites==5],
                spors$Sporozoite5[spors$Round==4 & spors$Bites==5])
spb5r4<-c(length(Sporozoite5d[Sporozoite5d==0]),length(Sporozoite5d[Sporozoite5d==1]),
          length(Sporozoite5d[Sporozoite5d==2]),length(Sporozoite5d[Sporozoite5d==3]),
          length(Sporozoite5d[Sporozoite5d==4]))

spors_C<-rbind(spb1r1,spb1r2,spb1r3,
               spb2r1,spb2r2,spb2r3,
               spb5r1,spb5r2,spb5r3,
               spb10r1,spb10r2,spb10r3)

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

plot(c(0,mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL"]),
       mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL"]),
       mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL"]),
       mean(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL"]))~c(0,MEANsp2),
     ylab="Parasitemia (%)", xlab="Sporozoite Score",ylim=c(0,15),xlim=c(0,3))
abline(lm(c(0,mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == "CONTROL"]),
   mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == "CONTROL"]),
   mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == "CONTROL"]),
   mean(spors$Parasitemia[spors$Bites==10 & spors$Treatment == "CONTROL"]))~c(0,MEANsp2)+0),lty=1)

##################################
## 1.2 ATV-25%

##1.2 Oocysts 
ATV25<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mosquito.txt",header=TRUE)
ATV25$OocPrev<-ifelse(ATV25$Oocyst==0,0,1)

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_ATV25<-c(sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 1],45),sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 2],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 3],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 1],45),sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 2],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 3],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 1],45),sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 2],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 3],45),
                 sample(ATV25$Oocyst[ATV25$Bites == 10 & ATV25$Round == 1],45),sample(ATV25$Oocyst[ATV25$Bites == 10 & ATV25$Round == 2],45),
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

###Sporozoite intensity
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


bsporsATV25<-rbind(spb1r1,spb1r2,spb1r3,
               spb2r1,spb2r2,spb2r3,
               spb5r1,spb5r2,spb5r3,
               spb10r1,spb10r2,spb10r3)

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

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_ATV50<-c(sample(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 1],45),sample(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 2],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 1 & ATV50$Round == 3],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 1],45),sample(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 2],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 2 & ATV50$Round == 3],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 1],45),sample(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 2],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 5 & ATV50$Round == 3],45),
                 sample(ATV50$Oocyst[ATV50$Bites == 10 & ATV50$Round == 1],45),sample(ATV50$Oocyst[ATV50$Bites == 10 & ATV50$Round == 2],45),
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

###Sporozoite intensity
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


bsporsATV50<-rbind(spb1r1,spb1r2,spb1r3,
                  spb2r1,spb2r2,spb2r3,
                  spb5r1,spb5r2,spb5r3,
                  spb10r1,spb10r2,spb10r3)

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

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_ATV85<-c(sample(ATV85$Oocyst[ATV85$Bites == 1 & ATV85$Round == 1],45),
                 sample(ATV85$Oocyst[ATV85$Bites == 2 & ATV85$Round == 1],45),
                 sample(ATV85$Oocyst[ATV85$Bites == 5 & ATV85$Round == 1],45),
                 sample(ATV85$Oocyst[ATV85$Bites == 10 & ATV85$Round == 1],45))

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
    sporsATV85$Parasitemia[sporsATV85$Bites==1 & sporsATV85$Round == 1],
    sporsATV85$Parasitemia[sporsATV85$Bites==2 & sporsATV85$Round == 1],
    sporsATV85$Parasitemia[sporsATV85$Bites==5 & sporsATV85$Round == 1],
    sporsATV85$Parasitemia[sporsATV85$Bites==10 & sporsATV85$Round == 1])

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


sporsATV85<-rbind(spb1r1,spb2r1,spb5r1,spb10r1)

MEANspATV85<-c(mean(Sporozoite1a,na.rm=T),mean(Sporozoite2a,na.rm=T),
               mean(Sporozoite5a,na.rm=T),mean(Sporozoite10a,na.rm=T))

##################################
## 1.5 t4B785

##1.5 Oocysts 
t4B785<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\4B7-85\\mosquito.txt",header=TRUE)
t4B785$OocPrev<-ifelse(t4B785$Oocyst==0,0,1)

##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_t4B785<-c(sample(t4B785$Oocyst[t4B785$Bites == 1 & t4B785$Round == 1],45),
                 sample(t4B785$Oocyst[t4B785$Bites == 2 & t4B785$Round == 1],45),
                 sample(t4B785$Oocyst[t4B785$Bites == 5 & t4B785$Round == 1],45),
                 sample(t4B785$Oocyst[t4B785$Bites == 10 & t4B785$Round == 1],45))

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
  sporst4B785$Parasitemia[sporst4B785$Bites==1 & sporst4B785$Round == 1],
  sporst4B785$Parasitemia[sporst4B785$Bites==2 & sporst4B785$Round == 1],
  sporst4B785$Parasitemia[sporst4B785$Bites==5 & sporst4B785$Round == 1],
  sporst4B785$Parasitemia[sporst4B785$Bites==10 & sporst4B785$Round == 1])

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


sporst4B785<-rbind(spb1r1,spb2r1,spb5r1,spb10r1)

MEANspt4B785<-c(mean(Sporozoite1a,na.rm=T),mean(Sporozoite2a,na.rm=T),
               mean(Sporozoite5a,na.rm=T),mean(Sporozoite10a,na.rm=T))



##################################
## 1.6 3D11 & ATV-85%

##1.6 Oocysts 
t3d11andATV85<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 35ATV-85\\mosquito.txt",header=TRUE)
t3d11andATV85$OocPrev<-ifelse(t3d11andATV85$Oocyst==0,0,1)



##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_t3d11andATV85<-c(
  sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 1 & t3d11andATV85$Round == 1],45),
  sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 2 & t3d11andATV85$Round == 1],45),
  sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 5 & t3d11andATV85$Round == 1],45),
  sample(t3d11andATV85$Oocyst[t3d11andATV85$Bites == 10 & t3d11andATV85$Round == 1],45))

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
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==1 & sporst3d11andATV85$Round == 1],
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==2 & sporst3d11andATV85$Round == 1],
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==5 & sporst3d11andATV85$Round == 1],
  sporst3d11andATV85$Parasitemia[sporst3d11andATV85$Bites==10 & sporst3d11andATV85$Round == 1])

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

sporst3d11andATV85<-rbind(spb1r1,spb2r1,spb5r1,spb10r1)

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
            sample(T3d11$Oocyst[T3d11$Bites == 10 & T3d11$Round == 3],45))

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
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 3])
newparasitT3d11<-t(parasitT3d11)

points(c(0,mean(T3d11$Parasitemia[T3d11$Bites==1]),
        mean(T3d11$Parasitemia[T3d11$Bites==2]),
        mean(T3d11$Parasitemia[T3d11$Bites==5]),
        mean(T3d11$Parasitemia[T3d11$Bites==10]))~c(0,1,2,5,10),lty=3,pch=20)
lines(c(0,mean(T3d11$Parasitemia[T3d11$Bites==1]),
         mean(T3d11$Parasitemia[T3d11$Bites==2]),
         mean(T3d11$Parasitemia[T3d11$Bites==5]),
         mean(T3d11$Parasitemia[T3d11$Bites==10]))~c(0,1,2,5,10),lty=3,pch=20)


parasitT3d11vector<-cbind(
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==1 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==2 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 3],
  T3d11$Parasitemia[T3d11$Bites==5 & T3d11$Round == 4],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 1],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 2],
  T3d11$Parasitemia[T3d11$Bites==10 & T3d11$Round == 3])

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
  T3d11$Gametocytemia[T3d11$Bites==1 & T3d11$Round == 4],
  T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 1],
  T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 2],
  T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 3],
  T3d11$Gametocytemia[T3d11$Bites==2 & T3d11$Round == 4],
  T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 1],
  T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 2],
  T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 3],
  T3d11$Gametocytemia[T3d11$Bites==5 & T3d11$Round == 4],
  T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 1],
  T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 2],
  T3d11$Gametocytemia[T3d11$Bites==10 & T3d11$Round == 3])

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

###Sporozoite intensity
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

T3d11_C<-rbind(spb1r1,spb1r2,spb1r3,spb1r4,
               spb2r1,spb2r2,spb2r3,spb2r4,
               spb5r1,spb5r2,spb5r3,spb5r4,
               spb10r1,spb10r2,spb10r3)

MEANspT3d11<-c(
  mean(Sporozoite1a,na.rm=T),mean(Sporozoite1b,na.rm=T),mean(Sporozoite1c,na.rm=T),mean(Sporozoite1d,na.rm=T),
  mean(Sporozoite2a,na.rm=T),mean(Sporozoite2b,na.rm=T),mean(Sporozoite2c,na.rm=T),mean(Sporozoite1d,na.rm=T),
  mean(Sporozoite5a,na.rm=T),mean(Sporozoite5b,na.rm=T),mean(Sporozoite5c,na.rm=T),mean(Sporozoite1d,na.rm=T),
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



##Minimuum is 45 can change sample number depending on the minimum treatment group
oocysts_t3d11and4B785<-c(
  sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 1 & t3d11and4B785$Round == 1],45),
  sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 2 & t3d11and4B785$Round == 1],45),
  sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 5 & t3d11and4B785$Round == 1],45),
  sample(t3d11and4B785$Oocyst[t3d11and4B785$Bites == 10 & t3d11and4B785$Round == 1],45))

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
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==1 & sporst3d11and4B785$Round == 1],
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==2 & sporst3d11and4B785$Round == 1],
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==5 & sporst3d11and4B785$Round == 1],
  sporst3d11and4B785$Parasitemia[sporst3d11and4B785$Bites==10 & sporst3d11and4B785$Round == 1])

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

sporst3d11and4B785<-rbind(spb1r1,spb2r1,spb5r1,spb10r1)

MEANspt3d11and4B785<-c(
  mean(Sporozoite1a,na.rm=T),
  mean(Sporozoite2a,na.rm=T),
  mean(Sporozoite5a,na.rm=T),
  mean(Sporozoite10a,na.rm=T))

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

###############
##
## ATV 25
##

oocystsC ## THE COMPLETE VECTOR OF CONTROLS, 
         ## EACH GROUP IS 45 UNITS 
         ## GROUPS DEFINED BY ROUND THEN BITE e.g. r1 b1, 
         ##                                        r1 b2, 
         ##                                        r1 b3, etc

oocysts_ATV25 ## 3 ROUNDS AND 4 BITING RATES (1, 2, 5, 10)

oocystsC2 <- c(oocystsC[1:135],oocystsC[181:315],oocystsC[361:495],oocystsC[541:675])

PREV_CNEW <- ifelse(newparaC_REAL == 0, 0, 1)
PREV_TNEW <- ifelse(newparasitATV25 == 0, 0, 1)


PREV_ATV25_data<-list(N_C=12,
            N_T=12,
            N_ooc=45,
            N_mice_C=c(10,10,10,30,10,10,30,10,10,23,5,5),
            max_mice_C=max(c(10,10,10,30,10,10,30,10,10,23,5,5)),
            N_mice_T=5,
            ooc_count_C = structure(.Data = c(oocystsC2),
                                    .Dim=c(45,12)),
            ooc_count_T = structure(.Data = c(oocysts_ATV25),
                                    .Dim=c(45,12)),
            prev_C = structure(.Data =PREV_CNEW,.Dim=c(12,30)),
            prev_T = structure(.Data =PREV_TNEW,.Dim=c(12,5)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,1002),
            sporo_count_C = structure(.Data=spors_C,.Dim=c(12,5)),
            sporo_count_T = structure(.Data=bsporsATV25,.Dim=c(12,5))
  )

stan_rdump(ls(ATV25_data), "ATV25_data_Ellie.R", envir=list2env(ATV25_data))
stan_rdump(ls(PREV_ATV25_data), "PREV_ATV25_data_Ellie.R", envir=list2env(PREV_ATV25_data))
###############
##
## ATV 50
##

oocystsC ## THE COMPLETE VECTOR OF CONTROLS, 
## EACH GROUP IS 45 UNITS 
## GROUPS DEFINED BY ROUND THEN BITE e.g. r1 b1, 
##                                        r1 b2, 
##                                        r1 b3, etc

oocysts_ATV50 ## 3 ROUNDS AND 4 BITING RATES (1, 2, 5, 10)

oocystsC2 <- c(oocystsC[1:135],oocystsC[181:315],oocystsC[361:495],oocystsC[541:675])


PREV_C  ## THE COMPLETE MATRIX
## EACH GROUP IS A COLUMN
## EACH ROW IS A MOUSE


PREV_T2D11<-ifelse(newparasitATV50==0,0,1)
PREV_ATV50_data<-list(N_C=12,
                      N_T=12,
                      N_ooc=45,
                      N_mice_C=c(10,10,10,30,10,10,30,10,10,23,5,5),
                      max_mice_C=max(c(10,10,10,30,10,10,30,10,10,23,5,5)),
                      N_mice_T=5,
                 ooc_count_C = structure(.Data = c(oocystsC2),
                                         .Dim=c(45,12)),
                 ooc_count_T = structure(.Data = c(oocysts_ATV50),
                                         .Dim=c(45,12)),
                 parasitemia_C = structure(.Data =PREV_CNEW,.Dim=c(12,30)),
                 parasitemia_T = structure(.Data =PREV_T2D11,.Dim=c(12,5)),
                 N_bin=5,
                 bin_edge=c(0,1,10,100,1000,1002),
                 sporo_count_C = structure(.Data=spors_C,.Dim=c(12,5)),
                 sporo_count_T = structure(.Data=bsporsATV50,.Dim=c(12,5))
)
stan_rdump(ls(PREV_ATV50_data), "PREV_ATV50_data.R", envir=list2env(PREV_ATV50_data))
#stan_rdump(ls(PARA_ATV50_data), "PARA_ATV50_data_Ellie.R", envir=list2env(PARA_ATV50_data))
###############
##
## 3D11 50%
##

oocystsC ## THE COMPLETE VECTOR OF CONTROLS, 
## EACH GROUP IS 45 UNITS 
## GROUPS DEFINED BY ROUND THEN BITE e.g. r1 b1, 
##                                        r1 b2, 
##                                        r1 b3, etc
oocystsC2 <- c(oocystsC[1:135],oocystsC[181:315],oocystsC[361:495],oocystsC[541:675])


PREV_C  ## THE COMPLETE MATRIX
## EACH GROUP IS A COLUMN
## EACH ROW IS A MOUSE

spors_C_3ROUNDS<-rbind(spors_C[1:3,],spors_C[5:7,],spors_C[9:11,],spors_C[13:15,])

oocystsT3d11_2 <- c(oocystsT3d11[1:135],oocystsT3d11[181:315],oocystsT3d11[361:495],oocystsT3d11[541:675])## Same as controls

T3d11_2<-rbind(T3d11_C[1:3,],T3d11_C[5:7,],T3d11_C[9:11,],T3d11_C[13:15,])

PREV_T3d11_2 <- cbind(PREV_T3d11[,1:3],PREV_T3d11[,5:7],PREV_T3d11[,9:11],PREV_T3d11[,13:15])  ## 12 COLUMNS REPRESENTING 3 ROUNDS, 4 BITE RATES

T3d11_data<-list(N_C=12,
                      N_T=12,
                      N_ooc=45,
                      N_mice_C=c(10,10,10,30,10,10,30,10,10,23,5,5),
                      max_mice_C=max(c(10,10,10,30,10,10,30,10,10,23,5,5)),
                      N_mice_T=5,
                 ooc_count_C = structure(.Data = c(oocystsC2),
                                         .Dim=c(45,12)),
                 ooc_count_T = structure(.Data = c(oocystsT3d11_2),
                                         .Dim=c(45,12)),
                 parasitemia_C = structure(.Data =newparaC_REAL/100,.Dim=c(12,30)),
                 parasitemia_T = structure(.Data =newparasitT3d11/100,.Dim=c(12,5)),
                 N_bin=5,
                 bin_edge=c(0,1,10,100,1000,1002),
                 sporo_count_C = structure(.Data=spors_C_3ROUNDS,.Dim=c(12,5)),
                 sporo_count_T = structure(.Data=T3d11_2,.Dim=c(12,5))
)
stan_rdump(ls(PREV_T3d11_data), "PREV_T3d11_data_Ellie.R", envir=list2env(PREV_T3d11_data))

###############
##
## ATV 85%
##

oocystsC ## THE COMPLETE VECTOR OF CONTROLS, 
## EACH GROUP IS 45 UNITS 
## GROUPS DEFINED BY ROUND THEN BITE e.g. r1 b1, 
##                                        r1 b2, 
##                                        r1 b3, etc

oocysts_ATV85 ## Only Round 1 matched

oocystsC3 <- c(oocystsC[1:45],oocystsC[181:225],oocystsC[361:405],oocystsC[541:585])

PREV_C  ## THE COMPLETE MATRIX
## EACH GROUP IS A COLUMN
## EACH ROW IS A MOUSE
PREVC_3 <- cbind(PREV_C[,1],PREV_C[,5],PREV_C[,9],PREV_C[,13])

PREV_ATV85  ## 12 COLUMNS REPRESENTING 3 ROUNDS, 4 BITE RATES

spors_C_1ROUND<-rbind(spors_C[1,],spors_C[5,],spors_C[9,],spors_C[13,])

sporsATV85

ATV85_data<-list(N_C=4,
                 N_T=4,
                 N_ooc=45,
                 N_mice=5,
                 ooc_count_C = structure(.Data = c(oocystsC3),
                                         .Dim=c(45,4)),
                 ooc_count_T = structure(.Data = c(oocysts_ATV85),
                                         .Dim=c(45,4)),
                 prev_C = structure(.Data =PREVC_3,.Dim=c(5,4)),
                 prev_T = structure(.Data =PREV_ATV85,.Dim=c(5,4)),
                 N_bin=5,
                 bin_edge=c(0,1,10,100,1000,1002),
                 s_count_C = structure(.Data=spors_C_1ROUND,.Dim=c(4,5)),
                 s_count_T = structure(.Data=sporsATV85,.Dim=c(4,5))
)

###############
##
## 4b7 85%
##

oocystsC ## THE COMPLETE VECTOR OF CONTROLS, 
## EACH GROUP IS 45 UNITS 
## GROUPS DEFINED BY ROUND THEN BITE e.g. r1 b1, 
##                                        r1 b2, 
##                                        r1 b3, etc

oocysts_t4B785 ## Only Round 1 matched

oocystsC3 <- c(oocystsC[1:45],oocystsC[181:225],oocystsC[361:405],oocystsC[541:585])

PREV_C  ## THE COMPLETE MATRIX
## EACH GROUP IS A COLUMN
## EACH ROW IS A MOUSE
PREVC_3 <- cbind(PREV_C[,1],PREV_C[,5],PREV_C[,9],PREV_C[,13])

PREV_t4B785   ## 12 COLUMNS REPRESENTING 1 ROUNDS, 4 BITE RATES

spors_C_1ROUND<-rbind(spors_C[1,],spors_C[5,],spors_C[9,],spors_C[13,])

sporst4B785 

t4B785_data<-list(N_C=4,
                 N_T=4,
                 N_ooc=45,
                 N_mice=5,
                 ooc_count_C = structure(.Data = c(oocystsC3),
                                         .Dim=c(45,4)),
                 ooc_count_T = structure(.Data = c(oocysts_t4B785),
                                         .Dim=c(45,4)),
                 prev_C = structure(.Data =PREVC_3,.Dim=c(5,4)),
                 prev_T = structure(.Data =PREV_t4B785,.Dim=c(5,4)),
                 N_bin=5,
                 bin_edge=c(0,1,10,100,1000,1002),
                 s_count_C = structure(.Data=spors_C_1ROUND,.Dim=c(4,5)),
                 s_count_T = structure(.Data=sporst4B785,.Dim=c(4,5))
)
###############
##
## 3D11 50% + ATV_85
##

oocystsC ## THE COMPLETE VECTOR OF CONTROLS, 
## EACH GROUP IS 45 UNITS 
## GROUPS DEFINED BY ROUND THEN BITE e.g. r1 b1, 
##                                        r1 b2, 
##                                        r1 b3, etc

oocysts_t3d11andATV85 ## Only Round 1 matched

oocystsC3 <- c(oocystsC[1:45],oocystsC[181:225],oocystsC[361:405],oocystsC[541:585])

PREV_C  ## THE COMPLETE MATRIX
## EACH GROUP IS A COLUMN
## EACH ROW IS A MOUSE
PREVC_3 <- cbind(PREV_C[,1],PREV_C[,5],PREV_C[,9],PREV_C[,13])

PREV_t3d11andATV85 ## 4 COLUMNS REPRESENTING 1 ROUNDS, 4 BITE RATES

spors_C_1ROUND<-rbind(spors_C[1,],spors_C[5,],spors_C[9,],spors_C[13,])

sporst3d11andATV85

t3d11andATV85_data<-list(N_C=4,
                 N_T=4,
                 N_ooc=45,
                 N_mice=5,
                 ooc_count_C = structure(.Data = c(oocystsC3),
                                         .Dim=c(45,4)),
                 ooc_count_T = structure(.Data = c(oocysts_t3d11andATV85),
                                         .Dim=c(45,4)),
                 prev_C = structure(.Data =PREVC_3,.Dim=c(5,4)),
                 prev_T = structure(.Data =PREV_t3d11andATV85,.Dim=c(5,4)),
                 N_bin=5,
                 bin_edge=c(0,1,10,100,1000,1002),
                 s_count_C = structure(.Data=spors_C_1ROUND,.Dim=c(4,5)),
                 s_count_T = structure(.Data=sporst3d11andATV85,.Dim=c(4,5))
)
###############
##
## 3D11 50% & 4B7 85%
##

oocystsC ## THE COMPLETE VECTOR OF CONTROLS, 
## EACH GROUP IS 45 UNITS 
## GROUPS DEFINED BY ROUND THEN BITE e.g. r1 b1, 
##                                        r1 b2, 
##                                        r1 b3, etc

oocysts_t3d11and4B785 ## Only Round 1 matched

oocystsC3 <- c(oocystsC[1:45],oocystsC[181:225],oocystsC[361:405],oocystsC[541:585])

PREV_C  ## THE COMPLETE MATRIX
## EACH GROUP IS A COLUMN
## EACH ROW IS A MOUSE
PREVC_3 <- cbind(PREV_C[,1],PREV_C[,5],PREV_C[,9],PREV_C[,13])

PREV_t3d11and4B785 ## 4 COLUMNS REPRESENTING 1 ROUNDS, 4 BITE RATES

spors_C_1ROUND<-rbind(spors_C[1,],spors_C[5,],spors_C[9,],spors_C[13,])

sporst3d11and4B785

t3d11and4B785_data<-list(N_C=4,
                         N_T=4,
                         N_ooc=45,
                         N_mice=5,
                         ooc_count_C = structure(.Data = c(oocystsC3),
                                                 .Dim=c(45,4)),
                         ooc_count_T = structure(.Data = c(oocysts_t3d11and4B785),
                                                 .Dim=c(45,4)),
                         prev_C = structure(.Data =PREVC_3,.Dim=c(5,4)),
                         prev_T = structure(.Data =PREV_t3d11and4B785,.Dim=c(5,4)),
                         N_bin=5,
                         bin_edge=c(0,1,10,100,1000,1002),
                         s_count_C = structure(.Data=spors_C_1ROUND,.Dim=c(4,5)),
                         s_count_T = structure(.Data=sporst3d11and4B785,.Dim=c(4,5))
)