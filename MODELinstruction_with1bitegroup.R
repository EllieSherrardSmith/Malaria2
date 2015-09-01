library(nlme)
library(rstan)
library(MASS)
library(boot)
library(coda)
library(R2OpenBUGS)
library(ggplot2)
library("Rlab")
library(contrast)
#install.packages('devtools')
#library(devtools)
#source_url("https://github.com/stan-dev/shinystan/raw/develop/install_shinystan.R")
#install_shinystan()
library(shinyStan)
library(rmngb)

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


oocysts<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocystsbites2to5.csv",header=TRUE)
head(oocysts)


##OOCYSTS
oocystsC<-c(oocysts$oocystsbites1control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites1control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites1control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites1control[oocysts$round=="day134"][1:24],
  oocysts$oocystsbites2control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites2control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites2control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites2control[oocysts$round=="day134"][1:24],
  oocysts$oocystsbites3control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites3control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites3control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites3control[oocysts$round=="day134"][1:24],
  oocysts$oocystsbites4control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites4control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites4control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites4control[oocysts$round=="day134"][1:24],
  oocysts$oocystsbites5control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites5control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites5control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites5control[oocysts$round=="day134"][1:24])
length(oocystsC)
prevoocC<-ifelse(oocystsC==0,0,1)
prevooc1<-c(sum(sum(prevoocC[1:24])/24,sum(prevoocC[25:48])/24,sum(prevoocC[49:72])/24,sum(prevoocC[73:96])/24)/4,
            sum(sum(prevoocC[97:120])/24,sum(prevoocC[121:144])/24,sum(prevoocC[145:168])/24,sum(prevoocC[169:192])/24)/4,
            sum(sum(prevoocC[193:216])/24,sum(prevoocC[217:240])/24,sum(prevoocC[241:264])/24,sum(prevoocC[265:288])/24)/4,
            sum(sum(prevoocC[289:312])/24,sum(prevoocC[313:336])/24,sum(prevoocC[337:360])/24,sum(prevoocC[360:384])/24)/4,
            sum(sum(prevoocC[385:408])/24,sum(prevoocC[409:432])/24,sum(prevoocC[433:456])/24,sum(prevoocC[457:480])/24)/4)

prevooc1ConGens<-c(sum(prevoocC[1:24])/24,sum(prevoocC[25:48])/24,sum(prevoocC[49:72])/24,sum(prevoocC[73:96])/24,
                   sum(prevoocC[97:120])/24,sum(prevoocC[121:144])/24,sum(prevoocC[145:168])/24,sum(prevoocC[169:192])/24,
                   sum(prevoocC[193:216])/24,sum(prevoocC[217:240])/24,sum(prevoocC[241:264])/24,sum(prevoocC[265:288])/24,
                   sum(prevoocC[289:312])/24,sum(prevoocC[313:336])/24,sum(prevoocC[337:360])/24,sum(prevoocC[360:384])/24,
                   sum(prevoocC[385:408])/24,sum(prevoocC[409:432])/24,sum(prevoocC[433:456])/24,sum(prevoocC[457:480])/24)

#freqoocC<-numeric(length(unique(oocystsC)))
#oocystsC2<-sort(unique(oocystsC))
#for (i in 1:length(oocystsC2)){ 
#  freqoocC[i]<-sum(ifelse(oocystsC==unique(oocystsC2)[i],1,0))}
#probNooc<-numeric(length(freqoocC))
#for (j in 1:length(freqoocC)){
#probNooc[j]<-freqoocC[j]/sum(freqoocC)}
#freqdistoocC<-data.frame(oocystsC2,probNooc);colnames(freqdistoocC)[1]<-"Nooc"

oocystsT<-c(oocysts$oocystsbites1atv[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites1atv[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites1atv[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites1atv[oocysts$round=="day134"][1:24],
  oocysts$oocystsbites2atv[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites2atv[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites2atv[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites2atv[oocysts$round=="day134"][1:24],
  oocysts$oocystsbites3atv[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites3atv[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites3atv[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites3atv[oocysts$round=="day134"][1:24],
  oocysts$oocystsbites4atv[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites4atv[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites4atv[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites4atv[oocysts$round=="day134"][1:24],
  oocysts$oocystsbites5atv[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites5atv[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites5atv[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites5atv[oocysts$round=="day134"][1:24])
length(oocystsT)
prevoocT<-ifelse(oocystsT==0,0,1)

prevoocTgens<-c(sum(prevoocT[1:24])/24,sum(prevoocT[25:48])/24,sum(prevoocT[49:72])/24,sum(prevoocT[73:96])/24,
                sum(prevoocT[97:120])/24,sum(prevoocT[121:144])/24,sum(prevoocT[145:168])/24,sum(prevoocT[169:192])/24,
                sum(prevoocT[193:216])/24,sum(prevoocT[217:240])/24,sum(prevoocT[241:264])/24,sum(prevoocT[265:288])/24,
                sum(prevoocT[289:312])/24,sum(prevoocT[313:336])/24,sum(prevoocT[337:360])/24,sum(prevoocT[360:384])/24,
                sum(prevoocC[385:408])/24,sum(prevoocC[409:432])/24,sum(prevoocC[433:456])/24,sum(prevoocC[457:480])/24)

prevoocT<-c(sum(sum(prevoocT[1:24])/24,sum(prevoocT[25:48])/24,sum(prevoocT[49:72])/24,sum(prevoocT[73:96])/24)/4,
            sum(sum(prevoocT[97:120])/24,sum(prevoocT[121:144])/24,sum(prevoocT[145:168])/24,sum(prevoocT[169:192])/24)/4,
            sum(sum(prevoocT[193:216])/24,sum(prevoocT[217:240])/24,sum(prevoocT[241:264])/24,sum(prevoocT[265:288])/24)/4,
            sum(sum(prevoocT[289:312])/24,sum(prevoocT[313:336])/24,sum(prevoocT[337:360])/24,sum(prevoocT[360:384])/24)/4,
            sum(sum(prevoocT[385:408])/24,sum(prevoocT[409:432])/24,sum(prevoocT[433:456])/24,sum(prevoocT[457:480])/24)/4)

EffOoc<-(prevooc1-prevoocT)/prevooc1

#freqoocT<-numeric(length(unique(oocystsT)))
#oocystsT2<-sort(unique(oocystsT))
#for (i in 1:length(oocystsT2)){ 
#  freqoocT[i]<-sum(ifelse(oocystsT==unique(oocystsT2)[i],1,0))}
#probNoocT<-numeric(length(freqoocT))
#for (j in 1:length(freqoocT)){
#  probNoocT[j]<-freqoocT[j]/sum(freqoocT)}
#freqdistoocT<-data.frame(oocystsT2,probNoocT);colnames(freqdistoocT)[1]<-"NoocT"

meanoocysts<-c(mean(oocysts$oocystsbites1control[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites1control[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites1control[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites1control[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites2control[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites2control[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites2control[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites2control[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites3control[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites3control[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites3control[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites3control[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites4control[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites4control[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites4control[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites4control[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites5control[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites5control[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites5control[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites5control[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites1atv[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites1atv[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites1atv[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites1atv[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites2atv[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites2atv[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites2atv[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites2atv[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites3atv[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites3atv[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites3atv[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites3atv[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites4atv[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites4atv[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites4atv[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites4atv[oocysts$round=="day134"],na.rm=TRUE),
  mean(oocysts$oocystsbites5atv[oocysts$round=="day41"],na.rm=TRUE),
  mean(oocysts$oocystsbites5atv[oocysts$round=="day72"],na.rm=TRUE),
  mean(oocysts$oocystsbites5atv[oocysts$round=="day103"],na.rm=TRUE),
  mean(oocysts$oocystsbites5atv[oocysts$round=="day134"],na.rm=TRUE))

EfficacyChurcher2012<-((mean(prevooc1)/mean(meanoocysts[1:20]))-(mean(prevoocT)/mean(meanoocysts[21:40])))/
  (mean(prevooc1)/mean(meanoocysts[1:20]))

spors<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=TRUE)
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)


##MEAN PARASITEMIA IN MICE
#mean(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 5])
#mean(spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 5])
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 4],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 4])~c(rep("Con",100),rep("Treat",100))))
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 1],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 1])~c(rep("Con",20),rep("Treat",20))))
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 2],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 2])~c(rep("Con",20),rep("Treat",20))))
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 3],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 3])~c(rep("Con",20),rep("Treat",20))))
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 4],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 4])~c(rep("Con",20),rep("Treat",20))))
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 5],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 5])~c(rep("Con",20),rep("Treat",20))))

#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 4 & spors$Round == 1],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 4 & spors$Round == 1])~c(rep("Con",5),rep("Treat",5))))
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 4 & spors$Round == 2],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 4 & spors$Round == 2])~c(rep("Con",5),rep("Treat",5))))
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 4 & spors$Round == 3],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 4 & spors$Round == 3])~c(rep("Con",5),rep("Treat",5))))
#summary(aov(c(spors$Parasitemia[spors$Treatment == 0 & spors$Bites == 4 & spors$Round == 4],spors$Parasitemia[spors$Treatment == 1 & spors$Bites == 4 & spors$Round == 4])~c(rep("Con",5),rep("Treat",5))))


parasit<-cbind(
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4])

parasitORIGmean<-c(
  mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4]))

#parasitem<-parasitemUPP<-parasitemLOW<-numeric(16)
#for (i in 1:16){
#  parasitem[i]<-sum(parasit[,i])/5

#a<-numeric(10000)
#for (j in 1:10000){ 

#  a[j]<-mean(sample(parasit[,i],4))
#  parasitemUPP[i]<-quantile(a,0.975)
#  parasitemLOW[i]<-quantile(a,0.025)
#}
#}
parasitT<-cbind(
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 4])
parasitATV32mean<-c(
  mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Parasitemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 4]))

gametC<-cbind(
  spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4])
#gametoC<-gametoCUPP<-gametoCLOW<-numeric(16)
#for (i in 1:16){
#  gametoC[i]<-sum(gametC[,i])/5
#a<-numeric(10000)
#for (j in 1:10000){ 

#  a[j]<-mean(sample(gametC[,i],4))
#  gametoCUPP[i]<-quantile(a,0.975)
#  gametoCLOW[i]<-quantile(a,0.025)
#}
#}
gametT<-cbind(
  spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4],
  spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 1],
  spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 2],
  spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 3],
  spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 4])


gametORIGmean<-c(
  mean(spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$Gametocytemia[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 4]))


par(mar=c(5,5,2,2))
boxplot(parasit[,1],parasitT[,1],parasit[,2],parasitT[,2],parasit[,3],parasitT[,3],parasit[,4],parasitT[,4],
        parasit[,5],parasitT[,5],parasit[,6],parasitT[,6],parasit[,7],parasitT[,7],parasit[,8],parasitT[,8],
        parasit[,9],parasitT[,9],parasit[,10],parasitT[,10],parasit[,11],parasitT[,11],parasit[,12],parasitT[,12],
        parasit[,13],parasitT[,13],parasit[,14],parasitT[,14],parasit[,15],parasitT[,15],parasit[,16],parasitT[,16],
        parasit[,17],parasitT[,17],parasit[,18],parasitT[,18],parasit[,19],parasitT[,19],parasit[,20],parasitT[,20],
        col=c(rep(c("darkred","red"),4),rep(c("darkgreen","green"),4),
              rep(c("darkblue","blue"),4),rep(c("grey15","grey67"),4),rep(c("grey15","grey67"),4)),frame=F,xaxt="n",
        ylab="Parasitemia (%)",cex.lab=2,xlab="Generations ")
legend(0,25.5,title="Mosquito to mouse biting ratio",legend=c("1","2","3","4","5","control (dark)","treatment (light)"),
       col=c("orange","red","green","blue","grey","grey15","grey67"),pch=15,lty=c(rep(NA,5),2,2),bty="n",
       cex=1.4)
axis(1,at=seq(1.5,39.5,2),labels=c(rep(seq(1,4,1),5)))

parasite2NoNegs <- ifelse(parasit > 0, parasit, NA)
parasite2NoNegsT <- ifelse(parasitT > 0, parasitT, NA)

par(mar=c(5,5,2,2))
boxplot(parasite2NoNegs[,1],parasite2NoNegsT[,1],parasite2NoNegs[,2],parasite2NoNegsT[,2],parasite2NoNegs[,3],parasite2NoNegsT[,3],parasite2NoNegs[,4],parasite2NoNegsT[,4],
        parasite2NoNegs[,5],parasite2NoNegsT[,5],parasite2NoNegs[,6],parasite2NoNegsT[,6],parasite2NoNegs[,7],parasite2NoNegsT[,7],parasite2NoNegs[,8],parasite2NoNegsT[,8],
        parasite2NoNegs[,9],parasite2NoNegsT[,9],parasite2NoNegs[,10],parasite2NoNegsT[,10],parasite2NoNegs[,11],parasite2NoNegsT[,11],parasite2NoNegs[,12],parasite2NoNegsT[,12],
        parasite2NoNegs[,13],parasite2NoNegsT[,13],parasite2NoNegs[,14],parasite2NoNegsT[,14],parasite2NoNegs[,15],parasite2NoNegsT[,15],parasite2NoNegs[,16],parasite2NoNegsT[,16],
        parasite2NoNegs[,17],parasite2NoNegsT[,17],parasite2NoNegs[,18],parasite2NoNegsT[,18],parasite2NoNegs[,19],parasite2NoNegsT[,19],parasite2NoNegs[,20],parasite2NoNegsT[,20],
        col=c(rep(c("orange","yellow"),4),rep(c("darkred","red"),4),rep(c("darkgreen","green"),4),
              rep(c("darkblue","blue"),4),rep(c("grey15","grey67"),4)),na.rm=T,frame=F,xaxt="n",
        ylab="Parasitemia (%)",cex.lab=2,xlab="Generations ")
legend(0,25.5,title="Mosquito to mouse biting ratio",legend=c("1","2","3","4","5","control (dark)","treatment (light)"),
       col=c("orange","red","green","blue","grey","grey15","grey67"),pch=15,lty=c(rep(NA,5),2,2),bty="n",
       cex=1.4)
axis(1,at=seq(1.5,39.5,2),labels=c(rep(seq(1,4,1),5)))


##PREVALENCE IN MICE
spcount<-expand.grid(seq(1,nrow(spors),1))
for(i in 1:nrow(spors)){
  for (j in 6:10){
    spcount[i,j-5]<-ifelse(is.na(spors[i,j])==FALSE,1,0)}}
for (i in 1:length(spors$prevBS)){
  spors$sporoCount[i]<-sum(spcount[i,])}
for (i in 1:nrow(spors)){
  spors$meanpermouse[i]<-sum(spors[i,6:10],na.rm=T)/spors$sporoCount[i]
}

PREV_C<-cbind(
  spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1],
  spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2],
  spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3],
  spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 1],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 2],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 3],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4])

PREV_T<-cbind(
    spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1],
    spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2],
    spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3],
    spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3],
  spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3],
  spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3],
  spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 1],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 2],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 3],
  spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 4])

PREVorigMean<-c(
  mean(spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4]),
  mean(spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 1]),
  mean(spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 2]),
  mean(spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 3]),
  mean(spors$prevBS[spors$Bites==5 & spors$Treatment == 1 & spors$Round == 4]))


###sPOROZOITES
sporsbites1<-subset(spors,Bites==1 & Treatment==0);sporsbites1
newa1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==1]);
spb1r1<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
newb1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==2]);
spb1r2<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
newc1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==3]);
spb1r3<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
newd1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==4]);
spb1r4<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))


sporsbites2<-subset(spors,Bites==2 & Treatment==0);sporsbites2
a1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==1],sporsbites2$Sporozoite2[sporsbites2$Round==1]);
spb2r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
b1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==2],sporsbites2$Sporozoite2[sporsbites2$Round==2]);
spb2r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
c1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==3],sporsbites2$Sporozoite2[sporsbites2$Round==3]);
spb2r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
d1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==4],sporsbites2$Sporozoite2[sporsbites2$Round==4]);
spb2r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))


sporsbites3<-subset(spors,Bites==3 & Treatment==0);sporsbites3
ee1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==1],sporsbites3$Sporozoite2[sporsbites3$Round==1],sporsbites3$Sporozoite3[sporsbites3$Round==1]);
spb3r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
ff1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==2],sporsbites3$Sporozoite2[sporsbites3$Round==2],sporsbites3$Sporozoite3[sporsbites3$Round==2]);
spb3r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
g1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==3],sporsbites3$Sporozoite2[sporsbites3$Round==3],sporsbites3$Sporozoite3[sporsbites3$Round==3]);
spb3r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
h1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==4],sporsbites3$Sporozoite2[sporsbites3$Round==4],sporsbites3$Sporozoite3[sporsbites3$Round==4]);
spb3r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites4<-subset(spors,Bites==4 & Treatment==0);sporsbites4
jj1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==1],sporsbites4$Sporozoite2[sporsbites4$Round==1],sporsbites4$Sporozoite3[sporsbites4$Round==1],sporsbites4$Sporozoite4[sporsbites4$Round==1]);
spb4r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
kk1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==2],sporsbites4$Sporozoite2[sporsbites4$Round==2],sporsbites4$Sporozoite3[sporsbites4$Round==2],sporsbites4$Sporozoite4[sporsbites4$Round==2]);
spb4r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
l1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==3],sporsbites4$Sporozoite2[sporsbites4$Round==3],sporsbites4$Sporozoite3[sporsbites4$Round==3],sporsbites4$Sporozoite4[sporsbites4$Round==3]);
spb4r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
m1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==4],sporsbites4$Sporozoite2[sporsbites4$Round==4],sporsbites4$Sporozoite3[sporsbites4$Round==4],sporsbites4$Sporozoite4[sporsbites4$Round==4]);
spb4r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites5<-subset(spors,Bites==5 & Treatment==0);sporsbites5
nn1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==1],sporsbites5$Sporozoite2[sporsbites5$Round==1],sporsbites5$Sporozoite3[sporsbites5$Round==1],sporsbites5$Sporozoite4[sporsbites5$Round==1],sporsbites5$Sporozoite5[sporsbites5$Round==1]);
spb5r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
oo1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==2],sporsbites5$Sporozoite2[sporsbites5$Round==2],sporsbites5$Sporozoite3[sporsbites5$Round==2],sporsbites5$Sporozoite4[sporsbites5$Round==2],sporsbites5$Sporozoite5[sporsbites5$Round==2]);
spb5r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
pp1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==3],sporsbites5$Sporozoite2[sporsbites5$Round==3],sporsbites5$Sporozoite3[sporsbites5$Round==3],sporsbites5$Sporozoite4[sporsbites5$Round==3],sporsbites5$Sporozoite5[sporsbites5$Round==3]);
spb5r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
qq1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==4],sporsbites5$Sporozoite2[sporsbites5$Round==4],sporsbites5$Sporozoite3[sporsbites5$Round==4],sporsbites5$Sporozoite4[sporsbites5$Round==4],sporsbites5$Sporozoite5[sporsbites5$Round==4]);
spb5r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

spors_C<-rbind(spb1r1,spb1r2,spb1r3,spb1r4,
  spb2r1,spb2r2,spb2r3,spb2r4,
  spb3r1,spb3r2,spb3r3,spb3r4,
  spb4r1,spb4r2,spb4r3,spb4r4,
  spb5r1,spb5r2,spb5r3,spb5r4)

sporsbites1<-subset(spors,Bites==1 & Treatment==1);sporsbites1
new_a<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==1]);
spb1r1<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
new_b<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==2]);
spb1r2<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
new_c<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==3]);
spb1r3<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
new_d<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==4]);
spb1r4<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))


sporsbites2<-subset(spors,Bites==2 & Treatment==1);sporsbites2
a<- sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==1],sporsbites2$Sporozoite2[sporsbites2$Round==1]);
spb2r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
b<-  sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==2],sporsbites2$Sporozoite2[sporsbites2$Round==2]);
spb2r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
c<-    sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==3],sporsbites2$Sporozoite2[sporsbites2$Round==3]);
spb2r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
d<-    sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==4],sporsbites2$Sporozoite2[sporsbites2$Round==4]);
spb2r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))


sporsbites3<-subset(spors,Bites==3 & Treatment==1);sporsbites3
ee<- sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==1],sporsbites3$Sporozoite2[sporsbites3$Round==1],sporsbites3$Sporozoite3[sporsbites3$Round==1]);
spb3r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
ff<-  sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==2],sporsbites3$Sporozoite2[sporsbites3$Round==2],sporsbites3$Sporozoite3[sporsbites3$Round==2]);
spb3r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
g<-  sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==3],sporsbites3$Sporozoite2[sporsbites3$Round==3],sporsbites3$Sporozoite3[sporsbites3$Round==3]);
spb3r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
h<- sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==4],sporsbites3$Sporozoite2[sporsbites3$Round==4],sporsbites3$Sporozoite3[sporsbites3$Round==4]);
spb3r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites4<-subset(spors,Bites==4 & Treatment==1);sporsbites4
jj<- sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==1],sporsbites4$Sporozoite2[sporsbites4$Round==1],sporsbites4$Sporozoite3[sporsbites4$Round==1],sporsbites4$Sporozoite4[sporsbites4$Round==1]);
spb4r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
kk<-  sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==2],sporsbites4$Sporozoite2[sporsbites4$Round==2],sporsbites4$Sporozoite3[sporsbites4$Round==2],sporsbites4$Sporozoite4[sporsbites4$Round==2]);
spb4r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
l<-  sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==3],sporsbites4$Sporozoite2[sporsbites4$Round==3],sporsbites4$Sporozoite3[sporsbites4$Round==3],sporsbites4$Sporozoite4[sporsbites4$Round==3]);
spb4r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
m<- sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==4],sporsbites4$Sporozoite2[sporsbites4$Round==4],sporsbites4$Sporozoite3[sporsbites4$Round==4],sporsbites4$Sporozoite4[sporsbites4$Round==4]);
spb4r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites5<-subset(spors,Bites==5 & Treatment==1);sporsbites5
nn<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==1],sporsbites5$Sporozoite2[sporsbites5$Round==1],sporsbites5$Sporozoite3[sporsbites5$Round==1],sporsbites5$Sporozoite4[sporsbites5$Round==1],sporsbites5$Sporozoite5[sporsbites5$Round==1]);
spb5r1<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
oo<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==2],sporsbites5$Sporozoite2[sporsbites5$Round==2],sporsbites5$Sporozoite3[sporsbites5$Round==2],sporsbites5$Sporozoite4[sporsbites5$Round==2],sporsbites5$Sporozoite5[sporsbites5$Round==2]);
spb5r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
pp<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==3],sporsbites5$Sporozoite2[sporsbites5$Round==3],sporsbites5$Sporozoite3[sporsbites5$Round==3],sporsbites5$Sporozoite4[sporsbites5$Round==3],sporsbites5$Sporozoite5[sporsbites5$Round==3]);
spb5r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
qq<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==4],sporsbites5$Sporozoite2[sporsbites5$Round==4],sporsbites5$Sporozoite3[sporsbites5$Round==4],sporsbites5$Sporozoite4[sporsbites5$Round==4],sporsbites5$Sporozoite5[sporsbites5$Round==4]);
spb5r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

spors_T<-rbind(spb1r1,spb1r2,spb1r3,spb1r4,
  spb2r1,spb2r2,spb2r3,spb2r4,
  spb3r1,spb3r2,spb3r3,spb3r4,
  spb4r1,spb4r2,spb4r3,spb4r4,
  spb5r1,spb5r2,spb5r3,spb5r4)
spors_Cprev<-spors_Tprev<-numeric(20)
for(i in 1:20){
  spors_Cprev[i]<-1-(spors_C[i,1]/sum(spors_C[i,]))
  spors_Tprev[i]<-1-(spors_T[i,1]/sum(spors_T[i,]))
}

effectSpors<-(spors_Cprev-spors_Tprev)/spors_Cprev
EFFSPmbr<-c(sum(effectSpors[1:4])/4,
            sum(effectSpors[5:8])/4,
            sum(effectSpors[9:12])/4,
            sum(effectSpors[13:16])/4,
            sum(effectSpors[17:20])/4)


MEANsporig<-c(mean(newa1),mean(newb1),mean(newc1),mean(newd1),
  mean(a1),mean(b1),mean(c1),mean(d1),
  mean(ee1),mean(ff1),mean(g1),mean(h1),
  mean(jj1),mean(kk1),mean(l1),mean(m1),
  mean(nn1),mean(oo1),mean(pp1),mean(qq1),
  mean(new_a),mean(new_b),mean(new_c),mean(new_d),  
  mean(a),mean(b),mean(c),mean(d),
  mean(ee),mean(ff),mean(g),mean(h),
  mean(jj),mean(kk),mean(l),mean(m),
  mean(nn),mean(oo),mean(pp),mean(qq))

prevcon<-prevtreat<-numeric(20)
for (i in 1:20){
  prevcon[i]<-c(sum(PREV_C[,i])/5)
  prevtreat[i]<-c(sum(PREV_T[,i])/5)
}

prevs<-c(prevcon,prevtreat)
prevmbrC<-c(sum(prevcon[1:4])/4,
            sum(prevcon[5:8])/4,
            sum(prevcon[9:12])/4,
            sum(prevcon[13:16])/4,
            sum(prevcon[17:20])/4)
prevmbrT<-c(sum(prevtreat[1:4])/4,
            sum(prevtreat[5:8])/4,
            sum(prevtreat[9:12])/4,
            sum(prevtreat[13:16])/4,
            sum(prevtreat[17:20])/4)
meanprevpergroup<-(prevcon+prevtreat)/2
##########################################################################
##
## 2. Exploring the relationship between parasitemia and sporozoites
##  
##
###########################################################################
### 

for (i in 1:length(spors$prevBS)) spors$sumspors[i]<-sum(spors$Sporozoite1[i],spors$Sporozoite2[i],spors$Sporozoite3[i],
                                                         spors$Sporozoite4[i],spors$Sporozoite5[i],na.rm=T)

  
mod1 <- glm.nb(spors$sumspors ~ spors$Parasitemia + 0)
summary.lm(mod1)

m3 <- glm(spors$sumspors ~ spors$Parasitemia + 0, family = "poisson")
pchisq(2 * (logLik(mod1) - logLik(m3)), df = 1, lower.tail = FALSE)
(est <- cbind(Estimate = coef(mod1), confint(mod1)))

para_Ca<-spors$Parasitemia[spors$Treatment==0]
para_Ta<-spors$Parasitemia[spors$Treatment==1]
spor_Ca<-spors$sumspors[spors$Treatment==0]
spor_Ta<-spors$sumspors[spors$Treatment==1]

##should sporo scores predict parasitemia or parasitemia predict post-feeding spor scores?!!
spordat <- list(N=200,
                x=spors$Parasitemia,
                y=spors$sumspors)
test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to para.stan", data=spordat,
              iter=1000, chains=4)

print(test1)
print(waic(test1))
params = extract(test1);names(params)
rstan::traceplot(test1, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,max(spors$Parasitemia),0.1)
pred1<-(mean(params$alpha[501:1000]) * nc^mean(params$sigma[501:1000]))/
  (mean(params$delta[501:1000]) + mean(params$beta[501:1000]) * nc^mean(params$sigma[501:1000])) 
par(mar=c(5,5,5,5))
plot(spors$sumspors~spors$Parasitemia,cex.lab=1.5,bty="n",
     ylab="Summed sporozoite score",xlab="Parasitemia (%)",ylim=c(0,25))

lines(nc,pred1,lwd=2,lty=2,col="red")
alpha <- mean(params$alpha)
beta <- mean(params$beta)
sigma <- mean(params$sigma)
delta <- mean(params$delta)
eps <- mean(params$eps)
x <- seq(0,max(spors$Parasitemia),0.1)
y <- pred1

e <- extract(test1, pars = c("alpha", "beta", "sigma","delta","eps"))

for(i in seq_along(e[[1]])) {
  lines(x, (e[[1]][i] * x^e[[3]][i]) / (e[[4]][i] + e[[2]][i] * x^e[[3]][i]), col = "#00000008")
}
lines(nc,pred1,lwd=2,lty=2,col="red")

##should sporo scores predict parasitemia or parasitemia predict post-feeding spor scores?!!
spordat <- list(N=200,
                x=spors$Parasitemia,
                y=spors$sumspors)
test1a <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to paralinearfit.stan", data=spordat,
              iter=1000, chains=4)

print(test1a)
print(waic(test1a))
params = extract(test1a);names(params)
#rstan::traceplot(test1, inc_warmup = FALSE)

par(mfrow=c(1,3));par(las=1)
## For sporssum to para.stan
nc<-seq(0,max(spors$Parasitemia),0.1)
pred<-(mean(params$alpha[501:1000]) + mean(params$beta[501:1000]) * nc) 
par(mar=c(5,5,5,5))
plot(spors$sumspors~spors$Parasitemia,cex.lab=1.4,bty="n",yaxt="n",cex.lab=1.5,
     ylab="Summed sporozoite score",xlab="Parasitemia (%)",ylim=c(0,25))
par(las=2);axis(2,at=seq(0,25,5),labels=c(0,5,10,15,20,25))
points(spors$sumspors[spors$Treatment==0]~spors$Parasitemia[spors$Treatment==0],pch=19)
points(spors$sumspors[spors$Treatment==1]~spors$Parasitemia[spors$Treatment==1],pch=1)

lines(nc,pred,lwd=2,lty=2,col="blue")
alpha <- mean(params$alpha)
beta <- mean(params$beta)
sigma <- mean(params$sigma)
x <- seq(0,max(spors$Parasitemia),0.1)
y <- pred

e <- extract(test1a, pars = c("alpha", "beta", "sigma"))

for(i in seq_along(e[[1]])) {
  lines(x, (e[[1]][i] + e[[2]][i] * x), col = "#00000003")
}
lines(nc,pred,lwd=2,lty=2,col="red")

se <- function(x) sqrt(var(x)/length(x))
WAICcomp <- waic(test1)$total[4]-waic(test1a)$total[4]


## For sporssum to para4negbin
spordat <- list(N=200,
                y=round(spors$Parasitemia),
                x=spors$sumspors)
test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to para4negbin.stan", data=spordat,
              iter=1000, chains=4)

par(mar=c(5,5,5,5))
plot(spors$Parasitemia~spors$sumspors,cex.lab=1.4,bty="n",
     xlab="Summed sporozoite score",ylab="Parasitemia (%)",ylim=c(0,25))

print(test2)
params = extract(test2);names(params)
rstan::traceplot(test2, inc_warmup = FALSE)

set.seed(123)

phi <- mean(params$phi)
b0 <- mean(params$b0)
b1 <- mean(params$b1)
x <- preds3$x
N <- length(x)
y <- rnbinom(N, size = phi, mu = exp(b0 + (x - mean(x)) * b1))

e <- extract(test2, pars = c("b0", "b1", "phi"))
true_pars <- c(b0 = b0, b1 = b1, phi = phi)
x_cent <- x - mean(x)
m_mass <- MASS::glm.nb(y ~ x_cent)
coefs_mass <- c(coef(m_mass), summary(m_mass)$theta)

for(i in seq_along(e[[1]])) {
  lines(x, exp(e[[1]][i] + e[[2]][i] * (x - mean(x))), col = "#00000006")
}
  #points(x, exp(mean(e[[1]])+mean(e[[2]])*x),col="red",lwd=2)
    ypred <- exp(mean(e[[1]])+mean(e[[2]])*x)
    preds <- data.frame(x,ypred);preds2<-preds[with(preds, order(x)), ]
    preds3 <- preds2[!duplicated(preds2[,c('x')]),]
    lines(preds3$x,preds3$ypred)
points(spors$sumspors~spors$Parasitemia,pch=20,col="blue")
#points(spors$Parasitemia[spors$Treatment==1]~spors$sumspors[spors$Treatment==1],pch=20,col="blue")

par(mfrow = c(1, 3))
for(i in 1:3) {
  if(i %in% 1:2) {
    plot(density(e[[i]]), main = names(true_pars)[i])
  } else {
    plot(density(e[[i]]), main = names(true_pars)[i], log = "x")
  }
  abline(v = true_pars[i], lwd = 2, col = "grey", lty = 2)
  abline(v = coefs_mass[i], lwd = 3, col = "red")
}

legend("topright", legend = c("posterior", "true", "MASS::glm.nb"),
       col = c("black", "grey", "red"), lty = c(1, 2, 1), lwd = c(1, 1.3, 3),
       bty = "n")


##########################################################################
##
## 3. Exploring the relationship between parasitemia and gametocytemia
##  
##
###########################################################################
parasORIGmean <- c(parasitORIGmean,parasitATV32mean)
  
spordat <- list(N=40,
                x=parasORIGmean,
                y=gametORIGmean)
test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to paralinearfit.stan", data=spordat,
              iter=1000, chains=4)

print(test2)
print(waic(test2))

params2 = extract(test2);names(params2)
#rstan::traceplot(test2, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,max(spors$Parasitemia),0.1)
pred<-(mean(params2$alpha[501:1000]) + mean(params2$beta[501:1000]) * nc) 
par(mar=c(5,5,5,5))
par(las=1)
plot(gametORIGmean~parasORIGmean,cex.lab=1.5,bty="n",xlim=c(0,12),yaxt="n",
     ylab="Gametocytemia (%)",xlab="Parasitemia (%)",ylim=c(0,1.5))
par(las=2);axis(2,at=seq(0,1.5,0.5),labels=c(0,0.5,1.0,1.5))

lines(nc,pred,lwd=2,lty=2,col="red")
alpha <- mean(params2$alpha)
beta <- mean(params2$beta)
sigma <- mean(params2$sigma)
x <- seq(0,max(spors$Parasitemia),0.1)
y <- pred

e <- extract(test2, pars = c("alpha", "beta", "sigma"))

for(i in seq_along(e[[1]])) {
  lines(x, (e[[1]][i] + e[[2]][i] * x), col = "#00000008")
}
lines(nc,pred,lwd=2,lty=2,col="red")
points(gametORIGmean[1:20]~parasORIGmean[1:20],pch=19)

##########################################################################
##
## 4. Fit the distributions of the data and estimate the real sporozoite counts and prevalence 
##    with and without treatment to get the difference in the probaility of transmission
##  
##
###########################################################################
data1<-list(N_C=20,
            N_T=20,
            N_ooc=24,
            N_mice=5,
            ooc_count_C = structure(.Data = c(oocystsC),
                                    .Dim=c(24,20)),
            ooc_count_T = structure(.Data = c(oocystsT),
                                    .Dim=c(24,20)),
            prev_C = structure(.Data =PREV_C,.Dim=c(5,20)),
            prev_T = structure(.Data =PREV_T,.Dim=c(5,20)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,1002),
            s_count_C = structure(.Data=spors_C,.Dim=c(20,5)),
            s_count_T = structure(.Data=spors_T,.Dim=c(20,5))#,
)

stan_rdump(ls(data1), "Ellie.dataALLOriginals.R", envir=list2env(data1))
fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\mice.censored_sp.stan", data=data1,
             iter=1000, chains=1)


##########################################################################
##
## 5. Figure 1: The experiemental set up
##  
##
###########################################################################
library(adegenet)
par(mar=c(5,2,1,1))
par(mfrow=c(1,1))
x<-seq(-10,100,1);y <- seq(-50,100,length=length(x))
plot(x,y,pch="",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")
polygon(c(c(-10,5), rev(c(-10,5))),
        c(c(70,70),
          rev(c(90,90))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
text(-2.5,86,"5 mice:",col="grey")
text(-2.5,82,"injected with",col="grey")
text(-2.5,78,expression(paste(10^bold("7"), " to", paste(10^bold("8")))),col="grey")
text(-2.5,74,substitute(paste(italic('P. berghei'),)),col="grey")

text(10,82,"Mice",col="red")
text(10,79,"treated",col="red")

arrows(15,80, x1 = 20, y1 = 80, length = 0.25, angle = 30,
       code = 2, col = "grey", lty = 1,
       lwd = 3)
polygon(c(c(20,35), rev(c(20,35))),
        c(c(70,70),
          rev(c(90,90))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
text(27.5,95,"500 niave mosquito:",col = "blue")
text(27.5,92,"feeding on",col = "blue")
text(27.5,89,"all 5 mice",col = "blue")
text(27.5,86,"at random",col = "blue")
polygon(c(c(20,35), rev(c(20,35))),
        c(c(80,80),
          rev(c(100,100))),
        border=NA, col=transp("bisque1",alpha=0.2))
arrows(35,90, x1 = 40, y1 = 90, length = 0.25, angle = 30,
       code = 2, col = "blue", lty = 1,
       lwd = 3)
polygon(c(c(40,50), rev(c(40,50))),
        c(c(80,80),
          rev(c(100,100))),
        border=NA, col=transp("bisque1",alpha=0.2))
text(45,95,substitute(paste(italic('m')," mosquito")),col = "blue")
text(45,92,"feeding on",col = "blue")
text(45,89,"1 naive",col = "blue")
text(45,86,"mouse",col = "blue")
polygon(c(c(40,50), rev(c(40,50))),
        c(c(70,70),
          rev(c(90,90))),
        border=NA, col=transp("aquamarine2",alpha=0.2))
text(55,85,"x 5", cex=4,col="aquamarine2")
arrows(60,80, x1 = 65, y1 = 80, length = 0.25, angle = 30,
       code = 2,col="grey", lty = 1,
       lwd = 3)
polygon(c(c(65,80), rev(c(65,80))),
        c(c(70,70),
          rev(c(90,90))),
        border=NA, col=transp("aquamarine2",alpha=0.2))
text(72.5,85,"Mouse blood sampled:")
text(72.5,82,"% gametocytemia")
text(72.5,79,"% parasitemia")

arrows(75,70, x1 = 30, y1 = 60, length = 0.25, angle = 20,
       code = 2, col = "grey", lty = 1,
       lwd = 3)

polygon(c(c(20,35), rev(c(20,35))),
        c(c(30,30),
          rev(c(50,50))),
        border=NA, col=transp("aquamarine2",alpha=0.2))
polygon(c(c(20,35), rev(c(20,35))),
        c(c(40,40),
          rev(c(60,60))),
        border=NA, col=transp("bisque2",alpha=0.2))
text(27.5,55,"500 niave mosquito:",col = "blue")
text(27.5,52,"feeding on",col = "blue")
text(27.5,49,"all 5 mice",col = "blue")
text(27.5,46,"at random",col = "blue")
arrows(35,50, x1 = 40, y1 = 50, length = 0.25, angle = 30,
       code = 2, col = "blue", lty = 1,
       lwd = 3)
polygon(c(c(40,50), rev(c(40,50))),
        c(c(40,40),
          rev(c(60,60))),
        border=NA, col=transp("bisque2",alpha=0.2))
text(45,55,substitute(paste(italic('m')," mosquito")),col = "blue")
text(45,52,"feeding on",col = "blue")
text(45,49,"1 naive",col = "blue")
text(45,46,"mouse",col = "blue")
polygon(c(c(40,50), rev(c(40,50))),
        c(c(30,30),
          rev(c(50,50))),
        border=NA, col=transp("aquamarine3",alpha=0.2))
text(55,45,"x 5", cex=4,col="aquamarine3")
arrows(60,40, x1 = 65, y1 = 40, length = 0.25, angle = 30,
       code = 2,col="grey", lty = 1,
       lwd = 3)
polygon(c(c(65,80), rev(c(65,80))),
        c(c(30,30),
          rev(c(50,50))),
        border=NA, col=transp("aquamarine3",alpha=0.2))
text(72.5,45,"Mouse blood sampled:")
text(72.5,42,"% gametocytemia")
text(72.5,39,"% parasitemia")

arrows(75,30, x1 = 30, y1 = 20, length = 0.25, angle = 20,
       code = 2, col = "grey", lty = 1,
       lwd = 3)

polygon(c(c(20,35), rev(c(20,35))),
        c(c(-10,-10),
          rev(c(10,10))),
        border=NA, col=transp("aquamarine2",alpha=0.2))
text(27.5,15,"500 niave mosquito:",col = "blue")
text(27.5,12,"feeding on",col = "blue")
text(27.5,9,"all 5 mice",col = "blue")
text(27.5,6,"at random",col = "blue")
polygon(c(c(20,35), rev(c(20,35))),
        c(c(0,0),
          rev(c(20,20))),
        border=NA, col=transp("bisque3",alpha=0.2))
arrows(35,10, x1 = 40, y1 = 10, length = 0.25, angle = 30,
       code = 2, col = "blue", lty = 1,
       lwd = 3)
polygon(c(c(40,50), rev(c(40,50))),
        c(c(0,0),
          rev(c(20,20))),
        border=NA, col=transp("bisque3",alpha=0.2))
text(45,15,substitute(paste(italic('m')," mosquito")),col = "blue")
text(45,12,"feeding on",col = "blue")
text(45,9,"1 naive",col = "blue")
text(45,6,"mouse",col = "blue")
polygon(c(c(40,50), rev(c(40,50))),
        c(c(-10,-10),
          rev(c(10,10))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
text(55,5,"x 5", cex=4,col="aquamarine4")
arrows(60,0, x1 = 65, y1 = 0, length = 0.25, angle = 30,
       code = 2,col="grey", lty = 1,
       lwd = 3)
polygon(c(c(65,80), rev(c(65,80))),
        c(c(-10,-10),
          rev(c(10,10))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
text(72.5,5,"Mouse blood sampled:")
text(72.5,2,"% gametocytemia")
text(72.5,-1,"% parasitemia")

###
arrows(75,-10, x1 = 30, y1 = -20, length = 0.25, angle = 20,
       code = 2, col = "grey", lty = 1,
       lwd = 3)

polygon(c(c(20,35), rev(c(20,35))),
        c(c(-50,-50),
          rev(c(-30,-30))),
        border=NA, col=transp("aquamarine2",alpha=0.2))
text(27.5,-25,"500 niave mosquito:",col = "blue")
text(27.5,-28,"feeding on",col = "blue")
text(27.5,-31,"all 5 mice",col = "blue")
text(27.5,-34,"at random",col = "blue")
polygon(c(c(20,35), rev(c(20,35))),
        c(c(-40,-40),
          rev(c(-20,-20))),
        border=NA, col=transp("bisque3",alpha=0.2))
arrows(35,-30, x1 = 40, y1 = -30, length = 0.25, angle = 30,
       code = 2, col = "blue", lty = 1,
       lwd = 3)
polygon(c(c(40,50), rev(c(40,50))),
        c(c(-40,-40),
          rev(c(-20,-20))),
        border=NA, col=transp("bisque3",alpha=0.2))
text(45,-25,substitute(paste(italic('m')," mosquito")),col = "blue")
text(45,-28,"feeding on",col = "blue")
text(45,-31,"1 naive",col = "blue")
text(45,-34,"mouse",col = "blue")
polygon(c(c(40,50), rev(c(40,50))),
        c(c(-50,-50),
          rev(c(-30,-30))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
text(55,-35,"x 5", cex=4,col="aquamarine4")
arrows(60,-40, x1 = 65, y1 = -40, length = 0.25, angle = 30,
       code = 2,col="grey", lty = 1,
       lwd = 3)
polygon(c(c(65,80), rev(c(65,80))),
        c(c(-50,-50),
          rev(c(-30,-30))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
text(72.5,-35,"Mouse blood sampled:")
text(72.5,-38,"% gametocytemia")
text(72.5,-41,"% parasitemia")
###

text(90,85,substitute(paste(italic('i')," = 1")), cex=4,col="blue")
text(90,45,substitute(paste(italic('i')," = 2")), cex=4,col="blue")
text(90,5,substitute(paste(italic('i')," = 3")), cex=4,col="blue")
text(90,-35,substitute(paste(italic('i')," = 4")), cex=4,col="blue")

text(10,52,"Mice",col="red")
text(10,49,"treated",col="red")
arrows(15,50, x1 = 20, y1 = 50, length = 0.25, angle = 30,
       code = 2, col = "grey", lty = 1,
       lwd = 3)

text(10,12,"Mice",col="red")
text(10,9,"treated",col="red")
arrows(15,10, x1 = 20, y1 = 10, length = 0.25, angle = 30,
       code = 2, col = "grey", lty = 1,
       lwd = 3)

text(10,-28,"Mice",col="red")
text(10,-31,"treated",col="red")
arrows(15,-30, x1 = 20, y1 = -30, length = 0.25, angle = 30,
       code = 2, col = "grey", lty = 1,
       lwd = 3)

par(las=1)
axis(1,at=c(0,10,40,65,100),
     labels=c("Day -10","Day 0","Day 21: mosquitoes infectious",
              "Day 31: Mice infectious",""),cex.axis=1.2)
