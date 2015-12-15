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

###########################
##
## Sporzoites, parasitemia, gametocytemia, mouse prevalence
##
##
###############################
spors<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=TRUE)
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)

for (i in 1:nrow(spors)){
   spors$sumspors[i] <- sum(c(spors[i,6],spors[i,7],spors[i,8],spors[i,9],spors[i,10]),na.rm=T)
   }

for (i in 1:nrow(spors)){
spors$sumspors2[i] <- sum(c(ifelse(spors[i,6]>0,1,spors[i,6]),
                           ifelse(spors[i,7]>0,1,spors[i,7]),
                           ifelse(spors[i,8]>0,1,spors[i,8]),
                           ifelse(spors[i,9]>0,1,spors[i,9]),
                           ifelse(spors[i,10]>0,1,spors[i,10])),na.rm=T)
  }

mb0 = c(length(spors$prevBS[spors$sumspors2==0 & spors$prevBS==0 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==1 & spors$prevBS==0 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==2 & spors$prevBS==0 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==3 & spors$prevBS==0 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==4 & spors$prevBS==0 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==5 & spors$prevBS==0 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==0 & spors$prevBS==0 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==1 & spors$prevBS==0 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==2 & spors$prevBS==0 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==3 & spors$prevBS==0 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==4 & spors$prevBS==0 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==5 & spors$prevBS==0 & spors$Treatment==1]))

mb1 = c(length(spors$prevBS[spors$sumspors2==0 & spors$prevBS==1 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==1 & spors$prevBS==1 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==2 & spors$prevBS==1 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==3 & spors$prevBS==1 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==4 & spors$prevBS==1 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==5 & spors$prevBS==1 & spors$Treatment==0]),
        length(spors$prevBS[spors$sumspors2==0 & spors$prevBS==1 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==1 & spors$prevBS==1 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==2 & spors$prevBS==1 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==3 & spors$prevBS==1 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==4 & spors$prevBS==1 & spors$Treatment==1]),
        length(spors$prevBS[spors$sumspors2==5 & spors$prevBS==1 & spors$Treatment==1]))


probofinfection_if_bit_byNmosqC <- mb1[1:6] / (mb0[1:6] + mb1[1:6])
probofinfection_if_bit_byNmosqT <- mb1[7:12] / (mb0[7:12] + mb1[7:12])
Num_inf_mosquitoes <- seq(0,5,1)

plot(probofinfection_if_bit_byNmosqC ~ Num_inf_mosquitoes)
lines(probofinfection_if_bit_byNmosqT ~ Num_inf_mosquitoes)
##regardless of the intensity of infection in the mosquito

sporsCons <- subset(spors, Treatment == 0)
sporstreat <- subset(spors, Treatment == 1)
indiv_mosqC <- sort(c(stack(sporsCons[,5:10])$values))
indiv_mosqT <- sort(c(stack(sporstreat[,5:10])$values))
probability_mosq_is_infectedC <- length(indiv_mosqC[indiv_mosqC != 0])/length(indiv_mosqC)
probability_mosq_is_infectedT <- length(indiv_mosqT[indiv_mosqT != 0])/length(indiv_mosqT)

prob_inf_if_bittenC <- probofinfection_if_bit_byNmosqC * probability_mosq_is_infectedC

prob_inf_if_bittenT <- probofinfection_if_bit_byNmosqT * probability_mosq_is_infectedT

plot(prob_inf_if_bittenC ~ Num_inf_mosquitoes,
     ylim=c(0,1),xlim=c(0,5),
     ylab="Probability of host infection if bitten",#
     xlab="Number of mosquitoes biting")
points(prob_inf_if_bittenT ~ Num_inf_mosquitoes,pch=20)

perbiteprobC <- prob_inf_if_bittenC[2:6] / c(1,2,3,4,5)
perbiteprobT <- prob_inf_if_bittenT[2:6] / c(1,2,3,4,5)

plot(perbiteprobC ~ c(1,2,3,4,5),
     ylim=c(0,1),xlim=c(0,5),
     ylab="Per bite probability of host infection if bitten",#
     xlab="Number of mosquitoes biting")
points(perbiteprobT ~ c(1,2,3,4,5),pch=20)

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
  c(19.5,22.4,16.8,17.1,21.8),
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4],
  c(19.5,22.4,16.8,17.1,21.8),
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 4],
  c(19.5,22.4,16.8,17.1,21.8),
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 0 & spors$Round == 4],
  c(19.5,22.4,16.8,17.1,21.8),
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 0 & spors$Round == 4],
  c(19.5,22.4,16.8,17.1,21.8),
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
  c(18.2,17.4,25.6,19.5,18.5),
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4],
  c(18.2,17.4,25.6,19.5,18.5),
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==2 & spors$Treatment == 1 & spors$Round == 4],
  c(18.2,17.4,25.6,19.5,18.5),
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==3 & spors$Treatment == 1 & spors$Round == 4],
  c(18.2,17.4,25.6,19.5,18.5),
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 1],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 2],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 3],
  spors$Parasitemia[spors$Bites==4 & spors$Treatment == 1 & spors$Round == 4],
  c(18.2,17.4,25.6,19.5,18.5),
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


### Run Figures from model output_1.R FOR "data" ###
E = c(2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)
c0 = numeric(32); for (i in 284:315) { c0[i-283]<- mean(data[,i])}
b0 = numeric(32); for (i in 252:283) { b0[i-251]<- mean(data[,i])}
x0 = c(prevcon[5:20],prevtreat[5:20])
R0 = numeric(32)
for (i in 1:32){
R0[i] <- E[i] * c0[i] * b0[i] * ((1+x0[i])/x0[i])
}
plot(R0[1:4]~c(1,2,3,4),ylim=c(0,10))
lines(R0[1:4]~c(1,2,3,4))
lines(R0[5:8]~c(1,2,3,4))
lines(R0[9:12]~c(1,2,3,4))
lines(R0[13:16]~c(1,2,3,4))

lines(R0[17:20]~c(1,2,3,4),lty=2)
lines(R0[21:24]~c(1,2,3,4),lty=2)
lines(R0[25:28]~c(1,2,3,4),lty=2)
lines(R0[29:32]~c(1,2,3,4),lty=2)

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
legend(1,1.5,legend=c("Control data","Treatment data"),
       cex=1.4,bty="n",pch=c(19,1))
##########################################################################
##
## 4. Exploring the relationship between parasitemia and oocysts
##  
##
###########################################################################
parasORIGmean <- c(parasitORIGmean,parasitATV32mean)

spordat <- list(N=40,
                x=parasORIGmean,
                y=meanoocysts)
test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to paralinearfit.stan", data=spordat,
              iter=1000, chains=4)

print(test2)
print(waic(test2))

test3 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan2.stan", data=spordat,
              iter=1000, chains=4)

##****setting for logisticfunction in stan2
#parameters {
#  real<lower=0> phi; // 
#    real<lower=0, upper=2> beta;  // 
#    real<lower=10> alpha;
#}

print(test3)
print(waic(test3))

params2 = extract(test2);names(params2)
params3 = extract(test3);names(params3)
#rstan::traceplot(test3, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,100,1)
pred<-(mean(params2$alpha) + mean(params2$beta) * nc)
pred2 <- (mean(params3$alpha) * nc) / sqrt(1 + nc^1/mean(params3$beta))
par(mar=c(5,5,5,5))
par(las=1)
plot(meanoocysts~parasORIGmean,cex.lab=1.5,bty="n",xlim=c(0,25),
     ylab="Mean oocysts",xlab="Parasitemia (%)",ylim=c(0,100))
par(las=2);axis(2,at=seq(0,100,50),labels=seq(0,100,50))

lines(nc,pred,lwd=2,lty=2,col="red")
lines(nc,pred2,lwd=2,lty=2,col="blue")

x <- seq(0,20,0.1)

e <- extract(test3, pars = c("alpha", "beta"))

for(i in seq_along(e[[1]])) {
  lines(x, ((e[[1]][i] * x)/sqrt(1 + x ^ 1/e[[2]][i])), col = "#00000008")
}
lines(nc,pred2,lwd=2,lty=2,col="red")

##########################################################################
##
## 5. Exploring the relationship between sporozoites and oocysts
##  
##
###########################################################################
spordat <- list(N=40,
                x=meanoocysts,
                y=MEANsporig)
test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to paralinearfit.stan", data=spordat,
              iter=1000, chains=4)

print(test2)
print(waic(test2))

test3 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan2.stan", data=spordat,
              iter=1000, chains=4)
##***settings for logisticfunction in stan2
#parameters {
#  real<lower=0> phi; // 
#    real<lower=0.1, upper=0.3> beta;  // 
#    real<lower=0.4, upper=0.6> alpha;
#}

print(test3)
print(waic(test3))

params2 = extract(test2);names(params2)
params3 = extract(test3);names(params3)
#rstan::traceplot(test2, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,100,1)
pred<-(mean(params2$alpha) + mean(params2$beta) * nc)
pred2 <- (mean(params3$alpha) * nc) / sqrt(1 + nc^1/mean(params3$beta))
par(mar=c(5,5,5,5))
par(las=1)
plot(MEANsporig~meanoocysts,cex.lab=1.5,bty="n",xlim=c(0,60),yaxt="n",
     xlab="Mean oocysts",ylab="Mean sporozoite post bite scores",ylim=c(0,3))
par(las=2);axis(2,at=seq(0,3,0.5),labels=seq(0,3,0.5))

#lines(nc,pred,lwd=2,lty=2,col="red")
lines(nc,pred2,lwd=2,lty=2,col="blue")

x <- seq(0,max(meanoocysts),0.1)

e <- extract(test3, pars = c("alpha", "beta"))

for(i in seq_along(e[[1]])) {
  lines(x, ((e[[1]][i] * x)/sqrt(1 + x ^ 1/e[[2]][i])), col = "#00000008")
}
lines(nc,pred2,lwd=2,lty=2,col="red")


##########################################################################
##
## 6. Fit the distributions of the data and estimate the real sporozoite counts and prevalence 
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
             iter=1000, chains=2)


##########################################################################
##
## 4. Fit the distributions of the data and estimate the prevalence 
##    with and without treatment to get the difference in the probaility of transmission
##    for the simple model
##
###########################################################################
oocyC <- ifelse(oocystsC < 1, 0, 1)
oocyT <- ifelse(oocystsT < 1, 0, 1)

data2<-list(N_C=16,
            N_T=16,
            N_mosq=24,
            N_mice=5,
            ooc_prev_C = structure(.Data = c(oocystsC[1:384]),
                                    .Dim=c(24,16)),
            ooc_prev_T = structure(.Data = c(oocystsT[1:384]),
                                    .Dim=c(24,16)),
            prev_C = structure(.Data =PREV_C[,1:16],.Dim=c(5,16)),
            prev_T = structure(.Data =PREV_T[,1:16],.Dim=c(5,16)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,1002),
            sporo_count_C = structure(.Data=spors_C[1:16,],.Dim=c(16,5)),
            sporo_count_T = structure(.Data=spors_T[1:16,],.Dim=c(16,5))
            )

stan_rdump(ls(data2), "Ellie.databites1to4Originals.R", envir=list2env(data2))


fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\mice.censored_sp_prevalence only04092015.stan", data=data2,
             iter=1000, chains=1)

##########################################################################
##
## 5. Figure 1: The experiemental set up
##  
##
###########################################################################
library(adegenet)
library(plotrix)
#dev.off()
#split.screen(rbind(
#  c(0,1,0.4, 1), 
#  c(0,1,0,0.4)))
par(mar=c(0,0,0,0))
#screen(1)
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
text(27.5,95,"500 naive mosquito:",col = "blue")
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
text(55,85,"x 5", cex=2,col="aquamarine2")
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
text(27.5,55,"500 naive mosquito:",col = "blue")
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
text(55,45,"x 5", cex=2,col="aquamarine3")
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
text(27.5,15,"500 naive mosquito:",col = "blue")
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
text(55,5,"x 5", cex=2,col="aquamarine4")
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
text(27.5,-25,"500 naive mosquito:",col = "blue")
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
text(55,-35,"x 5", cex=2,col="aquamarine4")
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

text(90,85,substitute(paste(italic('i')," = 1")), cex=2,col="blue")
text(90,45,substitute(paste(italic('i')," = 2")), cex=2,col="blue")
text(90,5,substitute(paste(italic('i')," = 3")), cex=2,col="blue")
text(90,-35,substitute(paste(italic('i')," = 4")), cex=2,col="blue")

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


#screen(2)

plot(c(0, 100), c(0, 100), type = "n",bty="n",xaxt="n",yaxt="n",
     ylab="",xlab="")
radius <- 2
radius2 <- 3
theta <- seq(20, 2 * pi, length = 200)
#draw.ellipse(c(30,70), c(80,80), c(5,10), c(10,5))

polygon(c(c(5,20), rev(c(5,20))),
        c(c(10,10),
          rev(c(80,80))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
polygon(c(c(5,20), rev(c(5,20))),
        c(c(10,10),
          rev(c(40,40))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
polygon(c(c(5,20), rev(c(5,20))),
        c(c(10,10),
          rev(c(30,30))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
polygon(c(c(5,20), rev(c(5,20))),
        c(c(10,10),
          rev(c(20,20))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
lines(x = radius2 * cos(theta)+10, y = radius * sin(theta)+30,col="red")
lines(x = radius2 * cos(theta)+15, y = radius * sin(theta)+30,col="red")

text((20+5)/2,75,"Initial parasitemia")
text(25/2,71,"distribution" )
text(10,30,expression(paste(mu[im], )))
text(15,30,expression(paste(phi[im], )))
text(12.5,25,expression(paste(italic("hierarchical"))))
text(12.5,22,expression(paste(italic("structure"))))

text(25/2,60,expression(paste(italic("~ Binomial"))))
text(25/2+2,57, "(zero-inflated)")

arrows(20,50, x1 = 25, y1 = 50, length = 0.25, angle = 30,
       code = 2, col = "grey", lty = 1,
       lwd = 3)
text(22,53,expression(paste(alpha^I, )))
text(22,47,expression(paste(beta^I, )))

polygon(c(c(25,40), rev(c(25,40))),
        c(c(10,10),
          rev(c(80,80))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
lines(x = radius2 * cos(theta)+30, y = radius * sin(theta)+30,col="red")
lines(x = radius2 * cos(theta)+35, y = radius * sin(theta)+30,col="red")

text((25+40)/2,75,"Oocyst distribution")
text(30,30,expression(paste(mu[ooc], )))
text(35,30,expression(paste(phi[ooc], )))
text((25+40)/2,60,expression(paste(italic("~ Negative binomial"))))


arrows(40,50, x1 = 45, y1 = 50, length = 0.25, angle = 30,
       code = 2, col = "grey", lty = 1,
       lwd = 3)
text(42,53,expression(paste(alpha^II, )))
text(42,47,expression(paste(beta^II, )))

polygon(c(c(45,60), rev(c(45,60))),
        c(c(10,10),
          rev(c(80,80))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
lines(x = radius2 * cos(theta)+50, y = radius * sin(theta)+30,col="red")
lines(x = radius2 * cos(theta)+55, y = radius * sin(theta)+30,col="red")

text((45+60)/2,75,"Sporozoite" )
     text((45+60)/2,71,"(binned) distribution")
text(50,30,expression(paste(mu[ooc], )))
text(55,30,expression(paste(phi[ooc], )))
text((45+60)/2,60,expression(paste(italic("~ Multinomial"))))

arrows(60,50, x1 = 70, y1 = 50, length = 0.25, angle = 30,
       code = 2, col = "grey", lty = 1,
       lwd = 3)
text(65,53,expression(paste(alpha^III, )))
text(65,47,expression(paste(beta^III, )))

polygon(c(c(70,100), rev(c(70,100))),
        c(c(10,10),
          rev(c(80,80))),
        border=NA, col=transp("aquamarine4",alpha=0.2))
radius3=5
lines(x = radius3 * cos(theta)+85, y = radius * sin(theta)+30,col="red",lwd=3)

text(170/2,75,"Final Parasitemia" )
text(170/2,71,"frequency" )
text(170/2,60,expression(paste(italic("~ Binomial"))))

text(85,30,expression(paste(theta[mouse], )))


##########################################################################
##
## 8. Mouse to mouse data
##  
##
###########################################################################

##OOCYSTS
oocystsC_2<-c(##parasitemia mouse 1 seeding oocysts
  oocysts$oocystsbites1control[oocysts$round=="day10"][1:24],
  oocysts$oocystsbites1control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites1control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites1control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites2control[oocysts$round=="day10"][1:24],
  oocysts$oocystsbites2control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites2control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites2control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites3control[oocysts$round=="day10"][1:24],
  oocysts$oocystsbites3control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites3control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites3control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites4control[oocysts$round=="day10"][1:24],
  oocysts$oocystsbites4control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites4control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites4control[oocysts$round=="day103"][1:24],
  oocysts$oocystsbites5control[oocysts$round=="day10"][1:24],
  oocysts$oocystsbites5control[oocysts$round=="day41"][1:24],
  oocysts$oocystsbites5control[oocysts$round=="day72"][1:24],
  oocysts$oocystsbites5control[oocysts$round=="day103"][1:24])

oocystsT_2<-c(oocysts$oocystsbites1atv[oocysts$round=="day10"][1:24],
              oocysts$oocystsbites1atv[oocysts$round=="day41"][1:24],
              oocysts$oocystsbites1atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites1atv[oocysts$round=="day103"][1:24],
            oocysts$oocystsbites2atv[oocysts$round=="day10"][1:24],
            oocysts$oocystsbites2atv[oocysts$round=="day41"][1:24],
            oocysts$oocystsbites2atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites2atv[oocysts$round=="day103"][1:24],
            oocysts$oocystsbites3atv[oocysts$round=="day10"][1:24],
            oocysts$oocystsbites3atv[oocysts$round=="day41"][1:24],
            oocysts$oocystsbites3atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites3atv[oocysts$round=="day103"][1:24],
            oocysts$oocystsbites4atv[oocysts$round=="day10"][1:24],
            oocysts$oocystsbites4atv[oocysts$round=="day41"][1:24],
            oocysts$oocystsbites4atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites4atv[oocysts$round=="day103"][1:24],
            oocysts$oocystsbites5atv[oocysts$round=="day10"][1:24],
            oocysts$oocystsbites5atv[oocysts$round=="day41"][1:24],
            oocysts$oocystsbites5atv[oocysts$round=="day72"][1:24],
            oocysts$oocystsbites5atv[oocysts$round=="day103"][1:24])


##############*******************Not needed now!!!############################
###sPOROZOITES
sporsbites1<-subset(spors,Bites==1 & Treatment==0);sporsbites1
newb1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==2]);
spb1r2<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
newc1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==3]);
spb1r3<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
newd1<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==4]);
spb1r4<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))


sporsbites2<-subset(spors,Bites==2 & Treatment==0);sporsbites2
b1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==2],sporsbites2$Sporozoite2[sporsbites2$Round==2]);
spb2r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
c1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==3],sporsbites2$Sporozoite2[sporsbites2$Round==3]);
spb2r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
d1<-sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==4],sporsbites2$Sporozoite2[sporsbites2$Round==4]);
spb2r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))


sporsbites3<-subset(spors,Bites==3 & Treatment==0);sporsbites3
ff1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==2],sporsbites3$Sporozoite2[sporsbites3$Round==2],sporsbites3$Sporozoite3[sporsbites3$Round==2]);
spb3r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
g1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==3],sporsbites3$Sporozoite2[sporsbites3$Round==3],sporsbites3$Sporozoite3[sporsbites3$Round==3]);
spb3r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
h1<-sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==4],sporsbites3$Sporozoite2[sporsbites3$Round==4],sporsbites3$Sporozoite3[sporsbites3$Round==4]);
spb3r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites4<-subset(spors,Bites==4 & Treatment==0);sporsbites4
kk1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==2],sporsbites4$Sporozoite2[sporsbites4$Round==2],sporsbites4$Sporozoite3[sporsbites4$Round==2],sporsbites4$Sporozoite4[sporsbites4$Round==2]);
spb4r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
l1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==3],sporsbites4$Sporozoite2[sporsbites4$Round==3],sporsbites4$Sporozoite3[sporsbites4$Round==3],sporsbites4$Sporozoite4[sporsbites4$Round==3]);
spb4r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
m1<-sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==4],sporsbites4$Sporozoite2[sporsbites4$Round==4],sporsbites4$Sporozoite3[sporsbites4$Round==4],sporsbites4$Sporozoite4[sporsbites4$Round==4]);
spb4r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites5<-subset(spors,Bites==5 & Treatment==0);sporsbites5
oo1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==2],sporsbites5$Sporozoite2[sporsbites5$Round==2],sporsbites5$Sporozoite3[sporsbites5$Round==2],sporsbites5$Sporozoite4[sporsbites5$Round==2],sporsbites5$Sporozoite5[sporsbites5$Round==2]);
spb5r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
pp1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==3],sporsbites5$Sporozoite2[sporsbites5$Round==3],sporsbites5$Sporozoite3[sporsbites5$Round==3],sporsbites5$Sporozoite4[sporsbites5$Round==3],sporsbites5$Sporozoite5[sporsbites5$Round==3]);
spb5r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
qq1<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==4],sporsbites5$Sporozoite2[sporsbites5$Round==4],sporsbites5$Sporozoite3[sporsbites5$Round==4],sporsbites5$Sporozoite4[sporsbites5$Round==4],sporsbites5$Sporozoite5[sporsbites5$Round==4]);
spb5r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

spors_C_2<-rbind(spb1r2,spb1r3,spb1r4,
               spb2r2,spb2r3,spb2r4,
               spb3r2,spb3r3,spb3r4,
               spb4r2,spb4r3,spb4r4,
               spb5r2,spb5r3,spb5r4)

sporsbites1<-subset(spors,Bites==1 & Treatment==1);sporsbites1
new_b<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==2]);
spb1r2<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
new_c<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==3]);
spb1r3<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))
new_d<-sporsb1rd1<-c(sporsbites1$Sporozoite1[sporsbites1$Round==4]);
spb1r4<-c(length(sporsb1rd1[sporsb1rd1==0]),length(sporsb1rd1[sporsb1rd1==1]),length(sporsb1rd1[sporsb1rd1==2]),length(sporsb1rd1[sporsb1rd1==3]),length(sporsb1rd1[sporsb1rd1==4]))


sporsbites2<-subset(spors,Bites==2 & Treatment==1);sporsbites2
b<-  sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==2],sporsbites2$Sporozoite2[sporsbites2$Round==2]);
spb2r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
c<-    sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==3],sporsbites2$Sporozoite2[sporsbites2$Round==3]);
spb2r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
d<-    sporsb2rd1<-c(sporsbites2$Sporozoite1[sporsbites2$Round==4],sporsbites2$Sporozoite2[sporsbites2$Round==4]);
spb2r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))


sporsbites3<-subset(spors,Bites==3 & Treatment==1);sporsbites3
ff<-  sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==2],sporsbites3$Sporozoite2[sporsbites3$Round==2],sporsbites3$Sporozoite3[sporsbites3$Round==2]);
spb3r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
g<-  sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==3],sporsbites3$Sporozoite2[sporsbites3$Round==3],sporsbites3$Sporozoite3[sporsbites3$Round==3]);
spb3r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
h<- sporsb2rd1<-c(sporsbites3$Sporozoite1[sporsbites3$Round==4],sporsbites3$Sporozoite2[sporsbites3$Round==4],sporsbites3$Sporozoite3[sporsbites3$Round==4]);
spb3r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites4<-subset(spors,Bites==4 & Treatment==1);sporsbites4
kk<-  sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==2],sporsbites4$Sporozoite2[sporsbites4$Round==2],sporsbites4$Sporozoite3[sporsbites4$Round==2],sporsbites4$Sporozoite4[sporsbites4$Round==2]);
spb4r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
l<-  sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==3],sporsbites4$Sporozoite2[sporsbites4$Round==3],sporsbites4$Sporozoite3[sporsbites4$Round==3],sporsbites4$Sporozoite4[sporsbites4$Round==3]);
spb4r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
m<- sporsb2rd1<-c(sporsbites4$Sporozoite1[sporsbites4$Round==4],sporsbites4$Sporozoite2[sporsbites4$Round==4],sporsbites4$Sporozoite3[sporsbites4$Round==4],sporsbites4$Sporozoite4[sporsbites4$Round==4]);
spb4r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

sporsbites5<-subset(spors,Bites==5 & Treatment==1);sporsbites5
oo<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==2],sporsbites5$Sporozoite2[sporsbites5$Round==2],sporsbites5$Sporozoite3[sporsbites5$Round==2],sporsbites5$Sporozoite4[sporsbites5$Round==2],sporsbites5$Sporozoite5[sporsbites5$Round==2]);
spb5r2<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
pp<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==3],sporsbites5$Sporozoite2[sporsbites5$Round==3],sporsbites5$Sporozoite3[sporsbites5$Round==3],sporsbites5$Sporozoite4[sporsbites5$Round==3],sporsbites5$Sporozoite5[sporsbites5$Round==3]);
spb5r3<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))
qq<-sporsb2rd1<-c(sporsbites5$Sporozoite1[sporsbites5$Round==4],sporsbites5$Sporozoite2[sporsbites5$Round==4],sporsbites5$Sporozoite3[sporsbites5$Round==4],sporsbites5$Sporozoite4[sporsbites5$Round==4],sporsbites5$Sporozoite5[sporsbites5$Round==4]);
spb5r4<-c(length(sporsb2rd1[sporsb2rd1==0]),length(sporsb2rd1[sporsb2rd1==1]),length(sporsb2rd1[sporsb2rd1==2]),length(sporsb2rd1[sporsb2rd1==3]),length(sporsb2rd1[sporsb2rd1==4]))

spors_T_2<-rbind(spb1r2,spb1r3,spb1r4,
               spb2r2,spb2r3,spb2r4,
               spb3r2,spb3r3,spb3r4,
               spb4r2,spb4r3,spb4r4,
               spb5r2,spb5r3,spb5r4)
####################################################



ooc_count_Ttemp = structure(.Data = c(oocystsT_2),
                            .Dim=c(24,20))
ooc_count_Ctemp = structure(.Data = c(oocystsC_2),
                            .Dim=c(24,20))

parasit2CONVERT<-round(parasit*1200)
parasitT2CONVERT<-round(parasitT*1200)

para_intC1 <- structure(.Data =c(parasit2CONVERT[,1:4],
                               parasit2CONVERT[,6:9],
                               parasit2CONVERT[,11:14],
                               parasit2CONVERT[,16:19],
                               parasit2CONVERT[,21:24]),.Dim=c(5,20))

para_intT1 <- structure(.Data =c(parasitT2CONVERT[,1:4],
                                 parasitT2CONVERT[,6:9],
                                 parasitT2CONVERT[,11:14],
                                 parasitT2CONVERT[,16:19],
                                 parasitT2CONVERT[,21:24]),.Dim=c(5,20))

para_endC1 <- structure(.Data =c(parasit2CONVERT[,2:5],
                                 parasit2CONVERT[,7:10],
                                 parasit2CONVERT[,12:15],
                                 parasit2CONVERT[,17:20],
                                 parasit2CONVERT[,22:25]),.Dim=c(5,20))

para_endT1 <- structure(.Data =c(parasitT2CONVERT[,2:5],
                                 parasitT2CONVERT[,7:10],
                                 parasitT2CONVERT[,12:15],
                                 parasitT2CONVERT[,17:20],
                                 parasitT2CONVERT[,22:25]),.Dim=c(5,20))
N_bite_C=5
N_round_C=4
N_bite_T=5
N_round_T=4
data2<-list(N_C=20,
            N_T=20,
            N_ooc=24,
            N_mice =5,
            N_bite=5,
            N_round=4,
            para_init_counts_C = structure(.Data=t(para_intC1),.Dim=c(20,5)),
            para_init_counts_T = structure(.Data=t(para_intT1),.Dim=c(20,5)),
            ooc_count_C = structure(.Data=t(ooc_count_Ctemp),.Dim=c(20,24)),
            ooc_count_T = structure(.Data=t(ooc_count_Ttemp),.Dim=c(20,24)),
            para_end_counts_C = structure(.Data=t(para_endC1),.Dim=c(20,5)),
            para_end_counts_T = structure(.Data=t(para_endT1),.Dim=c(20,5)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,1002),
            sporo_count_C = structure(.Data=spors_C,.Dim=c(20,5)),
            sporo_count_T = structure(.Data=spors_T,.Dim=c(20,5)),
            bite_C = c(rep(seq(1:N_bite_C),each=N_round_C)),
            round_C = c(rep(seq(1:N_round_C),N_bite_C)),
            bite_T = c(rep(seq(1:N_bite_T),each=N_round_T)),
            round_T = c(rep(seq(1:N_round_T),N_bite_T))
)

### e.g 17 bite array of x integers i.e if i put 17 in it gives me a specific

stan_rdump(ls(data2), "ElliedataMousetoMouse_CORRECTED.R", envir=list2env(data2))

fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\mice.censored_sp.stan", data=data1,
             iter=1000, chains=2)


#