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
oocystsC<-c(#oocysts$oocystsbites1control[oocysts$round=="day41"][1:24],
            #oocysts$oocystsbites1control[oocysts$round=="day72"][1:24],
            #oocysts$oocystsbites1control[oocysts$round=="day103"][1:24],
            #oocysts$oocystsbites1control[oocysts$round=="day134"][1:24],
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
            sum(sum(prevoocC[289:312])/24,sum(prevoocC[313:336])/24,sum(prevoocC[337:360])/24,sum(prevoocC[360:384])/24)/4)

prevooc1ConGens<-c(sum(prevoocC[1:24])/24,sum(prevoocC[25:48])/24,sum(prevoocC[49:72])/24,sum(prevoocC[73:96])/24,
            sum(prevoocC[97:120])/24,sum(prevoocC[121:144])/24,sum(prevoocC[145:168])/24,sum(prevoocC[169:192])/24,
            sum(prevoocC[193:216])/24,sum(prevoocC[217:240])/24,sum(prevoocC[241:264])/24,sum(prevoocC[265:288])/24,
            sum(prevoocC[289:312])/24,sum(prevoocC[313:336])/24,sum(prevoocC[337:360])/24,sum(prevoocC[360:384])/24)

#freqoocC<-numeric(length(unique(oocystsC)))
#oocystsC2<-sort(unique(oocystsC))
#for (i in 1:length(oocystsC2)){ 
#  freqoocC[i]<-sum(ifelse(oocystsC==unique(oocystsC2)[i],1,0))}
#probNooc<-numeric(length(freqoocC))
#for (j in 1:length(freqoocC)){
#probNooc[j]<-freqoocC[j]/sum(freqoocC)}
#freqdistoocC<-data.frame(oocystsC2,probNooc);colnames(freqdistoocC)[1]<-"Nooc"

oocystsT<-c(#oocysts$oocystsbites1atv[oocysts$round=="day41"][1:24],
            #oocysts$oocystsbites1atv[oocysts$round=="day72"][1:24],
            #oocysts$oocystsbites1atv[oocysts$round=="day103"][1:24],
            #oocysts$oocystsbites1atv[oocysts$round=="day134"][1:24],
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
            sum(prevoocT[289:312])/24,sum(prevoocT[313:336])/24,sum(prevoocT[337:360])/24,sum(prevoocT[360:384])/24)

prevoocT<-c(sum(sum(prevoocT[1:24])/24,sum(prevoocT[25:48])/24,sum(prevoocT[49:72])/24,sum(prevoocT[73:96])/24)/4,
            sum(sum(prevoocT[97:120])/24,sum(prevoocT[121:144])/24,sum(prevoocT[145:168])/24,sum(prevoocT[169:192])/24)/4,
            sum(sum(prevoocT[193:216])/24,sum(prevoocT[217:240])/24,sum(prevoocT[241:264])/24,sum(prevoocT[265:288])/24)/4,
            sum(sum(prevoocT[289:312])/24,sum(prevoocT[313:336])/24,sum(prevoocT[337:360])/24,sum(prevoocT[360:384])/24)/4)

EffOoc<-(prevooc1-prevoocT)/prevooc1

EfficacyChurcher2012<-((mean(prevooc1)/mean(meanoocysts[1:16]))-(mean(prevoocT)/mean(meanoocysts[17:32])))/
  (mean(prevooc1)/mean(meanoocysts[1:16]))
#freqoocT<-numeric(length(unique(oocystsT)))
#oocystsT2<-sort(unique(oocystsT))
#for (i in 1:length(oocystsT2)){ 
#  freqoocT[i]<-sum(ifelse(oocystsT==unique(oocystsT2)[i],1,0))}
#probNoocT<-numeric(length(freqoocT))
#for (j in 1:length(freqoocT)){
#  probNoocT[j]<-freqoocT[j]/sum(freqoocT)}
#freqdistoocT<-data.frame(oocystsT2,probNoocT);colnames(freqdistoocT)[1]<-"NoocT"

meanoocysts<-c(#mean(oocysts$oocystsbites1control[oocysts$round=="day41"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1control[oocysts$round=="day72"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1control[oocysts$round=="day103"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1control[oocysts$round=="day134"],na.rm=TRUE),
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
               #mean(oocysts$oocystsbites1atv[oocysts$round=="day41"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1atv[oocysts$round=="day72"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1atv[oocysts$round=="day103"],na.rm=TRUE),
               #mean(oocysts$oocystsbites1atv[oocysts$round=="day134"],na.rm=TRUE),
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


spors<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=TRUE)
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)
#spors[10:40,]
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


par(mar=c(5,5,2,2))
boxplot(parasit[,1],parasitT[,1],parasit[,2],parasitT[,2],parasit[,3],parasitT[,3],parasit[,4],parasitT[,4],
        parasit[,5],parasitT[,5],parasit[,6],parasitT[,6],parasit[,7],parasitT[,7],parasit[,8],parasitT[,8],
        parasit[,9],parasitT[,9],parasit[,10],parasitT[,10],parasit[,11],parasitT[,11],parasit[,12],parasitT[,12],
        parasit[,13],parasitT[,13],parasit[,14],parasitT[,14],parasit[,15],parasitT[,15],parasit[,16],parasitT[,16],
        col=c(rep(c("darkred","red"),4),rep(c("darkgreen","green"),4),
              rep(c("darkblue","blue"),4),rep(c("grey15","grey67"),4)),frame=F,xaxt="n",
        ylab="Parasitemia (%)",cex.lab=2,xlab="Generations ")
legend(0,25.5,title="Mosquito to mouse biting ratio",legend=c("2","3","4","5","control (dark)","treatment (light)"),
       col=c("red","green","blue","grey","grey15","grey67"),pch=15,lty=c(rep(NA,4),2,2),bty="n",
       cex=1.4)
axis(1,at=seq(1.5,31.5,2),labels=c(rep(seq(1,4,1),4)))

parasite2NoNegs <- ifelse(parasit > 0, parasit, NA)
parasite2NoNegsT <- ifelse(parasitT > 0, parasitT, NA)

par(mar=c(5,5,2,2))
boxplot(parasite2NoNegs[,1],parasite2NoNegsT[,1],parasite2NoNegs[,2],parasite2NoNegsT[,2],parasite2NoNegs[,3],parasite2NoNegsT[,3],parasite2NoNegs[,4],parasite2NoNegsT[,4],
        parasite2NoNegs[,5],parasite2NoNegsT[,5],parasite2NoNegs[,6],parasite2NoNegsT[,6],parasite2NoNegs[,7],parasite2NoNegsT[,7],parasite2NoNegs[,8],parasite2NoNegsT[,8],
        parasite2NoNegs[,9],parasite2NoNegsT[,9],parasite2NoNegs[,10],parasite2NoNegsT[,10],parasite2NoNegs[,11],parasite2NoNegsT[,11],parasite2NoNegs[,12],parasite2NoNegsT[,12],
        parasite2NoNegs[,13],parasite2NoNegsT[,13],parasite2NoNegs[,14],parasite2NoNegsT[,14],parasite2NoNegs[,15],parasite2NoNegsT[,15],parasite2NoNegs[,16],parasite2NoNegsT[,16],
        col=c(rep(c("darkred","red"),4),rep(c("darkgreen","green"),4),
              rep(c("darkblue","blue"),4),rep(c("grey15","grey67"),4)),na.rm=T,frame=F,xaxt="n",
        ylab="Parasitemia (%)",cex.lab=2,xlab="Generations ")
legend(0,25.5,title="Mosquito to mouse biting ratio",legend=c("2","3","4","5","control (dark)","treatment (light)"),
       col=c("red","green","blue","grey","grey15","grey67"),pch=15,lty=c(rep(NA,4),2,2),bty="n",
       cex=1.4)
axis(1,at=seq(1.5,31.5,2),labels=c(rep(seq(1,4,1),4)))

#parasitemT<-parasitemTUPP<-parasitemTLOW<-numeric(16)
#for (i in 1:16){
#  parasitemT[i]<-sum(parasitT[,i])/5
  
#  a<-numeric(10000)
#  for (j in 1:10000){ 
    
#    a[j]<-mean(sample(parasitT[,i],4))
#    parasitemTUPP[i]<-quantile(a,0.975)
#    parasitemTLOW[i]<-quantile(a,0.025)
#  }
#}
##MEAN Gametocytemia IN MICE
gametC<-cbind(
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

#gametoT<-gametoTUPP<-gametoTLOW<-numeric(16)
#for (i in 1:16){
#  gametoT[i]<-sum(gametT[,i])/5
#  a<-numeric(10000)
#  for (j in 1:10000){ 
    
#    a[j]<-mean(sample(gametT[,i],4))
#    gametoTUPP[i]<-quantile(a,0.975)
#    gametoTLOW[i]<-quantile(a,0.025)
#  }
#}
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
#spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 1],
#spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 2],
#spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 3],
#spors$prevBS[spors$Bites==1 & spors$Treatment == 0 & spors$Round == 4],
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
#  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1],
#  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2],
#  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3],
#  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4],
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

spors_C<-rbind(#spb1r1,spb1r2,spb1r3,spb1r4,
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
  
spors_T<-rbind(#spb1r1,spb1r2,spb1r3,spb1r4,
               spb2r1,spb2r2,spb2r3,spb2r4,
               spb3r1,spb3r2,spb3r3,spb3r4,
               spb4r1,spb4r2,spb4r3,spb4r4,
               spb5r1,spb5r2,spb5r3,spb5r4)
spors_Cprev<-spors_Tprev<-numeric(16)
for(i in 1:16){
spors_Cprev[i]<-1-(spors_C[i,1]/sum(spors_C[i,]))
spors_Tprev[i]<-1-(spors_T[i,1]/sum(spors_T[i,]))
}

effectSpors<-(spors_Cprev-spors_Tprev)/spors_Cprev
EFFSPmbr<-c(sum(effectSpors[1:4])/4,
            sum(effectSpors[5:8])/4,
            sum(effectSpors[9:12])/4,
            sum(effectSpors[13:16])/4)


MEANsporig<-c(#mean(newa1),mean(newb1),mean(newc1),mean(newd1),
          mean(a1),mean(b1),mean(c1),mean(d1),
            mean(ee1),mean(ff1),mean(g1),mean(h1),
            mean(jj1),mean(kk1),mean(l1),mean(m1),
            mean(nn1),mean(oo1),mean(pp1),mean(qq1),
          #mean(new_a),mean(new_b),mean(new_c),mean(new_d),  
          mean(a),mean(b),mean(c),mean(d),
            mean(ee),mean(ff),mean(g),mean(h),
            mean(jj),mean(kk),mean(l),mean(m),
            mean(nn),mean(oo),mean(pp),mean(qq))

prevcon<-prevtreat<-numeric(16)
  for (i in 1:16){
    prevcon[i]<-c(sum(PREV_C[,i])/5)
    prevtreat[i]<-c(sum(PREV_T[,i])/5)
    }

prevs<-c(prevcon,prevtreat)
prevmbrC<-c(sum(prevcon[1:4])/4,
           sum(prevcon[5:8])/4,
           sum(prevcon[9:12])/4,
           sum(prevcon[13:16])/4)
prevmbrT<-c(sum(prevtreat[1:4])/4,
            sum(prevtreat[5:8])/4,
            sum(prevtreat[9:12])/4,
            sum(prevtreat[13:16])/4)
meanprevpergroup<-(prevcon+prevtreat)/2
######################################
##
###
####  Data simple plots
###
###
##
######################################
par(mfrow=c(2,2))
parasitemia<-c(parasitem,parasitemT)
parasitemiaUPP<-c(parasitemUPP,parasitemTUPP)
parasitemiaLOW<-c(parasitemLOW,parasitemTLOW)
gametocytemia<-c(gametoC,gametoT)
gametocytemiaUPP<-c(gametoTUPP,gametoTUPP)
gametocytemiaLOW<-c(gametoTLOW,gametoTLOW)

asexual<-parasitemia-gametocytemia
asexualLOW<-parasitemiaUPP-gametocytemiaUPP
asexualUPP<-parasitemiaLOW-gametocytemiaLOW
plot(c(parasitem,parasitemT)~prevs,xlim=c(0,1),ylim=c(0,14))
data2<-data.frame(parasitemia,gametocytemia,prevs)
data2$asexpara<-data2$parasitemia-data2$gametocytemia##check what these represent

vec2<-seq(0,1,by=0.2)
boxplot(data2$parasitemia[data2$prevs==0],data2$parasitemia[data2$prevs==0.2],
        data2$parasitemia[data2$prevs==0.4],data2$parasitemia[data2$prevs==0.6],
        data2$parasitemia[data2$prevs==0.8],data2$parasitemia[data2$prevs==1],
        col="blue",xaxt="n",xlab="Mean Prevalence",ylab="Parasitemia (%)")
axis(1,at=seq(1,6,1),labels=vec2)

boxplot(data2$gametocytemia[data2$prevs==0],data2$gametocytemia[data2$prevs==0.2],
        data2$gametocytemia[data2$prevs==0.4],data2$gametocytemia[data2$prevs==0.6],
        data2$gametocytemia[data2$prevs==0.8],data2$gametocytemia[data2$prevs==1],
        col="blue",xaxt="n",xlab="Mean Prevalence",ylab="Gametocytemia (%)")
axis(1,at=seq(1,6,1),labels=vec2)


boxplot(data2$asexpara[data2$prevs==0],data2$asexpara[data2$prevs==0.2],
        data2$asexpara[data2$prevs==0.4],data2$asexpara[data2$prevs==0.6],
        data2$asexpara[data2$prevs==0.8],data2$asexpara[data2$prevs==1],
        col="blue",xaxt="n",xlab="Mean Prevalence",ylab="Asexual parasitemia (%)")
axis(1,at=seq(1,6,1),labels=vec2)

plot(parasitemia~gametocytemia)
points(parasitem~gametoC,col="red",pch=20)
points(parasitemT~gametoT,col="blue",pch=20)

summary.lm(lm(parasitemia~gametocytemia+0))

summary.lm(lm(parasitem~gametoC+0))
summary.lm(lm(parasitemT~gametoT+0))                

abline(lm(parasitem~gametoC+0),col="red",lty=2)
abline(lm(parasitemT~gametoT+0),col="blue",lty=2)

par(mfrow=c(1,1))
par(las=2)
plot(gametocytemia~asexual,xlab="Asexual parasites (%)",ylab="Gametocytes (%)",xaxt="n",cex.lab=1.2,pch="")
par(las=1)
axis(1,at=seq(0,10,1),labels=seq(0,10,1))

summary.lm(lm(gametocytemia~asexual+0))
summary(lm(gametocytemia[1:16]~asexual[1:16]+0))
summary(lm(gametocytemia[17:32]~asexual[17:32]+0))

pdat<-data.frame(x1 = seq(0,10,length=16))
model1<-lm(gametocytemia[17:32]~asexual[17:32]+0)
preds<-predict(model1,newdata=pdat,se.fit=TRUE)
sort(preds$fit)
upp<-sort(preds$fit)+sort(preds$se.fit)
low<-sort(preds$fit)-sort(preds$se.fit)

model2<-lm(gametocytemia[1:16]~asexual[1:16]+0)
preds2<-predict(model2,newdata=pdat,se.fit=TRUE)
sort(preds2$fit)
upp2<-sort(preds2$fit)+sort(preds2$se.fit)
low2<-sort(preds2$fit)-sort(preds2$se.fit)

polygon(c(sort(asexual[17:32]), rev(sort(asexual[17:32]))),c(low,
                                                 rev(upp)),border=NA, col="aquamarine1")

polygon(c(sort(asexual[1:16]), rev(sort(asexual[1:16]))),c(low2,
                                                             rev(upp2)),border=NA, col="aquamarine2")
points(gametocytemia[1:16]~asexual[1:16],col="black",pch=20)
points(gametocytemia[17:32]~asexual[17:32],col="grey70",pch=20)
abline(lm(gametocytemia[1:16]~asexual[1:16]+0),col="black",lty=2,xlim=c(0,5))
abline(lm(gametocytemia[17:32]~asexual[17:32]+0),col="grey70",lty=2,lwd=2)


predict(lm(gametocytemia[1:16]~asexual[1:16]+0))/asexual[1:16]
predict(lm(gametocytemia[17:32]~asexual[17:32]+0))/asexual[17:32]

allgametes<-c(gametocytemia,asexual)
typestage<-c(rep("gametocyte",32),rep("asexual",32))
expt<-c(rep("control",16),rep("treatment",16),rep("control",16),rep("treatment",16))
expt<-as.factor(expt)
mod1<-aov(gametocytemia~asexual*expt[1:32]+0)
mod2<-aov(gametocytemia~asexual+expt[1:32]+0)
anova(mod1,mod2)

reg.todo <- lm(gametocytemia~expt[1:32]/asexual - 1)
summary(reg.todo)



############################################################################
##
## 1. Fit for the mean distribution of sporozoite scores to oocyst counts
##
##
##
############################################################################
par(mfrow=c(2,1))
plot(MEANsp~meanoocysts,xlab="Mean oocysts",ylab="Mean sporozoite scores")
sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  d<-p.vec[4]
  
  pred<- (a * meanoocysts^c)/(d + b * meanoocysts^c)
  
  data1<-MEANsp
  
  loglik<- data1* log((pred)+0.001)+(1-data1)*log(1-((pred)-0.001))
  
  
  -sum(loglik,na.rm=T)
}
n.param<-4
satmod<-optim(c(0.9,0.2,0.6,4),sat.binom,method="L-BFGS-B",lower=c(0,0.2,0.5,4),upper=c(0.95,1,0.65,5))
satmod
satmod$par[1]<-0.9342045
satmod$par[2]<-0.2166749
satmod$par[3]<-0.5936314
satmod$par[4]<-4.1838088
nc<-seq(0,max(meanoocysts),1)
pred<-(satmod$par[1] * nc^satmod$par[3])/(satmod$par[4] + satmod$par[2] * nc^satmod$par[3]) 
lines(nc,pred,lwd=2,lty=3,col="red")

###Estimate sporozoites using this relationship
###
###ooc_count_C = structure(.Data = c(oocystsC),.Dim=c(24,16))##SEE BELOW LIST
###ooc_count_T = structure(.Data = c(oocystsT),.Dim=c(24,16))
###
###predspC<-satmod$par[1] * ooc_count_C^satmod$par[3])/(satmod$par[4] + satmod$par[2] * nc^satmod$par[3])
###predspT<-satmod$par[1] * ooc_count_T^satmod$par[3])/(satmod$par[4] + satmod$par[2] * nc^satmod$par[3])
###
#
## CIs for estimates from saturating function model
#

optim.model<-sat.binom(satmod$par)

size.of.grid<-20
a.range<-seq(0.7,0.99,length=size.of.grid)
b.range<-seq(0.15,0.6,length=size.of.grid)
c.range<-seq(0.3,0.8,length=size.of.grid)
d.range<-seq(2.5,5,length=size.of.grid)
pds<-expand.grid("a"=a.range,"b"=b.range,"c"=c.range,"d"=d.range)
pds$modcom<-NA

for(i in 1:length(pds$a)){
  p.vec<-c(pds$a[i],pds$b[i],pds$c[i],pds$d[i])
  ci.n.param<-length(p.vec) 
  ci.fit<-sat.binom(p.vec)     
  pds$modcom[i]<-ifelse(1-pchisq(2*(max(ci.fit,optim.model)-min(ci.fit,optim.model)),1) < 0.05,"discard","keep")
  ##print(i)
}
pds.new<-subset(pds,pds$modcom=="keep")

length(pds$a)
length(pds.new$a)

##

q1a<-quantile(pds.new$a,0.025);q1a##
q2a<-quantile(pds.new$a,0.975);q2a##
q1b<-quantile(pds.new$b,0.025);q1b## 
q2b<-quantile(pds.new$b,0.975);q2b##
q1c<-quantile(pds.new$c,0.025);q1c##
q2c<-quantile(pds.new$c,0.975);q2c##
q1d<-quantile(pds.new$d,0.025);q1d##
q2d<-quantile(pds.new$d,0.975);q2d##

predlower<-   (q1a * nc^q1c)/(q1d + q2b * nc^q1c) 
predupper<-   (q2a * nc^q2c)/(q2d + q1b * nc^q2c)  

par(mfrow=c(1,2));par(mar=c(5,5,5,5))
plot(meanoocysts,MEANsp,
     ylim=c(0,3),ylab="Mean sporozoite score",
     xlim=c(0,max(meanoocysts)),xlab="Oocyst mean intensity",cex.lab=2)
lines(predupper~nc)
lines(predlower~nc)
polygon(c(nc, rev(nc)),c(predupper,rev(predlower)),border=NA, col="aquamarine1")
lines(pred~nc)
points(meanoocysts,MEANsp,col="aquamarine4",pch=16)

############################################################################
##
## 2. Fit for the prevalence of infection in mice to oocyst counts from mosquito
##
##
##
############################################################################

datprev<-data.frame(prevs,meanoocysts,MEANsp)
datprev$treat<-c(rep("Con",16),rep("Treat",16))
datprev$prevs<-ifelse(datprev$meanoocysts==0,0,datprev$prevs)

par(mfrow=c(2,1));par(mar=c(5,5,2,2))
plot(c(0,prevs)~c(0,meanoocysts),ylim=c(0,1),xlim=c(0,max(meanoocysts)),xlab="Mean oocysts",ylab="Prevalence Mice")
log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(0,meanoocysts))) / (1 + exp(a + b * c(0,meanoocysts))) ) 
  prev1<-c(0,prevs)
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
logmod
nc<-seq(0,max(meanoocysts),1)
pred2<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
lines(nc,pred2,lwd=2,lty=2,col="red")


#fit individual Gompertz models to each TREATMENT group
out.nls<-nlsList(datprev$prevs ~ SSgompertz(log(datprev$meanoocysts),a0,b0,b1|datprev$treat),data=datprev)
## SSgompertz(x,a0,b0,b1): y(f(x)) = a0 exp(-b0 b1^x)
#Gompertz parameters for each group
coef(out.nls)
#plot(intervals(out.nls),layout=c(3,1))

apply(coef(out.nls), 2, mean)->parm.means
apply(coef(out.nls), 2, range)->parm.range
parm.means
parm.range

gom.binom<-function(p.vec){
  a0<-p.vec[1]
  b0<-p.vec[2]
  b1<-p.vec[3]
  
  pred1<- (a0 * exp (-b0 * b1 ^datprev$meanoocysts)) 
  data1<- datprev$prevs
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.7266714, 1.3155981, 0.8747033),gom.binom,method="L-BFGS-B",lower=c(0.7,0.3,0.6),upper=c(0.9,2.3,0.89))
gommod

nc<-seq(0,max(meanoocysts),1)
pred2a<-(gommod$par[1] * exp (-gommod$par[2] * gommod$par[3] ^  nc))
lines(nc,pred2a,lwd=2,lty=2,col="blue")


gom.binom<-function(p.vec){
  Z<-p.vec[1]
  B<-p.vec[2]
  G0<-p.vec[3]
  
  pred1<- (Z/B) * exp (-exp(G0 - B * meanoocysts))
  data1<- prevs
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.25, 0.24, 0.9),gom.binom,method="L-BFGS-B",lower=c(0.10,0.15,0.6),upper=c(0.15,0.8,1))
gommod

nc<-seq(0,max(meanoocysts),1)
pred2b<-(gommod$par[1]/gommod$par[2]) * exp (-exp(gommod$par[3] - gommod$par[2] * nc))
lines(nc,pred2b,lwd=2,lty=3,col="blue")




##########################################################################
##
## 3. Fit the distributions of the data and estimate the real sporozoite counts and prevalence 
##    with and without treatment to get the difference in the probaility of transmission
##  
##
###########################################################################
data1<-list(N_C=16,
            N_T=16,
            #N_Tatv=16 ##ATV
            #N_T4b7=16 ##4B7
            #N_Tvacc=16 ##PEvaccine
            #N_Tatv_vacc=16 ##PEvaccine + ATV
            #N_T4b7_vacc=16 ##PEvaccine + 4B7
            N_ooc=24,
            N_mice=5,
            ooc_count_C = structure(.Data = c(oocystsC),
                          .Dim=c(24,16)),
            ooc_count_T = structure(.Data = c(oocystsT),
                                    .Dim=c(24,16)),
            #ooc_count_Tatv = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T4b7 = structure(.Data = c(oocystsT4bv),
            #                        .Dim=c(24,16)),
            #ooc_count_Tvacc = structure(.Data = c(oocystsTvacc),
            #                        .Dim=c(24,16)),
            #ooc_count_Tatv_vacc = structure(.Data = c(oocystsTatv_vacc),
            #                        .Dim=c(24,16)),
            #ooc_count_T4b7_vacc = structure(.Data = c(oocystsT4b7_vacc),
            #                        .Dim=c(24,16)),
          #  parasitemiaC = parasitem,
          #  parasitemiaT = parasitemT,
            prev_C = structure(.Data =PREV_C,.Dim=c(5,16)),
            prev_T = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_Tatv = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T4b7 = structure(.Data =PREV_T4bv,.Dim=c(5,16)),
            #prev_Tvacc = structure(.Data =PREV_Tvacc,.Dim=c(5,16)),
            #prev_Tatv_vacc = structure(.Data =PREV_Tatv_vacc,.Dim=c(5,16)),
            #prev_T4b7_vacc = structure(.Data =PREV_T4b7_vacc,.Dim=c(5,16)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,1002),
            s_count_C = structure(.Data=spors_C,.Dim=c(16,5)),
            s_count_T = structure(.Data=spors_T,.Dim=c(16,5))#,
            #s_count_Tatv = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T4b7 = structure(.Data=spors_T4b7,.Dim=c(16,5)),
            #s_count_Tvacc = structure(.Data=spors_Tvacc,.Dim=c(16,5)),
            #s_count_Tatv_vacc = structure(.Data=spors_Tatv_vacc,.Dim=c(16,5)),
            #s_count_T4b7_vacc = structure(.Data=spors_T4b7_vacc,.Dim=c(16,5))
            )

stan_rdump(ls(data1), "Ellie.data.R", envir=list2env(data1))
fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\mice.censored_sp.stan", data=data1,
             iter=100, chains=2)

#fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\mice.censored_sp_fiting to parasitemia.stan", data=PARA_ATV25_data,
#             iter=1000, chains=2)


params = extract(fit1)


#write.csv(print(fit1),"C:\\Users\\Ellie\\Documents\\RStudioProjects\\2080Model\\fit1ATV25.csv")
my_shinystan<-as.shinystan(fit1)
launch_shinystan(my_shinystan)

#install.packages('devtools')
library(devtools)

source_url("https://github.com/stan-dev/shinystan/raw/develop/install_shinystan.R")
install_shinystan()

#######################################################
##
##  Probability of an infectious bite causing infection
##  ## Based on reasoning from White et al 2011 Proc paper
######################

## From the Blagborough et al 2013 controls

r = 5/12  ## 12 times only 1 mosquito in the controls had a sporozoite score of 1
          ##  5 times that the mouse was infected by those mosquitoes

pAnybite = 17/30  ## 30 times only 1 mosquito had sporozoites
                  ## 17 times a mouse was infected by these mosquitoes


k <- c(1:50)  ##assuming that the post bite sporozoite score correlates
              ##and the maximium number of sporozoites is 50

pk<-numeric(length(k))

for (i in 1:length(k)){
  
  pk[i] <- 1 - (1 - r)^k[i]  ##the probability that k sporozoites cause blood-stage infection

}

plot(c(0,pk) ~ c(0,k),bty="n",ylim=c(0,1),cex.lab=1.5,
     ylab = "Probability that k sporozoites cause bloodstage infections",
     xlab = "Number of sporozoites") 
             ## Sporozoite post feeding score of 1 corresponds to 1 sporozoite injected
             ## Sporozoite post feeding score of 2 corresponds to 2 sporozoite injected
             ## Sporozoite post feeding score of 3+ corresponds to 10 sporozoite injected
lines(c(0,pk) ~ c(0,k))



n <- rgeom(n=100, prob=30/44) ## 19 times that no mosquito was infected, 25 times one was infected with sporozoites
b <- numeric(length(n))


n2<- subset(n,n>0)
h <- expand.grid(numeric(length(n2)))

for (i in 1:length(k)){
  for (j in 1:length(n2)){
  h[j,i] <- ((1 - 1/n2[j])^(k[i]-1))* (1/n2[j]) ##Probability that k sporozoites are injected
  }
}

n3 <- sort(n2,decreasing=TRUE)
plot(sort(unique(h[,1]))~unique(n3),ylim=c(0,1),cex.lab=1.5,
     ylab="Probability that k sporozoites are injected",xlab="Mean of the distribution of sporozoites")
lines(sort(unique(h[,1]))~unique(n3))
for (i in 1:ncol(h)){
points(sort(unique(h[,i]))~unique(n3))
lines(sort(unique(h[,i]))~unique(n3),col="red",lty=2)
}


for (i in 1:length(n2)){
  
  b[i] <- ((n2[i] * r)/((n2[i]-1) * r + 1))
    
    }

plot(sort(unique(b)) ~ c(0,sort(unique(n2))),bty="n",cex.lab=1.5,
     ylab="Probability that bite from infectious mosquito causes bloodstage infection",
     xlab="Mean of the distribution of sporozoites")
lines(sort(unique(b)) ~ c(0,sort(unique(n2))))
##So expect a mean distribution of sporozoites of 2ish

## Blagborough data distribution of sporozoites if 
      ##1 corresponds to 1 sporozoite being injected
      ##2 corresponds to 2 being injected
      ##3+ correspond to normally distributed number injected with mean 10, sd 5
nreal <- c(rep(0,19),rep(1,12),rep(2,8),round(rnorm(mean=10,n=10,sd=5)))
mean(nreal)  ## = 2 Therefore prob infectious bite causes BS infection = 0.625
             ## This compares to our model estimate of 0.6720324

##So, if 
n = mean(nreal)
pk2 <- numeric(length(k))
  
for (i in 1:length(k)){
  pk2[i] <- ((1 - 1/n)^(k[i]-1))* (1/n)
}
plot(pk2~k,bty="n",ylim=c(0,1),cex.lab=1.5,
     ylab = "Probabilty k sporozoites injected",
     xlab = "Number of sporozoites")
lines(pk2~k)
bm <- numeric(length(k))
for (i in 1:length(k)){
bm[i] = -sum(((((1 - 1/n)^(k[i]-1))* (1/n))*(1 - (1 - r)^k[i]))-((n * r)/ ((n - 1) * r + 1)))
}

#mean(bm) ##The probability that a bite will cause blood stage malaria infection
#text(30,1,"Prob that 1 bite causes blood stage infection",cex=1.5)
#text(20,0.85,"Controls = 0.67",cex=1.5)

##############################
##
###
####
###### Repeat for treatments
####
###
##
## From the Blagborough et al 2013 controls

rT = 4/12  ## 11 times only 1 mosquito in the controls had a sporozoite score of 1
##  5 times that the mouse was infected by those mosquitoes

pAnybiteT = 15/27  ## 27 times only 1 mosquito had sporozoites
                   ## 15 times a mouse was infected by these mosquitoes


kT <- c(1:50)  ##assuming that the post bite sporozoite score correlates
##and the maximium number of sporozoites is 50

pkT<-numeric(length(kT))

for (i in 1:length(kT)){
  
  pkT[i] <- 1 - (1 - rT)^kT[i]  ##the probability that k sporozoites cause blood-stage infection
  
}

points(pkT ~ kT,pch=20,col="red",
     ylab = "Probability that k sporozoites cause bloodstage infections",
     xlab = "Number of sporozoites") 
## Sporozoite post feeding score of 1 corresponds to 1 sporozoite injected
## Sporozoite post feeding score of 2 corresponds to 2 sporozoite injected
## Sporozoite post feeding score of 3+ corresponds to 10 sporozoite injected
lines(pkT ~ kT,col="red",lty=2)



nT <- rgeom(n=100, prob=27/69) ## 42 times that no mosquito was infected, 27 times one was infected with sporozoites
bT <- numeric(length(nT))


n2T<- subset(n,n>0)
hT <- expand.grid(numeric(length(n2T)))

for (i in 1:length(kT)){
  for (j in 1:length(n2T)){
    hT[j,i] <- ((1 - 1/n2T[j])^(kT[i]-1))* (1/n2T[j]) ##Probability that k sporozoites are injected
  }
}

n3T <- sort(n2T,decreasing=TRUE)
plot(sort(unique(hT[,1]))~unique(n3T),ylim=c(0,1),
     ylab="Probability that k sporozoites are injected",xlab="Mean of the distribution of sporozoites")
lines(sort(unique(hT[,1]))~unique(n3T))
for (i in 1:ncol(hT)){
  points(sort(unique(hT[,i]))~unique(n3T))
  lines(sort(unique(hT[,i]))~unique(n3T),col="red",lty=2)
}


for (i in 1:length(n2T)){
  
  bT[i] <- ((n2T[i] * rT)/((n2T[i]-1) * rT + 1))
  
}

points(sort(unique(bT)) ~ c(0,sort(unique(n2T))),pch=20,col="red",
     ylab="Probability that bite from infectious mosquito causes bloodstage infection",
     xlab="Mean of the distribution of sporozoites")
lines(sort(unique(bT)) ~ c(0,sort(unique(n2T))),col="red",lty=2)

## Blagborough data distribution of sporozoites if 
##1 corresponds to 1 sporozoite being injected
##2 corresponds to 2 being injected
##3+ correspond to normally distributed number injected with mean 10, sd 5
nrealT <- c(rep(0,42),rep(1,12),rep(2,9),rnorm(mean=10,sd=5,n=6))
mean(nrealT)  ## = 2 Therefore prob infectious bite causes BS infection = 0.625
## This compares to our model estimate of 0.6720324

##So, if 
nT = mean(nrealT)
pk2T <- numeric(length(kT))

for (i in 1:length(kT)){
  pk2T[i] <- ((1 - 1/nT)^(kT[i]-1))* (1/nT)
}
points(pk2T~kT,col="red",pch=20,
     ylab = "Probabilty k sporozoites injected",
     xlab = "Number of sporozoites")
lines(pk2T~kT,col="red",lty=2)
bmT <- numeric(length(kT))
for (i in 1:length(kT)){
  bmT[i] = -sum(((((1 - 1/nT)^(kT[i]-1))* (1/nT))*(1 - (1 - rT)^kT[i]))-((nT * rT)/ ((nT - 1) * rT + 1)))
}

mean(bmT) ##The probability that 1 bite will cause blood stage malaria infection
text(21,0.75,"Treatment = 0.33",col="red",cex=1.5)


#######################################################
##
##  Probability of an infectious bite causing infection
##  ## Based on reasoning from White et al 2011 Proc paper
######################

## From the Blagborough et al 2013 controls
## Repeating for 2 positive mosquitoes

## Blagborough data distribution of sporozoites if 
##1 and 1 corresponds to 1-2 sporozoites being injected  This happens = 4 times (2 infected mice)
##1 and 2 corresponds to 1-3 being injected              This happens = 7 times (4 infected mice)
##1 and 3 corresponds to 1-10+ being injected            This happens = 5 times (4 infected mice)
##1 and 4 corresponds to 1-10+ being injected            Never occurs
          ##i.e. either the mosquito with a sporo score of 1 OR the mosq with sporo score 4 OR both infect the mouse
##2 and 2 corresponds to 2-4 being injected              This happens = 4 times (4 infected mice)
##2 and 3 corresponds to 2-10+ being injected            This happens = 5 times (4 infected mice)
##2 and 4 corresponds to 2-10+ being injected            Never occurs
##3 and 3 corresponds to 10+ being injected              This happens = 2 times (1 infected mice)
##3 and 4 corresponds to 10+ being injected              This happens = 2 times (2 infected mice)
##4 and 4 corresponds to 10+ being injected              Never occurs

##3+ correspond to normally distributed number injected with mean 10, sd 5

nreal2 <- c(rep(0,9),round(rnorm(mean=3,n=15,sd=1)),round(rnorm(mean=15,n=14,sd=5)))
mean(nreal2)  ## = 7.1 

## Blagborough data distribution of sporozoites if 
##1 and 1 and 1 corresponds to 1-3 sporozoites being injected  This happens = 1 times (1 infected mice)
##1 and 1 and 2 corresponds to 1-4 being injected              This happens = 1 times (1 infected mice)
##1 and 1 and 3 corresponds to 1-10+ being injected            Never occurs
##1 and 1 and 4 corresponds to 1-10+ being injected            Never occurs
        ##i.e. either the mosquito with a sporo score of 1 OR the mosq with sporo score 4 OR both infect the mouse
##1 and 2 and 2 corresponds to 1-5 being injected              Never occurs
##1 and 2 and 3 corresponds to 1-10+ being injected            This happens = 1 times (1 infected mice)
##1 and 2 and 4 corresponds to 1-10+ being injected            This happens = 2 times (2 infected mice)
##1 and 3 and 3 corresponds to 1-10+ being injected            This happens = 1 times (1 infected mice)
##1 and 3 and 4 corresponds to 1-10+ being injected            This happens = 4 times (2 infected mice)
##1 and 4 and 4 corresponds to 1-10+ being injected            Never occurs
##2 and 2 and 2 corresponds to 2-6 being injected              Never occurs
##2 and 2 and 3 corresponds to 2-10+ being injected            This happens = 1 times (1 infected mice)
##2 and 2 and 4 corresponds to 2-10+ being injected            Never occurs
##2 and 3 and 3 corresponds to 2-10+ being injected            This happens = 2 times (2 infected mice)
##2 and 3 and 4 corresponds to 2-10+ being injected            Never occurs
##2 and 4 and 4 corresponds to 2-10+ being injected            Never occurs
##3 and 3 and 3 corresponds to 10+ being injected              Never occurs
##3 and 3 and 4 corresponds to 10+ being injected              Never occurs
##3 and 4 and 4 corresponds to 10+ being injected              This happens = 3 times (3 infected mice)
##4 and 4 and 4 corresponds to 10+ being injected              Never occurs

##3+ correspond to normally distributed number injected with mean 10, sd 5

nreal3 <- c(0,round(rnorm(mean=3,n=2,sd=1)),round(rnorm(mean=15,n=12,sd=5)))
mean(nreal3)  ## = 7.1 


##So, if 
n = mean(nreal2)
pk2bites2 <- numeric(length(k))

for (i in 1:length(k)){
  pk2bites2[i] <- ((1 - 1/n)^(k[i]-1))* (1/n)
}

lines(pk2bites2~k,lty=2)
bm2bites <- numeric(length(k))
for (i in 1:length(k)){
  bm2bites[i] = -sum(((((1 - 1/n)^(k[i]-1))* (1/n))*(1 - (1 - r)^k[i]))-((n * r)/ ((n - 1) * r + 1)))
}

n = mean(nreal3)
pk2bites3 <- numeric(length(k))

for (i in 1:length(k)){
  pk2bites3[i] <- ((1 - 1/n)^(k[i]-1))* (1/n)
}

lines(pk2bites3~k,lty=3)
bm3bites <- numeric(length(k))
for (i in 1:length(k)){
  bm3bites[i] = -sum(((((1 - 1/n)^(k[i]-1))* (1/n))*(1 - (1 - r)^k[i]))-((n * r)/ ((n - 1) * r + 1)))
}

mean(bm) ##The probability that a single bite will cause blood stage malaria infection
mean(bm2bites) ##The probability that 2 bites will cause blood stage malaria infection
mean(bm3bites) ##The probability that 3 bites will cause blood stage malaria infection
text(30,1,"Prob that 1 bite causes blood stage infection",cex=1.5)
text(20,0.85,"Controls = 0.67",cex=1.5)

boxplot(bm,bm2bites,bm3bites)
##############################
##
###
####
###### Repeat for treatments
####
###
##
## From the Blagborough et al 2013 controls

rT = 4/12  ## 11 times only 1 mosquito in the controls had a sporozoite score of 1
##  5 times that the mouse was infected by those mosquitoes

pAnybiteT = 14/27  ## 25 times only 1 mosquito had sporozoites
## 15 times a mouse was infected by these mosquitoes


kT <- c(1:50)  ##assuming that the post bite sporozoite score correlates
##and the maximium number of sporozoites is 50

pkT<-numeric(length(kT))

for (i in 1:length(kT)){
  
  pkT[i] <- 1 - (1 - rT)^kT[i]  ##the probability that k sporozoites cause blood-stage infection
  
}

points(pkT ~ kT,pch=20,col="red",
       ylab = "Probability that k sporozoites cause bloodstage infections",
       xlab = "Number of sporozoites") 
## Sporozoite post feeding score of 1 corresponds to 1 sporozoite injected
## Sporozoite post feeding score of 2 corresponds to 2 sporozoite injected
## Sporozoite post feeding score of 3+ corresponds to 10 sporozoite injected
lines(pkT ~ kT,col="red",lty=2)



nT <- rgeom(n=100, prob=27/69) ## 42 times that no mosquito was infected, 27 times one was infected with sporozoites
bT <- numeric(length(nT))


n2T<- subset(n,n>0)
hT <- expand.grid(numeric(length(n2T)))

for (i in 1:length(kT)){
  for (j in 1:length(n2T)){
    hT[j,i] <- ((1 - 1/n2T[j])^(kT[i]-1))* (1/n2T[j]) ##Probability that k sporozoites are injected
  }
}

n3T <- sort(n2T,decreasing=TRUE)
plot(sort(unique(hT[,1]))~unique(n3T),ylim=c(0,1),
     ylab="Probability that k sporozoites are injected",xlab="Mean of the distribution of sporozoites")
lines(sort(unique(hT[,1]))~unique(n3T))
for (i in 1:ncol(hT)){
  points(sort(unique(hT[,i]))~unique(n3T))
  lines(sort(unique(hT[,i]))~unique(n3T),col="red",lty=2)
}


for (i in 1:length(n2T)){
  
  bT[i] <- ((n2T[i] * rT)/((n2T[i]-1) * rT + 1))
  
}

points(sort(unique(bT)) ~ c(0,sort(unique(n2T))),pch=20,col="red",
       ylab="Probability that bite from infectious mosquito causes bloodstage infection",
       xlab="Mean of the distribution of sporozoites")
lines(sort(unique(bT)) ~ c(0,sort(unique(n2T))),col="red",lty=2)

## Blagborough data distribution of sporozoites if 
##1 corresponds to 1 sporozoite being injected
##2 corresponds to 2 being injected
##3+ correspond to normally distributed number injected with mean 10, sd 5
nrealT <- c(rep(0,42),rep(1,12),rep(2,9),rnorm(mean=10,sd=5,n=6))
mean(nrealT)  ## = 2 Therefore prob infectious bite causes BS infection = 0.625
## This compares to our model estimate of 0.6720324

##So, if 
nT = mean(nrealT)
pk2T <- numeric(length(kT))

for (i in 1:length(kT)){
  pk2T[i] <- ((1 - 1/nT)^(kT[i]-1))* (1/nT)
}
points(pk2T~kT,col="red",pch=20,
       ylab = "Probabilty k sporozoites injected",
       xlab = "Number of sporozoites")
lines(pk2T~kT,col="red",lty=2)
bmT <- numeric(length(kT))
for (i in 1:length(kT)){
  bmT[i] = -sum(((((1 - 1/nT)^(kT[i]-1))* (1/nT))*(1 - (1 - rT)^kT[i]))-((nT * rT)/ ((nT - 1) * rT + 1)))
}

mean(bmT) ##The probability that 1 bite will cause blood stage malaria infection
text(21,0.75,"Treatment = 0.33",col="red",cex=1.5)


## From the Blagborough et al 2013 controls
## Repeating for 2 positive mosquitoes

## Blagborough data distribution of sporozoites if 
##0 - 15 times
##1 and 1 corresponds to 1-2 sporozoites being injected  This happens = 3 times (0 infected mice)
##1 and 2 corresponds to 1-3 being injected              This happens = 6 times (4 infected mice)
##1 and 3 corresponds to 1-10+ being injected            Never occurs
##1 and 4 corresponds to 1-10+ being injected            This happens = 1 times (1 infected mice)
##i.e. either the mosquito with a sporo score of 1 OR the mosq with sporo score 4 OR both infect the mouse
##2 and 2 corresponds to 2-4 being injected              This happens = 1 times (1 infected mice)
##2 and 3 corresponds to 2-10+ being injected            Never occurs
##2 and 4 corresponds to 2-10+ being injected            This happens = 1 times (1 infected mice)
##3 and 3 corresponds to 10+ being injected              This happens = 2 times (2 infected mice)
##3 and 4 corresponds to 10+ being injected              This happens = 1 times (1 infected mice)
##4 and 4 corresponds to 10+ being injected              Never occurs

##3+ correspond to normally distributed number injected with mean 10, sd 5

nreal2T <- c(rep(0,15),round(rnorm(mean=3,n=10,sd=1)),round(rnorm(mean=15,n=5,sd=5)))
mean(nreal2T)  ## = 7.1 

## Blagborough data distribution of sporozoites if 
##1 and 1 and 1 corresponds to 1-3 sporozoites being injected  Never occurs
##1 and 1 and 2 corresponds to 1-4 being injected              This happens = 1 times (1 infected mice)
##1 and 1 and 3 corresponds to 1-10+ being injected            Never occurs
##1 and 1 and 4 corresponds to 1-10+ being injected            Never occurs
##i.e. either the mosquito with a sporo score of 1 OR the mosq with sporo score 4 OR both infect the mouse
##1 and 2 and 2 corresponds to 1-5 being injected              This happens = 1 times (1 infected mice)
##1 and 2 and 3 corresponds to 1-10+ being injected            This happens = 1 times (1 infected mice)
##1 and 2 and 4 corresponds to 1-10+ being injected            This happens = 2 times (2 infected mice)
##1 and 3 and 3 corresponds to 1-10+ being injected            Never occurs
##1 and 3 and 4 corresponds to 1-10+ being injected            This happens = 2 times (2 infected mice)
##1 and 4 and 4 corresponds to 1-10+ being injected            This happens = 1 times (1 infected mice)
##2 and 2 and 2 corresponds to 2-6 being injected              
##2 and 2 and 3 corresponds to 2-10+ being injected            This happens = 2 times (2 infected mice)
##2 and 2 and 4 corresponds to 2-10+ being injected            
##2 and 3 and 3 corresponds to 2-10+ being injected            This happens = 2 times (2 infected mice)
##2 and 3 and 4 corresponds to 2-10+ being injected            This happens = 1 times (1 infected mice)
##2 and 4 and 4 corresponds to 2-10+ being injected            This happens = 1 times (1 infected mice)
##3 and 3 and 3 corresponds to 10+ being injected              This happens = 1 times (1 infected mice)
##3 and 3 and 4 corresponds to 10+ being injected              Never occurs
##3 and 4 and 4 corresponds to 10+ being injected              This happens = 2 times (2 infected mice)
##4 and 4 and 4 corresponds to 10+ being injected              Never occurs

##3+ correspond to normally distributed number injected with mean 10, sd 5

nreal3T <- c(round(rnorm(mean=3,n=2,sd=1)),round(rnorm(mean=15,n=15,sd=5)))
mean(nreal3T)  ## = 7.1 


##So, if 
n = mean(nreal2T)
pk2bites2T <- numeric(length(k))

for (i in 1:length(k)){
  pk2bites2T[i] <- ((1 - 1/n)^(k[i]-1))* (1/n)
}

lines(pk2bites2T~kT,lty=2,col="red")
bm2bitesT <- numeric(length(k))
for (i in 1:length(k)){
  bm2bitesT[i] = -sum(((((1 - 1/n)^(k[i]-1))* (1/n))*(1 - (1 - r)^k[i]))-((n * r)/ ((n - 1) * r + 1)))
}

n = mean(nreal3T)
pk2bites3T <- numeric(length(k))

for (i in 1:length(k)){
  pk2bites3T[i] <- ((1 - 1/n)^(k[i]-1))* (1/n)
}

lines(pk2bites3T~k,lty=3,col="red")
bm3bitesT <- numeric(length(k))
for (i in 1:length(k)){
  bm3bitesT[i] = -sum(((((1 - 1/n)^(k[i]-1))* (1/n))*(1 - (1 - r)^k[i]))-((n * r)/ ((n - 1) * r + 1)))
}

mean(bmT) ##The probability that a single bite will cause blood stage malaria infection
mean(bm2bitesT) ##The probability that 2 bites will cause blood stage malaria infection
mean(bm3bitesT) ##The probability that 3 bites will cause blood stage malaria infection
text(30,1,"Prob that 1 bite causes blood stage infection",cex=1.5)
text(20,0.85,"Controls = 0.67",cex=1.5)

par(mfrow=c(1,1))
boxplot(NA,bm,bmT,bm2bites,bm2bitesT,bm3bites,bm3bitesT,
        col=c("white","red"),xaxt="n",frame=F,ylim=c(0,1),
        xlab="Number of bites received from infected mosquitoes",
        ylab="Probability of bites causing blood stage infection")
axis(1,at=c(2.5,4.5,6.5),labels=c(1,2,3))

(mean(bm)-mean(bmT))/mean(bm)
(mean(bm2bites)-mean(bm2bitesT))/mean(bm2bites)
(mean(bm3bites)-mean(bm3bitesT))/mean(bm3bites)
####
####
####
#### What is going on with the sporozoites
sporsbites1<-subset(spors,Bites==1 & Treatment==1);sporsbites1
distsporsB1T <- c(sporsbites2[,6])
sporsbites1<-subset(spors,Bites==1 & Treatment==0);sporsbites1
distsporsB1C <- c(sporsbites1[,6])
summary(distsporsB1C)
summary(distsporsB1T)
sporsB1check <- c(distsporsB1C,distsporsB1T);typecheck <- c(rep("con",20),rep("treat",20)) 
summary.aov(lm(sporsB1check~typecheck))
boxplot(sporsB1check~typecheck)


sporsbites2<-subset(spors,Bites==2 & Treatment==1);sporsbites2
distsporsB2T <- c(sporsbites2[,6],sporsbites2[,7])
sporsbites2<-subset(spors,Bites==2 & Treatment==0);sporsbites2
distsporsB2C <- c(sporsbites2[,6],sporsbites2[,7])
summary(distsporsB2C)
summary(distsporsB2T)
sporsB2check <- c(distsporsB2C,distsporsB2T);typecheck <- c(rep("con",40),rep("treat",40)) 
summary.aov(lm(sporsB2check~typecheck))
boxplot(sporsB2check~typecheck)


sporsbites3<-subset(spors,Bites==3 & Treatment==1);sporsbites3
distsporsB3T <- c(sporsbites3[,6],sporsbites3[,7],sporsbites3[,8])
sporsbites3<-subset(spors,Bites==3 & Treatment==0);sporsbites3
distsporsB3C <- c(sporsbites3[,6],sporsbites3[,7],sporsbites3[,8])
summary(distsporsB3C)
summary(distsporsB3T)
sporsB3check <- c(distsporsB3C,distsporsB3T);typecheck <- c(rep("con",60),rep("treat",60)) 
summary.aov(lm(sporsB3check~typecheck))
boxplot(sporsB3check~typecheck)


sporsbites4<-subset(spors,Bites==4 & Treatment==1);sporsbites4
distsporsB4T <- c(sporsbites4[,6],sporsbites4[,7],sporsbites4[,8],sporsbites4[,9])
sporsbites4<-subset(spors,Bites==4 & Treatment==0);sporsbites4
distsporsB4C <- c(sporsbites4[,6],sporsbites4[,7],sporsbites4[,8],sporsbites4[,9])
summary(distsporsB4C)
summary(distsporsB4T)
sporsB4check <- c(distsporsB4C,distsporsB4T);typecheck <- c(rep("con",80),rep("treat",80)) 
summary.aov(lm(sporsB4check~typecheck))
boxplot(sporsB4check~typecheck)


sporsbites5<-subset(spors,Bites==5 & Treatment==1);sporsbites5
distsporsB5T <- c(sporsbites5[,6],sporsbites5[,7],sporsbites5[,8],sporsbites5[,9],sporsbites5[,10])
sporsbites5<-subset(spors,Bites==5 & Treatment==0);sporsbites5
distsporsB5C <- c(sporsbites5[,6],sporsbites5[,7],sporsbites5[,8],sporsbites5[,9],sporsbites5[,10])
summary(distsporsB5C)
summary(distsporsB5T)
sporsB5check <- c(distsporsB5C,distsporsB5T);typecheck <- c(rep("con",100),rep("treat",100)) 
summary.aov(lm(sporsB5check~typecheck))
boxplot(sporsB5check~typecheck)

