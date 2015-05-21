library(nlme)
library(rstan)
library(MASS)
library(boot)
library(coda)
library(R2OpenBUGS)
library(ggplot2)
library("Rlab")
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
prevooc<-ifelse(oocystsC==0,0,1)
prevooc1<-c(sum(sum(prevooc[1:24])/24,sum(prevooc[25:48])/24,sum(prevooc[49:72])/24,sum(prevooc[73:96])/24)/4,
            sum(sum(prevooc[97:120])/24,sum(prevooc[121:144])/24,sum(prevooc[145:168])/24,sum(prevooc[169:192])/24)/4,
            sum(sum(prevooc[193:216])/24,sum(prevooc[217:240])/24,sum(prevooc[241:264])/24,sum(prevooc[265:288])/24)/4,
            sum(sum(prevooc[289:312])/24,sum(prevooc[313:336])/24,sum(prevooc[337:360])/24,sum(prevooc[360:384])/24)/4)

freqoocC<-numeric(length(unique(oocystsC)))
oocystsC2<-sort(unique(oocystsC))
for (i in 1:length(oocystsC2)){ 
  freqoocC[i]<-sum(ifelse(oocystsC==unique(oocystsC2)[i],1,0))}
probNooc<-numeric(length(freqoocC))
for (j in 1:length(freqoocC)){
probNooc[j]<-freqoocC[j]/sum(freqoocC)}
freqdistoocC<-data.frame(oocystsC2,probNooc);colnames(freqdistoocC)[1]<-"Nooc"

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
prevooc<-ifelse(oocystsT==0,0,1)
prevoocT<-c(sum(sum(prevooc[1:24])/24,sum(prevooc[25:48])/24,sum(prevooc[49:72])/24,sum(prevooc[73:96])/24)/4,
            sum(sum(prevooc[97:120])/24,sum(prevooc[121:144])/24,sum(prevooc[145:168])/24,sum(prevooc[169:192])/24)/4,
            sum(sum(prevooc[193:216])/24,sum(prevooc[217:240])/24,sum(prevooc[241:264])/24,sum(prevooc[265:288])/24)/4,
            sum(sum(prevooc[289:312])/24,sum(prevooc[313:336])/24,sum(prevooc[337:360])/24,sum(prevooc[360:384])/24)/4)
EffOoc<-(prevooc1-prevoocT)/prevooc1

freqoocT<-numeric(length(unique(oocystsT)))
oocystsT2<-sort(unique(oocystsT))
for (i in 1:length(oocystsT2)){ 
  freqoocT[i]<-sum(ifelse(oocystsT==unique(oocystsT2)[i],1,0))}
probNoocT<-numeric(length(freqoocT))
for (j in 1:length(freqoocT)){
  probNoocT[j]<-freqoocT[j]/sum(freqoocT)}
freqdistoocT<-data.frame(oocystsT2,probNoocT);colnames(freqdistoocT)[1]<-"NoocT"

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
spors[10:40,]
##MEAN PARASITEMIA IN MICE
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
parasitem<-numeric(16)
for (i in 1:16){
  parasitem[i]<-sum(parasit[,i])/5}

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
parasitemT<-numeric(16)
for (i in 1:16){
  parasitemT[i]<-sum(parasitT[,i])/5}

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
gametoC<-numeric(16)
for (i in 1:16){
  gametoC[i]<-sum(gametC[,i])/5}

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
gametoT<-numeric(16)
for (i in 1:16){
  gametoT[i]<-sum(gametT[,i])/5}

##PREVALENCE IN MICE
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


MEANsp<-c(#mean(newa1),mean(newb1),mean(newc1),mean(newd1),
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
gametocytemia<-c(gametoC,gametoT)
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
            #N_T1=16 ##ATV
            #N_T2=16 ##4B7
            #N_T3=16 ##PEvaccine
            #N_T4=16 ##PEvaccine + ATV
            #N_T5=16 ##PEvaccine + 4B7
            N_ooc=24,
            N_mice=5,
            ooc_count_C = structure(.Data = c(oocystsC),
                          .Dim=c(24,16)),
            ooc_count_T = structure(.Data = c(oocystsT),
                                    .Dim=c(24,16)),
            #oc_count_T1 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T2 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T3 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T4 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            #ooc_count_T5 = structure(.Data = c(oocystsT),
            #                        .Dim=c(24,16)),
            prev_C = structure(.Data =PREV_C,.Dim=c(5,16)),
            prev_T = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T1 = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T2 = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T3 = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T4 = structure(.Data =PREV_T,.Dim=c(5,16)),
            #prev_T5 = structure(.Data =PREV_T,.Dim=c(5,16)),
            N_bin=5,
            bin_edge=c(0,1,10,100,1000,10000),
            s_count_C = structure(.Data=spors_C,.Dim=c(16,5)),
            s_count_T = structure(.Data=spors_T,.Dim=c(16,5))#,
            #s_count_T1 = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T2 = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T3 = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T4 = structure(.Data=spors_T,.Dim=c(16,5)),
            #s_count_T5 = structure(.Data=spors_T,.Dim=c(16,5))
            )

stan_rdump(ls(data1), "Ellie.data.R", envir=list2env(data1))
fit1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria\\TBD and RTSS Model\\mice.censored_sp2.stan", data=data1,
             iter=1000, chains=2)

