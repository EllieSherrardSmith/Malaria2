##SUMMARY FIGURES


##Functional relationship between oocysts, sporozoites and prevalence

##First creat a data.frame for each life stage with all the matched data on each set of rounds / bites...
oocysts<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocystsbites2to5.csv",header=TRUE)
head(oocysts)

datOOC<-data.frame(oocysts$oocystsbites2control[oocysts$round=="day41"][1:24],
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
                   oocysts$oocystsbites5control[oocysts$round=="day134"][1:24],
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
colnames(datOOC)<-c("ConB2G1","ConB2G2","ConB2G3","ConB2G4","ConB3G1","ConB3G2","ConB3G3","ConB3G4",
                    "ConB4G1","ConB4G2","ConB4G3","ConB4G4","ConB5G1","ConB5G2","ConB5G3","ConB5G4",
                    "ATV32B2G1","ATV32B2G2","ATV32B2G3","ATV32B2G4","ATV32B3G1","ATV32B3G2","ATV32B3G3","ATV32B3G4",
                    "ATV32B4G1","ATV32B4G2","ATV32B4G3","ATV32B4G4","ATV32B5G1","ATV32B5G2","ATV32B5G3","ATV32B5G4")

con<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mosquito.txt",header=TRUE)
con$OocPrev<-ifelse(con$Oocyst==0,0,1)
ATV25<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mosquito.txt",header=TRUE)
ATV25$OocPrev<-ifelse(ATV25$Oocyst==0,0,1)

oocystsC2<-data.frame(sample(con$Oocyst[con$Bites == 1 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 1 & con$Round == 2],45),
                      sample(con$Oocyst[con$Bites == 1 & con$Round == 3],45),
                      sample(con$Oocyst[con$Bites == 2 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 2 & con$Round == 2],45),
                      sample(con$Oocyst[con$Bites == 2 & con$Round == 3],45),
                      sample(con$Oocyst[con$Bites == 5 & con$Round == 1],45),sample(con$Oocyst[con$Bites == 5 & con$Round == 2],45),
                      sample(con$Oocyst[con$Bites == 5 & con$Round == 3],45),
                      sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 1],45),sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 2],45),
                      sample(ATV25$Oocyst[ATV25$Bites == 1 & ATV25$Round == 3],45),
                      sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 1],45),sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 2],45),
                      sample(ATV25$Oocyst[ATV25$Bites == 2 & ATV25$Round == 3],45),
                      sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 1],45),sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 2],45),
                      sample(ATV25$Oocyst[ATV25$Bites == 5 & ATV25$Round == 3],45))
colnames(oocystsC2)<-c("ConB1G1b","ConB1G2b","ConB1G3b","ConB2G1b","ConB2G2b","ConB2G3b",
                    "ConB5G1b","ConB5G2b","ConB5G3b",
                    "ATV25B1G1","ATV25B1G2","ATV25B1G3","ATV25B2G1","ATV25B2G2","ATV25B2G3",
                    "ATV25B5G1","ATV25B5G2","ATV25B5G3")

d2 <- stack(datOOC);d3 <- stack(oocystsC2); datOOC2 <- rbind(d2,d3)
as.vector(tapply(datOOC2$values,datOOC2$ind,mean))

par(mfrow=c(3,2))
par(mar=c(4,5,1,1))
plot(density(datOOC2$values[datOOC2$ind=="ConB2G1"]),col="blue",
     xaxt="n",yaxt="n",pch="",xlim=c(0,200),ylim=c(0,5),bty="n",
     ylab="Density",xlab="Oocysts per mosquito",main="")
for (i in 1:length(unique(datOOC2$ind))){
lines(density(datOOC2$values[datOOC2$ind==unique(datOOC2$ind)[i]]),col="red")
lines(density(datOOC2$values),col="black",lty=2,lwd=2)
  }
axis(1,par(las=1),at=seq(0,150,20),labels=seq(0,150,20))
#axis(2,par(las=2),at=seq(0,5,1),labels=seq(0,5,1))

spors<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=TRUE)
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)
## see MODELinstruction
datSPOR<-rbind(spors_C,spors_T,spors_C2[1:9,],bsporsATV25[1:9,])
datSPORnames<-c("ConB2G1","ConB2G2","ConB2G3","ConB2G4","ConB3G1","ConB3G2","ConB3G3","ConB3G4",
               "ConB4G1","ConB4G2","ConB4G3","ConB4G4","ConB5G1","ConB5G2","ConB5G3","ConB5G4",
               "ATV32B2G1","ATV32B2G2","ATV32B2G3","ATV32B2G4","ATV32B3G1","ATV32B3G2","ATV32B3G3","ATV32B3G4",
               "ATV32B4G1","ATV32B4G2","ATV32B4G3","ATV32B4G4","ATV32B5G1","ATV32B5G2","ATV32B5G3","ATV32B5G4",
               "ConB1G1b","ConB1G2b","ConB1G3b","ConB2G1b","ConB2G2b","ConB2G3b",
               "ConB5G1b","ConB5G2b","ConB5G3b",
               "ATV25B1G1","ATV25B1G2","ATV25B1G3","ATV25B2G1","ATV25B2G2","ATV25B2G3",
               "ATV25B5G1","ATV25B5G2","ATV25B5G3")
##from Modelinstruction,DataPreparation for Stan
MEANsporig ##original controls (2,3,4,5 bites), and ATV32
MEANsp[1:9]     ## new controls (1,2,5,10 BITES)

boxplot(datSPOR[,1],datSPOR[,2],datSPOR[,3],datSPOR[,4],datSPOR[,5],bty="n",frame=F)
for(i in 1:50){
lines(datSPOR[i,]~c(1:5),col="grey")
}
par(new=T)
boxplot(datSPOR[,1],datSPOR[,2],datSPOR[,3],datSPOR[,4],datSPOR[,5],col="aquamarine",xaxt="n",
        ylab="Frequency",xlab="Sporozoite binned count",frame=F)
axis(1,las=1,at=seq(1,5,1),labels=c("0","1-10","11-100","101-1000","1001+"))

spors<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=TRUE)
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)

prev1<-data.frame(spors$prevBS[spors$Bites==2 & spors$Treatment == 0 & spors$Round == 1],
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
spors$prevBS[spors$Bites==5 & spors$Treatment == 0 & spors$Round == 4],
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

colnames(prev1)<-c("ConB2G1","ConB2G2","ConB2G3","ConB2G4","ConB3G1","ConB3G2","ConB3G3","ConB3G4",
                    "ConB4G1","ConB4G2","ConB4G3","ConB4G4","ConB5G1","ConB5G2","ConB5G3","ConB5G4",
                    "ATV32B2G1","ATV32B2G2","ATV32B2G3","ATV32B2G4","ATV32B3G1","ATV32B3G2","ATV32B3G3","ATV32B3G4",
                    "ATV32B4G1","ATV32B4G2","ATV32B4G3","ATV32B4G4","ATV32B5G1","ATV32B5G2","ATV32B5G3","ATV32B5G4")

spors<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse3AddingCombinations.txt",header=TRUE)##Or do mouse3AddingCombinations
spors$prevBS<-ifelse(spors$Parasitemia > 0 | spors$Gametocytemia > 0, 1, 0)

sporsATV25<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mouse.txt",header=TRUE)
sporsATV25$prevBS<-ifelse(sporsATV25$Parasitemia > 0 | sporsATV25$Gametocytemia > 0, 1, 0)

prev2<-data.frame(
  sample(spors$prevBS[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$prevBS[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$prevBS[spors$Bites==1 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(spors$prevBS[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$prevBS[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$prevBS[spors$Bites==2 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(spors$prevBS[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 1],5),
  sample(spors$prevBS[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 2],5),
  sample(spors$prevBS[spors$Bites==5 & spors$Treatment == "CONTROL" & spors$Round == 3],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 1],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 2],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==1 & sporsATV25$Round == 3],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 1],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 2],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==2 & sporsATV25$Round == 3],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 1],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 2],5),
  sample(sporsATV25$prevBS[sporsATV25$Bites==5 & sporsATV25$Round == 3],5))
colnames(prev2)<-c("ConB1G1b","ConB1G2b","ConB1G3b","ConB2G1b","ConB2G2b","ConB2G3b",
                       "ConB5G1b","ConB5G2b","ConB5G3b",
                       "ATV25B1G1","ATV25B1G2","ATV25B1G3","ATV25B2G1","ATV25B2G2","ATV25B2G3",
                       "ATV25B5G1","ATV25B5G2","ATV25B5G3")
d11<-stack(prev1);d12<-stack(prev2);datPREV<-rbind(d11,d12)

PREVS<-as.vector(tapply(datPREV$values,datPREV$ind,mean))

############################################################################
##
## 1. Fit for the mean distribution of sporozoite scores to oocyst counts
##
##
##
############################################################################

plot(MEANsporig~meanoocysts,xlab="Mean oocysts",ylab="Mean sporozoite scores")
meanoocysts2<-c(meanoocystsC[1:3],meanoocystsC[5:7],meanoocystsC[9:11])
points(MEANsp[1:9]~meanoocysts2,pch=20)
points(MEANspATV25[1:9]~meanoocysts_ATV25[1:9],pch=20,col="grey")


sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  d<-p.vec[4]
  
  pred1<- (a * meanoocysts^c)/(d + b * meanoocysts^c)
  pred2<- (a * meanoocysts2^c)/(d + b * meanoocysts2^c)
  pred3<- (a * meanoocysts_ATV25[1:9]^c)/(d + b * meanoocysts_ATV25[1:9]^c)
  
  data1<-MEANsporig
  data2<-MEANsp[1:9]
  data3<-MEANspATV25[1:9]
  
  loglik1<- data1* log((pred1)+0.001)+(1-data1)*log(1-((pred1)-0.001))
  loglik2<- data2* log((pred2)+0.001)+(1-data2)*log(1-((pred2)-0.001))
  loglik3<- data3* log((pred3)+0.001)+(1-data3)*log(1-((pred3)-0.001))
  
  
  -sum(loglik1,loglik2,loglik3,na.rm=T)
}
n.param<-4
satmod<-optim(c(0.9,0.2,0.6,4),sat.binom,method="L-BFGS-B",lower=c(0,0.2,0.5,4),upper=c(0.95,1,0.65,5))
satmod

nc<-seq(0,max(meanoocysts),1)
pred<-(satmod$par[1] * nc^satmod$par[3])/(satmod$par[4] + satmod$par[2] * nc^satmod$par[3]) 
lines(nc,pred,lwd=2,lty=3,col="red")


############################################################################
##
## 2. Fit for the prevalence of infection in mice to oocyst counts from mosquito
##
##
##
############################################################################
par(mar=c(5,5,2,2))
OOCYSTS<-c(meanoocysts,meanoocysts2,meanoocysts_ATV25[1:9])
SPORS<-c(MEANsporig,MEANsp[1:9],MEANspATV25[1:9])
plot(c(0,PREVS)~c(0,OOCYSTS),ylim=c(0,1),xlim=c(0,max(OOCYSTS)),xlab="Mean oocysts",ylab="Prevalence Mice")

#log.binom<-function(p.vec){
  
#  a<-p.vec[1]
#  b<-p.vec[2]
  
#  pred1a<- ((exp(a + b * c(0,OOCYSTS))) / (1 + exp(a + b * c(0,OOCYSTS))) ) 
#  prev1<-c(0,PREVS)
  
#  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
#  -sum(loglik1a,  na.rm=T)
#}
#n.param<-2
#logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(10,10))
#logmod
nc<-seq(0,max(OOCYSTS),1)
#pred2<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) ) 
#lines(nc,pred2,lwd=2,lty=2,col="red")



#gom.binom<-function(p.vec){
#  a0<-p.vec[1]
#  b0<-p.vec[2]
#  b1<-p.vec[3]
  
#  pred1<- (a0 * exp (-b0 * b1 ^OOCYSTS)) 
#  data1<- PREVS
#  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
#  -sum(loglik1,na.rm=T)
#}
#n.param<-3
#gommod<-optim(c(0.7266714, 1.3155981, 0.8747033),gom.binom,method="L-BFGS-B",lower=c(0.5,0.3,0.6),upper=c(0.9,2.3,0.89))
#gommod

#nc<-seq(0,max(OOCYSTS),1)
#pred2a<-(gommod$par[1] * exp (-gommod$par[2] * gommod$par[3] ^  nc))
#lines(nc,pred2a,lwd=2,lty=2,col="blue")

gom.binom<-function(p.vec){
  Z<-p.vec[1]
  B<-p.vec[2]
  G0<-p.vec[3]
  
  pred1<- (Z/B) * exp (-exp(G0 - B * OOCYSTS))
  data1<- PREVS
  loglik1<- data1* log((pred1)+0.00001)+(1-data1)*log(1-((pred1)-0.00001))
  -sum(loglik1,na.rm=T)
}
n.param<-3
gommod<-optim(c(0.25, 0.24, 0.9),gom.binom,method="L-BFGS-B",lower=c(0.10,0.15,0.6),upper=c(0.15,0.8,1))
gommod

pred2b<-(gommod$par[1]/gommod$par[2]) * exp (-exp(gommod$par[3] - gommod$par[2] * nc))
lines(nc,pred2b,lwd=2,lty=3,col="blue")
