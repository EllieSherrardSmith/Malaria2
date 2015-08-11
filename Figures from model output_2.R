library(gridExtra)
library(ggplot2)
library(adegenet)
theme_set(theme_bw(24))

#Run MODELinstruction to create data used in this script

#A useful website:
#http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

###
####
##### Using data constructed in C:\Users\Ellie\Documents\RStudioProjects\Malaria\TBD and RTSS Model\clean scripts\MODELinstruction.R
####
###

##Model Output (ATV only)
data<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\MODEL OUTPUT\\DataFromModel1.csv")
data<-read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\OUTPUTS from Stan models\\ATV50.csv")
names(data) 
data<-data[501:1000,]##dropping burn in so keeping the appropriate data

####################################
##
## Mean data for the model estimates
##
############################

##Probability of mosquito infection
#Controls
sum(data[,224:235])/(500*12)

#Treatment
sum(data[,236:247])/(500*12)

#Efficacy on mosquito infection
(sum(data[,224:235])/(500*12)-sum(data[,236:247])/(500*12))/sum(data[,224:235])/(500*12)
data$sim_s_count_C.3

dateffCI<-matrix(nrow=500,ncol=16)
dateffCI<-(data[,224:235]-data[,236:247])/data[,224:235]
head(dateffCI)
EfficacyEstimate2<-sum(dateffCI)/(500*12)
x<-stack(dateffCI)
x2<-stack(dateffCI[,1:4])
EfficacyEstimate2Lower<-quantile(x$values,0.05)
EfficacyEstimate2Upper<-quantile(x$values,0.95)

##Probability of mouse infection
#Controls
sum(data[,200:211])/(500*12)
#Treatment
sum(data[,212:223])/(500*12)

#Efficacy on mouse infection
((sum(data[,200:211])/(500*12))-(sum(data[,212:223])/(500*12)))/(sum(data[,200:211])/(500*12))


##Overall efficacy
#(((sum(data[,284:299])/(500*16))-(sum(data[,300:315])/(500*16)))+
#  ((sum(data[,252:267])/(500*16))-(sum(data[,268:283])/(500*16))))/
#  ((sum(data[,284:299])/(500*16))+(sum(data[,252:267])/(500*16)))


####################################
##
## Explore distributions of the parameter estimates
##
############################
par(mfrow=c(3,4))
for(i in 7:18){
hist(data[,i],main=substitute(paste('Parameter ', data[a]), list(a=colnames(data)[i])))
}

MEANS<-numeric(9)
par(mfrow=c(3,3))
for (i in 51:59){
  hist(data[,i],main="",
       xlab=substitute(paste('Param: ', a), list(a=colnames(data)[i])))
  abline(v=mean(data[,i]),lty=2,col="red",lwd=2)
  MEANS[i-50]<-c(mean(data[,i]))
  eq <- round(bquote(.(MEANS[i-50])),3)
  text(mean(data[,i])-mean(data[,i])/1.1,150,eq)
}


##Checking the fit of the model against the data
##
### Oocysts
##
meanoocest<-uppoocest<-lowoocest<-numeric(length(32))
for(i in 60:91){
  meanoocest[i-59]<-mean(data[,i])
  uppoocest[i-59]<-quantile(data[,i],0.975)
  lowoocest[i-59]<-quantile(data[,i],0.025)
}
par(mfrow=c(1,1))
#boxplot(data[,60:91],ylim=c(0,50))

ooc_count_C = structure(.Data = c(oocystsC),
                        .Dim=c(24,16))
ooc_count_T = structure(.Data = c(oocystsT),
                        .Dim=c(24,16))
#boxplot(ooc_count_C,ylim=c(0,50))

dat2oocysts<-stack(data[,60:91])
Oocysts<-c(dat2oocysts[,1],oocystsC,oocystsT)
length(Oocysts)
data_type<-c(rep("Simulated",16000),rep("Real",768))
datgroup<-as.factor(c(rep(seq(1,32,1),each=500),rep(seq(1,32,1),each=24)))
Bites<-as.factor(c(rep(rep(seq(2,5,1),each=2000),2),rep(rep(seq(2,5,1),each=96),2)))
length(Bites)
Generations<-as.factor(c(rep(rep(seq(1,4,1),each=500),8),rep(rep(seq(1,4,1),each=24),8)))
length(Generations)
dat2<-data.frame(Oocysts,data_type,datgroup,Bites,Generations)
boxplot(dat2oocysts$values[1:8000],oocystsC,dat2oocysts$values[8001:16000],oocystsT,ylim=c(0,100))


dat2C_temp_sim<-dat2[1:8000,]
dat2C_temp_real<-dat2[16001:16384,]
dat2C<-rbind(dat2C_temp_sim,dat2C_temp_real)
dim(dat2C)

dat2T_temp_sim<-dat2[8001:16000,]
dat2T_temp_real<-dat2[16385:16768,]
dat2T<-rbind(dat2T_temp_sim,dat2T_temp_real)
dim(dat2T)

aOocysts<- ggplot(dat2, aes(x=Bites, y=Oocysts, fill=data_type)) + 
  geom_boxplot() + ggtitle("Overall data") + theme(legend.justification=c(1,1), legend.position=c(1,1)) + 
  scale_y_continuous(limits=c(-10,100), breaks=seq(0,100,50), expand = c(0, 0))

aOocystsC<- ggplot(dat2C, aes(x=Bites, y=Oocysts, fill=data_type)) + 
  geom_boxplot() + guides(fill=FALSE) + ggtitle("Controls") +
  scale_y_continuous(limits=c(-10,100), breaks=seq(0,100,50), expand = c(0, 0))

aOocystsT<- ggplot(dat2T, aes(x=Bites, y=Oocysts, fill=data_type),guides(fill=FALSE)) + 
  geom_boxplot()  + guides(fill=FALSE) + ggtitle("Treatments ATV") + 
  scale_y_continuous(limits=c(-10,100), breaks=seq(0,100,50), expand = c(0, 0))

multiplot(aOocystsC, aOocystsT, aOocysts, cols=3)
##################################
###
###################################
estprev<-stack(data[,252:283])
estpervmq<-stack(data[,284:315])
Prev_Mice<-c(estprev[,1],prevs)

  prevmq<-c(sum(prevooc[1:24])/24,sum(prevooc[25:48])/24,sum(prevooc[49:72])/24,sum(prevooc[73:96])/24,
            sum(prevooc[97:120])/24,sum(prevooc[121:144])/24,sum(prevooc[145:168])/24,sum(prevooc[169:192])/24,
            sum(prevooc[193:216])/24,sum(prevooc[217:240])/24,sum(prevooc[241:264])/24,sum(prevooc[265:288])/24,
            sum(prevooc[289:312])/24,sum(prevooc[313:336])/24,sum(prevooc[337:360])/24,sum(prevooc[361:384])/24)
            
  prevmqT<-c(sum(prevoocT[1:24])/24,sum(prevoocT[25:48])/24,sum(prevoocT[49:72])/24,sum(prevoocT[73:96])/24,
                sum(prevoocT[97:120])/24,sum(prevoocT[121:144])/24,sum(prevoocT[145:168])/24,sum(prevoocT[169:192])/24,
                sum(prevoocT[193:216])/24,sum(prevoocT[217:240])/24,sum(prevoocT[241:264])/24,sum(prevoocT[265:288])/24,
                sum(prevoocT[289:312])/24,sum(prevoocT[313:336])/24,sum(prevoocT[337:360])/24,sum(prevoocT[361:384])/24)

Prev_Mosquito<-c(estpervmq[,1],prevmq,prevmqT)
length(Prev_Mice)
length(Prev_Mosquito)
data_type<-c(rep("Simulated",16000),rep("Real",32))
datgroup<-as.factor(c(rep(seq(1,32,1),each=500),seq(1,32,1)))
Bites<-as.factor(c(rep(rep(seq(2,5,1),each=2000),2),rep(rep(seq(2,5,1),each=4),2)))
Generations<-as.factor(c(rep(rep(seq(1,4,1),each=500),8),rep(rep(seq(1,4,1),each=1),8)))

datTheta<-data.frame(Prev_Mice,Prev_Mosquito,data_type,datgroup,Bites,Generations)


bprevMice<-ggplot(datTheta, aes(x=Bites, y=Prev_Mice, fill=data_type)) + 
            geom_boxplot() + guides(fill=FALSE) + ggtitle("Mice") + labs(y = "Prevalence") + 
            scale_y_continuous(limits=c(-0.05,1), breaks=seq(0,1,0.1), expand = c(0, 0))

bprevMosq<-ggplot(datTheta, aes(x=Bites, y=Prev_Mosquito, fill=data_type)) + ggtitle("Mosquito") + 
            geom_boxplot() + labs(y = "Prevalence") + theme(legend.justification=c(1,0), legend.position=c(1,0)) + 
            scale_y_continuous(limits=c(-0.05,1), breaks=seq(0,1,0.1), expand = c(0, 0))

multiplot(bprevMice,bprevMosq,cols=2)
####
###
####
###
####
data_oocysts_min<-data_oocysts_mean<-data_oocysts_max<-datT_ooc_min<-datT_ooc_max<-numeric(16)
for(i in 60:75){
  data_oocysts_min[i-59]<-min(data[,i])
  data_oocysts_max[i-59]<-max(data[,i])
  data_oocysts_mean[i-59]<-mean(data[,i])

  
  datT_ooc_min[i-59]<-min(data[,i+16])
  datT_ooc_max[i-59]<-max(data[,i+16])
}
nc <- maxoocC <- meanoocC <- c(1:16)
par(mfrow=c(1,1))
for (i in 1:16){
  meanoocC[i]<-mean(ooc_count_C[,i])
  maxoocC[i]<-max(ooc_count_C[,i])
}

plot(nc,meanoocC,xaxt="n",ylab="Number of oocysts",ylim=c(0,2000))


polygon(c(nc, rev(nc)),c(data_oocysts_min,rev(data_oocysts_mean)),border=NA, col=transp("aquamarine1",alpha=0.2))
polygon(c(nc, rev(nc)),c(data_oocysts_max,rev(data_oocysts_mean)),border=NA, col=transp("aquamarine4",alpha=0.2))
points(nc,meanoocC,xaxt="n",ylab="Number of oocysts")
points(nc,maxoocC,pch=20)

##
###
#####  Plotting the useful outcomes
###
##

#################################################################
## Relative reduction in probability of mosquito infection
###################################################################
thet_mosq_means<-numeric(24)
for(i in 224:247){
  thet_mosq_means[i-223]<-mean(data[,i])
}

    a1<-stack(data[,224:226]);a2<-stack(data[,227:229]);a3<-stack(data[,230:232]);a4<-stack(data[,233:235]);
    thet_mosq_bitesC<-c(mean(a1$value),mean(a2$value),mean(a3$value),mean(a4$value))
    thet_mosq_bitesC_range<-c(a1$value,a2$value,a3$value,a4$value)

    B1<-stack(data[,236:238]);B2<-stack(data[,239:241]);B3<-stack(data[,242:244]);B4<-stack(data[,245:247]);
    thet_mosq_bitesT<-c(mean(B1$value),mean(B2$value),mean(B3$value),mean(B4$value))
    thet_mosq_bitesT_range<-c(B1$value,B2$value,B3$value,B4$value)

EffectSizeMosq_permbr<-(thet_mosq_bitesC-thet_mosq_bitesT)/thet_mosq_bitesC
EffectSizeMosq<-(mean(thet_mosq_bitesC)-mean(thet_mosq_bitesT))/mean(thet_mosq_bitesC)

dat_a1<-(a1$value-B1$value)/a1$value; dat_a2<-(a2$value-B2$value)/a2$value; dat_a3<-(a3$value-B3$value)/a3$value; dat_a4<-(a4$value-B4$value)/a4$value

thet_effect_mosq <- c(dat_a1,dat_a2,dat_a3,dat_a4)
length(thet_effect_mosq)
Bites <- rep(c(1,2,5,10),each=1500)
datmosq<-data.frame(thet_effect_mosq,Bites)

R_EffectSizeMosq<-c(mean(dat_a1),mean(dat_a2),mean(dat_a3),mean(dat_a4))
R_EffectSizeMosqU<-c(quantile(dat_a1,0.975),quantile(dat_a2,0.975),quantile(dat_a3,0.975),quantile(dat_a4,0.975))
R_EffectSizeMosqL<-c(quantile(dat_a1,0.025),quantile(dat_a2,0.025),quantile(dat_a3,0.025),quantile(dat_a4,0.025))

##
##BECAUSE OF THE NEGS
(R_EffectSizeMosq+1)/2
(R_EffectSizeMosqU+1)/2
(R_EffectSizeMosqL+1)/2

par(mfrow=c(1,2))
plot((R_EffectSizeMosq+1)/2~c(1,2,5,10),pty="",main="ATV effect on mosquito population",
     xaxt="n",yaxt="n",pch="",xlim=c(0.5,10.5),
     ylab=expression(paste("Effect Size ", theta[mosquitoes], )),xlab="Bites",ylim=c(-1,1))
axis(1,par(las=1),at=seq(0,10,1),labels=seq(0,10,1))

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(1,2,5,10))) / (1 + exp(a + b * c(1,2,5,10))) ) 
  prev1<-(R_EffectSizeMosqL+1)/2
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-100,-100),upper=c(100,10))
logmod
nc<-seq(1.5,10.5,0.01)
pred2L<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(1,2,5,10))) / (1 + exp(a + b * c(1,2,5,10))) ) 
  prev1<-(R_EffectSizeMosqU+1)/2
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-100,-100),upper=c(100,10))
logmod

pred2U<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
polygon(c(nc, rev(nc)),c(pred2U,rev(pred2L)),border=NA, col="aquamarine1")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(1,2,5,10))) / (1 + exp(a + b * c(1,2,5,10))) ) 
  prev1<-(R_EffectSizeMosq+1)/2
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-100,-100),upper=c(100,10))
logmod
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
lines(nc,pred,lwd=2,lty=2,col="black")


#axis(2,par(las=2),at=seq(0,1,0.25),labels=c(-1,-0.5,0,0.5,1))
abline(h=0.5,lty=2,col="grey")

par(new=T)
boxplot(datmosq$thet_effect_mosq~datmosq$Bites,xaxt="n",yaxt="n",ylim=c(-0.9,1))
points(R_EffectSizeMosq~c(1:4),pch=20,col="red")


Relative_reduction_in_probability_of_mosquito_infection<-(thet_mosq_means[1:16]-thet_mosq_means[17:32])/thet_mosq_means[1:16]
Bites<-rep(seq(2,5,1),each=4)
Generations<-rep(c(1,2,3,4),4)
datProb_thet_mosq<-data.frame(Relative_reduction_in_probability_of_mosquito_infection,Bites,Generations)

mosq1 <- ggplot(datProb_thet_mosq, aes(factor(Bites), Relative_reduction_in_probability_of_mosquito_infection))  +  
  geom_boxplot(aes(fill = Bites)) + scale_y_continuous(limits = c(-0.6,1), breaks=seq(-0.6,1,0.6))

myplot1 <- mosq1 + guides(fill=FALSE) + theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
   labs(x="Number of bites (mbr)", y=expression(paste("Reduction in ", theta [mosquito], )), title="ATV impact on mosquito infection")


#################################################################
## Relative reduction in probability of mouse infection
###################################################################
thet_mo_means<-numeric(32)
for(i in 200:223){
  thet_mo_means[i-199]<-mean(data[,i])
}


a1<-stack(data[,200:202]);a2<-stack(data[,203:205]);a3<-stack(data[,206:208]);a4<-stack(data[,209:211]);
thet_mouse_bitesC<-c(mean(a1$value),mean(a2$value),mean(a3$value),mean(a4$value))
thet_mouse_bitesC_range<-c(a1$value,a2$value,a3$value,a4$value)

B1<-stack(data[,212:214]);B2<-stack(data[,215:217]);B3<-stack(data[,218:220]);B4<-stack(data[,221:223]);
thet_mouse_bitesT<-c(mean(B1$value),mean(B2$value),mean(B3$value),mean(B4$value))
thet_mouse_bitesT_range<-c(B1$value,B2$value,B3$value,B4$value)

EffectSizemouse_permbr<-(thet_mouse_bitesC-thet_mouse_bitesT)/thet_mouse_bitesC
EffectSizemouse<-(mean(thet_mouse_bitesC)-mean(thet_mouse_bitesT))/mean(thet_mouse_bitesC)

dat_a1<-(a1$value-B1$value)/a1$value; dat_a2<-(a2$value-B2$value)/a2$value; dat_a3<-(a3$value-B3$value)/a3$value; dat_a4<-(a4$value-B4$value)/a4$value

thet_effect_mouse <- c(dat_a1,dat_a2,dat_a3,dat_a4)
length(thet_effect_mouse)
Bites <- rep(c(2:5),each=2000)
datmouse<-data.frame(thet_effect_mouse,Bites)

R_EffectSizeMouse<-c(mean(dat_a1),mean(dat_a2),mean(dat_a3),mean(dat_a4))
R_EffectSizeMouseU<-c(quantile(dat_a1,0.975),quantile(dat_a2,0.975),quantile(dat_a3,0.975),quantile(dat_a4,0.975))
R_EffectSizeMouseL<-c(quantile(dat_a1,0.025),quantile(dat_a2,0.025),quantile(dat_a3,0.025),quantile(dat_a4,0.025))

##
##BECAUSE OF THE NEGS
(R_EffectSizeMouse+1)/2
(R_EffectSizeMouseU+1)/2
(R_EffectSizeMouseL+1)/2


plot((R_EffectSizeMouse+1)/2~c(2:5),pty="",main="ATV effect on mouse population",
     xaxt="n",yaxt="n",pch="",xlim=c(1.5,5.5),
     ylab=expression(paste("Effect Size ", theta[mice], )),xlab="Bites",ylim=c(0,1))
axis(1,par(las=1),at=seq(0,5,1),labels=seq(0,5,1))

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(2:5))) / (1 + exp(a + b * c(2:5))) ) 
  prev1<-(R_EffectSizeMouseL+1)/2
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(100,10))
logmod
nc<-seq(1.5,5.5,0.01)
pred2L<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(2:5))) / (1 + exp(a + b * c(2:5))) ) 
  prev1<-(R_EffectSizeMouseU+1)/2
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(100,10))
logmod

pred2U<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
polygon(c(nc, rev(nc)),c(pred2U,rev(pred2L)),border=NA, col="aquamarine1")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(2:5))) / (1 + exp(a + b * c(2:5))) ) 
  prev1<-(R_EffectSizeMouse+1)/2
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(100,10))
logmod
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
lines(nc,pred,lwd=2,lty=2,col="black")


axis(2,par(las=2),at=seq(0,1,0.25),labels=c(-1,-0.5,0,0.5,1))
abline(h=0.5,lty=2,col="grey")

par(new=T)
boxplot(datmouse$thet_effect_mouse~datmouse$Bites,xaxt="n",yaxt="n",ylim=c(-0.9,1))
points(R_EffectSizeMouse~c(1:4),pch=20,col="red")



OverallEffectSize <- ((mean(thet_mouse_bitesC)-mean(thet_mouse_bitesT)) + (mean(thet_mosq_bitesC)-mean(thet_mosq_bitesT)))/(mean(thet_mouse_bitesC) + mean(thet_mosq_bitesC))

Relative_reduction_in_probability_of_mouse_infection<-(thet_mo_means[1:16]-thet_mo_means[17:32])/thet_mo_means[1:16]
Bites<-rep(seq(2,5,1),each=4)
Generations<-rep(c(1,2,3,4),4)
datProb_thet_mouse<-data.frame(Relative_reduction_in_probability_of_mouse_infection,Bites,Generations)

mouse1 <- ggplot(datProb_thet_mouse, aes(factor(Bites), Relative_reduction_in_probability_of_mouse_infection))  +  
  geom_boxplot(aes(fill = Bites)) + scale_y_continuous(limits = c(-0.6,1), breaks=seq(-0.6,1,0.6))

myplot2 <- mouse1 + guides(fill=FALSE) + theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
  labs(x="Number of bites (mbr)", y=expression(paste("Reduction in ", theta [mouse], )), title="ATV impact on mouse infection")


grid.arrange(myplot1, myplot2, ncol=2)

########################################
## what is the relative reduction in R0 
## calculated using Smith et al. 2007
##########################################
names(data)
k_temp<-c(rep(2,4),rep(3,4),rep(4,4),rep(5,4))
k<-rep(k_temp,2)
b<-c<-S<-EIR<-X<-R0<-numeric(32)

  for(i in 1:32){
      b[i]<-mean(data[,i+251])
      c[i]<-mean(data[,i+283])
      S[i]<-EIR[i]<-k[i]

      X[i]<-prevs[i]
      r<-1
R0[i]<-(b[i]/r)*((EIR[i]*(1+c[i]*S[i]*X[i]))/X[i])
    }

reductionR0<-(R0[1:16] - R0[17:32]) / R0[1:16]
reductionR0[is.infinite(reductionR0)] <- 1 

##par(mfrow=c(1,1))
##plot(reductionR0~c(1:16),xaxt="n",
##     ylab=expression(paste("Relative reduction in  ", R[0])),pch=20,
##     xlab="Data Group")
##axis(1,at=seq(1,16,1),labels=c("Bites 2: gen 1","2","3","4",
##                               "Bites 3: gen 1","2","3","4",
##                               "Bites 4: gen 1","2","3","4",
##                               "Bites 5: gen 1","2","3","4"))

par(las=1)
plot(c(mean(reductionR0[1:4]),mean(reductionR0[5:8]),mean(reductionR0[9:12]),mean(reductionR0[13:16]))
                  ~c(2:5),pch="",
          ylab=expression(paste("Relative reduction in  ", R[0])),yaxt="n",ylim=c(-0.4,1),
          xlab="",xaxt="n",xlim=c(1,5.5))
axis(1,at=seq(1,5,1),labels=c("1 Bite", "2 Bites","3 Bites","4 Bites","5 Bites"))
par(las=2)
axis(2,at=seq(-0.5,1,0.5),labels=c(-0.5,0,0.5,1))

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(2:5))) / (1 + exp(a + b * c(2:5))) ) 
  prev1<-c(min(reductionR0[1:4]),min(reductionR0[5:8]),min(reductionR0[9:12]),min(reductionR0[13:16]))
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(100,10))
logmod
nc<-seq(0,5.5,0.01)
pred2L<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(2:5))) / (1 + exp(a + b * c(2:5))) ) 
  prev1<-c(max(reductionR0[1:4]),max(reductionR0[5:8]),max(reductionR0[9:12]),max(reductionR0[13:16]))
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(100,10))
logmod

pred2U<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
polygon(c(nc, rev(nc)),c(pred2U,rev(pred2L)),border=NA, col="aquamarine1")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(2:5))) / (1 + exp(a + b * c(2:5))) ) 
  prev1<-c(mean(reductionR0[1:4]),mean(reductionR0[5:8]),mean(reductionR0[9:12]),mean(reductionR0[13:16]))
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(100,10))
logmod
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
lines(nc,pred,lwd=2,lty=2,col="black")



par(new=T)


boxplot(NA,reductionR0[1:4],
        reductionR0[5:8],
        reductionR0[9:12],
        reductionR0[13:16],
          xaxt="n",ylim=c(-0.4,1),yaxt="n",
          at=c(1,1.6,2.7,3.8,5))
abline(h=0,lty=2,col="grey")



#########################################################
##
##  ARE PARASITEMIA AND GAMETOCYTEMIA CORRELATED?
##   Stack the control and treatment data and use groups as additional explanatory variable
##
##
##
#######################################################

parasitemiaAll<-c(parasitem,parasitemT)
gametocytemiaAll<-c(gametoC,gametoT)
type<-c(rep("Control",16),rep("Treated",16))

glma <- glm(parasitemiaAll~gametocytemiaAll+type)
glmb <- glm(parasitemiaAll~gametocytemiaAll)
anova(glma,glmb,test="Chi")
summary.lm(glmb)
plot(glmb)


##############################################################
##
## DOES THE PROBABILITY OF MOSQUITO INFECTION CORRELATE WITH PARASITEMIA/GAMETOCYTES?
##
##
##
####################################################

thet_mosq_means
glmc <- glm(thet_mosq_means~parasitemiaAll+type)
glmd <- glm(thet_mosq_means~parasitemiaAll+0)
anova(glmc,glmd,test="Chi")
summary.lm(glmd)
plot(glmd)
plot(thet_mosq_means~parasitemiaAll)

datprob<-data.frame(thet_mosq_means,parasitemiaAll,gametocytemiaAll,type)
datprob$thet_mosq_means<-ifelse(datprob$parasitemiaAll==0,0,datprob$thet_mosq_means)

plot(datprob$parasitemiaAll,datprob$thet_mosq_means,ylim=c(0,1))
abline(lm(datprob$thet_mosq_means~datprob$parasitemiaAll+0),lty=2,col="grey")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * datprob$parasitemiaAll)) / (1 + exp(a + b * datprob$parasitemiaAll)) ) 
  prev1<-datprob$thet_mosq_means
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(100,10))
logmod
nc<-seq(0,20,0.01)
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
lines(nc,pred,lwd=2,lty=2,col="grey40")

gom.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  c<-p.vec[3]
  
  pred1a<- (a * exp (b * exp(c * datprob$parasitemiaAll)))  
  prev1<-datprob$thet_mosq_means
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-3
gommod<-optim(c(1,-2,-0.8),gom.binom,method="L-BFGS-B",lower=c(0.001,-5,-1),upper=c(1,0,-0.01))
gommod
nc<-seq(0,20,0.01)
pred2<-(gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc)))
lines(nc,pred2,lwd=2,lty=2,col="black")
