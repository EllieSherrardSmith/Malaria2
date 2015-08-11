##SUMMARY FIGURES

##Functional relationship between mean oocysts, sporozoites and prevalence
OOCYSTS<-c(meanoocysts, ##from MODELinstructionR
           tempOOCYSTS) ##from DataPreparation for Stan.R
SPOROS<-c(MEANsporig,tempSPORS)
PARASITEMIA<-c(parasitORIGmean,parasitATV32mean,tempPARA)
GAMETE<-c(gametORIGmean,tempGAMET)
PREVALENCE<-c(PREVorigMean,tempPREV)
group_names<-c("ConB2G1","ConB2G2","ConB2G3","ConB2G4","ConB3G1","ConB3G2","ConB3G3","ConB3G4",
                "ConB4G1","ConB4G2","ConB4G3","ConB4G4","ConB5G1","ConB5G2","ConB5G3","ConB5G4",
                "ATV32B2G1","ATV32B2G2","ATV32B2G3","ATV32B2G4","ATV32B3G1","ATV32B3G2","ATV32B3G3","ATV32B3G4",
                "ATV32B4G1","ATV32B4G2","ATV32B4G3","ATV32B4G4","ATV32B5G1","ATV32B5G2","ATV32B5G3","ATV32B5G4",
                "ConB1G1b","ConB1G2b","ConB1G3b","ConB2G1b","ConB2G2b","ConB2G3b",
                "ConB5G1b","ConB5G2b","ConB5G3b",
                "ATV25B1G1","ATV25B1G2","ATV25B1G3","ATV25B2G1","ATV25B2G2","ATV25B2G3",
                "ATV25B5G1","ATV25B5G2","ATV25B5G3","ATV25B10G1","ATV25B10G2","ATV25B10G3",
               "ATV50B1G1","ATV50B1G2","ATV50B1G3","ATV50B2G1","ATV50B2G2","ATV50B2G3",
               "ATV50B5G1","ATV50B5G2","ATV50B5G3","ATV50B10G1","ATV50B10G2","ATV50B10G3",
               "T3d11B1G1","T3d11B1G2","T3d11B1G3","T3d11B2G1","T3d11B2G2","T3d11B2G3",
               "T3d11B5G1","T3d11B5G2","T3d11B5G3","T3d11B10G1","T3d11B10G2","T3d11B10G3")
sumdat<-data.frame(group_names,OOCYSTS,SPOROS,PARASITEMIA,GAMETE,PREVALENCE)
head(sumdat)


###########################################################################
##
## 1. Fit for the mean sporozoite scores to oocyst counts
##                mean parasitemia
##                mean gametocytemia
##                mean prevalence
##
############################################################################
par(mfrow=c(2,2))
##
###
####
###### Mean sporozoites
####
###
##

Summary_data1<-list(N=77,
                    ooc_mean = c(OOCYSTS),
                    spor_mean = c(SPOROS))

              test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\ooc_to_spors_mod1.stan", data=Summary_data1,
              iter=1000, chains=4)
    
    print(test1)
    params = extract(test1);names(params)
  
plot(sumdat$SPOROS~sumdat$OOCYSTS,xlab="Mean oocysts",ylab="Mean sporozoite scores",ylim=c(0,4))

    nc<-seq(0,max(OOCYSTS),1)
    pred<-(mean(params$alpha[501:1000]) * nc^mean(params$sigma[501:1000]))/
          (mean(params$delta[501:1000]) + mean(params$beta[501:1000]) * nc^mean(params$sigma[501:1000])) 

lines(nc,pred,lwd=2,lty=2,col="red")

#sat.binom<-function(p.vec){
  
  #a<-p.vec[1]
#  a<-p.vec[1]
#  b<-p.vec[2]
#  c<-p.vec[3]
#  d<-p.vec[4]
  
#  pred1<- (a * sumdat$OOCYSTS^c)/(d + b * sumdat$OOCYSTS^c)
  
#  data1<-sumdat$SPOROS
  
#  loglik1<- data1* log((pred1)+0.001)+(1-data1)*log(1-((pred1)-0.001))
    
#  -sum(loglik1,na.rm=T)
#}
#n.param<-4
#satmod<-optim(c(0.9,0.2,0.6,4),sat.binom,method="L-BFGS-B",lower=c(0,0.2,0.5,4),upper=c(0.95,1,0.65,5))
#satmod

#nc<-seq(0,max(sumdat$OOCYSTS),1)
#pred<-(satmod$par[1] * nc^satmod$par[3])/(satmod$par[4] + satmod$par[2] * nc^satmod$par[3]) 
#lines(nc,pred,lwd=2,lty=3,col="red")


##
###
####
###### Mean Prevalence
####
###
##

Summary_data2<-list(N=77,
                    ooc_mean = c(OOCYSTS),
                    prev_mean = c(PREVALENCE))

test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\ooc_to_prev_mod1.stan", data=Summary_data2,
              iter=1000, chains=4)

    print(test2)
    params = extract(test2);names(params)

plot(sumdat$PREVALENCE~sumdat$OOCYSTS,xlab="Mean oocysts",ylab="Mean prevalence in mice",ylim=c(0,1))

pred2<-(mean(params$alpha[501:1000])/mean(params$beta[501:1000])) * 
  exp(-exp(mean(params$delta[501:1000]) - mean(params$beta[501:1000]) * nc))
  
lines(nc,pred2,lwd=2,lty=2,col="blue")    

#test2b <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\ooc_to_prev_mod2.stan", data=Summary_data2,
#              iter=1000, chains=4)

#print(test2b)
#params = extract(test2b);names(params)

#pred2b<-(mean(params$alpha[501:1000])*nc^mean(params$sigma[501:100]))/
#  (mean(params$delta[501:1000]) + mean(params$beta[501:1000]) * nc^mean(params$sigma[501:100])) 
#lines(nc,pred2b,lwd=2,lty=3,col="blue")

##
###
####
###### Mean parasitemia
####
###
##

Summary_data3<-list(N=77,
                    ooc_mean = c(OOCYSTS),
                    para_mean = c(PARASITEMIA))

#test3 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\ooc_to_para_mod1.stan", data=Summary_data3,
#              iter=1000, chains=4)

#print(test3)
#params = extract(test3);names(params)

plot(sumdat$PARASITEMIA~sumdat$OOCYSTS,xlab="Mean oocysts",ylab="Mean parasitemia in mice")

#pred3<-(mean(params$alpha[501:1000]) * nc^mean(params$sigma[501:1000]))/
#  (mean(params$delta[501:1000]) + mean(params$beta[501:1000]) * nc^mean(params$sigma[501:1000]))
#lines(nc,pred3)

test3b <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\ooc_to_para_mod2.stan", data=Summary_data3,
              iter=1000, chains=4)

print(test3b) ##better (lower and less variable lp hence lower AIC)
params = extract(test3b);names(params)

pred3b<-(mean(params$alpha[501:1000])/mean(params$beta[501:1000])) * exp (-exp(mean(params$delta[501:1000]) - mean(params$beta[501:1000]) * nc)) 
lines(nc,pred3b,lwd=2,lty=2,col="blue")

##
###
####
###### Mean gametocytemia
####
###
##

Summary_data4<-list(N=77,
                    ooc_mean = c(sumdat$OOCYSTS),
                    gama_mean = c(sumdat$GAMETE))

test4 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\ooc_to_gama_mod1.stan", data=Summary_data4,
              iter=1000, chains=4)

print(test4) ##better
params = extract(test4);names(params)

plot(sumdat$GAMETE~sumdat$OOCYSTS,xlab="Mean oocysts",ylab="Mean gametocytemia in mice")

pred4<-(mean(params$alpha[501:1000]) * nc^mean(params$sigma[501:1000]))/
  (mean(params$delta[501:1000]) + mean(params$beta[501:1000]) * nc^mean(params$sigma[501:1000]))
lines(nc,pred4,col="blue",lty=2,lwd=2)

#test4b <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\ooc_to_gama_mod2.stan", data=Summary_data4,
#               iter=1000, chains=4)

#print(test4b) ##better (lower and less variable lp hence lower AIC)
#params = extract(test4b);names(params)

#pred4b<-(mean(params$alpha[501:1000])/mean(params$beta[501:1000])) * exp (-exp(mean(params$delta[501:1000]) - mean(params$beta[501:1000]) * nc)) 
#lines(nc,pred4b,lwd=2,lty=3,col="blue")


############################################################################
##
## 3. Fit for the prevalence of infection in mice to sporozoite scores from mosquito
##            parasitemia
##            gametocytemia
## Now can use the individual moue data
############################################################################


###############################################################
##
##
##Functional relationships for the mice individual infection status
##
##
################################################################
INFSTAT<-c(spors$prevBS, ##original data
           infstat    )     ##new data controls, ATV25, 50 and T3d11
MEANSPORPERMOUSE<-c(spors$meanpermouse,meansporoscore)
PARASITEMIAPERMOUSE<-c(spors$Parasitemia,mouseparasitemia)
GAMETOCYTPERMOUSE<-c(spors$Gametocytemia,mousegametocytemia)


dat<-data.frame(MEANSPORPERMOUSE,INFSTAT,PARASITEMIAPERMOUSE,GAMETOCYTPERMOUSE)
datb<-rbind(dat[1:331,],dat[365:658,])
datc<-datb[complete.cases(datb),]
dim(datc)

############################################################################
##
## 4. Fit infection in mice to sporozoite scores from mosquito
##
## #Bernoulli distributions
##
############################################################################

##
###
####
###### Infection status
####
###
##

Summ_data1<-list(N=583,
                inf = c(datc$INFSTAT),
                spor_mean = c(datc$MEANSPORPERMOUSE))
test5 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\spor_to_prev_mod1.stan", data=Summ_data1,
                 iter=1000, chains=2) ###SLOWER!!More data points - only run 4 chains for final function

print(test5)
params = extract(test5);names(params)

nc2<-seq(0,4,0.01)

plot(dat$INFSTAT~dat$MEANSPORPERMOUSE)
predprevs<-(mean(params$alpha) * nc2^mean(params$sigma))/(mean(params$delta) + mean(params$beta) * nc2^mean(params$sigma))
lines(nc2,predprevs)

#####################################
######################START HERE TOMORROW!!!! TRY other functions dor the prevalence

##
###
####
###### Parasitemia
####
###
##

Summ_data1<-list(N=583,
                 inf = c(datc$INFSTAT),
                 spor_mean = c(datc$PARASITEMIAPERMOUSE))
test5 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\spor_to_prev_mod1.stan", data=Summ_data1,
              iter=1000, chains=2)

print(test5)
params = extract(test5);names(params)

nc2<-seq(0,4,0.01)

plot(dat$INFSTAT~dat$MEANSPORPERMOUSE)
predprevs<-(mean(params$alpha) * nc2^mean(params$sigma))/(mean(params$delta) + mean(params$beta) * nc2^mean(params$sigma))
lines(nc2,predprevs)






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

