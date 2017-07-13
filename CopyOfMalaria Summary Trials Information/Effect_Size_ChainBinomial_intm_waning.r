#####################################################################################################
##                                                                                                 ##                       
### Fitted chain binomial - replicating Blagborough et al 2013 Nat Comms, 5 mouse-mosquito rounds  ##  
##                                                                                                 ##  
#####################################################################################################


######################################################
##
##
##
##
## CONTROLS B
##
########################################################
# Use standard control data - the same for all experiments
conMosquito<-read.table("H:\\Ellie\\Synergy TBI and PEV\\ALL DATA RE_ARRANGED_20062015\\controls\\mosquito_inc2015cons.txt",header=TRUE)
conMouse<-read.table("H:\\Ellie\\Synergy TBI and PEV\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse_inc2015cons_incM2M.txt",header=TRUE)
levels(conMosquito$Treatment)[1] <- 0; levels(conMosquito$Treatment)[2] <- 0
levels(conMouse$Treatment)[1] <- 0; levels(conMouse$Treatment)[2] <- 0

tapply(conMouse$Treatment[conMouse$Bites==1],conMouse$Round[conMouse$Bites==1],summary)
tapply(conMouse$Treatment[conMouse$Bites==2],conMouse$Round[conMouse$Bites==2],summary)
tapply(conMouse$Treatment[conMouse$Bites==5],conMouse$Round[conMouse$Bites==5],summary)
tapply(conMouse$Treatment[conMouse$Bites==10],conMouse$Round[conMouse$Bites==10],summary)

rounds=1:4;bites=c(1,2,5,10)
data_summary_infecteds_C <- data_summary_inf_MOSQs_C <- array(dim=c(4,4),NA)

for(i in 1:4){
  for(j in 1:4){
    data_summary_infecteds_C[i,j] <-  
      sum(ifelse(conMouse$Parasitemia[conMouse$Bites==bites[j] & conMouse$Round == rounds[i]] < 1, 0, 1))
    data_summary_inf_MOSQs_C[i,j] <- 
      sum(ifelse(conMosquito$Oocyst[conMosquito$Bites==bites[j] & conMosquito$Round == rounds[i]] < 1, 0, 1))
    
  }
}
sum(ifelse(conMosquito$Oocyst[conMosquito$Bites==0 & conMosquito$Round == 0] < 1, 0, 1))
tapply(conMosquito$Treatment[conMosquito$Bites==0],conMosquito$Round[conMosquito$Bites==0],summary)
tapply(conMosquito$Treatment[conMosquito$Bites==1],conMosquito$Round[conMosquito$Bites==1],summary)
tapply(conMosquito$Treatment[conMosquito$Bites==2],conMosquito$Round[conMosquito$Bites==2],summary)
tapply(conMosquito$Treatment[conMosquito$Bites==5],conMosquito$Round[conMosquito$Bites==5],summary)
tapply(conMosquito$Treatment[conMosquito$Bites==10],conMosquito$Round[conMosquito$Bites==10],summary)
mean(conMosquito$Oocyst[conMosquito$Bites==0 & conMosquito$Round == 0])
mean(conMosquito$Oocyst[conMosquito$Round == 1])


n_total_miceC = array(dim=c(5,4),data=NA)
n_total_miceC[,1] <- c(5,10,10,10,5)
n_total_miceC[,2] <- c(5,30,10,10,5)
n_total_miceC[,3] <- c(5,30,10,10,10)
n_total_miceC[,4] <- c(5,25,5,5,5)

n_inf_miceC = array(dim=c(5,4),data=NA)
n_inf_miceC[1,] <- c(5,5,5,5)
n_inf_miceC[2:5,1:4] <- data_summary_infecteds_C

n_total_mosqC = array(dim=c(5,4),data=NA)
n_total_mosqC[,1] = c(100,95,95,95,45)
n_total_mosqC[,2] = c(100,82,95,95,45)
n_total_mosqC[,3] = c(100,95,95,95,95)
n_total_mosqC[,4] = c(100,50,50,50,50)

n_inf_mosqC = array(dim=c(5,4),data=NA)
n_inf_mosqC[1,] = c(78,78,78,78)
n_inf_mosqC[2:5,1:4] =  data_summary_inf_MOSQs_C

##################################################################
##
## 3. 4B7-50%
##
##
data.mouseTemp3=read.table("H:\\Ellie\\Synergy TBI and PEV\\ALL DATA RE_ARRANGED_20062015\\Figures for Andrew 11072017\\intm_mouse.txt",header=T)
data.mosquTemp3=read.table("H:\\Ellie\\Synergy TBI and PEV\\ALL DATA RE_ARRANGED_20062015\\Figures for Andrew 11072017\\intm_mosquito.txt",header=T)
data.mosquTemp3$Treatment<-as.factor(data.mosquTemp3$Treatment)
data.mouseTemp3$Treatment<-as.factor(data.mouseTemp3$Treatment)

tapply(data.mouseTemp3$Treatment[data.mouseTemp3$Bites==1],data.mouseTemp3$Round[data.mouseTemp3$Bites==1],summary)
tapply(data.mouseTemp3$Treatment[data.mouseTemp3$Bites==2],data.mouseTemp3$Round[data.mouseTemp3$Bites==2],summary)
tapply(data.mouseTemp3$Treatment[data.mouseTemp3$Bites==5],data.mouseTemp3$Round[data.mouseTemp3$Bites==5],summary)
tapply(data.mouseTemp3$Treatment[data.mouseTemp3$Bites==10],data.mouseTemp3$Round[data.mouseTemp3$Bites==10],summary)

data_summary_infecteds_T3 <- data_summary_inf_MOSQs_T3 <- array(dim=c(4,4),data=NA)

for(i in 1:4){
  for(j in 1:4){
    data_summary_infecteds_T3[i,j] <-  
      sum(ifelse(data.mouseTemp3$Parasitemia[data.mouseTemp3$Bites==bites[j] & data.mouseTemp3$Round == rounds[i]] > 0, 1, 0))
    data_summary_inf_MOSQs_T3[i,j] <- 
      sum(ifelse(data.mosquTemp3$Oocyst[data.mosquTemp3$Bites==bites[j] & data.mosquTemp3$Round == rounds[i]] > 0, 0, 1))
  }
}

sum(ifelse(data.mosquTemp3$Oocyst[data.mosquTemp3$Bites==1 & data.mosquTemp3$Round == 0] > 0, 1, 0))

tapply(data.mosquTemp3$Treatment[data.mosquTemp3$Bites==1],data.mosquTemp3$Round[data.mosquTemp3$Bites==1],summary)
tapply(data.mosquTemp3$Treatment[data.mosquTemp3$Bites==2],data.mosquTemp3$Round[data.mosquTemp3$Bites==2],summary)
tapply(data.mosquTemp3$Treatment[data.mosquTemp3$Bites==5],data.mosquTemp3$Round[data.mosquTemp3$Bites==5],summary)
tapply(data.mosquTemp3$Treatment[data.mosquTemp3$Bites==10],data.mosquTemp3$Round[data.mosquTemp3$Bites==10],summary)
mean(data.mosquTemp3$Oocyst[data.mosquTemp3$Bites==1 & data.mosquTemp3$Round == 0])
mean(data.mosquTemp3$Oocyst[data.mosquTemp3$Round == 1])

#data.mosquTemp

n_total_miceT3 = array(dim=c(5,4),data=5)

n_inf_miceT3 = array(dim=c(5,4),data=NA)
n_inf_miceT3[1,] <- c(5,5,5,5)
n_inf_miceT3[2:5,1:4] <- data_summary_infecteds_T3

n_total_mosqT3 = array(dim=c(5,4),data=NA)
n_total_mosqT3[,1] = c(50,50,50,50,50)
n_total_mosqT3[,2] = c(50,50,50,50,50)
n_total_mosqT3[,3] = c(50,50,50,50,50)
n_total_mosqT3[,4] = c(50,50,50,50,50)

n_inf_mosqT3 = array(dim=c(5,4),data=NA)
n_inf_mosqT3[1,] = c(5,5,5,5)
n_inf_mosqT3[2:5,1:4] =  data_summary_inf_MOSQs_T3


TOTAL_MOSQUITOES_ZERO3 = 50
NROUNDS3 = 5


data.mosqu <- rbind(conMosquito[,1:4],
                    data.mosquTemp3[,1:4])
data.mouse <- rbind(conMouse[,3:7],
                    data.mouseTemp3[,1:5])
levels(data.mouse$Treatment)[1] <- 0
levels(data.mouse$Treatment)[2] <- 1
summary(data.mouse)

levels(data.mosqu$Treatment)[1] <- 0
levels(data.mosqu$Treatment)[2] <- 1
summary(data.mosqu)


#################
##
## Confirm the starting values for the inf and number arrays
##
###################################################
vrs.binom<-function(p.vec){
  
  v<-p.vec[1]  ##0 when no tbi  ***** 4b7 25
  r<-p.vec[2]
  s<-p.vec[3]
  
  nrounds = NROUNDS3 ## Change as necessary for the treatment arm of the experiment
  nroundsm1 = NROUNDS3 - 1 ## and adjust for the arrays as necessary
  
  #The original number of infected individuals 
  #Number of rounds, Number of bites, Treatment, Species
  inf<-array(NA, dim = c(nrounds,4,2,2))
  # Enter p0 data where it is assumed that all mice are infected
  inf[1,,1,2]<-5 ##controls Mice
  inf[1,,1,1]<- s * 100 ##controls Mosquito
  
  inf[1,,2,2]<-5 ##treated Mice
  inf[1,,2,1]<-s * 50  ##treated Mosquito at round 0 *************8 cHECK THIS!

  # Total numbers of mice and mosquitoes
  number<-array(NA, dim = c(nrounds,4,2,2))
  # The total number of hosts of mosquitoes (maximum number)
  number[1:nrounds,,1,2]<-n_total_miceC[1:nrounds,]  ##controls Mice
  number[1:nrounds,,1,1]<-n_total_mosqC[1:nrounds,]  ##controls Mosquito afterwards
  
  # The total number of hosts of mosquitoes (maximum number)
  number[1:nrounds,,2,2]<-n_total_miceT3[1:nrounds,]  ##TREAT Mice
  number[1:nrounds,,2,1]<-n_total_mosqT3[1:nrounds,]  ##TREAT Mosquito afterwards
  

  mbr.vec<-c(1,2,5,10)
  # Predcit (p1-p5 and q1-q5), loops for rounds and bites 
  for(a in 1:4){
    for(b in 1:4){
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="0")
      number[a+1,b,1,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,1,2] <- (1 - (1 - r * (inf[a,b,1,1]/number[a,b,1,1])) ^ mbr.vec[b]) * number[a+1,b,1,2]                # mosquito to mouse
      inf[a+1,b,1,1] <- ((inf[a+1,b,1,2]/number[a+1,b,1,2]) * s) * length(mosqu.temp$Oocyst)                         # mouse to mosquito
      ##treated      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="1")
      number[a+1,b,2,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,2,2] <- (1 - (1 - r * (inf[a,b,2,1]/number[a,b,2,1]) ) ^ mbr.vec[b]) * number[a+1,b,2,2]               # mosquito to mouse
      inf[a+1,b,2,1] <- ((inf[a+1,b,2,2]/number[a+1,b,2,2]) * s) * (1-v) * length(mosqu.temp$Oocyst)                   # mouse to mosquito
    }
  }
  
  # Arrange data into appropriate format for fitting
  data.inf<-array(NA, dim = c(nrounds,4,2,2))    
  data.inf[1,1:4,1,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="0"]>0)
  data.inf[1,1:4,2,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="1"]>0)
  
  for(a in 1:nroundsm1){
    for(b in 1:4){
      data.inf[a+1,b,1,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="0"]>0)
      data.inf[a+1,b,1,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="0"]>0)
      
      data.inf[a+1,b,2,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="1"]>0)
      data.inf[a+1,b,2,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="1"]>0)
    }
  }
  
  # inclusion/exclusion of q0 data
  #inf[1,2:5,,1]<-NA  
  inf[1,,,]<-NA
  loglik<- data.inf * log((inf/number))+(number-data.inf)*log((1-(inf/number)))
  -sum(loglik,na.rm=T)  
}
n.param=3
#vrs.model<-optim(rep(0.05,n.param),vrs.binom,method="L-BFGS-B")
#vrs.model


vrs.model<-optim(c(0.3,0.8,0.8),vrs.binom,method="L-BFGS-B",lower=c(0.01,0.01,0.01),upper=c(0.99,0.99,0.99))
vrs.model

#
## CIs
#

size.of.grid<-20
optim.model<-vrs.binom(vrs.model$par)
v.range<-seq(0,1,length=size.of.grid)
r.range<-seq(0,1,length=size.of.grid)
s.range<-seq(0,1,length=size.of.grid)

ci.grid.v<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid))
ci.grid.r<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid))
ci.grid.s<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid))

for(a in 1:size.of.grid){
  for(b in 1:size.of.grid){
    for(d in 1:size.of.grid){
      p.vec<-c(v.range[a],r.range[b],s.range[d])
      ci.n.param<-length(vrs.model$par) 
      ci.fit<-vrs.binom(p.vec)     
      ci.grid.v[a,b,d]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v.range[a],NA)
      ci.grid.r[a,b,d]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,r.range[b],NA)
      ci.grid.s[a,b,d]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,s.range[d],NA)
    }
  }
  print(a)
}

v.ci<-range(ci.grid.v, na.rm=T)##effect size
r.ci<-range(ci.grid.r, na.rm=T)##per bite probability of infection from an infected mosquito to susceptible mouse
s.ci<-range(ci.grid.s, na.rm=T)##per bite probability of infection from an infected mouse to susceptible mosquito 
rbind(v.ci,r.ci,s.ci)
v<-mean(ci.grid.v, na.rm=T);r<-mean(ci.grid.r, na.rm=T);s<-mean(ci.grid.s, na.rm=T)
rbind(v,r,s)

rEstimates3d1150<-c(ci.grid.r,na.rm=T);rEstimates3d1150 <- rEstimates3d1150[!is.na(rEstimates3d1150)];length(rEstimates3d1150)
sEstimates3d1150<-c(ci.grid.s,na.rm=T);sEstimates3d1150 <- sEstimates3d1150[!is.na(sEstimates3d1150)];length(sEstimates3d1150)


