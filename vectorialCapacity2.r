
##Vectoral Capacity from Scott and Taken 2012

#***# First load ModelInstruction.r
#***# First load Figures from model output_1.r

ProbInfCtomouse<-sum(data[,252:267])/(500*16) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite2<-sum(data[,252]+data[,256]+data[,260]+data[,264])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite3<-sum(data[,253]+data[,257]+data[,261]+data[,265])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite4<-sum(data[,254]+data[,258]+data[,262]+data[,266])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite5<-sum(data[,255]+data[,259]+data[,263]+data[,267])/(500*4) ##mosquito (from model outputs for total model1)
mosqtomousetheta_perbite<-c(ProbInfCtomouseBite2,ProbInfCtomouseBite3,ProbInfCtomouseBite4,ProbInfCtomouseBite5)

ProbInfTtomouse<-sum(data[,268:283])/(500*16)
ProbInfCtomouseBite2T<-sum(data[,268]+data[,272]+data[,276]+data[,280])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite3T<-sum(data[,269]+data[,273]+data[,277]+data[,281])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite4T<-sum(data[,270]+data[,274]+data[,278]+data[,282])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite5T<-sum(data[,271]+data[,275]+data[,279]+data[,283])/(500*4) ##mosquito (from model outputs for total model1)
mosqtomousethetaT_perbite<-c(ProbInfCtomouseBite2T,ProbInfCtomouseBite3T,ProbInfCtomouseBite4T,ProbInfCtomouseBite5T)

m=c(2,3,4,5) ##number of bites per mosquito per day / ratio of mosquito to host (0.5,0.33,0.25,0.20)
a=0.8##
b=c(sum(prevooc1ConGens[1]+prevooc1ConGens[5]+prevooc1ConGens[9]+prevooc1ConGens[13])/4,
    sum(prevooc1ConGens[2]+prevooc1ConGens[6]+prevooc1ConGens[10]+prevooc1ConGens[14])/4,
    sum(prevooc1ConGens[3]+prevooc1ConGens[7]+prevooc1ConGens[11]+prevooc1ConGens[15])/4,
    sum(prevooc1ConGens[4]+prevooc1ConGens[8]+prevooc1ConGens[12]+prevooc1ConGens[16])/4)*mosqtomousetheta_perbite ##prevalence of infected mosquitoes for mouse controlsA
b2=c(sum(prevoocTgens[1]+prevoocTgens[5]+prevoocTgens[9]+prevoocTgens[13])/4,
     sum(prevoocTgens[2]+prevoocTgens[6]+prevoocTgens[10]+prevoocTgens[14])/4,
     sum(prevoocTgens[3]+prevoocTgens[7]+prevoocTgens[11]+prevoocTgens[15])/4,
     sum(prevoocTgens[4]+prevoocTgens[8]+prevoocTgens[12]+prevoocTgens[16])/4)*mosqtomousethetaT_perbite ##prevalence of infected mosquitoes for mouse ATV32
p=0.9
n=21

ProbInfCtomosq<-sum(data[,284:299])/(500*16) ##mosquito (from model outputs for total model1)
ProbInfCtomosq<-c(sum(data[,284:287])/(500*4),sum(data[,288:291])/(500*4),
                  sum(data[,292:295])/(500*4),sum(data[,296:299])/(500*4))##mosquito (from model outputs for total model1)
ProbInfTtomosq<-sum(data[,300:315])/(500*16)
ProbInfTtomosq<-c(sum(data[,300:303])/(500*4),sum(data[,304:307])/(500*4),
                  sum(data[,308:311])/(500*4),sum(data[,312:315])/(500*4))##mosquito (from model outputs for total model1)

b2

C0_8<-expand.grid(seq(1:length(b)))

for(j in 1:4){
for (i in 1:4){
  C0_8[i,j]=(m[j]*(a^2)*b[i]*(p^n))/-log(p)
}
}

plot(c(C0_8[,1])~c(1,2,3,4),xlim=c(0,4),ylim=c(0,3),cex.lab=1.4,
     ylab="Vectorial Capacity", xlab="Generation",bty="n")
lines(c(C0_8[,1])~c(1,2,3,4),col="red",lwd=2)
lines(c(C0_8[,2])~c(1,2,3,4),col="green",lwd=2)
lines(c(C0_8[,3])~c(1,2,3,4),col="blue",lwd=2)
lines(c(C0_8[,4])~c(1,2,3,4),col="black",lwd=2)

C2<-expand.grid(seq(1:length(b)))
for(j in 1:4){
  for (i in 1:length(b2)){
    C2[i,j]=(m[j]*(a^2)*b2[i]*(p^n))/-log(p)
  }
}
lines(c(C2[,1])~c(1:4),col="red",lty=2,lwd=2)
lines(c(C2[,2])~c(1:4),col="green",lty=2,lwd=2)
lines(c(C2[,3])~c(1:4),col="blue",lty=2,lwd=2)
lines(c(C2[,4])~c(1:4),col="black",lty=2,lwd=2)

legend(0,3,legend=c("a (daily rate host is bitten = 0.8)","p (daily mosquito survival rate = 0.9)",
                    "n (extrinsic incubation rate = 21)",
                    "b (vector competence = mosquito prevalence x probability of transmission)",
                    "2 mosquitoes per mouse (m)","3","4","5",
                                        "controls","ATV-32%"),
                    col=c("","","","","red","green","blue","black","black","black"),
                    lty=c(NA,NA,NA,NA,1,1,1,1,1,2),bty="n",lwd=2,cex=1.4)
                    
##This is just vector competence..
par(mar=c(5,5,2,2))
plot(b~c(2,3,4,5),bty="n",xaxt="n",
     ylim=c(0,0.5),xlab="Mean per generation",cex.lab=2,ylab="Vector competence")
axis(1,par(las=1),at=c(2:5),labels=c(2,3,4,5))
lines(b~c(2,3,4,5))
lines(b2~c(2,3,4,5),lty=2)

((b-b2)/b)*100



##what happens for increasing a's?
C_m2<-expand.grid(seq(1:length(a_test)))
a_test<-seq(0.01,0.9,0.01)
for(j in 1:length(a_test)){
  for(i in 1:4){
    C_m2[j,i]=(m[i]*(a_test[j]^2)*b[i]*(p^n))/-log(p)
  }}

C_m2T<-expand.grid(seq(1:length(a_test)))
a_test<-seq(0.01,0.9,0.01)
for(j in 1:length(a_test)){
  for(i in 1:4){
    C_m2T[j,i]=(m[i]*(a_test[j]^2)*b2[i]*(p^n))/-log(p)
  }}

plot(c(0,C_m2[,1])~c(0,a_test),xlim=c(0,1),ylim=c(0,2),pch="",
     ylab="Vectorial Capacity", xlab="Daily rate at which mosquito bites host")
lines(c(0,C_m2[,1])~c(0,a_test),col="red")
lines(c(0,C_m2[,2])~c(0,a_test),col="green")
lines(c(0,C_m2[,3])~c(0,a_test),col="blue")
lines(c(0,C_m2[,4])~c(0,a_test),col="black")

lines(c(0,C_m2T[,1])~c(0,a_test),col="red",lty=2)
lines(c(0,C_m2T[,2])~c(0,a_test),col="green",lty=2)
lines(c(0,C_m2T[,3])~c(0,a_test),col="blue",lty=2)
lines(c(0,C_m2T[,4])~c(0,a_test),col="black",lty=2)

###
#### What about probability of infection x vectorial capacity (Smith )
###

Cprob<-C0_8 * ProbInfCtomosq
CprobT<-C0_8 * ProbInfTtomosq

plot(c(0,Cprob[,1])~c(0,m),xlim=c(0,5),ylim=c(0,1),ylab="Vectorial Capacity x Prob of mosquito infection", xlab="Number of Bites")
lines(c(0,Cprob[,1])~c(0,m),col="red",lty=1,lwd=2)
lines(c(0,Cprob[,2])~c(0,m),col="green",lty=1,lwd=2)
lines(c(0,Cprob[,3])~c(0,m),col="blue",lty=1,lwd=2)
lines(c(0,Cprob[,4])~c(0,m),col="black",lty=1,lwd=2)

lines(c(0,CprobT[,1])~c(0,m),col="red",lty=2,lwd=2)
lines(c(0,CprobT[,2])~c(0,m),col="green",lty=2,lwd=2)
lines(c(0,CprobT[,3])~c(0,m),col="blue",lty=2,lwd=2)
lines(c(0,CprobT[,4])~c(0,m),col="black",lty=2,lwd=2)

##
###
####
##### The lifetime transmission potential of a mosquito
####  (Smith and McKenzie 2004 )
###
##
ProbInfCtomouse<-sum(data[,252:267])/(500*16) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite2<-sum(data[,252:255])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite3<-sum(data[,256:259])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite4<-sum(data[,260:263])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite5<-sum(data[,264:267])/(500*4) ##mosquito (from model outputs for total model1)
mosqtomousetheta_perbite<-c(ProbInfCtomouseBite2,ProbInfCtomouseBite3,ProbInfCtomouseBite4,ProbInfCtomouseBite5)

ProbInfTtomouse<-sum(data[,268:283])/(500*16)
ProbInfCtomouseBite2T<-sum(data[,268:271])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite3T<-sum(data[,272:275])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite4T<-sum(data[,276:279])/(500*4) ##mosquito (from model outputs for total model1)
ProbInfCtomouseBite5T<-sum(data[,280:283])/(500*4) ##mosquito (from model outputs for total model1)
mosqtomousethetaT_perbite<-c(ProbInfCtomouseBite2T,ProbInfCtomouseBite3T,ProbInfCtomouseBite4T,ProbInfCtomouseBite5T)

m=c(2,3,4,5) ##number of bites per mosquito per day / ratio of mosquito to host (0.5,0.33,0.25,0.20)
a=0.8##
b=prevooc1*mosqtomousetheta_perbite ##prevalence of infected mosquitoes for mouse controlsA
b2=prevoocT*mosqtomousethetaT_perbite ##prevalence of infected mosquitoes for mouse ATV32
p=0.9
n=21

ProbInfCtomosq<-sum(data[,284:299])/(500*16) ##mosquito (from model outputs for total model1)
ProbInfCtomosq<-c(sum(data[,284:287])/(500*4),sum(data[,288:291])/(500*4),
                  sum(data[,292:295])/(500*4),sum(data[,296:299])/(500*4))##mosquito (from model outputs for total model1)
ProbInfTtomosq<-sum(data[,300:315])/(500*16)
ProbInfTtomosq<-c(sum(data[,300:303])/(500*4),sum(data[,304:307])/(500*4),
                  sum(data[,308:311])/(500*4),sum(data[,312:315])/(500*4))##mosquito (from model outputs for total model1)

g = -log(p)
X = c(sum(PREVorigMean[1:4])/4,sum(PREVorigMean[5:8])/4,
      sum(PREVorigMean[9:12])/4,sum(PREVorigMean[13:16])/4)## proportion of hosts that are infected
X2 <- c(sum(PREVorigMean[17:20])/4,sum(PREVorigMean[21:24])/4,
        sum(PREVorigMean[25:28])/4,sum(PREVorigMean[29:32])/4)

Cbeta<-Tbeta<-as.numeric(4)

for(i in 1:4){
 Cbeta[i] <- ##the lifetime transmission potential of a mosquito
  ((a^2) * b[i] * ProbInfCtomosq[i] * X[i] * exp(-g * n))/(g * (g + a * ProbInfCtomosq[i] * X[i]))

 Tbeta[i] <- ##the lifetime transmission potential of a mosquito
   ((a^2) * b2[i] * ProbInfTtomosq[i] * X2[i] * exp(-g * n))/(g * (g + a * ProbInfTtomosq[i] * X2[i]))
 }

RelDiffBeta<-(Cbeta-Tbeta)/Cbeta


## RELATIVE DIFFERENCES: Cbeta increases with more mosquitos present
par(mfrow=c(1,1))
plot(RelDiffBeta~m,ylim=c(-1,1),
     xlab="Ratio of mosquitoes per host",xlim=c(2,5),xaxt="n",
     ylab="Relative difference",bty="n",
     cex.lab=1.4)
abline(a=0,b=0,col="grey",lty=2)
lines(RelDiffBeta~m,col="purple",lty=2,lwd=2)
axis(1,at=c(2,3,4,5),labels=c(2,3,4,5))
text(2.46,0.07,"Higher for controls",cex=2)
text(2.50,-0.07,"Higher for treatments",cex=2)
legend(3.5,1,legend=c("lifetime transmission potential","Individual vectorial capacity",
                    "Entomolgocial inoculation Rate","Ro"),
       lty=2,lwd=2,col=c("purple","orange","aquamarine","black"),cex=1.5,bty="n")
##
###
#### Individual vector capacity - Smith & McKenzie 2004
###
##

IC_Con = (a * ProbInfCtomosq * exp(-g * n))/g
IC_Treat = (a * ProbInfTtomosq * exp(-g * n))/g

reldiffIC<-(IC_Con-IC_Treat)/IC_Con
lines(reldiffIC~m,col="orange",lty=2,lwd=2)

##
###
####
##### EIR
####
###
##

EIR_c <- (m * a^2 * ProbInfCtomosq * X * exp(-g * n)) / (g + a * ProbInfCtomosq * X)

EIR_T <- (m * a^2 * ProbInfTtomosq * X2 * exp(-g * n)) / (g + a * ProbInfTtomosq * X2)

relEIR<-(EIR_c-EIR_T)/EIR_c
lines(relEIR~m,lty=2,lwd=2,col="aquamarine")

##
###
#### R0 (Smith and McKenzie 2004)
###
##

r = 10 ## number of days that the mouse is infectious in our expt

R0_C <- (m * a^2 * b * ProbInfCtomosq * exp(-g * n))/(g * r)

R0_T <- (m * a^2 * b2 * ProbInfTtomosq * exp(-g * n))/(g * r)
par(mfrow=c(1,1))
plot(c(0,R0_C)~c(0,m),ylim=c(0,0.5), cex.lab=1.4,
     ylab="R0",xlab="Ratio of mosquitoes per host")
lines(c(0,R0_C)~c(0,m),lwd=2)
lines(c(0,R0_T)~c(0,m),lty=20,lwd=2)

legend(0,0.20,legend=c("control","ATV-32%","c = Probability of transmission mouse to mosquito","g = ln(p)"),
       lty=c(1,2,NA,NA),bty="n",cex=1.2)

RelR01=(R0_C-R0_T)/R0_C

((mean(b) * mean(ProbInfCtomosq))-(mean(b2) * mean(ProbInfTtomosq)))/(mean(b) * mean(ProbInfCtomosq))

plot(RelR01~m,ylim=c(-1,1), cex.lab=1.4,pch=20,xaxt="n",cex=4,
     ylab="Relative difference in R0",xlab="Ratio of mosquitoes per host")
lines(RelR01~m,lwd=2,lty=2,col="black")
abline(a=0,b=0,col="grey",lty=2)
axis(1,at=c(2,3,4,5),labels=c(2,3,4,5))
text(2.42,0.07,"Higher for controls",cex=2)
text(2.50,-0.07,"Higher for treatments",cex=2)
#legend(0,0.20,legend=c("control","ATV-32%","c = Probability of transmission mouse to mosquito","g = ln(p)"),
#       lty=c(1,2,NA,NA),bty="n",cex=1.2)

##
###
#### R0 (Massad & Coutinho 2012)
###
##

r = 10 ## number of days that the mouse is infectious in our expt
mu = 0.1
m<-c(2,3,4,5)
R0_C <- (m * a^2 * b * exp(-mu * n))/(mu * r)

R0_T <- (m * a^2 * b2 * exp(-mu * n))/(mu * r)

plot(c(0,R0_C)~c(0,m),ylim=c(0,0.2), cex.lab=1.4,
     ylab="R0",xlab="Ratio of mosquitoes per host")
lines(c(0,R0_C)~c(0,m),lwd=2,col="blue")
lines(c(0,R0_T)~c(0,m),lty=20,col="blue",lwd=2)

legend(0,0.20,legend=c("control","ATV-32%","c = Probability of transmission mouse to mosquito","g = ln(p)"),
       lty=c(1,2,NA,NA),bty="n",cex=1.2)

################################################################################
## What about the effects through generations?

## 2-5 bites, 4 generations
mosqtomousetheta_per2bites<-c(sum(data[,252])/500,sum(data[,253])/500,sum(data[,254])/500,sum(data[,255])/500)
mosqtomousetheta_per3bites<-c(sum(data[,256])/500,sum(data[,257])/500,sum(data[,258])/500,sum(data[,259])/500)
mosqtomousetheta_per4bites<-c(sum(data[,260])/500,sum(data[,261])/500,sum(data[,262])/500,sum(data[,263])/500)
mosqtomousetheta_per5bites<-c(sum(data[,264])/500,sum(data[,265])/500,sum(data[,266])/500,sum(data[,267])/500)
constheta1<-c(mosqtomousetheta_per2bites,mosqtomousetheta_per3bites,mosqtomousetheta_per4bites,mosqtomousetheta_per5bites)

mosqtomousethetaT_per2bites<-c(sum(data[,268])/500,sum(data[,269])/500,sum(data[,270])/500,sum(data[,271])/500)
mosqtomousethetaT_per3bites<-c(sum(data[,272])/500,sum(data[,273])/500,sum(data[,274])/500,sum(data[,275])/500)
mosqtomousethetaT_per4bites<-c(sum(data[,276])/500,sum(data[,277])/500,sum(data[,278])/500,sum(data[,279])/500)
mosqtomousethetaT_per5bites<-c(sum(data[,280])/500,sum(data[,281])/500,sum(data[,282])/500,sum(data[,283])/500)
treattheta1<-c(mosqtomousethetaT_per2bites,mosqtomousethetaT_per3bites,mosqtomousethetaT_per4bites,mosqtomousethetaT_per5bites)

m=c(2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5) ##number of bites per mosquito per day / ratio of mosquito to host (0.5,0.33,0.25,0.20)
a=0.8##
b=prevooc1ConGens*constheta1 ##prevalence of infected mosquitoes for mouse controlsA
b2=prevoocTgens*treattheta1 ##prevalence of infected mosquitoes for mouse ATV32
p=0.9
n=21

ProbInfTtomosqGens<-ProbInfCtomosqGens<-numeric(length(16))
for(i in 1:16){
ProbInfCtomosqGens[i]<-sum(data[,283+i])/500}##mosquito (from model outputs for total model1)
for (j in 1:16){
ProbInfTtomosqGens[j]<-sum(data[,299+j])/500 }


C0_8<-expand.grid(seq(1:length(b)))

for(j in 1:4){
  for (i in 1:16){
    C0_8[i,j]=(unique(m)[j]*(a^2)*b[i]*(p^n))/-log(p)
  }
}
plot(C0_8[1:4,1]~c(1,2,3,4),xlim=c(0,5),ylim=c(0,1.2),ylab="Vectorial Capacity", xlab="Generations")
lines(C0_8[1:4,1]~c(1,2,3,4),col="red",pch=20)
lines(C0_8[5:8,1]~c(1,2,3,4),col="green",pch=20)
lines(C0_8[9:12,1]~c(1,2,3,4),col="blue",pch=20)
lines(C0_8[13:16,1]~c(1,2,3,4),col="black",pch=20)

C2<-expand.grid(seq(1:length(b)))
for(j in 1:4){
  for (i in 1:length(b2)){
    C2[i,j]=(unique(m)[j]*(a^2)*b2[i]*(p^n))/-log(p)
  }
}
lines(C2[1:4,1]~c(1,2,3,4),col="red",lty=2)
lines(C2[5:8,1]~c(1,2,3,4),col="green",lty=2)
lines(C2[9:12,1]~c(1,2,3,4),col="blue",lty=2)
lines(C2[13:16,1]~c(1,2,3,4),col="black",lty=2)

g = -log(p)
X = PREVorigMean[1:16]## proportion of hosts that are infected
X2 <- PREVorigMean[17:32]

par(mfrow=c(1,2))
boxplot(b[1:4],b2[1:4],
         b[5:8],b2[5:8],
         b[9:12],b2[9:12],
         b[13:16],b2[13:16],ylab="Vector competence",xlab="Mosquito to mouse ratio",
        #col=c("dark red","red","dark green","green","dark blue","blue","grey15","grey67"),
        col=c("grey15","grey67"),
        frame=F,cex.lab=1.5,xaxt="n")
axis(1,at=c(0.5,1.5,3.5,5.5,7.5,8.5),labels=c("","2","3","4","5",""))
legend(1,0.8,legend=c("Controls","ATV-32%"),
       pch=15,col=c("grey15","grey67"),bty="n",cex=2)


100*c((mean(b[1:4])-mean(b2[1:4]))/mean(b[1:4]),
(mean(b[5:8])-mean(b2[5:8]))/mean(b[5:8]),
(mean(b[9:12])-mean(b2[9:12]))/mean(b[9:12]),
(mean(b[13:16])-mean(b2[13:16]))/mean(b[13:16]))


##
###
#### R0
###
##

r = 10 ## number of days that the mouse is infectious in our expt
mouseMosqGens<-c(sum(ProbInfCtomosqGens[1]+ProbInfCtomosqGens[5]+
                    ProbInfCtomosqGens[9]+ProbInfCtomosqGens[13])/4,
                 sum(ProbInfCtomosqGens[2]+ProbInfCtomosqGens[6]+
                      ProbInfCtomosqGens[10]+ProbInfCtomosqGens[14])/4,
                 sum(ProbInfCtomosqGens[3]+ProbInfCtomosqGens[7]+
                      ProbInfCtomosqGens[11]+ProbInfCtomosqGens[15])/4,
                 sum(ProbInfCtomosqGens[4]+ProbInfCtomosqGens[8]+
                      ProbInfCtomosqGens[12]+ProbInfCtomosqGens[16])/4)

mouseMosqGensT<-c(sum(ProbInfTtomosqGens[1]+ProbInfTtomosqGens[5]+
                      ProbInfTtomosqGens[9]+ProbInfTtomosqGens[13])/4,
                  sum(ProbInfTtomosqGens[2]+ProbInfTtomosqGens[6]+
                      ProbInfTtomosqGens[10]+ProbInfTtomosqGens[14])/4,
                  sum(ProbInfTtomosqGens[3]+ProbInfTtomosqGens[7]+
                      ProbInfTtomosqGens[11]+ProbInfTtomosqGens[15])/4,
                  sum(ProbInfTtomosqGens[4]+ProbInfTtomosqGens[8]+
                      ProbInfTtomosqGens[12]+ProbInfTtomosqGens[16])/4)
R0_CGens<-R0_TGens<-expand.grid(seq(1,16,1))
for(j in 1:16){
for (i in 1:4){
R0_CGens[j,i] <- (unique(m)[i] * a^2 * b[j] * ProbInfCtomosqGens[j] * exp(-g * n))/(g * r)

R0_TGens[j,i] <- (unique(m)[i] * a^2 * b2[j] * ProbInfTtomosqGens[j] * exp(-g * n))/(g * r)
}}
plot(c(R0_CGens[1,1],R0_CGens[5,1],R0_CGens[9,1],R0_CGens[13,1])~c(1,2,3,4),ylim=c(0,0.2),
     xlim=c(0,4),ylab="R0",xlab="Generation",bty="n")
lines(c(R0_CGens[1,1],R0_CGens[5,1],R0_CGens[9,1],R0_CGens[13,1])~c(1,2,3,4),col="red",lwd=2)
lines(c(R0_CGens[1,2],R0_CGens[5,2],R0_CGens[9,2],R0_CGens[13,2])~c(1,2,3,4),col="green",lwd=2)
lines(c(R0_CGens[1,3],R0_CGens[5,3],R0_CGens[9,3],R0_CGens[13,3])~c(1,2,3,4),col="blue",lwd=2)
lines(c(R0_CGens[1,4],R0_CGens[5,4],R0_CGens[9,4],R0_CGens[13,4])~c(1,2,3,4),col="black",lwd=2)

lines(c(R0_TGens[1,1],R0_TGens[5,1],R0_TGens[9,1],R0_TGens[13,1])~c(1,2,3,4),col="red",lty=2,lwd=2)
lines(c(R0_TGens[1,2],R0_TGens[5,2],R0_TGens[9,2],R0_TGens[13,2])~c(1,2,3,4),col="green",lty=2,lwd=2)
lines(c(R0_TGens[1,3],R0_TGens[5,3],R0_TGens[9,3],R0_TGens[13,3])~c(1,2,3,4),col="blue",lty=2,lwd=2)
lines(c(R0_TGens[1,4],R0_TGens[5,4],R0_TGens[9,4],R0_TGens[13,4])~c(1,2,3,4),col="black",lty=2,lwd=2)

legend(0,0.18,legend=c("2 mosquitoes per mouse","3","4","5","controls","ATV-32%"),
              col=   c("red","green","blue","black","black","black"),
              lty=c(rep(1,5),2),lwd=2)  

plot(c(R0_CGens[1,1],R0_CGens[5,1],R0_CGens[9,1],R0_CGens[13,1])~c(ProbInfCtomosqGens[1],ProbInfCtomosqGens[5],ProbInfCtomosqGens[9],ProbInfCtomosqGens[13]),ylim=c(0,0.2),
     xlim=c(0,1),ylab="R0",xlab="Transmission probability Mouse to mosquito",bty="n")
lines(c(R0_CGens[1,1],R0_CGens[5,1],R0_CGens[9,1],R0_CGens[13,1])~c(ProbInfCtomosqGens[1],ProbInfCtomosqGens[5],ProbInfCtomosqGens[9],ProbInfCtomosqGens[13]),col="red",lwd=2)
lines(c(R0_CGens[1,2],R0_CGens[5,2],R0_CGens[9,2],R0_CGens[13,2])~c(ProbInfCtomosqGens[2],ProbInfCtomosqGens[6],ProbInfCtomosqGens[10],ProbInfCtomosqGens[14]),col="green",lwd=2)
lines(c(R0_CGens[1,3],R0_CGens[5,3],R0_CGens[9,3],R0_CGens[13,3])~c(ProbInfCtomosqGens[3],ProbInfCtomosqGens[7],ProbInfCtomosqGens[11],ProbInfCtomosqGens[15]),col="blue",lwd=2)
lines(c(R0_CGens[1,4],R0_CGens[5,4],R0_CGens[9,4],R0_CGens[13,4])~c(ProbInfCtomosqGens[4],ProbInfCtomosqGens[8],ProbInfCtomosqGens[12],ProbInfCtomosqGens[16]),col="black",lwd=2)

lines(c(R0_TGens[1,1],R0_TGens[5,1],R0_TGens[9,1],R0_TGens[13,1])~c(ProbInfTtomosqGens[1],ProbInfTtomosqGens[5],ProbInfTtomosqGens[9],ProbInfTtomosqGens[13]),col="red",lty=2,lwd=2)
lines(c(R0_TGens[1,2],R0_TGens[5,2],R0_TGens[9,2],R0_TGens[13,2])~c(ProbInfTtomosqGens[2],ProbInfTtomosqGens[6],ProbInfTtomosqGens[10],ProbInfTtomosqGens[14]),col="green",lty=2,lwd=2)
lines(c(R0_TGens[1,3],R0_TGens[5,3],R0_TGens[9,3],R0_TGens[13,3])~c(ProbInfTtomosqGens[3],ProbInfTtomosqGens[7],ProbInfTtomosqGens[11],ProbInfTtomosqGens[15]),col="blue",lty=2,lwd=2)
lines(c(R0_TGens[1,4],R0_TGens[5,4],R0_TGens[9,4],R0_TGens[13,4])~c(ProbInfTtomosqGens[4],ProbInfTtomosqGens[8],ProbInfTtomosqGens[12],ProbInfTtomosqGens[16]),col="black",lty=2,lwd=2)

legend(0,0.18,legend=c("2 mosquitoes per mouse","3","4","5","controls","ATV-32%"),
       col=   c("red","green","blue","black","black","black"),
       lty=c(rep(1,5),2),lwd=2)  
