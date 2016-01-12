#####################################################################################################
##                                                                                                 ##                       
### Fitted chain binomial - replicating Blagborough et al 2013 Nat Comms, 5 mouse-mosquito rounds  ##
### ALL TRIALS DATA IN A SINGLE MODEL                                                              ##
##                                                                                                 ##  
#####################################################################################################


###New data with 2, 5 and 10 mosquito biting rates
conMosquito<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mosquito.txt",header=TRUE)
conMosquito<-conMosquito[,1:4]
conMouse<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse3AddingCombinations.txt",header=TRUE)
conMouse<-conMouse[2:16]
levels(conMosquito$Treatment)[1]<-"CONTROL"

data.mouseTemp1=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mouse.txt",header=T)
data.mosquTemp1=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-25 Example\\mosquito.txt",header=T)
data.mosquTemp1$Treatment<-as.factor(data.mosquTemp1$Treatment)
data.mouseTemp1$Treatment<-as.factor(data.mouseTemp1$Treatment)
levels(data.mouseTemp1$Treatment)[1]<-"ATV25"
levels(data.mosquTemp1$Treatment)[1]<-"ATV25"


data.mouseTemp2=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-50 Graph\\mouse.txt",header=T)
data.mosquTemp2=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-50 Graph\\mosquito.txt",header=T)
data.mosquTemp2$Treatment<-as.factor(data.mosquTemp2$Treatment)
data.mouseTemp2$Treatment<-as.factor(data.mouseTemp2$Treatment)
data.mosquTemp2<-data.mosquTemp2[,1:4]
levels(data.mouseTemp2$Treatment)[1]<-"ATV50"
levels(data.mosquTemp2$Treatment)[1]<-"ATV50"

data.mouseTemp3=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-65\\mouse.txt",header=T)
data.mosquTemp3=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-65\\mosquito.txt",header=T)
data.mosquTemp3$Treatment<-as.factor(data.mosquTemp3$Treatment)
data.mouseTemp3$Treatment<-as.factor(data.mouseTemp3$Treatment)
data.mosquTemp3<-data.mosquTemp3[,1:4]
levels(data.mouseTemp3$Treatment)[1]<-"ATV65"
levels(data.mosquTemp3$Treatment)[1]<-"ATV65"

data.mouseTemp4=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-85\\mouse.txt",header=T)
data.mosquTemp4=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\ATV-85\\mosquito.txt",header=T)
data.mosquTemp4$Treatment<-as.factor(data.mosquTemp4$Treatment)
data.mouseTemp4$Treatment<-as.factor(data.mouseTemp4$Treatment)
data.mosquTemp4<-data.mosquTemp4[,1:4]
levels(data.mouseTemp4$Treatment)
levels(data.mosquTemp4$Treatment)[1]<-"ATV85"

data.mouseTemp5=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\4B7-85\\mouse.txt",header=T)
data.mosquTemp5=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\4B7-85\\mosquito.txt",header=T)
data.mosquTemp5$Treatment<-as.factor(data.mosquTemp5$Treatment)
data.mouseTemp5$Treatment<-as.factor(data.mouseTemp5$Treatment)
data.mosquTemp5<-data.mosquTemp5[,1:4]
levels(data.mouseTemp5$Treatment)[1]<-"4B7"
levels(data.mosquTemp5$Treatment)[1]<-"4B7"

data.mouseTemp6=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3D11-50\\mouse.txt",header=T)
data.mosquTemp6=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3D11-50\\mosquito.txt",header=T)
data.mosquTemp6$Treatment<-as.factor(data.mosquTemp6$Treatment)
data.mouseTemp6$Treatment<-as.factor(data.mouseTemp6$Treatment)
levels(data.mouseTemp6$Treatment)
levels(data.mosquTemp6$Treatment)

data.mouseTemp7=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 35ATV-85\\mouse.txt",header=T)
data.mosquTemp7=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 35ATV-85\\mosquito.txt",header=T)
data.mosquTemp7$Treatment<-as.factor(data.mosquTemp7$Treatment)
data.mouseTemp7$Treatment<-as.factor(data.mouseTemp7$Treatment)
data.mosquTemp7<-data.mosquTemp7[,1:4]
levels(data.mouseTemp7$Treatment)[1]<-"3d11ATV85"
levels(data.mosquTemp7$Treatment)[1]<-"3d11ATV85"

data.mouseTemp8=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 50and 4B7 85\\mouse.txt",header=T)
data.mosquTemp8=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\3d11 50and 4B7 85\\mosquito.txt",header=T)
data.mosquTemp8$Treatment<-as.factor(data.mosquTemp8$Treatment)
data.mouseTemp8$Treatment<-as.factor(data.mouseTemp8$Treatment)
data.mosquTemp8<-data.mosquTemp8[,1:4]
levels(data.mouseTemp8$Treatment)[1]<-"3d114b785"
levels(data.mosquTemp8$Treatment)[1]<-"3d114b785"

data.mosquA<-rbind(conMosquito,data.mosquTemp1)
data.mosquB<-rbind(data.mosquA,data.mosquTemp2)
data.mosquC<-rbind(data.mosquB,data.mosquTemp3)
data.mosquD<-rbind(data.mosquC,data.mosquTemp4)
data.mosquE<-rbind(data.mosquD,data.mosquTemp5)
data.mosquF<-rbind(data.mosquE,data.mosquTemp6)
data.mosquG<-rbind(data.mosquF,data.mosquTemp7)
data.mosqu<-rbind(data.mosquG,data.mosquTemp8)

data.mouse<-rbind(conMouse,data.mouseTemp1,data.mouseTemp2,data.mouseTemp3,data.mouseTemp4,
                  data.mouseTemp5,data.mouseTemp6,data.mouseTemp7,data.mouseTemp8)

summary(data.mouse);summary(data.mosqu)

#################
##
## Confirm the starting values for the inf and number arrays
##
###################################################
tapply(data.mouse$Treatment[data.mouse$Bites==1],data.mouse$Round[data.mouse$Bites==1],summary)
tapply(data.mouse$Treatment[data.mouse$Bites==2],data.mouse$Round[data.mouse$Bites==2],summary)
tapply(data.mouse$Treatment[data.mouse$Bites==5],data.mouse$Round[data.mouse$Bites==5],summary)
tapply(data.mouse$Treatment[data.mouse$Bites==10],data.mouse$Round[data.mouse$Bites==10],summary)
tapply(data.mosqu$Treatment[data.mosqu$Bites==1],data.mosqu$Round[data.mosqu$Bites==1],summary)
tapply(data.mosqu$Treatment[data.mosqu$Bites==2],data.mosqu$Round[data.mosqu$Bites==2],summary)
tapply(data.mosqu$Treatment[data.mosqu$Bites==5],data.mosqu$Round[data.mosqu$Bites==5],summary)
tapply(data.mosqu$Treatment[data.mosqu$Bites==10],data.mosqu$Round[data.mosqu$Bites==10],summary)


rs.binom<-function(p.vec){
  
  # v<-p.vec[1]  # use the efficacy values for the respective treatments
  r<-p.vec[1]
  s<-p.vec[2]
  
  # Round, Bites, Treatment, Species; predicted (except for p0) number infected ##3 rounds
  inf<-array(NA, dim = c(2,4,9,2))
  
  # Total numbers of mice and mosquitoes
  number<-array(NA, dim = c(2,4,9,2))
  
  # Enter p0 data
  inf[1,1,1,2]<-10  ##controls Mice
  inf[1,2:3,1,2]<-30  ##controls Mice
  inf[1,4,1,2]<-23  ##controls Mice
  inf[1,,2:9,2]<-5 ##Treatments Mice
  
  number[,1,1,2]<-10
  number[,2:3,1,2]<-30
  number[,4,1,2]<-23
  number[,,2:9,2]<-5
  
  # Predict q0
  number[1,1:4,1,1] <- c(95,82,95,50) ##Controls Mosquito
  inf[1,1:4,1,1] <- s * c(95,82,95,50)
  
  number[1,1:4,2,1] <- 50
  inf[1,1:4,2,1] <- s * (1-0.00) * 50  ##ATV25
  
  number[1,1:4,3,1] <- 51
  inf[1,1:4,3,1] <- s * (1-0.01435407) * 51 ##ATV50
  
  number[1,1:4,4,1] <- c(51,51,101,101)
  inf[1,1:4,4,1] <- s * (1-0.7456140) * c(51,51,101,101) ##ATV65
  
  number[1,1:4,5,1] <- 50
  inf[1,1:4,5,1] <- s * (1-0.8296399) * 50 ##ATV85  
  
  number[1,1:4,6,1] <- 50
  inf[1,1:4,6,1] <- s * (1-0.8328173) * 50 ##4B785  
  
  number[1,1:4,7,1] <- c(50,50,51,51)
  inf[1,1:4,7,1] <- s * (1-0.1978947) * c(50,50,51,51) ##3D11 50%
  
  number[1,1:4,8,1] <- 50
  inf[1,1:4,8,1] <- s * (1-0.9816934) * 50 ##3D11 50% & ATV 85%
  
  number[1,1:4,9,1] <- 51
  inf[1,1:4,9,1] <- s * (1-0.9816934) * 51 ##3D11 50% & 4B7 85%
  
  
  mbr.vec<-c(1,2,5,10)
  # Predcit (p1-p5 and q1-q5), loops for rounds and bites 
  for(a in 1){
    for(b in 1:4){
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="CONTROL")
      number[a+1,b,1,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,1,2] <- (1 - (1 - r * (inf[a,b,1,1]/number[a,b,1,1])) ^ mbr.vec[b]) * number[a+1,b,1,2]                # mosquito to mouse
      inf[a+1,b,1,1] <- ((inf[a+1,b,1,2]/number[a+1,b,1,2]) * s) * number[a+1,b,1,1]                          # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="ATV25")
      number[a+1,b,2,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,2,2] <- (1 - (1 - r * (inf[a,b,2,1]/number[a,b,2,1]) ) ^ mbr.vec[b]) * number[a+1,b,2,2]               # mosquito to mouse
      inf[a+1,b,2,1] <- ((inf[a+1,b,2,2]/number[a+1,b,2,2]) * s) * (1-0.0075) * number[a+1,b,2,1]                  # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="ATV50")
      number[a+1,b,3,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,3,2] <- (1 - (1 - r * (inf[a,b,3,1]/number[a,b,3,1]) ) ^ mbr.vec[b]) * number[a+1,b,3,2]               # mosquito to mouse
      inf[a+1,b,3,1] <- ((inf[a+1,b,3,2]/number[a+1,b,3,2]) * s) * (1-0.0628) * number[a+1,b,3,1]                  # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="ATV65")
      number[a+1,b,4,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,4,2] <- (1 - (1 - r * (inf[a,b,4,1]/number[a,b,4,1]) ) ^ mbr.vec[b]) * number[a+1,b,4,2]               # mosquito to mouse
      inf[a+1,b,4,1] <- ((inf[a+1,b,4,2]/number[a+1,b,4,2]) * s) * (1-0.7714) * number[a+1,b,4,1]                  # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="ATV85")
      number[a+1,b,5,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,5,2] <- (1 - (1 - r * (inf[a,b,5,1]/number[a,b,5,1]) ) ^ mbr.vec[b]) * number[a+1,b,5,2]               # mosquito to mouse
      inf[a+1,b,5,1] <- ((inf[a+1,b,5,2]/number[a+1,b,5,2]) * s) * (1-0.834) * number[a+1,b,5,1]                  # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="4B7")
      number[a+1,b,6,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,6,2] <- (1 - (1 - r * (inf[a,b,6,1]/number[a,b,6,1]) ) ^ mbr.vec[b]) * number[a+1,b,6,2]               # mosquito to mouse
      inf[a+1,b,6,1] <- ((inf[a+1,b,6,2]/number[a+1,b,6,2]) * s) * (1-0.8442) * number[a+1,b,6,1]                  # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="3d11")
      number[a+1,b,7,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,7,2] <- (1 - (1 - r * (inf[a,b,7,1]/number[a,b,7,1]) ) ^ mbr.vec[b]) * number[a+1,b,7,2]               # mosquito to mouse
      inf[a+1,b,7,1] <- ((inf[a+1,b,7,2]/number[a+1,b,7,2]) * s) * (1-0.2864) * number[a+1,b,7,1]                  # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="3d11ATV85")
      number[a+1,b,8,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,8,2] <- (1 - (1 - r * (inf[a,b,8,1]/number[a,b,8,1]) ) ^ mbr.vec[b]) * number[a+1,b,8,2]               # mosquito to mouse
      inf[a+1,b,8,1] <- ((inf[a+1,b,8,2]/number[a+1,b,8,2]) * s) * (1-0.9673) * number[a+1,b,8,1]                  # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="3d114b785")
      number[a+1,b,9,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,9,2] <- (1 - (1 - r * (inf[a,b,2,1]/number[a,b,9,1]) ) ^ mbr.vec[b]) * number[a+1,b,9,2]               # mosquito to mouse
      inf[a+1,b,9,1] <- ((inf[a+1,b,9,2]/number[a+1,b,9,2]) * s) * (1-0.9835) * number[a+1,b,9,1]                  # mouse to mosquito
    }
  }
  
  # Arrange data into appropriate format for fitting
  data.inf<-array(NA, dim = c(2,4,9,2))    
  data.inf[1,1:4,1,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="CONTROL"]>0)
  data.inf[1,1:4,2,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="ATV25"]>0)
  data.inf[1,1:4,3,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="ATV50"]>0)
  data.inf[1,1:4,4,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="ATV65"]>0)
  data.inf[1,1:4,5,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="ATV85"]>0)
  data.inf[1,1:4,6,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="4B7"]>0)
  data.inf[1,1:4,7,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="3d11"]>0)
  data.inf[1,1:4,8,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="3d11ATV85"]>0)
  data.inf[1,1:4,9,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment=="3d114b785"]>0)
  
  for(a in 1){
    for(b in 1:4){
      data.inf[a+1,b,1,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="CONTROL"]>0)
      data.inf[a+1,b,1,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="CONTROL"]>0)
      
      data.inf[a+1,b,2,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="ATV25"]>0)
      data.inf[a+1,b,2,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="ATV25"]>0)
      
      data.inf[a+1,b,3,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="ATV50"]>0)
      data.inf[a+1,b,3,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="ATV50"]>0)
      
      data.inf[a+1,b,4,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="ATV65"]>0)
      data.inf[a+1,b,4,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="ATV65"]>0)
      
      data.inf[a+1,b,5,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="ATV85"]>0)
      data.inf[a+1,b,5,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="ATV85"]>0)
      
      data.inf[a+1,b,6,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="4B7"]>0)
      data.inf[a+1,b,6,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="4B7"]>0)
      
      data.inf[a+1,b,7,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="3d11"]>0)
      data.inf[a+1,b,7,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="3d11"]>0)
      
      data.inf[a+1,b,8,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="3d11ATV85"]>0)
      data.inf[a+1,b,8,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="3d11ATV85"]>0)
      
      data.inf[a+1,b,9,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment=="3d114b785"]>0)
      data.inf[a+1,b,9,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment=="3d114b785"]>0)
      
    }
  }
  
  # inclusion/exclusion of q0 data
  #inf[1,2:5,,1]<-NA  
  inf[1,,,]<-NA
  
  loglik<- data.inf * log((inf/number))+(number-data.inf)*log((1-(inf/number)))
  -sum(loglik,na.rm=T)  
}

n.param<-2
vrs.model<-optim(rep(0.05,n.param),rs.binom,method="L-BFGS-B",lower=rep(0.01,n.param),upper=rep(0.99,n.param))
vrs.model


#
## CIs
#

size.of.grid<-40
optim.model<-rs.binom(vrs.model$par)
#v.range<-seq(0,1,length=size.of.grid)
r.range<-seq(0,1,length=size.of.grid)
s.range<-seq(0,1,length=size.of.grid)

#ci.grid.v<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid))
ci.grid.r<-array(0, dim = c(size.of.grid,size.of.grid))
ci.grid.s<-array(0, dim = c(size.of.grid,size.of.grid))

#for(a in 1:size.of.grid){
for(b in 1:size.of.grid){
  for(d in 1:size.of.grid){
    p.vec<-c(r.range[b],s.range[d])
    ci.n.param<-length(vrs.model$par) 
    ci.fit<-rs.binom(p.vec)     
    ci.grid.r[b,d]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,r.range[b],NA)
    ci.grid.s[b,d]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,s.range[d],NA)
  }
  print(b)
}


#v.ci<-range(ci.grid.v, na.rm=T)##effect size
r.ci<-range(ci.grid.r, na.rm=T)##per bite probability of infection from an infected mosquito to susceptible mouse
s.ci<-range(ci.grid.s, na.rm=T)##per bite probability of infection from an infected mouse to susceptible mosquito 
rbind(r.ci,s.ci)
r<-mean(ci.grid.r, na.rm=T);s<-mean(ci.grid.s, na.rm=T)
rbind(r,s)