#
## Fitted chain binomial - replicating Blagborough et al 2013 Nat Comms, 5 mouse-mosquito rounds
#

data.mouse=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\hand over data and scripts\\5g chain binomial effect size\\atv32_mouse_final_data.txt",header=TRUE)
data.mosqu=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\hand over data and scripts\\5g chain binomial effect size\\atv32_mosquito_final_data.txt",header=TRUE)

data.mouse <- subset(data.mouse,Bites == "5")
data.mosqu <- subset(data.mosqu,Bites == "5" | Bites == "0")

data.mouse <- subset(data.mouse,Round == "4")
data.mosqu <- subset(data.mosqu,Round == "4" | Round == "0")


vrs.binom<-function(p.vec){
  m = 5  
  g = 4
  v<-p.vec[1]
  r<-p.vec[2]
  s<-p.vec[3]
  
  # Round, Bites, Treatment, Species; predicted (except for p0) number infected
  inf<-array(NA, dim = c(2,2,2,2))
  
  # Total numbers of mice and mosquitoes
  number<-array(NA, dim = c(2,2,2,2))
  
  # Enter p0 data
  inf[1,,1,2]<-5
  inf[1,,2,2]<-5
  number[1:2,,1,2]<-5
  number[1:2,,2,2]<-5
  
  # Predict q0
  number[1,1:2,1,1] <- 50
  inf[1,1:2,1,1] <- s * 50

  number[1,1:2,2,1] <- 50
  inf[1,1:2,2,1] <- s * (1-v) * 50
    
  # Predcit (p1-p5 and q1-q5), loops for rounds and bites 
  for(a in 1){
    for(b in 1:2){

      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==m & data.mosqu$Round==g & data.mosqu$Treatment==0)
      number[a+1,b,1,1] <- length(mosqu.temp$Oocyst)
    
      inf[a+1,b,1,2] <- (1 - (1 - r * (inf[a,b,1,1]/number[a,b,1,1])) ^ m) * number[a+1,b,1,2]                # mosquito to mouse
      inf[a+1,b,1,1] <- ((inf[a+1,b,1,2]/number[a+1,b,1,2]) * s) * number[a+1,b,1,1]                          # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==m & data.mosqu$Round==g & data.mosqu$Treatment==1)
      number[a+1,b,2,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,2,2] <- (1 - (1 - r * (inf[a,b,2,1]/number[a,b,2,1]) ) ^ m) * number[a+1,b,2,2]               # mosquito to mouse
      inf[a+1,b,2,1] <- ((inf[a+1,b,2,2]/number[a+1,b,2,2]) * s) * (1-v) * number[a+1,b,2,1]                  # mouse to mosquito
    }
  }
  
# Arrange data into appropriate format for fitting
  data.inf<-array(NA, dim = c(2,2,2,2))    
  data.inf[1,1:2,1,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==0]>0)
  data.inf[1,1:2,2,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==1]>0)
  for(a in 1){
    for(b in 1:2){
      data.inf[a+1,b,1,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==m & data.mouse$Round==g & data.mouse$Treatment==0]>0)
      data.inf[a+1,b,1,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==m & data.mosqu$Round==g & data.mosqu$Treatment==0]>0)

      data.inf[a+1,b,2,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==m & data.mouse$Round==g & data.mouse$Treatment==1]>0)
      data.inf[a+1,b,2,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==m & data.mosqu$Round==g & data.mosqu$Treatment==1]>0)
    }
  }
  
  # inclusion/exclusion of q0 data
  #inf[1,2:5,,1]<-NA  
  inf[1,,,]<-NA

  loglik<- data.inf * log((inf/number))+(number-data.inf)*log((1-(inf/number)))
  -sum(loglik,na.rm=T)  
}

n.param<-3
vrs.model<-optim(rep(0.05,n.param),vrs.binom,method="L-BFGS-B",lower=rep(0.001,n.param),upper=rep(0.99,n.param))
vrs.model

#
## CIs
#

size.of.grid<-20
optim.model<-vrs.binom(vrs.model$par)
v.range<-seq(0.001,0.99,length=size.of.grid)
r.range<-seq(0.1,0.999,length=size.of.grid)
s.range<-seq(0.2,0.999,length=size.of.grid)

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

v.ci<-c(vrs.model$par[1],range(ci.grid.v, na.rm=T))
r.ci<-c(vrs.model$par[2],range(ci.grid.r, na.rm=T))
s.ci<-c(vrs.model$par[3],range(ci.grid.s, na.rm=T))
rbind(v.ci,r.ci,s.ci)



#
## Fitted chain binomial - replicating Blagborough et al 2013 Nat Comms, 4 mouse-mosquito rounds
#

data.mouse=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\hand over data and scripts\\5g chain binomial effect size\\atv32_mouse_final_data.txt",header=TRUE)
data.mosqu=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\hand over data and scripts\\5g chain binomial effect size\\atv32_mosquito_final_data.txt",header=TRUE)

vrs.binom<-function(p.vec){
  
  v<-p.vec[1]
  r<-p.vec[2]
  s<-p.vec[3]
  
  # Round, Bites, Treatment, Species; predicted (except for p0) number infected
  inf<-array(NA, dim = c(5,5,2,2))
  
  # Total numbers of mice and mosquitoes
  number<-array(NA, dim = c(5,5,2,2))
  
  # Enter p0 data
  inf[1,,1,2]<-5
  inf[1,,2,2]<-5
  number[1:5,,1,2]<-5
  number[1:5,,2,2]<-5
  
  # Predict q0
  number[1,1:5,1,1] <- 50
  inf[1,1:5,1,1] <- s * 50
  
  number[1,1:5,2,1] <- 50
  inf[1,1:5,2,1] <- s * (1-v) * 50
  
  # Predcit (p1-p5 and q1-q5), loops for rounds and bites 
  for(a in 1:4){
    for(b in 1:5){
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==b & data.mosqu$Round==a & data.mosqu$Treatment==0)
      number[a+1,b,1,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,1,2] <- (1 - (1 - r * (inf[a,b,1,1]/number[a,b,1,1])) ^ b) * number[a+1,b,1,2]                # mosquito to mouse
      inf[a+1,b,1,1] <- ((inf[a+1,b,1,2]/number[a+1,b,1,2]) * s) * number[a+1,b,1,1]                          # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==b & data.mosqu$Round==a & data.mosqu$Treatment==1)
      number[a+1,b,2,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,2,2] <- (1 - (1 - r * (inf[a,b,2,1]/number[a,b,2,1]) ) ^ b) * number[a+1,b,2,2]               # mosquito to mouse
      inf[a+1,b,2,1] <- ((inf[a+1,b,2,2]/number[a+1,b,2,2]) * s) * (1-v) * number[a+1,b,2,1]                  # mouse to mosquito
    }
  }
  
  # Arrange data into appropriate format for fitting
  data.inf<-array(NA, dim = c(5,5,2,2))    
  data.inf[1,1:5,1,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==0]>0)
  data.inf[1,1:5,2,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==1]>0)
  for(a in 1:4){
    for(b in 1:5){
      data.inf[a+1,b,1,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==b & data.mouse$Round==a & data.mouse$Treatment==0]>0)
      data.inf[a+1,b,1,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==b & data.mosqu$Round==a & data.mosqu$Treatment==0]>0)
      
      data.inf[a+1,b,2,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==b & data.mouse$Round==a & data.mouse$Treatment==1]>0)
      data.inf[a+1,b,2,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==b & data.mosqu$Round==a & data.mosqu$Treatment==1]>0)
    }
  }
  
  # inclusion/exclusion of q0 data
  #inf[1,2:5,,1]<-NA  
  inf[1,,,]<-NA
  
  loglik<- data.inf * log((inf/number))+(number-data.inf)*log((1-(inf/number)))
  -sum(loglik,na.rm=T)  
}

n.param<-3
vrs.model<-optim(rep(0.05,n.param),vrs.binom,method="L-BFGS-B",lower=rep(0.01,n.param),upper=rep(0.99,n.param))
vrs.model

#
## CIs
#

size.of.grid<-20
optim.model<-vrs.binom(vrs.model$par)
v.range<-seq(0.1,0.3,length=size.of.grid)
r.range<-seq(0.6,0.9,length=size.of.grid)
s.range<-seq(0.6,0.9,length=size.of.grid)

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

v.ci<-c(vrs.model$par[1],range(ci.grid.v, na.rm=T))
r.ci<-c(vrs.model$par[2],range(ci.grid.r, na.rm=T))
s.ci<-c(vrs.model$par[3],range(ci.grid.s, na.rm=T))
rbind(v.ci,r.ci,s.ci)


