#
## M2M, estimate effect sizes for nitd609 and oz439 from mouse-to-mouse experiments
#

data.mouse3=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\M2M_Jan 2016\\m2m_12012016_mous_slot2.txt",header=T)
data.mosqu3=read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\M2M_Jan 2016\\m2m_12012016_mosq_slot2.txt",header=T)
#data.mouse4=read.table("Q:\\Paddy\\Analysis\\Sept mouse to mouse\\M2M-4_mou.txt",header=T)
#data.mosqu4=read.table("Q:\\Paddy\\Analysis\\Sept mouse to mouse\\M2M-4_moq.txt",header=T)

data.mouse3
data.mosqu3
data.mouse4
data.mosqu4

# remove data from other treatments
ds3<-data.mouse3[data.mouse3$treat!="ATV" & data.mouse3$treat!="SD" ,]
dq3<-data.mosqu3[data.mosqu3$treat!="ATV" & data.mosqu3$treat!="SD" ,]
#ds4<-data.mouse4[data.mouse4$treat!="pq" & data.mouse4$treat!="sd" & data.mouse4$treat!="atv",]
#dq4<-data.mosqu4[data.mosqu4$treat!="pq" & data.mosqu4$treat!="sd" & data.mosqu4$treat!="atv",]

ds3
dq3
ds4
dq4

vrs.binom<-function(p.vec){
  
  v1<-p.vec[1]
  v2<-p.vec[2]
  v3<-p.vec[3]
  r<-p.vec[4]
  s<-p.vec[5]
  
  # Round, Bites, Treatment, Species; number infected
  inf1<-array(NA, dim = c(2,3,4,2))
  
  # Total number
  number1<-array(NA, dim = c(2,3,4,2))
  
  # Enter p0
  inf1[1,,,2]<-5
  
  number1[,1,1,2]<-ds3$tot_mou[1]
  number1[,1,2,2]<-ds3$tot_mou[2]
  number1[,1,3,2]<-ds3$tot_mou[3]
  number1[,1,4,2]<-ds3$tot_mou[4]
  number1[,2,1,2]<-ds3$tot_mou[5]
  number1[,2,2,2]<-ds3$tot_mou[6]
  number1[,2,3,2]<-ds3$tot_mou[7]
  number1[,2,4,2]<-ds3$tot_mou[8]
  number1[,3,1,2]<-ds3$tot_mou[9]
  number1[,3,2,2]<-ds3$tot_mou[10]
  number1[,3,3,2]<-ds3$tot_mou[11]
  number1[,3,4,2]<-ds3$tot_mou[12]
  
  # Pre-calc q0
  for(k in 1:4){
    number1[,,k,1] <- dq3$tot_mosq[k]  
  }
  
  # mosquito infection
  inf1[1,,1,1] <- s * number1[1,,1,1]
  inf1[1,,2,1] <- s * number1[1,,2,1]*(1-v1)
  inf1[1,,3,1] <- s * number1[1,,3,1]*(1-v2)
  inf1[1,,4,1] <- s * number1[1,,3,1]*(1-v3)
  
  # mouse infection
  a<-1
  mbr.vec<-c(2,5,10)
  for(b in 1:3){
    inf1[a+1,b,,2] <- (1 - (1 - r * (inf1[a,b,,1]/number1[a,b,,1])) ^ mbr.vec[b]) * number1[a+1,b,,2]
  }
  
  # Arrange data into appropriate format for fitting
  data.inf1<-array(NA, dim = c(2,3,4,2))  
  
  for(k in 1:4){
    data.inf1[1,,k,1] <- dq3$inf_mosq[k]
  }
  
  a<-1
  for(b in 1:3){
    data.inf1[a+1,b,1,2]<-ds3$inf_mou[ds3$treat=="Blank" & ds3$mbr==mbr.vec[b]]
    data.inf1[a+1,b,2,2]<-ds3$inf_mou[ds3$treat=="DHA" & ds3$mbr==mbr.vec[b]]
    data.inf1[a+1,b,3,2]<-ds3$inf_mou[ds3$treat=="666" & ds3$mbr==mbr.vec[b]]
    data.inf1[a+1,b,4,2]<-ds3$inf_mou[ds3$treat=="852" & ds3$mbr==mbr.vec[b]]
  }
  
  # remove entered mouse data 
  inf1[1,,,2]<-NA
  
  # and pseudoreplicated mosquito data
  inf1[1,2:3,,1]<-NA
  
  inf<-inf1
  data.inf<-data.inf1
  number<-number1
  
  loglik<- data.inf * log((inf/number))+(number-data.inf)*log((1-(inf/number)))
  -sum(loglik,na.rm=T)  
}

n.param<-5
vrs.model<-optim(rep(0,n.param),vrs.binom,method="L-BFGS-B",lower=rep(0.01,n.param),upper=rep(0.99,n.param))
vrs.model

#
##
#

optim.model<-vrs.binom(vrs.model$par)

size.of.grid<-50
v1.range<-seq(0.01,0.99,length=size.of.grid)
v2.range<-seq(0.01,0.99,length=size.of.grid)
v3.range<-seq(0.01,0.99,length=size.of.grid)
v4.range<-seq(0.01,0.99,length=size.of.grid)

ci.grid.v1<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.v2<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.v3<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid))
ci.grid.v4<-array(0, dim = c(size.of.grid,size.of.grid,size.of.grid,size.of.grid))

for(a in 1:size.of.grid){
  for(b in 1:size.of.grid){
    for(d in 1:size.of.grid){
      for(e in 1:size.of.grid){
        p.vec<-c(v1.range[a],v2.range[b],v3.range[d],v4.range[e])
        ci.n.param<-length(p.vec) 
        ci.fit<-vrs.binom(p.vec)     
        ci.grid.v1[a,b,d,e]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v1.range[a],NA)
        ci.grid.v2[a,b,d,e]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v2.range[b],NA)
        ci.grid.v3[a,b,d,e]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v3.range[d],NA)
        ci.grid.v4[a,b,d,e]<-ifelse(ci.fit<=optim.model+qchisq(0.95, ci.n.param)/2,v4.range[e],NA)
      }
    }
  }
  print(a)
}

v1.ci<-range(ci.grid.v1, na.rm=T)
v2.ci<-range(ci.grid.v2, na.rm=T)
v3.ci<-range(ci.grid.v3, na.rm=T)
v4.ci<-range(ci.grid.v4, na.rm=T)

rbind(v1.ci,v2.ci,v3.ci,v4.ci)
