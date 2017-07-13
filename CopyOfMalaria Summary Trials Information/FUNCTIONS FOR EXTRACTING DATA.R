##
####
###### FUNCTION TO EXTRACT OOCYST DATA

oocysts_graph <- function(data,nrnds,nbites,rounds,comb) {
  
  bites <- rep(c(1,2,5,10),each=nrnds)
  bites2 <- c(1,2,5,10)

  ###Oocyst intensity mean and CI95%
  a <- aa <- array(dim=c(100,nrnds*nbites))
  store <- store_prev <- array(dim = c(nrnds, nbites))
  
  storeLU <- array( dim=c(nrnds*nbites,2))
  storeLU_prev <- array( dim=c(nrnds*nbites,2))
  for (k in 1:4) {
    for (j in 1:4)  {
      for(i in 1:100) {
        for (m in 1:comb) {
          a[i,m] <- mean(sample(data$Oocyst[data$Round==rounds[m] & data$Bites==bites[m]],replace=T),na.rm=T)
          storeLU[m,1:2] <- quantile(a[,m],c(0.025,0.975),na.rm=T)
          store[j,k] <- mean(data$Oocyst[data$Round==rounds[j] & data$Bites==bites2[k]],na.rm=T)
        }
      }
    }
  }
  for (k in 1:4) {
    for (j in 1:4)  {
      for(i in 1:100) {
        for (m in 1:comb) {
          a[i,m] <- mean(sample(data$OocPrev[data$Round==rounds[m] & data$Bites==bites[m]],replace=T),na.rm=T)
          storeLU_prev[m,1:2] <- quantile(a[,m],c(0.025,0.975),na.rm=T)
          store_prev[j,k] <- mean(data$OocPrev[data$Round==rounds[j] & data$Bites==bites2[k]],na.rm=T)
        }
      }
    }
  }
  return (list(store,storeLU,
               100*store_prev,100*storeLU_prev))
}

##
####
###### FUNCTION TO EXTRACT BLOODSTAGE DATA

bloodstage_graph <- function(data,nrnds,nbites,rounds,comb) {
  
  bites <- rep(c(1,2,5,10),each=nrnds)
  bites2 <- c(1,2,5,10)
  
  ###Oocyst intensity mean and CI95%
  a <- array(dim=c(100,nrnds*nbites))
  storeblood <- array(dim = c(nrnds, nbites))
    storebloodLU <- array( dim=c(nrnds*nbites,2))
  
  for (k in 1:4) {
    for (j in 1:4)  {
      for(i in 1:100) {
        for (m in 1:comb) {
          a[i,m] <- mean(sample(data$bloodstage[data$Round==rounds[m] & data$Bites==bites[m]],replace=T),na.rm=T)
          storebloodLU[m,1:2] <- quantile(a[,m],c(0.025,0.975),na.rm=T)
          storeblood[j,k] <- mean(data$bloodstage[data$Round==rounds[j] & data$Bites==bites2[k]],na.rm=T)
        }
      }
    }
  }
  return (list(storeblood,storebloodLU))
}


##
####
###### FUNCTION TO EXTRACT SPOROZOITE DATA

spors_f <- function(data,comb,nrowsdata,
                    num_sp, ##This is the 1st column containing the sporozoite data
                    num_sp_9, ##This is the last column containing the sporozoite data
                    seque) {
  
  spors_prevs <- array (dim=c(nrow(data),10))
   
  for (i in num_sp:num_sp_9) {
        spors_prevs[,i-5] <- ifelse(data[,i]==0,0,1)
  }

  a <- array(dim=c(100,comb))
  store_spors <- numeric(comb)
  store_sporsLU <- array( dim=c(comb,2))
  
  spors_sum <- numeric(comb)
    for (j in 1:comb) {
    store_spors[j] <- mean(c(spors_prevs[seque[j],],
                         spors_prevs[seque[j]+1,],
                         spors_prevs[seque[j]+2,],
                         spors_prevs[seque[j]+3,],
                         spors_prevs[seque[j]+4,]),na.rm=T)
  }
  
  for(i in 1:100) {
    for (j in 1:comb) {
      a[i,j] <- mean(sample(c(spors_prevs[seque[j],],
                       spors_prevs[seque[j]+1,],
                       spors_prevs[seque[j]+2,],
                       spors_prevs[seque[j]+3,],
                       spors_prevs[seque[j]+4,]),replace=T),na.rm=T)
      store_sporsLU[j,1:2] <-  quantile(a[,j],c(0.025,0.975),na.rm=T)
    }
  }
  
  return(list(spors_prevs,
              store_spors,
              store_sporsLU#,
            #  store_spors_int,
            #  store_spors_intLU
            ))
}


###Intensity  

spor_score <- function(spors,Rounds,nrnds,nbites,comb) {
  
  Sporozoite1a <- Sporozoite1a2 <- numeric (nrnds)
  Sporozoite2a <- Sporozoite2a2 <- numeric (nrnds)
  Sporozoite5a <- Sporozoite5a2 <- numeric (nrnds)
  Sporozoite10a <- Sporozoite10a2 <- numeric (nrnds)
  
  store_spors_int <- array(dim=c(nrnds,nbites))
  store_spors_intLU_1 <- store_spors_intLU_2 <- store_spors_intLU_5 <- store_spors_intLU_10 <- array(dim=c(nrnds,2))
  Spa <- array(dim=c(100,nrnds))
  store_sporsUL <- array(dim=c(comb,2)) 
  
  for (j in 1:100) {
    for (i in 1:nrnds)
      {
    Spa[j,i]<-mean(sample(c(spors$Sporozoite1[spors$Round==Rounds[i] & spors$Bites==1]),replace=T),na.rm=T)
    store_spors_intLU_1[i,1:2] <-  quantile(Spa[,i],c(0.025,0.975),na.rm=T)
    store_spors_int[i,1] <- mean(spors$Sporozoite1[spors$Round==Rounds[i] & spors$Bites==1])
    }}


  for (j in 1:100) {
    for (i in 1:nrnds)
    {
      Spa[j,i]<-mean(sample(c(spors$Sporozoite1[spors$Round==Rounds[i] & spors$Bites==2],
                              spors$Sporozoite2[spors$Round==Rounds[i] & spors$Bites==2]),replace=T),na.rm=T)
      store_spors_intLU_2[i,1:2] <-  quantile(Spa[,i],c(0.025,0.975),na.rm=T)
      store_spors_int[i,2] <- mean(c(spors$Sporozoite1[spors$Round==Rounds[i] & spors$Bites==2],
                                     spors$Sporozoite2[spors$Round==Rounds[i] & spors$Bites==2]),na.rm=T)
    }}
  
    
  for (j in 1:100) {
    for (i in 1:nrnds)
    {
      Spa[j,i]<-mean(sample(c(spors$Sporozoite1[spors$Round==Rounds[i] & spors$Bites==5],
                              spors$Sporozoite2[spors$Round==Rounds[i] & spors$Bites==5],
                              spors$Sporozoite3[spors$Round==Rounds[i] & spors$Bites==5],
                              spors$Sporozoite4[spors$Round==Rounds[i] & spors$Bites==5],
                              spors$Sporozoite5[spors$Round==Rounds[i] & spors$Bites==5]),replace=T),na.rm=T)
      store_spors_intLU_5[i,1:2] <-  quantile(Spa[,i],c(0.025,0.975),na.rm=T)
      store_spors_int[i,3] <- mean(c(spors$Sporozoite1[spors$Round==Rounds[i] & spors$Bites==5],
                                     spors$Sporozoite2[spors$Round==Rounds[i] & spors$Bites==5],
                                     spors$Sporozoite3[spors$Round==Rounds[i] & spors$Bites==5],
                                     spors$Sporozoite4[spors$Round==Rounds[i] & spors$Bites==5],
                                     spors$Sporozoite5[spors$Round==Rounds[i] & spors$Bites==5]),na.rm=T)
    }}
  

  for (j in 1:100) {
    for (i in 1:nrnds)
    {
      Spa[j,i]<-mean(sample(c(spors$Sporozoite1[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite2[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite3[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite4[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite5[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite6[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite7[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite8[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite9[spors$Round==Rounds[i] & spors$Bites==10],
                              spors$Sporozoite10[spors$Round==Rounds[i] & spors$Bites==10]),replace=T),na.rm=T)
      store_spors_intLU_10[i,1:2] <-  quantile(Spa[,i],c(0.025,0.975),na.rm=T)
      store_spors_int[i,4] <- mean(c(spors$Sporozoite1[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite2[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite3[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite4[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite5[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite6[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite7[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite8[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite9[spors$Round==Rounds[i] & spors$Bites==10],
                                     spors$Sporozoite10[spors$Round==Rounds[i] & spors$Bites==10]),na.rm=T)
    }
  }
  
  store_sporsUL <- rbind(store_spors_intLU_1,store_spors_intLU_2,store_spors_intLU_5,store_spors_intLU_10)
 
  return(list(store_spors_int,
              store_sporsUL))
  
}



#####################################
##
## Data and functions for graphs
##
##
#######################################

t4b765b<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\4B7-65\\mouse.txt",header=TRUE)
t4b765b$bloodstage<-ifelse(t4b765b$Parasitemia==0,0,1)
summary(t4b765b)
t4b765<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\4B7-65\\mosquito.txt",header=TRUE)
t4b765$OocPrev<-ifelse(t4b765$Oocyst==0,0,1)

atv50<-read.table("H:\\Ellie\\Synergy TBI and PEV\\ALL DATA RE_ARRANGED_20062015\\Figures for Andrew 11072017\\slow_mosquito.txt",header=TRUE)
atv50$OocPrev<-ifelse(atv50$Oocyst==0,0,1)
atv50b<-read.table("H:\\Ellie\\Synergy TBI and PEV\\ALL DATA RE_ARRANGED_20062015\\Figures for Andrew 11072017\\slow_mouse.txt",header=TRUE)
atv50b$bloodstage<-ifelse(atv50b$Parasitemia==0,0,1)

##############################################
##############################################
##oocysts_graph(data,nrnds,nbites,rounds,comb)
##bloodstage_graph(data,nrnds,nbites,rounds,comb)
##spors_f(data,comb,nrowsdata,
                     ##   num_sp, ##This is the 1st column containing the sporozoite data
                     ##   num_sp_9, ##This is the last column containing the sporozoite data
                     ##   seque)
##spor_score <- function(spors,Rounds,nrnds,nbites,comb)
        
ooc_4B7 <- oocysts_graph(atv50,nrnds=4,nbites=4,rounds=rep(c(1,2,3,4),4),16)
para_4B7 <- bloodstage_graph(atv50b, nrnds=4,nbites=4,rounds=rep(c(1,2,3,4),4),16)
sporprev_4B7 <- spors_f(atv50b, 16, nrow(atv50b), 6,15, seque <- seq(1,76,5))
spor_int_4B7 <- spor_score(atv50b, Rounds=c(0,1,2,3),4,4,16)



##########################
#########
#### Figures for Andrew


is.mosi<-0
n.rounds=4
n.bites=4
t.bites<-c(1,2,5,10)


par(mfcol=c(4,5),mar= c(3, 4, 1, 2))

is.mosi<-0

##Controls: mouse read.csv("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\sporozoites.csv",header=T)
##Controls: mosquito read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Blagborough data Nat Comms\\oocysts.txt",header=T)

##t4b7_65%
##Bloodstage
prevH.round.1<-t(matrix(ncol=4,nrow=4,para_4B7[[1]]))
prevH.lower.1<-t(matrix(ncol=4,nrow=4,
                      c(para_4B7[[2]][1:4,1],
                      para_4B7[[2]][5:8,1],
                      para_4B7[[2]][9:12,1],
                      para_4B7[[2]][13:16,1])))
prevH.upper.1<-t(matrix(ncol=4,nrow=4,
                      c(para_4B7[[2]][1:4,2],
                        para_4B7[[2]][5:8,2],
                        para_4B7[[2]][9:12,2],
                        para_4B7[[2]][13:16,2])))

for(mb in 1:4){
  
  hh=rbind(prevH.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevH.lower.1[mb,])
  ciU<-rbind(prevH.upper.1[mb,])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100, 
                col = c("dodgerblue4"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100 , col = mybarcol, lwd = 1.5)
  
}

##Oocyst prevalence
prevMO.round.1<-t(ooc_4B7[[3]][1:4,1:4])/100
prevMO.lower.1<-t(matrix(ncol=4,nrow=4,
                         c(ooc_4B7[[4]][1:4,1],
                           ooc_4B7[[4]][5:8,1],
                           ooc_4B7[[4]][9:12,1],
                           ooc_4B7[[4]][13:16,1])))/100
prevMO.upper.1<-t(matrix(ncol=4,nrow=4,
                         c(ooc_4B7[[4]][1:4,2],
                           ooc_4B7[[4]][5:8,2],
                           ooc_4B7[[4]][9:12,2],
                           ooc_4B7[[4]][13:16,2])))/100

for(mb in 1:4){
  
  hh=rbind(prevMO.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMO.lower.1[mb,])
  ciU<-rbind(prevMO.upper.1[mb,])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100, 
                col = c("forestgreen"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100 , col = mybarcol, lwd = 1.5)
  
}
##Oocyst intensity
prevMOI.round.1<-t(ooc_4B7[[1]][1:4,1:4])
bootMOI.lower1<-t(matrix(ncol=4,nrow=4,
                         c(ooc_4B7[[2]][1:4,1],
                           ooc_4B7[[2]][5:8,1],
                           ooc_4B7[[2]][9:12,1],
                           ooc_4B7[[2]][13:16,1])))
bootMOI.upper1<-t(matrix(ncol=4,nrow=4,
                         c(ooc_4B7[[2]][1:4,2],
                           ooc_4B7[[2]][5:8,2],
                           ooc_4B7[[2]][9:12,2],
                           ooc_4B7[[2]][13:16,2])))

for(mb in 1:4){
  
  hh=rbind(prevMOI.round.1[mb,])
  hh.max<-apply(hh, 2, max)+5
  ciL<-rbind(bootMOI.lower1[mb,])
  ciU<-rbind(bootMOI.upper1[mb,])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  
  mybarcol <- "gray20"
  mp <- barplot(hh,
                col = c("firebrick3"),
                ylim= c(0,45),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  #text(colMeans(mp)+0.2,hh.max,  labels =less.than.001[mb,], col = "red")
  
  #mtext(side = 1, at = colMeans(mp), line = 2,
  #      text = paste("efficacy", formatC(colMeans(hh))), col = "red")
  
}

##Sporozoite prevalence

sporprev_4B7

prevMOS.round.1<-matrix(ncol=4,nrow=4,sporprev_4B7[[2]][1:16])
prevMOS.lower.1<-t(matrix(ncol=4,nrow=4,
                          c(sporprev_4B7[[3]][1,1],sporprev_4B7[[3]][5,1],sporprev_4B7[[3]][9,1],sporprev_4B7[[3]][13,1],
                            sporprev_4B7[[3]][2,1],sporprev_4B7[[3]][6,1],sporprev_4B7[[3]][10,1],sporprev_4B7[[3]][14,1],
                            sporprev_4B7[[3]][3,1],sporprev_4B7[[3]][7,1],sporprev_4B7[[3]][11,1],sporprev_4B7[[3]][15,1],
                            sporprev_4B7[[3]][4,1],sporprev_4B7[[3]][8,1],sporprev_4B7[[3]][12,1],sporprev_4B7[[3]][16,1])))
prevMOS.upper.1<-t(matrix(ncol=4,nrow=4,
                          c(sporprev_4B7[[3]][1,2],sporprev_4B7[[3]][5,2],sporprev_4B7[[3]][9,2],sporprev_4B7[[3]][13,2],
                            sporprev_4B7[[3]][2,2],sporprev_4B7[[3]][6,2],sporprev_4B7[[3]][10,2],sporprev_4B7[[3]][14,2],
                            sporprev_4B7[[3]][3,2],sporprev_4B7[[3]][7,2],sporprev_4B7[[3]][11,2],sporprev_4B7[[3]][15,2],
                            sporprev_4B7[[3]][4,2],sporprev_4B7[[3]][8,2],sporprev_4B7[[3]][12,2],sporprev_4B7[[3]][16,2])))
for(mb in 1:4){
  
  hh=rbind(prevMOS.round.1[mb,])
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(prevMOS.lower.1[mb,])
  ciU<-rbind(prevMOS.upper.1[mb,])
  colnames(hh)<-seq(1,4,1)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh*100, 
                col = c("orange"),
                ylim= c(0,100),
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.4)
  segments(mp, ciL*100, mp, ciU*100 , col = mybarcol, lwd = 1.5)
  
}
##Sporozoite intensity

#spor_int_4B7

MOSI.round.1<-t(matrix(ncol=4,nrow=4,spor_int_4B7[[1]][1:4,1:4]))
bootMOSI.lower1<-t(matrix(ncol=4,nrow=4,
                          c(spor_int_4B7[[2]][1:4,1],
                            spor_int_4B7[[2]][5:8,1],
                            spor_int_4B7[[2]][9:12,1],
                            spor_int_4B7[[2]][13:16,1])))
bootMOSI.upper1<-t(matrix(ncol=4,nrow=4,
                          c(spor_int_4B7[[2]][1:4,2],
                            spor_int_4B7[[2]][5:8,2],
                            spor_int_4B7[[2]][9:12,2],
                            spor_int_4B7[[2]][13:16,2])))

for(nb in 1:4){
  
  colnames(MOSI.round.1)<-c(1,2,3,4)
  mp <- barplot(MOSI.round.1[nb,],ylim=c(0,2.5),col = "mediumpurple",cex.names = 1.4)
  
  par(las=1,col.axis="black")
  
  segments(mp, bootMOSI.lower1[nb,], mp, bootMOSI.upper1[nb,] , col = mybarcol, lwd = 1.5)
  
}
