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

b7_85<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\4B7-85\\mosquito.txt",header=TRUE)
b7_85$OocPrev<-ifelse(b7_85$Oocyst==0,0,1)
b7_85$Round<-as.factor(b7_85$Round)
b7_85$Bites<-as.factor(b7_85$Bites)

summary(b7_85)

oocystsT4bv<-c(b7_85$Oocyst[b7_85$Round == "1" & b7_85$Bites == "1"],
               b7_85$Oocyst[b7_85$Round == "1" & b7_85$Bites == "2"],
               b7_85$Oocyst[b7_85$Round == "1" & b7_85$Bites == "5"],
               b7_85$Oocyst[b7_85$Round == "1" & b7_85$Bites == "10"])
length(oocystsT4bv)
prevoocT4bv<-ifelse(oocystsT4bv==0,0,1)
prevoocT4bv<-c(sum(sum(prevoocT4bv[1:50])/50,sum(prevoocT4bv[51:100])/50,
                   sum(prevoocT4bv[101:150])/50,sum(prevoocT4bv[151:200])/50)/4)
EffOoc<-(prevooc1-prevoocT4bv)/prevooc1

meanoocysts4bv<-c(
  mean(b7_85$Oocyst[b7_85$Round=="1" & b7_85$Bites == "1"]),
  mean(b7_85$Oocyst[b7_85$Round=="1" & b7_85$Bites == "2"]),
  mean(b7_85$Oocyst[b7_85$Round=="1" & b7_85$Bites == "5"]),
  mean( b7_85$Oocyst[b7_85$Round=="1" & b7_85$Bites == "10"])
  )



b7_85b<-read.table("C:\\Users\\Ellie\\Dropbox\\Malaria\\Data Malaria\\Blagborough data Nat Comms\\grant data\\Andrew Blagborough\\4B7-85\\mouse.txt",header=TRUE)
b7_85b$bloodstage<-ifelse(b7_85b$Parasitemia==0,0,1)
summary(b7_85b)

##MEAN PARASITEMIA IN MICE
mean(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5])

parasit_4B7<-cbind(
  b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 1],
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 1])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 1])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 1])),
  b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 2],
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 2])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 2])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 2])),
  b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5],
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5])),
  b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 10],
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 10])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 10])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 10])))
parasitem_4B7<-parasitemUPP_4B7<-parasitemLOW_4B7<-numeric(16)
for (i in 1:16){
  parasitem_4B7[i]<-sum(parasit_4B7[,i])/5
}

##MEAN Gametocytemia IN MICE
gamet_4B7<-cbind(
  b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 1],
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 1])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 1])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 1])),
  b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 2],
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 2])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 2])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 2])),
  b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5],
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 5])),
  b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 10],
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 10])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 10])),
  rep(0,length(b7_85b$Parasitemia[b7_85b$Treatment == "4B785" & b7_85b$Bites == 10])))
gametoT_4B7<-numeric(16)
for (i in 1:16){
  gametoT_4B7[i]<-sum(gamet_4B7[,i])/5
}

##PREVALENCE IN MICE
b7_85b$prevBS<-ifelse(b7_85b$Parasitemia == 0,0,1)
PREVT_4B7<-cbind(
  #  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 1],
  #  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 2],
  #  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 3],
  #  spors$prevBS[spors$Bites==1 & spors$Treatment == 1 & spors$Round == 4],
  b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1],
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  b7_85b$prevBS[b7_85b$Bites==2 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1],
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  b7_85b$prevBS[b7_85b$Bites==5 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1],
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  b7_85b$prevBS[b7_85b$Bites==10 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1],
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])),
  rep(0,length(b7_85b$prevBS[b7_85b$Bites==1 & b7_85b$Treatment == "4B785" & b7_85b$Round == 1])))

###sPOROZOITES

###Sporozoites mean and CI95%
##create prevalence
b7_85b$sp1<-ifelse(b7_85b$Sporozoite1==0,0,1)
b7_85b$sp2<-ifelse(b7_85b$Sporozoite2==0,0,1)
b7_85b$sp3<-ifelse(b7_85b$Sporozoite3==0,0,1)
b7_85b$sp4<-ifelse(b7_85b$Sporozoite4==0,0,1)
b7_85b$sp5<-ifelse(b7_85b$Sporozoite5==0,0,1)
b7_85b$sp6<-ifelse(b7_85b$Sporozoite6==0,0,1)
b7_85b$sp7<-ifelse(b7_85b$Sporozoite7==0,0,1)
b7_85b$sp8<-ifelse(b7_85b$Sporozoite8==0,0,1)
b7_85b$sp9<-ifelse(b7_85b$Sporozoite9==0,0,1)
b7_85b$sp10<-ifelse(b7_85b$Sporozoite10==0,0,1)


sp1a<-b7_85b$sp1[b7_85b$Round==1 & b7_85b$Bites==1]

sp2a<-c(b7_85b$sp1[b7_85b$Round==1 & b7_85b$Bites==2],b7_85b$sp2[b7_85b$Round==1 & b7_85b$Bites==2])

sp5a<-c(b7_85b$sp1[b7_85b$Round==1 & b7_85b$Bites==5],b7_85b$sp2[b7_85b$Round==1 & b7_85b$Bites==5],
        b7_85b$sp3[b7_85b$Round==1 & b7_85b$Bites==5],b7_85b$sp4[b7_85b$Round==1 & b7_85b$Bites==5],
        b7_85b$sp5[b7_85b$Round==1 & b7_85b$Bites==5])

sp10a<-c(b7_85b$sp1[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp2[b7_85b$Round==1 & b7_85b$Bites==10],
         b7_85b$sp3[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp4[b7_85b$Round==1 & b7_85b$Bites==10],
         b7_85b$sp5[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp6[b7_85b$Round==1 & b7_85b$Bites==10],
         b7_85b$sp7[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp8[b7_85b$Round==1 & b7_85b$Bites==10],
         b7_85b$sp9[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$sp10[b7_85b$Round==1 & b7_85b$Bites==10])

###Sporozoite intensity
Sporozoite1a<-b7_85b$Sporozoite1[b7_85b$Round==1 & b7_85b$Bites==1]

Sporozoite2a<-c(b7_85b$Sporozoite1[b7_85b$Round==1 & b7_85b$Bites==2],b7_85b$Sporozoite2[b7_85b$Round==1 & b7_85b$Bites==2])

Sporozoite5a<-c(b7_85b$Sporozoite1[b7_85b$Round==1 & b7_85b$Bites==5],b7_85b$Sporozoite2[b7_85b$Round==1 & b7_85b$Bites==5],
                b7_85b$Sporozoite3[b7_85b$Round==1 & b7_85b$Bites==5],b7_85b$Sporozoite4[b7_85b$Round==1 & b7_85b$Bites==5],
                b7_85b$Sporozoite5[b7_85b$Round==1 & b7_85b$Bites==5])

Sporozoite10a<-c(b7_85b$Sporozoite1[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite2[b7_85b$Round==1 & b7_85b$Bites==10],
                 b7_85b$Sporozoite3[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite4[b7_85b$Round==1 & b7_85b$Bites==10],
                 b7_85b$Sporozoite5[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite6[b7_85b$Round==1 & b7_85b$Bites==10],
                 b7_85b$Sporozoite7[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite8[b7_85b$Round==1 & b7_85b$Bites==10],
                 b7_85b$Sporozoite9[b7_85b$Round==1 & b7_85b$Bites==10],b7_85b$Sporozoite10[b7_85b$Round==1 & b7_85b$Bites==10])

sporsbites14b7<-subset(b7_85b,Bites==1 & Treatment=="4B785");sporsbites14b7
new_a4b7<-sporsb1rd14b7<-c(sporsbites14b7$Sporozoite1[sporsbites1$Round==1]);
spb14b7<-c(length(sporsb1rd14b7[sporsb1rd14b7==0]),length(sporsb1rd14b7[sporsb1rd14b7==1]),
             length(sporsb1rd14b7[sporsb1rd14b7==2]),length(sporsb1rd14b7[sporsb1rd14b7==3]),
             length(sporsb1rd14b7[sporsb1rd14b7==4]))


sporsbites24b7<-subset(b7_85b,Bites==2 & Treatment=="4B785");sporsbites24b7
ab7_85b<- sporsb2rd14b7<-c(sporsbites24b7$Sporozoite1[sporsbites24b7$Round==1],
                              sporsbites24b7$Sporozoite2[sporsbites24b7$Round==1]);
sporsb2rd14b7<-c(sporsb2rd14b7[1:7],0,sporsb2rd14b7[9:10])
spb24b7<-c(length(sporsb2rd14b7[sporsb2rd14b7==0]),length(sporsb2rd14b7[sporsb2rd14b7==1]),
           length(sporsb2rd14b7[sporsb2rd14b7==2]),length(sporsb2rd14b7[sporsb2rd14b7==3]),
           length(sporsb2rd14b7[sporsb2rd14b7==4]))

sporsbites54b7<-subset(b7_85b,Bites==5 & Treatment=="4B785");sporsbites54b7
ee4b7<- sporsb5rd14b7<-c(sporsbites54b7$Sporozoite1[sporsbites54b7$Round==1],
                         sporsbites54b7$Sporozoite2[sporsbites54b7$Round==1],
                         sporsbites54b7$Sporozoite3[sporsbites54b7$Round==1],
                         sporsbites54b7$Sporozoite4[sporsbites54b7$Round==1],
                         sporsbites54b7$Sporozoite5[sporsbites54b7$Round==1]);
spb54b7<-c(length(sporsb5rd14b7[sporsb5rd14b7==0]),length(sporsb5rd14b7[sporsb5rd14b7==1]),
           length(sporsb5rd14b7[sporsb5rd14b7==2]),length(sporsb5rd14b7[sporsb5rd14b7==3]),
           length(sporsb5rd14b7[sporsb5rd14b7==4]))

sporsbites104b7<-subset(b7_85b,Bites==10 & Treatment=="4B785");sporsbites104b7
jj4b7<- sporsb10rd14b7<-c(sporsbites104b7$Sporozoite1[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite2[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite3[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite4[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite5[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite6[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite7[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite8[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite9[sporsbites104b7$Round==1],
                       sporsbites104b7$Sporozoite10[sporsbites104b7$Round==1]);
spb104b7<-c(length(sporsb10rd14b7[sporsb10rd14b7==0]),length(sporsb10rd14b7[sporsb10rd14b7==1]),
           length(sporsb10rd14b7[sporsb10rd14b7==2]),length(sporsb10rd14b7[sporsb10rd14b7==3]),
           length(sporsb10rd14b7[sporsb10rd14b7==4]))

spors_T4b7<-rbind(#spb1r1,spb1r2,spb1r3,spb1r4,
  spb14b7,rep(0,5),rep(0,5),rep(0,5),
  spb24b7,rep(0,5),rep(0,5),rep(0,5),
  spb54b7,rep(0,5),rep(0,5),rep(0,5),
  spb104b7,rep(0,5),rep(0,5),rep(0,5))
spors_T4b7prev<-numeric(16)
for(i in 1:16){
  spors_T4b7prev[i]<-1-(spors_T4b7[i,1]/sum(spors_T4b7[i,]))
  spors_T4b7prev[is.na(spors_T4b7prev)] <- 0
}

#########
effectSpors4b7<-(spors_Cprev-spors_T4b7prev)/spors_Cprev
EFFSPmbr4b7<-c(sum(effectSpors4b7[1:4])/4,
            sum(effectSpors4b7[5:8])/4,
            sum(effectSpors4b7[9:12])/4,
            sum(effectSpors4b7[13:16])/4)


MEANsp4b7<-c(mean(new_a4b7),0,0,0,
             mean(ab7_85b,na.rm=T),0,0,0,
             mean(ee4b7),0,0,0,
             mean(jj4b7),0,0,0)

prevcon<-prevtreat4B7<-numeric(16)
for (i in 1:16){
  prevcon[i]<-c(sum(PREV_C[,i])/5)
  prevtreat4B7[i]<-c(sum(PREVT_4B7[,i])/5)
}

prevs4B7<-c(prevcon,prevtreat4B7)
prevmbrC<-c(sum(prevcon[1:4])/4,
            sum(prevcon[5:8])/4,
            sum(prevcon[9:12])/4,
            sum(prevcon[13:16])/4)
prevmbrT4B7<-c(sum(prevtreat4B7[1:4])/4,
            sum(prevtreat4B7[5:8])/4,
            sum(prevtreat4B7[9:12])/4,
            sum(prevtreat4B7[13:16])/4)
meanprevpergroup4B7<-(prevcon+prevtreat4B7)/2