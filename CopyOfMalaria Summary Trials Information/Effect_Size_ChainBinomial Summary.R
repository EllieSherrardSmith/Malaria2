#####################################################################################################
##                                                                                                 ##                       
### Fitted chain binomial -                                                                        ##  
##                                                                                                 ##  
#####################################################################################################


## Testing whether there is a significant difference between the 
## probability estimates for mosquito to mouse and mouse to mosquito
## transmission.

## Take the estimates from the 95% CI for each study and
## compare using ANOVA

data_r<-c(rEstimatesATV25,rEstimatesATV50,rEstimatesATV65,rEstimatesATV85,
          rEstimates4B785,rEstimates3d1150,rEstimates3d1150andATV85,rEstimates3d1150and4B785)
data_s<-c(sEstimatesATV25,sEstimatesATV50,sEstimatesATV65,sEstimatesATV85,
          sEstimates4B785,sEstimates3d1150,sEstimates3d1150andATV85,sEstimates3d1150and4B785)

data_treatment<-c(rep("ATV 25%",11),rep("ATV 50%",16),rep("ATV 65%",42),rep("ATV 85%",47),
                  rep("4B7 85%",53),rep("3D11 50%",24),rep("3D11 50% & ATV 85%",47),rep("3D11 50% & 4B7 85%",36))
tested<-data.frame(data_r,data_s,data_treatment)

par(mfrow=c(2,1))
par(mar=c(2,5,1,1))

mb<-tapply(tested$data_r,tested$data_treatment,mean)
mb2<-c(mb[5],mb[6],mb[7],mb[8],mb[4],mb[1],mb[3],mb[2])
mbL<-c(quantile(rEstimatesATV25,0.025),quantile(rEstimatesATV50,0.025),quantile(rEstimatesATV65,0.025),
       quantile(rEstimatesATV85,0.025),quantile(rEstimates4B785,0.025),quantile(rEstimates3d1150,0.025),
       quantile(rEstimates3d1150andATV85,0.025),quantile(rEstimates3d1150and4B785,0.025)) 
mbU<-c(quantile(rEstimatesATV25,0.975),quantile(rEstimatesATV50,0.975),quantile(rEstimatesATV65,0.975),
         quantile(rEstimatesATV85,0.975),quantile(rEstimates4B785,0.975),quantile(rEstimates3d1150,0.975),
         quantile(rEstimates3d1150andATV85,0.975),quantile(rEstimates3d1150and4B785,0.975))
  hh=rbind(mb2)
  hh.max<-apply(hh, 2, max)+6
  ciL<-rbind(mbL)
  ciU<-rbind(mbU)
  par(las=1,col.axis="black")
  
  mybarcol <- "gray20"
  mp <- barplot(hh, 
                col = c("dodgerblue4"),
                ylim= c(0,1),
                ylab= "mosquito to mouse transmission",cex.lab=1.4,
                #        main = "Parasetemia", font.main = 4,
                #        sub = "Transmission cycle", col.sub = mybarcol,
                cex.names = 1.2)
  segments(mp, ciL, mp, ciU , col = mybarcol, lwd = 1.5)
  
abline(h=mean(tested$data_r),lty=2)

sb<-tapply(tested$data_s,tested$data_treatment,mean)
sb2<-c(sb[5],sb[6],sb[7],sb[8],sb[4],sb[1],sb[3],sb[2])
sbL<-c(quantile(sEstimatesATV25,0.025),quantile(sEstimatesATV50,0.025),quantile(sEstimatesATV65,0.025),
       quantile(sEstimatesATV85,0.025),quantile(sEstimates4B785,0.025),quantile(sEstimates3d1150,0.025),
       quantile(sEstimates3d1150andATV85,0.025),quantile(sEstimates3d1150and4B785,0.025)) 
sbU<-c(quantile(sEstimatesATV25,0.975),quantile(sEstimatesATV50,0.975),quantile(sEstimatesATV65,0.975),
       quantile(sEstimatesATV85,0.975),quantile(sEstimates4B785,0.975),quantile(sEstimates3d1150,0.975),
       quantile(sEstimates3d1150andATV85,0.975),quantile(sEstimates3d1150and4B785,0.975))
hh2=rbind(sb2)
hh.max<-apply(hh2, 2, max)+6
ciL2<-rbind(sbL)
ciU2<-rbind(sbU)
par(las=1,col.axis="black")

mybarcol <- "gray20"
mp2 <- barplot(hh2, 
              col = c("dodgerblue4"),
              ylim= c(0,1),
              ylab= "Probability of mouse to mosquito transmission",cex.lab=1.4,
              #        main = "Parasetemia", font.main = 4,
              #        sub = "Transmission cycle", col.sub = mybarcol,
              cex.names = 1.2)
segments(mp2, ciL2, mp2, ciU2 , col = mybarcol, lwd = 1.5)

abline(h=mean(tested$data_s),lty=2)


hist(tested$data_r);hist(tested$data_s)
mod1 <- aov(data_r ~ data_treatment, data = tested )
mod1a <- lmer(data_r ~ (1|data_treatment), data = tested )
summary(mod1a);summary(mod1);summary.lm(mod1)

tested2<-subset(tested,data_treatment!="ATV 25%")
mod1 <- aov(data_r ~ data_treatment, data = tested2 )
summary(mod1);summary.lm(mod1)

tested3<-subset(tested2,data_treatment!="ATV 50%")
mod1 <- aov(data_r ~ data_treatment, data = tested3 )
summary(mod1);summary.lm(mod1)
mean(tested3$data_r)

mod2 <- aov(data_s ~ data_treatment, data = tested )
summary(mod2);summary.lm(mod2)
tested4<-subset(tested,data_treatment!="ATV 65%")
mod2 <- aov(data_s ~ data_treatment, data = tested4 )
summary(mod2);summary.lm(mod2)

######################################################
##
## Re-estimate effect size with the fixed estimates for r and s
##
##

v.binom<-function(p.vec){
  
  v<-p.vec[1]
  r<-0.4976526
  s<-0.7065391
  
  # Round, Bites, Treatment, Species; predicted (except for p0) number infected ##3 rounds
  inf<-array(NA, dim = c(3,4,2,2))
  
  # Total numbers of mice and mosquitoes
  number<-array(NA, dim = c(3,4,2,2))
  
  # Enter p0 data
  inf[1,,1,2]<-10
  inf[1,4,1,2]<-5
  inf[1,,2,2]<-5
  number[1:3,,1,2]<-10
  number[1:3,4,1,2]<-5
  number[3,4,1,2]<-0
  number[1:3,,2,2]<-5
  
  # Predict q0
  number[1,1:4,1,1] <- c(95,82,95,50)
  
  inf[1,1:4,1,1] <- s * c(95,82,95,50)
  
  number[1,1:4,2,1] <- 50
  inf[1,1:4,2,1] <- s * (1-v) * 50
  
  mbr.vec<-c(1,2,5,10)
  # Predcit (p1-p5 and q1-q5), loops for rounds and bites 
  for(a in 1:2){
    for(b in 1:4){
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment==0)
      number[a+1,b,1,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,1,2] <- (1 - (1 - r * (inf[a,b,1,1]/number[a,b,1,1])) ^ mbr.vec[b]) * number[a+1,b,1,2]                # mosquito to mouse
      inf[a+1,b,1,1] <- ((inf[a+1,b,1,2]/number[a+1,b,1,2]) * s) * number[a+1,b,1,1]                          # mouse to mosquito
      
      mosqu.temp<-subset(data.mosqu,data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment==1)
      number[a+1,b,2,1] <- length(mosqu.temp$Oocyst)
      
      inf[a+1,b,2,2] <- (1 - (1 - r * (inf[a,b,2,1]/number[a,b,2,1]) ) ^ mbr.vec[b]) * number[a+1,b,2,2]               # mosquito to mouse
      inf[a+1,b,2,1] <- ((inf[a+1,b,2,2]/number[a+1,b,2,2]) * s) * (1-v) * number[a+1,b,2,1]                  # mouse to mosquito
    }
  }
  
  # Arrange data into appropriate format for fitting
  data.inf<-array(NA, dim = c(3,4,2,2))    
  data.inf[1,1:4,1,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==0]>0)
  data.inf[1,1:4,2,1] <- sum(data.mosqu$Oocyst[data.mosqu$Bites==0 & data.mosqu$Round==0 & data.mosqu$Treatment==1]>0)
  for(a in 1:2){
    for(b in 1:4){
      data.inf[a+1,b,1,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment==0]>0)
      data.inf[a+1,b,1,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment==0]>0)
      
      data.inf[a+1,b,2,2]<-sum(data.mouse$Parasitemia[data.mouse$Bites==mbr.vec[b] & data.mouse$Round==a & data.mouse$Treatment==1]>0)
      data.inf[a+1,b,2,1]<-sum(data.mosqu$Oocyst[data.mosqu$Bites==mbr.vec[b] & data.mosqu$Round==a & data.mosqu$Treatment==1]>0)
    }
  }
  
  # inclusion/exclusion of q0 data
  #inf[1,2:5,,1]<-NA  
  inf[1,,,]<-NA
  
  loglik<- data.inf * log((inf/number))+(number-data.inf)*log((1-(inf/number)))
  -sum(loglik,na.rm=T)  
}


##############################################
##
##  Compare the controls from the orginal study to the new ones (June 2015)
##
##

con<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mosquito.txt",header=TRUE)
con$OocPrev<-ifelse(con$Oocyst==0,0,1)

##Is there a difference between Controls from NatComms to NEW?
con$fBites<-as.factor(con$Bites)
con1<-subset(con,Bites==1)
con2<-subset(con,Bites==2)
con5<-subset(con,Bites==5)

#modcomp<-lme(Oocyst~Bites, random = ~1|DATASOURCE, method="REML",data = con)
#summary(modcomp)

modcomp1<-glm.nb(Oocyst~DATASOURCE, link = "log", data = con1)
summary(modcomp1,cor=FALSE)
(modcomp1$null.deviance - modcomp1$deviance)/modcomp1$null.deviance
drop1(modcomp1,test="Chi")

modcomp2<-glm.nb(Oocyst~DATASOURCE, link = "log", data = con2)
summary(modcomp2,cor=FALSE)
(modcomp2$null.deviance - modcomp2$deviance)/modcomp2$null.deviance
drop1(modcomp2,test="Chi")

modcomp5<-glm.nb(Oocyst~DATASOURCE, link = "log", data = con5)
summary(modcomp5,cor=FALSE)
(modcomp5$null.deviance - modcomp5$deviance)/modcomp5$null.deviance
drop1(modcomp5,test="Chi")

summary.lm(glm(OocPrev~DATASOURCE,data=con1))
summary.lm(glm(OocPrev~DATASOURCE,data=con2))
summary.lm(glm(OocPrev~DATASOURCE,data=con5))

conb<-read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\ALL DATA RE_ARRANGED_20062015\\controls\\mouse.txt",header=TRUE)
conb$bloodstage<-ifelse(conb$Parasitemia  == 0,0,1)
summary(conb)
conbcomp<-subset(conb,Bites==1|Bites==2|Bites==5)
conbcomp$fBites<-as.factor(conbcomp$Bites)

conb1<-subset(conb,Bites==1)
conb2<-subset(conb,Bites==2)
conb5<-subset(conb,Bites==5)

summary.lm(glm(Parasitemia~DATASOURCE,data=conb1))
summary.lm(glm(Parasitemia~DATASOURCE,data=conb2))
summary.lm(glm(Parasitemia~DATASOURCE,data=conb5))

summary.lm(glm(Gametocytemia~DATASOURCE,data=conb1))
summary.lm(glm(Gametocytemia~DATASOURCE,data=conb2))
summary.lm(glm(Gametocytemia~DATASOURCE,data=conb5))

summary.lm(glm(bloodstage~DATASOURCE,data=conb1))
summary.lm(glm(bloodstage~DATASOURCE,data=conb2))
summary.lm(glm(bloodstage~DATASOURCE,data=conb5))


image1 <- ggplot(conbcomp, aes(x=fBites, y=Parasitemia, fill=DATASOURCE)) +
geom_boxplot() + ggtitle("Controls") + theme_bw()+ guides(fill=FALSE) + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),text = element_text(size=25),
        axis.line = element_line(colour = "black"))


image2 <- ggplot(conbcomp, aes(x=fBites, y=Gametocytemia, fill=DATASOURCE)) +
  geom_boxplot() + ggtitle("Controls") + theme_bw()+ guides(fill=FALSE) + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),text = element_text(size=25),
        axis.line = element_line(colour = "black"))

require(gridExtra)
grid.arrange(image1,image2, ncol=2)
tapply(conbcomp$Gametocytemia,conbcomp$DATASOURCE,summary)
