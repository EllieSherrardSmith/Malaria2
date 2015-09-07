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
#data<-read.csv("D:\\ModelATV_Prev output.csv")
names(data) 
data<-data[501:1000,]##dropping burn in so keeping the appropriate data

par(mfrow=c(1,3));hist(data[,61],breaks=20,main="Simulated oocysts (Con, 1 Bites, Round 1)",
                       xlab="",ylab="Frequency",cex.lab=2,cex=2,col="aquamarine")
                  hist(data[,66],breaks=20,main="Simulated oocysts (Con, 3 Bites, Round 3)",
                       xlab="Oocyst counts per mosquito",ylab="",cex.lab=2,col="aquamarine")
                  hist(data[,88],breaks=20,main="Simulated oocysts (Treat, 4 Bites, Round 2)",
                       xlab="",ylab="",cex.lab=2,col="aquamarine")

###########################
##
##
## Sporozoite data
##
par(mfrow=c(2,2))
boxplot(data$"sim_s_count_C.1.1",data$"sim_s_count_T.1.1",
        data$"sim_s_count_C.1.2",data$"sim_s_count_T.1.2",
        data$"sim_s_count_C.1.3",data$"sim_s_count_T.1.3",
        data$"sim_s_count_C.1.4",data$"sim_s_count_T.1.4",
        data$"sim_s_count_C.1.5",data$"sim_s_count_T.1.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 2, Round 1",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[1,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[1,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.2.1",data$"sim_s_count_T.2.1",
        data$"sim_s_count_C.2.2",data$"sim_s_count_T.2.2",
        data$"sim_s_count_C.2.3",data$"sim_s_count_T.2.3",
        data$"sim_s_count_C.2.4",data$"sim_s_count_T.2.4",
        data$"sim_s_count_C.2.5",data$"sim_s_count_T.2.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 2, Round 2",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[2,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[2,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.3.1",data$"sim_s_count_T.3.1",
        data$"sim_s_count_C.3.2",data$"sim_s_count_T.3.2",
        data$"sim_s_count_C.3.3",data$"sim_s_count_T.3.3",
        data$"sim_s_count_C.3.4",data$"sim_s_count_T.3.4",
        data$"sim_s_count_C.3.5",data$"sim_s_count_T.3.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 2, Round 3",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex=2)
points(spors_C[3,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[3,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.4.1",data$"sim_s_count_T.4.1",
        data$"sim_s_count_C.4.2",data$"sim_s_count_T.4.2",
        data$"sim_s_count_C.4.3",data$"sim_s_count_T.4.3",
        data$"sim_s_count_C.4.4",data$"sim_s_count_T.4.4",
        data$"sim_s_count_C.4.5",data$"sim_s_count_T.4.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 2, Round 4",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[4,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[4,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

par(mfrow=c(2,2))
boxplot(data$"sim_s_count_C.5.1",data$"sim_s_count_T.5.1",
        data$"sim_s_count_C.5.2",data$"sim_s_count_T.5.2",
        data$"sim_s_count_C.5.3",data$"sim_s_count_T.5.3",
        data$"sim_s_count_C.5.4",data$"sim_s_count_T.5.4",
        data$"sim_s_count_C.5.5",data$"sim_s_count_T.5.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 3, Round 1",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[5,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[5,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.6.1",data$"sim_s_count_T.6.1",
        data$"sim_s_count_C.6.2",data$"sim_s_count_T.6.2",
        data$"sim_s_count_C.6.3",data$"sim_s_count_T.6.3",
        data$"sim_s_count_C.6.4",data$"sim_s_count_T.6.4",
        data$"sim_s_count_C.6.5",data$"sim_s_count_T.6.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 3, Round 2",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[6,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[6,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.7.1",data$"sim_s_count_T.7.1",
        data$"sim_s_count_C.7.2",data$"sim_s_count_T.7.2",
        data$"sim_s_count_C.7.3",data$"sim_s_count_T.7.3",
        data$"sim_s_count_C.7.4",data$"sim_s_count_T.7.4",
        data$"sim_s_count_C.7.5",data$"sim_s_count_T.7.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 3, Round 3",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex=2)
points(spors_C[7,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[7,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.8.1",data$"sim_s_count_T.8.1",
        data$"sim_s_count_C.8.2",data$"sim_s_count_T.8.2",
        data$"sim_s_count_C.8.3",data$"sim_s_count_T.8.3",
        data$"sim_s_count_C.8.4",data$"sim_s_count_T.8.4",
        data$"sim_s_count_C.8.5",data$"sim_s_count_T.8.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 3, Round 4",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[8,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[8,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

par(mfrow=c(2,2))
boxplot(data$"sim_s_count_C.9.1",data$"sim_s_count_T.9.1",
        data$"sim_s_count_C.9.2",data$"sim_s_count_T.9.2",
        data$"sim_s_count_C.9.3",data$"sim_s_count_T.9.3",
        data$"sim_s_count_C.9.4",data$"sim_s_count_T.9.4",
        data$"sim_s_count_C.9.5",data$"sim_s_count_T.9.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 4, Round 1",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[9,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[9,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.10.1",data$"sim_s_count_T.10.1",
        data$"sim_s_count_C.10.2",data$"sim_s_count_T.10.2",
        data$"sim_s_count_C.10.3",data$"sim_s_count_T.10.3",
        data$"sim_s_count_C.10.4",data$"sim_s_count_T.10.4",
        data$"sim_s_count_C.10.5",data$"sim_s_count_T.10.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 4, Round 2",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[10,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[10,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.11.1",data$"sim_s_count_T.11.1",
        data$"sim_s_count_C.11.2",data$"sim_s_count_T.11.2",
        data$"sim_s_count_C.11.3",data$"sim_s_count_T.11.3",
        data$"sim_s_count_C.11.4",data$"sim_s_count_T.11.4",
        data$"sim_s_count_C.11.5",data$"sim_s_count_T.11.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 4, Round 3",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex=2)
points(spors_C[11,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[11,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.12.1",data$"sim_s_count_T.12.1",
        data$"sim_s_count_C.12.2",data$"sim_s_count_T.12.2",
        data$"sim_s_count_C.12.3",data$"sim_s_count_T.12.3",
        data$"sim_s_count_C.12.4",data$"sim_s_count_T.12.4",
        data$"sim_s_count_C.12.5",data$"sim_s_count_T.12.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 4, Round 4",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[12,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[12,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

par(mfrow=c(2,2))
boxplot(data$"sim_s_count_C.13.1",data$"sim_s_count_T.13.1",
        data$"sim_s_count_C.13.2",data$"sim_s_count_T.13.2",
        data$"sim_s_count_C.13.3",data$"sim_s_count_T.13.3",
        data$"sim_s_count_C.13.4",data$"sim_s_count_T.13.4",
        data$"sim_s_count_C.13.5",data$"sim_s_count_T.13.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 5, Round 1",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[13,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[13,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.14.1",data$"sim_s_count_T.14.1",
        data$"sim_s_count_C.14.2",data$"sim_s_count_T.14.2",
        data$"sim_s_count_C.14.3",data$"sim_s_count_T.14.3",
        data$"sim_s_count_C.14.4",data$"sim_s_count_T.14.4",
        data$"sim_s_count_C.14.5",data$"sim_s_count_T.14.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 5, Round 2",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[14,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[14,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.15.1",data$"sim_s_count_T.15.1",
        data$"sim_s_count_C.15.2",data$"sim_s_count_T.15.2",
        data$"sim_s_count_C.15.3",data$"sim_s_count_T.15.3",
        data$"sim_s_count_C.15.4",data$"sim_s_count_T.15.4",
        data$"sim_s_count_C.15.5",data$"sim_s_count_T.15.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 5, Round 3",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex=2)
points(spors_C[15,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[15,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.16.1",data$"sim_s_count_T.16.1",
        data$"sim_s_count_C.16.2",data$"sim_s_count_T.16.2",
        data$"sim_s_count_C.16.3",data$"sim_s_count_T.16.3",
        data$"sim_s_count_C.16.4",data$"sim_s_count_T.16.4",
        data$"sim_s_count_C.16.5",data$"sim_s_count_T.16.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 5, Round 4",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[16,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[16,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.17.1",data$"sim_s_count_T.17.1",
        data$"sim_s_count_C.17.2",data$"sim_s_count_T.17.2",
        data$"sim_s_count_C.17.3",data$"sim_s_count_T.17.3",
        data$"sim_s_count_C.17.4",data$"sim_s_count_T.17.4",
        data$"sim_s_count_C.17.5",data$"sim_s_count_T.17.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 5, Round 4",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[16,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[16,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.18.1",data$"sim_s_count_T.18.1",
        data$"sim_s_count_C.18.2",data$"sim_s_count_T.18.2",
        data$"sim_s_count_C.18.3",data$"sim_s_count_T.18.3",
        data$"sim_s_count_C.18.4",data$"sim_s_count_T.18.4",
        data$"sim_s_count_C.18.5",data$"sim_s_count_T.18.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 5, Round 4",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[16,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[16,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.19.1",data$"sim_s_count_T.19.1",
        data$"sim_s_count_C.19.2",data$"sim_s_count_T.19.2",
        data$"sim_s_count_C.19.3",data$"sim_s_count_T.19.3",
        data$"sim_s_count_C.19.4",data$"sim_s_count_T.19.4",
        data$"sim_s_count_C.19.5",data$"sim_s_count_T.19.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 5, Round 4",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[16,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[16,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

boxplot(data$"sim_s_count_C.20.1",data$"sim_s_count_T.20.1",
        data$"sim_s_count_C.20.2",data$"sim_s_count_T.20.2",
        data$"sim_s_count_C.20.3",data$"sim_s_count_T.20.3",
        data$"sim_s_count_C.20.4",data$"sim_s_count_T.20.4",
        data$"sim_s_count_C.20.5",data$"sim_s_count_T.20.5",
        xaxt="n",ylab="Simulated sporozoite counts",main= "Bite 5, Round 4",
        col=c("aquamarine","blueviolet"),frame=F,cex.lab=1.5)
axis(1,at=c(1.5,3.5,5.5,7.5,9.5),labels=c("0","1-10","11-100","101-1000","1001+"),cex.lab=2)
points(spors_C[16,]~c(1,3,5,7,9),col="red",pch=20,cex=2)
points(spors_T[16,]~c(2,4,6,8,10),col="orange",pch=20,cex=2)

###########################
##
##
## Oocyst data
##
oocreal1<-oocysts$oocystsbites2control[oocysts$round=="day41"][1:24]
oocsim1<-data$"sim_ooc_count_C.1"
oocreal2<-oocysts$oocystsbites2control[oocysts$round=="day72"][1:24]
oocsim2<-data$"sim_ooc_count_C.2"
oocreal3<-oocysts$oocystsbites2control[oocysts$round=="day103"][1:24]
oocsim3<-data$"sim_ooc_count_C.3"
oocreal4<-oocysts$oocystsbites2control[oocysts$round=="day134"][1:24]
oocsim4<-data$"sim_ooc_count_C.4"

length(oocreal);length(oocsim)
Oocysts.Bite2<-c(sort(oocreal1),sort(oocsim1),sort(oocreal2),sort(oocsim2),
                 sort(oocreal3),sort(oocsim3),sort(oocreal4),sort(oocsim4))
Data<-c(rep("Observed",24),rep("Simulated",500),
         rep("Observed",24),rep("Simulated",500),
         rep("Observed",24),rep("Simulated",500),
         rep("Observed",24),rep("Simulated",500))
dataEx2<-data.frame(Oocysts.Bite2,Data)

b2<- ggplot(dataEx2, aes(Oocysts.Bite2, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')

oocreal5<-oocysts$oocystsbites3control[oocysts$round=="day41"][1:24]
oocsim5<-data$"sim_ooc_count_C.5"
oocreal6<-oocysts$oocystsbites3control[oocysts$round=="day72"][1:24]
oocsim6<-data$"sim_ooc_count_C.6"
oocreal7<-oocysts$oocystsbites3control[oocysts$round=="day103"][1:24]
oocsim7<-data$"sim_ooc_count_C.7"
oocreal8<-oocysts$oocystsbites3control[oocysts$round=="day134"][1:24]
oocsim8<-data$"sim_ooc_count_C.8"

Oocysts.Bite3<-c(sort(oocreal5),sort(oocsim5),sort(oocreal6),sort(oocsim6),
                 sort(oocreal7),sort(oocsim7),sort(oocreal8),sort(oocsim8))
dataEx3<-data.frame(Oocysts.Bite2,Data)

b3<- ggplot(dataEx3, aes(Oocysts.Bite3, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')

oocreal9<-oocysts$oocystsbites4control[oocysts$round=="day41"][1:24]
oocsim9<-data$"sim_ooc_count_C.9"
oocreal10<-oocysts$oocystsbites4control[oocysts$round=="day72"][1:24]
oocsim10<-data$"sim_ooc_count_C.10"
oocreal11<-oocysts$oocystsbites4control[oocysts$round=="day103"][1:24]
oocsim11<-data$"sim_ooc_count_C.11"
oocreal12<-oocysts$oocystsbites4control[oocysts$round=="day134"][1:24]
oocsim12<-data$"sim_ooc_count_C.12"

Oocysts.Bite4<-c(sort(oocreal9),sort(oocsim9),sort(oocreal10),sort(oocsim10),
                 sort(oocreal11),sort(oocsim11),sort(oocreal12),sort(oocsim12))
dataEx4<-data.frame(Oocysts.Bite4,Data)

b4<- ggplot(dataEx4, aes(Oocysts.Bite4, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')

oocreal13<-oocysts$oocystsbites5control[oocysts$round=="day41"][1:24]
oocsim13<-data$"sim_ooc_count_C.13"
oocreal14<-oocysts$oocystsbites5control[oocysts$round=="day72"][1:24]
oocsim14<-data$"sim_ooc_count_C.14"
oocreal15<-oocysts$oocystsbites5control[oocysts$round=="day103"][1:24]
oocsim15<-data$"sim_ooc_count_C.15"
oocreal16<-oocysts$oocystsbites5control[oocysts$round=="day134"][1:24]
oocsim16<-data$"sim_ooc_count_C.16"

Oocysts.Bite5<-c(sort(oocreal13),sort(oocsim13),sort(oocreal14),sort(oocsim14),
                 sort(oocreal15),sort(oocsim15),sort(oocreal16),sort(oocsim16))
dataEx5<-data.frame(Oocysts.Bite5,Data)

b5<- ggplot(dataEx5, aes(Oocysts.Bite5, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')

multiplot(b2,b4,b3,b5,cols=2)

###########################
##
##
## Prevalence data
##
par(mfrow=c(1,2))
plot(c(mean(data$"sim_prev_C.1"),mean(data$"sim_prev_C.2"),
       mean(data$"sim_prev_C.3"),mean(data$"sim_prev_C.4"),
       mean(data$"sim_prev_C.5"),mean(data$"sim_prev_C.6"),
       mean(data$"sim_prev_C.7"),mean(data$"sim_prev_C.8"),
       mean(data$"sim_prev_C.9"),mean(data$"sim_prev_C.10"),
       mean(data$"sim_prev_C.11"),mean(data$"sim_prev_C.12"),
       mean(data$"sim_prev_C.13"),mean(data$"sim_prev_C.14"),
       mean(data$"sim_prev_C.15"),mean(data$"sim_prev_C.16"))~
       seq(1,16,1),ylim=c(0,5),xaxt="n",bty="n",cex.lab=1.4,
     ylab="Number of mice infected",main="Control group",
     xlab="Mosquito biting ratio (4 generations)")
polygon(c(c(0.55,1.5), rev(c(0.55,1.5))),
        c(c(quantile(data$"sim_prev_C.1",0.25),quantile(data$"sim_prev_C.1",0.25)),
                     rev(c(quantile(data$"sim_prev_C.1",0.75),quantile(data$"sim_prev_C.1",0.75)))),
                         border=NA, col=transp("aquamarine1",alpha=0.2))
polygon(c(c(1.5,2.5), rev(c(1.5,2.5))),
        c(c(quantile(data$"sim_prev_C.2",0.25),quantile(data$"sim_prev_C.2",0.25)),
          rev(c(quantile(data$"sim_prev_C.2",0.75),quantile(data$"sim_prev_C.2",0.75)))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
polygon(c(c(2.5,3.5), rev(c(2.5,3.5))),
        c(c(quantile(data$"sim_prev_C.3",0.25),quantile(data$"sim_prev_C.3",0.25)),
          rev(c(quantile(data$"sim_prev_C.3",0.75),quantile(data$"sim_prev_C.3",0.75)))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
polygon(c(c(3.5,4.45), rev(c(3.5,4.45))),
        c(c(quantile(data$"sim_prev_C.4",0.25),quantile(data$"sim_prev_C.4",0.25)),
          rev(c(quantile(data$"sim_prev_C.4",0.75),quantile(data$"sim_prev_C.4",0.75)))),
        border=NA, col=transp("aquamarine1",alpha=0.2))

polygon(c(c(4.55,5.5), rev(c(4.55,5.5))),
        c(c(quantile(data$"sim_prev_C.5",0.25),quantile(data$"sim_prev_C.5",0.25)),
          rev(c(quantile(data$"sim_prev_C.5",0.75),quantile(data$"sim_prev_C.5",0.75)))),
        border=NA, col=transp("blueviolet",alpha=0.2))
polygon(c(c(5.5,6.5), rev(c(5.5,6.5))),
        c(c(quantile(data$"sim_prev_C.6",0.25),quantile(data$"sim_prev_C.6",0.25)),
          rev(c(quantile(data$"sim_prev_C.6",0.75),quantile(data$"sim_prev_C.6",0.75)))),
        border=NA, col=transp("blueviolet",alpha=0.2))
polygon(c(c(6.5,7.5), rev(c(6.5,7.5))),
        c(c(quantile(data$"sim_prev_C.7",0.25),quantile(data$"sim_prev_C.7",0.25)),
          rev(c(quantile(data$"sim_prev_C.7",0.75),quantile(data$"sim_prev_C.7",0.75)))),
        border=NA, col=transp("blueviolet",alpha=0.2))
polygon(c(c(7.5,8.45), rev(c(7.5,8.45))),
        c(c(quantile(data$"sim_prev_C.8",0.25),quantile(data$"sim_prev_C.8",0.25)),
          rev(c(quantile(data$"sim_prev_C.8",0.75),quantile(data$"sim_prev_C.8",0.75)))),
        border=NA, col=transp("blueviolet",alpha=0.2))

polygon(c(c(8.55,9.5), rev(c(8.55,9.5))),
        c(c(quantile(data$"sim_prev_C.9",0.25),quantile(data$"sim_prev_C.9",0.25)),
          rev(c(quantile(data$"sim_prev_C.9",0.75),quantile(data$"sim_prev_C.9",0.75)))),
        border=NA, col=transp("blue2",alpha=0.2))
polygon(c(c(9.5,10.5), rev(c(9.5,10.5))),
        c(c(quantile(data$"sim_prev_C.10",0.25),quantile(data$"sim_prev_C.10",0.25)),
          rev(c(quantile(data$"sim_prev_C.10",0.75),quantile(data$"sim_prev_C.10",0.75)))),
        border=NA, col=transp("blue2",alpha=0.2))
polygon(c(c(10.5,11.5), rev(c(10.5,11.5))),
        c(c(quantile(data$"sim_prev_C.11",0.25),quantile(data$"sim_prev_C.11",0.25)),
          rev(c(quantile(data$"sim_prev_C.11",0.75),quantile(data$"sim_prev_C.11",0.75)))),
        border=NA, col=transp("blue2",alpha=0.2))
polygon(c(c(11.5,12.45), rev(c(11.5,12.45))),
        c(c(quantile(data$"sim_prev_C.12",0.25),quantile(data$"sim_prev_C.12",0.25)),
          rev(c(quantile(data$"sim_prev_C.12",0.75),quantile(data$"sim_prev_C.12",0.75)))),
        border=NA, col=transp("blue2",alpha=0.2))

polygon(c(c(12.55,13.5), rev(c(12.55,13.5))),
        c(c(quantile(data$"sim_prev_C.13",0.25),quantile(data$"sim_prev_C.13",0.25)),
          rev(c(quantile(data$"sim_prev_C.13",0.75),quantile(data$"sim_prev_C.13",0.75)))),
        border=NA, col=transp("brown4",alpha=0.2))
polygon(c(c(13.5,14.5), rev(c(13.5,14.5))),
        c(c(quantile(data$"sim_prev_C.14",0.25),quantile(data$"sim_prev_C.14",0.25)),
          rev(c(quantile(data$"sim_prev_C.14",0.75),quantile(data$"sim_prev_C.14",0.75)))),
        border=NA, col=transp("brown4",alpha=0.2))
polygon(c(c(14.5,15.5), rev(c(14.5,15.5))),
        c(c(quantile(data$"sim_prev_C.15",0.25),quantile(data$"sim_prev_C.15",0.25)),
          rev(c(quantile(data$"sim_prev_C.15",0.75),quantile(data$"sim_prev_C.15",0.75)))),
        border=NA, col=transp("brown4",alpha=0.2))
polygon(c(c(15.5,16.45), rev(c(15.5,16.45))),
        c(c(quantile(data$"sim_prev_C.16",0.25),quantile(data$"sim_prev_C.16",0.25)),
          rev(c(quantile(data$"sim_prev_C.16",0.75),quantile(data$"sim_prev_C.16",0.75)))),
        border=NA, col=transp("brown4",alpha=0.2))

points(c(sum(PREV_C[,1]),sum(PREV_C[,2]),sum(PREV_C[,3]),sum(PREV_C[,4]),
         sum(PREV_C[,5]),sum(PREV_C[,6]),sum(PREV_C[,7]),sum(PREV_C[,8]),
         sum(PREV_C[,9]),sum(PREV_C[,10]),sum(PREV_C[,11]),sum(PREV_C[,12]),
         sum(PREV_C[,13]),sum(PREV_C[,14]),sum(PREV_C[,15]),sum(PREV_C[,16])),pch=20,cex=2,col="red")

axis(1,at=seq(2.5,14.5,4),labels=c(2,3,4,5))

plot(c(mean(data$"sim_prev_T.1"),mean(data$"sim_prev_T.2"),
       mean(data$"sim_prev_T.3"),mean(data$"sim_prev_T.4"),
       mean(data$"sim_prev_T.5"),mean(data$"sim_prev_T.6"),
       mean(data$"sim_prev_T.7"),mean(data$"sim_prev_T.8"),
       mean(data$"sim_prev_T.9"),mean(data$"sim_prev_T.10"),
       mean(data$"sim_prev_T.11"),mean(data$"sim_prev_T.12"),
       mean(data$"sim_prev_T.13"),mean(data$"sim_prev_T.14"),
       mean(data$"sim_prev_T.15"),mean(data$"sim_prev_T.16"))~
       seq(1,16,1),ylim=c(0,5),xaxt="n",bty="n",cex.lab=1.4,
     ylab="Number of mice infected",main="Treatment group",
     xlab="Mosquito biting ratio (4 generations)")
polygon(c(c(0.55,1.5), rev(c(0.55,1.5))),
        c(c(quantile(data$"sim_prev_T.1",0.25),quantile(data$"sim_prev_T.1",0.25)),
          rev(c(quantile(data$"sim_prev_T.1",0.75),quantile(data$"sim_prev_T.1",0.75)))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
polygon(c(c(1.5,2.5), rev(c(1.5,2.5))),
        c(c(quantile(data$"sim_prev_T.2",0.25),quantile(data$"sim_prev_T.2",0.25)),
          rev(c(quantile(data$"sim_prev_T.2",0.75),quantile(data$"sim_prev_T.2",0.75)))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
polygon(c(c(2.5,3.5), rev(c(2.5,3.5))),
        c(c(quantile(data$"sim_prev_T.3",0.25),quantile(data$"sim_prev_T.3",0.25)),
          rev(c(quantile(data$"sim_prev_T.3",0.75),quantile(data$"sim_prev_T.3",0.75)))),
        border=NA, col=transp("aquamarine1",alpha=0.2))
polygon(c(c(3.5,4.45), rev(c(3.5,4.45))),
        c(c(quantile(data$"sim_prev_T.4",0.25),quantile(data$"sim_prev_T.4",0.25)),
          rev(c(quantile(data$"sim_prev_T.4",0.75),quantile(data$"sim_prev_T.4",0.75)))),
        border=NA, col=transp("aquamarine1",alpha=0.2))

polygon(c(c(4.55,5.5), rev(c(4.55,5.5))),
        c(c(quantile(data$"sim_prev_T.5",0.25),quantile(data$"sim_prev_T.5",0.25)),
          rev(c(quantile(data$"sim_prev_T.5",0.75),quantile(data$"sim_prev_T.5",0.75)))),
        border=NA, col=transp("blueviolet",alpha=0.2))
polygon(c(c(5.5,6.5), rev(c(5.5,6.5))),
        c(c(quantile(data$"sim_prev_T.6",0.25),quantile(data$"sim_prev_T.6",0.25)),
          rev(c(quantile(data$"sim_prev_T.6",0.75),quantile(data$"sim_prev_T.6",0.75)))),
        border=NA, col=transp("blueviolet",alpha=0.2))
polygon(c(c(6.5,7.5), rev(c(6.5,7.5))),
        c(c(quantile(data$"sim_prev_T.7",0.25),quantile(data$"sim_prev_T.7",0.25)),
          rev(c(quantile(data$"sim_prev_T.7",0.75),quantile(data$"sim_prev_T.7",0.75)))),
        border=NA, col=transp("blueviolet",alpha=0.2))
polygon(c(c(7.5,8.45), rev(c(7.5,8.45))),
        c(c(quantile(data$"sim_prev_T.8",0.25),quantile(data$"sim_prev_T.8",0.25)),
          rev(c(quantile(data$"sim_prev_T.8",0.75),quantile(data$"sim_prev_T.8",0.75)))),
        border=NA, col=transp("blueviolet",alpha=0.2))

polygon(c(c(8.55,9.5), rev(c(8.55,9.5))),
        c(c(quantile(data$"sim_prev_T.9",0.25),quantile(data$"sim_prev_T.9",0.25)),
          rev(c(quantile(data$"sim_prev_T.9",0.75),quantile(data$"sim_prev_T.9",0.75)))),
        border=NA, col=transp("blue2",alpha=0.2))
polygon(c(c(9.5,10.5), rev(c(9.5,10.5))),
        c(c(quantile(data$"sim_prev_T.10",0.25),quantile(data$"sim_prev_T.10",0.25)),
          rev(c(quantile(data$"sim_prev_T.10",0.75),quantile(data$"sim_prev_T.10",0.75)))),
        border=NA, col=transp("blue2",alpha=0.2))
polygon(c(c(10.5,11.5), rev(c(10.5,11.5))),
        c(c(quantile(data$"sim_prev_T.11",0.25),quantile(data$"sim_prev_T.11",0.25)),
          rev(c(quantile(data$"sim_prev_T.11",0.75),quantile(data$"sim_prev_T.11",0.75)))),
        border=NA, col=transp("blue2",alpha=0.2))
polygon(c(c(11.5,12.45), rev(c(11.5,12.45))),
        c(c(quantile(data$"sim_prev_T.12",0.25),quantile(data$"sim_prev_T.12",0.25)),
          rev(c(quantile(data$"sim_prev_T.12",0.75),quantile(data$"sim_prev_T.12",0.75)))),
        border=NA, col=transp("blue2",alpha=0.2))

polygon(c(c(12.55,13.5), rev(c(12.55,13.5))),
        c(c(quantile(data$"sim_prev_T.13",0.25),quantile(data$"sim_prev_T.13",0.25)),
          rev(c(quantile(data$"sim_prev_T.13",0.75),quantile(data$"sim_prev_T.13",0.75)))),
        border=NA, col=transp("brown4",alpha=0.2))
polygon(c(c(13.5,14.5), rev(c(13.5,14.5))),
        c(c(quantile(data$"sim_prev_T.14",0.25),quantile(data$"sim_prev_T.14",0.25)),
          rev(c(quantile(data$"sim_prev_T.14",0.75),quantile(data$"sim_prev_T.14",0.75)))),
        border=NA, col=transp("brown4",alpha=0.2))
polygon(c(c(14.5,15.5), rev(c(14.5,15.5))),
        c(c(quantile(data$"sim_prev_T.15",0.25),quantile(data$"sim_prev_T.15",0.25)),
          rev(c(quantile(data$"sim_prev_T.15",0.75),quantile(data$"sim_prev_T.15",0.75)))),
        border=NA, col=transp("brown4",alpha=0.2))
polygon(c(c(15.5,16.45), rev(c(15.5,16.45))),
        c(c(quantile(data$"sim_prev_T.16",0.25),quantile(data$"sim_prev_T.16",0.25)),
          rev(c(quantile(data$"sim_prev_T.16",0.75),quantile(data$"sim_prev_T.16",0.75)))),
        border=NA, col=transp("brown4",alpha=0.2))

points(c(sum(PREV_T[,1]),sum(PREV_T[,2]),sum(PREV_T[,3]),sum(PREV_T[,4]),
         sum(PREV_T[,5]),sum(PREV_T[,6]),sum(PREV_T[,7]),sum(PREV_T[,8]),
         sum(PREV_T[,9]),sum(PREV_T[,10]),sum(PREV_T[,11]),sum(PREV_T[,12]),
         sum(PREV_T[,13]),sum(PREV_T[,14]),sum(PREV_T[,15]),sum(PREV_T[,16])),pch=20,cex=2,col="red")

axis(1,at=seq(2.5,14.5,4),labels=c(2,3,4,5))

####################################
##
## Mean data for the model estimates
##
############################

##Probability of infection
#Controls
sum(data[,252:267])/(500*16) ##mouse
sum(data[,284:299])/(500*16) ##mosquito

avec<-bvec<-numeric(10000)
vec1<-stack(data[,252:267]);vec2<-stack(data[,284:299])
for(i in 1:10000)  avec[i]<-mean(sample(vec1$values,replace=T),na.rm=T)
quantile(avec,c(0.025,0.975));thet_mouseCL <- 0.6697934;thet_mouseCU <- 0.6742318
for(i in 1:10000)  bvec[i]<-mean(sample(vec2$values,replace=T),na.rm=T)
quantile(bvec,c(0.025,0.975));thet_mosqCL <- 0.5112327;thet_mosqCU <- 0.5168620


#Treatment
sum(data[,268:283])/(500*16)
sum(data[,300:315])/(500*16)

cvec<-dvec<-numeric(10000)
vec3<-stack(data[,268:283]);vec4<-stack(data[,300:315])
for(i in 1:10000)  cvec[i]<-mean(sample(vec3$values,replace=T),na.rm=T)
quantile(cvec,c(0.025,0.975));thet_mouseTL <- 0.5626823;thet_mouseTU <- 0.5747212
for(i in 1:10000)  dvec[i]<-mean(sample(vec4$values,replace=T),na.rm=T)
quantile(dvec,c(0.025,0.975));thet_mosqTL <- 0.4664724;thet_mosqTU <- 0.4797919


#Efficacy on mosquito infection
(sum(data[,284:299])/(500*16)-sum(data[,300:315])/(500*16))/sum(data[,284:299])/(500*16)

dateffCI<-matrix(nrow=500,ncol=16)
dateffCI<-(data[,284:299]-data[,300:315])/data[,284:299]
head(dateffCI)
EfficacyEstimate2<-sum(dateffCI)/(500*16)
x<-stack(dateffCI)
x2<-stack(dateffCI[,1:4])
EfficacyEstimate2Lower<-quantile(x$values,0.05)
EfficacyEstimate2Upper<-quantile(x$values,0.95)

##Probability of mouse infection
#Controls
sum(data[,252:267])/(500*16)
#Treatment
sum(data[,268:283])/(500*16)

#Efficacy on mouse infection
100*((sum(data[,252:267])/(500*16))-(sum(data[,268:283])/(500*16)))/(sum(data[,252:267])/(500*16))


##Overall efficacy
(((sum(data[,284:299])/(500*16))-(sum(data[,300:315])/(500*16)))+
  ((sum(data[,252:267])/(500*16))-(sum(data[,268:283])/(500*16))))/
  ((sum(data[,284:299])/(500*16))+(sum(data[,252:267])/(500*16)))

((thet_mouseCL-thet_mouseTL)+(thet_mosqCL-thet_mosqTL))/(thet_mouseCL+thet_mosqCL)
((thet_mouseCU-thet_mouseTU)+(thet_mosqCU-thet_mosqTU))/(thet_mouseCU+thet_mosqCU)


##2 bites effect size
(((sum(data[,284:287])/(500*4))-(sum(data[,300:303])/(500*4)))+
   ((sum(data[,252:255])/(500*4))-(sum(data[,268:271])/(500*4))))/
  ((sum(data[,284:287])/(500*4))+(sum(data[,252:255])/(500*4)))

##3 bites effect size
(((sum(data[,288:291])/(500*4))-(sum(data[,304:307])/(500*4)))+
   ((sum(data[,256:259])/(500*4))-(sum(data[,272:275])/(500*4))))/
  ((sum(data[,288:291])/(500*4))+(sum(data[,256:259])/(500*4)))

##4 bites effect size
(((sum(data[,292:295])/(500*4))-(sum(data[,308:311])/(500*4)))+
   ((sum(data[,260:263])/(500*4))-(sum(data[,276:279])/(500*4))))/
  ((sum(data[,292:295])/(500*4))+(sum(data[,260:263])/(500*4)))

##5 bites effect size
(((sum(data[,296:299])/(500*4))-(sum(data[,312:315])/(500*4)))+
   ((sum(data[,264:267])/(500*4))-(sum(data[,280:283])/(500*4))))/
  ((sum(data[,296:299])/(500*4))+(sum(data[,264:267])/(500*4)))

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

multiplot(aOocystsC, aOocystsT, aOocysts, cols=3)##run multiplot function
##################################
###
###################################
estprev<-stack(data[,252:283])
estpervmq<-stack(data[,284:315])
Prev_Mice<-c(estprev[,1],prevs)

  prevmq<-c(sum(prevoocC[1:24])/24,sum(prevoocC[25:48])/24,sum(prevoocC[49:72])/24,sum(prevoocC[73:96])/24,
            sum(prevoocC[97:120])/24,sum(prevoocC[121:144])/24,sum(prevoocC[145:168])/24,sum(prevoocC[169:192])/24,
            sum(prevoocC[193:216])/24,sum(prevoocC[217:240])/24,sum(prevoocC[241:264])/24,sum(prevoocC[265:288])/24,
            sum(prevoocC[289:312])/24,sum(prevoocC[313:336])/24,sum(prevoocC[337:360])/24,sum(prevoocC[361:384])/24)
            
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
thet_mosq_means<-numeric(32)
for(i in 284:315){
  thet_mosq_means[i-283]<-mean(data[,i],na.rm=T)
}

    a1<-stack(data[,284:287]);a2<-stack(data[,288:291]);a3<-stack(data[,292:295]);a4<-stack(data[,296:299]);
    thet_mosq_bitesC<-c(mean(a1$value),mean(a2$value),mean(a3$value),mean(a4$value))
    thet_mosq_bitesC_range<-c(a1$value,a2$value,a3$value,a4$value)

    B1<-stack(data[,300:303]);B2<-stack(data[,304:307]);B3<-stack(data[,308:311]);B4<-stack(data[,312:315]);
    thet_mosq_bitesT<-c(mean(B1$value),mean(B2$value),mean(B3$value),mean(B4$value))
    thet_mosq_bitesT_range<-c(B1$value,B2$value,B3$value,B4$value)

EffectSizeMosq_permbr<-(thet_mosq_bitesC-thet_mosq_bitesT)/thet_mosq_bitesC
EffectSizeMosq<-(mean(thet_mosq_bitesC)-mean(thet_mosq_bitesT))/mean(thet_mosq_bitesC)

dat_a1<-(a1$value-B1$value)/a1$value; dat_a2<-(a2$value-B2$value)/a2$value; dat_a3<-(a3$value-B3$value)/a3$value; dat_a4<-(a4$value-B4$value)/a4$value

thet_effect_mosq <- c(dat_a1,dat_a2,dat_a3,dat_a4)
length(thet_effect_mosq)
Bites <- rep(c(2,3,4,5),each=2000)
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
plot((R_EffectSizeMosq+1)/2~c(2,3,4,5),pty="",main="ATV effect on mosquito population",
     xaxt="n",yaxt="n",pch="",xlim=c(1.5,5.5),
     ylab=expression(paste("Effect Size ", theta[mosquitoes], )),xlab="Bites",ylim=c(0,1))
axis(1,par(las=1),at=seq(2,5,1),labels=seq(2,5,1))

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(2,3,4,5))) / (1 + exp(a + b * c(2,3,4,5))) ) 
  prev1<-(R_EffectSizeMosqL+1)/2
  
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
  
  pred1a<- ((exp(a + b * c(2,3,4,5))) / (1 + exp(a + b * c(2,3,4,5))) ) 
  prev1<-(R_EffectSizeMosqU+1)/2
  
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
  
  pred1a<- ((exp(a + b * c(2,3,4,5))) / (1 + exp(a + b * c(2,3,4,5))) ) 
  prev1<-(R_EffectSizeMosq+1)/2
  
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
boxplot(c(NA,datmosq$thet_effect_mosq)~c(1,datmosq$Bites),xaxt="n",yaxt="n",ylim=c(-0.9,1))
points(R_EffectSizeMosq~c(1,2,3,4),pch=20,col="red")


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
for(i in 252:283){
  thet_mo_means[i-251]<-mean(data[,i])
}


a1<-stack(data[,252:255]);a2<-stack(data[,256:259]);a3<-stack(data[,260:263]);a4<-stack(data[,264:267]);
thet_mouse_bitesC<-c(mean(a1$value),mean(a2$value),mean(a3$value),mean(a4$value))
thet_mouse_bitesC_range<-c(a1$value,a2$value,a3$value,a4$value)

B1<-stack(data[,268:271]);B2<-stack(data[,272:275]);B3<-stack(data[,276:279]);B4<-stack(data[,280:283]);
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

parasitemiaAll<-c(parasitORIGmean,parasitATV32mean)
gametocytemiaAll<-c(gametORIGmean)
type<-c(rep("Control",20),rep("Treated",20))

glma <- glm(gametocytemiaAll~parasitemiaAll+type)
glmb <- glm(gametocytemiaAll~parasitemiaAll)
anova(glma,glmb,test="Chi")
summary.lm(glmb)
plot(gametocytemiaAll~parasitemiaAll)



##############################################################
##
## DOES THE PROBABILITY OF MOUSE INFECTION CORRELATE WITH PARASITEMIA/GAMETOCYTES?
##
##
##
####################################################
thet_mo_means

glmc <- glm(thet_mo_means~parasitemiaAll+type+0)
glmd <- glm(thet_mo_means~parasitemiaAll+0)
anova(glmc,glmd,test="Chi")
summary.lm(glmd)
plot(glmd)
plot(thet_mosq_means~parasitemiaAll)

datprob<-data.frame(thet_mo_means,thet_mosq_means,parasitemiaAll,gametocytemiaAll,type)
datprob$thet_mo_means<-ifelse(datprob$parasitemiaAll==0,0,datprob$thet_mo_means)
datprob$thet_mosq_means<-ifelse(datprob$parasitemiaAll==0,0,datprob$thet_mosq_means)

plot(datprob$parasitemiaAll,datprob$thet_mo_means,ylim=c(0,1))
points(datprob$parasitemiaAll,datprob$thet_mosq_means,pch=20)
abline(lm(datprob$thet_mo_means~datprob$parasitemiaAll+0),lty=2,col="grey")

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(0,datprob$parasitemiaAll))) / (1 + exp(a + b * c(0,datprob$parasitemiaAll))) ) 
  prev1<-c(0,datprob$thet_mo_means)
  
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
  
  pred1a<- (a * exp (b * exp(c * c(0,datprob$parasitemiaAll))))  
  prev1<-c(0,datprob$thet_mo_means)
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-3
gommod<-optim(c(1,-2,-0.8),gom.binom,method="L-BFGS-B",lower=c(0.001,-5,-1),upper=c(1,0,-0.01))
gommod
nc<-seq(0,20,0.01)
pred2<-(gommod$par[1] * exp (gommod$par[2] * exp(gommod$par[3] * nc)))
lines(nc,pred2,lwd=2,lty=2,col="black")


####################
## Contour Plots
log_mu_ooc_TMeanBites2 <-   data$rho__log_mean_ooc_T + 
  data$tau_bite__log_mean_ooc_T * data$rho_bite__log_mean_ooc_T.1 + 
  data$tau_round__log_mean_ooc_T * data$rho_round__log_mean_ooc_T.1

log_mu_ooc_TMeanBites3 <-   data$rho__log_mean_ooc_T + 
  data$tau_bite__log_mean_ooc_T * data$rho_bite__log_mean_ooc_T.2 + 
  data$tau_round__log_mean_ooc_T * data$rho_round__log_mean_ooc_T.2

log_mu_ooc_TMeanBites4 <-   data$rho__log_mean_ooc_T + 
  data$tau_bite__log_mean_ooc_T * data$rho_bite__log_mean_ooc_T.3 + 
  data$tau_round__log_mean_ooc_T * data$rho_round__log_mean_ooc_T.3

log_mu_ooc_TMeanBites5 <-   data$rho__log_mean_ooc_T + 
  data$tau_bite__log_mean_ooc_T * data$rho_bite__log_mean_ooc_T.4 + 
  data$tau_round__log_mean_ooc_T * data$rho_round__log_mean_ooc_T.4

log_phi_ooc_TMeanB2 <-  data$rho__log_od_ooc_T + 
  data$tau_bite__log_od_ooc_T * data$rho_bite__log_od_ooc_T.1 + 
  data$tau_round__log_od_ooc_T * data$rho_round__log_od_ooc_T.1

log_phi_ooc_TMeanB3 <-  data$rho__log_od_ooc_T + 
  data$tau_bite__log_od_ooc_T * data$rho_bite__log_od_ooc_T.2 + 
  data$tau_round__log_od_ooc_T * data$rho_round__log_od_ooc_T.2

log_phi_ooc_TMeanB4 <-  data$rho__log_od_ooc_T + 
  data$tau_bite__log_od_ooc_T * data$rho_bite__log_od_ooc_T.3 + 
  data$tau_round__log_od_ooc_T * data$rho_round__log_od_ooc_T.3

log_phi_ooc_TMeanB5 <-  data$rho__log_od_ooc_T + 
  data$tau_bite__log_od_ooc_T * data$rho_bite__log_od_ooc_T.4 + 
  data$tau_round__log_od_ooc_T * data$rho_round__log_od_ooc_T.4

log_muB2 <-  data$beta_mu.1 * log_mu_ooc_TMeanBites2 + 
  data$beta_mu.2 * log_phi_ooc_TMeanB2 + data$alpha_mu + 4
log_phiB2 <-  data$beta_phi.1 * log_mu_ooc_TMeanBites2 +
  data$beta_phi.2 * log_phi_ooc_TMeanB2 + data$alpha_phi + 4
#plot(log_muB2~log_phiB2,col="red",ylim=c(-10,10),xlim=c(-4,10))

log_muB3 <-  data$beta_mu.1 * log_mu_ooc_TMeanBites3 + 
  data$beta_mu.2 * log_phi_ooc_TMeanB3 + data$alpha_mu + 4
log_phiB3 <-  data$beta_phi.1 * log_mu_ooc_TMeanBites3 +
  data$beta_phi.2 * log_phi_ooc_TMeanB3 + data$alpha_phi + 4
#points(log_muB3~log_phiB3,col="green")

log_muB4 <-  data$beta_mu.1 * log_mu_ooc_TMeanBites4 + 
  data$beta_mu.2 * log_phi_ooc_TMeanB4 + data$alpha_mu + 4
log_phiB4 <-  data$beta_phi.1 * log_mu_ooc_TMeanBites4 +
  data$beta_phi.2 * log_phi_ooc_TMeanB4 + data$alpha_phi + 4
#points(log_muB4~log_phiB4,col="blue")

log_muB5 <-  data$beta_mu.1 * log_mu_ooc_TMeanBites5 + 
  data$beta_mu.2 * log_phi_ooc_TMeanB5 + data$alpha_mu + 4
log_phiB5 <-  data$beta_phi.1 * log_mu_ooc_TMeanBites5 +
  data$beta_phi.2 * log_phi_ooc_TMeanB5 + data$alpha_phi + 4
#points(log_muB5~log_phiB5,col="black")

#logit_theta_T2 <-    data$beta_theta.1 * log_muB2 +
 # data$beta_theta.2 * log_phiB2 + data$alpha_theta

#logit_theta_T3 <-    data$beta_theta.1 * log_muB3 +
 # data$beta_theta.2 * log_phiB3 + data$alpha_theta

#logit_theta_T4 <-    data$beta_theta.1 * log_muB4 +
 # data$beta_theta.2 * log_phiB4 + data$alpha_theta

#logit_theta_T5 <-    data$beta_theta.1 * log_muB5 +
 # data$beta_theta.2 * log_phiB5 + data$alpha_theta

x = log_muB2
y = log_phiB2; bites <- rep(2,length(x))
ztemp =(data$theta_T.1+data$theta_T.2+data$theta_T.3+data$theta_T.4)/4
zBtemp =(data$theta_T_mosquito.1+data$theta_T_mosquito.2+data$theta_T_mosquito.3+data$theta_T_mosquito.4)/4
dattrT2<-data.frame(log_mu_ooc_TMeanBites2,log_phi_ooc_TMeanB2,x,y,ztemp,zBtemp,bites)

x = log_muB3
y = log_phiB3; bites <- rep(3,length(x))
ztemp =(data$theta_T.5+data$theta_T.6+data$theta_T.7+data$theta_T.8)/4
zBtemp =(data$theta_T_mosquito.5+data$theta_T_mosquito.6+data$theta_T_mosquito.7+data$theta_T_mosquito.8)/4
dattrT3<-data.frame(log_mu_ooc_TMeanBites3,log_phi_ooc_TMeanB3,x,y,ztemp,zBtemp,bites)

x = log_muB4
y = log_phiB4; bites <- rep(4,length(x))
ztemp =(data$theta_T.9+data$theta_T.10+data$theta_T.11+data$theta_T.12)/4
zBtemp =(data$theta_T_mosquito.9+data$theta_T_mosquito.10+data$theta_T_mosquito.11+data$theta_T_mosquito.12)/4
dattrT4<-data.frame(log_mu_ooc_TMeanBites4,log_phi_ooc_TMeanB4,x,y,ztemp,zBtemp,bites)

x = log_muB5
y = log_phiB5; bites <- rep(5,length(x))
ztemp =(data$theta_T.13+data$theta_T.14+data$theta_T.15+data$theta_T.16)/4
zBtemp =(data$theta_T_mosquito.13+data$theta_T_mosquito.14+data$theta_T_mosquito.15+data$theta_T_mosquito.16)/4
dattrT5<-data.frame(log_mu_ooc_TMeanBites5,log_phi_ooc_TMeanB5,x,y,ztemp,zBtemp,bites)

dattrTEST <-rbind(dattrT2,dattrT3,dattrT4,dattrT5)
dattrT <- as.data.frame(dattrTEST)

#contour(interp(dattr$x, dattr$y, dattr$ztemp),color.palette = cm.colors,duplicate=FALSE)

library(akima)
par(mar=c(5,5,2,2))
#par(mfrow=c(2,4))
my.heat.colors <- function(x) { rev(heat.colors(x, alpha=1)) }
my.terrain.colors <- function(x) { rev(terrain.colors(x, alpha=1)) }
my.cm.colors <- function(x) { rev(cm.colors(x, alpha=1)) }
my.topo.colors <- function(x) { rev(topo.colors(x, alpha=1)) }

my.matrix1  <- interp(dattrT2$x, dattrT2$y, dattrT2$ztemp)
my.matrix2  <- interp(dattrT3$x, dattrT3$y, dattrT3$ztemp)
my.matrix3  <- interp(dattrT4$x, dattrT4$y, dattrT4$ztemp)
my.matrix4  <- interp(dattrT5$x, dattrT5$y, dattrT5$ztemp)

par(mar=c(5,5,5,5))
filled.contour(my.matrix1, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Treatment Bites 2 (Theta_mouse)")
filled.contour(my.matrix2, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Treatment Bites 3 (Theta_mouse)")
filled.contour(my.matrix3, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Treatment Bites 4 (Theta_mouse)")
filled.contour(my.matrix4, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Treatment Bites 5 (Theta_mouse)")

my.matrix1B  <- interp(dattrT2$x, dattrT2$y, dattrT2$zBtemp)
my.matrix2B  <- interp(dattrT3$x, dattrT3$y, dattrT3$zBtemp)
my.matrix3B  <- interp(dattrT4$x, dattrT4$y, dattrT4$zBtemp)
my.matrix4B  <- interp(dattrT5$x, dattrT5$y, dattrT5$zBtemp)
filled.contour(my.matrix1B, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Treatment Bites 2 (Theta_mosquito)")
filled.contour(my.matrix2B, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Treatment Bites 3 (Theta_mosquito)")
filled.contour(my.matrix3B, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Treatment Bites 4 (Theta_mosquito)")
filled.contour(my.matrix4B, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Treatment Bites 5 (Theta_mosquito)")

my.matrix1C  <- interp(dattrT2$log_mu_ooc_TMeanBites2, dattrT2$log_phi_ooc_TMeanB2, dattrT2$ztemp)
my.matrix2C  <- interp(dattrT3$log_mu_ooc_TMeanBites3, dattrT3$log_phi_ooc_TMeanB3, dattrT3$ztemp)
my.matrix3C  <- interp(dattrT4$log_mu_ooc_TMeanBites4, dattrT4$log_phi_ooc_TMeanB4, dattrT4$ztemp)
my.matrix4C  <- interp(dattrT5$log_mu_ooc_TMeanBites5, dattrT5$log_phi_ooc_TMeanB5, dattrT5$ztemp)
filled.contour(my.matrix1C, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Treatment Bites 2 (Theta_mouse)")
filled.contour(my.matrix2C, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Treatment Bites 3 (Theta_mouse)")
filled.contour(my.matrix3C, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Treatment Bites 4 (Theta_mouse)")
filled.contour(my.matrix4C, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Treatment Bites 5 (Theta_mouse)")

my.matrix1D  <- interp(dattrT2$log_mu_ooc_TMeanBites2, dattrT2$log_phi_ooc_TMeanB2, dattrT2$zBtemp)
my.matrix2D  <- interp(dattrT3$log_mu_ooc_TMeanBites3, dattrT3$log_phi_ooc_TMeanB3, dattrT3$zBtemp)
my.matrix3D  <- interp(dattrT4$log_mu_ooc_TMeanBites4, dattrT4$log_phi_ooc_TMeanB4, dattrT4$zBtemp)
my.matrix4D  <- interp(dattrT5$log_mu_ooc_TMeanBites5, dattrT5$log_phi_ooc_TMeanB5, dattrT5$zBtemp)
filled.contour(my.matrix1D, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-6,1),zlim=c(0,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Treatment Bites 2 (Theta_mosquito)")
filled.contour(my.matrix2D, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-6,1),zlim=c(0,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Treatment Bites 3 (Theta_mosquito)")
filled.contour(my.matrix3D, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-6,1),zlim=c(0,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Treatment Bites 4 (Theta_mosquito)")
filled.contour(my.matrix4D, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-6,1),zlim=c(0,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Treatment Bites 5 (Theta_mosquito)")

my.matT <- interp(dattrT$x,dattrT$y,dattrT$bites) ##JUST CHANGE NAMES TO RBIND THE DATA
filled.contour(my.matT,nlevels=4,color=my.heat.colors)
##############################
## Repeat for controls
log_mu_ooc_CMeanBites2 <-   data$rho__log_mean_ooc_C + 
  data$tau_bite__log_mean_ooc_C * data$rho_bite__log_mean_ooc_C.1 + 
  data$tau_round__log_mean_ooc_C * data$rho_round__log_mean_ooc_C.1

log_mu_ooc_CMeanBites3 <-   data$rho__log_mean_ooc_C + 
  data$tau_bite__log_mean_ooc_C * data$rho_bite__log_mean_ooc_C.2 + 
  data$tau_round__log_mean_ooc_C * data$rho_round__log_mean_ooc_C.2

log_mu_ooc_CMeanBites4 <-   data$rho__log_mean_ooc_C + 
  data$tau_bite__log_mean_ooc_C * data$rho_bite__log_mean_ooc_C.3 + 
  data$tau_round__log_mean_ooc_C * data$rho_round__log_mean_ooc_C.3

log_mu_ooc_CMeanBites5 <-   data$rho__log_mean_ooc_C + 
  data$tau_bite__log_mean_ooc_C * data$rho_bite__log_mean_ooc_C.4 + 
  data$tau_round__log_mean_ooc_C * data$rho_round__log_mean_ooc_C.4

log_phi_ooc_CMeanB2 <-  data$rho__log_od_ooc_C + 
  data$tau_bite__log_od_ooc_C * data$rho_bite__log_od_ooc_C.1 + 
  data$tau_round__log_od_ooc_C * data$rho_round__log_od_ooc_C.1

log_phi_ooc_CMeanB3 <-  data$rho__log_od_ooc_C + 
  data$tau_bite__log_od_ooc_C * data$rho_bite__log_od_ooc_C.2 + 
  data$tau_round__log_od_ooc_C * data$rho_round__log_od_ooc_C.2

log_phi_ooc_CMeanB4 <-  data$rho__log_od_ooc_C + 
  data$tau_bite__log_od_ooc_C * data$rho_bite__log_od_ooc_C.3 + 
  data$tau_round__log_od_ooc_C * data$rho_round__log_od_ooc_C.3

log_phi_ooc_CMeanB5 <-  data$rho__log_od_ooc_C + 
  data$tau_bite__log_od_ooc_C * data$rho_bite__log_od_ooc_C.4 + 
  data$tau_round__log_od_ooc_C * data$rho_round__log_od_ooc_C.4

log_muB2 <-  data$beta_mu.1 * log_mu_ooc_CMeanBites2 + 
  data$beta_mu.2 * log_phi_ooc_CMeanB2 + data$alpha_mu + 4
log_phiB2 <-  data$beta_phi.1 * log_mu_ooc_CMeanBites2 +
  data$beta_phi.2 * log_phi_ooc_CMeanB2 + data$alpha_phi + 4
#plot(log_muB2~log_phiB2,col="red",ylim=c(-10,10),xlim=c(-4,10))

log_muB3 <-  data$beta_mu.1 * log_mu_ooc_CMeanBites3 + 
  data$beta_mu.2 * log_phi_ooc_CMeanB3 + data$alpha_mu + 4
log_phiB3 <-  data$beta_phi.1 * log_mu_ooc_CMeanBites3 +
  data$beta_phi.2 * log_phi_ooc_CMeanB3 + data$alpha_phi + 4
#points(log_muB3~log_phiB3,col="green")

log_muB4 <-  data$beta_mu.1 * log_mu_ooc_CMeanBites4 + 
  data$beta_mu.2 * log_phi_ooc_CMeanB4 + data$alpha_mu + 4
log_phiB4 <-  data$beta_phi.1 * log_mu_ooc_CMeanBites4 +
  data$beta_phi.2 * log_phi_ooc_CMeanB4 + data$alpha_phi + 4
#points(log_muB4~log_phiB4,col="blue")

log_muB5 <-  data$beta_mu.1 * log_mu_ooc_CMeanBites5 + 
  data$beta_mu.2 * log_phi_ooc_CMeanB5 + data$alpha_mu + 4
log_phiB5 <-  data$beta_phi.1 * log_mu_ooc_CMeanBites5 +
  data$beta_phi.2 * log_phi_ooc_CMeanB5 + data$alpha_phi + 4
#points(log_muB5~log_phiB5,col="black")

x = log_muB2
y = log_phiB2; bites <- rep(2,length(x))
ztemp =(data$theta_C.1+data$theta_C.2+data$theta_C.3+data$theta_C.4)/4
zBtemp =(data$theta_C_mosquito.1+data$theta_C_mosquito.2+data$theta_C_mosquito.3+data$theta_C_mosquito.4)/4
dattr2<-data.frame(x,y,log_mu_ooc_CMeanBites2,log_phi_ooc_CMeanB2,ztemp,zBtemp,bites)

x = log_muB3
y = log_phiB3; bites <- rep(3,length(x))
ztemp =(data$theta_C.5+data$theta_C.6+data$theta_C.7+data$theta_C.8)/4
zBtemp =(data$theta_C_mosquito.5+data$theta_C_mosquito.6+data$theta_C_mosquito.7+data$theta_C_mosquito.8)/4
dattr3<-data.frame(x,y,log_mu_ooc_CMeanBites3,log_phi_ooc_CMeanB3,ztemp,zBtemp,bites)

x = log_muB4
y = log_phiB4; bites <- rep(4,length(x))
ztemp =(data$theta_C.9+data$theta_C.10+data$theta_C.11+data$theta_C.12)/4
zBtemp =(data$theta_C_mosquito.9+data$theta_C_mosquito.10+data$theta_C_mosquito.11+data$theta_C_mosquito.12)/4
dattr4<-data.frame(x,y,log_mu_ooc_CMeanBites4,log_phi_ooc_CMeanB4,ztemp,zBtemp,bites)

x = log_muB5
y = log_phiB5; bites <- rep(5,length(x))
ztemp =(data$theta_C.13+data$theta_C.14+data$theta_C.15+data$theta_C.16)/4
zBtemp =(data$theta_C_mosquito.13+data$theta_C_mosquito.14+data$theta_C_mosquito.15+data$theta_C_mosquito.16)/4
dattr5<-data.frame(x,y,log_mu_ooc_CMeanBites5,log_phi_ooc_CMeanB5,ztemp,zBtemp,bites)

dattrTEST <-rbind(dattr2,dattr3,dattr4,dattr5)
dattr <- as.data.frame(dattrTEST)

#contour(interp(dattr$x, dattr$y, dattr$ztemp),color.palette = cm.colors,duplicate=FALSE)

my.matrix5  <- interp(dattr2$x, dattr2$y, dattr2$ztemp)
my.matrix6  <- interp(dattr3$x, dattr3$y, dattr3$ztemp)
my.matrix7  <- interp(dattr4$x, dattr4$y, dattr4$ztemp)
my.matrix8  <- interp(dattr5$x, dattr5$y, dattr5$ztemp)

my.matrix5B  <- interp(dattr2$x, dattr2$y, dattr2$zBtemp)
my.matrix6B  <- interp(dattr3$x, dattr3$y, dattr3$zBtemp)
my.matrix7B  <- interp(dattr4$x, dattr4$y, dattr4$zBtemp)
my.matrix8B  <- interp(dattr5$x, dattr5$y, dattr5$zBtemp)

my.matrix5C  <- interp(dattr2$log_mu_ooc_CMeanBites2, dattr2$log_phi_ooc_CMeanB2, dattr2$ztemp)
my.matrix6C  <- interp(dattr3$log_mu_ooc_CMeanBites3, dattr3$log_phi_ooc_CMeanB3, dattr3$ztemp)
my.matrix7C  <- interp(dattr4$log_mu_ooc_CMeanBites4, dattr4$log_phi_ooc_CMeanB4, dattr4$ztemp)
my.matrix8C  <- interp(dattr5$log_mu_ooc_CMeanBites5, dattr5$log_phi_ooc_CMeanB5, dattr5$ztemp)

my.matrix5D  <- interp(dattr2$log_mu_ooc_CMeanBites2, dattr2$log_phi_ooc_CMeanB2, dattr2$zBtemp)
my.matrix6D  <- interp(dattr3$log_mu_ooc_CMeanBites3, dattr3$log_phi_ooc_CMeanB3, dattr3$zBtemp)
my.matrix7D  <- interp(dattr4$log_mu_ooc_CMeanBites4, dattr4$log_phi_ooc_CMeanB4, dattr4$zBtemp)
my.matrix8D  <- interp(dattr5$log_mu_ooc_CMeanBites5, dattr5$log_phi_ooc_CMeanB5, dattr5$zBtemp)

filled.contour(my.matrix5, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Controls Bites 2 (Theta mouse)")
filled.contour(my.matrix6, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Controls Bites 3 (Theta mouse)")
filled.contour(my.matrix7, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Controls Bites 4 (Theta mouse)")
filled.contour(my.matrix8, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Controls Bites 5 (Theta mouse)")

filled.contour(my.matrix5B, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Controls Bites 2 (Theta mosquito)")
filled.contour(my.matrix6B, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Controls Bites 3 (Theta mosquito)")
filled.contour(my.matrix7B, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Controls Bites 4 (Theta mosquito)")
filled.contour(my.matrix8B, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_sporozoites",ylab="phi_sporozoites",main="Controls Bites 5 (Theta mosquito)")

filled.contour(my.matrix5C, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Controls Bites 2 (Theta mouse)")
filled.contour(my.matrix6C, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Controls Bites 3 (Theta mouse)")
filled.contour(my.matrix7C, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Controls Bites 4 (Theta mouse)")
filled.contour(my.matrix8C, nlevels=12, color=my.heat.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Controls Bites 5 (Theta mouse)")

filled.contour(my.matrix5D, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Controls Bites 2 (Theta mosquito)")
filled.contour(my.matrix6D, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Controls Bites 3 (Theta mosquito)")
filled.contour(my.matrix7D, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Controls Bites 4 (Theta mosquito)")
filled.contour(my.matrix8D, nlevels=12, color=my.topo.colors,xlim=c(-14,6),ylim=c(-5,1),zlim=c(0.1,0.9),xlab="mu_oocysts",ylab="phi_oocysts",main="Controls Bites 5 (Theta mosquito)")

#my.matC  <- interp(dattr$x, dattr$y, dattr$bites)
#filled.contour(my.matC, nlevels=5, color=my.heat.colors,xlab="mu",ylab="phi",xlim=c(-14,6),ylim=c(-5,1),zlim=c(2:5))
#filled.contour(my.matT,nlevels=5,color=my.heat.colors,xlab="mu",ylab="phi",xlim=c(-14,6),ylim=c(-5,1),zlim=c(2,3,4,5),
 #              key.title = title(main = "Bites"))


#######################################################
##
## Probability of infection
##
###################################################
plot(thet_mosq_means~thet_mo_means)
points(thet_mosq_means[1:4]~thet_mo_means[1:4],col="orange",pch=19)
points(thet_mosq_means[5:8]~thet_mo_means[5:8],col="red",pch=19)
points(thet_mosq_means[9:12]~thet_mo_means[9:12],col="purple",pch=19)
points(thet_mosq_means[13:16]~thet_mo_means[13:16],col="blue",pch=19)

points(thet_mosq_means[17:20]~thet_mo_means[17:20],col="orange",pch=20,cex=2)
points(thet_mosq_means[21:24]~thet_mo_means[21:24],col="red",pch=20,cex=2)
points(thet_mosq_means[25:28]~thet_mo_means[25:28],col="purple",pch=20,cex=2)
points(thet_mosq_means[29:32]~thet_mo_means[29:32],col="blue",pch=20,cex=2)

((mean(thet_mosq_means[1:4])-mean(thet_mosq_means[17:20]))/mean(thet_mosq_means[1:4]))
((mean(thet_mosq_means[5:8])-mean(thet_mosq_means[21:24]))/mean(thet_mosq_means[5:8]))
((mean(thet_mosq_means[12:15])-mean(thet_mosq_means[25:28]))/mean(thet_mosq_means[9:12]))
((mean(thet_mosq_means[13:16])-mean(thet_mosq_means[29:32]))/mean(thet_mosq_means[13:16]))

((mean(thet_mo_means[1:4])-mean(thet_mo_means[17:20]))/mean(thet_mo_means[1:4]))
((mean(thet_mo_means[5:8])-mean(thet_mo_means[21:24]))/mean(thet_mo_means[5:8]))
((mean(thet_mo_means[9:12])-mean(thet_mo_means[25:28]))/mean(thet_mo_means[9:12]))
((mean(thet_mo_means[13:16])-mean(thet_mo_means[29:32]))/mean(thet_mo_means[13:16]))

effectsizeint <- c(((mean(thet_mosq_means[1:4])-mean(thet_mosq_means[17:20])) +
   (mean(thet_mo_means[1:4])-mean(thet_mo_means[17:20])) ) /
  (mean(thet_mosq_means[1:4]) + mean(thet_mo_means[1:4])),
((mean(thet_mosq_means[5:8])-mean(thet_mosq_means[21:24])) +
   (mean(thet_mo_means[5:8])-mean(thet_mo_means[21:24])) ) /
  (mean(thet_mosq_means[5:8]) + mean(thet_mo_means[5:8])),
((mean(thet_mosq_means[9:12])-mean(thet_mosq_means[25:28])) +
   (mean(thet_mo_means[9:12])-mean(thet_mo_means[25:28])) ) /
  (mean(thet_mosq_means[9:12]) + mean(thet_mo_means[9:12])),
((mean(thet_mosq_means[13:16])-mean(thet_mosq_means[29:32])) +
   (mean(thet_mo_means[13:16])-mean(thet_mo_means[29:32])) ) /
  (mean(thet_mosq_means[13:16]) + mean(thet_mo_means[13:16])))

##
## OR
allestimates <- ((thet_mosq_means[1:16] - thet_mosq_means[17:32]) +
                   (thet_mo_means[1:16] - thet_mo_means[17:32]))/
  (thet_mosq_means[1:16] + thet_mo_means[1:16])

mean(allestimates[1:4]);max(allestimates[1:4]);min(allestimates[1:4])
mean(allestimates[5:8]);max(allestimates[5:8]);min(allestimates[5:8])
mean(allestimates[9:12]);max(allestimates[9:12]);min(allestimates[9:12])
mean(allestimates[13:16]);max(allestimates[13:16]);min(allestimates[13:16])

effectsizeint <- c(mean(allestimates[1:4]),mean(allestimates[5:8]),
                   mean(allestimates[9:12]),mean(allestimates[13:16]))
#######
###
##     EFFECT SIZE
#
##   From the Chain-binomial model we get
####
########

plot(c(67,62.2,38.6,22.7,17.8)~c(1,2,3,4,5),
     ylim=c(-60,100),ylab="Effect size",cex=2,
     xlim=c(1,5.5),xlab="Number of bites",cex.lab=2)##effect size for 1,2,3,4,5 bites using chain binomial
segments(x0=1,y0=49.5,x1=1,y1=85)
segments(x0=2,y0=51.8,x1=2,y1=70.8)
segments(x0=3,y0=32.6,x1=3,y1=43.2)
segments(x0=4,y0=17.4,x1=4,y1=27.9)
segments(x0=5,y0=12.1,x1=5,y1=22.6)

###
##
#
##From the intensity model we get:
points(effectsizeint*100~c(2.1,3.1,4.1,5.1),pch=20,cex=2)

segments(x0=2.1,y0=100*max(allestimates[1:4]),x1=2.1,y1=100*min(allestimates[1:4]),lty=2)
segments(x0=3.1,y0=100*max(allestimates[5:8]),x1=3.1,y1=100*min(allestimates[5:8]),lty=2)
segments(x0=4.1,y0=100*max(allestimates[9:12]),x1=4.1,y1=100*min(allestimates[9:12]),lty=2)
segments(x0=5.1,y0=100*max(allestimates[13:16]),x1=5.1,y1=100*min(allestimates[13:16]),lty=2)

segments(x0=0,x1=5.5,y0=0,y1=0,lty=2,col="grey")


#######
###
##     Prob infection MOSQUITO TO MOUSE
#
##   From the Chain-binomial model we get
####
########

plot(c(99,99,99,76.9,73.5)/100~c(1,2,3,4,5),
     ylim=c(0,1),ylab="Per bite in probability of infection (mosq to mouse)",cex=2,
     xlim=c(1,5.5),xlab="Number of bites",cex.lab=1.2)##effect size for 1,2,3,4,5 bites using chain binomial
segments(x0=1,y0=0.784,x1=1,y1=1)
segments(x0=2,y0=0.895,x1=2,y1=1)
segments(x0=3,y0=0.89,x1=3,y1=1)
segments(x0=4,y0=0.647,x1=4,y1=0.90)
segments(x0=5,y0=0.647,x1=5,y1=0.837)

chbin_pr_mouse <- c(99,99,99,76.9,73.5)/100
n_bites <- c(1,2,3,4,5)

lm(chbin_pr_mouse~n_bites)

log.binom<-function(p.vec){
  
  a<-p.vec[1]
  b<-p.vec[2]
  
  pred1a<- ((exp(a + b * c(1:5))) / (1 + exp(a + b * c(1:5))) ) 
  prev1<-chbin_pr_mouse
  
  loglik1a<- prev1* log((pred1a)+0.00001)+(1-prev1)*log(1-((pred1a)-0.00001))
  -sum(loglik1a,  na.rm=T)
}
n.param<-2
logmod<-optim(c(0,0),log.binom,method="L-BFGS-B",lower=c(-10,-10),upper=c(100,10))
logmod
nc<-seq(0,5,0.01)
pred<-((exp(logmod$par[1] + logmod$par[2] * nc)) / (1 + exp(logmod$par[1] + logmod$par[2] * nc)) )
lines(nc,pred,lwd=2,lty=2,col="black")


###
##
#
##From the intensity model we get:

probtomouseall <- c((thet_mo_means[1]+thet_mo_means[2]+thet_mo_means[3]+thet_mo_means[4]+
                     thet_mo_means[17]+thet_mo_means[18]+thet_mo_means[19]+thet_mo_means[20])/8,
                  (thet_mo_means[5]+thet_mo_means[6]+thet_mo_means[7]+thet_mo_means[8]+
                     thet_mo_means[21]+thet_mo_means[22]+thet_mo_means[23]+thet_mo_means[24])/8,
                  (thet_mo_means[9]+thet_mo_means[10]+thet_mo_means[11]+thet_mo_means[12]+
                     thet_mo_means[25]+thet_mo_means[26]+thet_mo_means[27]+thet_mo_means[28])/8,
                  (thet_mo_means[13]+thet_mo_means[14]+thet_mo_means[15]+thet_mo_means[16]+
                     thet_mo_means[29]+thet_mo_means[30]+thet_mo_means[31]+thet_mo_means[31])/8)

#probtomouseall <- c((thet_mo_means[17]+thet_mo_means[18]+thet_mo_means[19]+thet_mo_means[20])/4,
#                    (thet_mo_means[21]+thet_mo_means[22]+thet_mo_means[23]+thet_mo_means[24])/4,
#                    (thet_mo_means[25]+thet_mo_means[26]+thet_mo_means[27]+thet_mo_means[28])/4,
#                    (thet_mo_means[29]+thet_mo_means[30]+thet_mo_means[31]+thet_mo_means[31])/4)

probtomouse  <- probtomouseall/c(2,3,4,5)
                 
par(las=2)
boxplot(c(thet_mo_means[1:4]/c(2,2,2,2)),
        c(thet_mo_means[17:20]/c(2,2,2,2)),
        c(thet_mo_means[5:8]/c(3,3,3,3)),
        c(thet_mo_means[21:24]/c(3,3,3,3)),
        c(thet_mo_means[9:12]/c(4,4,4,4)),
        c(thet_mo_means[25:28]/c(4,4,4,4)),
        c(thet_mo_means[13:16]/c(5,5,5,5)),
        c(thet_mo_means[29:32]/c(5,5,5,5)),
        col=c("aquamarine4","bisque4"),xaxt="n",
        frame=F,ylab="Per bite probability of infection (mosquito to mouse)",
        ylim=c(0,0.4))
par(las=1)
axis(1,at=c(1.5,3.5,5.5,7.5),labels=c(2,3,4,5))
      
theta_mo_controls <- c(thet_mo_means[1:4]/c(2,2,2,2),
thet_mo_means[5:8]/c(3,3,3,3),
thet_mo_means[9:12]/c(4,4,4,4),
thet_mo_means[13:16]/c(5,5,5,5))

theta_mo_controls <- cbind(theta_mo_controls[1:4],
                           theta_mo_controls[5:8],
                           theta_mo_controls[9:12],
                           theta_mo_controls[13:16])
t(theta_mo_controls)
theta_mo_treats <- c(mean(thet_mo_means[17:20]/c(2,2,2,2)),
mean(thet_mo_means[21:24]/c(3,3,3,3)),
mean(thet_mo_means[25:28]/c(4,4,4,4)),
mean(thet_mo_means[29:32]/c(5,5,5,5)))


datalm <- list(N=4,
               y=c(theta_mo_controls[3,]),
               x=c(2,3,4,5))
test1a <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to paralinearfit.stan", data=datalm,
               iter=1000, chains=4)

print(test1a)
print(waic(test1a))

params2 = extract(test1a);names(params2)
#rstan::traceplot(test2, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,5,0.1)
pred<-(mean(params2$alpha[501:1000]) + mean(params2$beta[501:1000]) * nc) 
par(mar=c(5,5,5,5))
par(las=1)
y=theta_mo_controls[3,]
x=c(2,3,4,5)
points(y~x,cex.lab=1.5,bty="n",xlim=c(0,5),yaxt="n",
     ylab="Gametocytemia (%)",xlab="Parasitemia (%)",ylim=c(0,1))
par(las=2);axis(2,at=seq(0,1,0.5),labels=c(0,0.5,1.0))

lines(nc,pred,lwd=2,lty=2,col="red")
alpha <- mean(params2$alpha)
beta <- mean(params2$beta)
sigma <- mean(params2$sigma)
x <- seq(0,max(spors$Parasitemia),0.1)
y <- pred

e <- extract(test2, pars = c("alpha", "beta", "sigma"))

for(i in seq_along(e[[1]])) {
  lines(x, (e[[1]][i] + e[[2]][i] * x), col = "#00000008")
}
lines(nc,pred,lwd=2,lty=2,col="red")
points(gametORIGmean[1:20]~parasORIGmean[1:20],pch=19)

#######
###
##     Prob infection MOUSE TO MOSQUITO
#
##   From the Chain-binomial model we get
####
########
prob_tomouse_chbin <- c(49.1,84.6,99,46.5,
                        30.7,62.7,51.7,27.7,
                        61.6,19.9,35.5,26.1,
                        35.9,41.1,42.8,27.2,
                        56.8,48.4,35,30.6)

prob_tomosqu_chbin <- c(99,55.6,57.1,85.5,
                        83.2,50.8,70.4,80.6,
                        73.9,78.6,73.7,90.6,
                        96.6,80.7,77.5,95.6,
                        65.0,76.2,78.8,70)


plot(prob_tomouse_chbin/100~c(rep(1,5),rep(2,5),rep(3,5),rep(4,5)),col="blue",
       ylim=c(0,1),ylab="Per bite probability of infection (mouse to mosquito)",cex=1.2,
       xlim=c(1,5.5),xlab="Number of bites",cex.lab=1.2)##effect size for 1,2,3,4,5 bites using chain binomial

par(las=2)
boxplot(prob_tomouse_chbin[1:4]/100,NA,NA,
        prob_tomouse_chbin[5:8]/100,thet_mosq_means[1:4],thet_mosq_means[17:20],
        prob_tomouse_chbin[9:12]/100,thet_mosq_means[5:8],thet_mosq_means[21:24],
        prob_tomouse_chbin[13:16]/100,thet_mosq_means[9:12],thet_mosq_means[25:28],
        prob_tomouse_chbin[17:20]/100,thet_mosq_means[13:16],thet_mosq_means[29:32],
        col=c("red","blue","green"),
        ylim=c(0,1),ylab="Per bite probability of infection (mouse to mosquito)",cex.lab=1.2,
        xaxt="n",xlab="Number of bites",frame=F
        )
axis(1,at=c(2,5,8,11,14),labels=c(1,2,3,4,5),par(las=1))


#plot(prob_tomosqu_chbin/100~c(rep(1,5),rep(2,5),rep(3,5),rep(4,5)),col="blue",
#     ylim=c(0,1),ylab="Per bite probability of infection (mosquito to mouse)",cex=1.2,
#     xlim=c(1,5.5),xlab="Number of bites",cex.lab=1.2)##effect size for 1,2,3,4,5 bites using chain binomial

par(las=2)
boxplot(prob_tomosqu_chbin[1:4]/100,NA,NA,
        prob_tomosqu_chbin[5:8]/200,thet_mo_means[1:4]/2,thet_mo_means[17:20]/2,
        prob_tomosqu_chbin[9:12]/300,thet_mo_means[5:8]/3,thet_mo_means[21:24]/3,
        prob_tomosqu_chbin[13:16]/400,thet_mo_means[9:12]/4,thet_mo_means[25:28]/4,
        prob_tomosqu_chbin[17:20]/500,thet_mo_means[13:16]/5,thet_mo_means[29:32]/5,
        col=c("red","blue","green"),
        ylim=c(0,1),ylab="Per bite probability of infection (mosquito to mouse)",cex.lab=1.2,
        xaxt="n",xlab="Number of bites",frame=F
)
axis(1,at=c(2,5,8,11,14),labels=c(1,2,3,4,5),par(las=1))

par(mfrow=c(2,1))
par(las=2)
boxplot(prob_tomouse_chbin[1:4]/100,NA,NA,
        prob_tomouse_chbin[5:8]/200,stack(data[,284:287])$values/2,stack(data[,300:303])$values/2,
        prob_tomouse_chbin[9:12]/300,stack(data[,288:291])$values/3,stack(data[,304:307])$values/3,
        prob_tomouse_chbin[13:16]/400,stack(data[,292:295])$values/4,stack(data[,308:311])$values/4,
        prob_tomouse_chbin[17:20]/500,stack(data[,296:299])$values/5,stack(data[,312:315])$values/5,
        col=c("red","blue","green"),
        ylim=c(0,1),ylab="Per bite probability of infection (mouse to mosquito)",cex.lab=1.2,
        xaxt="n",xlab="Number of bites",frame=F
)
axis(1,at=c(2,5,8,11,14),labels=c(1,2,3,4,5),par(las=1))

par(las=2)
boxplot(prob_tomosqu_chbin[1:4]/100,NA,NA,
        prob_tomosqu_chbin[5:8]/200,stack(data[,252:255])$values/2,stack(data[,268:271])$values/2,
        prob_tomosqu_chbin[9:12]/300,stack(data[,256:259])$values/3,stack(data[,272:275])$values/3,
        prob_tomosqu_chbin[13:16]/400,stack(data[,260:263])$values/4,stack(data[,276:279])$values/4,
        prob_tomosqu_chbin[17:20]/500,stack(data[,264:267])$values/5,stack(data[,280:283])$values/5,
        col=c("red","blue","green"),
        ylim=c(0,1),ylab="Per bite probability of infection (mosquito to mouse)",cex.lab=1.2,
        xaxt="n",xlab="Number of bites",frame=F
)
axis(1,at=c(2,5,8,11,14),labels=c(1,2,3,4,5),par(las=1))

chbin <- c(mean(prob_tomouse_chbin[1:4]/100),mean(prob_tomouse_chbin[5:8]/200),
           mean(prob_tomouse_chbin[9:12]/300),mean(prob_tomouse_chbin[13:16]/400),
           mean(prob_tomouse_chbin[17:20]/500))
yint <- c(mean(thet_mo_means[1:4]/2),mean(thet_mo_means[5:8]/3),mean(thet_mo_means[9:12]/4),
          mean(thet_mo_means[13:16]/5))

chbinmosq <- c(mean(prob_tomosqu_chbin[1:4]/100),mean(prob_tomosqu_chbin[5:8]/200),
           mean(prob_tomosqu_chbin[9:12]/300),mean(prob_tomosqu_chbin[13:16]/400),
           mean(prob_tomosqu_chbin[17:20]/500))
yintmosq <- c(mean(thet_mosq_means[1:4]/2),mean(thet_mosq_means[5:8]/3),mean(thet_mosq_means[9:12]/4),
          mean(thet_mosq_means[13:16]/5))

x1 <- c(1,2,3,4,5);x2 <- c(2,3,4,5)

thetadat <- list(N=5,
                x=x1,
                y=chbin)
thetadat2 <- list(N=4,
                 x=x2,
                 y=yint)
thetadat3 <- list(N=5,
                  x=x1,
                  y=chbinmosq)
thetadat4 <- list(N=4,
                  x=x2,
                  y=yintmosq)
test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan.stan", data=thetadat,
              iter=1000, chains=4)
test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan.stan", data=thetadat2,
              iter=1000, chains=4)
test3 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan.stan", data=thetadat3,
              iter=1000, chains=4)
test4 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to paralinearfit.stan", data=thetadat4,
              iter=1000, chains=4)

print(test1);print(test2);print(test3);print(test4)
print(waic(test4))

params = extract(test1);names(params)
params2 = extract(test2);names(params2)
params3 = extract(test3);names(params3)
params4 = extract(test4);names(params4)

rstan::traceplot(test1, inc_warmup = FALSE)
rstan::traceplot(test2, inc_warmup = FALSE)
rstan::traceplot(test3, inc_warmup = FALSE)
rstan::traceplot(test4, inc_warmup = FALSE)

nc<-seq(1,5,0.1)
pred1<- 1/(mean(params$alpha[501:1000]) + nc * exp(-mean(params$beta[501:1000])))^ mean(params$sigma[501:1000])
pred2<- 1/(mean(params2$alpha[501:1000]) + nc * exp(-mean(params2$beta[501:1000])))^ mean(params2$sigma[501:1000])
#pred2b<-(mean(params2$alpha[501:1000]) + mean(params2$beta[501:1000]) * nc) 
pred3<- 1/(mean(params3$alpha[501:1000]) + nc * exp(-mean(params3$beta[501:1000])))^ mean(params3$sigma[501:1000])
#pred4<- 1/(mean(params4$alpha[501:1000]) + nc * exp(-mean(params4$beta[501:1000])))^ mean(params4$sigma[501:1000])
pred4b<-(mean(params4$alpha[501:1000]) + mean(params4$beta[501:1000]) * nc) 

par(mar=c(5,5,5,5))
plot(chbin~x1,cex.lab=1,bty="n",
     ylab="Theta to mouse chain binomial",xlab="Number of Bites",ylim=c(0,1))

points(yint~x2,pch=20)

lines(nc,pred1,lwd=2,lty=2,col="red")
lines(nc,pred2,lwd=2,lty=2,col="blue")

x <- seq(1,5,0.1)
y <- pred1

e <- extract(test1, pars = c("alpha", "beta", "sigma"))

for(i in seq_along(e[[1]])) {
  lines(x, 1/(e[[1]][i] + x * exp(-e[[2]][i]))^e[[3]][i], col = "#00000008")
}
lines(nc,pred1,lwd=2,lty=2,col="red")
points(x1,chbin,col="red")


x <- seq(1,5,0.1)
y <- pred2

e <- extract(test2, pars = c("alpha", "beta", "sigma"))

for(i in seq_along(e[[1]])) {
  lines(x, 1/(e[[1]][i] + x * exp(-e[[2]][i]))^e[[3]][i], col = "#00000008")
}

lines(nc,pred2,lwd=2,lty=2,col="blue")
points(x2,yint,col="blue",pch=20)


plot(chbinmosq~x1,cex.lab=1.2,bty="n",
     ylab="Theta to mosquito chain binomial",xlab="Number of Bites",ylim=c(0,1))

points(yintmosq~x2,pch=20)

lines(nc,pred3,lwd=2,lty=2,col="red")
lines(nc,pred4b,lwd=2,lty=2,col="blue")

x <- seq(1,5,0.1)
y <- pred3

e <- extract(test3, pars = c("alpha", "beta", "sigma"))

for(i in seq_along(e[[1]])) {
  lines(x, 1/(e[[1]][i] + x * exp(-e[[2]][i]))^e[[3]][i], col = "#00000008")
}
lines(nc,pred3,lwd=2,lty=2,col="red")
points(x1,chbinmosq,col="red")

x <- seq(1,5,0.1)
y <- pred4b

e <- extract(test4, pars = c("alpha", "beta", "sigma"))

for(i in seq_along(e[[1]])) {
  #lines(x, 1/(e[[1]][i] + x * exp(-e[[2]][i]))^e[[3]][i], col = "#00000008")
  lines(x, (e[[1]][i] + e[[2]][i] * x), col = "#00000003")
}
lines(nc,pred4b,lwd=2,lty=2,col="blue")
points(x2,yintmosq,col="blue",pch=20)

summary(lm(yintmosq~x2))
