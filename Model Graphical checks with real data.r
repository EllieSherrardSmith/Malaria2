library(nlme)
library(rstan)
library(MASS)
library(boot)
library(coda)
library(R2OpenBUGS)
library(ggplot2) 
library("Rlab")
library(contrast)
#install.packages('devtools')
#library(devtools)
#source_url("https://github.com/stan-dev/shinystan/raw/develop/install_shinystan.R")
#install_shinystan()
library(shinyStan)
library(rmngb)

###################################################################################
## 
##  ##         ##       ########      ##                   ###     ##  ##  ########
##  ###      ##  ##        ##       ##  ##               ##   ##   ##  ##     ##
##  ## #    ##    ##       ##      ##    ##             ##     ##  ##  ##     ##
##  ##  #   ########       ##      ########             ##     ##  ##  ##     ## 
##  ##  #   ##    ##       ##     ##     ##             ##     ##  ##  ##     ##
##  ## #   ##      ##      ##    ##       ##             ##   ##   ##  ##     ##
##  ###   ##        ##     ##   ##         ##              ###      ####      ##
##  
###################################################################################

dat<- read.csv("C:\\Users\\Ellie\\Documents\\RStudioProjects\\TBI_testing_model\\mouse_to_mouse\\output_prepped.csv",header=TRUE)
names(dat)

###########################
##
##
## Sporozoite data
##
par(mfrow=c(2,3))
par(las=2)
boxplot(dat$"sporo_count_C_ppc.1.1",dat$"sporo_count_T_ppc.1.1",
        dat$"sporo_count_C_ppc.1.2",dat$"sporo_count_T_ppc.1.2",
        dat$"sporo_count_C_ppc.1.3",dat$"sporo_count_T_ppc.1.3",
        dat$"sporo_count_C_ppc.1.4",dat$"sporo_count_T_ppc.1.4",
        dat$"sporo_count_C_ppc.1.5",dat$"sporo_count_T_ppc.1.5",
        col=c("aquamarine","violet"),frame=FALSE,main="1 Bite: Round 1",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[1,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[1,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)

par(las=2)
boxplot(dat$"sporo_count_C_ppc.2.1",dat$"sporo_count_T_ppc.2.1",
        dat$"sporo_count_C_ppc.2.2",dat$"sporo_count_T_ppc.2.2",
        dat$"sporo_count_C_ppc.2.3",dat$"sporo_count_T_ppc.2.3",
        dat$"sporo_count_C_ppc.2.4",dat$"sporo_count_T_ppc.2.4",
        dat$"sporo_count_C_ppc.2.5",dat$"sporo_count_T_ppc.2.5",
        col=c("aquamarine","violet"),frame=FALSE,main="1 Bite: Round 2",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[2,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[2,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)

par(las=2)
boxplot(dat$"sporo_count_C_ppc.3.1",dat$"sporo_count_T_ppc.3.1",
        dat$"sporo_count_C_ppc.3.2",dat$"sporo_count_T_ppc.3.2",
        dat$"sporo_count_C_ppc.3.3",dat$"sporo_count_T_ppc.3.3",
        dat$"sporo_count_C_ppc.3.4",dat$"sporo_count_T_ppc.3.4",
        dat$"sporo_count_C_ppc.3.5",dat$"sporo_count_T_ppc.3.5",
        col=c("aquamarine","violet"),frame=FALSE,main="1 Bite: Round 3",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[3,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[3,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)

par(las=2)
boxplot(dat$"sporo_count_C_ppc.4.1",dat$"sporo_count_T_ppc.4.1",
        dat$"sporo_count_C_ppc.4.2",dat$"sporo_count_T_ppc.4.2",
        dat$"sporo_count_C_ppc.4.3",dat$"sporo_count_T_ppc.4.3",
        dat$"sporo_count_C_ppc.4.4",dat$"sporo_count_T_ppc.4.4",
        dat$"sporo_count_C_ppc.4.5",dat$"sporo_count_T_ppc.4.5",
        col=c("aquamarine","violet"),frame=FALSE,main="2 Bites: Round 1",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[4,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[4,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)


par(las=2)
boxplot(dat$"sporo_count_C_ppc.5.1",dat$"sporo_count_T_ppc.5.1",
        dat$"sporo_count_C_ppc.5.2",dat$"sporo_count_T_ppc.5.2",
        dat$"sporo_count_C_ppc.5.3",dat$"sporo_count_T_ppc.5.3",
        dat$"sporo_count_C_ppc.5.4",dat$"sporo_count_T_ppc.5.4",
        dat$"sporo_count_C_ppc.5.5",dat$"sporo_count_T_ppc.5.5",
        col=c("aquamarine","violet"),frame=FALSE,main="2 Bites: Round 2",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[5,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[5,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)


par(las=2)
boxplot(dat$"sporo_count_C_ppc.6.1",dat$"sporo_count_T_ppc.6.1",
        dat$"sporo_count_C_ppc.6.2",dat$"sporo_count_T_ppc.6.2",
        dat$"sporo_count_C_ppc.6.3",dat$"sporo_count_T_ppc.6.3",
        dat$"sporo_count_C_ppc.6.4",dat$"sporo_count_T_ppc.6.4",
        dat$"sporo_count_C_ppc.6.5",dat$"sporo_count_T_ppc.6.5",
        col=c("aquamarine","violet"),frame=FALSE,main="2 Bites: Round 3",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[6,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[6,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)

par(las=2)
boxplot(dat$"sporo_count_C_ppc.7.1",dat$"sporo_count_T_ppc.7.1",
        dat$"sporo_count_C_ppc.7.2",dat$"sporo_count_T_ppc.7.2",
        dat$"sporo_count_C_ppc.7.3",dat$"sporo_count_T_ppc.7.3",
        dat$"sporo_count_C_ppc.7.4",dat$"sporo_count_T_ppc.7.4",
        dat$"sporo_count_C_ppc.7.5",dat$"sporo_count_T_ppc.7.5",
        col=c("aquamarine","violet"),frame=FALSE,main="3 Bites: Round 1",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[7,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[7,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)
par(las=2)
boxplot(dat$"sporo_count_C_ppc.8.1",dat$"sporo_count_T_ppc.8.1",
        dat$"sporo_count_C_ppc.8.2",dat$"sporo_count_T_ppc.8.2",
        dat$"sporo_count_C_ppc.8.3",dat$"sporo_count_T_ppc.8.3",
        dat$"sporo_count_C_ppc.8.4",dat$"sporo_count_T_ppc.8.4",
        dat$"sporo_count_C_ppc.8.5",dat$"sporo_count_T_ppc.8.5",
        col=c("aquamarine","violet"),frame=FALSE,main="3 Bites: Round 2",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[8,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[8,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)
par(las=2)
boxplot(dat$"sporo_count_C_ppc.9.1",dat$"sporo_count_T_ppc.9.1",
        dat$"sporo_count_C_ppc.9.2",dat$"sporo_count_T_ppc.9.2",
        dat$"sporo_count_C_ppc.9.3",dat$"sporo_count_T_ppc.9.3",
        dat$"sporo_count_C_ppc.9.4",dat$"sporo_count_T_ppc.9.4",
        dat$"sporo_count_C_ppc.9.5",dat$"sporo_count_T_ppc.9.5",
        col=c("aquamarine","violet"),frame=FALSE,main="3 Bites: Round 3",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[9,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[9,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)

par(las=2)
boxplot(dat$"sporo_count_C_ppc.10.1",dat$"sporo_count_T_ppc.10.1",
        dat$"sporo_count_C_ppc.10.2",dat$"sporo_count_T_ppc.10.2",
        dat$"sporo_count_C_ppc.10.3",dat$"sporo_count_T_ppc.10.3",
        dat$"sporo_count_C_ppc.10.4",dat$"sporo_count_T_ppc.10.4",
        dat$"sporo_count_C_ppc.10.5",dat$"sporo_count_T_ppc.10.5",
        col=c("aquamarine","violet"),frame=FALSE,main="4 Bites: Round 1",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[10,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[10,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)
par(las=2)
boxplot(dat$"sporo_count_C_ppc.11.1",dat$"sporo_count_T_ppc.11.1",
        dat$"sporo_count_C_ppc.11.2",dat$"sporo_count_T_ppc.11.2",
        dat$"sporo_count_C_ppc.11.3",dat$"sporo_count_T_ppc.11.3",
        dat$"sporo_count_C_ppc.11.4",dat$"sporo_count_T_ppc.11.4",
        dat$"sporo_count_C_ppc.11.5",dat$"sporo_count_T_ppc.11.5",
        col=c("aquamarine","violet"),frame=FALSE,main="4 Bites: Round 2",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[11,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[11,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)
par(las=2)
boxplot(dat$"sporo_count_C_ppc.12.1",dat$"sporo_count_T_ppc.12.1",
        dat$"sporo_count_C_ppc.12.2",dat$"sporo_count_T_ppc.12.2",
        dat$"sporo_count_C_ppc.12.3",dat$"sporo_count_T_ppc.12.3",
        dat$"sporo_count_C_ppc.12.4",dat$"sporo_count_T_ppc.12.4",
        dat$"sporo_count_C_ppc.12.5",dat$"sporo_count_T_ppc.12.5",
        col=c("aquamarine","violet"),frame=FALSE,main="4 Bites: Round 3",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[12,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[12,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)


par(las=2)
boxplot(dat$"sporo_count_C_ppc.13.1",dat$"sporo_count_T_ppc.13.1",
        dat$"sporo_count_C_ppc.13.2",dat$"sporo_count_T_ppc.13.2",
        dat$"sporo_count_C_ppc.13.3",dat$"sporo_count_T_ppc.13.3",
        dat$"sporo_count_C_ppc.13.4",dat$"sporo_count_T_ppc.13.4",
        dat$"sporo_count_C_ppc.13.5",dat$"sporo_count_T_ppc.13.5",
        col=c("aquamarine","violet"),frame=FALSE,main="5 Bites: Round 1",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[13,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[13,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)
par(las=2)
boxplot(dat$"sporo_count_C_ppc.14.1",dat$"sporo_count_T_ppc.14.1",
        dat$"sporo_count_C_ppc.14.2",dat$"sporo_count_T_ppc.14.2",
        dat$"sporo_count_C_ppc.14.3",dat$"sporo_count_T_ppc.14.3",
        dat$"sporo_count_C_ppc.14.4",dat$"sporo_count_T_ppc.14.4",
        dat$"sporo_count_C_ppc.14.5",dat$"sporo_count_T_ppc.14.5",
        col=c("aquamarine","violet"),frame=FALSE,main="5 Bites: Round 2",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[14,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[14,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)
par(las=2)
boxplot(dat$"sporo_count_C_ppc.15.1",dat$"sporo_count_T_ppc.15.1",
        dat$"sporo_count_C_ppc.15.2",dat$"sporo_count_T_ppc.15.2",
        dat$"sporo_count_C_ppc.15.3",dat$"sporo_count_T_ppc.15.3",
        dat$"sporo_count_C_ppc.15.4",dat$"sporo_count_T_ppc.15.4",
        dat$"sporo_count_C_ppc.15.5",dat$"sporo_count_T_ppc.15.5",
        col=c("aquamarine","violet"),frame=FALSE,main="5 Bites: Round 3",
        ylab="Frequency",xlab="Number of sporozoites",xaxt="n")
axis(1,par(las=1),at=seq(1.5,9.5,2),labels=c("0","1-10","11-100","101-1000","1000+"))
points(sporo_count_C[15,]~c(1,3,5,7,9),pch=20,col="red",cex=2)
points(sporo_count_T[15,]~c(2,4,6,8,10),pch=20,col="blue",cex=2)

###########################
##
##
## Oocyst data
##

oocreal1r1 = ooc_count_C[1,]
oocreal1r2 = ooc_count_C[2,]
oocreal1r3 = ooc_count_C[3,]
oocreal2r1 = ooc_count_C[4,]
oocreal2r2 = ooc_count_C[5,]
oocreal2r3 = ooc_count_C[6,]
oocreal3r1 = ooc_count_C[7,]
oocreal3r2 = ooc_count_C[8,]
oocreal3r3 = ooc_count_C[9,]
oocreal4r1 = ooc_count_C[10,]
oocreal4r2 = ooc_count_C[11,]
oocreal4r3 = ooc_count_C[12,]
oocreal5r1 = ooc_count_C[13,]
oocreal5r2 = ooc_count_C[14,]
oocreal5r3 = ooc_count_C[15,]

oocsim1r1<-dat$"ooc_count_C_ppc.1"[1:1000]
oocsim1r2<-dat$"ooc_count_C_ppc.2"[1:1000]
oocsim1r3<-dat$"ooc_count_C_ppc.3"[1:1000]
oocsim2r1<-dat$"ooc_count_C_ppc.4"[1:1000]
oocsim2r2<-dat$"ooc_count_C_ppc.5"[1:1000]
oocsim2r3<-dat$"ooc_count_C_ppc.6"[1:1000]
oocsim3r1<-dat$"ooc_count_C_ppc.7"[1:1000]
oocsim3r2<-dat$"ooc_count_C_ppc.8"[1:1000]
oocsim3r3<-dat$"ooc_count_C_ppc.9"[1:1000]
oocsim4r1<-dat$"ooc_count_C_ppc.10"[1:1000]
oocsim4r2<-dat$"ooc_count_C_ppc.11"[1:1000]
oocsim4r3<-dat$"ooc_count_C_ppc.12"[1:1000]
oocsim5r1<-dat$"ooc_count_C_ppc.13"[1:1000]
oocsim5r2<-dat$"ooc_count_C_ppc.14"[1:1000]
oocsim5r3<-dat$"ooc_count_C_ppc.15"[1:1000]

length(oocreal1r1);length(oocsim1r1)
Oocysts.Bite1<-c(sort(oocreal1r1),sort(oocsim1r1),sort(oocreal1r2),sort(oocsim1r2),
                 sort(oocreal1r3),sort(oocsim1r3))
Data<-c(rep("Observed",24),rep("Simulated",1000),
        rep("Observed",24),rep("Simulated",1000),
        rep("Observed",24),rep("Simulated",1000))
dataEx2<-data.frame(Oocysts.Bite1,Data)

b1<- ggplot(dataEx2, aes(Oocysts.Bite1, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')

Oocysts.Bite2<-c(sort(oocreal2r1),sort(oocsim2r1),sort(oocreal2r2),sort(oocsim2r2),
                 sort(oocreal2r3),sort(oocsim2r3))
dataEx2<-data.frame(Oocysts.Bite2,Data)

b2<- ggplot(dataEx2, aes(Oocysts.Bite2, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')

Oocysts.Bite3<-c(sort(oocreal3r1),sort(oocsim3r1),sort(oocreal3r2),sort(oocsim3r2),
                 sort(oocreal3r3),sort(oocsim3r3))
dataEx3<-data.frame(Oocysts.Bite3,Data)

b3<- ggplot(dataEx3, aes(Oocysts.Bite3, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')

Oocysts.Bite4<-c(sort(oocreal4r1),sort(oocsim4r1),sort(oocreal4r2),sort(oocsim4r2),
                 sort(oocreal4r3),sort(oocsim4r3))
dataEx4<-data.frame(Oocysts.Bite4,Data)

b4<- ggplot(dataEx4, aes(Oocysts.Bite4, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')

Oocysts.Bite5<-c(sort(oocreal5r1),sort(oocsim5r1),sort(oocreal5r2),sort(oocsim5r2),
                 sort(oocreal5r3),sort(oocsim5r3))
dataEx5<-data.frame(Oocysts.Bite5,Data)

b5<- ggplot(dataEx5, aes(Oocysts.Bite5, fill = Data)) + 
  geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(b1,b2,b4,b3,b5,cols=2)


############################################################
##
##
## Parasitemia initial
##
par(mfrow=c(1,1))
par(las=2)
boxplot(dat$"para_init_count_C_ppc.1",dat$"para_init_count_T_ppc.1",
        dat$"para_init_count_C_ppc.2",dat$"para_init_count_T_ppc.2",
        dat$"para_init_count_C_ppc.3",dat$"para_init_count_T_ppc.3",
        dat$"para_init_count_C_ppc.4",dat$"para_init_count_T_ppc.4",
        dat$"para_init_count_C_ppc.5",dat$"para_init_count_T_ppc.5",        
        dat$"para_init_count_C_ppc.6",dat$"para_init_count_T_ppc.6",        
        dat$"para_init_count_C_ppc.7",dat$"para_init_count_T_ppc.7",
        dat$"para_init_count_C_ppc.8",dat$"para_init_count_T_ppc.8",        
        dat$"para_init_count_C_ppc.9",dat$"para_init_count_T_ppc.9",
        
        dat$"para_init_count_C_ppc.10",dat$"para_init_count_T_ppc.10",
        dat$"para_init_count_C_ppc.11",dat$"para_init_count_T_ppc.11",        
        dat$"para_init_count_C_ppc.12",dat$"para_init_count_T_ppc.12",
        
        dat$"para_init_count_C_ppc.13",dat$"para_init_count_T_ppc.13",
        dat$"para_init_count_C_ppc.14",dat$"para_init_count_T_ppc.14",        
        dat$"para_init_count_C_ppc.15",dat$"para_init_count_T_ppc.15",
        col=c("aquamarine","violet"),frame=FALSE,ylim=c(0,14000),
        ylab="Count",xlab="Initial Parasitemia",xaxt="n")
axis(1,par(las=1),at=seq(1.5,29.5,2),labels=c("","1 bite","",
                                              "","2 bites","",
                                              "","3 bites","",
                                              "","4 bites","",
                                              "","5 bites",""))

meansC <- meansT <- numeric(15)
for (i in 1:15){
  meansC[i] <- mean(para_init_count_C[i,])
  meansT[i] <- mean(para_init_count_T[i,])
}
points(meansC~seq(1,29,2),pch=20,col="red",cex=2)
points(meansT~seq(2,30,2),pch=20,col="blue",cex=2)


############################################################
##
##
## Parasitemia end
##

par(las=2)
boxplot(dat$"para_end_count_C_ppc.1",dat$"para_end_count_T_ppc.1",
        dat$"para_end_count_C_ppc.2",dat$"para_end_count_T_ppc.2",
        dat$"para_end_count_C_ppc.3",dat$"para_end_count_T_ppc.3",
        dat$"para_end_count_C_ppc.4",dat$"para_end_count_T_ppc.4",
        dat$"para_end_count_C_ppc.5",dat$"para_end_count_T_ppc.5",        
        dat$"para_end_count_C_ppc.6",dat$"para_end_count_T_ppc.6",        
        dat$"para_end_count_C_ppc.7",dat$"para_end_count_T_ppc.7",
        dat$"para_end_count_C_ppc.8",dat$"para_end_count_T_ppc.8",        
        dat$"para_end_count_C_ppc.9",dat$"para_end_count_T_ppc.9",
        
        dat$"para_end_count_C_ppc.10",dat$"para_end_count_T_ppc.10",
        dat$"para_end_count_C_ppc.11",dat$"para_end_count_T_ppc.11",        
        dat$"para_end_count_C_ppc.12",dat$"para_end_count_T_ppc.12",
        
        dat$"para_end_count_C_ppc.13",dat$"para_end_count_T_ppc.13",
        dat$"para_end_count_C_ppc.14",dat$"para_end_count_T_ppc.14",        
        dat$"para_end_count_C_ppc.15",dat$"para_end_count_T_ppc.15",
        col=c("aquamarine","violet"),frame=FALSE,ylim=c(0,14000),
        ylab="Count",xlab="Final Parasitemia",xaxt="n")
axis(1,par(las=1),at=seq(1.5,29.5,2),labels=c("","1 bite","",
                                              "","2 bites","",
                                              "","3 bites","",
                                              "","4 bites","",
                                              "","5 bites",""))

meansCend <- meansTend <- numeric(15)
for (i in 1:15){
  meansCend[i] <- mean(para_end_count_C[i,])
  meansTend[i] <- mean(para_end_count_T[i,])
}
points(meansCend~seq(1,29,2),pch=20,col="red",cex=2)
points(meansTend~seq(2,30,2),pch=20,col="blue",cex=2)

############################################################
###
##
##
##  Estimating Efficacy
##
###########################################################
a <- numeric(15)
##BITES 1
a[1] <- (mean(dat$"para_init_count_T_ppc.1",na.rm=T)-mean(dat$"para_end_count_T_ppc.1",na.rm=T))/mean(dat$"para_init_count_T_ppc.1",na.rm=T)-
(mean(dat$"para_init_count_C_ppc.1",na.rm=T)-mean(dat$"para_end_count_C_ppc.1",na.rm=T))/mean(dat$"para_init_count_C_ppc.1",na.rm=T)

a[2] <- (mean(dat$"para_init_count_T_ppc.2",na.rm=T)-mean(dat$"para_end_count_T_ppc.2",na.rm=T))/mean(dat$"para_init_count_T_ppc.2",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.2",na.rm=T)-mean(dat$"para_end_count_C_ppc.2",na.rm=T))/mean(dat$"para_init_count_C_ppc.2",na.rm=T)


a[3] <- (mean(dat$"para_init_count_T_ppc.3",na.rm=T)-mean(dat$"para_end_count_T_ppc.3",na.rm=T))/mean(dat$"para_init_count_T_ppc.3",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.3",na.rm=T)-mean(dat$"para_end_count_C_ppc.3",na.rm=T))/mean(dat$"para_init_count_C_ppc.3",na.rm=T)

##BITES 2
a[4] <- (mean(dat$"para_init_count_T_ppc.4",na.rm=T)-mean(dat$"para_end_count_T_ppc.4",na.rm=T))/mean(dat$"para_init_count_T_ppc.4",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.4",na.rm=T)-mean(dat$"para_end_count_C_ppc.4",na.rm=T))/mean(dat$"para_init_count_C_ppc.4",na.rm=T)

a[5] <- (mean(dat$"para_init_count_T_ppc.5",na.rm=T)-mean(dat$"para_end_count_T_ppc.5",na.rm=T))/mean(dat$"para_init_count_T_ppc.5",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.5",na.rm=T)-mean(dat$"para_end_count_C_ppc.5",na.rm=T))/mean(dat$"para_init_count_C_ppc.5",na.rm=T)

a[6] <- (mean(dat$"para_init_count_T_ppc.6",na.rm=T)-mean(dat$"para_end_count_T_ppc.6",na.rm=T))/mean(dat$"para_init_count_T_ppc.6",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.6",na.rm=T)-mean(dat$"para_end_count_C_ppc.6",na.rm=T))/mean(dat$"para_init_count_C_ppc.6",na.rm=T)

##BITES 3
a[7] <- (mean(dat$"para_init_count_T_ppc.7",na.rm=T)-mean(dat$"para_end_count_T_ppc.7",na.rm=T))/mean(dat$"para_init_count_T_ppc.7",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.7",na.rm=T)-mean(dat$"para_end_count_C_ppc.7",na.rm=T))/mean(dat$"para_init_count_C_ppc.7",na.rm=T)

a[8] <- (mean(dat$"para_init_count_T_ppc.8",na.rm=T)-mean(dat$"para_end_count_T_ppc.8",na.rm=T))/mean(dat$"para_init_count_T_ppc.8",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.8",na.rm=T)-mean(dat$"para_end_count_C_ppc.8",na.rm=T))/mean(dat$"para_init_count_C_ppc.8",na.rm=T)

a[9] <- (mean(dat$"para_init_count_T_ppc.9",na.rm=T)-mean(dat$"para_end_count_T_ppc.9",na.rm=T))/mean(dat$"para_init_count_T_ppc.9",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.9",na.rm=T)-mean(dat$"para_end_count_C_ppc.9",na.rm=T))/mean(dat$"para_init_count_C_ppc.9",na.rm=T)

##BITES 4
a[10] <- (mean(dat$"para_init_count_T_ppc.10",na.rm=T)-mean(dat$"para_end_count_T_ppc.10",na.rm=T))/mean(dat$"para_init_count_T_ppc.10",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.10",na.rm=T)-mean(dat$"para_end_count_C_ppc.10",na.rm=T))/mean(dat$"para_init_count_C_ppc.10",na.rm=T)

a[11] <- (mean(dat$"para_init_count_T_ppc.11",na.rm=T)-mean(dat$"para_end_count_T_ppc.11",na.rm=T))/mean(dat$"para_init_count_T_ppc.11",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.11",na.rm=T)-mean(dat$"para_end_count_C_ppc.11",na.rm=T))/mean(dat$"para_init_count_C_ppc.11",na.rm=T)

a[12] <- (mean(dat$"para_init_count_T_ppc.12",na.rm=T)-mean(dat$"para_end_count_T_ppc.12",na.rm=T))/mean(dat$"para_init_count_T_ppc.12",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.12",na.rm=T)-mean(dat$"para_end_count_C_ppc.12",na.rm=T))/mean(dat$"para_init_count_C_ppc.12",na.rm=T)

##BITES 5
a[13] <- (mean(dat$"para_init_count_T_ppc.13",na.rm=T)-mean(dat$"para_end_count_T_ppc.13",na.rm=T))/mean(dat$"para_init_count_T_ppc.13",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.13",na.rm=T)-mean(dat$"para_end_count_C_ppc.13",na.rm=T))/mean(dat$"para_init_count_C_ppc.13",na.rm=T)

a[14] <- (mean(dat$"para_init_count_T_ppc.14",na.rm=T)-mean(dat$"para_end_count_T_ppc.14",na.rm=T))/mean(dat$"para_init_count_T_ppc.14",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.14",na.rm=T)-mean(dat$"para_end_count_C_ppc.14",na.rm=T))/mean(dat$"para_init_count_C_ppc.14",na.rm=T)

a[15] <- (mean(dat$"para_init_count_T_ppc.15",na.rm=T)-mean(dat$"para_end_count_T_ppc.15",na.rm=T))/mean(dat$"para_init_count_T_ppc.15",na.rm=T)-
  (mean(dat$"para_init_count_C_ppc.15",na.rm=T)-mean(dat$"para_end_count_C_ppc.15",na.rm=T))/mean(dat$"para_init_count_C_ppc.15",na.rm=T)

a <- ifelse(a > 1,1,a)
A1 <- c(mean(a[1:3]),mean(a[4:6]),mean(a[7:9]),mean(a[10:12]),mean(a[13:15]))
aa <- (A1+1)/2

samp <- numeric(10000)
for (i in 1:10000) samp[i] <- mean(sample(a, replace=T), na.rm=TRUE)
quantile(samp, c(0.025,0.975))
mean(a)

data2 <- list(N=5,
              x=c(1:5),
              y=aa)
test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction_for model groupsPARA.stan", data=data2,
              iter=1000, chains=4)

print(test1)
print(waic(test1))
params = extract(test1);names(params)
#rstan::traceplot(test1, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,5,0.1)
par(mfrow=c(1,2))
pred1<-(exp(mean(params$alpha[501:1000]) - mean(params$beta[501:1000]) * nc)) / 
          (1 + exp(mean(params$alpha[501:1000])  - mean(params$beta[501:1000]) * nc)) 
par(mar=c(5,5,5,5))
plot(aa~c(1:5),cex.lab=1.5,bty="n",xaxt="n",yaxt="n",ylim=c(0.3,1),xlim=c(0,5.5),
     ylab="ATV efficacy against parasitemia in the host",xlab="Number of bites")
axis(2,par(las=2),at=seq(0.3,1,0.1),labels=seq(-0.4,1,0.2))
axis(1,par(las=1),at=seq(1,5,1),labels=seq(1,5,1))

lines(nc,pred1,lwd=2,lty=2,col="red")

e <- extract(test1, pars = c("alpha", "beta"))

for(i in seq_along(e[[1]])) {
  lines(nc, (exp(e[[1]][i] - e[[2]][i] * nc) / (1 + exp(e[[1]][i] - e[[2]][i] * nc))), col = "#00000008")
}
lines(nc,pred1,lwd=2,lty=2,col="red")


par(new=T)
boxplot(NA,a[1:3],a[4:6],a[7:9],a[10:12],a[13:15],
        xaxt="n",yaxt="n",frame=FALSE,at=c(0,0.4,1.5,2.6,3.7,4.8),
        col="aquamarine")
abline(h=0,xlim=c(0,5),lty=2,col="grey")


#ooc <- parainit <- paraend <- numeric(15)
##BITES 1
#for ( i in 1:15){
#ooc[i] <- (mean(dat[,104+i],na.rm=T)-mean(dat[,119+i],na.rm=T))/mean(dat[,104+i],na.rm=T)
#parainit[i] <- (mean(dat[,74+i],na.rm=T)-mean(dat[,89+i],na.rm=T))/mean(dat[,74+i],na.rm=T)
#paraend[i] <- (mean(dat[,284+i],na.rm=T)-mean(dat[,299+i],na.rm=T))/mean(dat[,284+i],na.rm=T)
#  }


paradiffC <- paradiffT <- numeric(15)
##BITES 1
for (i in 1:15) {
  paradiffC[i] <- (sum(ifelse(dat[,74+i]>0,1,0),na.rm=T)/length(dat[,74+i]) - sum(ifelse(dat[,284+i]>0,1,0),na.rm=T)/length(dat[,284+i])) /
                     (sum(ifelse(dat[,74+i]>0,1,0),na.rm=T)/length(dat[,74+i]))
  paradiffT[i] <- (sum(ifelse(dat[,89+i]>0,1,0),na.rm=T)/length(dat[,89+i]) - sum(ifelse(dat[,299+i]>0,1,0),na.rm=T)/length(dat[,299+i])) / 
                     (sum(ifelse(dat[,89+i]>0,1,0),na.rm=T)/length(dat[,89+i]))
  
}
b1 <- (paradiffT - paradiffC)
bb <- c(mean(b1[1:3]),mean(b1[4:6]),mean(b1[7:9]),mean(b1[10:12]),mean(b1[13:15]))


for (i in 1:10000) samp[i] <- mean(sample(b1, replace=T), na.rm=TRUE)
quantile(samp, c(0.025,0.975))
mean(b1)


dataprev <- list(N=5,
              x=c(1:5),
              y=(bb+1)/2)
test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction_for model groups.stan", data=dataprev,
              iter=1000, chains=4)

print(test2)
print(waic(test2))
params2 = extract(test2);names(params)
#rstan::traceplot(test2, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,5,0.1)
pred3<- (exp(mean(params2$alpha[501:1000]) - mean(params2$beta[501:1000]) * nc)) / (1 + exp(mean(params2$alpha[501:1000]) -mean(params2$beta[501:1000]) * nc)) 
 
par(mar=c(5,5,5,5))
plot((bb+1)/2~c(1:5),cex.lab=1.5,bty="n",xlim=c(0,5.5),xaxt="n",yaxt="n",ylim=c(0.3,1),
     ylab="ATV efficacy against prevalence in the host",xlab="Number of bites")
axis(2,par(las=2),at=seq(0.3,1,0.1),labels=seq(-0.4,1,0.2))
axis(1,par(las=1),at=seq(1,5,1),labels=seq(1,5,1))

lines(nc,pred3,lwd=2,lty=2,col="red")

e <- extract(test2, pars = c("alpha", "beta"))

for(i in seq_along(e[[1]])) {
  lines(nc, (exp(e[[1]][i] - e[[2]][i] * nc) / (1 + exp(e[[1]][i] - e[[2]][i] * nc))), col = "#00000008")
}
lines(nc,pred3,lwd=2,lty=2,col="red")

abline(h=0.5,lty=2,col="grey")

par(new=T)
boxplot(NA,b1[1:3],b1[4:6],b1[7:9],b1[10:12],b1[13:15],
        xaxt="n",yaxt="n",frame=FALSE,ylim=c(-0.6,1),at=c(0,0.4,1.5,2.6,3.7,4.8),
        col="aquamarine")

################################
##
## Probability of infection
##
################################
probtoMOUSE <- numeric (6)
for (i in 69:74){
probtoMOUSE[i-68] <- mean(dat[,i],na.rm=T)
}

mean(c(rep(0,sum(dat$"sporo_count_C_ppc.1.1",na.rm=T)),
  rep(1,sum(dat$"sporo_count_C_ppc.1.2",na.rm=T)),
    rep(2,sum(dat$"sporo_count_C_ppc.1.3",na.rm=T)),
    rep(3,sum(dat$"sporo_count_C_ppc.1.4",na.rm=T)),
    rep(4,sum(dat$"sporo_count_C_ppc.1.5",na.rm=T))))

mu_spors <- var_spors <-numeric(15)
for (i in 1:15){
mu_spors[i] <- (sum(dat[,134+i],na.rm=T)*0+
   sum(dat[,149+i],na.rm=T)*1+
   sum(dat[,164+i],na.rm=T)*2+
   sum(dat[,179+i],na.rm=T)*3+
   sum(dat[,194+i],na.rm=T)*4)/5000

var_spors[i] <- var(c(rep(0,sum(dat[,134+i],na.rm=T)),
                      rep(1,sum(dat[,149+i],na.rm=T)),
                      rep(2,sum(dat[,164+i],na.rm=T)),
                      rep(3,sum(dat[,179+i],na.rm=T)),
                      rep(4,sum(dat[,194+i],na.rm=T))))
}

phi_spor <- mu_spors^2 / (var_spors - mu_spors)


##
###
####
##### Estimating probability of infection from the outputs
####
###
##
ProbToMosq <- ProbToMouse <- numeric (30)
for (i in 1:30){
ProbToMosq[i] <- sum(ifelse(dat[,104+i] > 0,1,0),na.rm=T)/1000
ProbToMouse[i] <- sum(ifelse(dat[,284+i] > 0,1,0),na.rm=T)/1000
}

perbiteProbToMosqC <- ProbToMosq[1:15]/c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)
perbiteProbToMosqT <- ProbToMosq[16:30]/c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)

perbiteProbToMouseC <- ProbToMouse[1:15]/c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)
perbiteProbToMouseT <- ProbToMouse[16:30]/c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)

obsth2mousC <- obsth2mousT <- obsth1mosqC <- obsth1mosqT <- numeric(15)
for (i in 1:15){
obsth2mousC[i] <- sum(ifelse(para_end_count_C[i,]>0,1,0),na.rm=T)/5
obsth1mosqC[i] <- sum(ifelse(ooc_count_C[i,]>0,1,0),na.rm=T)/24

obsth2mousT[i] <- sum(ifelse(para_end_count_T[i,]>0,1,0),na.rm=T)/5
obsth1mosqT[i] <- sum(ifelse(ooc_count_T[i,]>0,1,0),na.rm=T)/24
}


th1mosq <- c(mean(perbiteProbToMosqC[1:3]),mean(perbiteProbToMosqC[4:6]),mean(perbiteProbToMosqC[7:9]),
             mean(perbiteProbToMosqC[10:12]),mean(perbiteProbToMosqC[13:15]))
th2mous <- c(mean(perbiteProbToMouseC[1:3]),mean(perbiteProbToMouseC[4:6]),mean(perbiteProbToMouseC[7:9]),
             mean(perbiteProbToMouseC[10:12]),mean(perbiteProbToMouseC[13:15]))
par(mfrow=c(2,1))
boxplot(perbiteProbToMosqC[1:3],perbiteProbToMosqT[1:3],
        perbiteProbToMosqC[4:6],perbiteProbToMosqT[4:6],
        perbiteProbToMosqC[7:9],perbiteProbToMosqT[7:9],
        perbiteProbToMosqC[10:12],perbiteProbToMosqT[10:12],
        perbiteProbToMosqC[13:15],perbiteProbToMosqT[13:15],col=c("aquamarine","violet"),
        xaxt="n",ylab="Per bite infection probability to vector",cex.lab=1.2,
        xlab="Number of bites received",frame=FALSE)
axis(1,at=seq(1.5,9.5,2),labels=c(1,2,3,4,5))
boxplot(perbiteProbToMouseC[1:3],perbiteProbToMouseT[1:3],
        perbiteProbToMouseC[4:6],perbiteProbToMouseT[4:6],
        perbiteProbToMouseC[7:9],perbiteProbToMouseT[7:9],
        perbiteProbToMouseC[10:12],perbiteProbToMouseT[10:12],
        perbiteProbToMouseC[13:15],perbiteProbToMouseT[13:15],col=c("aquamarine","violet"),
        xaxt="n",ylab="Per bite infection probability to vertebrate",cex.lab=1.2,
        xlab="Number of bites received",frame=FALSE)
axis(1,at=seq(1.5,9.5,2),labels=c(1,2,3,4,5))


x1 <- c(1,2,3,4,5)

thetadat <- list(N=5,
                 x=x1,
                 y=th2mous)

thetadat3 <- list(N=5,
                  x=x1,
                  y=th1mosq)

test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan.stan", data=thetadat,
              iter=1000, chains=4)
test3 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan.stan", data=thetadat3,
              iter=1000, chains=4)

print(test1);print(test3)


params = extract(test1);names(params)
params3 = extract(test3);names(params3)


rstan::traceplot(test1, inc_warmup = FALSE)
rstan::traceplot(test3, inc_warmup = FALSE)

nc<-seq(1,5,0.1)
pred1<- 1/(mean(params$alpha) + nc * exp(-mean(params$beta)))^ mean(params$sigma)
pred3<- 1/(mean(params3$alpha) + nc * exp(-mean(params3$beta)))^ mean(params3$sigma)

(1 / pow((alpha + x[n] * exp(-beta))
         
par(mfrow=c(2,1))
par(mar=c(5,5,5,5))
plot(th2mous~x1,cex.lab=1.2,bty="n",xaxt="n",yaxt="n",xlim=c(1,6),
     ylab="Per bite probability of infection to mouse",xlab="Number of bites",ylim=c(0,1))
axis(2,par(las=2),at=c(0,0.2,0.4,0.6,0.8,1.0),line=0,labels=c(0,0.2,0.4,0.6,0.8,1.0))
axis(1,par(las=1),at=c(1,2,3,4,5),labels=c(1,2,3,4,5))

x <- seq(1,5,0.1)
y <- pred1

e <- extract(test1, pars = c("alpha", "beta", "sigma","phi"))

for(i in seq_along(e[[1]])) {
  lines(x, 1/(e[[1]][i] + x * exp(-e[[2]][i]))^e[[3]][i], col = "#00000008")
}
lines(nc,(1/(mean(e[[1]][i]) + x * exp(-mean(e[[2]][i])))^mean(e[[3]][i]))+e[[4]][i],lwd=2,lty=2,col="red")
points(x1,th2mous,col="red",pch=20,cex=1.5)


par(new=T)
par(mar=c(5,5,5,5))

boxplot(obsth2mousC[1:3]/1,obsth2mousC[4:6]/2,obsth2mousC[7:9]/3,
        obsth2mousC[10:12]/4,obsth2mousC[13:15]/5, 
        yaxt="n",xaxt="n",ylim=c(0,1),xlim=c(1,6),frame=FALSE)

par(mar=c(5,5,5,5))
plot(th1mosq~x1,cex.lab=1.2,bty="n",xlim=c(1,6),
     ylab="Per bite probability of infection to vector",xlab="Number of Bites",ylim=c(0,1))

x <- seq(1,5,0.1)
y <- pred3

e <- extract(test3, pars = c("alpha", "beta", "sigma","phi"))

for(i in seq_along(e[[1]])) {
  lines(x, 1/(e[[1]][i] + x * exp(-e[[2]][i]))^e[[3]][i], col = "#00000008")
}
lines(nc,y,lwd=2,lty=2,col="red")
points(x1,th1mosq,col="red",pch=20,cex=1.5)

par(new=T)
par(mar=c(5,5,5,5))
boxplot(obsth1mosqC[1:3]/1,obsth1mosqC[4:6]/2,obsth1mosqC[7:9]/3,
        obsth1mosqC[10:12]/4,obsth1mosqC[13:15]/5, 
        yaxt="n",xaxt="n",ylim=c(0,1),xlim=c(1,6),frame=FALSE)
