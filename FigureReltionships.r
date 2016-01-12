library(igraph)

dev.off()

split.screen(rbind(
  c(0.01,0.23,0.3, 0.7), 
  c(0.23,0.4,0, 1),
  c(0.37,0.65,0.1, 0.5),
  c(0.37,0.65,0.5, 0.92),
c(0.66, 0.99, 0, 0.33),
c(0.66, 0.99, 0.33, 0.70),
c(0.66, 0.99, 0.68, 1)))

split.screen(rbind(
  c(0.01,0.43,0.3,0.7),
  c(0.38,0.55,0,1),
  c(0.52,1,0.1,0.5),
  c(0.52,1,0.5,0.92)))

#######################################
##
## Parasitemia and oocysts
##
########################################
screen(1) ## the first screen...
parasORIGmean <- c(parasitORIGmean,parasitATV32mean)

spordat <- list(N=40,
                x=meanoocysts,
                y=parasORIGmean)

test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan2.stan", data=spordat,
              iter=1000, chains=4)

##****setting for logisticfunction in stan2
#parameters {
#  real<lower=0> phi; // 
#    real<lower=0> beta;  // 
#    real<lower=0> alpha;
#}

print(test1)
print(waic(test1))

params1 = extract(test1);names(params1)
#rstan::traceplot(test3, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,1000,1)
pred2 <- (mean(params1$alpha) * nc) / sqrt(1 + nc^1/mean(params1$beta))


par(mar=c(4,4,0,0))
par(las=1)
plot(parasORIGmean~meanoocysts,cex.lab=1.2,bty="n",ylim=c(0,15),
     xlab="Mean oocysts",ylab="Mean parasitemia (%)",xlim=c(0,60))
par(las=2);axis(2,at=seq(0,15,5),labels=seq(0,15,5))
legend(45,15,"A  ")
lines(nc,pred2,lwd=2,lty=2,col="blue")

x <- seq(0,60,0.1)

e <- extract(test1, pars = c("alpha", "beta"))

for(i in seq_along(e[[1]])) {
  lines(x, ((e[[1]][i] * x)/sqrt(1 + x ^ 1/e[[2]][i])), col = "#00000008")
}
lines(nc,pred2,lwd=2,lty=2,col="red")
points(parasORIGmean[21:40]~meanoocysts[21:40],col="red",pch=20)
legend(0,15,legend=c("Control","Treatment"),
       col=c("black","red"),pch=c(1,19),
       bty="n",cex=1.2)
#######################################
##
## names
##
########################################
screen(2) ## the second screen...
par(mar=c(0,0,0,0))
plot(seq(0,100)~seq(0,100),xaxt="n",yaxt="n",
     pch=NA,bty="n",ylab="",xlab="")

text(50,90,"Oocysts",col="darkgrey",cex=1.2)

text(50,10,"Parasitemia",col="darkgrey",cex=1.2)

arrows(20,87, x1 = 20, y1 = 13, length = 0.25, angle = 30,
       code = 2, col = "blue", lty = 3,
       lwd = 1)
text(50,50,"Sporozoites",col="darkgrey",cex=1.2)
arrows(75,87, x1 = 75, y1 = 53, length = 0.25, angle = 30,
       code = 2, col = "blue", lty = 2,
       lwd = 2)

arrows(75,47, x1 = 75, y1 = 13, length = 0.25, angle = 30,
       code = 2, col = "blue", lty = 2,
       lwd = 2)

#######################################
##
## Sporozoites AND Parasitemia 
##
########################################
screen(3)
par(mar=c(4,4,0,0))
spordat <- list(N=200,
                y=round(spors$Parasitemia),
                x=spors$meanpermouse)
test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to paralinearfit.stan", data=spordat,
              iter=1000, chains=4)

par(las=1)
plot(spors$Parasitemia~spors$meanpermouse,cex.lab=1.2,bty="n",xlim=c(0,5),xaxt="n",
     xlab="Mean sporozoite score per mouse",ylab="Parasitemia (%)",ylim=c(0,25))
axis(1,par(las=1),at=c(0,1,2,3,4,5),lab=c(0,1,2,3,4,5))
legend(4,25,"C")
print(test2);print(waic(test2))
params = extract(test2);names(params)

x<-seq(0,4,0.1)
e <- extract(test2, pars = c("alpha", "beta"))

for(i in seq_along(e[[1]])) {
  lines(x, (e[[1]][i] + e[[2]][i] * x), col = "#00000008")
}
lines((mean(params$beta)*x + mean(params$alpha))~x,col="red",lty=2,lwd=2)
points(spors$Parasitemia[spors$Treatment==1]~spors$meanpermouse[spors$Treatment==1],col="red",pch=20)

#######################################
##
## Oocysts to Sporozoites 
##
########################################
screen(4)
par(mar=c(4,4,0,0))

spordat <- list(N=40,
                x=meanoocysts,
                y=MEANsporig)

test3 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan3.stan", data=spordat,
              iter=1000, chains=4)
##***settings for logisticfunction in stan2
#parameters {
#  real<lower=0> phi; // 
#    real<lower=0.1, upper=0.3> beta;  // 
#    real<lower=0.4, upper=0.6> alpha;
#}

print(test3)
print(waic(test3))

params3 = extract(test3);names(params3)
#rstan::traceplot(test2, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,100,1)
pred2 <- (mean(params3$alpha) * nc) / sqrt(1 + nc^1/mean(params3$beta))

par(las=1)
plot(MEANsporig~meanoocysts,cex.lab=1.2,bty="n",xlim=c(0,60),yaxt="n",
     xlab="Mean oocysts",ylab="Mean sporozoite score",ylim=c(0,3))
par(las=2);axis(2,at=seq(0,3,1),labels=seq(0,3,1))
legend(50,3,"B")
#lines(nc,pred,lwd=2,lty=2,col="red")
lines(nc,pred2,lwd=2,lty=2,col="blue")

x <- seq(0,max(meanoocysts),0.1)

e <- extract(test3, pars = c("alpha", "beta"))

for(i in seq_along(e[[1]])) {
  lines(x, ((e[[1]][i] * x)/sqrt(1 + x ^ 1/e[[2]][i])), col = "#00000008")
}
lines(nc,pred2,lwd=2,lty=2,col="red")
points(MEANsporig[21:40]~meanoocysts[21:40],pch=20,col="red")

#######################################
##
## Parasitemia to per bite probability in mosquito
##
########################################
screen(5)

modat <- list(N=32,
              x=c(parasORIGmean[5:20],parasORIGmean[25:40]),
              y=thet_mosq_means)

testb1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to para2.stan", data=modat,
             iter=1000, chains=4)
##***SETTINGS for sporssum to para2
#parameters {
#  real<lower=0.1, upper=0.5> alpha;
#  real<lower=0.1, upper=0.9>  beta;
#  real<lower=0, upper=1> delta;
#  real<lower=0> sigma;
#}
print(testb1)
print(waic(testb1))

paramsb1 = extract(testb1);names(paramsb1)
#rstan::traceplot(test2, inc_warmup = FALSE)

## For sporssum to para.stan
nc<-seq(0,13,1)
pred2 <- (mean(paramsb1$alpha)/mean(paramsb1$beta)) * exp(-exp(mean(paramsb1$delta) - mean(paramsb1$beta) * nc))

par(mar=c(4.5,4.5,0,0))
par(las=1)
plot(thet_mosq_means~c(parasORIGmean[5:20],parasORIGmean[25:40]),cex.lab=1.2,bty="n",
     xlim=c(0,15),yaxt="n",
     xlab="Mean parasitemia (%)",
     ylab=expression(paste("Per bite ", theta[mosquitoes], )),ylim=c(0,0.4))
par(las=2);axis(2,at=seq(0,0.4,0.2),labels=seq(0,0.4,0.2))
legend(12,0.4,"F")
lines(nc,pred2,col="red",lwd=2,lty=2)

x <- seq(0,13,0.1)

e <- extract(testb1, pars = c("alpha", "beta", "delta"))

for(i in seq_along(e[[1]])) {
  lines(x, ((e[[1]][i]/e[[2]][i]) * exp(-exp(e[[3]][i] - e[[2]][i] * x))), col = "#00000008")
}
lines(nc,pred2,lwd=2,lty=2,col="red")
points(thet_mosq_means[17:32]~parasORIGmean[25:40],pch=20,col="red")

#######################################
##
## Sporozoites to per bite probability in mouse
##
########################################
screen(6)

datlist <- list(N=32,
                y=thet_mo_means,
                x=c(MEANsporig[5:20],MEANsporig[25:40]))

testb <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\logisticfunction in stan4.stan", data=datlist,
              iter=1000, chains=4)

##****setting prior in logisticfunction in stan2
#parameters {
#real<lower=0> phi; // 
#  real<lower=0, upper=0.6> beta;  // 
#  real<lower=0, upper=1> alpha;
#

print(testb);print(waic(testb))
paramsb = extract(testb);names(paramsb)

#rstan::traceplot(test, inc_warmup = FALSE)
par(mar=c(4.5,4.5,0,0))
plot(thet_mo_means~c(MEANsporig[5:20],MEANsporig[25:40]),cex.lab=1.2,bty="n",xaxt="n",yaxt="n",
     ylab=expression(paste("Per bite ", theta[mouse], )),
     xlab="Mean sporozoite score",ylim=c(0,0.4))
axis(2,par(las=2),at=c(0,0.2,0.4),labels=c(0,0.2,0.4))
axis(1,par(las=1),at=seq(0,2.3,1),labels=seq(0,2.3,1))
legend(1.9,0.38,"E")
nc <- seq(0,max(MEANsporig),0.1)
pred1<- (mean(paramsb$alpha) * nc) / sqrt(1 + nc ^ (1/mean(paramsb$beta)))

lines(nc,pred1,lwd=2,lty=2,col="blue")

x <- seq(0,max(MEANsporig),0.1)

e <- extract(testb, pars = c("alpha", "beta"))

for(i in seq_along(e[[1]])) {
  lines(x, (e[[1]][i] * x) / sqrt(1 + x ^ (1/e[[2]][i])), col = "#00000008")
}
lines(nc,pred1,lwd=2,lty=2,col="red")
points(thet_mo_means[17:32]~MEANsporig[25:40],pch=20,col="red")
#######################################
##
## Oocysts to per bite probability 
##
########################################
screen(7)

modat <- list(N=32,
                x=c(meanoocysts[5:20],meanoocysts[25:40]),
                y=thet_mo_means)

testb2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to para2.stan", data=modat,
               iter=1000, chains=4)
print(testb2);print(waic(testb2))
#rstan::traceplot(test2, inc_warmup = FALSE)
paramsb2 <- extract(testb2);names(paramsb2)
##***SETTINGS for sporssum to para2
#parameters {
#  real<lower=0.1, upper=0.5> alpha;
#  real<lower=0.1, upper=0.9>  beta;
#  real<lower=0, upper=1> delta;
#  real<lower=0> sigma;
#}

par(mar=c(4.5,4.5,0,0))
plot(thet_mo_means~c(meanoocysts[5:20],meanoocysts[25:40]),cex.lab=1.2,bty="n",xaxt="n",yaxt="n",
     ylab=expression(paste("Per bite ", theta[mouse], )),
     xlab="Mean oocysts",ylim=c(0,0.4))
axis(2,par(las=2),at=c(0,0.2,0.4),line=0,labels=c(0,0.2,0.4))
axis(1,par(las=1),at=seq(0,max(meanoocysts),10),labels=seq(0,max(meanoocysts),10))
legend(50,0.4,"D")
nc<-seq(0,60,1)
pred2 <- (mean(paramsb2$alpha)/mean(paramsb2$beta)) * exp(-exp(mean(paramsb2$delta) - mean(paramsb2$beta) * nc))

lines(nc,pred2)

x2 <- seq(0,60,0.1)

e2 <- extract(testb2, pars = c("alpha", "beta", "delta"))

for(i in seq_along(e2[[1]])) {
  lines(x2, ((e2[[1]][i]/e2[[2]][i]) * exp(-exp(e2[[3]][i] - e2[[2]][i] * x2))), col = "#00000008")
}
lines(nc,pred2,lwd=2,lty=2,col="red")
points(thet_mo_means[17:32]~meanoocysts[25:40],pch=20,col="red")
