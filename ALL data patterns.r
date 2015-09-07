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

##########################################################################
##
## 2. Exploring the relationship between parasitemia and sporozoites
##  
##
###########################################################################
### 
data <- read.table("C:\\Users\\Ellie\\Documents\\Data Malaria\\Mouse Data All\\ParasiteAllMASTER5.txt",header=TRUE)
names(data)
for (i in 1:length(data$Sporozoite1)){
data$Sporozoite1[i] <- ifelse(data$Sporozoite1[i]==5,NA,data$Sporozoite1[i])
data$Sporozoite2[i] <- ifelse(data$Sporozoite2[i]==5,NA,data$Sporozoite2[i])
data$Sporozoite3[i] <- ifelse(data$Sporozoite3[i]==5,NA,data$Sporozoite3[i])
data$Sporozoite4[i] <- ifelse(data$Sporozoite4[i]==5,NA,data$Sporozoite4[i])
data$Sporozoite5[i] <- ifelse(data$Sporozoite5[i]==5,NA,data$Sporozoite5[i])
data$Sporozoite6[i] <- ifelse(data$Sporozoite6[i]==5,NA,data$Sporozoite6[i])
data$Sporozoite7[i] <- ifelse(data$Sporozoite7[i]==5,NA,data$Sporozoite7[i])
data$Sporozoite8[i] <- ifelse(data$Sporozoite8[i]==5,NA,data$Sporozoite8[i])
data$Sporozoite9[i] <- ifelse(data$Sporozoite9[i]==5,NA,data$Sporozoite9[i])
data$Sporozoite10[i] <- ifelse(data$Sporozoite10[i]==5,NA,data$Sporozoite10[i])
}

for (i in 1:length(data$Para4)) data$sumdata[i]<-sum(data$Sporozoite1[i],data$Sporozoite2[i],data$Sporozoite3[i],
                                                         data$Sporozoite4[i],data$Sporozoite5[i],
                                                         data$Sporozoite6[i],data$Sporozoite7[i],
                                                         data$Sporozoite8[i],data$Sporozoite9[i],data$Sporozoite10[i],na.rm=T)



mod1 <- glm.nb(data$sumdata ~ data$Para10 + 0)
summary.lm(mod1)

m3 <- glm(data$sumdata ~ data$Para10 + 0, family = "poisson")
pchisq(2 * (logLik(mod1) - logLik(m3)), df = 1, lower.tail = FALSE)
(est <- cbind(Estimate = coef(mod1), confint(mod1)))

data2<-data.frame(data$Para10,data$sumdata)
data2 <- na.omit(data2)

##should sporo scores predict parasitemia or parasitemia predict post-feeding spor scores?!!
spordat <- list(N=744,
                x=data2$data.Para10,
                y=data2$data.sumdata)
test1a <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\sporssum to paralinearfit.stan", data=spordat,
               iter=1000, chains=4)

print(test1a)
print(waic(test1a))
params = extract(test1a);names(params)
#rstan::traceplot(test1, inc_warmup = FALSE)

par(mfrow=c(1,1));par(las=1)
## For datasum to para.stan
nc<-seq(0,max(data2$data.Para10),0.1)
pred<-(mean(params$alpha[501:1000]) + mean(params$beta[501:1000]) * nc) 
par(mar=c(5,5,5,5))
plot(data2$data.sumdata~data2$data.Para10,cex.lab=1.4,bty="n",yaxt="n",cex.lab=1.5,
     ylab="Summed sporozoite score",xlab="Parasitemia (%)",ylim=c(0,25))
par(las=2);axis(2,at=seq(0,25,5),labels=c(0,5,10,15,20,25))

lines(nc,pred,lwd=2,lty=2,col="blue")
alpha <- mean(params$alpha)
beta <- mean(params$beta)
sigma <- mean(params$sigma)
x <- seq(0,max(data2$data.Para10),0.1)
y <- pred

e <- extract(test1a, pars = c("alpha", "beta", "sigma"))

for(i in seq_along(e[[1]])) {
  lines(x, (e[[1]][i] + e[[2]][i] * x), col = "#00000003")
}
lines(nc,pred,lwd=2,lty=2,col="red")

se <- function(x) sqrt(var(x)/length(x))
WAICcomp <- waic(test1)$total[4]-waic(test1a)$total[4]

##should sporo scores predict parasitemia or parasitemia predict post-feeding spor scores?!!
spordat <- list(N=755,
                x=data$sumpara,
                y=data$sumdata)
test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\datasum to para.stan", data=spordat,
              iter=1000, chains=4)

print(test1)
print(waic(test1))
params = extract(test1);names(params)
rstan::traceplot(test1, inc_warmup = FALSE)

## For datasum to para.stan
nc<-seq(0,max(data$Parasitemia),0.1)
pred1<-(mean(params$alpha[501:1000]) * nc^mean(params$sigma[501:1000]))/
  (mean(params$delta[501:1000]) + mean(params$beta[501:1000]) * nc^mean(params$sigma[501:1000])) 
par(mar=c(5,5,5,5))
plot(data$sumdata~data$Parasitemia,cex.lab=1.5,bty="n",
     ylab="Summed sporozoite score",xlab="Parasitemia (%)",ylim=c(0,25))

lines(nc,pred1,lwd=2,lty=2,col="red")
alpha <- mean(params$alpha)
beta <- mean(params$beta)
sigma <- mean(params$sigma)
delta <- mean(params$delta)
eps <- mean(params$eps)
x <- seq(0,max(data$Parasitemia),0.1)
y <- pred1

e <- extract(test1, pars = c("alpha", "beta", "sigma","delta","eps"))

for(i in seq_along(e[[1]])) {
  lines(x, (e[[1]][i] * x^e[[3]][i]) / (e[[4]][i] + e[[2]][i] * x^e[[3]][i]), col = "#00000008")
}
lines(nc,pred1,lwd=2,lty=2,col="red")


## For datasum to para4negbin
spordat <- list(N=200,
                y=round(data$Parasitemia),
                x=data$sumdata)
test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\Malaria2\\datasum to para4negbin.stan", data=spordat,
              iter=1000, chains=4)

par(mar=c(5,5,5,5))
plot(data$Parasitemia~data$sumdata,cex.lab=1.4,bty="n",
     xlab="Summed sporozoite score",ylab="Parasitemia (%)",ylim=c(0,25))

print(test2)
params = extract(test2);names(params)
rstan::traceplot(test2, inc_warmup = FALSE)

set.seed(123)

phi <- mean(params$phi)
b0 <- mean(params$b0)
b1 <- mean(params$b1)
x <- preds3$x
N <- length(x)
y <- rnbinom(N, size = phi, mu = exp(b0 + (x - mean(x)) * b1))

e <- extract(test2, pars = c("b0", "b1", "phi"))
true_pars <- c(b0 = b0, b1 = b1, phi = phi)
x_cent <- x - mean(x)
m_mass <- MASS::glm.nb(y ~ x_cent)
coefs_mass <- c(coef(m_mass), summary(m_mass)$theta)

for(i in seq_along(e[[1]])) {
  lines(x, exp(e[[1]][i] + e[[2]][i] * (x - mean(x))), col = "#00000006")
}
#points(x, exp(mean(e[[1]])+mean(e[[2]])*x),col="red",lwd=2)
ypred <- exp(mean(e[[1]])+mean(e[[2]])*x)
preds <- data.frame(x,ypred);preds2<-preds[with(preds, order(x)), ]
preds3 <- preds2[!duplicated(preds2[,c('x')]),]
lines(preds3$x,preds3$ypred)
points(data$sumdata~data$Parasitemia,pch=20,col="blue")
#points(data$Parasitemia[data$Treatment==1]~data$sumdata[data$Treatment==1],pch=20,col="blue")

par(mfrow = c(1, 3))
for(i in 1:3) {
  if(i %in% 1:2) {
    plot(density(e[[i]]), main = names(true_pars)[i])
  } else {
    plot(density(e[[i]]), main = names(true_pars)[i], log = "x")
  }
  abline(v = true_pars[i], lwd = 2, col = "grey", lty = 2)
  abline(v = coefs_mass[i], lwd = 3, col = "red")
}

legend("topright", legend = c("posterior", "true", "MASS::glm.nb"),
       col = c("black", "grey", "red"), lty = c(1, 2, 1), lwd = c(1, 1.3, 3),
       bty = "n")
