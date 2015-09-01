##Calculate normal probablity distribution
## This is the probability of an individual getting infected by a parasite
## it could be due to the spatial spread of infectious stages 
## or differences in the imune system of hosts etc but regardless
## some individuals have a higher probablity of infection than others


da<-rnorm(100, mean = 0.4, sd = 0.3)
da<-ifelse(da<0,0,da)
length(da)
hist(da)##probablity of infection OR relative susceptibility to infections

##then try making this narrow (i.e. a greater proportion of people have no chance of infection)
##or broader (i.e. more evenly dispersed infections)

inf<-round(rnorm(100, mean = 10, sd = 1))
N = 100  ##Popualtion size


##Model the disease dynamics as an infection moves through
##the population with R0 = 2; starting with a single infection
##with a max capacity 
##infecting individuals at the defined probability

dN <- round(da * inf)  ##define the distribution of infection at t0
hist(dN)
tdays <- 1000  ## Number of days over which to run the simulation

r0mean=2
spread<-rnorm(100,mean=2,sd=0.5) ##contacts


infstat<-expand.grid(seq(1,100,1))
for (i in 1:1000){
  infstat[,1]<- da * spread * 10
  infstat[,i+1] <- infstat[,i] * spread
}

## Calculate prevalence and intensity distributions for the poppualtion at t = 1000

N = 100
t = c(1:1000)
S = 95
I = 5
beta = da
gamma = rnorm(100, mean = 0.2, sd = 0.05)

dsdt = dtdt = expand.grid(1:100)

for (i in 2:1000){
  dsdt[,1]<- (beta * S * I)/N + gamma * I
  didt[,1]<- (beta * S * I)/N - gamma * I

  dsdt[,i]<- (beta * dsdt[,i-1] * didt[,i-1])/N + gamma * didt[,i-1]
  didt[,i]<- (beta * dsdt[,i-1] * didt[,i-1])/N - gamma * didt[,i-1]
}

## So you can havve a population with a normally distributed susceptibility to parasite infections
## that ends up being heterogeneously infected by the parasite

##what happens when the probablity of becoming infected is also skewed?




