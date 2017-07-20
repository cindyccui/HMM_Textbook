# Fitting Poisson to Earthquake series (From book)

source("./R/HMM_functions.R")

## A.2.1 Fitting Poisson{HMMs to the earthquakes series
dat <- read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")
#(or set your own path)
x   <-dat[,2]
d   <-dat[,1]
n   <-length(x)
#====================================== fit 2-state HMM
m<-2
lambda0<-c(15,25)
gamma0<-matrix(
  c(
    0.9,0.1,
    0.1,0.9
  ),m,m,byrow=TRUE)
mod2s<-pois.HMM.mle(x,m,lambda0,gamma0,stationary=TRUE)
delta0<-c(1,1)/2
mod2h<-pois.HMM.mle(x,m,lambda0,gamma0,delta=delta0,stationary=FALSE)
mod2s; mod2h
#====================================== fit 3-state HMM
m<-3
lambda0<-c(10,20,30)
gamma0<-matrix(
  c(
    0.8,0.1,0.1,
    0.1,0.8,0.1,
    0.1,0.1,0.8
  ),m,m,byrow=TRUE)
mod3s<-pois.HMM.mle(x,m,lambda0,gamma0,stationary=TRUE)
delta0 <- c(1,1,1)/3
mod3h<-pois.HMM.mle(x,m,lambda0,gamma0,delta=delta0,stationary=FALSE)
mod3s; mod3h
#====================================== fit 4-state HMM
m<-4
lambda0<-c(10,15,20,30)
gamma0<-matrix(
  c(
    0.85,0.05,0.05,0.05,
    0.05,0.85,0.05,0.05,
    0.05,0.05,0.85,0.05,
    0.05,0.05,0.05,0.85
  ),m,m,byrow=TRUE)
mod4s<-pois.HMM.mle(x,m,lambda0,gamma0,stationary=TRUE)
delta0<-c(1,1,1,1)/4
mod4h<-pois.HMM.mle(x,m,lambda0,gamma0,delta=delta0,stationary=FALSE)
mod4s; mod4h
