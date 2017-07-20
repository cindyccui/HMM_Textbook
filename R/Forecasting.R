# Forecasting probabilities

source("./R/HMM_functions.R")

### A.2.2 Forecast probabilities
#=== Use it for 1-step-ahead and plot the forecast distribution.
h<-1
xf<-0:50
forecasts<-pois.HMM.forecast(xf,h,x,mod3s)
fc<-forecasts[1,]
par(mfrow=c(1,1),las=1)
plot(xf,fc,type="h",
     main=paste("Earthquake series: forecast distribution for", d[n]+1),
     xlim=c(0,max(xf)),ylim=c(0,0.12),xlab="count",ylab="probability",lwd=3)

#=== Forecast 1-4 steps ahead and plot these.
h<-4
xf<-0:45
forecasts<-pois.HMM.forecast(xf,h,x,mod3s)

par(mfrow=c(2,2),las=1)
for (i in 1:4)
{
  fc<-forecasts[i,]
  plot(xf,fc,type="h",main=paste("Forecast distribution for", d[n]+i),
       xlim=c(0,max(xf)),ylim=c(0,0.12),xlab="count",ylab="probability",lwd=3)
}

#=== Compute the marginal distribution (called "dstat" below)
#    for mod3h.
#=== This is also the long-term forecast.
m<-3.

lambda<-mod3h$lambda
delta<-solve(t(diag(m)-mod3h$gamma+1),rep(1,m))
dstat<-numeric(length(xf))
for (j in 1:m) dstat <- dstat + delta[j]*dpois(xf,lambda[j])

#=== Compare the 50-year-ahead forecast with the long-term forecast.
h<-50
xf<-0:45
forecasts<-pois.HMM.forecast(xf,h,x,mod3h)
fc<-forecasts[h,]
par(mfrow=c(1,1),las=1)
plot(xf,fc,type="h",
     main=paste("Forecast distribution for", d[n]+h),
     xlim=c(0,max(xf)),ylim=c(0,0.12),xlab="count",ylab="probability",lwd=3)
lines(xf,dstat,col="gray",lwd=3)
