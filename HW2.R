#Question 2b
hertz<-c(37.16,14.36,17.59,19.73,30.77,26.29,30.03,29.02,22.63,39.21)
thrift<-c(29.49,12.19,15.07,15.17,24.52,22.32,25.30,22.74,19.35,34.44)
dif<-hertz-thrift
qqnorm(dif,main="Hertz vs Thrift")
qqline(dif)

#Question 2c
nd<-length(dif)
pd<-pt(0.95,nd-1)
ts<-mean(dif)/(sd(dif)/sqrt(nd))
if(ts>pd){cat("Reject")}else{cat("Fail to reject")}

#Question 4a
low<-c(7.6,8.2,6.8,5.8,6.9,6.6,6.3,7.7,6.0)
mod<-c(6.7,8.1,9.4,8.6,7.8,7.7,8.9,7.9,8.3,8.7,7.1,8.4)
par(mfrow = c(1,2)) 
boxplot(low,xlab="Low",ylab="Spread")
boxplot(mod,xlab="Moderate",ylab="Spread")

#Question 4b
par(mfrow = c(1,2)) 
qqnorm(low,main="Low")
qqline(low)
qqnorm(mod,main="Moderate")
qqline(mod)

#Question 4c
lsd<-sd(low)
msd<-sd(mod)
nl<-length(low)
nm<-length(mod)
ntot<-nl+nm
pp<-pf(lsd^2/msd^2,nl-1,nm-1)
p<-2*min(pp,1-pp)
cat("t*=",lsd^2/msd^2)
cat("pvalue=",p)

#Question 4d
sp2<-((nl-1)*lsd^2+(nm-1)*msd^2)/(ntot-2)
ts<-(mean(mod)-mean(low))/sqrt(sp2/(ntot-2))
pv<-pt(ts,ntot-2)
if(1-pv<0.05){cat("Reject")}else{cat("Fail to reject")}
