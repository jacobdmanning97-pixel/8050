copierdata<-read.table("C:\\Users\\jacob\\OneDrive\\Documents\\Stats\\8050\\HW4\\Copier.txt")
mins<-as.numeric(copierdata[-1,1])
cops<-as.numeric(copierdata[-1,2])
model<-lm(mins~cops)
modelcoef<-as.numeric(coef(model))
y4<-modelcoef[1]+modelcoef[2]*4
y7<-modelcoef[1]+modelcoef[2]*7
nmodel<-length(cops)
xbar<-mean(cops)
radius4<-qt(0.975,nmodel-2)*sqrt(sum(residuals(model)^2)*(1/nmodel+(4-xbar)^2/sum((cops-xbar)^2)))
radius7<-qt(0.975,nmodel-2)*sqrt(sum(residuals(model)^2)*(1/nmodel+(7-xbar)^2/sum((cops-xbar)^2)))

cat("Family of Intervals: y4 (",y4-radius4,",",y4+radius4,") and y7 (",y7-radius7,",",y7+radius7,")")

radiusb0<-qt(0.975,nmodel-2)*sqrt(sum(residuals(model)^2)*(1/nmodel+xbar^2/sum((cops-xbar)^2)))
radiusb1<-qt(0.975,nmodel-2)*sqrt(sum(residuals(model)^2/sum((cops-xbar)^2)))

cat("Family of Intervals: b0 (",modelcoef[1]-radiusb0,",",modelcoef[1]+radiusb0,") and b1 (",modelcoef[2]-radiusb1,",",modelcoef[2]+radiusb1,")")


y<-c(16,5,10,15,13,22)
xi<-c(4,1,2,3,3,4)
x0<-c(1,1,1,1,1,1)
n<-length(x0)
X<-matrix(c(x0,xi),nrow=n,ncol=2,byrow="False")
J<-outer(x0,x0)
H<-X%*%solve(t(X)%*%X)%*%t(X)
xh<-c(1,4)

sum(y^2)
t(X)%*%X
t(X)%*%y
solve(t(X)%*%X)

b<-solve(t(X)%*%X)%*%t(X)%*%y
b
r<-y-H%*%y
r
SSR<-drop(t(y)%*%(H-1/n*J)%*%y)
SSR
SSE<-drop(t(y)%*%(diag(n)-H)%*%y)
SSE
MSE<-SSE/(n-2)
Vb<-MSE*solve(t(X)%*%X)
Vb
yh<-drop(xh%*%b)
yh
s2pred<-drop(MSE*(1+t(xh)%*%solve(t(X)%*%X)%*%xh))
s2pred
H
MSE*(diag(n)-H)

library(MASS)
salesdata<-read.table("C:\\Users\\jacob\\OneDrive\\Documents\\Stats\\8050\\HW4\\SalesGrowth.txt")
sales<-as.numeric(salesdata[-1,1])
years<-as.numeric(salesdata[-1,2])
plot(years,sales,main="Sales vs Years",xlab="Years",ylab="Sales")
nlin<-lm(sales~years)
abline(nlin)

plot(years,residuals(nlin),main="Residuals vs Years",xlab="Years",ylab="Residuals")

bc<-boxcox(sales~years)
(lambda <- bc$x[which.max(bc$y)])

sqrtsales<-sqrt(sales)
nsqrt<-lm(sqrtsales~years)
plot(years,sqrtsales,main="Y'=Y^(1/2)",xlab="Years",ylab="sqrt(Sales)")
abline(nsqrt)
coefsqrt<-as.numeric(coef(nsqrt))
cat("Y'=",coefsqrt[1],"+",coefsqrt[2],"x")

plot(residuals(nsqrt),coefsqrt[1]+coefsqrt[2]*years,main="Residuals vs Fitted Values",xlab="Residuals",ylab="Fitted Values")
qqnorm(coefsqrt[1]+coefsqrt[2]*years)
qqline(coefsqrt[1]+coefsqrt[2]*years)
