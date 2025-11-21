ps<-read.table("C:\\Users\\jacob\\OneDrive\\Documents\\Stats\\8050\\HW5\\PatientSatisfaction.txt",header=T)

y<-ps[,1]
x1<-ps[,2]
x2<-ps[,3]
x3<-ps[,4]
n<-length(y)
p<-4

pairs(ps, labels=c("Y","x1","x2","x3"))

model<-lm(y~x1+x2+x3)
modcof<-model$coefficients
cat("E[Y_i]=",modcof[1],"+",modcof[2],"x_i1+",modcof[3],"x_i2+",modcof[4],"x_i3")

rse<-summary(model)$sigma
sumsq<-anova(model)$"Sum Sq"

f<-sum(sumsq)/(p-1)/rse^2

1-pf(f,p-1,n-p)

R2a<-1-rse^2/(rse^2*(n-p)+sum(sumsq))/(n-1)
R2a

v<-vcov(model)
ciR<-drop(qt(0.95,n-p)*sqrt(t(c(1,35,45,2.2)) %*% v %*% c(1,35,45,2.2)))
cimean<-predict(model,newdata=data.frame(x1=35,x2=45,x3=2.2))#We could get CI from this function by itself
cat("CI=(",cimean-ciR,",",cimean+ciR,")")

sl<-read.table("C:\\Users\\jacob\\OneDrive\\Documents\\Stats\\8050\\HW5\\SteroidLevels.txt",header=T)
sly<-sl[,1]
slx<-sl[,2]-mean(sl[,2])
slx2<-slx^2
slmodel<-lm(sly~slx+slx2)
slcof<-slmodel$coefficients
plot(slx,sly)
curve(slcof[1]+slcof[2]*x+slcof[3]*x^2,-10,10,add=T)
summary(slmodel)$r.squared

slrse<-summary(slmodel)$sigma
slsumsq<-anova(slmodel)$"Sum Sq"

slf<-sum(slsumsq)/(2)/slrse^2
1-pf(slf,2,length(sly)-3)

predict(slmodel,newdata=data.frame(slx=15,slx2=225),interval="prediction",level=0.99)

sltval<-summary(slmodel)$coefficients[,"t value"]
quadt<-sltval[3]
2*min(1-pt(quadt,length(sly)-3),pt(quadt,length(sly)-3))

fit1<-lm(y~x2)
fit2<-lm(y~x1+x2)
fit3<-lm(y~x3+x2)
fit4<-lm(y~x1)
anova(fit1,fit2,fit3,fit4)
anova(fit1)
anova(fit2)
anova(fit3)
anova(fit4)

SSE.R1 <- anova(lm(y~x1+x2))[3,2]
SSE.F <- anova(lm(y~x1+x2+x3))[4,2]

F.star1<- ((SSE.R1-SSE.F)/1)/(SSE.F/(n-p))
print(paste("Test statistic=",F.star1,"p-value=", 1-pf(F.star1,1,n-p)))

SSE.R2 <- anova(lm(y~x1))[2,2]

F.star2<- ((SSE.R2-SSE.F)/2)/(SSE.F/(n-p))
print(paste("Test statistic=",F.star2,"p-value=", 1-pf(F.star2,2,n-p)))

SSE.R3<-anova(lm(y+x1~x3))[2,2]
F.star3<- ((SSE.R3-SSE.F)/1)/(SSE.F/(n-p))
print(paste("Test statistic=",F.star1,"p-value=", 1-pf(F.star1,1,n-p)))
