#Problem 1
gpadata<-read.table("C:\\Users\\jacob\\OneDrive\\Documents\\Stats\\8050\\HW3\\GPAdata.txt")
act<-gpadata[,2]
gpa<-gpadata[,1]

actbar<-mean(act)
gpabar<-mean(gpa)

b1<-sum((act-actbar)*(gpa-gpabar))/sum((act-actbar)^2)
b0<-gpabar-b1*actbar

cat("GPA_i=",b0,"+",b1,"ACT_i")

a<-0.05
n<-length(act)
actvals<-seq(min(act)-1,max(act)+1,length.out=500)
MSE<-sum((gpa-b0-b1*act)^2)/(n-2)
flow<-function(x){b0+b1*x-qt(1-a/2,n-2)*sqrt(MSE*(1/n+(x-actbar)^2/(sum((act-actbar)^2))))}
fhigh<-function(x){b0+b1*x+qt(1-a/2,n-2)*sqrt(MSE*(1/n+(x-actbar)^2/(sum((act-actbar)^2))))}

plot(act,gpa,col="black",xlab="ACT",ylab="GPA",main="ACT vs GPA")
abline(b0,b1,col="red")
lines(actvals,flow(actvals),col="blue")
lines(actvals,fhigh(actvals),col="blue")
legend("bottomright", 
       legend = c("Data", "Regression Line", "Confidence Interval"),
       col = c("black", "red", "blue"),
       bg = adjustcolor("white", alpha.f = 0.1),
       lty = c(NA, 1, 2),      
       pch = c(1, NA, NA),     
       lwd = c(NA, 1, 1))

gpa30<-b0+b1*30
YhCIradius<-qt(1-a/2,n-2)*sqrt(MSE*(1/n+(30-actbar)^2/(sum((act-actbar)^2))))
PIradius<-qt(1-a/2,n-2)*sqrt(MSE*(1+1/n+(30-actbar)^2/(sum((act-actbar)^2))))
cat("CI=(",gpa30-YhCIradius,",",gpa30+YhCIradius,")")
cat("PI=(",gpa30-PIradius,",",gpa30+PIradius,")")

a<-0.01
b0CIradius<-qt(1-a/2,n-2)*sqrt(MSE*(1/n+actbar^2/(sum((act-actbar)^2))))
b1CIradius<-qt(1-a/2,n-2)*sqrt(MSE/(sum((act-actbar)^2)))
cat("CI=(",b0-b0CIradius,",",b0+b0CIradius,")")
cat("CI=(",b1-b1CIradius,",",b1+b1CIradius,")")

t0<-b0/sqrt(MSE*(1/n+actbar^2/(sum((act-actbar)^2))))
t1<-b1/sqrt(MSE/(sum((act-actbar)^2)))
p0<-pt(t0,n-2)
p1<-pt(t1,n-2)
cat("b0 pvalue=",2*min(p0,1-p0))
cat("b1 pvalue=",2*min(p1,1-p1))

SSR<-sum((b0+b1*act-gpabar)^2)
atable<-data.frame(Source=c("SS","df","MS","F*","pvalue"),
                   Regression=c(SSR,1,SSR,SSR/MSE,1-pf(SSR/MSE,1,n-2)),
                   Error=c(MSE*(n-2),n-2,MSE,NA,NA),
                   Total=c(SSR+MSE*(n-2),n-1,NA,NA,NA))
print(t(atable))

fb1<-(b1/sqrt(MSE/(sum((act-actbar)^2))))^2
fp<-pf(fb1,1,n-2)
cat("b1=0? pvalue=",1-fp)

R2<-1-(MSE*(n-2))/(SSR+MSE*(n-2))
r<-sign(b1)*sqrt(R2)
cat("R^2=",R2,"and r=",r)

boxplot(act,main="ACT scores")

plot(gpa-b0-b1*act,b0+b1*act,main="Fitted Values vs Residuals",
     xlab="Residuals",ylab="Fitted Values")

qqnorm(gpa-b0-b1*act)
qqline(gpa-b0-b1*act)

threshold<-26
less<-gpadata[gpadata[,2]<=threshold,]
great<-gpadata[gpadata[,2]>threshold,]

actless<-less[,2]
actgreat<-great[,2]

gpaless<-less[,1]
gpagreat<-great[,1]

resless<-gpaless-b0-b1*actless
resgreat<-gpagreat-b0-b1*actgreat

medless<-median(resless)
medgreat<-median(resgreat)

d1<-abs(resless-medless)
d2<-abs(resgreat-medgreat)

tbf<-(mean(d1)-mean(d2))/sqrt((sum((d1-mean(d1))^2)+sum((d2-mean(d2))^2))*
                                (1/length(gpaless)+1/length(gpagreat))/(n-2))
pbf<-pt(tbf,n-2)
cat("Pvalue for BF=",2*min(pbf,1-pbf))

#Problem 2
muscledata<-read.table("C:\\Users\\jacob\\OneDrive\\Documents\\Stats\\8050\\HW3\\Muscledata.txt")
age<-muscledata[,2]
mm<-muscledata[,1]

agebar<-mean(age)
mmbar<-mean(mm)

b1<-sum((age-agebar)*(mm-mmbar))/sum((age-agebar)^2)
b0<-mmbar-b1*agebar

cat("MM_i=",b0,"+",b1,"Age_i")

a<-0.05
n<-length(age)
agevals<-seq(min(age)-1,max(age)+1,length.out=500)
MSE<-sum((mm-b0-b1*age)^2)/(n-2)
flow<-function(x){b0+b1*x-qt(1-a/2,n-2)*sqrt(MSE*(1/n+(x-agebar)^2/(sum((age-agebar)^2))))}
fhigh<-function(x){b0+b1*x+qt(1-a/2,n-2)*sqrt(MSE*(1/n+(x-agebar)^2/(sum((age-agebar)^2))))}

plot(age,mm,col="black",xlab="Age",ylab="MM",main="Age vs MM")
abline(b0,b1,col="red")
lines(agevals,flow(agevals),col="blue")
lines(agevals,fhigh(agevals),col="blue")
legend("bottomleft", 
       legend = c("Data", "Regression Line", "Confidence Interval"),
       col = c("black", "red", "blue"),
       bg = adjustcolor("white", alpha.f = 0.1),
       lty = c(NA, 1, 2),      
       pch = c(1, NA, NA),     
       lwd = c(NA, 1, 1))

mm55<-b0+b1*55
YhCIradius<-qt(1-a/2,n-2)*sqrt(MSE*(1/n+(55-agebar)^2/(sum((age-agebar)^2))))
PIradius<-qt(1-a/2,n-2)*sqrt(MSE*(1+1/n+(55-agebar)^2/(sum((age-agebar)^2))))
cat("CI=(",mm55-YhCIradius,",",mm55+YhCIradius,")")
cat("PI=(",mm55-PIradius,",",mm55+PIradius,")")

a<-0.01
b0CIradius<-qt(1-a/2,n-2)*sqrt(MSE*(1/n+agebar^2/(sum((age-agebar)^2))))
b1CIradius<-qt(1-a/2,n-2)*sqrt(MSE/(sum((age-agebar)^2)))
cat("CI=(",b0-b0CIradius,",",b0+b0CIradius,")")
cat("CI=(",b1-b1CIradius,",",b1+b1CIradius,")")

t0<-b0/sqrt(MSE*(1/n+agebar^2/(sum((age-agebar)^2))))
t1<-b1/sqrt(MSE/(sum((age-agebar)^2)))
p0<-pt(t0,n-2)
p1<-pt(t1,n-2)
cat("b0 pvalue=",2*min(p0,1-p0))
cat("b1 pvalue=",2*min(p1,1-p1))

SSR<-sum((b0+b1*age-mmbar)^2)

atable<-data.frame(Source=c("SS","df","MS","F*","pvalue"),
                   Regression=c(SSR,1,SSR,SSR/MSE,1-pf(SSR/MSE,1,n-2)),
                   Error=c(MSE*(n-2),n-2,MSE,NA,NA),
                   Total=c(SSR+MSE*(n-2),n-1,NA,NA,NA))
print(t(atable))

fb1<-(b1/sqrt(MSE/(sum((age-agebar)^2))))^2
fp<-pf(fb1,1,n-2)
cat("b1=0? pvalue=",1-fp)

R2<-1-(MSE*(n-2))/(SSR+MSE*(n-2))
r<-sign(b1)*sqrt(R2)
cat("R^2=",R2,"and r=",r)

boxplot(age,main="Age")

plot(mm-b0-b1*age,b0+b1*age,main="Filled values vs Residuals",
     xlab="Residuals",ylab="Fitted Values")

qqnorm(mm-b0-b1*age)
qqline(mm-b0-b1*age)

threshold<-60
less<-muscledata[muscledata[,2]<=threshold,]
great<-muscledata[muscledata[,2]>threshold,]

ageless<-less[,2]
agegreat<-great[,2]

mmless<-less[,1]
mmgreat<-great[,1]

resless<-mmless-b0-b1*ageless
resgreat<-mmgreat-b0-b1*agegreat

medless<-median(resless)
medgreat<-median(resgreat)

d1<-abs(resless-medless)
d2<-abs(resgreat-medgreat)

tbf<-(mean(d1)-mean(d2))/sqrt((sum((d1-mean(d1))^2)+sum((d2-mean(d2))^2))*
                                (1/length(mmless)+1/length(mmgreat))/(n-2))
pbf<-pt(tbf,n-2)
cat("Pvalue for BF=",2*min(pbf,1-pbf))
