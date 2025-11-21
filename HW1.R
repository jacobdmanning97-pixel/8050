#Question 3
#a
for(n in 2:100){
  count=0
  for(i in 1:10000){
    list<-rt(n,5)
    ybar<-mean(list)
    a=0.05
    lb<-ybar-qt(1-a/2,n-1)*sd(list)/sqrt(n)
    upb<-ybar+qt(1-a/2,n-1)*sd(list)/sqrt(n)
    if (lb>0 || upb<0){
      count<-count+1
    }
  }
  if (1-count/10000>0.95){break}
}
print(count)
print(n)

#b
for(n in 200:400){
  count=0
  for(i in 1:10000){
    list<-rchisq(n,1)
    ybar<-mean(list)
    a=0.05
    lb<-ybar-qt(1-a/2,n-1)*sd(list)/sqrt(n)
    upb<-ybar+qt(1-a/2,n-1)*sd(list)/sqrt(n)
    if (lb>1 || upb<1){
      count<-count+1
    }
  }
  if (1-count/i>0.95){break}
}
print(count)
print(n)

#Question 4
13.2-qt(1-0.025,795)*1.6/sqrt(796)
13.2+qt(1-0.025,795)*1.6/sqrt(796)

#Question 5
hertz<-c(37.16,14.36,17.59,19.73,30.77,26.29,30.03,29.02,22.63,39.21)
thrift<-c(29.49,12.19,15.07,15.17,24.52,22.32,25.30,22.74,19.35,34.44)
qqplot(hertz,thrift)
abline(0, 1, col = "red")
