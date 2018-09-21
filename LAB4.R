#Here is code for Stat 587B Lab#4


#Code Set 1
#Here is some code for Exercise 2 Section 5.5

#Question 1

alpha<-function(l1,l2,t1,t2){
  (l2-l1)/(l1*(t2-t1))
}

L1<-rnorm(10000,mean=1,sd=.00005)#initial length
L2<-rnorm(10000,mean=1.00095,sd=.00005)#Final length
T1<-rnorm(10000,mean=50,sd=.1)#initial Temp
T2<-rnorm(10000,mean=100,sd=.1)#Final Temp
a<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times

for(i in 1:10000) {a[i]<-alpha(L1[i],L2[i],T1[i],T2[i])}
summary(a)
sd(a)

hist(a)

#Question 2

alphat<-function(t){
  ((2*pi)^2)/(t^2)
}

alphal<-function(l,t){
  ((2*pi)^2 * -2 * l)/(t^3)
}

L<-rnorm(10000,mean=5,sd=.0208)# length
TP<-rnorm(10000,mean=2.48,sd=.1)#time period

a1<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times
a2<-rep(0,10000)

for(i in 1:10000) {a1[i]<-alphat(TP)
a2[i]<-alphal(L,TP)
}
a1
a2
varg<-a1^2 * (0.0208)^2 + a2^2 * (0.1)^2
summary(varg)
#sd(varg)

#hist(varg)
g=sqrt(varg)
summary(g)
# QUESTION 3

#Code Set 2
#Here is some code for studying the actual distribution of
#some approximately standard normal variables built on "iid"
#(random sampling from a fixed distribution) models

M<-matrix(runif(50000,min=0,max=1),nrow=10000,byrow=T)

av<-1:10000

for (i in 1:10000){
  av[i]<-mean(M[i,])  
}

z<-1:10000
for (i in 1:10000){
  z[i]<-(av[i]-.5)*sqrt(60)
}

r<-range(0,hist(z)$density,dnorm(0,sd=1))

hist(z,freq=FALSE,ylim=r)
curve(dnorm(x,mean=0,sd=1),add=TRUE)

plot(ecdf(z))
curve(pnorm(x),add=TRUE,col="red")

#Now use the sample standard deviation rather than the model sigma=1/sqrt(12)
#to make "z" values and see that these don't look as much like standard normal
#observations as do the properly standardized values of xbar

s<-1:10000
for (i in 1:10000){
  s[i]<-sd(M[i,])  
}

z<-1:10000
for (i in 1:10000){
  z[i]<-(av[i]-.5)*sqrt(5)/s[i]
}

r<-range(0,hist(z,breaks=100)$density,dnorm(0,sd=1))

#the breaks=100 changes the default number of histogram bins so we can
#see some detail in the center of the distribution

hist(z,breaks=100,freq=FALSE,ylim=r)
curve(dnorm(x,mean=0,sd=1),n=500,add=TRUE)

#the n=500 provides enough plotted points (connected with line segments)
#to produce a nice plot near the mean of the distribution

summary(z)

plot(ecdf(z))
curve(pnorm(x),add=TRUE, col="red")
#distribution of z^ are more spreadout and z are more closely like std normals

#Code Set 3
#Here is some code for making and checking the performance 
#of large n CIs for mu (for U(0,1) observations)
#First Use the model standard deviation

M<-matrix(runif(50000,min=0,max=1),nrow=10000,byrow=T)
Low<-rep(0,10000)
Up<-rep(0,10000)

chk<-rep(0,10000)
for (i in 1:10000){
  av[i]<-mean(M[i,])  
}

for(i in 1:10000) {Low[i]<-av[i]-1.96*sqrt(1/60)}
for(i in 1:10000) {Up[i]<-av[i]+1.96*sqrt(1/60)}
for(i in 1:10000) {if((Low[i]<.5)&(.5<Up[i])) chk[i]<-1}

cbind(Low[1:10],Up[1:10],chk[1:10])
mean(chk)

#Now use the sample standard deviation to make intervals for mu

Low<-rep(0,10000)
Up<-rep(0,10000)
chk<-rep(0,10000)
s<-rep(0,10000)
for (i in 1:10000){
  s[i]<-sd(M[i,])  
}

for(i in 1:10000) {Low[i]<-av[i]-1.96*s[i]*sqrt(1/5)}
for(i in 1:10000) {Up[i]<-av[i]+1.96*s[i]*sqrt(1/5)}
for(i in 1:10000) {if((Low[i]<.5)&(.5<Up[i])) chk[i]<-1}

cbind(Low[1:10],Up[1:10],chk[1:10])
mean(chk)


