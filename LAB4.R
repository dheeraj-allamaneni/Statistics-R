#Here is code for Stat 587B Lab#4


#Code Set 1
#Here is some code for Exercise 2 Section 5.5

#Question 1

#this function is commonly used for all the below computing.
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

#Now L1 constant
L1<-rep(1,10000)

L2<-rnorm(10000,mean=1.00095,sd=.00005)#Final length
T1<-rnorm(10000,mean=50,sd=.1)#initial Temp
T2<-rnorm(10000,mean=100,sd=.1)#Final Temp
a1<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times

for(i in 1:10000) {a1[i]<-alpha(L1[i],L2[i],T1[i],T2[i])}
summary(a1)
sd(a1)

hist(a1)


#Now L2 Constant

L1<-rnorm(10000,mean=1,sd=.00005)#initial length
L2<-rep(1,10000)
T1<-rnorm(10000,mean=50,sd=.1)#initial Temp
T2<-rnorm(10000,mean=100,sd=.1)#Final Temp
a2<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times

for(i in 1:10000) {a2[i]<-alpha(L1[i],L2[i],T1[i],T2[i])}
summary(a2)
sd(a2)

hist(a2)


#Now T1 Constant

L1<-rnorm(10000,mean=1,sd=.00005)#initial length
L2<-rnorm(10000,mean=1.00095,sd=.00005)#Final length
T1<-rep(50,10000)
T2<-rnorm(10000,mean=100,sd=.1)#Final Temp
a3<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times

for(i in 1:10000) {a3[i]<-alpha(L1[i],L2[i],T1[i],T2[i])}
summary(a3)
sd(a3)

hist(a3)



#Now T2 Constant


L1<-rnorm(10000,mean=1,sd=.00005)#initial length
L2<-rnorm(10000,mean=1.00095,sd=.00005)#Final length
T1<-rnorm(10000,mean=50,sd=.1)#initial Temp
T2<-rep(100,10000)
a4<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times

for(i in 1:10000) {a4[i]<-alpha(L1[i],L2[i],T1[i],T2[i])}
summary(a4)
sd(a4)


#QUESTION 2

gravityfun<-function(l1,t1){
  ((2*pi)^2 * l1)/(t1^2)
}

L1<-rnorm(10000,mean=5,sd=0.0208)#Length

T1<-rnorm(10000,mean=2.48,sd=.1)#Time Period
grav<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times

for(i in 1:10000) {grav[i]<-gravityfun(L1[i],T1[i])}
summary(grav)
sd(grav)

hist(grav)

#NOW L constant

L1<-rep(5,10000)

T1<-rnorm(10000,mean=2.48,sd=.1)#Time Period
grav1<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times

for(i in 1:10000) {grav1[i]<-gravityfun(L1[i],T1[i])}
summary(grav1)
sd(grav1)

hist(grav1)


#NOW Time period const

gravityfun<-function(l1,t1){
  ((2*pi)^2 * l1)/(t1^2)
}

L1<-rnorm(10000,mean=5,sd=0.0208)#Length

T1<-rep(2.48,10000)
grav2<-rep(0,10000)#This repeats value 0 10000 times. this is a vector of values 0 ten thousand times

for(i in 1:10000) {grav2[i]<-gravityfun(L1[i],T1[i])}
summary(grav2)
sd(grav2)

hist(grav2)

#The g value comes out to be 
mean(grav)
#and the standard deviation is
sd(grav)
#IN this problem both L and Time period show the similar contribution to the value g. 
#The actual values of g varies from 32.09ft/sec2 and 32.26ft/sec2

#################################
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
###################################


# QUESTION 3

# N = 5
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

###############################################

#  N=100

M<-matrix(runif(1000000,min=0,max=1),nrow=10000,byrow=T)

av<-1:10000

for (i in 1:10000){
  av[i]<-mean(M[i,])  
}

z<-1:10000
for (i in 1:10000){
  z[i]<-(av[i]-.5)*sqrt(1200)
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
  z[i]<-(av[i]-.5)*sqrt(100)/s[i]
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

#Sample Mean dstn for the given data stanard deviation is Standard Normal distribution. Where as with the 
#Known sample standard deviation is not. Mainly beacuse N = 5 is a very small sample size
#Where as in N=100  the sample mean distribution and standard deviation are standard normal because 100 is quite a large sample.

##############################################

#Question 4

#First case N = 5

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


#Second N=100


M<-matrix(runif(1000000,min=0,max=1),nrow=10000,byrow=T)
Low<-rep(0,10000)
Up<-rep(0,10000)

chk<-rep(0,10000)
for (i in 1:10000){
  av[i]<-mean(M[i,])  
}

for(i in 1:10000) {Low[i]<-av[i]-1.96*sqrt(1/1200)}
for(i in 1:10000) {Up[i]<-av[i]+1.96*sqrt(1/1200)}
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

for(i in 1:10000) {Low[i]<-av[i]-1.96*s[i]*sqrt(1/100)}
for(i in 1:10000) {Up[i]<-av[i]+1.96*s[i]*sqrt(1/100)}
for(i in 1:10000) {if((Low[i]<.5)&(.5<Up[i])) chk[i]<-1}

cbind(Low[1:10],Up[1:10],chk[1:10])
mean(chk)
#Target confidence level is 95% and the calculated confidence level is 95.42%; But for the sample standard deviation we get confidence value or level as 86.47 for n= 5 as the sample size is very small.
#For N = 100Target confidence level is 95% and confidence level calculated is 94.92 (very close).
#known sample SD confidence level as 94.53 for n=100 ; as the sample is large enough.

