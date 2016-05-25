#The mean of exponential distribution is 1/lambda
#the standard deviation is also 1/lambda
#The exponential distribution can be simulated in R with rexp(n, lambda)


lambda <- 0.2
iterates <- 1000
n <- 40
theoMean <- 1/lambda
stdDev <- 1/lambda

#Create an exponential distribution: rexp(n,lambda)

#take the mean of this distribution: mean(rexp(n,lambda))

#Repeat 1000 times, and put it in a vector of means

#Initialise vMeans

vecMeans = NULL

#As we are generating random numbers, we set the seed so we can replicate the
#analysis later on

set.seed(123456)

for (i in 1 : iterates) vecMeans = c(vecMeans, mean(rexp(n,lambda)))

#Draw a histogram
hist(vecMeans)

#Task 1. Show the sample mean and compare it to the theoretical mean of the
#distribution.

#The sample mean (or mean of means):
mean(vecMeans)

#The theoretical mean: 1/lambda
1/lambda

#Compare:
mean(vecMeans) - 1/lambda

#Task 2. Show how variable the sample is (via variance) and compare it to the
#theoretical variance of the distribution.

#The Sample variance:
var(vecMeans)

#Theoretical variance: stdDev^2 / n
stdDev^2 / n

#Compare:
var(vecMeans) - stdDev^2 / n

#Task 3: Show that the distribution is approximately normal.
#CLT says: the arithmetic mean of a sufficiently large number of iterates of
#independent random variables will be approximately normally distributed.
#We do that by overlaying a normal distribution with the same mean and ranges
#as the histogram

xrange <- seq(min(vecMeans),max(vecMeans),length.out = n)
yrange <- dnorm(xrange,theoMean,sd = stdDev/sqrt(n)) * (iterates/2)
lines(xrange, yrange, lty=4)

#**************************************************************************
#PART TWO
#**************************************************************************

library(datasets)

ds <- ToothGrowth

#Basic exploration of the DataSet
summary(ds)

head(ds)
tail(ds)
str(ds)

#the null hypothesis is rejected when p < .05 and not rejected when p > .05.
#Confidence intervals of difference parameters not containing 0 imply that there is a statistically significant difference between the populations.

#A plots of toothelength in relation to supplement type
boxplot(ds$len~ds$supp,main="Toothlength in relation to supplement",ylab="length in mm",xlab="supplement type")

boxplot(ds$len~ds$dose,main="Toothlength in relation to dose", ylab="length in mm", xlab="dose in mg/day")

#Check H_0 that the lenght of teeth growth is related to supplement type 
t.test(ds$len ~ ds$supp)
#p > .05 and CI contains 0 => H_0 can not be rejected
#I.e. we can't say that the type of dose have any impact on the average toothgrowth.

#Plot of length in relation to dose and supplement
boxplot(ds$len ~ interaction(ds$dose,ds$supp),main="Toothlength in relation to dose.supp",ylab="length in mm", xlab="dose.supp")

#Now how about the teeth growth in relation to dose and supplement type?
#prepare for t.test
dose05_OJ <- subset(ds,ds$dose==.5 & ds$supp=="OJ")
dose05_VC <- subset(ds,ds$dose==.5 & ds$supp=="VC")
dose1_OJ <- subset(ds,ds$dose==1 & ds$supp=="OJ")
dose1_VC <- subset(ds,ds$dose==1 & ds$supp=="VC")
dose2_OJ <- subset(ds,ds$dose==2 & ds$supp=="OJ")
dose2_VC <- subset(ds,ds$dose==2 & ds$supp=="VC")

#Or length in relation to increase of dose alone, regardless of supplement type?
#prepare for t.test
dose05 <- subset(ds,ds$dose==.5)
dose1 <- subset(ds,ds$dose==1)
dose2 <- subset(ds,ds$dose==2)


#t.test of length in relation to supplement type and dosis

t.test(dose05_OJ$len,dose05_VC$len)
#p < 0.05 and CI doesn't contain 0  => H_0 can be rejected
#I.e. If dose is 0.5 mg/day there seems to be difference in toothgrowth, depending on supplement type
#where OJ seem to be a better catalyst for toothgrowth than VC

t.test(dose1_OJ$len,dose1_VC$len)
#p < 0.05 and CI doesn't contain 0  => H_0 can be rejected
#I.e. If dose is 1 mg/day there seems to be difference in toothgrowth, depending on supplement type
#where OJ seem to be a better catalyst for toothgrowth than VC

t.test(dose2_OJ$len,dose2_VC$len)
#p > 0.05 and CI contain 0  => H_0 can not be rejected
#I.e. If dose is 2 mg/day there don't seem to be difference in toothgrowth, depending on supplement type
#Both OJ and VC seem to be equally good catalyst for toothgrowth


#t.test of length in relation to dosis increase regardless of supplement type
t.test(dose05$len,dose1$len)
#p < 0 and CI doesn't contain 0 => H_0 can be rejected
#I.e. regardless of supplement a dose-increase from 0.5 to 1.0 mg/day seem to increase toothgrowth

t.test(dose1$len,dose2$len)
#p < 0 and CI doesn't contain 0 => H_0 can be rejected
#I.e. regardless of supplement a dose-increase from 1 to 2.0 mg/day seem to increase toothgrowth

#Conclusion:
#1. Giving additives to guinea pigs either in the form of OJ or VC is resulting in an increase of toothgrowth

#2. OJ seems to be slightly better than VC using doses of 0.5 or 1 mg/day. At 2 mg/day, there seems to be no
#significant difference in which supplement you use

#3. Using VC, an increase in daily dose seem to increase toothgrowth in a linear fashion,
#whereas for OJ, the effect seems to drop off the higher the dosis.

#4. More data would improve the results, and hence the conclusions :-)

