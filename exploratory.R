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

seed(123456)

for (i in 1 : iterates) vecMeans = c(vecMeans, mean(rexp(n,lambda)))

#Draw a histogram
hist(vecMeans)

#Task 1. Show the sample mean and compare it to the theoretical mean of the
#distribution.

#The sample mean:
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

mean(ds$len ~ ds$dose==.5)


#Some plots to see if any correlations exists
boxplot(ds$len~ds$supp)
boxplot(ds$len~ds$dose)


t.test(ds$len ~ ds$supp)
#p <> 0 and 0 in Conf.Int => H_0 can not be rejected
t.test(ds$len ~ ds$dose==.5)
#p = 0 and not 0 in Conf.Int => H_0 can be rejected
t.test(ds$len ~ ds$dose==1)
#p <> 0 and 0 in Conf.Int => H_0 can not be rejected
t.test(ds$len ~ ds$dose==2)
#p = 0 and not 0 in Conf.Int => H_0 can be rejected
