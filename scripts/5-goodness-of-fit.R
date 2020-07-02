
library(data.table)
library(ggplot2)
library(gridExtra)
library(fitdistrplus)

dust <- fread("output/all.bats.csv")


# fit the negative binomial distribution
fit <- fitdist(dust$total.inf*10, "gamma")

set.seed(13579)
N <- 10000

y <- dnbinom(N, size = 1.23, prob = 0.1)
hist(y)
hist(dust$total.inf*10)

ks.test(dust$total.inf*10, y)

rnegbin()

hist(y)

binwidth = 0.5
meanDust <- mean(log(dust$total.inf))
sdDust <-  sd(log(dust$total.inf))
n <- nrow(dust)

hist(dust$total.inf, prob=TRUE, breaks=145)

# load library
library(fitdistrplus)

# fit the negative binomial distribution
fit <- fitdist(dust$total.inf, "gamma")

# get the fitted densities. mu and size from fit.
fitG <- dgamma(1:143, shape=1.231371, rate=7.847440)

# add fitted line (blue) to histogram
lines(fitG, lwd="3", col="blue")

# Goodness of fit with the chi squared test  
# get the frequency table
t <- table(round(dust$total.inf, 1))   

# convert to dataframe
df <- as.data.frame(t)

# get frequencies
observed_freq <- df$Freq

# perform the chi-squared test
ks.test(dust$total.inf, fitG)
