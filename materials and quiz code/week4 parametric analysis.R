#designers used Adobe Illustrator or Adobe InDesign to create a benchmark set of classic children's illustrations. The amount of time they took was recorded, in minutes. 
designide = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/designtime.csv")
View(designide)
designide$Subject = factor(designide$Subject) # convert to nominal factor
summary(designide)

library(plyr)
ddply(designide, ~ Tool, function(data) summary(data$Time))
ddply(designide, ~ Tool, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

hist(designide[designide$Tool == "Illustrator",]$Time) # histogram
hist(designide[designide$Tool == "InDesign",]$Time) # histogram
plot(Time ~ Tool, data=designide) # boxplot

# Shapiro-Wilk normality test on response
shapiro.test(designide[designide$Tool == "Illustrator",]$Time) # p = 0.0113
shapiro.test(designide[designide$Tool == "InDesign",]$Time)

# but really what matters most is the residuals
m_1 = aov(Time ~ Tool, data=designide) # fit model
shapiro.test(residuals(m_1)) # test residuals
qqnorm(residuals(m_1)); qqline(residuals(m_1)) # plot residuals

# tests for homoscedasticity (homogeneity of variance)
library(carData)
library(car)
leveneTest(Time ~ Tool, data=designide, center=mean) # Levene's test
leveneTest(Time ~ Tool, data=designide, center=median) # Brown-Forsythe test

# Kolmogorov-Smirnov test for log-normality
# fit the distribution to a lognormal to estimate fit parameters
# then supply those to a K-S test with the lognormal distribution fn (see ?plnorm)
# see ?Distributions for many other named probability distributions
library(MASS)
fit = fitdistr(designide[designide$Tool == "Illustrator",]$Time, "lognormal")$estimate
ks.test(designide[designide$Tool == "Illustrator",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(designide[designide$Tool == "InDesign",]$Time, "lognormal")$estimate
ks.test(designide[designide$Tool == "InDesign",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)


# create a new column in designide defined as log(Time)
designide$logTime = log(designide$Time) # log transform
View(designide) # verify

summary(designide)
library(plyr)
ddply(designide, ~ Tool, function(data) summary(data$Time))
ddply(designide, ~ Tool, summarise, logTime.mean=mean(logTime), logTime.sd=sd(logTime))

# Welch t-test for unequal variances on log transformed time reponse
t.test(logTime ~ Tool, data=designide, var.equal=FALSE) # Welch t-test


## Nonparametric equivalent of independent-samples t-test

# Mann-Whitney U test
library(coin)
wilcox_test(Time ~ Tool, data=designide, distribution="exact")
wilcox_test(logTime ~ Tool, data=designide, distribution="exact") # note: same result