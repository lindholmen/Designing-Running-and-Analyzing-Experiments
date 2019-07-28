## Interaction Design Specialization, Course 7
## Designing, Running, and Analyzing Experiments
##
## Instructor: 
##
##   Jacob O. Wobbrock, Ph.D.
##   The Information School
##   University of Washington
##   Seattle, WA 98195
##   wobbrock@uw.edu

## R notes: Where library(name) is used, you may first need to do install.packages("name")
## to download and install the R package that enables access to that library. To ensure
## you have the latest version of R installed, see http://www.r-statistics.com/tag/installr/


###
## Scenario:   Analyzing user preferences
##
## Statistics: One- and two-sample tests of proportions with chi-square test, 
##             binomial test, multinomial test, G-test, Fisher's exact test
###

## One-sample tests of proportions

# read in a data file with 2 response categories
prefsAB = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/prefsAB.csv")
View(prefsAB)
prefsAB$Subject = factor(prefsAB$Subject) # convert to nominal factor
summary(prefsAB)
plot(prefsAB$Pref)

# Pearson chi-square test
prfs = xtabs( ~ Pref, data=prefsAB)
prfs # show counts
chisq.test(prfs)

# binomial test
binom.test(prfs)

# read in a data file with 3 response categories
prefsABC = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/prefsABC.csv")
View(prefsABC)
prefsABC$Subject = factor(prefsABC$Subject) # convert to nominal factor
summary(prefsABC)
plot(prefsABC$Pref)

# Pearson chi-square test
prfs = xtabs( ~ Pref, data=prefsABC)
prfs # show counts
chisq.test(prfs)

# multinomial test
library(XNomial)
xmulti(prfs, c(1/3, 1/3, 1/3), statName="Prob")

# post hoc binomial tests with correction for multiple comparisons
aa = binom.test(sum(prefsABC$Pref == "A"), nrow(prefsABC), p=1/3)
bb = binom.test(sum(prefsABC$Pref == "B"), nrow(prefsABC), p=1/3)
cc = binom.test(sum(prefsABC$Pref == "C"), nrow(prefsABC), p=1/3)
p.adjust(c(aa$p.value, bb$p.value, cc$p.value), method="holm")


## Two-sample tests of proportions

# revisit our data file with 2 response categories, but now with sex (M/F)
prefsABsex = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/prefsABsex.csv")
View(prefsABsex)
prefsABsex$Subject = factor(prefsABsex$Subject) # convert to nominal factor
summary(prefsABsex)
plot(prefsABsex[prefsABsex$Sex == "M",]$Pref)
plot(prefsABsex[prefsABsex$Sex == "F",]$Pref)

# Pearson chi-square test
prfs = xtabs( ~ Pref + Sex, data=prefsABsex) # the '+' sign indicates two vars
View(prfs)
chisq.test(prfs)

# prepare for loading library RVAideMemoire because the package called mixOmics that was recently pulled from CRAN,

install.packages("igraph")
install.packages("rgl")
install.packages("corpcor")
install.packages("RColorBrewer")
install.packages("dplyr")
install.packages("tidyr")
install.packages("reshape2")
install.packages("matrixStats")
install.packages("gridExtra")
install.packages("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/mixOmics_6.3.2.tar.gz", repos=NULL, type="source")
# G-test, asymptotic like chi-square
install.packages("RVAideMemoire")
library(RVAideMemoire)
G.test(prfs)

# Fisher's exact test
fisher.test(prfs)

# revisit our data file with 3 response categories, but now with sex (M/F)
prefsABCsex = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/prefsABCsex.csv")
View(prefsABCsex)
prefsABCsex$Subject = factor(prefsABCsex$Subject) # convert to nominal factor
summary(prefsABCsex)
plot(prefsABCsex[prefsABCsex$Sex == "M",]$Pref)
plot(prefsABCsex[prefsABCsex$Sex == "F",]$Pref)

# Pearson chi-square test
prfs = xtabs( ~ Pref + Sex, data=prefsABCsex)
View(prfs)
chisq.test(prfs)

# G-test
G.test(prfs)

# Fisher's exact test
fisher.test(prfs)

# manual post hoc binomial tests for (m)ales -- do any prefs for A-C sig. differ from chance for males?
ma = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "M",]$Pref == "A"), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
mb = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "M",]$Pref == "B"), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
mc = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "M",]$Pref == "C"), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
p.adjust(c(ma$p.value, mb$p.value, mc$p.value), method="holm") # correct for multiple comparisons

# manual post hoc binomial tests for (f)emales -- do any prefs for A-C sig. differ from chance for females?
fa = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "F",]$Pref == "A"), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
fb = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "F",]$Pref == "B"), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
fc = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "F",]$Pref == "C"), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
p.adjust(c(fa$p.value, fb$p.value, fc$p.value), method="holm") # correct for multiple comparisons



###
## Scenario:   Comparing pages visited in an A/B test
##
## Statistics: Descriptive statistics, independent-samples t-test
###

## Independent-samples t-test

# read in a data file with page views from an A/B test
pgviews = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/pgviews.csv")
View(pgviews)
pgviews$Subject = factor(pgviews$Subject) # convert to nominal factor
summary(pgviews)
sum(pgviews[pgviews$Site == "A",]$Pages)
sum(pgviews[pgviews$Site == "B",]$Pages)
summary(pgviews[pgviews$Site == "A",])
summary(pgviews[pgviews$Site == "B",])

# descriptive statistics by Site
library(plyr)
ddply(pgviews, ~ Site, function(data) summary(data$Pages))
ddply(pgviews, ~ Site, summarise, Pages.mean=mean(Pages), Pages.sd=sd(Pages))

# graph histograms and a boxplot
hist(pgviews[pgviews$Site == "A",]$Pages)
hist(pgviews[pgviews$Site == "B",]$Pages)
plot(Pages ~ Site, data=pgviews)

# independent-samples t-test
t.test(Pages ~ Site, data=pgviews, var.equal=TRUE)



###
## Scenario:   Comparing task completion times in authoring tools
##
## Statistics: ANOVA assumptions, data transformations, one-way ANOVA, 
##             post hoc comparisons, nonparametric tests
###

## Independent-samples t-test

# read in a data file with task completion times (min) from 2 programming tools
ide2 = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/ide2.csv")
View(ide2)
ide2$Subject = factor(ide2$Subject) # convert to nominal factor
summary(ide2)

# view descriptive statistics by IDE
library(plyr)
ddply(ide2, ~ IDE, function(data) summary(data$Time))
ddply(ide2, ~ IDE, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

# graph histograms and a boxplot
hist(ide2[ide2$IDE == "VStudio",]$Time) # histogram
hist(ide2[ide2$IDE == "Eclipse",]$Time) # histogram
plot(Time ~ IDE, data=ide2) # boxplot

# independent-samples t-test (suitable? maybe not, because...)
t.test(Time ~ IDE, data=ide2, var.equal=TRUE)


## Testing ANOVA assumptions

# Shapiro-Wilk normality test on response
shapiro.test(ide2[ide2$IDE == "VStudio",]$Time)
shapiro.test(ide2[ide2$IDE == "Eclipse",]$Time)

# but really what matters most is the residuals
m = aov(Time ~ IDE, data=ide2) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# Kolmogorov-Smirnov test for log-normality
# fit the distribution to a lognormal to estimate fit parameters
# then supply those to a K-S test with the lognormal distribution fn (see ?plnorm)
# see ?Distributions for many other named probability distributions
library(MASS)
fit = fitdistr(ide2[ide2$IDE == "VStudio",]$Time, "lognormal")$estimate
ks.test(ide2[ide2$IDE == "VStudio",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(ide2[ide2$IDE == "Eclipse",]$Time, "lognormal")$estimate
ks.test(ide2[ide2$IDE == "Eclipse",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)

# tests for homoscedasticity (homogeneity of variance)
library(car)
leveneTest(Time ~ IDE, data=ide2, center=mean) # Levene's test
leveneTest(Time ~ IDE, data=ide2, center=median) # Brown-Forsythe test

# Welch t-test for unequal variances handles
# the violation of homoscedasticity. but not
# the violation of normality.
t.test(Time ~ IDE, data=ide2, var.equal=FALSE) # Welch t-test


## Data transformation

# create a new column in ide2 defined as log(Time)
ide2$logTime = log(ide2$Time) # log transform
View(ide2) # verify

# explore for intuition-building
hist(ide2[ide2$IDE == "VStudio",]$logTime) # histogram
hist(ide2[ide2$IDE == "Eclipse",]$logTime) # histogram
plot(logTime ~ IDE, data=ide2) # boxplot

# re-test for normality
shapiro.test(ide2[ide2$IDE == "VStudio",]$logTime)
shapiro.test(ide2[ide2$IDE == "Eclipse",]$logTime)
m = aov(logTime ~ IDE, data=ide2) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# re-test for homoscedasticity
library(car)
leveneTest(logTime ~ IDE, data=ide2, center=median) # Brown-Forsythe test

# independent-samples t-test (now suitable for logTime)
t.test(logTime ~ IDE, data=ide2, var.equal=TRUE)


## Nonparametric equivalent of independent-samples t-test

# Mann-Whitney U test
library(coin)
wilcox_test(Time ~ IDE, data=ide2, distribution="exact")
wilcox_test(logTime ~ IDE, data=ide2, distribution="exact") # note: same result


## One-way ANOVA

# read in a data file with task completion times (min) now from 3 tools
ide3 = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/ide3.csv")
View(ide3)
ide3$Subject = factor(ide3$Subject) # convert to nominal factor
summary(ide3)

# view descriptive statistics by IDE
library(plyr)
ddply(ide3, ~ IDE, function(data) summary(data$Time))
ddply(ide3, ~ IDE, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

# explore new response distribution
hist(ide3[ide3$IDE == "VStudio",]$Time)
hist(ide3[ide3$IDE == "Eclipse",]$Time)
hist(ide3[ide3$IDE == "PyCharm",]$Time) # new one
plot(Time ~ IDE, data=ide3) # boxplot

# test normality for new IDE
shapiro.test(ide3[ide3$IDE == "PyCharm",]$Time)
m = aov(Time ~ IDE, data=ide3) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# test log-normality of new IDE
library(MASS)
fit = fitdistr(ide3[ide3$IDE == "PyCharm",]$Time, "lognormal")$estimate
ks.test(ide3[ide3$IDE == "PyCharm",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE) # lognormality

# compute new log(Time) column and re-test
ide3$logTime = log(ide3$Time) # add new column
View(ide3) # verify
shapiro.test(ide3[ide3$IDE == "PyCharm",]$logTime)
m = aov(logTime ~ IDE, data=ide3) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# test homoscedasticity
library(car)
leveneTest(logTime ~ IDE, data=ide3, center=median) # Brown-Forsythe test

# one-way ANOVA, suitable now to logTime
m = aov(logTime ~ IDE, data=ide3) # fit model
anova(m) # report anova

# post hoc independent-samples t-tests
plot(Time ~ IDE, data=ide3) # for convenience
library(multcomp)
summary(glht(m, mcp(IDE="Tukey")), test=adjusted(type="holm")) # Tukey means compare all pairs
# note: equivalent to this using emm instead of mcp
library(emmeans)
summary(glht(m, emm(pairwise ~ IDE)), test=adjusted(type="holm"))


## Nonparametric equivalent of one-way ANOVA

# Kruskal-Wallis test
library(coin)
kruskal_test(Time ~ IDE, data=ide3, distribution="asymptotic") # can't do exact with 3 levels
kruskal_test(logTime ~ IDE, data=ide3, distribution="asymptotic") # note: same result
# for reporting Kruskal-Wallis as chi-square, we can get N with nrow(ide3)

# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
vs.ec = wilcox.test(ide3[ide3$IDE == "VStudio",]$Time, ide3[ide3$IDE == "Eclipse",]$Time, exact=FALSE)
vs.py = wilcox.test(ide3[ide3$IDE == "VStudio",]$Time, ide3[ide3$IDE == "PyCharm",]$Time, exact=FALSE)
ec.py = wilcox.test(ide3[ide3$IDE == "Eclipse",]$Time, ide3[ide3$IDE == "PyCharm",]$Time, exact=FALSE)
p.adjust(c(vs.ec$p.value, vs.py$p.value, ec.py$p.value), method="holm")

# alternative approach is using PMCMR for nonparam pairwise comparisons
library(PMCMR)
posthoc.kruskal.conover.test(Time ~ IDE, data=ide3, p.adjust.method="holm") # Conover & Iman (1979)



###
## Scenario:   Finding contacts in a smartphone contacts manager
##
## Statistics: Paired-samples t-test, one-way repeated measures ANOVA, 
##             Mauchly's test of sphericity, post hoc comparisons, nonparametrics
###

## Paired-samples t-test

# read in a data file with times (sec) to find a set of contacts
srchscrl = read.csv("srchscrl.csv")
View(srchscrl)
srchscrl$Subject = factor(srchscrl$Subject) # convert to a nominal factor
srchscrl$Order = factor(srchscrl$Order) # convert to a nominal factor
summary(srchscrl)

# view descriptive statistics by Technique
library(plyr)
ddply(srchscrl, ~ Technique, function(data) summary(data$Time))
ddply(srchscrl, ~ Technique, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

# explore the Time response
hist(srchscrl[srchscrl$Technique == "Search",]$Time) # histogram
hist(srchscrl[srchscrl$Technique == "Scroll",]$Time) # histogram
plot(Time ~ Technique, data=srchscrl) # boxplot

# test anova assumptions
shapiro.test(srchscrl[srchscrl$Technique == "Search",]$Time) # Shapiro-Wilk
shapiro.test(srchscrl[srchscrl$Technique == "Scroll",]$Time)

# fit a model for testing residuals -- the Error fn is used
# to indicate within-Ss effects, i.e., each Subject was
# exposed to all levels of Technique. generally, Error(S/(A*B*C))
# means each S was exposed to every level of A, B, C and S
# is a column encoding subject ids.
m = aov(Time ~ Technique + Error(Subject/Technique), data=srchscrl)
# we get residuals for Subject
shapiro.test(residuals(m$Subject))
qqnorm(residuals(m$Subject)) 
qqline(residuals(m$Subject))
# and also for Subject:Technique
shapiro.test(residuals(m$`Subject:Technique`))
qqnorm(residuals(m$`Subject:Technique`))
qqline(residuals(m$`Subject:Technique`))

# homoscedasticity
library(car)
leveneTest(Time ~ Technique, data=srchscrl, center=median) # Brown-Forsythe test

# now test for an order effect -- did counterbalancing work?
library(reshape2)	
# for a paired-samples t-test we must use a wide-format table; most
# R fns do not require a wide-format table, but the dcast function
# offers a quick way to translate long-format into wide-format when
# we need it.
srchscrl.wide.order = dcast(srchscrl, Subject ~ Order, value.var="Time") # go wide
View(srchscrl.wide.order) # verify
t.test(srchscrl.wide.order$"1", srchscrl.wide.order$"2", paired=TRUE, var.equal=TRUE)

# finally, the paired-samples t-test
srchscrl.wide.tech = dcast(srchscrl, Subject ~ Technique, value.var="Time") # go wide
View(srchscrl.wide.tech)
t.test(srchscrl.wide.tech$Search, srchscrl.wide.tech$Scroll, paired=TRUE, var.equal=TRUE)
plot(Time ~ Technique, data=srchscrl) # confirm


## Nonparametric equivalent of paired-samples t-test

# explore the Errors response; error counts are often Poisson
library(plyr)
ddply(srchscrl, ~ Technique, function(data) summary(data$Errors))
ddply(srchscrl, ~ Technique, summarise, Errors.mean=mean(Errors), Errors.sd=sd(Errors))

hist(srchscrl[srchscrl$Technique == "Search",]$Errors) # histogram
hist(srchscrl[srchscrl$Technique == "Scroll",]$Errors) # histogram
plot(Errors ~ Technique, data=srchscrl) # boxplot

# we might once again test ANOVA assumptions (normality, homoscedasticity)
# but we have now covered that amply, so we'll omit those steps until there's 
# something new to be learned. remember, they're guidelines anyway, not law.

# try to fit a Poisson distribution for count data. note that ks.test 
# only works for continuous distributions, but Poisson distributions 
# are discrete, so use fitdist, not fitdistr, and test with gofstat.
library(fitdistrplus)
fit = fitdist(srchscrl[srchscrl$Technique == "Search",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(srchscrl[srchscrl$Technique == "Scroll",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test

# Wilcoxon signed-rank test on Errors
library(coin)
wilcoxsign_test(Errors ~ Technique | Subject, data=srchscrl, distribution="exact")
# note: the term afer the "|" indicates the within-Ss blocking term for matched pairs

# now also examine Effort, the ordinal Likert scale response (1-7)
library(plyr)
ddply(srchscrl, ~ Technique, function(data) summary(data$Effort))
ddply(srchscrl, ~ Technique, summarise, Effort.mean=mean(Effort), Effort.sd=sd(Effort))
hist(srchscrl[srchscrl$Technique == "Search",]$Effort, breaks=c(1:7), xlim=c(1,7)) # histogram
hist(srchscrl[srchscrl$Technique == "Scroll",]$Effort, breaks=c(1:7), xlim=c(1,7)) # histogram
plot(Effort ~ Technique, data=srchscrl, ylim=c(1,7)) # boxplot

# our response is ordinal within-Ss, so use nonparametric Wilcoxon signed-rank
library(coin)
wilcoxsign_test(Effort ~ Technique | Subject, data=srchscrl, distribution="exact")


## One-way repeated measures ANOVA

# read in a data file now with a third method, voice recognition
srchscrlvce = read.csv("srchscrlvce.csv")
View(srchscrlvce)
srchscrlvce$Subject = factor(srchscrlvce$Subject) # convert to nominal factor
srchscrlvce$Order = factor(srchscrlvce$Order) # convert to nominal factor
summary(srchscrlvce)

# view descriptive statistics by Technique
library(plyr)
ddply(srchscrlvce, ~ Technique, function(data) summary(data$Time))
ddply(srchscrlvce, ~ Technique, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

# graph histograms and boxplot
hist(srchscrlvce[srchscrlvce$Technique == "Search",]$Time)
hist(srchscrlvce[srchscrlvce$Technique == "Scroll",]$Time)
hist(srchscrlvce[srchscrlvce$Technique == "Voice",]$Time) # new one
plot(Time ~ Technique, data=srchscrlvce) # boxplot

# repeated measures ANOVA
library(ez)
# ez lets us specify the dependent variable (Time), within-Ss 
# variables (Technique), and the variable that identifies 
# subjects (Subject).
m = ezANOVA(dv=Time, within=Technique, wid=Subject, data=srchscrlvce)
# we then check the model for violations of sphericity. Sphericity is 
# the situation where the variances of the differences between all 
# combinations of levels of a within-Ss factor are equal. It always
# holds for within-Ss factors that have just 2 levels, but for 3+
# levels, sphericity can be tested with Mauchly's Test of Sphericity.
m$Mauchly # p<.05 indicates a violation
# if no violation, examine the uncorrected ANOVA in m$ANOVA. 
# if violation, instead look at m$Sphericity and use the 
# Greenhouse-Geisser correction, GGe.
m$ANOVA
# include the corrected DFs for each corrected effect
pos = match(m$`Sphericity Corrections`$Effect, m$ANOVA$Effect) # positions of within-Ss efx in m$ANOVA
m$Sphericity$GGe.DFn = m$Sphericity$GGe * m$ANOVA$DFn[pos] # Greenhouse-Geisser
m$Sphericity$GGe.DFd = m$Sphericity$GGe * m$ANOVA$DFd[pos]
m$Sphericity$HFe.DFn = m$Sphericity$HFe * m$ANOVA$DFn[pos] # Huynh-Feldt
m$Sphericity$HFe.DFd = m$Sphericity$HFe * m$ANOVA$DFd[pos]
m$Sphericity # show results

# the same uncorrected results are given by
m = aov(Time ~ Technique + Error(Subject/Technique), data=srchscrlvce) # fit model
summary(m) # show anova

# manual post hoc pairwise comparisons with paired-samples t-tests
library(reshape2)	
srchscrlvce.wide.tech = dcast(srchscrlvce, Subject ~ Technique, value.var="Time") # go wide
View(srchscrlvce.wide.tech)
se.sc = t.test(srchscrlvce.wide.tech$Search, srchscrlvce.wide.tech$Scroll, paired=TRUE)
se.vc = t.test(srchscrlvce.wide.tech$Search, srchscrlvce.wide.tech$Voice, paired=TRUE)
sc.vc = t.test(srchscrlvce.wide.tech$Scroll, srchscrlvce.wide.tech$Voice, paired=TRUE)
p.adjust(c(se.sc$p.value, se.vc$p.value, sc.vc$p.value), method="holm")


## Nonparametric equivalent of one-way repeated measures ANOVA

# first, examine Errors for 3 techniques
library(plyr)
ddply(srchscrlvce, ~ Technique, function(data) summary(data$Errors))
ddply(srchscrlvce, ~ Technique, summarise, Errors.mean=mean(Errors), Errors.sd=sd(Errors))
hist(srchscrlvce[srchscrlvce$Technique == "Search",]$Errors)
hist(srchscrlvce[srchscrlvce$Technique == "Scroll",]$Errors)
hist(srchscrlvce[srchscrlvce$Technique == "Voice",]$Errors) # new one
plot(Errors ~ Technique, data=srchscrlvce) # boxplot

# are the Voice error counts possibly Poisson distributed 
# as they seemed for Scroll and Search?
library(fitdistrplus)
fit = fitdist(srchscrlvce[srchscrlvce$Technique == "Voice",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test

# Friedman test on Errors
library(coin)
friedman_test(Errors ~ Technique | Subject, data=srchscrlvce, distribution="asymptotic")

# manual post hoc Wilcoxon signed-rank test multiple comparisons
se.sc = wilcox.test(srchscrlvce[srchscrlvce$Technique == "Search",]$Errors, srchscrlvce[srchscrlvce$Technique == "Scroll",]$Errors, paired=TRUE, exact=FALSE)
se.vc = wilcox.test(srchscrlvce[srchscrlvce$Technique == "Search",]$Errors, srchscrlvce[srchscrlvce$Technique == "Voice",]$Errors, paired=TRUE, exact=FALSE)
sc.vc = wilcox.test(srchscrlvce[srchscrlvce$Technique == "Scroll",]$Errors, srchscrlvce[srchscrlvce$Technique == "Voice",]$Errors, paired=TRUE, exact=FALSE)
p.adjust(c(se.sc$p.value, se.vc$p.value, sc.vc$p.value), method="holm")

# alternative approach is using PMCMR for nonparam pairwise comparisons
library(PMCMR)
posthoc.friedman.conover.test(srchscrlvce$Errors, srchscrlvce$Technique, srchscrlvce$Subject, p.adjust.method="holm") # Conover (1999)

# second, examine Effort Likert-scale ratings for all 3 techniques
library(plyr)
ddply(srchscrlvce, ~ Technique, function(data) summary(data$Effort))
ddply(srchscrlvce, ~ Technique, summarise, Effort.mean=mean(Effort), Effort.sd=sd(Effort))
hist(srchscrlvce[srchscrlvce$Technique == "Search",]$Effort, breaks=c(1:7), xlim=c(1,7))
hist(srchscrlvce[srchscrlvce$Technique == "Scroll",]$Effort, breaks=c(1:7), xlim=c(1,7))
hist(srchscrlvce[srchscrlvce$Technique == "Voice",]$Effort, breaks=c(1:7), xlim=c(1,7)) # new one
plot(Effort ~ Technique, data=srchscrlvce) # boxplot

# Friedman test on Effort
library(coin)
friedman_test(Effort ~ Technique | Subject, data=srchscrlvce, distribution="asymptotic")
# note! this omnibus test is not significant so post hoc comparisons are not justified.
# if we were to do them, we would use a set of 3 wilcoxon signed-rank tests corrected
# with Holm's sequential Bonferroni correction, just as we did for Errors, above.



###
## Scenario:   Text entry on smartphone keyboards in different postures
##
## Statistics: Factorial ANOVA, repeated measures ANOVA, main effects, 
##             interaction effects, the Aligned Rank Transform for 
##             "nonparametric ANOVAs"
###

# Mixed Factorial ANOVA on WPM
# Note: "Mixed" here is not "mixed effects" as in LMMs.
# Those will be used farther down below. "Mixed" here is 
# mixing between-Ss and within-Ss factors. By contrast, 
# "mixed" in LMMs is mixing fixed and random effects,
# which we'll cover later. Mixed factorial designs are 
# also called "mixed designs" or "split-plot designs."
# It is easy to extrapolate to purely between-Ss or 
# within-Ss factorial designs from what we do here.

# read in data file of smartphone text entry by 24 people
mbltxt = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/mbltxt.csv")
View(mbltxt)
mbltxt$Subject = factor(mbltxt$Subject) # convert to nominal factor
mbltxt$Posture_Order = factor(mbltxt$Posture_Order) # convert to nominal factor
summary(mbltxt)

# explore the WPM data
library(plyr)
ddply(mbltxt, ~ Keyboard * Posture, function(data) summary(data$WPM))
ddply(mbltxt, ~ Keyboard * Posture, summarise, WPM.mean=mean(WPM), WPM.sd=sd(WPM))

# histograms for two factors
hist(mbltxt[mbltxt$Keyboard == "iPhone" & mbltxt$Posture == "Sit",]$WPM)
hist(mbltxt[mbltxt$Keyboard == "iPhone" & mbltxt$Posture == "Stand",]$WPM)
hist(mbltxt[mbltxt$Keyboard == "iPhone" & mbltxt$Posture == "Walk",]$WPM)
hist(mbltxt[mbltxt$Keyboard == "Galaxy" & mbltxt$Posture == "Sit",]$WPM)
hist(mbltxt[mbltxt$Keyboard == "Galaxy" & mbltxt$Posture == "Stand",]$WPM)
hist(mbltxt[mbltxt$Keyboard == "Galaxy" & mbltxt$Posture == "Walk",]$WPM)
boxplot(WPM ~ Keyboard * Posture, data=mbltxt, xlab="Keyboard.Posture", ylab="WPM") # boxplots
with(mbltxt, interaction.plot(Posture, Keyboard, WPM, ylim=c(0, max(mbltxt$WPM)))) # interaction plot

# test for a Posture order effect to ensure counterbalancing worked
library(ez)
m = ezANOVA(dv=WPM, between=Keyboard, within=Posture_Order, wid=Subject, data=mbltxt)
m$Mauchly # n.s.
m$ANOVA 

# now perform the two-way mixed factorial repeated measures ANOVA
m = ezANOVA(dv=WPM, between=Keyboard, within=Posture, wid=Subject, data=mbltxt)
m$Mauchly # sig. so use GGe correction
 
# note: "ges" in m$ANOVA is the generalized eta-squared measure
# of effect size, preferred to eta-squared or partial eta-squared. 
# see Bakeman (2005) in the References at ?ezANOVA.
# Now compute the corrected DFs for each corrected effect
pos = match(m$`Sphericity Corrections`$Effect, m$ANOVA$Effect) # positions of within-Ss efx in m$ANOVA
m$Sphericity$GGe.DFn = m$Sphericity$GGe * m$ANOVA$DFn[pos] # Greenhouse-Geisser
m$Sphericity$GGe.DFd = m$Sphericity$GGe * m$ANOVA$DFd[pos]
m$Sphericity$HFe.DFn = m$Sphericity$HFe * m$ANOVA$DFn[pos] # Huynh-Feldt
m$Sphericity$HFe.DFd = m$Sphericity$HFe * m$ANOVA$DFd[pos]
m$Sphericity # show results

# for completeness, note the above's uncorrected results are the same as from this
m = aov(WPM ~ Keyboard * Posture + Error(Subject/Posture), data=mbltxt) # fit model
summary(m) # show table

# manual post hoc pairwise comparisons in light of sig. interaction
library(reshape2)
mbltxt.wide = dcast(mbltxt, Subject + Keyboard ~ Posture, value.var="WPM") # go wide
View(mbltxt.wide)
sit = t.test(mbltxt.wide$Sit ~ Keyboard, data=mbltxt.wide) # iPhone vs. Galaxy WPM sitting
std = t.test(mbltxt.wide$Stand ~ Keyboard, data=mbltxt.wide) # iPhone vs. Galaxy WPM standing
wlk = t.test(mbltxt.wide$Walk ~ Keyboard, data=mbltxt.wide) # iPhone vs. Galaxy WPM walking
p.adjust(c(sit$p.value, std$p.value, wlk$p.value), method="holm")

# just curious: also compare iPhone 'sit' and 'walk'
t.test(mbltxt.wide[mbltxt.wide$Keyboard == "iPhone",]$Sit, mbltxt.wide[mbltxt.wide$Keyboard == "iPhone",]$Walk, paired=TRUE)
boxplot(mbltxt.wide[mbltxt.wide$Keyboard == "iPhone",]$Sit, mbltxt.wide[mbltxt.wide$Keyboard == "iPhone",]$Walk,xlab="iPhone.Sit vs. iPhone.Walk", ylab="WPM") # custom boxplot


## Nonparametric approach to factorial ANOVA
## The Aligned Rank Transform (ART) procedure
## http://depts.washington.edu/madlab/proj/art/ 

# explore the Error_Rate data
library(plyr)
ddply(mbltxt, ~ Keyboard * Posture, function(data) summary(data$Error_Rate))
ddply(mbltxt, ~ Keyboard * Posture, summarise, Error_Rate.mean=mean(Error_Rate), Error_Rate.sd=sd(Error_Rate))

# histograms, boxplots, and interaction plot
hist(mbltxt[mbltxt$Keyboard == "iPhone" & mbltxt$Posture == "Sit",]$Error_Rate)
hist(mbltxt[mbltxt$Keyboard == "iPhone" & mbltxt$Posture == "Stand",]$Error_Rate)
hist(mbltxt[mbltxt$Keyboard == "iPhone" & mbltxt$Posture == "Walk",]$Error_Rate)
hist(mbltxt[mbltxt$Keyboard == "Galaxy" & mbltxt$Posture == "Sit",]$Error_Rate)
hist(mbltxt[mbltxt$Keyboard == "Galaxy" & mbltxt$Posture == "Stand",]$Error_Rate)
hist(mbltxt[mbltxt$Keyboard == "Galaxy" & mbltxt$Posture == "Walk",]$Error_Rate)
boxplot(Error_Rate ~ Keyboard * Posture, data=mbltxt, xlab="Keyboard.Posture", ylab="Error_Rate") # boxplots
with(mbltxt, interaction.plot(Posture, Keyboard, Error_Rate, ylim=c(0, max(mbltxt$Error_Rate)))) # interaction?

# Aligned Rank Transform on Error_Rate
library(ARTool) # for art, artlm
m = art(Error_Rate ~ Keyboard * Posture + (1|Subject), data=mbltxt) # uses LMM
anova(m) # report anova
shapiro.test(residuals(m)) # normality?
qqnorm(residuals(m)); qqline(residuals(m)) # seems to conform

# conduct post hoc pairwise comparisons within each factor
with(mbltxt, interaction.plot(Posture, Keyboard, Error_Rate, ylim=c(0, max(mbltxt$Error_Rate)))) # for convenience
library(emmeans) # for emmeans
emmeans(artlm(m, "Keyboard"), pairwise ~ Keyboard)
emmeans(artlm(m, "Posture"), pairwise ~ Posture)
#emmeans(artlm(m, "Keyboard : Posture"), pairwise ~ Keyboard : Posture) # don't do this in ART!

# the above contrast-testing method is invalid for cross-factor pairwise comparisons in ART.
# and you can't just grab aligned-ranks for manual t-tests. instead, use testInteractions 
# from the phia package to perform "interaction contrasts." see vignette("art-contrasts").
library(phia)
testInteractions(artlm(m, "Keyboard:Posture"), pairwise=c("Keyboard", "Posture"), adjustment="holm")
# in the output, A-B : C-D is interpreted as a difference-of-differences, i.e., the difference 
# between (A-B | C) and (A-B | D). in words, is the difference between A and B significantly 
# different in condition C from condition D?



###
## Scenario:   Revisiting earlier data with nominal, ordinal, and count responses
##
## Statistics: Generalized Linear Models (GLM) for nominal logistic regression, 
##             ordinal logistic regression, and Poisson regression
###

# Generalized Linear Models (GLM) extend Linear Models (LM) for studies 
# with between-Ss factors to acommodate nominal (incl. binomial) or ordinal 
# responses, or with non-normal response distributions (e.g., Poisson, 
# exponential, gamma). All GLMs have a distribution and a link fn relating 
# their factors to their response. The GLM generalizes the LM, which is a 
# GLM with a normal distribution and "identity" link fn. See, e.g., 
# http://en.wikipedia.org/wiki/Generalized_linear_model

## GLM 1: Nominal logistic regression for preference responses
## -----  Multinomial distribution w/ logit link fn

# re-read our data showing preferences by sex
prefsABCsex.2 = read.csv("prefsABCsex.csv") # revisiting so add ".2"
View(prefsABCsex.2)
prefsABCsex.2$Subject = factor(prefsABCsex.2$Subject) # convert to nominal factor
summary(prefsABCsex.2)
plot(prefsABCsex.2[prefsABCsex.2$Sex == "M",]$Pref)
plot(prefsABCsex.2[prefsABCsex.2$Sex == "F",]$Pref)

# analyze Pref by Sex with multinomial logistic regression,
# also sometimes called nominal logistic regression
library(nnet) # for multinom
library(car) # for Anova
# set sum-to-zero contrasts for the Anova call
contrasts(prefsABCsex.2$Sex) <- "contr.sum"
m = multinom(Pref ~ Sex, data=prefsABCsex.2) # multinomial logistic
Anova(m, type=3) # note: not "anova" from stats pkg
# note: if Pref had only had two response categories, we might use 
# binomial regression, which uses the same syntax as Poisson regression 
# below, but with family=binomial.

# recall our testing from before to see which preferences by males were
# significantly different from chance (answer: really liked C).
ma = binom.test(sum(prefsABCsex.2[prefsABCsex.2$Sex == "M",]$Pref == "A"), nrow(prefsABCsex.2[prefsABCsex.2$Sex == "M",]), p=1/3)
mb = binom.test(sum(prefsABCsex.2[prefsABCsex.2$Sex == "M",]$Pref == "B"), nrow(prefsABCsex.2[prefsABCsex.2$Sex == "M",]), p=1/3)
mc = binom.test(sum(prefsABCsex.2[prefsABCsex.2$Sex == "M",]$Pref == "C"), nrow(prefsABCsex.2[prefsABCsex.2$Sex == "M",]), p=1/3)
p.adjust(c(ma$p.value, mb$p.value, mc$p.value), method="holm") # correct for multiple comparisons

# and for females, their preferences differed significantly from 
# chance for a different choice (answer: really disliked A).
fa = binom.test(sum(prefsABCsex.2[prefsABCsex.2$Sex == "F",]$Pref == "A"), nrow(prefsABCsex.2[prefsABCsex.2$Sex == "F",]), p=1/3)
fb = binom.test(sum(prefsABCsex.2[prefsABCsex.2$Sex == "F",]$Pref == "B"), nrow(prefsABCsex.2[prefsABCsex.2$Sex == "F",]), p=1/3)
fc = binom.test(sum(prefsABCsex.2[prefsABCsex.2$Sex == "F",]$Pref == "C"), nrow(prefsABCsex.2[prefsABCsex.2$Sex == "F",]), p=1/3)
p.adjust(c(fa$p.value, fb$p.value, fc$p.value), method="holm") # correct for multiple comparisons


## GLM 2: Ordinal logistic regression for Likert responses
## -----  Multinomial distribution w/ cumulative logit link fn

# re-read our data showing Effort Likert rating for finding contacts on a smartphone
srchscrlvce.2 = read.csv("srchscrlvce.csv") # revisiting so add ".2"
View(srchscrlvce.2)
srchscrlvce.2$Subject = (1:nrow(srchscrlvce.2)) # recode as between-Ss study
srchscrlvce.2$Subject = factor(srchscrlvce.2$Subject) # convert to nominal factor
srchscrlvce.2$Order = NULL # drop order, n/a for between-Ss 
View(srchscrlvce.2) # verify
summary(srchscrlvce.2)

# re-familiarize ourselves with the Effort Likert response
library(plyr)
ddply(srchscrlvce.2, ~ Technique, function(data) summary(data$Effort))
ddply(srchscrlvce.2, ~ Technique, summarise, Effort.mean=mean(Effort), Effort.sd=sd(Effort))
hist(srchscrlvce.2[srchscrlvce.2$Technique == "Search",]$Effort, breaks=c(1:7), xlim=c(1,7))
hist(srchscrlvce.2[srchscrlvce.2$Technique == "Scroll",]$Effort, breaks=c(1:7), xlim=c(1,7))
hist(srchscrlvce.2[srchscrlvce.2$Technique == "Voice",]$Effort, breaks=c(1:7), xlim=c(1,7))
plot(Effort ~ Technique, data=srchscrlvce.2) # boxplot

# analyze Effort Likert ratings by Technique with ordinal logistic regression
library(MASS) # for polr
library(car) # for Anova
srchscrlvce.2$Effort = ordered(srchscrlvce.2$Effort) # must be an ordinal response
# set sum-to-zero contrasts for the Anova call
contrasts(srchscrlvce.2$Technique) <- "contr.sum"
m = polr(Effort ~ Technique, data=srchscrlvce.2, Hess=TRUE) # ordinal logistic
Anova(m, type=3) # n.s.

# post hoc pairwise comparisons are NOT justified due to lack of sig.
# but here's how we would do them, just for completeness
library(multcomp)
summary(glht(m, mcp(Technique="Tukey")), test=adjusted(type="holm")) # Tukey means compare all pairs
library(lsmeans) # equivalent way using lsmeans, pairs, and as.glht (Note: no emmeans here b/c Effort is ordinal)
summary(lsmeans::as.glht(pairs(lsmeans::lsmeans(m, pairwise ~ Technique))), test=adjusted(type="holm"))


## GLM 3: Poisson regression for count responses
## -----  Poisson distribution w/ log link fn

# our data also has an "Errors" response, count data
# re-familiarize ourselves with the Errors response
library(plyr)
ddply(srchscrlvce.2, ~ Technique, function(data) summary(data$Errors))
ddply(srchscrlvce.2, ~ Technique, summarise, Errors.mean=mean(Errors), Errors.sd=sd(Errors))
hist(srchscrlvce.2[srchscrlvce.2$Technique == "Search",]$Errors)
hist(srchscrlvce.2[srchscrlvce.2$Technique == "Scroll",]$Errors)
hist(srchscrlvce.2[srchscrlvce.2$Technique == "Voice",]$Errors)
plot(Errors ~ Technique, data=srchscrlvce.2) # boxplot

# re-verify that these data are Poisson-distributed
library(fitdistrplus)
fit = fitdist(srchscrlvce.2[srchscrlvce.2$Technique == "Search",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(srchscrlvce.2[srchscrlvce.2$Technique == "Scroll",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(srchscrlvce.2[srchscrlvce.2$Technique == "Voice",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test

# analyze using Poisson regression
# set sum-to-zero contrasts for the Anova call
contrasts(srchscrlvce.2$Technique) <- "contr.sum"
# family parameter identifies both distribution and link fn
m = glm(Errors ~ Technique, data=srchscrlvce.2, family=poisson)
Anova(m, type=3)
qqnorm(residuals(m)); qqline(residuals(m)) # s'ok! Poisson regression makes no normality assumption

# conduct pairwise comparisons among levels of Technique
library(multcomp)
summary(glht(m, mcp(Technique="Tukey")), test=adjusted(type="holm")) # Tukey means compare all pairs



###
## Scenario:   Revisiting our mobile text entry data, now with all 1440
##             trials included -- no averaging -- and using models with 
##             fixed and random effects, called mixed effects models.
##
## Statistics: Linear Mixed Models (LMM) and Generalized Linear Mixed 
##             Models (GLMM) extending LMs and GLMs, respectively, with 
##             random effects to handle within-Ss factors.
###

# Linear Mixed Models (LMM) do everything Linear Models (LM) do but
# can have both fixed and random effects. Random effects allow us
# to handle within-Ss factors by modeling "Subject" as a random
# effect. Generalized Linear Mixed Models (GLMM) do everything 
# Generalized Linear Models (GLM) do, but also can have both fixed 
# and random effects. LMMs and GLMMs are called "mixed effects 
# models." See https://en.wikipedia.org/wiki/Generalized_linear_mixed_model

## Linear Mixed Model (LMM) on WPM

# read in data file of smartphone text entry by 24 people, but now
# it has every single trial performed, not averaged over trials.
mbltxttrials = read.csv("mbltxttrials.csv")
View(mbltxttrials)
mbltxttrials$Subject = factor(mbltxttrials$Subject) # convert to nominal factor
mbltxttrials$Posture_Order = factor(mbltxttrials$Posture_Order) # convert to nominal factor
mbltxttrials$Trial = factor(mbltxttrials$Trial) # convert to nominal factor
summary(mbltxttrials)

# explore the WPM data
library(plyr)
ddply(mbltxttrials, ~ Keyboard * Posture, function(data) summary(data$WPM))
ddply(mbltxttrials, ~ Keyboard * Posture, summarise, WPM.mean=mean(WPM), WPM.sd=sd(WPM))

# histograms for two factors
hist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Sit",]$WPM)
hist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Stand",]$WPM)
hist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Walk",]$WPM)
hist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Sit",]$WPM)
hist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Stand",]$WPM)
hist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Walk",]$WPM)
boxplot(WPM ~ Keyboard * Posture, data=mbltxttrials, xlab="Keyboard.Posture", ylab="WPM") # boxplots
with(mbltxttrials, interaction.plot(Posture, Keyboard, WPM, ylim=c(0, max(mbltxttrials$WPM)))) # interaction?

# libraries for LMMs we'll use on WPM
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova

# set sum-to-zero contrasts for the Anova calls
contrasts(mbltxttrials$Keyboard) <- "contr.sum"
contrasts(mbltxttrials$Posture) <- "contr.sum"
contrasts(mbltxttrials$Posture_Order) <- "contr.sum"
contrasts(mbltxttrials$Trial) <- "contr.sum"

# LMM order effect test
# Keyboard, Posture_Order, Keyboard:Posture_Order 
# and Trial are all fixed effects. Trial is nested 
# within Keyboard, Posture_Order. Subject is a 
# random effect.
m = lmer(WPM ~ (Keyboard * Posture_Order)/Trial + (1|Subject), data=mbltxttrials)
Anova(m, type=3, test.statistic="F")

# main LMM test on WPM
# Keyboard, Posture, Keyboard:Posture and Trial are
# all fixed effects. Trial is nested within 
# Keyboard, Posture. Subject is a random effect.
m = lmer(WPM ~ (Keyboard * Posture)/Trial  + (1|Subject), data=mbltxttrials)
Anova(m, type=3, test.statistic="F")

# we should consider Trial to be a random effect and we obtain
# almost exactly the same results, but takes longer to run.
# NOTE: the syntax in the Coursera video was incorrect for this 
# and has been corrected here.
#m = lmer(WPM ~ (Keyboard * Posture)/(1|Trial) + (1|Subject), data=mbltxttrials)  # old, incorrect
m = lmer(WPM ~ Keyboard * Posture + (1|Keyboard:Posture:Trial) + (1|Subject), data=mbltxttrials) # new, correct
Anova(m, type=3, test.statistic="F")

# perform post hoc pairwise comparisons
library(multcomp) # for glht
library(emmeans) # for emm
summary(glht(m, emm(pairwise ~ Keyboard * Posture)), test=adjusted(type="holm"))
with(mbltxttrials, interaction.plot(Posture, Keyboard, WPM, ylim=c(0, max(mbltxttrials$WPM)))) # for convenience


## Generalized Linear Mixed Model (GLMM) on Error_Rate

# turn Error_Rate into Errors counted out of 100
mbltxttrials$Errors = mbltxttrials$Error_Rate * 100
View(mbltxttrials) # verify
summary(mbltxttrials)

# explore new Errors column
library(plyr)
ddply(mbltxttrials, ~ Keyboard * Posture, function(data) summary(data$Errors))
ddply(mbltxttrials, ~ Keyboard * Posture, summarise, Errors.mean=mean(Errors), Errors.sd=sd(Errors))

# histograms for two factors
hist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Sit",]$Errors)
hist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Stand",]$Errors)
hist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Walk",]$Errors)
hist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Sit",]$Errors)
hist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Stand",]$Errors)
hist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Walk",]$Errors)
boxplot(Errors ~ Keyboard * Posture, data=mbltxttrials, xlab="Keyboard.Posture", ylab="Errors") # boxplots
with(mbltxttrials, interaction.plot(Posture, Keyboard, Errors, ylim=c(0, max(mbltxttrials$Errors)))) # interaction?

# see if new Errors data seems Poisson-distributed
library(fitdistrplus)
fit = fitdist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Sit",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Stand",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(mbltxttrials[mbltxttrials$Keyboard == "iPhone" & mbltxttrials$Posture == "Walk",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Sit",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Stand",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(mbltxttrials[mbltxttrials$Keyboard == "Galaxy" & mbltxttrials$Posture == "Walk",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test

# libraries for GLMMs with Poisson regression we'll use on Errors
library(lme4) # for glmer
library(car) # for Anova

# set sum-to-zero contrasts for the Anova call
contrasts(mbltxttrials$Keyboard) <- "contr.sum"
contrasts(mbltxttrials$Posture) <- "contr.sum"
contrasts(mbltxttrials$Trial) <- "contr.sum"

# main GLMM test on Errors
# Keyboard, Posture, Keyboard:Posture and Trial are
# all fixed effects. Trial is nested within 
# Keyboard, Posture. Subject is a random effect.
m = glmer(Errors ~ (Keyboard * Posture)/Trial + (1|Subject), data=mbltxttrials, family=poisson, nAGQ=0)
Anova(m, type=3)
# note that in glmer, we set nAGQ to zero for speed.
# the results were almost the same as for nAGQ = 1, 
# the default, which takes a few minutes to complete.

# not in Coursera video; treat "Trial" as a nested random effect.
#m = glmer(Errors ~ (Keyboard * Posture)/(1|Trial) + (1|Subject), data=mbltxttrials, family=poisson, nAGQ=0) # old, incorrect syntax
m = glmer(Errors ~ (Keyboard * Posture) + (1|Keyboard:Posture:Trial) + (1|Subject), data=mbltxttrials, family=poisson, nAGQ=0) # new, correct syntax
Anova(m, type=3)

# perform post hoc pairwise comparisons
with(mbltxttrials, interaction.plot(Posture, Keyboard, Errors, ylim=c(0, max(mbltxttrials$Errors)))) # for convenience
library(multcomp) # for glht
library(emmeans) # for emm
summary(glht(m, emm(pairwise ~ Keyboard * Posture)), test=adjusted(type="holm"))
