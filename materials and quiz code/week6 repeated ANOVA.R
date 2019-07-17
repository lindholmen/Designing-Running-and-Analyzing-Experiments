websearch = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/websearch2.csv")
View(websearch)
websearch$Subject = factor(websearch$Subject) # convert to nominal factor
websearch$Order = factor(websearch$Order) # convert to nominal factor
summary(websearch)

library(plyr)
ddply(websearch, ~ Engine, function(data) summary(data$Searches))
ddply(websearch, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))


# graph histograms and boxplot
hist(websearch[websearch$Engine == "Bing",]$Searches)
hist(websearch[websearch$Engine == "Google",]$Searches)
plot(Searches ~ Engine, data=websearch) # boxplot


# now test for an order effect -- did counterbalancing work?
library(reshape2)	
# for a paired-samples t-test we must use a wide-format table; most
# R fns do not require a wide-format table, but the dcast function
# offers a quick way to translate long-format into wide-format when
# we need it.
websearch.wide.order = dcast(websearch, Subject ~ Order, value.var="Searches") # go wide
View(websearch.wide.order) # verify
t.test(websearch.wide.order$"1", websearch.wide.order$"2", paired=TRUE, var.equal=TRUE)


# finally, the paired-samples t-test
websearch.wide.tech = dcast(websearch, Subject ~ Engine, value.var="Searches") # go wide
View(websearch.wide.tech)
t.test(websearch.wide.tech$Bing, websearch.wide.tech$Google, paired=TRUE, var.equal=TRUE)
plot(Searches ~ Engine, data=websearch) # confirm

## Nonparametric equivalent of paired-samples t-test
# Wilcoxon signed-rank test on Errors
library(coin)
wilcoxsign_test(Effort ~ Engine | Subject, data=websearch, distribution="exact")
# note: the term afer the "|" indicates the within-Ss blocking term for matched pairs


## One-way repeated measures ANOVA

# read in a data file now with a third method
websearch3 = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/websearch3.csv")
View(websearch3)
websearch3$Subject = factor(websearch3$Subject) # convert to nominal factor
websearch3$Order = factor(websearch3$Order) # convert to nominal factor
summary(websearch3)

library(plyr)
ddply(websearch3, ~ Engine, function(data) summary(data$Searches))
ddply(websearch3, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))


library(ez)
m = ezANOVA(dv=Searches, within=Order, wid=Subject, data=websearch3)
m$Mauchly
m$ANOVA

library(ez)
m = ezANOVA(dv=Searches, within=Engine, wid=Subject, data=websearch3)
m$Mauchly
m$ANOVA

# manual post hoc pairwise comparisons with paired-samples t-tests
library(reshape2)	
websearch3.wide.tech = dcast(websearch3, Subject ~ Engine, value.var="Searches") # go wide
View(websearch3.wide.tech)
gg.bi = t.test(websearch3.wide.tech$Google, websearch3.wide.tech$Bing, paired=TRUE)
bi.ya = t.test(websearch3.wide.tech$Bing, websearch3.wide.tech$Yahoo, paired=TRUE)
ya.gg = t.test(websearch3.wide.tech$Yahoo, websearch3.wide.tech$Google, paired=TRUE)
p.adjust(c(gg.bi$p.value, bi.ya$p.value, ya.gg$p.value), method="holm")


## Nonparametric equivalent of one-way repeated measures ANOVA
# Friedman test 
library(coin)
friedman_test(Effort ~ Engine | Subject, data=websearch3, distribution="asymptotic")

# manual post hoc Wilcoxon signed-rank test multiple comparisons
se.sc = wilcox.test(websearch3[websearch3$Engine == "Google",]$Effort, websearch3[websearch3$Engine == "Bing",]$Effort, paired=TRUE, exact=FALSE)
se.vc = wilcox.test(websearch3[websearch3$Engine == "Bing",]$Effort, websearch3[websearch3$Engine == "Yahoo",]$Effort, paired=TRUE, exact=FALSE)
sc.vc = wilcox.test(websearch3[websearch3$Engine == "Google",]$Effort, websearch3[websearch3$Engine == "Yahoo",]$Effort, paired=TRUE, exact=FALSE)
p.adjust(c(se.sc$p.value, se.vc$p.value, sc.vc$p.value), method="holm")





