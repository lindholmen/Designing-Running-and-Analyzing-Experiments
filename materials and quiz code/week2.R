prefs = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/deviceprefs.csv")
View(prefs)

prefs$Subject = factor(prefs$Subject) # convert to nominal factor
prefs$Disability = factor(prefs$Disability) 

summary(prefs)
plot(prefs$Pref)

#nodisable: touchpad	16 , trackball 2
plot(prefs[prefs$Disability == "0",]$Pref)
#disable: touchpad	5 , trackball 7
plot(prefs[prefs$Disability == "1",]$Pref)

# Pearson chi-square test
prfs = xtabs( ~ Pref, data=prefs)
prfs # show counts
# X-squared = 4.80
chisq.test(prfs)

# for people without disabilities, 
# perform a binomial test to see whether their preference for touchpads differed significantly from chance.
# 0.0013
binom.test(sum(prefs[prefs$Disability == "0",]$Pref == "touchpad"), nrow(prefs[prefs$Disability == "0",]), p=1/2)
#equals the following:
# prfs.no.dis = xtabs( ~ Pref, data=prefs[prefs$Disability == "0",])
# binom.test(prfs.no.dis)


# For people with disabilities, 
# perform a binomial test to see whether their preference for touchpads differed significantly from chance
# p-value = 0.7744
binom.test(sum(prefs[prefs$Disability == "1",]$Pref == "touchpad"), nrow(prefs[prefs$Disability == "1",]), p=1/2)


#Conduct a two-sample Chi-Square test of proportions on preferences by disability status. 
# chi-square 5.56
prfs2 = xtabs( ~ Pref + Disability, data=prefs) # the '+' sign indicates two vars
View(prfs2)
chisq.test(prfs2)

# G-test, g=7.79
library(RVAideMemoire)
G.test(prfs2)

# Fisher's exact test, p= 0.0125
fisher.test(prfs2)

