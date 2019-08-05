#1
web = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/websearch3.csv") 
View(web)

web$Subject = factor(web$Subject) # convert to nominal factor
web$Order = factor(web$Order) # convert to nominal factor
summary(web)

boxplot(Searches ~ Engine, data=web, xlab="Engine", ylab="Searches") # boxplots

#2.  explore the websearch data
library(plyr)
ddply(web, ~ Engine, function(data) summary(data$Searches))
ddply(web, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))
#. Google 152.6667

#3. Conduct a linear mixed model (LMM) analysis of variance on Searches by Engine. To the nearest ten-thousandth (four digits), what is the p-value of such a test? Hint: Use the lme4 library and its lmer function with Subject as a random effect. Also load the lmerTest library. Then use the car library and its Anova function with type=3 and test.statistic="F". Prior to either, set sum-to-zero contrasts for Engine.
# libraries for LMMs we'll use on WPM
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova

# set sum-to-zero contrasts for the Anova calls
contrasts(web$Engine) <- "contr.sum"

# main LMM test 
m = lmer(Searches ~ Engine  + (1|Subject), data=web)
Anova(m, type=3, test.statistic="F")
#p= 0.05636 

#4.no to post hoc pairwise comparison

#5. Correct your p-values with Holm's sequential Bonferroni procedure. To the nearest ten-thousandth (four digits), what is the lowest corrected p-value resulting from such tests? Hint: Use the multcomp library and its mcp function within a call to its glht function.
# perform post hoc pairwise comparisons
library(multcomp) # for glht
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))
# p = 0.0454

#6. True - Because the omnibus linear mixed model (LMM) analysis of variance did not result in a significant main effect of Engine on Searches, post hoc pairwise comparisons were not justified. As a result, despite one such comparison having p<.05, strictly speaking this "finding" must be disregarded


#7. 
social = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/socialvalue.csv") 
View(social)

social$Subject = factor(social$Subject) # convert to nominal factor
social$ClipOrder = factor(social$ClipOrder) # convert to nominal factor
social$SocialOrder = factor(social$SocialOrder) # convert to nominal factor
summary(social)

#8.On average and to the nearest whole number, how many more posts were valued on Facebook than on Twitter after seeing a positive film clip?
library(plyr)
ddply(social, ~ Clip * Social, function(data) summary(data$Valued))
ddply(social, ~ Clip * Social, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))
# 10

#9.Conduct a linear mixed model (LMM) analysis of variance on Valued by Social and Clip. To the nearest ten-thousandth (four digits), what is the p-value of the interaction effect? Hint: Use the lme4 library and its lmer function with Subject as a random effect. Then use the car library and its Anova function with type=3 and test.statistic="F". Prior to either, set sum-to-zero contrasts for both Social and Clip.
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova
# set sum-to-zero contrasts for the Anova calls
contrasts(social$Clip) <- "contr.sum"
contrasts(social$Social) <- "contr.sum"
# main LMM test 
m = lmer(Valued ~ Clip * Social  + (1|Subject), data=social)
Anova(m, type=3, test.statistic="F")
# p = 0.017930 

#10. two planned pairwise comparisons of how the film clips may have influenced judgments about the value of social media.
library(multcomp)
library(emmeans)
summary(glht(m, emm(pairwise ~  Clip*Social)), test=adjusted(type="none"))
p.adjust(c(0.000225,0.594397), method="holm")
# [1] 0.000450 0.594397

#11. 
teaser = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/teaser.csv") 
View(teaser)
#20 subjects

teaser$Subject = factor(teaser$Subject) # convert to nominal factor
teaser$Order = factor(teaser$Order) # convert to nominal factor
teaser$Liked = factor(teaser$Liked) # convert to nominal factor
summary(teaser)

#12. balanced Latin Square

#13. 
library(plyr)
ddply(teaser, ~ Teaser, function(data) summary(data$Liked))
# action liked by the most

# 14. Using a generalized linear mixed model (GLMM), conduct a test of order effects on Liked to ensure counterbalancing worked. To the nearest ten-thousandth (four digits), what is the p-value for the Order main effect? Hint: Use the lme4 library and its glmer function with family=binomial and Subject as a random effect. Use the default value for nAGQ, which is 1; since this is the default, you do not need to specify nAGQ, or you can set nAGQ=1. (Higher values of nAGQ produce better estimates [max. 25], but increase execution time considerably.) Then use the car library and its Anova function with type=3. Prior to either, set sum-to-zero contrasts for Order.
library(lme4) # for glmer
library(car) # for Anova
contrasts(teaser$Order) <- "contr.sum"
m = glmer(Liked ~ Order + (1|Subject), data=teaser, family=binomial, nAGQ=1)
Anova(m, type=3)
# 0.4169

#15.Using a generalized linear mixed model (GLMM), conduct a test of Liked by Teaser.
contrasts(teaser$Teaser) <- "contr.sum"
m = glmer(Liked ~ Teaser  + (1|Subject), data=teaser, family=binomial, nAGQ=1)
Anova(m, type=3)
# chisq 26.695 

#16. 
library(multcomp)
summary(glht(m, mcp(Teaser="Tukey")), test=adjusted(type="holm")) # Tukey means compare all pairs
# How many of the tests are statistically significant?  5

#17.
vocab = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/vocab.csv") 
View(vocab)
# 30 subjects
vocab$Subject = factor(vocab$Subject) # convert to nominal factor
vocab$Order = factor(vocab$Order) # convert to nominal factor
summary(vocab)

#18.Create an interaction plot with Social on the X-axis and Sex as the traces. How many times, if any, do these lines cross?
with(vocab, interaction.plot(Social, Sex, Vocab, ylim=c(0, max(vocab$Vocab)))) # interaction plot
# no

#19.Perform three Kolmogorov-Smirnov goodness-of-fit tests on Vocab for each level of Social using exponential distributions. To the nearest ten-thousandth (four digits), what is the lowest p-value of these three tests? 
#Hint: Use the MASS library and its fitdistr function on Vocab separately for each level of Social. Use "exponential" as the distribution type. 
#Save the estimate as a fit. Then use ks.test with "pexp" passing fit[1] as the rate and requesting an exact test. Ignore any warnings produced about ties.
# Kolmogorov-Smirnov test for exponential
library(MASS)
fit = fitdistr(vocab[vocab$Social == "Facebook",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Facebook",]$Vocab, "pexp", rate=fit[1], exact=TRUE)
#p-value = 0.2734 - lowest
fit = fitdistr(vocab[vocab$Social == "Gplus",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Gplus",]$Vocab, "pexp", rate=fit[1], exact=TRUE)
# p-value = 0.5111
fit = fitdistr(vocab[vocab$Social == "Twitter",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Twitter",]$Vocab, "pexp", rate=fit[1],exact=TRUE)
#p-value = 0.8966

#20.Use a generalized linear mixed model (GLMM) to conduct a test of order effects on Vocab to ensure counterbalancing worked. To the nearest ten-thousandth (four digits), what is the p-value for the Order main effect
library(lme4) # for glmer
library(car) # for Anova
contrasts(vocab$Sex) <- "contr.sum"
contrasts(vocab$Order) <- "contr.sum"
m = glmer(Vocab ~ Sex * Order + (1|Subject), data=vocab, family=Gamma(link="log"), nAGQ=1)
Anova(m, type=3)
# 0.7047  pvalue

# 21.
m = glmer(Vocab ~ Sex * Social + (1|Subject), data=vocab, family=Gamma(link="log"))
Anova(m, type=3)
# interaction p value: 0.8407

# 22. perform post hoc pairwise comparisons among levels of Social adjusted with Holm's sequential Bonferroni procedure. To the nearest ten-thousandth (four digits), what is the p-value of the only non-significant pairwise comparison?
library(multcomp)
summary(glht(m, mcp(Social="Tukey")), test=adjusted(type="holm"))
# p 0.5780
summary(glht(m, lsm(pairwise ~ Sex * Social)), test=adjusted(type="holm"))

#23. 
web = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/websearch3.csv") 
View(web)

web$Subject = factor(web$Subject)
web$Effort = ordered(web$Effort)
summary(web)
# 7 categories of effort

#24.
library(ordinal) # for clmm
library(RVAideMemoire) # for Anova.clmm
web2 = as.data.frame(web) # copy
m = clmm(Effort ~ Engine + (1|Subject), data=web2)
Anova.clmm(m, type=3)
#  pvalue = 0.0174

# 25. 
plot(as.numeric(Effort) ~ Engine, data=web2)
library(lme4)
library(multcomp)
m = lmer(as.numeric(Effort) ~ Engine + (1|Subject), data=web2)
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))
# Google - Bing == 0  non sig pvalue:   0.9381

