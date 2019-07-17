alphabet = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials/alphabets.csv")
View(alphabet)
alphabet$Subject = factor(alphabet$Subject) # convert to nominal factor
summary(alphabet)

library(plyr)
ddply(alphabet, ~ Alphabet, function(data) summary(data$WPM))
ddply(alphabet, ~ Alphabet, summarise, WPM.mean=mean(WPM), WPM.sd=sd(WPM))


# explore new response distribution
hist(alphabet[alphabet$Alphabet == "EdgeWrite",]$WPM)
hist(alphabet[alphabet$Alphabet == "Graffiti",]$WPM)
hist(alphabet[alphabet$Alphabet == "Unistrokes",]$WPM)
plot(WPM ~ Alphabet, data=alphabet) # boxplot

# test normality 
shapiro.test(alphabet[alphabet$Alphabet == "EdgeWrite",]$WPM)
m = aov(WPM ~ Alphabet, data= alphabet) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

shapiro.test(alphabet[alphabet$Alphabet == "Graffiti",]$WPM)
m = aov(WPM ~ Alphabet, data= alphabet) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

shapiro.test(alphabet[alphabet$Alphabet == "Unistrokes",]$WPM)
m = aov(WPM ~ Alphabet, data= alphabet) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

library(car)
leveneTest(WPM ~ Alphabet, data=alphabet, center=median) # Brown-Forsythe test


# one-way ANOVA, 
m = aov(WPM ~ Alphabet, data=alphabet) # fit model
anova(m) # report anova


library(multcomp)
summary(glht(m, mcp(Alphabet="Tukey")), test=adjusted(type="holm")) # Tukey means compare all pairs

## Nonparametric equivalent of one-way ANOVA

# Kruskal-Wallis test
library(coin)
kruskal_test(WPM ~ Alphabet, data=alphabet, distribution="asymptotic") # can't do exact with 3 levels

u.g = wilcox.test(alphabet[alphabet$Alphabet == "Unistrokes",]$WPM, alphabet[alphabet$Alphabet == "Graffiti",]$WPM, exact=FALSE)
u.e = wilcox.test(alphabet[alphabet$Alphabet == "Unistrokes",]$WPM, alphabet[alphabet$Alphabet == "EdgeWrite",]$WPM, exact=FALSE)
e.g = wilcox.test(alphabet[alphabet$Alphabet == "EdgeWrite",]$WPM, alphabet[alphabet$Alphabet == "Graffiti",]$WPM, exact=FALSE)


p.adjust(c(u.g$p.value,u.e$p.value,e.g$p.value), method="holm")









