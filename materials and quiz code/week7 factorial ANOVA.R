
# A 2×2 mixed factorial design with a between-subjects factor for Sex (M, F) and a within-subjects factor for Avatar (M, F).
mwa = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/avatars.csv")
View(mwa)
mwa$Subject = factor(mwa$Subject) # convert to nominal factor

# 2. How many subjects took part in this experiment?
summary(mwa) # 60


# 3. on average how many positive sentiments were expressed for the most positive combination of Sex and Avatar
#  Male(sex) Female(avatar)   100.73333
library(plyr)
ddply(mwa, ~ Sex * Avatar, function(data) summary(data$Positives))
ddply(mwa, ~ Sex * Avatar, summarise, Positives.mean=mean(Positives), Positives.sd=sd(Positives))


hist(mwa[mwa$Sex == "Male" & mwa$Avatar == "Male",]$Positives)
hist(mwa[mwa$Sex == "Male" & mwa$Avatar == "Female",]$Positives)
hist(mwa[mwa$Sex == "Female" & mwa$Avatar == "Male",]$Positives)
hist(mwa[mwa$Sex == "Female" & mwa$Avatar == "Female",]$Positives)
boxplot(Positives ~ Sex * Avatar, data=mwa, xlab="Sex.Avatar", ylab="Positives") # boxplots

# 4. Create an interaction plot with Sex on the X-axis and Avatar as the traces. Do the lines cross? Yes
with(mwa, interaction.plot(Sex, Avatar, Positives, ylim=c(0, max(mwa$Positives)))) # interaction plot

# 5. Create an interaction plot with Avatar on the X-axis and Sex as the traces. Do the lines cross? No
with(mwa, interaction.plot(Avatar, Sex, Positives, ylim=c(0, max(mwa$Positives)))) # interaction plot


#6. Conduct a factorial ANOVA on Positives by Sex and Avatar. 
# To the nearest hundredth (two digits), what is the largest F statistic from such a test
library(ez)
m = ezANOVA(dv=Positives, between=c("Sex","Avatar"), wid=Subject, data=mwa)
m$Mauchly 
m$ANOVA
# F max = 17.04


# 7. Which effects are statistically significant in the factorial ANOVA of Positives by Sex and Avatar? (Mark all that apply.)
# Affect of sex: p<.05
# Affect of Avatar: p>.05
# Affect of Interaction: p<.05


# 8. incorrect(but close)

library(reshape2)
mwa2.wide = dcast(mwa, Subject + Avatar ~ Sex, value.var="Positives") # go wide
View(mwa2.wide)
male = t.test(mwa2.wide$Male ~ Avatar, data=mwa2.wide) # whether men produced different numbers of positive sentiments for male avatars versus female avatars. 
female = t.test(mwa2.wide$Female ~ Avatar, data=mwa2.wide) # whether women produced different numbers of positive sentiments for male avatars versus female avatars
p.adjust(c(male$p.value, female$p.value), method="holm")
#[1] 0.19076039 0.02048602


# 8.v2 incorrect
library(reshape2)
mwa.wide = dcast(mwa, Subject + Sex ~ Avatar, value.var="Positives") # go wide
View(mwa.wide)
m_avatar = t.test(mwa.wide$Male ~ Sex, data=mwa.wide) # Male vs. Female avatar male
f_avatar = t.test(mwa.wide$Female ~ Sex, data=mwa.wide) # Male vs. Female avatar female
p.adjust(c(m_avatar$p.value, f_avatar$p.value), method="holm")

# 8.v3 should be correct
# whether women produced different numbers of positive sentiments for male avatars versus female avatars
X= t.test(mwa[mwa$Sex == "Female" & mwa$Avatar == "Male",]$Positives, mwa[mwa$Sex == "Female" & mwa$Avatar == "Female",]$Positives, paired=FALSE, var.equal=TRUE)
#p-value = 0.009647
# whether man produced different numbers of positive sentiments for male avatars versus female avatars
Y = t.test(mwa[mwa$Sex == "Male" & mwa$Avatar == "Male",]$Positives, mwa[mwa$Sex == "Male" & mwa$Avatar == "Female",]$Positives, paired=FALSE, var.equal=TRUE)
#p-value = 0.1907
#using Holm's sequential Bonferroni procedure to correct for multiple comparisons
p.adjust(c(X$p.value, Y$p.value), method="holm")
#0.01929438 0.19073468

########
notes = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/notes.csv")
View(notes)
notes$Subject = factor(notes$Subject) # convert to nominal factor
notes$Order = factor(notes$Order) # convert to nominal factor
summary(notes)
#11. How many subjects took part in this experiment?
# 20


# 12.To the nearest whole number, on average how many words were recorded with the most heavily used combination of Phone and Notes?
library(plyr)
ddply(notes, ~ Phone * Notes, function(data) summary(data$Words))
ddply(notes, ~ Phone * Notes, summarise, Words.mean=mean(Words), Words.sd=sd(Words))
# iPhone Built-in      533.7 


# 13.  interaction plot with Phone on the X-axis and Notes as the traces> NO
boxplot(Words ~ Phone * Notes, data=notes, xlab="Phone.Notes", ylab="Words") # boxplots
with(notes, interaction.plot(Phone, Notes, Words, ylim=c(0, max(notes$Words)))) # interaction plot

# 14. Create an interaction plot with Notes on the X-axis and Phone as the traces> NO
with(notes, interaction.plot(Notes, Phone, Words, ylim=c(0, max(notes$Words)))) # interaction plot

# 15.factorial ANOVA to test for any order effect that the presentation order of the Notes factor may have had. 

library(ez)
m = ezANOVA(dv=Words, between=Phone, within=Order, wid=Subject, data=notes)
m$Mauchly # n.s.
m$ANOVA 
#p = 4.684126e-01

#16. Question 16
#In our test of possible order effects, Mauchly's test of sphericity is irrelevant because our within-subjects factor only has two levels, which cannot present a sphericity violation.
#TRUE

# 17.Conduct a factorial ANOVA on Words by Phone and Notes. To the nearest hundredth (two digits), what is the largest F statistic produced by such a test? 
# passing one between parameter and one within parameter.
m = ezANOVA(dv=Words, between=Phone, within=Notes, wid=Subject, data=notes)
m$Mauchly 
m$ANOVA
# f MAX = 43.56256949

# 18. Not correct:
library(reshape2)
notes.wide = dcast(notes, Subject + Notes ~ Phone, value.var="Words") # go wide
View(notes.wide)
iphone = t.test(notes.wide$iPhone ~ Notes, data=notes.wide) # Add-on vs. Built-in iPhone
android = t.test(notes.wide$Android ~ Notes, data=notes.wide) # Add-on vs. Built-in Android
p.adjust(c(iphone$p.value, android$p.value), method="holm")
# [1] 0.3667111 0.4280599

#v2.Correct:
library(reshape2)
notes2.wide = dcast(notes, Subject + Phone ~ Notes, value.var="Words") # go wide
View(notes2.wide)
iphone2 = t.test(notes2.wide[notes2.wide$Phone == "iPhone",]$`Add-on`,  notes2.wide[notes2.wide$Phone == "iPhone",]$`Built-in`, paired=TRUE, var.equal=TRUE)
android2 = t.test(notes2.wide[notes2.wide$Phone == "Android",]$`Add-on`,  notes2.wide[notes2.wide$Phone == "Android",]$`Built-in`, paired=TRUE, var.equal=TRUE)
p.adjust(c(iphone2$p.value, android2$p.value), method="holm")
# 0.1960779 0.4675674

#19.None of above

#######

socialvalue = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/socialvalue.csv")
View(socialvalue)
socialvalue$Subject = factor(socialvalue$Subject) # convert to nominal factor
socialvalue$SocialOrder = factor(socialvalue$SocialOrder) # convert to nominal factor
socialvalue$ClipOrder = factor(socialvalue$ClipOrder) # convert to nominal factor
#21
summary(socialvalue)

#22. To the nearest hundredth (two digits), on average how many posts out of 100 were valued for the most valued combination of Clip and Social?
library(plyr)
ddply(socialvalue, ~ Clip * Social, function(data) summary(data$Valued))
ddply(socialvalue, ~ Clip * Social, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))
# positive Facebook     68.7500

#23. Create an interaction plot with Social on the X-axis and Clip as the traces
boxplot(Valued ~ Clip * Social, data=socialvalue, xlab="Clip.Social", ylab="Valued") # boxplots
with(socialvalue, interaction.plot(Social, Clip, Valued, ylim=c(0, max(socialvalue$Valued)))) # interaction plot
#No cross

#24.Create an interaction plot with Clip on the X-axis and Social as the traces. Do the lines cross?
with(socialvalue, interaction.plot(Clip, Social, Valued, ylim=c(0, max(socialvalue$Valued))))
#Yes

# 25. Conduct a factorial ANOVA to test for any order effects that the presentation order of the Clip factor and/or the Social factor may have had. 
#To the nearest ten-thousandth (four digits), what is the p-value for the ClipOrder main effect? 
#Hint: Use the ez library and its ezANOVA function. 
#Pass both ClipOrder and SocialOrder as the within parameter using a vector created with the "c" function.
library(ez)
m = ezANOVA(dv=Valued, within=c("ClipOrder","SocialOrder"), wid=Subject, data=socialvalue)
m$Mauchly # n.s.
m$ANOVA 
#0.3483818 


#  26. Conduct a factorial ANOVA on Valued by Clip and Social. 
#To the nearest hundredth (two digits), what is the largest F statistic produced by such a test? 
#Hint: Use the ez library and its ezANOVA function. 
#Pass both Clip and Social as the within parameter using a vector created with the "c" function.
library(ez)
m = ezANOVA(dv=Valued, within=c("Clip","Social"), wid=Subject, data=socialvalue)
m$Mauchly 
m$ANOVA 
#F max 6.99533219 

#27.
library(reshape2)
socialvalue.wide = dcast(socialvalue, Subject ~ Social * Clip, value.var="Valued") # go wide
View(socialvalue.wide)

fb = t.test(socialvalue.wide$Facebook_negative,socialvalue.wide$Facebook_positive,paired = TRUE,var.equal = TRUE) # facebook
twitter = t.test(socialvalue.wide$Twitter_negative,socialvalue.wide$Twitter_positive,,paired = TRUE,var.equal = TRUE) # twitter
p.adjust(c(fb$p.value, twitter$p.value), method="holm")
#[1] 0.04077153 0.06482275

# 28
#On Facebook, people valued significantly more posts after seeing a positive film clip than a negative film clip.


#29.nonparametric Aligned Rank Transform procedure on Valued by Clip and Social. 
library(ARTool) # for art, artlm
m = art(Valued ~ Clip * Social + (1|Subject), data=socialvalue) # uses LMM
anova(m) # report anova
# Largest F 17.13224

#30 Pairwise comparisons among levels of Clip and among levels of Social could be conducted using the following code, 
#but these are unnecessary after our main effects tests because each of these factors only has two levels.
library(emmeans)
emmeans(artlm(m, "Clip"), pairwise ~ Clip)
emmeans(artlm(m, "Social"), pairwise ~ Social)
#True!

#31. The difference in the number of valued posts after people saw negative film clips vs. positive film clips in the Facebook condition is significantly different from that difference in the Twitter condition.
library(phia)
testInteractions(artlm(m, "Clip:Social"), pairwise=c("Clip", "Social"), adjustment="holm")
# chisq 11.318

#32 True





