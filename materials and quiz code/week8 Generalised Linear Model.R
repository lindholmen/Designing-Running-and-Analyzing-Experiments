#1
devicepref = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/deviceprefssex.csv") 
View(devicepref)


#2. Use binomial regression to examine Pref by Disability and Sex. 
devicepref$Subject = factor(devicepref$Subject) # convert to nominal factor
devicepref$Disability = factor(devicepref$Disability) # convert to nominal factor

library(nnet) # 
library(car) # for Anova
# set sum-to-zero contrasts for the Anova call
contrasts(devicepref$Sex) <- "contr.sum"
contrasts(devicepref$Disability) <- "contr.sum"

m = glm(Pref ~ Disability*Sex, data=devicepref, family=binomial)
Anova(m, type=3)
# p= 0.403997  

#3. 
contrasts(devicepref$Sex) <- "contr.sum"
contrasts(devicepref$Disability) <- "contr.sum"
m = multinom(Pref ~ Disability*Sex, data=devicepref) # multinomial logistic
Anova(m, type=3)
#p= 0.404087 

#4. whether there was a significant preference for touchpads or trackballs within each Disability × Sex combination. Conduct such an exploration using post hoc binomial tests.
trackball.m0 = binom.test(sum(devicepref[devicepref$Sex == "M" & devicepref$Disability == "0",]$Pref == "trackball"), nrow(devicepref[devicepref$Sex == "M" & devicepref$Disability == "0",]), p=1/2)
trackball.m1 = binom.test(sum(devicepref[devicepref$Sex == "M" & devicepref$Disability == "1",]$Pref == "trackball"), nrow(devicepref[devicepref$Sex == "M" & devicepref$Disability == "1",]), p=1/2)
touchpad.f0 = binom.test(sum(devicepref[devicepref$Sex == "F" & devicepref$Disability == "0",]$Pref == "touchpad"), nrow(devicepref[devicepref$Sex == "F" & devicepref$Disability == "0",]), p=1/2)
touchpad.f1 = binom.test(sum(devicepref[devicepref$Sex == "F" & devicepref$Disability == "1",]$Pref == "touchpad"), nrow(devicepref[devicepref$Sex == "F" & devicepref$Disability == "1",]), p=1/2)
p.adjust(c(trackball.m0$p.value, trackball.m1$p.value, touchpad.f0$p.value,touchpad.f1$p.value), method="holm") 
# [1] 0.0625000 1.0000000 0.1962891 1.0000000
# lowest p : 0.0625000




#5.
hwreco = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/hwreco.csv") 
View(hwreco)
hwreco$Subject = factor(hwreco$Subject) # convert to nominal factor
summary(hwreco)
# 50 subjects

#6. Create an interaction plot with Recognizer on the X-axis and Hand as the traces. How many times, if any, do the two traces cross?
with(hwreco, interaction.plot(Recognizer, Hand, Errors, ylim=c(0, max(hwreco$Errors)))) # interaction plot
# 2 times

#7.
# re-verify that these data are Poisson-distributed
library(fitdistrplus)
fit = fitdist(hwreco[hwreco$Recognizer == "A",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
# lowest Chi-squared p-value:  0.4049767 

fit = fitdist(hwreco[hwreco$Recognizer == "B",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(hwreco[hwreco$Recognizer == "C",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test

#8.
contrasts(hwreco$Recognizer) <- "contr.sum"
contrasts(hwreco$Hand) <- "contr.sum"
m = glm(Errors ~ Recognizer * Hand, data=hwreco, family=poisson)
Anova(m, type=3)
# p= 0.001528

#9.
library(multcomp)
library(emmeans)
summary(glht(m, emm(pairwise ~  Recognizer*Hand)), test=adjusted(type="none"))
# Just look for the 3 p-values pertinent to the comparison between left and right in each Recognizer group.
p.adjust(c(0.001925, 0.095955, 0.243171), method="holm")
#[1] 0.005775 0.191910 0.243171

#10.
with(hwreco, interaction.plot(Recognizer, Hand, Errors, ylim=c(0, max(hwreco$Errors)))) # interaction plot
#There was a significant Recognizer × Hand interaction.
#For recognizer "A", there were significantly more errors for right-handed writers than left-handed writers.
#[1] 0.005775 0.191910 0.243171
#The handwriting error counts seemed to be Poisson-distributed.


#11.How many subjects took part in this study?  600
flightbooking = read.csv("/Users/yemao/documents/coursera/Designing Running and Analyzing Experiments/materials and quiz code/bookflights.csv") 
View(flightbooking)

flightbooking$Subject = factor(flightbooking$Subject) # convert to nominal factor
flightbooking$International = factor(flightbooking$International) # convert to nominal factor
library(MASS) # for polr
library(car) # for Anova
flightbooking$Ease = ordered(flightbooking$Ease)
summary(flightbooking)


#12.Create an interaction plot with Website on the X-axis and International as the traces. How many times, if any, do the two traces cross?
with(flightbooking, interaction.plot(Website, International, as.numeric(Ease), ylim=c(0, max(flightbooking$Ease)))) # interaction plot
# cross 1 time

#13.
library(MASS) # for polr
library(car) # for Anova
flightbooking$Ease = ordered(flightbooking$Ease)
contrasts(flightbooking$Website) <- "contr.sum"
contrasts(flightbooking$International) <- "contr.sum"
m = polr(Ease ~ Website * International, data=flightbooking, Hess=TRUE) # ordinal logistic
Anova(m, type=3) # n.s.
# p= 0.0332

#14. ????
library(multcomp)
library(lsmeans)
summary(lsmeans::as.glht(pairs(lsmeans::lsmeans(m, pairwise ~ Website * International))), test=adjusted(type="none")) # this works!
# get "Error: 'as.glht' is not an exported object from 'namespace:lsmeans'"
# asked in the forum and unresolved!!

#15.
#There was a significant main effect of Website on Ease.
#There was a significant Website × International interaction.
#Expedia was perceived as significantly easier for booking international flights than domestic flights.
#Orbitz was perceived as significantly easier for booking domestic flights than international flights.




























#
