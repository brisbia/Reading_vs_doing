# Code from Analyze questionnaires.r.

quest = read.csv("Questionnaire data.csv")
ch5_11 = quest[-(which(quest$Chapter == "Chapter 12")),]
library(stats) # for AIC

numericLevels <- function(x){
	# Translates agree strongly to 2, disagree strongly to -2
	y = rep(3,length(x))
	y[which(x == "agree strongly")] = 2
	y[which(x == "agree")] = 1
	y[which(x == "neither agree nor disagree")] = 0
	y[which(x == "disagree")] = -1
	y[which(x == "disagree strongly")] = -2
	return(y)
} #end function numericLevels

##################################################################
## Interest level

# Is proportion of (Agree or Strongly agree) different between methods? 
success = apply(table(quest$Method, quest$Q2)[,1:2],1,sum)
trials = apply(table(quest$Method, quest$Q2),1,sum)
prop.test(x = success, n = trials)
# What if we exclude chapter 12?
success = apply(table(ch5_11$Method, ch5_11$Q2)[,1:2],1,sum)
trials = apply(table(ch5_11$Method, ch5_11$Q2),1,sum)
prop.test(x = success, n = trials)

# Is proportion of Strongly agree different between methods? 
success = table(quest$Method, quest$Q2)[,2]
trials = apply(table(quest$Method, quest$Q2),1,sum)
prop.test(x = success, n = trials)
# What if we exclude chapter 12?
success = table(ch5_11$Method, ch5_11$Q2)[,2]
trials = apply(table(ch5_11$Method, ch5_11$Q2),1,sum)
prop.test(x = success, n = trials)

# Linear regression of interest 
numericInterest = numericLevels(quest$Q2)
fit = lm(numericInterest ~ as.factor(quest$Method) + as.factor(quest$Chapter)) 
summary(fit) # Method B's coeff is negative, p-val = .07
# Without ch. 12
numericInterest = numericLevels(ch5_11$Q2)
fit = lm(numericInterest ~ as.factor(ch5_11$Method) + as.factor(ch5_11$Chapter))
summary(fit)

# Logistic regression of interest 
numericInterest = numericLevels(quest$Q2)+2
success = rep(0, length(numericInterest))
success[which(numericInterest >= 3)] = 1 # "agree" or "strongly agree"
fit = glm(success ~ as.factor(quest$Method) + as.factor(quest$Chapter),binomial)
summary(fit) # pval of method B = .127, coeff < 0
# Just "strongly agree"
success = rep(0, length(numericInterest))
success[which(numericInterest == 4)] = 1 # "strongly agree"
fit = glm(success ~ as.factor(quest$Method) + as.factor(quest$Chapter),binomial)
summary(fit) # pval of method B = .19
exp(coef(fit))  # Odds ratios

# Logistic regression of interest, without chapter 12 
numericInterest = numericLevels(ch5_11$Q2)+2
success = rep(0, length(numericInterest))
success[which(numericInterest >= 3)] = 1 # "agree" or "strongly agree"
fit = glm(success ~ as.factor(ch5_11$Method) + as.factor(ch5_11$Chapter),binomial) 
summary(fit) # pval of method B = .127, coeff < 0
# Just "strongly agree"
success = rep(0, length(numericInterest))
success[which(numericInterest == 4)] = 1 # "strongly agree"
fit = glm(success ~ as.factor(ch5_11$Method) + as.factor(ch5_11$Chapter),binomial) 
summary(fit) # pval of method B = .19

##################################################################3
## Frustration

# Is proportion of (Agree or Strongly agree) different between methods? 
success = apply(table(quest$Method, quest$Q3)[,1:2],1,sum)
trials = apply(table(quest$Method, quest$Q3),1,sum)
prop.test(x = success, n = trials)
# Yes, p-val = .002
# Without ch. 12
success = apply(table(ch5_11$Method, ch5_11$Q3)[,1:2],1,sum)
trials = apply(table(ch5_11$Method, ch5_11$Q3),1,sum)
prop.test(x = success, n = trials)
# Still .002

# Proportion of Strongly agree
success = table(quest$Method, quest$Q3)[,2]
# Only 0 and 2 successes, omitting this analysis

# Linear regression of frustration 
numericFrustration = numericLevels(quest$Q3)
fit = lm(numericFrustration ~ as.factor(quest$Method) + as.factor(quest$Chapter)) 
summary(fit) #pval of Method = 6.85*10^-7
# Without ch. 12
numericFrustration = numericLevels(ch5_11$Q3)
fit = lm(numericFrustration ~ as.factor(ch5_11$Method) + as.factor(ch5_11$Chapter)) 
summary(fit) #pval of Method = 7.93*10^-7

# Logistic Regression of frustration, "agree" or "strongly agree" 
numericFrustration = numericLevels(quest$Q3)+2
success = rep(0, length(numericFrustration))
success[which(numericFrustration >= 3)] = 1 # "agree" or "strongly agree"
fit = glm(success ~ as.factor(quest$Method) + as.factor(quest$Chapter),binomial)
summary(fit)  # pval of method B = .00152

# Logistic Regression of frustration without ch. 12, "agree" or "strongly agree" 
numericFrustration = numericLevels(ch5_11$Q3)+2
success = rep(0, length(numericFrustration))
success[which(numericFrustration >= 3)] = 1 # "agree" or "strongly agree"
fit = glm(success ~ as.factor(ch5_11$Method) + as.factor(ch5_11$Chapter),binomial) 
summary(fit) # pval of method B = .00152


#######################################################################
## Perceived learning

# Is proportion of (Learned a Lot) or (Learned a Lot or a fair amount) different between methods? 
trials = apply(table(quest$Method, quest$Q1),1,sum)
success = table(quest$Method, quest$Q1)[,6] # learned a lot
prop.test(x = success, n = trials) # pval = .028
success = apply(table(quest$Method, quest$Q1)[,4:6],1,sum) # learned a lot or a fair amount
prop.test(x = success, n = trials) # pval = .96

# Without ch. 12 
trials = apply(table(ch5_11$Method, ch5_11$Q1),1,sum)
success = table(ch5_11$Method, ch5_11$Q1)[,6] # learned a lot
prop.test(x = success, n = trials) # pval = .11
success = apply(table(ch5_11$Method, ch5_11$Q1)[,4:6],1,sum) # learned a lot or a fair amount
prop.test(x = success, n = trials) # pval = .76

#Perceived learning linear model 
fit = lm(quest$Q1 ~ as.factor(quest$Method) + as.factor(quest$Chapter)) 
summary(fit) # pval of method B = .15466

#Without ch. 12 
fit = lm(ch5_11$Q1 ~ as.factor(ch5_11$Method) + as.factor(ch5_11$Chapter)) 
summary(fit) # pval of method B = .147

#Logistic regression, learned a lot or a fair amount 
success = rep(0, dim(quest)[1])
success[which(quest$Q1 >= 2)] = 1
fit = glm(success ~ as.factor(quest$Method) + as.factor(quest$Chapter),binomial)
summary(fit) # pval of method B = .63
# Without ch. 12
success = rep(0, dim(ch5_11)[1])
success[which(ch5_11$Q1 >= 2)] = 1 
fit = glm(success ~ as.factor(ch5_11$Method) + as.factor(ch5_11$Chapter),binomial)
summary(fit) # pval of method B still = .63

# Logistic regression, "learned a lot" 
success = rep(0, dim(quest)[1])
success[which(quest$Q1 == 3)] = 1 # learned a lot
fit = glm(success ~ as.factor(quest$Method) + as.factor(quest$Chapter),binomial)
summary(fit) # pval of method B = .084; coeff is negative
# Without ch. 12
success = rep(0, dim(ch5_11)[1])
success[which(ch5_11$Q1 == 3)] = 1 # learned a lot
fit = glm(success ~ as.factor(ch5_11$Method) + as.factor(ch5_11$Chapter),binomial)
summary(fit) # pval of method B still = .084; coeff is negative

########################################################################
## Relationships between student perceptions
# Simplify to 3 categories
quest$Q1simple = vector(length = length(quest$Q1))
quest$Q1simple[which(quest$Q1 == 3)] = "A lot"
quest$Q1simple[which(quest$Q1 %in% c(2, 2.5))] = "A fair amount"
quest$Q1simple[which(quest$Q1 %in% c(0, 1, 1.5))] = "Little to nothing"

quest$Q2simple = vector(length = length(quest$Q2))
quest$Q2simple[which(quest$Q2 == "agree")] = "Interested"
quest$Q2simple[which(quest$Q2 == "agree strongly")] = "Interested"
quest$Q2simple[which(quest$Q2 == "disagree")] = "Bored"
quest$Q2simple[which(quest$Q2 == "disagree strongly")] = "Bored"
quest$Q2simple[which(quest$Q2 == "neither agree nor disagree")] = "Neutral"

quest$Q3simple = vector(length = length(quest$Q3))
quest$Q3simple[which(quest$Q3 == "agree")] = "Frustrated"
quest$Q3simple[which(quest$Q3 == "agree strongly")] = "Frustrated"
quest$Q3simple[which(quest$Q3 == "disagree")] = "Confident"
quest$Q3simple[which(quest$Q3 == "disagree strongly")] = "Confident"
quest$Q3simple[which(quest$Q3 == "neither agree nor disagree")] = "Neutral"


# without ch 12
ch5_11$Q1simple = vector(length = length(ch5_11$Q1))
ch5_11$Q1simple[which(ch5_11$Q1 == 3)] = "A lot"
ch5_11$Q1simple[which(ch5_11$Q1 %in% c(2, 2.5))] = "A fair amount"
ch5_11$Q1simple[which(ch5_11$Q1 %in% c(0, 1, 1.5))] = "Little to nothing"

ch5_11$Q2simple = vector(length = length(ch5_11$Q2))
ch5_11$Q2simple[which(ch5_11$Q2 == "agree")] = "Interested"
ch5_11$Q2simple[which(ch5_11$Q2 == "agree strongly")] = "Interested"
ch5_11$Q2simple[which(ch5_11$Q2 == "disagree")] = "Bored"
ch5_11$Q2simple[which(ch5_11$Q2 == "disagree strongly")] = "Bored"
ch5_11$Q2simple[which(ch5_11$Q2 == "neither agree nor disagree")] = "Neutral"

ch5_11$Q3simple = vector(length = length(ch5_11$Q3))
ch5_11$Q3simple[which(ch5_11$Q3 == "agree")] = "Frustrated"
ch5_11$Q3simple[which(ch5_11$Q3 == "agree strongly")] = "Frustrated"
ch5_11$Q3simple[which(ch5_11$Q3 == "disagree")] = "Confident"
ch5_11$Q3simple[which(ch5_11$Q3 == "disagree strongly")] = "Confident"
ch5_11$Q3simple[which(ch5_11$Q3 == "neither agree nor disagree")] = "Neutral"

#####################
# Is perceived learning associated with frustration? 
table(quest$Q1simple, quest$Q3simple)
test = chisq.test(table(quest$Q1simple, quest$Q3simple)) 
test # p-val = 1.6 * 10 ^-15
test$expected # only one expected count < 5:  Frust + Little to Nothing = 4.5, should be fine
# Without ch. 12
chisq.test(table(ch5_11$Q1simple, ch5_11$Q3simple)) # pval = 2.22*10^(-14) # only one expected count < 5:  Frust + Little to Nothing = 4.04, should be fine

# Barplot of learning & frustration

# Relative frequencies (out of all people who learned a lot, a fair amount, or a little, respectively)
toPlot=table(quest$Q1simple, quest$Q3simple)
toPlot = toPlot[,c(1,3,2)]
toPlot = toPlot[c(2,1,3),]
toPlot=t(toPlot)
toPlot/rep(colSums(toPlot),each=3)

barplot(toPlot/rep(colSums(toPlot),each=3),beside=T,col=c("blue", "lightblue","gray"), las=1)
legend("topright", legend=c("Confident", "Neutral", "Frustrated"), fill=c("blue", "lightblue", "gray"))

# Linear model 
numericFrustration = numericLevels(quest$Q3)
fit = lm(quest$Q1 ~ numericFrustration + as.factor(quest$Chapter) + as.factor(quest$Method))
summary(fit)
# without ch 12
numericFrustration_ch5_11 = numericLevels(ch5_11$Q3)
fit = lm(ch5_11$Q1 ~ numericFrustration_ch5_11 + as.factor(ch5_11$Chapter) + as.factor(ch5_11$Method))
summary(fit)

############################
# Is perceived learning associated with interest level?
toPlot = table(quest$Q1simple, quest$Q2simple)
toPlot = toPlot[,c(2,3,1)]
toPlot = toPlot[c(2,1,3),]

toPlot = t(toPlot)
barplot(toPlot/rep(colSums(toPlot),each=3),beside=T,col=c("red","pink","gray"), las=1)
legend("topright", legend=c("Interested", "Neutral", "Bored"), fill=c("red","pink", "gray"))


chisq.test(table(quest$Q1simple, quest$Q2simple)) #pval < 2.2*10^(-16)
# all expected counts > 5

# without ch 12
chisq.test(table(ch5_11$Q1simple, ch5_11$Q2simple)) #pval < 2.2*10^(-16) # only one expected count < 5, of bored + little to nothing = 4.92.

# Linear model 
numericInterest = numericLevels(quest$Q2)
fit = lm(numericInterest ~ quest$Q1 + as.factor(quest$Chapter) + as.factor(quest$Method))
summary(fit)

# Without ch 12
numericInterest_ch5_11 = numericLevels(ch5_11$Q2)
fit = lm(numericInterest_ch5_11 ~ ch5_11$Q1 + as.factor(ch5_11$Chapter) + as.factor(ch5_11$Method))
summary(fit)

##########################
# Is interest level associated with frustration?

# Chi-squared test 
chisq.test(table(quest$Q2simple, quest$Q3simple)) #pval = 1.147*10^(-10) # expected count for bored, frustrated = 3.29, probably fine, because it's the only one <5.  Also, fisher's test gives v. similar results
fisher.test(table(quest$Q2simple, quest$Q3simple))
# without ch 12
chisq.test(table(ch5_11$Q2simple, ch5_11$Q3simple)) #pval = 1.908*10^(-10); bored+frustrated still the only expected count < 5 at 3.08.

# bar plot of boredom & frustration
toPlot = table(quest$Q2simple, quest$Q3simple)
toPlot = toPlot[,c(1,3,2)]
toPlot = toPlot[c(2,3,1),]

toPlot = t(toPlot)
barplot(toPlot/rep(colSums(toPlot),each=3),beside=T,col=c("red", "pink", "gray"), las=1)
legend("topright", legend=c("Confident", "Neutral", "Frustrated"), fill=c("red", "pink", "gray"))

# Linear model
fit = lm(numericInterest ~ numericFrustration + as.factor(quest$Chapter) + as.factor(quest$Method))
summary(fit)

#without ch 12
fit = lm(numericInterest_ch5_11 ~ numericFrustration_ch5_11 + as.factor(ch5_11$Chapter) + as.factor(ch5_11$Method))
summary(fit)