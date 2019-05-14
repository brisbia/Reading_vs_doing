# Code from Logistic for paper.r.  Cleaned to use model numbers in comments, to match the paper.

library(readr) # for read_csv
library(dplyr) # for filter
Demographics = read_csv("Demographics.csv")

########################
### Pull in data from Hwk as predictor with review Qs.r
Exams = read.csv("Parsed exam data with dates.csv")
Hwk = read.csv("Parsed Hwk.csv")
DueDates = read.csv("Homework due dates.csv")

allStudents = unique(Exams$newIDs)
Exams$AvgAttemptsWith0 = NA
Exams$AvgAttemptsNo0 = NA
Exams$MedianAttemptsWith0 = NA
Exams$MedianAttemptsNo0 = NA
Exams$MaxAttempts = NA
Exams$PctSuccessWithUnattempted = NA
Exams$PctSuccessAttemptedOnly = NA

for(chapNum in c(5,7,9,10,11,12)){
	for(meth in 1:2){
		# Find dates of all exams covering this chapter and method
		examDates = unique(Exams[which(Exams$chapter == chapNum & Exams$method == c("A", "B")[meth]), 6:7])
		for(currExam in 1:dim(examDates)[1]){
			# Find all homeworks due on or before this date
			before = DueDates[which(DueDates$Month < examDates$monthlist[currExam] |(DueDates$Month == examDates$monthlist[currExam] & DueDates$Day <= examDates$datelist[currExam])), 3]
			
			for(student in allStudents){
				demogRow = which(Demographics$`Random ID` == student)
				hwkRows = which(Hwk$Student == student & Hwk$Chapter == chapNum & Hwk$Method == meth & Hwk$Homework %in% before)
				examRows = which(Exams$newIDs == student & Exams$chapter == chapNum & Exams$method == c("A", "B")[meth] & Exams$monthlist == examDates$monthlist[currExam] & Exams$datelist == examDates$datelist[currExam])
				
				Exams$AvgAttemptsWith0[examRows] = mean(Hwk$Attempts[hwkRows])
				Exams$MedianAttemptsWith0[examRows] = median(Hwk$Attempts[hwkRows])
				Exams$MaxAttempts[examRows] = max(Hwk$Attempts[hwkRows])
				Exams$PctSuccessWithUnattempted[examRows] = mean(Hwk$Success[hwkRows])
				
				AttemptedOnly = which(Hwk$Student == student & Hwk$Chapter == chapNum & Hwk$Method == meth & Hwk$Attempts > 0)
				Exams$AvgAttemptsNo0[examRows] = mean(Hwk$Attempts[AttemptedOnly])
				Exams$MedianAttemptsNo0[examRows] = median(Hwk$Attempts[AttemptedOnly])
				Exams$PctSuccessAttemptedOnly[examRows] = mean(Hwk$Success[AttemptedOnly])
				
				# Demographic variables
				Exams$Sex[examRows] = Demographics$Sex[demogRow]
				Exams$Age[examRows] = Demographics$Age[demogRow]
				Exams$Required[examRows] = ifelse(Demographics$`Major reqs/reccs (* indicates that other stats courses are valid options)`[demogRow] %in% c("req", "req*"), "Yes", "No")
				Exams$PrevSem[examRows] = Demographics$`Total number of semesters`[demogRow]
				Exams$PrevSemUWEC[examRows] = Demographics$`Number of semesters completed at UWEC`[demogRow]
				Exams$Credits[examRows] = Demographics$`Number of transfer credits (non-CP or CR)`[demogRow] + Demographics$`CP, CR credits`[demogRow]
				Exams$PrevMath[examRows] = Demographics$`Previous college-level math (does not count Math 20)`[demogRow]
			} # end iter over students
		} # end iter over exams
	} # end iter over methods
} # end iter over chapter numbers 

Exams_complete = Exams[complete.cases(Exams),] # for purposes of step() only, because this causes us to go from 1521 rows to 1434 rows.

###############################

#### Regress hwk predictors on other hwk predictors, to reduce multicollinearity
PctSuccUnattModel = lm(PctSuccessWithUnattempted ~ AvgAttemptsNo0 + MedianAttemptsNo0 + PctSuccessAttemptedOnly + MaxAttempts + AvgAttemptsWith0 + MedianAttemptsWith0, data = Exams)
AvgNo0Model = lm(AvgAttemptsNo0 ~ MedianAttemptsNo0 + PctSuccessAttemptedOnly + MaxAttempts + AvgAttemptsWith0 + MedianAttemptsWith0, data = Exams)
MedianModel = lm(MedianAttemptsNo0 ~ PctSuccessAttemptedOnly + MaxAttempts + AvgAttemptsWith0 + MedianAttemptsWith0, data = Exams)
PctSuccModel = lm(PctSuccessAttemptedOnly ~ MaxAttempts + AvgAttemptsWith0 + MedianAttemptsWith0, data = Exams)
MaxModel = lm(MaxAttempts ~ AvgAttemptsWith0 + MedianAttemptsWith0, data = Exams)
AvgModel = lm(AvgAttemptsWith0 ~ MedianAttemptsWith0, data = Exams)

Exams$PctSuccUnattResid = ifelse(!is.na(Exams$PctSuccessAttemptedOnly), PctSuccUnattModel$residuals, NA)
Exams$AvgNo0Resid = ifelse(!is.na(Exams$PctSuccessAttemptedOnly), AvgNo0Model$residuals, NA)
Exams$MedianNo0Resid = ifelse(!is.na(Exams$PctSuccessAttemptedOnly), MedianModel$residuals, NA)
Exams$PctSuccResid = ifelse(!is.na(Exams$PctSuccessAttemptedOnly), PctSuccModel$residuals, NA)
Exams$MaxResid = MaxModel$residuals
Exams$AvgResid = AvgModel$residuals
##########
Exams$firstTopic = rep("N", length(Exams$newscores))
Exams$firstTopic[which(Exams$chapter %in% c(9,10) & Exams$method == "B")] = "Y"
Exams$firstTopic[which(Exams$chapter %in% c(5, 7, 11, 12) & Exams$method == "A")] = "Y"
########



# Exams has 1521 rows (39 students * 39 problems)
# 18 have missing scores (student didn't attempt that problem/take that quiz)
# 30 have missing AvgNo0Resid, MedianNo0Resid, and PctSuccessAttemptedOnly (student didn't attempt any relevant homework problems), for a total of 48

# Make a maximal complete data set for comparing AICs
missingScores = which(is.na(Exams$newscores))
missingHwk = which(is.na(Exams$AvgNo0Resid))
Exams_AIC = Exams[-c(missingScores, missingHwk),]


# Model 0 
ModelZ = glm( newscores ~ method + as.factor(newIDs) + as.factor(chapter), data = Exams, family = "binomial")
ModelZbest = step(ModelZ)  # best:  as.factor(newIDs) + as.factor(chapter)
summary(ModelZbest)

# Model 1 
ModelA = glm( newscores ~ dayslist + method + as.factor(newIDs) + as.factor(chapter), data = Exams, family = "binomial")
ModelAbest = step(ModelA) 
summary(ModelAbest)
#dayslist             -2.862e-02  4.135e-03  -6.921 4.50e-12 ***
#methodB               3.713e-01  1.501e-01   2.474  0.01335 * 
exp(coef(ModelAbest)) # odds ratios
# Compute AIC
ModelA.1 = glm( newscores ~ dayslist + method + as.factor(newIDs) + as.factor(chapter), data = Exams_AIC, family = "binomial")
AIC(ModelA.1)

# Model 2 
ModelB = glm( newscores ~ dayslist * method + as.factor(newIDs) + as.factor(chapter), data = Exams, family = "binomial")
ModelBbest = step(ModelB) 
summary(ModelBbest)
#dayslist             -3.610e-02  5.547e-03  -6.508 7.61e-11 ***
#methodB               6.279e-02  2.111e-01   0.297  0.76609 
#dayslist:methodB      1.429e-02  6.988e-03   2.045  0.04087 * 
exp(coef(ModelBbest)) # odds ratios
ModelB.1 = glm( newscores ~ dayslist * method + as.factor(newIDs) + as.factor(chapter), data = Exams_AIC, family = "binomial")
AIC(ModelB.1)

# Model 3 
ModelC = glm( newscores ~ dayslist * method + as.factor(newIDs) + AvgResid + AvgNo0Resid + MedianAttemptsWith0 + MedianNo0Resid + PctSuccUnattResid + PctSuccResid + MaxResid + as.factor(chapter), data = Exams, family = "binomial")
ModelCbest = step(ModelC) # This doesn't require the use of Exams_complete, because the final model selected includes AvgNo0Resid, which has the missing data, so we never change the amount of missing data by dropping predictors.

# Best:  dayslist + method + as.factor(newIDs) +  AvgNo0Resid + MaxResid + as.factor(chapter) + dayslist:method
summary(ModelCbest)
# Have verified that this gives the same results as removing the 48 missing values and re-doing the regression and variable selection.
#dayslist              -0.038808   0.005639  -6.882 5.90e-12 ***
#methodB               -0.023747   0.217564  -0.109  0.91308
#dayslist:methodB       0.014670   0.007136   2.056  0.03980 * 
#AvgNo0Resid           -0.394799   0.133135  -2.965  0.00302 ** 
#MaxResid               0.119070   0.043835   2.716  0.00660 ** 
exp(coef(ModelCbest))
# fit to Exams_AIC.  Should be an identical model, just doesn't have to drop any variables.

ModelC.1 = glm( newscores ~ dayslist * method + as.factor(newIDs) +  AvgNo0Resid + MaxResid + as.factor(chapter), data = Exams_AIC, family = "binomial")  #yes, identical, and doesn't drop any variables.
AIC(ModelC.1)

# Model 4 
rows_no5_full = which(Exams$chapter != 5)
ModelD = glm( newscores ~ dayslist * method + as.factor(newIDs) + AvgResid + AvgNo0Resid + MedianAttemptsWith0 + MedianNo0Resid + PctSuccUnattResid + PctSuccResid + MaxResid + as.factor(chapter), data = Exams[rows_no5_full,], family = "binomial")
ModelDbest = step(ModelD)  # best:  dayslist + method + as.factor(newIDs) + AvgNo0Resid + as.factor(chapter)
summary(ModelDbest)
#dayslist              -0.030921   0.006402  -4.830 1.36e-06 ***
#methodB                0.389654   0.181374   2.148  0.03169 *  
#AvgNo0Resid           -0.342022   0.166501  -2.054  0.03996 *  
exp(coef(ModelDbest))


# Model 5 
ModelE = glm( newscores ~ dayslist * method + as.factor(newIDs) + as.factor(chapter) + firstTopic, data = Exams, family = "binomial")
#dayslist             -3.633e-02  5.554e-03  -6.541 6.09e-11 ***
#methodB               9.066e-02  2.155e-01   0.421  0.67404   
#firstTopicY           9.589e-02  1.497e-01   0.641  0.52180    
#dayslist:methodB      1.417e-02  6.984e-03   2.029  0.04249 * 

ModelEbest = step(ModelE)  # best: dayslist + method + as.factor(newIDs) + as.factor(chapter) + dayslist:method, same as Model 2
summary(ModelEbest)

# for AIC
ModelE.1 = glm( newscores ~ dayslist * method + as.factor(newIDs) + as.factor(chapter), data = Exams_AIC, family = "binomial")
AIC(ModelE.1) # Same as Model 2, as it should be

# Model 6 #set of predictors in best model is OK, when we use ExamsSimple = Exams[-missingDemog,]
# Remove rows with missing demographic data, for purposes of `step()`:
missingDemog = which(is.na(Exams$PrevMath))
ExamsSimple = Exams[-missingDemog,]

ModelF = glm( newscores ~ dayslist * method + AvgResid + AvgNo0Resid + MedianAttemptsWith0 + MedianNo0Resid + PctSuccUnattResid + PctSuccResid + MaxResid + as.factor(chapter) + Sex + Age + Required + Credits + PrevMath + PrevSemUWEC, data = ExamsSimple, family = "binomial")
ModelFbest = step(ModelF) # best:   dayslist + method + AvgNo0Resid + MedianAttemptsWith0 + MaxResid + as.factor(chapter) + Age + Credits + dayslist:method

# fit best on Exams (full dataset) to get coefficients:
ModelF.1 = glm( newscores ~ dayslist + method + AvgNo0Resid + MedianAttemptsWith0 + MaxResid + as.factor(chapter) + Age + Credits + dayslist:method, data = Exams, family = "binomial")
summary(ModelF.1)

#dayslist             -0.036111   0.005317  -6.791 1.11e-11 ***
#methodB              -0.064555   0.208701  -0.309  0.75708    
#dayslist:methodB      0.014092   0.006734   2.093  0.03636 *  
#Age                  -0.099302   0.031926  -3.110  0.00187 ** 
#Credits               0.012214   0.003781   3.230  0.00124 ** 
exp(coef(ModelF.1))

# fit best on Exams_AIC:
ModelF.2 = glm( newscores ~ dayslist + method + AvgNo0Resid + MedianAttemptsWith0 + MaxResid + as.factor(chapter) + Age + Credits + dayslist:method, data = Exams_AIC, family = "binomial")


AIC(ModelA.1, ModelB.1, ModelC.1, ModelF.2)