## TEAM 15 CASE 4
# Aidan MacCuish, Avery McNamee, Austin Meadows, and Kira Qian
rm(list=ls())

# loading in & inspecting the dataset
covid <- read.csv("Case4.csv", stringsAsFactors=TRUE)
# view(covid)
summary(covid)
str(covid)

## PROBLEM 1
# Creating the Model:
shortlogmodel <- glm(intubated~age+Gender, family=binomial, data=covid)
summary(shortlogmodel)
  # Both variables are significant.

# Checking Assumptions:
# 1) Linearity of the Logit
attach(covid)
plot(age, log(age))
  # The relationship of age with the log of itself appears nonlinear, which arises 
  # some concern regarding the linearity of this predictor.

interaction <- age*log(age)
checkinteract<-glm(intubated~interaction, family=binomial, data=covid)
summary(checkinteract)
  # To confirm the non-linearity of the age predictor, an interaction model was created. 
  # As the predictors are significant, it appears that age is, in fact, non-linear.

# 2) Absense of Multicollinearity
library(car)
vif(shortlogmodel)
  # As the vif score is less than 5 and 10, multicollinearity between the age and Gender predictors is not a concern.  
    
# 3) Strongly Influential Outliers
library(broom)
library(tidyverse)
modelResults <- augment(checkinteract) %>% mutate(index=1:n())
ggplot(modelResults, aes(index, .std.resid)) + geom_point(aes(color=intubated))
ggplot(modelResults, aes(index,.cooksd))+ geom_point(aes(color=intubated))
sum(abs(modelResults$.std.resid)>3)
  # 0 - There appear to be 0 outliers present in our model.
  # ****we are looking at point distance from the group - far away ones are potential outliers

# 4) Independence of Errors
  #***** visual check to see if data includes repeated measures (WHAT DOES THIS MEAN)*************************


# Regression Equation:
# log(intubated/(1-intubated)) = -2.1418297 + 0.0143261*age - 0.3001395*GenderWoman

'''
b0<-shortlogmodel$coefficients[1];b0
b1<-shortlogmodel$coefficients[2];b1
b2<-shortlogmodel$coefficients[3];b2
probintubated<-exp(b0+b1)/(1+exp(b0+b1));probintubated
equation = exp(b0+b1*age+b2*GenderWoman)/(1+exp(b0+b1*age+b2*GenderWoman))  
### I just added these lines but not sure if they are correct - kira
'''

# Interpretations:

coef(shortlogmodel)
# (Intercept)        age GenderWoman 
# -2.1460002   0.0143261  -0.3001395 
# An increase in age is associated with an increase in the probability of intubation. 
# To be precise, a one-unit increase in age is associated with an increase in the log odds of being intubated by 0.0143461 units.
# In addition, being a woman is associated with a 0.3001395 less chance of intubation than being a man.

exp(coef(shortlogmodel))
# (Intercept)         age GenderWoman 
#   0.1169510   1.0144292   0.7407149 
# We can further interpret the age coefficient as for every one year increase in age, the odds of intubation increases by a  
# factor of 1.0144292. In addition, for every (*******NEED TO REVISIT FOR CATEGORICAL*******)


# Calculating Model Accuracy and Error Rates
probs<-predict(shortlogmodel, type="response")
pred<-ifelse(probs>.5, "Yes", "No")
table(pred, covid$intubated)              
# pred    No   Yes
#   No 51579 11470
mean(pred!=covid$intubated)
  # Error Rate: 0.181922
mean(pred==covid$intubated)
  # Accuracy Rate: 0.818078


#################################

# PROBLEM 2
# dividing the data into training and testing groups 
library(caret)
divideData <- createDataPartition(covid$intubated,p=.3, list=FALSE)
train <- covid[divideData,]
test <- covid[-divideData,]

#################################

## LOGISTIC REGRESSION
logisticmodel <- glm(intubated~., data=train, family=binomial)
summary(logisticmodel)

# Test Assumptions
# 1) Linearity of the Logit


# 2) Absense of Multicollinearity (********NEED HELP HERE*********)
vif(logisticmodel)


# 3) Strongly Influential Outliers
modelResults <- augment(checkinteract) %>% mutate(index=1:n())
ggplot(modelResults, aes(index, .std.resid)) + geom_point(aes(color=intubated))
ggplot(modelResults, aes(index,.cooksd))+ geom_point(aes(color=intubated))
sum(abs(modelResults$.std.resid)>3)
  # 0 - There appear to be 0 outliers present in our model.
  # ****we are looking at point distance from the group - far away ones are potential outliers

# 4) Independence of Errors
#***** visual check to see if data includes repeated measures (WHAT DOES THIS MEAN)*************************


# Regression Equation:
## DO WE NEED THIS????

# Interpretations:

coef(logisticmodel)
## DO WE ALSO NEED THESE?
exp(coef(logisticmodel))



# Calculating Model Accuracy and Error Rates      (****WHY ARE THESE NOT WORKING***********)
probs <- predict(logisticmodel, test, type="response") ###### fixed! a 'test' should be added into the predict()! - kira
pred <- ifelse(probs>.5, "Yes", "No")

table(pred, test$intubated)           ###### this data changes each time!  -Kira 
# pred     No   Yes
# No  36101  8026
# Yes     4     3
mean(pred!=test$intubated)
# Test Error Rate: 0.1819459
mean(pred==test$intubated)
# Test Accuracy Rate: 0.8180541


###############################

## LDA MODEL

##Use the preprocess function to center and scale your data and execute on both train and test datasets. 
preprocessing <- train %>% preProcess(method=c("center","scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

##Make an lda model with the training dataset that you just transformed. Print that model to see initial results. 
library(MASS)
ldamodel <- lda(intubated~., data=traintransformed)
ldamodel

# Checking assumptions
# 1) No strongly influential outliers
# 2) Multivariate normality
# 3) Multicollinearity
vif(ldamodel)

# 4) Homoscedasticity
library(lmtest)
bptest(ldamodel)

# 5) Independence (no repeated measures)***


##Graphing the LDA
ldaforgraph <- cbind(traintransformed, predict(ldamodel)$x)
ggplot(ldaforgraph, aes(LD1)) + geom_point(aes(color=intubated))  # how to graph when we only have 1 LD?
###### I don't think we can plot when we have only 1 LD cause the ggplot needs a second LD as the y coordinates - kira

##Make predictions with the ldamodel using the transformed test data.
covid.lda.values <- predict(ldamodel)
covid.lda.values

prediction <- ldamodel %>% predict(testtransformed)
names(prediction) # "class"     "posterior" "x"

## Calculating accuracy rates
mean(prediction$class == testtransformed$intubated) ###### we need to be careful that these data change each time!  -Kira 
  # 0.8179182 = 81.79% Test Accuracy Rate; our model correctly classifies observations 81.79% of the time
table(prediction$class, testtransformed$intubated) # confusion matrix
#  No   Yes
#  No  36097  8028
#  Yes     8     1

# LDA Regression Equation
##### I actually don't think we need this cause it's too long, 
##### but we can write a few elements if we want to include this part, just like this
# LD1+0.2348894449*age-0.1623293967*GenderWoman...    ###### but actually these coefficients change each time we run it! -kira

# Interpretation
# The True Positive is 36098, which means we correctly predicted the positive class (not intubated) 36098 times
# The True Negative is 3, which means we correctly predicted the negative class (intubated) 3 times
# The False Positive (Type I Error) is 8026, which means we incorrectly indicated 'not intubated' presents 8026 times
# The False Negative (Type II Error) is 7, which means we incorrectly indicated 'intubated' presents 7 times

# Confusion Matrix Interpretation
confusionMatrix(prediction$class,testtransformed$intubated)
# The Accuracy is 0.818, which means the frequency that we correctly predicted classes is 0.818
# The Confidence Interval is (0.8144, 0.8216), which means that we are 95% confident that our accuracy rate will be between 0.8144 and 0.8216
# The Sensitivity is 0.9998061, which means the frequency that we predicted 'not intubated' when they are actually also 'not intubated' is 99.98061%
# The Specificity is 0.0003736, which means the frequency that we predicted 'intubated' when they are actually also 'intubated' is 0.03736%
# The Pos Pred Value is 0.8181035, which means when we predicted 'not intubated', 81.81035% of them are correct
# The Neg Pred Value is 0.3000000, which means when we predicted 'intubated', 30% of them are correct
# The Prevalence is 0.8180768, which means 81.80768% of the reference are not intubated, which have the condition of interest
# The Detection Rate is 0.8179182, which means the frequency of correctly predicting 'not intubated' is 81.79182%
# The Detection Prevalence is 0.9997734, which means the frequency that we predicted 'not intubated' (positive class) is 99.97734%
# The Balanced Accuracy is 0.5000899, which means the average of Sensitivity and Specificity is 50.00899%


#################################

# QDA MODEL
qdamodel <- qda(intubated~., data=traintransformed)
  # Error in qda.default(x, grouping, ...) : 
  # some group is too small for 'qda'

  # changing the train/testing group to p=.8 still gives this error message:
      # "Error in qda.default(x, grouping, ...) : rank deficiency in group No"

  # Error Message Interpretation: As QDA is a more sophisticated technique, so it essentially needs a larger sample size.
  # Essentially, this model will not run, as the sample size from our data is not sufficient to create our model. 


#################################

# KNN MODEL
knnmodel <- train(intubated~., data=train, method="knn", preProcess=c("center", "scale"))
plot(knnmodel)
knnmodel$bestTune    ##### I don't know why I have 9 for this part, but probably I am wrong! - kira
# the best k is 5 (it is the one with the highest accuracy)


# Make predictions
knnclass <- predict(knnmodel, newdata=test)
head(knnclass)
  # we need to make sure that we have our dependent variable (intubated) correct - it is correct here because we have those two levels
  # of Nonowner and Owner (********CHANGE THIS**************)

## Calculate Accuracy Rates:
table(knnclass, test$intubated)
# knnclass    No   Yes
# No  35330  7699
# Yes   775   330

# Interpretation
# The True Positive is 35330, which means we correctly predicted the positive class (not intubated) 35330 times
# The True Negative is 330, which means we correctly predicted the negative class (intubated) 330 times
# The False Positive (Type I Error) is 7699, which means we incorrectly indicated 'not intubated' presents 7699 times
# The False Negative (Type II Error) is 775, which means we incorrectly indicated 'intubated' presents 775 times


# Test Accuracy Rate
mean(knnclass==test$intubated)
# The accuracy rate is 0.8079938

# Test Error Rate
mean(knnclass!=test$intubated)
# The error rate is 0.1920062

confusionMatrix(knnclass, test$intubated)

# Confusion Matrix Interpretation
# The Accuracy is 0.808, which means the frequency that we correctly predicted classes is 0.808
# The Confidence Interval is (0.8043, 0.8117), which means that we are 95% confident that our accuracy rate will be between 0.8043 and 0.8117
# The Sensitivity is 0.9785, which means the frequency that we predicted 'not intubated' when they are actually also 'not intubated' is 97.85%
# The Specificity is 0.0411, which means the frequency that we predicted 'intubated' when they are actually also 'intubated' is 4.11%
# The Pos Pred Value is 0.8211, which means when we predicted 'not intubated', 82.11% of them are correct
# The Neg Pred Value is 0.2986, which means when we predicted 'intubated', 29.86% of them are correct
# The Prevalence is 0.8181, which means 81.81% of the reference are not intubated, which have the condition of interest
# The Detection Rate is 0.8005, which means the frequency of correctly predicting 'not intubated' is 80.05%
# The Detection Prevalence is 0.9750, which means the frequency that we predicted 'not intubated' (positive class) is 97.50%
# The Balanced Accuracy is 0.5098, which means the average of Sensitivity and Specificity is 50.98%


#################################


# COMPARING ACCURACY RATES

# Logistic Model:
# LDA:
# QDA:
# KNN:

#################################


# CONCLUSION AND ANALYSIS





>>>>>>> Stashed changes
