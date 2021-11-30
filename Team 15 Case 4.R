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

mean(pred==covid$intubated)
  # Accuracy Rate: 0.818078
  # Essentially, our model correctly classifies observations 81.81% of the time.

mean(pred!=covid$intubated)
# Error Rate: 0.181922
# Essentially, our model incorrectly classifies observations 18.19% of the time.

#################################

# PROBLEM 2
set.seed(100)

# dividing the data into training and testing groups 
library(caret)
divideData <- createDataPartition(covid$intubated,p=.3, list=FALSE)
train <- covid[divideData,]
test <- covid[-divideData,]

#################################

## LOGISTIC REGRESSION
logisticmodel <- glm(intubated~., data=train, family=binomial)
summary(logisticmodel)

# Calculating Model Accuracy and Error Rates
probs <- predict(logisticmodel, test, type="response")
pred <- ifelse(probs>.5, "Yes", "No")

mean(pred==test$intubated)
# Test Accuracy Rate: 0.8178728
  # Essentially, our model correctly classifies observations 81.79% of the time.

mean(pred!=test$intubated)
  # Test Error Rate: 0.1821272
  # Essentially, our model incorrectly classifies observations 18.21% of the time.

# Confusion Matrix
table(pred, test$intubated)              
  # pred     No   Yes
  #   No  36091  8024
  #   Yes    14     5

# Confusion Matrix Interpretation
  # True Positives: 36091
  # True Negatives: 5
  # False Positives: 8024
  # False Negatives: 14
  
  # Specificity:
5/(8024+5)
  # 0.0006227426
  # We did not predict our negative class Yes (intubated) very well - only about .06% of the time.

  # Sensitivity:
36091/(36091+14)
  # 0.9996122
  # We predicted our positive class No (not intubated) very well - about 99.9% of the time!

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

##Make predictions with the ldamodel using the transformed test data.
covid.lda.values <- predict(ldamodel)
covid.lda.values

prediction <- ldamodel %>% predict(testtransformed)
names(prediction) # "class"     "posterior" "x"

## Calculating accuracy rates
mean(prediction$class==testtransformed$intubated) 
  # 0.8177596 = 81.78% Test Accuracy Rate
  # Essentially, our model correctly classifies observations 81.78% of the time.
mean(prediction$class!=testtransformed$intubated) 
  # Test Error Rate:  0.1822404
  # Essentially, our model incorrectly classifies observations 18.22% of the time.

# Confusion Matrix
table(prediction$class, testtransformed$intubated)
#         No   Yes
#  No  36080  8018
#  Yes    25    11


# Confusion Matrix Interpretation
  # True Positives: 36080
  # True Negatives: 11
  # False Positives: 8018
  # False Negatives: 25

  # Specificity:
11/(8018+11)
  # 0.001370034
  # We did not predict our negative class Yes (intubated) very well - only about .14% of the time.

  # Sensitivity:
36080/(36080+25)
  # 0.9993076
  # We predicted our positive class No (no intubated) very well - about 99.93% of the time!

#################################

# QDA MODEL
qdamodel <- qda(intubated~., data=traintransformed)
  # Error in qda.default(x, grouping, ...) : 
  # some group is too small for 'qda'

  # changing the train/testing group to p=.8 still gives this error message:
      # "Error in qda.default(x, grouping, ...) : rank deficiency in group No"

  # Error Message Interpretation: As QDA is a more sophisticated technique, so it essentially needs a larger sample size.
  # Essentially, this model will not run, as the sample size from our training data is not sufficient to create our model. 


#################################

# KNN MODEL
knnmodel <- train(intubated~., data=train, method="knn", preProcess=c("center", "scale"))
plot(knnmodel)
knnmodel$bestTune
  # The best k is 9
  # Essentially, the k with the highest accuracy is 9.


knnclass <- predict(knnmodel, newdata=test)
head(knnclass)
  # we need to make sure that we have our dependent variable (intubated) correct - 
  # it is correct here because we have those two levels of No and Yes

## Calculate Accuracy Rates:
table(knnclass, test$intubated)
# knnclass    No   Yes
#      No  35156  7639
#      Yes   949   390

# Test Accuracy Rate
mean(knnclass==test$intubated)
  # Test Accuracy Rate: 0.8054108
  # Essentially, our model correctly classifies observations 80.54% of the time.

# Test Error Rate
mean(knnclass!=test$intubated)
  # Test Error Rate: 0.1945892
  # Essentially, our model incorrectly classifies observations 19.46% of the time.

confusionMatrix(knnclass, test$intubated)
  # Sensitivity: 0.97372 
      # We predicted our positive class No (not intubated) very well - about 97.37% of the time!
  # Specificity: 0.04857 
      # We did not predict our negative class Yes (intubated) very well - only about 4.86% of the time.

  # Note: our positive class is "No"

# Confusion Matrix Interpretation:
  # True Positives: 35176
  # True Negatives: 237
  # False Positives: 7792
  # False Negatives: 929


#################################

# COMPARING ACCURACY RATES

# Logistic Model: 0.8178728
# LDA: 0.8177596
# QDA: N/A
# KNN: 0.8023972    

#################################

# CONCLUSION AND ANALYSIS

# Based on an analysis of the accuracy and error rates for the logistic, LDA, QDA, and KNN models
# when applied to the covid dataset, our recommendation is to select the Logistic Model, as it has the highest
# shared accuracy rate with LDA and is more parsimonious in nature. Essentially, the relatively easier
# interpretability of the logistic model, combined with its high accuracy rate in predicting whether or not a patient
# is intubated based on a variety of factors, makes it the most appropriate and effective model.


### COMMENT ON THE SHAPE OF THE DATA
# linear decision boundary
