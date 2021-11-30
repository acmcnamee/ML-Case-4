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
  # A visual check with the ggplot also confirms that there are no extraneous/far away points.

# 4) Independence of Errors
  # This assumption is not violated, as upon a visual check, the data does not appear to have any repeated measures.


# Regression Equation:
b0<-shortlogmodel$coefficients[1];b0
b1<-shortlogmodel$coefficients[2];b1
b2<-shortlogmodel$coefficients[3];b2
probintubated<-exp(b0+b1+b2)/(1+exp(b0+b1+b2));probintubated

# Regression Equation = exp(b0+b1*age+b2*GenderWoman)/(1+exp(b0+b1*age+b2*GenderWoman)) 


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
# factor of 1.0144292. In addition, the odds of a woman being intubated increases by a factor of 0.7407149. 


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

# Interpretation
# The True Positive is 36091, which means we correctly predicted the positive class (not intubated) 36091 times
# The True Negative is 5, which means we correctly predicted the negative class (intubated) 5 times
# The False Positive (Type I Error) is 8024, which means we incorrectly indicated 'not intubated' presents 8024 times
# The False Negative (Type II Error) is 14, which means we incorrectly indicated 'intubated' presents 14 times

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
  # The True Positive is 36080, which means we correctly predicted the positive class (not intubated) 36080 times
  # The True Negative is 11, which means we correctly predicted the negative class (intubated) 11 times
  # The False Positive (Type I Error) is 8018, which means we incorrectly indicated 'not intubated' presents 8018 times
  # The False Negative (Type II Error) is 25, which means we incorrectly indicated 'intubated' presents 25 times

confusionMatrix(prediction$class,testtransformed$intubated)
  # The Accuracy is 0.818, which means the frequency that we correctly predicted classes is 0.818
  # The Confidence Interval is (0.8141, 0.8214), which means that we are 95% confident that our accuracy rate will be between 0.8141 and 0.8214
  # The Sensitivity is 0.99931, which means the frequency that we predicted 'not intubated' when they are actually also 'not intubated' is 99.931%
  # The Specificity is 0.00137, which means the frequency that we predicted 'intubated' when they are actually also 'intubated' is 0.137%
  # The Pos Pred Value is 0.81818, which means when we predicted 'not intubated', 81.81% of them are correct
  # The Neg Pred Value is 0.30556, which means when we predicted 'intubated', 30.56% of them are correct
  # The Prevalence is 0.81808, which means 81.81% of the reference are not intubated, which have the condition of interest
  # The Detection Rate is 0.81751, which means the frequency of correctly predicting 'not intubated' is 81.75%
  # The Detection Prevalence is 0.99918, which means the frequency that we predicted 'not intubated' (positive class) is 99.92%
  # The Balanced Accuracy is 0.50034, which means the average of Sensitivity and Specificity is 50.03%


#################################

# QDA MODEL
qdamodel <- qda(intubated~., data=traintransformed)
  # Error in qda.default(x, grouping, ...) : rank deficiency in group No
  # changing the train/testing group to p=.8 still gives this error message.

  # Error Message Interpretation: As QDA is a more sophisticated technique, so it essentially needs a larger sample size.
  # In addition, it appears that some variables in the dataset may be collinear and one or more covariance matrices cannot be
  # inverted to obtain estimates. Therefore, this model will not run, as the sample size from our training data 
  # is not sufficient to create our model. 

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

# Confusion Matrix Interpretation:
  # The True Positive is 35156, which means we correctly predicted the positive class (not intubated) 35156 times
  # The True Negative is 390, which means we correctly predicted the negative class (intubated) 390 times
  # The False Positive (Type I Error) is 7639, which means we incorrectly indicated 'not intubated' presents 7639 times
  # The False Negative (Type II Error) is 949, which means we incorrectly indicated 'intubated' presents 949 times

confusionMatrix(knnclass, test$intubated)
  # The Accuracy is 0.8053, which means the frequency that we correctly predicted classes is 0.8053
  # The Confidence Interval is (0.8016, 0.809), which means that we are 95% confident that our accuracy rate will be between 0.8016 and 0.809
  # The Sensitivity is 0.97347, which means the frequency that we predicted 'not intubated' when they are actually also 'not intubated' is 97.35%
  # The Specificity is 0.04907, which means the frequency that we predicted 'intubated' when they are actually also 'intubated' is 4.91%
  # The Pos Pred Value is 0.82154, which means when we predicted 'not intubated', 82.15% of them are correct
  # The Neg Pred Value is 0.29142, which means when we predicted 'intubated', 29.14% of them are correct
  # The Prevalence is 0.81808, which means 81.81% of the reference are not intubated, which have the condition of interest
  # The Detection Rate is 0.79637, which means the frequency of correctly predicting 'not intubated' is 79.64%
  # The Detection Prevalence is 0.96937, which means the frequency that we predicted 'not intubated' (positive class) is 96.94%
  # The Balanced Accuracy is 0.51127, which means the average of Sensitivity and Specificity is 51.12%


#################################

# COMPARING ACCURACY RATES

# Logistic Model: 0.8178728
# LDA: 0.8177596
# QDA: N/A
# KNN: 0.8054108    

#################################

# CONCLUSION AND ANALYSIS

# Based on an analysis of the accuracy and error rates for the logistic, LDA, QDA, and KNN models
# when applied to the covid dataset, our recommendation is to select the Logistic Model, as it has the highest
# shared accuracy rate with LDA and is more parsimonious in nature. Essentially, the relatively easier
# interpretability of the logistic model, combined with its high accuracy rate in predicting whether or not a patient
# is intubated based on a variety of factors, makes it the most appropriate and effective model.


# In addition, based on the selection of the logistic model, we can make assumptions about the shape of the data. 
# Specifically, when a logistic regression model is used, it assumes a linear decision boundary. As this model
# has the highest accuracy out of all other models (LDA, QDA, and KNN), we can assume in selection that the data
# has a fairly linear decision boundary. In addition,  LDA assumes that the observations are drawn from a Normal distribution 
# with a common covariance matrix in each class,and so can provide some improvements over logistic regression when this 
# assumption approximately holds. Conversely, logistic regression can outperform LDA if these Normal and covariance assumptions 
# are not met, which could be another occurence with the covid dataset.

