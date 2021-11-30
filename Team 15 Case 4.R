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

coef(logisticmodel)
## DO WE ALSO NEED THESE?
exp(coef(logisticmodel))


# Calculating Model Accuracy and Error Rates
probs <- predict(logisticmodel, test, type="response")
pred <- ifelse(probs>.5, "Yes", "No")

table(pred, test$intubated)              
# pred     No   Yes
#   No  36103  8028
#   Yes     2     1

mean(pred==test$intubated)
# Test Accuracy Rate: 0.8180541

mean(pred!=test$intubated)
# Test Error Rate: 0.1819459


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

##Graphing the LDA
ldaforgraph <- cbind(traintransformed, predict(ldamodel)$x)
ggplot(ldaforgraph, aes(LD1)) + geom_point(aes(color=intubated))  # how to graph when we only have 1 LD?

##Make predictions with the ldamodel using the transformed test data.
covid.lda.values <- predict(ldamodel)
covid.lda.values

prediction <- ldamodel %>% predict(testtransformed)
names(prediction) # "class"     "posterior" "x"

## Calculating accuracy rates
mean(prediction$class == testtransformed$intubated) 
  # 0.8179182 = 81.79% Test Accuracy Rate; our model correctly classifies observations 81.79% of the time
table(prediction$class, testtransformed$intubated) # confusion matrix
#         No   Yes
#  No  36097  8028
#  Yes     8     1


# Confusion Matrix Interpretation



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
knnmodel$bestTune
# the best k is 5 (it is the one with the highest accuracy)


knnclass <- predict(knnmodel, newdata=test)
head(knnclass)
  # we need to make sure that we have our dependent variable (intubated) correct - it is correct here because we have those two levels
  # of No and Yes

## Calculate Accuracy Rates:
table(knnclass, test$intubated)
  # knnclass    No   Yes
  #      No  35293  7675
  #      Yes   812   354

# Test Accuracy Rate
mean(knnclass==test$intubated)
  # Test Accuracy Rate: 0.8076993

# Test Error Rate
mean(knnclass!=test$intubated)
  # Test Error Rate:  0.1923007

confusionMatrix(knnclass, test$intubated)
  # Sensitivity: 0.97751 
  # Specificity: 0.04409

  # Note: our positive class is "No"

#################################

# COMPARING ACCURACY RATES

# Logistic Model: 0.8180541
# LDA: 0.8179182
# QDA: N/A
# KNN: 0.8077    

#################################

# CONCLUSION AND ANALYSIS

# Based on an analysis of the accuracy and error rates for the logistic, LDA, QDA, and KNN models
# when applied to the covid dataset, our recommendation is to select the Logistic Model, as it has the highest
# shared accuracy rate with LDA and is more parsimonious in nature. Essentially, the relatively easier
# interpretability of the logistic model, combined with its high accuracy rate in predicting whether or not a patient
# is intubated based on a variety of factors, makes it the most appropriate and effective model.



