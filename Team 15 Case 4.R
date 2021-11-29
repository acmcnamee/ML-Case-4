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

## LOGISTIC REGRESSION   (****NEED HELP HERE***)
logisticmodel <- glm(intubated~., data=train)
summary(logisticmodel)

# Test Assumptions
# 1) Linearity of the Logit
# 2) Absense of Multicollinearity
# 3) Strongly Influential Outliers


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

# 5) Independence (no repeated measures)


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
#  No   Yes
#  No  36097  8028
#  Yes     8     1


# LDA Regression Equation
# Interpretation


#################################

# QDA MODEL
qdamodel <- qda(intubated~., data=traintransformed)
  # Error in qda.default(x, grouping, ...) : 
  # some group is too small for 'qda'
  # error message interpretation: qda is a more sophisticated technique, so it essentially needs a larger sample size
  # this model will not run!! (****need to confirm and put interpretation here************)


#################################

# KNN MODEL
knnmodel <- train(intubated~., data=train, method="knn", preProcess=c("center", "scale"))
plot(knnmodel)
knnmodel$bestTune
# the best k is 5 (it is the one with the highest accuracy)


## make predictions
knnclass <- predict(knnmodel, newdata=test)
head(knnclass)
# we need to make sure that we have our dependent variable (ownership) correct - it is correct here because we have those two levels
# of Nonowner and Owner

## calculate accuracy rate
table(knnclass, test$Ownership)
  # knnclass   Nonowner Owner
  # Nonowner        6     1
  # Owner           6    11

# we correctly coded 6 nonowners and 11 owners
# remember: Actual values are across the top
# we mistakenly predicted 1 owner as an nonowner, and 6 nonowners as owner

# accuracy rate: we can use either!!
mean(knnclass==test$intubated)
# 0.7083333

# error rate: we can use either!!
mean(knnclass!=test$intubated)
# 0.2916667

confusionMatrix(knnclass, test$intubated)


#################################


# COMPARING ACCURACY RATES


#################################


# CONCLUSION AND ANALYSIS





