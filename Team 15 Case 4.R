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
logCovid1 <- glm(intubated~age+Gender, family=binomial, data=covid)
summary(logCovid1)
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
vif(logCovid1)
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

coef(logCovid1)
# (Intercept)        age GenderWoman 
# -2.1460002   0.0143261  -0.3001395 
# An increase in age is associated with an increase in the probability of intubation. 
# To be precise, a one-unit increase in age is associated with an increase in the log odds of being intubated by 0.0143461 units.
# In addition, being a woman is associated with a 0.3001395 less chance of intubation than being a man.

exp(coef(logCovid1))
# (Intercept)         age GenderWoman 
#   0.1169510   1.0144292   0.7407149 
# We can further interpret the age coefficient as for every one year increase in age, the odds of intubation increases by a  
# factor of 1.0144292. In addition, for every (*******NEED TO REVISIT FOR CATEGORICAL*******)


# Calculating Model Accuracy and Error Rates
probs<-predict(logCovid1, type="response")
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
divideData <- createDataPartition(covid$intubated,p=.8, list=FALSE)
train <- covid[divideData,]
test <- covid[-divideData,]

## LOGISTIC REGRESSION   (****NEED HELP HERE***)
logCovid2 <- glm(intubated~.- pregnant, data=train)
summary(logCovid2)

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
covid.lda <- lda(intubated~., data=traintransformed)
covid.lda

# Checking assumptions
# 1) No strongly influential outliers
# 2) Multivariate normality
# 3) Multicollinearity
vif(covid.lda)

# 4) Homoscedasticity
library(lmtest)
bptest(covid.lda)

# 5) Independence (no repeated measures)


##Graphing the LDA
ldaforgraph <- cbind(traintransformed, predict(covid.lda)$x)
ggplot(ldaforgraph, aes(LD1)) + geom_point(aes(color=intubated))  # how to graph when we only have 1 LD?

##Make predictions with the ldamodel using the transformed test data.
covid.lda.values <- predict(covid.lda)
covid.lda.values

prediction <- covid.lda %>% predict(testtransformed)
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

#################################


# COMPARING ACCURACY RATES


#################################


# CONCLUSION AND ANALYSIS





