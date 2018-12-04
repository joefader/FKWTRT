library(gam)
library(splines)
library(ggplot2)
library(tidyverse)

###########################################################################
## glm logistic regression
###########################################################################

## binary plot like log reg
sets_alldata %>%
  filter((DECLARED_TRIP == 'D')) %>%
  ggplot(aes(x = set.lag, y = MM_YN)) + geom_point()

sets_alldata %>%
  filter((DECLARED_TRIP == 'D')) %>%
  ggplot(aes(x = HAUL_BEGIN_LAT, y = MM_YN)) + geom_point()

##
# using fxn glm, specify response vector and binomial
# response is initially logical, glm coerces into binary (1,0)

D_sets.date = all_sets.date %>%
  filter((DECLARED_TRIP == 'D'))
D_sets.dplag = all_sets.date %>%
  filter(DECLARED_TRIP == 'D', DP_LAG1 == 1)

attach(D_sets.date)
D_sets.date$MM_YN <- as.factor(D_sets.date$MM_YN)
sapply(D_sets.date, class)

fit=glm(MM_YN ~ SET_BEGIN_LAT + SET_BEGIN_LON + NUM_HKS_SET + TARGET_sum + set.lag, data=D_sets.date, family=binomial)
summary(fit)

fit_lag = glm(MM_YN ~ set.lag, data = D_sets.dplag, family = binomial)
summary(fit_lag)

## predictions
glm.probs = predict(fit, type = "response")
glm.probs[1:10]
contrasts(MM_YN)

glm.pred =rep(0, 44980)
glm.pred[glm.probs >.1] = 1 # changes to Up any of vector that are >.5

table(glm.pred, MM_YN ) # confusion matrix to show classification errors
(40096+145) / 44980 # training error rate (1 minus this number)
mean(glm.pred==MM_YN) # same, fraction predicted correctly

## use subset of data to train, and remaining to test

D_sets.date$year <- format(D_sets.date$SET_BEGIN_DATE, "%Y")
train <- (year < 2011) #creates boolean vector in which observations before 2011 are TRUE
D_sets.2011 <- D_sets.date[!train,] #subsets data to those in which train is FALSE, ie in 2010
dim(D_sets.2011)
MM_YN.2011 = MM_YN [!train]

## now do log regression on training data (pre 2011), followed by test on 2011-2016 data
fit = glm(MM_YN ~ SET_BEGIN_LAT + SET_BEGIN_LON + NUM_HKS_SET + TARGET_sum + set.lag,
          data=D_sets.date, family=binomial, subset = train)

glm.probs <- predict(fit, D_sets.2011, type = "response")

## see how test did
glm.pred = rep(0, 22376)
glm.pred[glm.probs >.1] = 1
table(glm.pred, MM_YN.2011)
mean(glm.pred == MM_YN.2011)
mean(glm.pred != MM_YN.2011)  ## test error rate

###only got to here, not able to get mean/ error rates for subset data returns NA

# ## try removing least important variables, none were significant, but using 2 highest here
# glm.fit <- glm(Direction ~ Lag1 + Lag2,
#                data=Smarket, family = binomial, subset = train)
# glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
# 
# ## this improves things slightly, if model predicts decline, right 50% of time,
# ## if it predicts an increase, right 56% of time
# glm.pred =rep("Down", 252)
# glm.pred[glm.probs >.5] = "Up"
# table(glm.pred, Direction.2005)
# mean(glm.pred==Direction.2005)
# mean(glm.pred!=Direction.2005)
# 
# ## what does model predict based on specific values?
# predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5),
#                                     Lag2 =c(1.1, -0.8)), type ="response")
# 






