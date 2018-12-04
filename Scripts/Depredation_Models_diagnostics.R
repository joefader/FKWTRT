


library(tidyverse)
library(mgcv)
library(feather)
library(grid )

####################################################################################
## explore models after run
####################################################################################


# available models:
gam_ts_reml
gam_ts_sp1
gam_ts_gam2

# set model name to use throughout
model_name <- gam_ts_gam6
# summary of model, tests
summary(model_name)
anova(gam_ts_gam7,gam_ts_gam8)
vis.gam(model_name)
AIC(gam_ts_gam6,gam_ts_gam7,gam_ts_gam8)

####################################################################################
## plots of predictors
####################################################################################

## gam plots
par(mfrow=c(1,1))
plot.gam(model_name, residuals = T, col= "black")
vis.gam(model_name, residuals = T)

plot(model_name, residuals=T, se=T, pages=5, select=NULL, scale=0,
     pers=FALSE, jit=FALSE, xlab=NULL, ylab=NULL, main=NULL, xlim=NULL,
     too.far=0.9, all.terms=FALSE, shade=FALSE)

plot(model_name, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=FALSE, shade=FALSE, ask=F)
plot.gam(model_name, page=5)
plot(model_name,shade=F,seWithMean=TRUE,scale=0) # better for linear?

## term plots
termplot(model_name, se=T, ask=F, ylim='free', rug=T)

####################################################################################

# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
x1 <- "SOAK"
# my fxn for ggplots of smooths, not sure lpmatrix is appropriate...
plotsmooths <- function(x1, model_name){
  raw_vector <- model_name$model[noquote(x1)]
  p <- predict(model_name, type="lpmatrix")
  beta <- model_name$coefficients[grepl(x1, names(model_name$coefficients))]
  s <- p[,grepl(x1, colnames(p))] %*% beta
  ggplot(data=cbind.data.frame(s, raw_vector), aes(x=raw_vector, y=s)) + geom_()# +coord_cartesian(ylim=c(-10,10))
}

plotsmooths("ChlA",model_name)

####################################################################################

# from ISLR for plotting 0 to 1, depends on pfit from prediction section below
# haven't finished, first line invalid plot method
plot(model_name$model[noquote(x1)], model_name$y, type= )
points ( jitter ( age ), I(( wage >250) /5) ,cex =.5, pch ="|" ,
           col =" darkgrey ")
lines (age.grid ,pfit , lwd =2, col =" blue ")
matlines (age.grid ,se.bands ,lwd =1, col =" blue", lty =3)


####################################################################################
## from example online
# create a sequence of x variable that spans your range
max_x <- max(model_name$model[noquote(x1)])
min_X <- min(model_name$model[noquote(x1)])
x_seq<-seq(min_X, max_x, length=300)
x_seq<-data.frame(x1=x_seq)

# predict only the x term (the sum of the
# term predictions and the intercept gives you the overall 
# prediction)

pred_terms <- predict(model_name, type="terms", newdata=x_seq, se.fit=TRUE)

# set up the x variable, the fit and the upper and lower
# confidence interval

x_seq_x1 <- x_seq$x1
fit <- pred_terms$fit
fit.up95 <- fit-1.96*pred_terms$se.fit
fit.low95 <- fit+1.96*pred_terms$se.fit

# plot the x smooth but leave blank for
# now so that we can add the line on top of the polygon
plot(x_seq_x1, fit, type="n", lwd=3, xlim=c(-3,90), ylim=c(-20,30),
     main="variable x",
     ylab=paste("s(x1,", round(sum(model_name$edf[-1]),2), ")", sep=""))

# If you want confidence lines instead of a grey poly you can
# use this code
#lines(temperature, fit.up95, lty="dotted")
#lines(temperature, fit.low95, lty="dotted")

# For the confidence grey polygon
polygon(c(x_seq_x1, rev(x_seq_x1)), 
        c(fit.low95,rev(fit.up95)), col="grey",
        border=NA)

lines(temperature, fit,  lwd=2)

####################################################################################
# from Dean/Gui - vis.gam
####################################################################################
# Same, but on the probability scale (not habitat/habitat):
#plot(model_name, shade=T, trans=function(x)exp(x)/(1+exp(x))) 

plot(model_name, residuals = T, shade=T) 
vis.gam(model_name, view=SOAK,ticktype="detailed",color="heat",theta=-35)  
vis.gam(model_name, view=c("MIN_LEN","HKS_PER_FLT"),plot.type="contour",color="heat")
vis.gam(model_name, view=c("HAUL_BEGIN_LON","HAUL_BEGIN_LAT"),plot.type="contour",color="heat")

# More visualization:
?vis.gam

par(mfrow=c(1,1))
vis.gam(model_name, type="response", plot.type="contour", color="terrain") # defaults to the first two variables - others are held at median
vis.gam(model_name, type="response", plot.type="persp", color="terrain", theta=-35) # theta changes the perspective

vis.gam(model_name, view=c("tra","pack_MAM"), type="response", plot.type="contour", color="terrain") 
vis.gam(model_name, view=c("tra","pack_MAM"), type="response", plot.type="persp", color="terrain", theta=-35) 


####################################################################################
## check residuals
####################################################################################

gam.check(model_name)
rsd <- residuals(model_name) # resids for every observation
qq.gam(model_name,rep=100); plot(fitted(model_name),rsd)
## need to extract from training set the actual observations used to fit
plot(model_name$model[noquote(x1)], rsd)
plot(sets_deep_sample$HAUL_BEGIN_LON,rsd)




####################################################################################
### prediction
####################################################################################
## first just training data on scale of the predictor
predict(model_name)[1:20]
pv <- predict(model_name,se=TRUE)
pv$fit[1:5]
pv$se[1:5]
# then on response scale
pv <- predict(model_name,type="response",se=TRUE)
pv$fit[1:5]
pv$se[1:5]
range(pv$fit)
range(pv$se)

## now predictions on test data
# make test dataset of correct length (same as # observations training used)
sets_newtest <- sets_deep_test[1:length(rsd),]
## predictions for predictors and response
predict_test_pred <- predict(model_name, newdata=sets_newtest, se=T)
predict_test_pred$fit
predict_test_resp <- predict(model_name, newdata=sets_newtest, type = "response", se=TRUE)
predict_test_resp$fit
predict_test_resp$se.fit

####################################################################################
### prediction - logistic confidence intervals
####################################################################################

## from page 295 in ISLR
# need to model se as logistic or else conf intervals will include zero
# se_bands gives correct confidence intervals that are all positive (2* so ~95%)
pfit <- exp(predict_test_pred$fit)/(1+exp(predict_test_pred$fit))
se_bands_logit = cbind(predict_test_pred$fit + 2*predict_test_pred$se.fit, predict_test_pred$fit - 2*predict_test_pred$se.fit)
se_bands <- exp(se_bands_logit) / (1 + exp(se_bands_logit))

####################################################################################
### prediction - contribution of predictors
####################################################################################


## contributions of each predictor for each prediction, not sure how to interpret, why some negative?
term_contributions <- predict(model_name, newdata=sets_newtest, type="terms",se=TRUE)
term_contr_fit <- as.data.frame(term_contributions$fit)
mean(term_contr_fit[,1],na.rm=T)

# an intuitive plot, with the fitted model and actual data:
# linear.predictor = x value, fitted.values = y values from full model
# ylim makes y axis bt 0-1, default for x here
# points fxn overlays points without replotting, there are other similar fxns eg lines to put a line in
plot(model_name$linear.predictor, model_name$fitted.values, ylim=c(0,1),col="blue", type="p")
points(model_name$linear.predictor, model_name$y, col="red")
## nicer version in gg
mod_fit <- as.data.frame(cbind(model_name$fitted.values, model_name$linear.predictors, model_name$y))
mod_fit %>%
  ggplot() + geom_point(aes(x =V2, y = V1)) + geom_point(aes(x =V2, y = V3), color="blue")

# approximate explanatory value (R^2 as deviance):
# percent of deviance explained..residual error variance over total variance, then 1 -..
model_dev <- 1 - (model_name$deviance/model_name$null)


# make a vector of zeros to record yes/no predictions
gam_pred = rep(0, nrow(sets_newtest))
gam_pred[predict_test_resp$fit > 0.1] = 1 # changes to Up any of vector that are > x
summary(predict_test_resp$fit)
summary(gam_pred)

## confusion matrix to show classification errors
# percent correct is (sum of diagonals)/(sum over full table)
confmatrix <- table(gam_pred, sets_newtest$MM_YN ) 
conf_total <-  sum(confmatrix)
conf_pos <- confmatrix[1,1] + confmatrix[2,2]
conf_pos/conf_total
(40096+145) / 44980 # training error rate (1 minus this number)
mean(glm.pred==MM_YN) # same, fraction predicted correctly

####################################################################################
### ROC tuning from Deans material
####################################################################################
# tuning the model using ROC curves ...
# attach the library:
library(ROCR)
# create a "prediction" object (table of predicted & actual values):
mmyn_pred <- prediction(model_name$fitted.values, model_name$y)
# create a "performance" object with true positives, false positives:
mmyn_perf <- performance(mmyn_pred, "tpr", "fpr")
# plot a ROC curve:
plot(mmyn_perf, colorize=TRUE)
abline(0,1) # adds the 1:1 line

# compute the area under the ROC curve (AUC):
performance(mmyn_pred, "auc")

# plot sensitivity and specificity together:
plot(performance(mmyn_pred, "sens"),col="blue", lwd=2,ylab="Performance")
plot(performance(mmyn_pred, "spec"), add=TRUE, col="red",lwd=2)
legend("top", horiz=T,legend=c("Sensitivity","Specificity"), text.col=c("red","blue"),bty="n")

# to retrieve actual cut-off values, use our function 
# in script cutoff.ROCR.R
source("Scripts/cutoff.ROCR.R")

cutoff.ROCR(mmyn_pred)	# accept the default: "tpr", target=0.95
cutoff.ROCR(mmyn_pred, method="max")	# maximize TPR + TNR 
cutoff.ROCR(mmyn_pred, "x")		# intersection
cutoff.ROCR(mmyn_pred, "tpr", target=0.90)	# change the TPR target

# see how you did in tuning:
cutoff <- cutoff.ROCR(mmyn_pred, method="max")
mmyn_pred_px <- model_name$fitted.value
mmyn_pred_px[mmyn_pred_px<cutoff] <- 0
mmyn_pred_px[mmyn_pred_px>=cutoff] <- 1
table(mmyn_pred_px,model_name$y)
# (note we greatly improved the classification of TPs at the cost of TNs)



####################################################################################
### 
####################################################################################




gam_mgcv_1 <- mgcv::gam(MM_YN ~ s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10),
                             data=sets_deep_all, family=binomial,gamma=1.4)
gam_mgcv_2 <- mgcv::gam(MM_YN ~ te(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=50),
                        data=sets_deep_all, family=binomial,gamma=1.4)       


summary(gam_mgcv_2)
anova(gam_mgcv_2)
plot(gam_mgcv_2, residuals = T, col= "black")
                               

                               s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                               s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                               
                             
                               s(HAUL_BEGIN_LAT, HAUL_BEGIN_LON, bs='ts', k=50) + 
                               s(ChlA, by=EL_LA_NO, bs='ts', k =20) + s(SSH, by=EL_LA_NO, bs='ts', k =20) +
                               s(SST_2, by=EL_LA_NO, bs='ts', k =20) +
                               s(YEAR, bs='ts', sp=0.6) + s(MONTH, bs='ts', sp=0.6) + 
                               s(NUM_HKS_SET, bs='ts', sp=0.6) + s(SOAK, bs='ts', sp=0.6) + 
                               s(TUNA, bs='ts', sp=0.6) + s(CPUE, bs='ts', sp=0.6) + 
                               s(Slope, bs='ts', sp=0.6) + s(Depth, bs='ts', sp=0.6) + 
                               s(Cont_Dist, bs='ts', sp=0.6) + s(Seamt_Dist, bs='ts', sp=0.6) + 
                               s(EDDY_DIST, by = CYCL_TYPE, bs='ts', k=10) +
                               VESSEL_ID + postTRT + CYCL_TYPE + EL_LA_NO,
                             data=sets_deep_train, family=binomial, gamma=1.4)
