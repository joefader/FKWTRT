library(ROCR)
# create a "prediction" object (table of predicted & actual values):
piwa.pred <- prediction(piwa.glm$fitted.values, piwa)

MM_pred <- prediction(depred_all3$fitted.values, sets_deep_train$MM_any)
MM_pred <- prediction(as.data.frame(test_gam), sets_deep_test$MM_any)


# create a "performance" object with true positives, false positives:
piwa.perf <- performance (piwa.pred, "tpr", "fpr")
MM_perf <- performance(MM_pred, "tpr", "fpr")

# plot a ROC curve:
plot(MM_perf, colorize=TRUE) # colorized line is all cutoffs from [0,1]
abline(0,1) # adds the 1:1 line
# compute the area under the ROC curve (AUC):
performance(MM_pred, "auc")

# plot sensitivity and specificity together:
plot(performance(MM_pred, "sens"),col="blue", lwd=2,ylab="Performance")
plot(performance(MM_pred, "spec"), add=TRUE, col="red",lwd=2)
legend("top", horiz=T,legend=c("Sensitivity","Specificity"), text.col=c("blue","red"),bty="n")

# to retrieve actual cut-off values, use our function
# in script cutoff.ROCR.R
# either File->Open Script [browse to this file], or
source("cutoff.ROCR.R") # no way to find the intersect in rocr so dean wrote some code for it..
cutoff.ROCR(piwa.pred) # accept the default: default is 'max'
cutoff.ROCR(piwa.pred, method="max") # maximize TPR + TNR..if lots of negatives, might want to maximize
TNs, imbalanced if very diff sample sizes
cutoff.ROCR(piwa.pred, method="tpr") #default for tpr method is .95
cutoff.ROCR(piwa.pred, "x") # intersection...balances error rates, equivalent errors
cutoff.ROCR(piwa.pred, "tpr", target=0.90) # change the TPR target
cutoff.ROCR(oven.pred)
cutoff.ROCR(oven.pred, method='tpr')
# see how you did in tuning:
# for writeup choose whichever method, then assigning value to cutoff var
cutoff <- cutoff.ROCR(piwa.pred, method="max")
piwa.glm.px <- piwa.glm$fitted.value
piwa.glm.px[piwa.glm.px<cutoff] <- 0
piwa.glm.px[piwa.glm.px>=cutoff] <- 1
table(piwa.glm.px,piwa)
# (note we greatly improved the classification of TPs at the cost of TNs)
cutoff <- cutoff.ROCR(oven.pred, method="max")
oven.glm.px <- oven.glm$fitted.value
oven.glm.px[oven.glm.px<cutoff] <- 0
oven.glm.px[oven.glm.px>=cutoff] <- 1
table(oven.glm.px,oven)
cutoff <- cutoff.ROCR(oven.pred, method="tpr")
oven.glm.tpr <- oven.glm$fitted.value
oven.glm.tpr[oven.glm.tpr<cutoff] <- 0
oven.glm.tpr[oven.glm.tpr>=cutoff] <- 1
table(oven.glm.tpr,oven)
cutoff <- cutoff.ROCR(oven.pred, method="x")
oven.glm.x <- oven.glm$fitted.value
oven.glm.x[oven.glm.x<cutoff] <- 0
oven.glm.x[oven.glm.x>=cutoff] <- 1
table(oven.glm.x,oven)