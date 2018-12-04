## figures

library(tidyverse)
library(mgcv)
library(feather)
library(grid )

####################################################################################
## some default stuff
####################################################################################

par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
par(oma=c(2,2,2,2))
## gam plots
model_name <- depred_lag3
par(mfrow=c(1,1))
plot.gam(model_name, residuals = T, col= "black")
vis.gam(model_name, residuals = T)


plot(model_name, residuals=T, se=T, select=NULL, scale=0,
     pers=FALSE, jit=FALSE, #xlab=NULL, ylab=NULL, main=NULL, xlim=NULL,
     all.terms=T, shade=FALSE)

plot(model_name, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=T, shade=FALSE, ask=F)
plot.gam(model_name, page=5)
plot(model_name,shade=F,seWithMean=TRUE,scale=0) # better for linear?

## term plots
termplot(model_name, se=T, ask=F, ylim='free', rug=T,partial.resid=T)


####################################################################################
## depredation
####################################################################################

plot(depred_hks4, se=T, select=NULL, scale=0, pages=4,
     jit=F, xlim=NULL, all.terms=T, shade=FALSE, ask=F)
par(mfrow=c(1,1))

par(mfrow=c(1,1))
par(mar=c(5,5,2,1))
par(oma=c(0,0,1,1))
## year
plot(depred_hks4, se=T, select=11, scale=0, ylim= c(-1,1),
     jit=F, all.terms=T, shade=FALSE, ask=F, ylab=("Depredation (Year)"), xlab=("Year"),
     cex.axis=1.4,cex.lab=1.9)

## month
plot(depred_hks4, se=T, select=12, scale=0, rug=T, ylim= c(-1,1), xlim=c(1,12),
     jit=F, all.terms=T, shade=FALSE, ask=F, ylab=("Depredation (Month)"), xlab=("Month"),
     cex.axis=1.4,cex.lab=1.9)

## num hooks
plot(depred_hks4, se=T, select=13, scale=0, xlim=c(0,4000),
     jit=F, all.terms=T, shade=FALSE, ask=F, ylab=("Depredation (Num. Hooks)"),
     xlab=("Num. of hooks set"),cex.axis=1.6,cex.lab=1.8)

## soak
plot(depred_hks4, se=T, select=14, scale=0, rug=T,
     jit=F, all.terms=T, shade=FALSE, ask=F, ylab=("Depredation (Soak)"), xlab=("Soak Time (Hours)"),
     cex.axis=1.7,cex.lab=1.9)

## cpue
plot(depred_hks4, se=T, select=19, scale=0, rug=T, xlim=c(0,100),
     jit=F, all.terms=T, shade=FALSE, ask=F, ylab=("Depredation (CPUE)"), xlab=("CPUE (Fish/1000 hooks)"),
     cex.lab=1.8, cex.axis=1.8)

## soak
plot(depred_hks4, se=T, select=14, scale=0, rug=T,
     jit=F, all.terms=T, shade=FALSE, ask=F, ylab=("Depredation (Soak)"), xlab=("Soak Time (Hours)"),
     cex.axis=1.7,cex.lab=1.9)

termplot(depred_ts4, se=T, ask=F, ylim='free', rug=F)


####################################################################################
####################################################################################
## bycatch
####################################################################################

fkw_hksall3

par(mfrow=c(1,1), mar=c(5,6,2,1), oma=c(0,0,1,1))
    
## CPUE
plot(fkw_hksall3, se=T, select=5, scale=0, ylab=("FKW (CPUE)"), xlab=("CPUE (Fish/1000 hooks)"),
     jit=F,xlim=c(0,45),ylim=c(-8,4), all.terms=T, shade=FALSE, ask=F,
     cex.lab=1.8,cex.axis=1.6)
## MM sum
plot(fkw_hksall3, se=T, select=6, scale=0, ylab=("FKW (MM sum)"), xlab=("Marine mammal damaged fish"),
     jit=F,xlim=c(0,45), all.terms=T, shade=FALSE, ask=F,cex.lab=1.8,cex.axis=1.6)
## MM sum - zoomed in
plot(fkw_hksall3, se=T, select=6, scale=0, ylab=("FKW (MM sum)"), xlab=("Marine mammal damaged fish"),
     jit=F,xlim=c(0,10), all.terms=T, shade=FALSE, ask=F,cex.lab=1.8,cex.axis=1.6)
## BET
plot(fkw_hks3, se=T, select=3, scale=0,
     jit=F, xlim=c(0,40), all.terms=T, shade=FALSE, ask=F)

## hooksperfloat
plot(fkw_hks3, se=T, select=2, scale=0,
     jit=F, xlim=c(15,35), all.terms=T, shade=FALSE, ask=F)



plot(fkw_hksall3, se=T, select=NULL, scale=0,
     jit=F, xlim=NULL, all.terms=T, shade=FALSE, ask=F)

plot(depred_ts4, se=T, select=23, scale=0, rug=F, 
     all.terms=T, shade=FALSE, ask=F, xlab="Pacific Decadal Oscillation")