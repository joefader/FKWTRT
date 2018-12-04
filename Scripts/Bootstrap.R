#############################################################################
# bootstrap/permutation testing for FKW observer data
# Joseph Fader
# January 4, 2018

library(tidyverse)
library(feather)
library(boot)
library(perm)

#############################################################################
### cpue
#############################################################################

sets_deep_all %>% 
  #filter(DP_LAG1 == 1) %>%
  group_by(MM_any) %>%
  summarise(
    n(),
    mean(CPUE),
    sd(CPUE),
    mean(KEPT),
    sd(KEPT),
    mean(KEPT/NUM_HKS_SET*1000),
    sd(KEPT/NUM_HKS_SET*1000)
  )

f <- function(c) {
  A <- sample(filter(sets_deep_all, MM_YN == 0)[,c], nrow(filter(sets_deep_all, MM_YN == 0)), replace=T) 
  B <- sample(filter(sets_deep_all, MM_YN == 1)[,c], nrow(filter(sets_deep_all, MM_YN == 1)), replace=T)
  mean(A)-mean(B)
}

sets_deep_all <- as.data.frame(sets_deep_all)
## cpue
# sample mean difference
cpue_sam <- mean(filter(sets_deep_all, MM_YN == 0)[,"CPUE"]) - mean(filter(sets_deep_all, MM_YN == 1)[,"CPUE"])
# bootstrap
cpue_boot <- replicate(1000, f(51))
mean(cpue_boot)
# bias
mean(cpue_boot) - (mean(filter(sets_deep_all, MM_YN == 0)[,"CPUE"]) - mean(filter(sets_deep_all, MM_YN == 1)[,"CPUE"]))
# standard error
sd(cpue_boot)
# t confidence intervals
mean(cpue_boot) + 1.96*sd(cpue_boot)
mean(cpue_boot) - 1.96*sd(cpue_boot)
# percentile cis
quantile(cpue_boot, c(0.025, 0.975))
# plot
hist(cpue_boot)

#############################################################################
### 
#############################################################################

sets_deep_all %>% 
  filter(DP_LAG1 == 1) %>%
  group_by(MM_YN) %>%
  summarise(
    n(),
    mean(LAG_DIST),
    sd(LAG_DIST),
    mean(SET_LAG),
    sd(SET_LAG)
  )
## make df of sets with previous depredation
dplag <- sets_deep_all %>%
  filter(DP_LAG1 == 1) %>%
  select(DP_LAG1, MM_YN, LAG_DIST, SET_LAG) %>% 
  drop_na() %>%
  arrange(MM_YN)

dplag <- as.data.frame(dplag)
## sample mean difference
sapply(dplag, class)
mean(filter(dplag, MM_YN == 1)$LAG_DIST)
mean(filter(dplag, MM_YN == 0)[,"LAG_DIST"]) - mean(filter(dplag, MM_YN == 1)[,"LAG_DIST"])
mean(filter(dplag, MM_YN == 0)[,"SET_LAG"], na.rm=T) - mean(filter(dplag, MM_YN == 1)[,"SET_LAG"], na.rm=T)

dplag %>% 
  ggplot(aes(x = LAG_DIST, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 100 )
dplag %>% 
  ggplot(aes(x = SET_LAG, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 10 )

# lag0 <- sets_deep_all %>%
#   filter(DP_LAG1 == 1, MM_YN == 0) %>%
#   select(DP_LAG1, MM_YN, LAG_DIST, SET_LAG)
# lag1 <- sets_deep_all %>%
#   filter(DP_LAG1 == 1, MM_YN == 1) %>%
#   select(DP_LAG1, MM_YN, LAG_DIST, SET_LAG)




#############################################################################
### using boot package
#############################################################################
## function for boot from R documentation
meanDiff <- function(d, f)
{    n <- nrow(d)
gp1 <- 1:table(as.numeric(d$MM_YN))[1]
# m1 <- sum(d[gp1,3])/length(gp1)
# m2 <- sum(d[-gp1,3])/(n-length(gp1))
m1 <- sum(d[gp1,3] * f[gp1])/sum(f[gp1])
m2 <- sum(d[-gp1,3] * f[-gp1])/sum(f[-gp1])
m1 - m2
}
## boot
boot_lag <- boot(dplag, statistic = meanDiff, R = 3000, stype = "f", strata = dplag[,2])

20.17 + 1.96*6.05
20.17 - 1.96*6.05
boot.ci(boot.out = boot_lag, type = c("norm", "basic", "perc")) #, "bca"))
plot(boot_lag)

### R example
meanDiff <- function(d, f)
{    n <- nrow(d)
gp1 <- 1:table(as.numeric(d$series))[1]
m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
ss1 <- sum(d[gp1,1]^2 * f[gp1]) - (m1 *  m1 * sum(f[gp1]))
ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 *  m2 * sum(f[-gp1]))
c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
}

grav1 <- gravity[as.numeric(gravity[,2]) >= 7,]
boot(grav1, meanDiff, R = 999, stype = "f", strata = grav1[,2])
grav1 %>% group_by(series) %>% 
  summarize(
    mean(g)
  )

?boot
?resample
?gravity
View(gravity)

#############################################################################
### own function
#############################################################################

f <- function(c) {
  A <- sample(filter(dplag, MM_YN == 0)[,c], nrow(filter(dplag, MM_YN == 0)), replace=T) 
  B <- sample(filter(dplag, MM_YN == 1)[,c], nrow(filter(dplag, MM_YN == 1)), replace=T)
  mean(A)-mean(B)
}

## lag distance
# sample mean difference
lag_dist_sam <- mean(filter(dplag, MM_YN == 0)[,"LAG_DIST"]) - mean(filter(dplag, MM_YN == 1)[,"LAG_DIST"])
# bootstrap
lag_dist_boot <- replicate(1000, f(3))
mean(lag_dist_boot)
# bias
mean(lag_dist_boot) - (mean(filter(dplag, MM_YN == 0)[,"LAG_DIST"]) - mean(filter(dplag, MM_YN == 1)[,"LAG_DIST"]))
# standard error
sd(lag_dist_boot)
# t confidence intervals
mean(lag_dist_boot) + 1.96*sd(lag_dist_boot)
mean(lag_dist_boot) - 1.96*sd(lag_dist_boot)
# percentile cis
quantile(lag_dist_boot, c(0.025, 0.975))
# plot
hist(lag_dist_boot)



## lag time
# sample mean difference
lag_time_sam <- mean(filter(dplag, MM_YN == 0)[,"SET_LAG"], na.rm=T) - mean(filter(dplag, MM_YN == 1)[,"SET_LAG"], na.rm=T)
# bootstrap
lag_time_boot <- replicate(5000, f("SET_LAG"))
mean(lag_time_boot)
# bias
mean(lag_time_boot) - (mean(filter(dplag, MM_YN == 0)[,"SET_LAG"]) - mean(filter(dplag, MM_YN == 1)[,"SET_LAG"]))
# standard error
sd(lag_time_boot)
# t confidence intervals
mean(lag_time_boot) + 1.96*sd(lag_time_boot)
mean(lag_time_boot) - 1.96*sd(lag_time_boot)
# percentile cis
quantile(lag_time_boot, c(0.025, 0.975))
# plots
hist(lag_time_boot)


?sample
?replicate

filter(dplag, MM_YN == 1)[,3]
nrow(filter(dplag, MM_YN == 1))


#############################################################################
### permutation test
#############################################################################

fperm <- function(c) {
  n <- nrow(dplag)
  Ai <- sample(1:n, size = nrow(filter(dplag, MM_YN == 0)), replace = F)
  A <- dplag[Ai, c]
  B <- dplag[-Ai, c]
  mean(A)-mean(B)
}

## lag distance
# sample mean difference
lag_dist_sam
# permute
lag_dist_perm <- replicate(5000, fperm(3))
# figures. prob T gives densities so can plot smooth over
hist(lag_dist_perm)
hist(lag_dist_perm, prob="TRUE")
abline(v = mean(lag_dist_sam), col="red")
lines(density(lag_dist_perm), col = "blue")
lines(density(lag_dist_perm, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

# p values
1-ecdf(lag_dist_perm)(lag_dist_sam)
plot(ecdf(lag_dist_perm))





## lag time
# sample mean difference
lag_time_sam
# permute
lag_time_perm <- replicate(5000, fperm(4))
# figures. prob T gives densities so can plot smooth over
hist(lag_time_perm)
hist(lag_time_perm, prob="TRUE")
abline(v = mean(lag_time_sam), col="red")
lines(density(lag_time_perm), col = "blue")
lines(density(lag_time_perm, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

mean(lag_time_perm)

1-ecdf(lag_time_perm)(lag_time_sam)
plot(ecdf(lag_time_perm))

?hist


#############################################################################
### bootstrap percent/ratio of repeat dp by lag classes - binary resampling
#############################################################################

# original sample counts
dplag %>% group_by(MM_YN) %>% 
  count()

# function resampling all of dplag then finding mean (ie ratio of dp occurrences)
lag_sample <- function(df, c) {
  A <- sample(df[,c], nrow(df), replace=T)
  sum(A == '1') / length(A)
}

## function to resample/bootstrap original sampling function, returns parameters
boot_stats <- function(reps, df, c){
  boot_means <- replicate(reps, lag_sample(df, c))
  boot_avg <- mean(boot_means)
  sample_avg <- sum(df$MM_YN == '1') / nrow(df)
  bias <- boot_avg - sample_avg
  cis <- quantile(boot_means, c(0.025, 0.975))
  return(c("boot_avg" = boot_avg, "orig_sample_avg" = sample_avg, "bias" = bias, cis))
}

boot_stats(5000, dplag, 2)

# make subsets of lag classes
lag_class <- function(lower, upper) {
  sets_deep_all %>%
  select(DP_LAG1, MM_YN, LAG_DIST, SET_LAG) %>% 
  #filter(DP_LAG1 == 1 & LAG_DIST > lower & LAG_DIST <= upper) %>%
  filter(DP_LAG1 == 1 & SET_LAG > lower & SET_LAG <= upper) %>%
  drop_na()
}
distlag_0to50 <- boot_stats(5000, lag_class(0, 50), 2)
distlag_50to75 <- boot_stats(5000, lag_class(50,75), 2)
distlag_75to100 <- boot_stats(5000, lag_class(75,100), 2)
distlag_100plus <- boot_stats(5000, lag_class(100,2000), 2)

timelag_0to36 <- boot_stats(5000, lag_class(0, 36), 2)
timelag_36to60 <- boot_stats(5000, lag_class(36,60), 2)
timelag_60plus <- boot_stats(5000, lag_class(60,200), 2)




