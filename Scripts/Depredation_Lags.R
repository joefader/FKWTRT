############################################################
### sets with preceding depredation


############################################################
### summary
############################################################

sets_deep_MMlag <- sets_deep_all %>%
  filter(lag(MM_YN) == 1)

sets_deep_MMlag %>% group_by(MM_YN) %>%
  summarise(
    total = n(),
    timelag = mean(SET_LAG, na.rm=T),
    distlag = mean(LAG_DIST, na.rm=T),
    mindist = min(LAG_DIST,na.rm=T),
    maxdist = max(LAG_DIST,na.rm=T),
    mintime = min(SET_LAG,na.rm=T),
    maxtime = max(SET_LAG,na.rm=T)
  )
436/2638



############################################################
### EDA
############################################################

## time lag

# binary plot like log reg
sets_deep_MMlag %>%
  ggplot(aes(x = SET_LAG, y = MM_YN)) + geom_point()
# freq plot
ggplot(data = sets_deep_MMlag, aes(x = SET_LAG, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 60)
# histo of set lag
sets_deep_MMlag %>%
  ggplot(aes(SET_LAG)) + geom_histogram(binwidth = 1000)
# boxplot of set lag for depred
ggplot(data = sets_deep_all, mapping = aes(x = MM_YN, y = SET_LAG)) +
  geom_boxplot()
gam_spacetime <- mgcv::gam(MM_YN ~ te(SET_LAG, LAG_DIST, k = 20),
                           data = sets_deep_MMlag, family=binomial, gamma=1.4)

## dist lag

# binary plot like log reg
sets_deep_MMlag %>%
  ggplot(aes(x = LAG_DIST, y = MM_YN)) + geom_point()
# freq plot
ggplot(data = sets_deep_MMlag, aes(x = LAG_DIST^0.5, y = ..density.., colour = MM_YN)) +
  geom_freqpoly(binwidth = 5)
# histo of set lag
sets_deep_MMlag %>%
  ggplot(aes(LAG_DIST)) + geom_histogram(binwidth = 50)
sets_deep_MMlag %>%
       count(cut_width(LAG_DIST, 100))
# boxplot of set lag for depred
ggplot(data = sets_deep_MMlag, mapping = aes(x = MM_YN, y = LAG_DIST)) +
  geom_boxplot()
ggplot(data = sets_deep_MMlag, mapping = aes(x = LAG_DIST)) +
  geom_dotplot()


############################################################
### models
############################################################

gam_spacetime <- mgcv::gam(MM_YN ~ te(SET_LAG, LAG_DIST, k = 20) +
                             s(NUM_HKS_SET, bs='ts', sp=0.6),
                           data = sets_deep_MMlag, family=binomial, gamma=1.4)
summary(gam_spacetime)
vis.gam(gam_spacetime, view=c("SET_LAG", "LAG_DIST"),plot.type="contour",color="heat")


b1 <- glm(MM_YN~ SET_LAG + LAG_DIST,
          data = sets_deep_MMlag, family=binomial)

b2 <- gam(MM_YN~ s(SET_LAG, bs='cr') + s(LAG_DIST, bs='cr') + s(NUM_FLTS, bs='cr'),
                   data=sets_deep_MMlag, family=binomial, method = "REML")
summary(b1)
vis.gam(b2);title("tensor anova")
plot(b2, residuals=T,rug=NULL,se=TRUE,pages=0,select=NULL,scale=0)


