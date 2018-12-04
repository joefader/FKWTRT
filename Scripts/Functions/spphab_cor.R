# Correlate spp presence/absence with habitat variables.
# X, a single species; Y, the environmental/habitat variables (numeric)
spphab.cor <- function (spp, env, trim = TRUE, alpha = 0.05, sort=TRUE) 
{
  n <- dim(env)[[2]]
  variable <- colnames(env)
  coef <- rep(NA,n)
  P <- rep(NA,n)
  for (i in 1:n) {
    ct <- cor.test(spp,env[,i])
    coef[i] <- ct$estimate
    P[i] <- ct$p.value
  }
  shc <- data.frame(variable,coef,P)
  shc
  if (trim) {
    shc <- shc[P <= alpha,]
  }
  if (sort) {
  shc <- shc[order(abs(shc$coef),decreasing=TRUE),]
  }
}
