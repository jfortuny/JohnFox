##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Appendix on Bootstrapping Regressions   ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##

library(car)     # for Duncan data and (later) dataEllipse and access to MASS
mod.duncan.hub <- rlm(prestige ~ income + education, data=Duncan)
summary(mod.duncan.hub)
boot.huber <- function(data, indices, maxit=20){
    data <- data[indices, ]  # select obs. in bootstrap sample
    mod <- rlm(prestige ~ income + education, data=data, maxit=maxit)
    coefficients(mod)  # return coefficient vector
}
set.seed(12345) # for reproducibility
library(boot)
system.time(duncan.boot <- boot(Duncan, boot.huber, 1999, maxit=200))
duncan.boot
duncan.array <- boot.array(duncan.boot)
duncan.array[1:2, ]
plot(duncan.boot, index=2)  # income coef.
plot(duncan.boot, index=3)  # education coef.
dataEllipse(duncan.boot$t[, 2], duncan.boot$t[, 3],
    xlab="income coefficient", ylab="education coefficient",
    cex=0.3, levels=c(.5, .95, .99), robust=TRUE)

boot.ci(duncan.boot, index=2, type=c("norm", "perc", "bca"))
# education coefficient:
boot.ci(duncan.boot, index=3, type=c("norm", "perc", "bca"))
par(mfcol=c(2, 1))
jack.after.boot(duncan.boot, index=2, main="(a) income coefficient")
jack.after.boot(duncan.boot, index=3, main="(b) education coefficient")
fit <- fitted(mod.duncan.hub)
e <- residuals(mod.duncan.hub)
X <- model.matrix(mod.duncan.hub)
boot.huber.fixed <- function(data, indices, maxit=20){
    y <- fit + e[indices]
    mod <- rlm(y ~ X - 1, maxit=maxit) # constant is already in X
    coefficients(mod)
}
set.seed(54321) # for reproducibility
(duncan.fix.boot <- boot(Duncan, boot.huber.fixed, 1999, maxit=200))
par(mfcol=c(2, 1))
jack.after.boot(duncan.fix.boot, index=2, main="(a) income coefficient")
jack.after.boot(duncan.fix.boot, index=3, main="(b) education coefficient")
b <- coefficients(mod.duncan.hub)
sumry <- summary(mod.duncan.hub)  # to obtain coef. cov. matrix
V <- sumry$cov.unscaled * sumry$stddev^2  # coef. cov. matrix
L <- c(0, 1, -1)  # hypothesis matrix
b1 <- b[2] - b[3]
(t <- b1/sqrt(L %*% V %*% L))  # test statistic
2*pnorm(t, lower.tail=FALSE)   # p-value
boot.test <- function(data, indices, maxit=20){
    data <- data[indices, ]
    mod <- rlm(prestige ~ income + education, data=data, maxit=maxit)
    sumry <- summary(mod)
    V.star <- sumry$cov.unscaled * sumry$stddev^2
    b.star <- coefficients(mod)
    b1.star <- b.star[2] - b.star[3]
    t <- (b1.star - b1)/sqrt(L %*% V.star %*% L)
    t
}
set.seed(2468) # for reproducibility
duncan.test.boot <- boot(Duncan, boot.test, 999, maxit=200)
summary(duncan.test.boot$t)
par(mfrow=c(1, 2))
qqnorm(duncan.test.boot$t)
qqline(duncan.test.boot$t)
abline(0, 1, lty=2)
hist(duncan.test.boot$t, breaks=50)
box()
abline(v=t, lwd=3)
# 2-sided p-value:
2 * (1 + sum(duncan.test.boot$t > as.vector(t)))/1000
