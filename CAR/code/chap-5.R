
    ###*** START ECHO=FALSE ***###
if (exists(".options")) options(.options)
remove(list=objects(.GlobalEnv), envir=.GlobalEnv)
while(length(search()) > 9) detach()
options(width=66,show.signif.stars=FALSE,digits=4)
palette(rep("black", 8))
require("lattice")
lattice.options(default.theme = canonical.theme(color = FALSE))
library(methods)
print.summary.glm <-
function (x, digits = max(3, getOption("digits") - 3), symbolic.cor = x$symbolic.cor,
    signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call, width.cutoff=50), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
    cat("Deviance Residuals: \n")
    if (x$df.residual > 5) {
        x$deviance.resid <- quantile(x$deviance.resid, na.rm = TRUE)
        names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q",
            "Max")
    }
    print.default(x$deviance.resid, digits = digits, na.print = "",
        print.gap = 2)
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    }
    else {
        df <- if ("df" %in% names(x))
            x[["df"]]
        else NULL
        if (!is.null(df) && (nsingular <- df[3L] - df[1L]))
            cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n",
                sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if (!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4L, dimnames = list(cn,
                colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
            na.print = "NA", ...)
    }
    cat("\n(Dispersion parameter for ", x$family$family, " family taken to be ",
        format(x$dispersion), ")\n\n", apply(cbind(paste(format(c("Null",
            "Residual"), justify = "right"), "deviance:"), format(unlist(x[c("null.deviance",
            "deviance")]), digits = max(5, digits + 1)), " on",
            format(unlist(x[c("df.null", "df.residual")])), " degrees of freedom\n"),
            1L, paste, collapse = " "), sep = "")
    if (nzchar(mess <- naprint(x$na.action)))
        cat("  (", mess, ")\n", sep = "")
    cat("AIC: ", format(x$aic, digits = max(4, digits + 1)),
        "\n\n", "Number of Fisher Scoring iterations: ", x$iter,
        "\n", sep = "")
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1) {
            cat("\nCorrelation of Coefficients:\n")
            if (is.logical(symbolic.cor) && symbolic.cor) {
                print(symnum(correl, abbr.colnames = NULL))
            }
            else {
                correl <- format(round(correl, 2), nsmall = 2,
                  digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop = FALSE], quote = FALSE)
            }
        }
    }
    cat("\n")
    invisible(x)
}
    ###*** END ECHO=FALSE ***###


    ###*** START ECHO=FALSE ***###
Probit <- binomial(link=probit)
Logit <- binomial(link=logit)
Cloglog <- binomial(link=cloglog)
range <- seq(-10,10,length=1000)
plot(range,Logit$linkinv(range),type="l", xlim=c(-5,5), lty=1,
       xlab=expression(eta(x)), ylab=expression(mu(x)))
lines(sqrt(pi^2/3)*range, Probit$linkinv(range), lty=2)
lines(range,Cloglog$linkinv(range), lty=4, lwd=2)
legend("topleft",c("logit", "probit", "cloglog"), lty=c(1,2,4),
       lwd=c(1,1,2), inset=0.02)
    ###*** END ECHO=FALSE ***###


    ###*** START ECHO=FALSE ***###
set.seed(100)
    ###*** END ECHO=FALSE ***###

library(car)
some(Mroz)  # sample 10 rows
nrow(Mroz)
mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
   family=binomial, data=Mroz)

    ###*** START EVAL=FALSE ***###
mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
   family=binomial(link=logit), data=Mroz)
    ###*** END EVAL=FALSE ***###

summary(mroz.mod)
round(exp(cbind(Estimate=coef(mroz.mod), confint(mroz.mod))), 2)
mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)
anova(mroz.mod.2, mroz.mod, test="Chisq")
Anova(mroz.mod)
head(predict(mroz.mod)) # first 6 values
head(predict(mroz.mod, type="response"))
closeness <- factor(rep(c("one.sided", "close"), c(3, 3)),
    levels=c("one.sided", "close"))
preference <- factor(rep(c("weak", "medium", "strong"), 2),
    levels=c("weak", "medium", "strong"))
voted <- c(91, 121, 64, 214, 284, 201)
did.not.vote <- c(39, 49, 24, 87, 76, 25)
logit.turnout <- log(voted/did.not.vote)
Campbell <- data.frame(closeness, preference, voted, did.not.vote,
     logit=logit.turnout)
Campbell
oldpar <- par(mar=c(5.1, 4.1, 4.1, 4.1)) # add room for right-side axis
with(Campbell,
    interaction.plot(preference, closeness, logit,
        type="b", pch=c(1, 16), cex=2, ylab="log(Voted/Did Not Vote)"))
probabilityAxis(side="right", at=seq(0.7, 0.875, by=0.025),
    axis.title="Proportion(Voted)")  # right y-axis
par(oldpar) # restore default margins

    ###*** START ECHO=FALSE ***###
par(mar=c(5.1, 4.1, 4.1, 4.1))  # leave room for right axis
plot(rep(1:3, 2), logit.turnout, type="n", axes=FALSE,
    xlab="Intensity of Preference",
    ylab="Logit(Voted/Did Not Vote)")
axis(1, at=1:3, labels=c("Weak", "Medium", "Strong"))  # x-axis
axis(2)  # y-axis
prob.axis(side="right", at=seq(.7, .85, by=.05),
    axis.title="Proportion(Voted)")  # right y-axis
box()
points(1:3, logit.turnout[1:3], pch=1, type="b",
    lty=1, lwd=3, cex=2)  # one-sided
points(1:3, logit.turnout[4:6], pch=16, type="b",
    lty=2, lwd=3, cex=2)  # close
#text(locator(2), c("Close", "One-Sided"))  # position the labels
    ###*** END ECHO=FALSE ***###

campbell.mod <- glm(cbind(voted, did.not.vote) ~
    closeness*preference, family=binomial, data=Campbell)
summary(campbell.mod)
predict(campbell.mod, type="link")
campbell.mod.2 <- update(campbell.mod,
    . ~ . - closeness:preference) # no interactions
anova(campbell.mod.2, campbell.mod, test="Chisq")
Anova(campbell.mod)
c(df=df.residual(campbell.mod.2), Test=deviance(campbell.mod.2))

    ###*** START ECHO=FALSE ***###
set.seed(123456)
    ###*** END ECHO=FALSE ***###

Campbell.long <- data.frame(close=NULL, prefer=NULL,
    turn=NULL) # initialize an empty data frame
for (j in 1:6) { # loop over combinations of factors
  x1 <- with(Campbell,
    data.frame(close=closeness[j],
        prefer=preference[j],
        turn=rep("did.not.vote", did.not.vote[j]))) # non-voters rows
  x2 <- with(Campbell,
    data.frame(close=closeness[j],
        prefer=preference[j],
        turn=rep("voted", voted[j]))) # rows for voters
 Campbell.long <- rbind(Campbell.long, x1, x2) # build up rows
}
some(Campbell.long) # sample rows
nrow(Campbell.long)
ftable(xtabs(~ close + prefer + turn, data=Campbell.long))
campbell.mod.long <- glm(turn ~ close*prefer,
    family=binomial, data=Campbell.long)
summary(campbell.mod.long)
Anova(campbell.mod.long)

    ###*** START ECHO=FALSE ***###
set.seed(12345)
    ###*** END ECHO=FALSE ***###

some(Ornstein)
nrow(Ornstein)

    ###*** START ECHO=FALSE ***###
tab <- xtabs(~interlocks, data=Ornstein)
    ###*** END ECHO=FALSE ***###


    ###*** START Sinput ***###
(tab <- xtabs(~ interlocks, data=Ornstein))
    ###*** END Sinput ***###

x <- as.numeric(names(tab)) # names are distinct values of interlocks
plot(x, tab, type="h", xlab="Number of Interlocks", ylab="Frequency")
points(x, tab, pch=16)
mod.ornstein <- glm(interlocks ~ log2(assets) + nation + sector,
   family=poisson, data=Ornstein)
summary(mod.ornstein)
Anova(mod.ornstein)

    ###*** START ECHO=FALSE ***###
opt <- options(digits=6)
    ###*** END ECHO=FALSE ***###

exp(coef(mod.ornstein))

    ###*** START ECHO=FALSE ***###
options(opt)
    ###*** END ECHO=FALSE ***###

library(effects)
plot(allEffects(mod.ornstein, default.levels=50), ask=FALSE)

    ###*** START ECHO=FALSE ***###
AMSsurvey$class <- NULL # delete this column from the data frame
    ###*** END ECHO=FALSE ***###

head(AMSsurvey)  # first 6 rows
nrow(AMSsurvey)
(tab.sex.citizen <- xtabs(count ~ sex + citizen, data=AMSsurvey))
chisq.test(tab.sex.citizen, correct=FALSE) # suppress Yates correction
(AMS2 <- as.data.frame(tab.sex.citizen))
(phd.mod.indep <- glm(Freq ~ sex + citizen, family=poisson, data=AMS2))
pchisq(2.57, df=1, lower.tail=FALSE)
sum(residuals(phd.mod.indep, type="pearson")^2)
phd.mod.all <- glm(count ~ type*sex*citizen, # saturated model
    family=poisson, data=AMSsurvey)
Anova(phd.mod.all)
summary(phd.mod.1 <- update(phd.mod.all,
    . ~ .- sex:citizen - type:sex:citizen))
pchisq(1.9568, df=6, lower.tail=FALSE)
Campbell
library(reshape)
(Campbell1 <- melt(Campbell,
    id.vars=c("closeness", "preference"),
    measure.var=c("voted", "did.not.vote"),
    variable_name="turnout"))
mod.loglin <- glm(value ~ closeness*preference*turnout,
   family=poisson, data=Campbell1)
Anova(mod.loglin)

    ###*** START ECHO=FALSE ***###
set.seed=12345
    ###*** END ECHO=FALSE ***###

some(Salaries)
nrow(Salaries)
ftable(tab1 <- xtabs(~ rank + discipline + sex, data=Salaries))
(Salaries1 <- data.frame(tab1))

    ###*** START ECHO=FALSE ***###
set.seed(101)
    ###*** END ECHO=FALSE ***###

some(Womenlf)
nrow(Womenlf)
library(nnet)
Womenlf$partic <- factor(Womenlf$partic,
    levels=c("not.work", "parttime", "fulltime"))
mod.multinom <- multinom(partic ~ hincome + children + region,
    data=Womenlf)
Anova(mod.multinom)
mod.multinom.1 <- update(mod.multinom, . ~ . - region)
summary(mod.multinom.1, Wald=TRUE)
library(effects)
plot(allEffects(mod.multinom.1), ask=FALSE)
plot(effect("hincome*children", mod.multinom.1))
Womenlf$working <- with(Womenlf,
       recode(partic, " 'not.work' = 'no'; else = 'yes' "))
Womenlf$fulltime <- with(Womenlf,recode (partic,
       " 'fulltime' = 'yes'; 'parttime' = 'no'; 'not.work' = NA "))
xtabs(~ partic + working, data=Womenlf)
xtabs(~ partic + fulltime, data=Womenlf)
mod.working <- glm(working ~ hincome + children + region,
  family=binomial, data=Womenlf)
summary(mod.working)
mod.fulltime <- update(mod.working, fulltime ~ .)
summary(mod.fulltime)
Anova(mod.working)
Anova(mod.fulltime)
mod.working.1 <- update(mod.working, . ~ . - region)
mod.fulltime.1 <- update(mod.fulltime, . ~ . - region)

    ###*** START ECHO=FALSE ***###
Predictors <- expand.grid(hincome=1:45,
                          children=c("absent", "present"))
    ###*** END ECHO=FALSE ***###


    ###*** START Sinput ***###
(Predictors <- expand.grid(hincome=1:45,
               children=c("absent", "present")))
    ###*** END Sinput ***###

p.work <- predict(mod.working.1, newdata=Predictors, type="response")
p.fulltime <- predict(mod.fulltime.1, newdata=Predictors,
               type="response")
p.full <- p.work*p.fulltime
p.part <- p.work*(1 - p.fulltime)
p.not <- 1 - p.work
par(mfrow=c(1, 2))  # 1 row and 2 columns of panels
plot(c(1, 45), c(0, 1),
    type="n", xlab="Husband's Income", ylab="Fitted Probability",
    main="Children Absent")
lines(1:45, p.not[1:45], lty="solid", lwd=3)   # not working
lines(1:45, p.part[1:45], lty="dashed", lwd=3)  # part-time
lines(1:45, p.full[1:45], lty="dotted", lwd=3)  # full-time
legend("topright", lty=1:3, lwd=3, cex=0.75, inset=0.01,
    legend=c("not working", "part-time", "full-time"))
plot(c(1, 45), c(0, 1),
    type="n", xlab="Husband's Income", ylab="Fitted Probability",
    main="Children Present")
lines(1:45, p.not[46:90], lty="solid", lwd=3)
lines(1:45, p.part[46:90], lty="dashed", lwd=3)
lines(1:45, p.full[46:90], lty="dotted", lwd=3)

    ###*** START ECHO=FALSE ***###
x <- seq(1, 100, length=500)
logit.1 <- 5 -.2*x
logit.2 <- 7 -.2*x
logit.3 <- 13 -.2*x
p.1 <- 1/(1 + exp(logit.1))
p.2 <- 1/(1 + exp(logit.2))
p.3 <- 1/(1 + exp(logit.3))
plot(c(1, 100), c(0,1),
    type='n', xlab=expression(eta), ylab='Probability',
    axes=F)
axis(2)
box()
lines(x, p.1, lty=1, lwd=3)
lines(x, p.2, lty=2, lwd=3)
lines(x, p.3, lty=3, lwd=3)
legend("bottomright", lty=1:3, lwd=3,
    legend=c('Pr(y > 1)', 'Pr(y > 2)', 'Pr(y > 3)'))
remove(x, logit.1, logit.2, logit.3, p.1, p.2, p.3)
    ###*** END ECHO=FALSE ***###

library(MASS) # actually previously loaded by library(car)
mod.polr <- polr(partic ~ hincome + children, data=Womenlf)
summary(mod.polr)
pchisq(deviance(mod.polr) - deviance(mod.multinom.1),
    df = 6 - 4, lower.tail=FALSE)
plot(effect("hincome*children", mod.polr))
plot(effect("hincome*children", mod.polr), style="stacked")
plot(effect("hincome*children", mod.polr, latent=TRUE))

    ###*** START ECHO=FALSE ***###
x <- seq(0.1, 4, by=.02)
d.5 <- dgamma(x, shape=.5, scale=1/.5)
d1 <- dgamma(x, shape=1, scale=1/1)
d2 <- dgamma(x, shape=2, scale=1/2)
d5 <- dgamma(x, shape=5, scale=1/5)
plot(rep(x,4), c(d.5, d1, d2, d5), type='n',
 xlab="y", ylab="p(y)")
lines(x, d.5, lty=1, lwd=2)
lines(x, d1, lty=1, lwd=2)
lines(x, d2, lty=1, lwd=2)
lines(x, d5, lty=1, lwd=2)
text (c(0.1,0.2, 0.8, 1.0), c( 1.1, 0.85, 0.65, 0.9), pos=4,
labels=c(expression(alpha == 0.5),
    expression(alpha == 1),expression(alpha == 2),expression(alpha == 5)))
abline(h=0, lty=2)
    ###*** END ECHO=FALSE ***###

trans.gamma <- glm(time ~ t1 + t2, family=Gamma(link=identity),
                data=Transact)
summary(trans.gamma)
gamma.shape(trans.gamma)
(phihat <- sum(residuals(mod.ornstein, type="pearson")^2)/
         df.residual(mod.ornstein))
summary(mod.ornstein, dispersion=phihat)
Anova(mod.ornstein, test="F")

    ###*** START EVAL=FALSE ***###
mod.ornstein.q <- update(mod.ornstein, family=quasipoisson)
    ###*** END EVAL=FALSE ***###

mod.ornstein.nb <- update(mod.ornstein, family=negative.binomial(1.5))
thetas <- seq(0.5, 2.5, by=0.5)
aics <- rep(0, 5) # allocate vector
for (i in seq(along=thetas)) aics[i] <- AIC(update(mod.ornstein.nb,
         family=negative.binomial(thetas[i])))
rbind(thetas, aics)
summary(mod.ornstein.nb)
summary(glm.nb(interlocks ~ log2(assets) + nation + sector,
     data=Ornstein))

    ###*** START EVAL=FALSE ***###
args(glm)
    ###*** END EVAL=FALSE ***###


    ###*** START Sinput ***###
args(glm)
    ###*** END Sinput ***###

