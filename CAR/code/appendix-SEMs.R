##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##   Script for Appendix on Structural Equatioin Models  ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##

library(sem)
Klein

Klein$P.lag <- with(Klein, c(NA, P[-length(P)]))
Klein$X.lag <- with(Klein, c(NA, X[-length(X)]))
Klein$A <- Klein$Year - 1931
head(Klein)

# Klein's econometric model fit by 2SLS

eqn.1 <- tsls(C ~ P + P.lag + I(Wp + Wg),
    instruments= ~ G + T + Wg + A + P.lag + K.lag + X.lag, data=Klein)
summary(eqn.1)
eqn.2 <- tsls(I ~ P + P.lag + K.lag,
    instruments= ~ G + T + Wg + A + P.lag + K.lag + X.lag, data=Klein)
summary(eqn.2)
eqn.3 <- tsls(Wp ~ X + X.lag + A,
    instruments= ~ G + T + Wg + A + P.lag + K.lag + X.lag, data=Klein)
summary(eqn.3)


# Duncan, Haller, and Portes's peer influences model

R.dhp <- readMoments(diag=FALSE, names=c("ROccAsp", "REdAsp", "FOccAsp",
                "FEdAsp", "RParAsp", "RIQ", "RSES", "FSES", "FIQ", "FParAsp"))
    .6247     
    .3269  .3669       
    .4216  .3275  .6404
    .2137  .2742  .1124  .0839
    .4105  .4043  .2903  .2598  .1839
    .3240  .4047  .3054  .2786  .0489  .2220
    .2930  .2407  .4105  .3607  .0186  .1861  .2707
    .2995  .2863  .5191  .5007  .0782  .3355  .2302  .2950
    .0760  .0702  .2784  .1988  .1147  .1021  .0931 -.0438  .2087

model.dhp <- specifyModel()
RParAsp  -> RGenAsp, gam11
RIQ      -> RGenAsp, gam12
RSES     -> RGenAsp, gam13
FSES     -> RGenAsp, gam14
RSES     -> FGenAsp, gam23
FSES     -> FGenAsp, gam24
FIQ      -> FGenAsp, gam25
FParAsp  -> FGenAsp, gam26
FGenAsp  -> RGenAsp, beta12
RGenAsp  -> FGenAsp, beta21
RGenAsp  -> ROccAsp,  NA,     1
RGenAsp  -> REdAsp,  lam21
FGenAsp  -> FOccAsp,  NA,     1
FGenAsp  -> FEdAsp,  lam42
RGenAsp <-> FGenAsp, ps12

R.dhp
model.dhp

sem.dhp <- sem(model.dhp, R.dhp, N=329,
    fixed.x=c("RParAsp", "RIQ", "RSES", "FSES", "FIQ", "FParAsp"))
sem.dhp
summary(sem.dhp)

model.dhp.2 <- specifyEquations()
REdAsp = lamy*RGenAsp
ROccAsp = 1*RGenAsp
FEdAsp = lamy*FGenAsp
FOccAsp = 1*FGenAsp
RGenAsp = gam1*RParAsp + gam2*RIQ + gam3*RSES + gam4*FSES + beta*FGenAsp
FGenAsp = gam1*FParAsp + gam2*FIQ + gam3*FSES + gam4*RSES + beta*RGenAsp
V(RGenAsp) = psi
V(FGenAsp) = psi
C(RGenAsp, FGenAsp) = psi12
V(ROccAsp) = theps1
V(REdAsp) = theps2
V(FOccAsp) = theps1
V(FEdAsp) = theps2

sem.dhp.2 <- sem(model.dhp.2, R.dhp, N=329,
    fixed.x=c("RParAsp", "RIQ", "RSES", "FSES", "FIQ", "FParAsp"))
summary(sem.dhp.2)
anova(sem.dhp, sem.dhp.2)

modIndices(sem.dhp.2)

model.dhp.3 <- update(model.dhp.2)
add, FOccAsp <-> ROccAsp, theps24

sem.dhp.3 <- sem(model.dhp.3, R.dhp, N=329,
    fixed.x=c("RParAsp", "RIQ", "RSES", "FSES", "FIQ", "FParAsp"))
summary(sem.dhp.3)

standardizedCoefficients(sem.dhp.3)


# Bollen's industrialization and democracy model

head(Bollen)

model.bollen <- specifyEquations()
y1 = 1*Demo60
y2 = lam2*Demo60
y3 = lam3*Demo60
y4 = lam4*Demo60
y5 = 1*Demo65
y6 = lam2*Demo65
y7 = lam3*Demo65
y8 = lam4*Demo65
x1 = 1*Indust
x2 = lam6*Indust
x3 = lam7*Indust
c(y1, y5) = theta15
c(y2, y4) = theta24
c(y2, y6) = theta26
c(y3, y7) = theta37
c(y4, y8) = theta48
c(y6, y8) = theta68
Demo60 = gamma11*Indust
Demo65 = gamma21*Indust + beta21*Demo60
v(Indust) = phi

sem.bollen <- sem(model.bollen, data=Bollen)
summary(sem.bollen)
summary(sem.bollen, robust=TRUE)

round(sqrt(diag(vcov(sem.bollen)))/sqrt(diag(vcov(sem.bollen, robust=TRUE))), 2)

# Canadian National Election Study CFA with ordinal data

head(CNES)

model.cnes <- cfa()
F: MBSA2, MBSA7, MBSA8, MBSA9

model.cnes

library(polycor)
hcor <- function(data) hetcor(data, std.err=FALSE)$correlations
(R.cnes <- hcor(CNES))

summary(sem.cnes <- sem(model.cnes, R.cnes, N=1529))

set.seed(12345) # for reproducibility
system.time(boot.cnes <- bootSem(sem.cnes, R=100, cov=hcor, data=CNES))    
(boot.summary <- summary(boot.cnes, type="norm"))    
round(sqrt(diag(vcov(sem.cnes)))/boot.summary$table[, "Std.Error"], 2)    

# Mental tests CFA with missing data

head(Tests)
nrow(Tests)

mod.cfa.tests <- cfa(raw=TRUE)
verbal: x1, x2, x3
math: y1, y2, y3

mod.cfa.tests

  # FIML estimator:
cfa.tests <- sem(mod.cfa.tests, data=Tests, na.action=na.pass, 
                 objective=objectiveFIML, fixed.x="Intercept")
summary(cfa.tests, saturated=TRUE)

  # complete-case analysis, ML estimator:
cfa.tests.cc <- sem(mod.cfa.tests, data=Tests, raw=TRUE, fixed.x="Intercept")
library(car)  # for compareCoefs()
compareCoefs(cfa.tests, cfa.tests.cc)

  # multiple imputation of missing data

imps <- miSem(mod.cfa.tests, data=Tests, fixed.x="Intercept", 
    raw=TRUE, seed=12345)
summary(imps)

# Holzinger-Swineford multi-group CFA

library(MBESS)
data(HS.data)
head(HS.data)

mod.hs <- cfa()
spatial: visual, cubes, paper, flags
verbal: general, paragrap, sentence, wordc, wordm
memory: wordr, numberr, figurer, object, numberf, figurew
math: deduct, numeric, problemr, series, arithmet

mod.mg <- multigroupModel(mod.hs, groups=c("Female", "Male")) 
class(mod.mg)

sem.mg <- sem(mod.mg, data=HS.data, group="Gender",
              formula = ~ visual + cubes + paper + flags +
              general + paragrap + sentence + wordc + wordm +
              wordr + numberr + figurer + object + numberf + figurew +
              deduct + numeric + problemr + series + arithmet
              )
summary(sem.mg)

mod.mg.eq <- multigroupModel(mod.hs, groups=c("Female", "Male"), allEqual=TRUE)
sem.mg.eq <- sem(mod.mg.eq, data=HS.data, group="Gender",
              formula = ~ visual + cubes + paper + flags +
                general + paragrap + sentence + wordc + wordm +
                wordr + numberr + figurer + object + numberf + figurew +
                deduct + numeric + problemr + series + arithmet
              )
sem.mg.eq

anova(sem.mg.eq, sem.mg)
BIC(sem.mg)
BIC(sem.mg.eq)

modIndices(sem.mg.eq)

