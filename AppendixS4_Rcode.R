### APPENDIX_S4 - WHALEN SEAGRASS FOODWEB EXPERIMENTAL RESULTS

dat <- read.csv("AppendixS4_data.csv")
names(dat)
attach(dat)

##############################
# CONVENTIONAL ANALYSIS
##############################

##### MODELS IN MANUSCRIPT
### Model A Simple ANOVA
modA <- aov(Epiphytes ~ Treatment)
drop1(modA, ~., test="F")

### Model B Simple MANOVA
Ygc <- cbind(Gammarids,Caprellids)
modB <- manova(Ygc ~ Treatment)
summary.manova(modB)

### Model C Full MANOVA
Yegc <- cbind(Epiphytes,Gammarids,Caprellids)
modC <- manova(Yegc ~ Treatment)
summary.manova(modC)

### Model D MANCOVA
modD <-manova(Ygc ~ Treatment + Epiphytes)
summary.manova(modD)

### Model E ANCOVA #1
modE <- aov(Epiphytes ~ Treatment + Macroalgae + Seagrassdens)
drop1(modE, ~., test="F")

### Model F ANCOVA #2
modF <- aov(Epiphytes ~ Treatment + Gammarids + Caprellids)
drop1(modF, ~., test="F")

### Model G ANCOVA #3
modG <- aov(Epiphytes ~ Treatment + Gammarids + Caprellids + Macroalgae + Seagrassdens)
drop1(modG, ~., test="F")


##### MODELS IN MANUSCRIPT RUN AS REGRESSIONS
# Load library "lm.beta" - has function "lm.beta" to standardize coefs
library(lm.beta) 

### Model A Simple ANOVA
modA.lm <- lm(Epiphytes ~ Treatment)
summary(lm.beta(modA.lm))

### Model B Simple MANOVA
Ygc <- cbind(Gammarids,Caprellids)
modB.lm <- lm(Ygc ~ Treatment)
summary(lm.beta(modB.lm))

### Model C Full MANOVA
Yegc <- cbind(Epiphytes,Gammarids,Caprellids)
modC.lm <- lm(Yegc ~ Treatment)
summary(lm.beta(modC.lm))

### Model D MANCOVA
modD.lm <-lm(Ygc ~ Treatment + Epiphytes)
summary(lm.beta(modD.lm))

### Model E ANCOVA #1
modE.lm <- lm(Epiphytes ~ Treatment + Macroalgae + Seagrassdens)
summary(lm.beta(modE.lm)) # request standardized coeffients

### Model F ANCOVA #2
modF.lm <- lm(Epiphytes ~ Treatment + Gammarids + Caprellids)
summary(lm.beta(modF.lm))

### Model G ANCOVA #3
modG.lm <- lm(Epiphytes ~ Treatment + Gammarids + Caprellids + Macroalgae + Seagrassdens)
summary(lm.beta(modG.lm))


################################
# STRUCTURAL EQUATION MODELING #
################################
library(piecewiseSEM)

### Model A - a priori model
pw.modA <- psem(lm(Gammarids ~ Treatment, data=dat), 
                lm(Caprellids ~ Treatment, data=dat),
                lm(Epiphytes ~ Gammarids + Caprellids + Macroalgae + Seagrassdens, data=dat))
dSep(pw.modA)


### Model B.1 - add links 'Gammarids  ~ Macroalgae' and 'Caprellids ~ Macroalgae based on modification indices
pw.modB.1 <- psem(lm(Gammarids ~ Treatment + Macroalgae, data=dat), 
                  lm(Caprellids ~ Treatment + Macroalgae, data=dat),
                  lm(Epiphytes ~ Gammarids + Caprellids + Macroalgae + Seagrassdens, data=dat))
dSep(pw.modB.1)


### Model B.final - final model, with 'Gammarids ~ Seagrassdens' added
pw.modB.final <- psem(lm(Gammarids ~ Treatment + Macroalgae + Seagrassdens, data=dat), 
                      lm(Caprellids ~ Treatment + Macroalgae, data=dat),
                      lm(Epiphytes ~ Gammarids + Caprellids + Macroalgae + Seagrassdens, data=dat))
dSep(pw.modB.final)
summary(pw.modB.final)

# request range-standardized coefficients
coefs(pw.modB.final, standardize = "range")

# request r-squares
rsquared(pw.modB.final)


####################################################################
##### CONDITIONING ON A COLLIDER SIMULATION STUDIES - BONUS MATERIAL
##### Three-variable model A-> M <- B
####################################################################
### Simulate the Data
n = 10000
set.seed(1)

### create independent exogenous predictors
A <- rnorm(n, 0, 0.25)
B <- rnorm(n, 0, 0.30)

# coefficients for model
bexd    =  0.60
bexp    = -0.50
bcmp    = -0.55
bend    = -0.35
benc    = -0.45

# collider is M variable
M <- 0.60*A + 0.30*B + rnorm(n, 0, 0.20)

dat <- data.frame(A, B, M)
print(cor(dat), digits=4) # note A and B effectively uncorrelated


##### Regression Models
### Model 1: M ~ A + B
summary(lm(M ~ A + B))

### Model 2: A ~ M + B - Illustrates that conditioning A on M induces (non-causal) association between A and B
summary(lm(A ~ M + B))

##### Two-Stage Demonstration of the Conditioning
## Stage 1 conditioning of A and B on M - conditioned variables are A.res1 and B.res1
A.res1 <- resid(lm(A ~ M))
B.res1 <- resid(lm(B ~ M))

## Residual Correlation Illustrates conditioning A and/or B on M induces correlation between A.res and B.res
cor.test(A.res1, B.res1)

## this extends to induced correlations between A.res and B, and A and B.res
cor.test(A.res1, B)
cor.test(A, B.res1)

## as shown above, A and B not correlated until one of them is conditioned on M
cor.test(A, B)
