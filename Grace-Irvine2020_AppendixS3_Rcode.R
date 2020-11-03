##### APPENDIX_S3_RCODE - WILDFIRE SEM

##############################################################################
##### SEM Analyses using 'piecewiseSEM': Local Estimation and Evaluation #####
##############################################################################
library(piecewiseSEM)

fdat <- read.csv("Grace-Irvine2020_AppendixS1_data.csv")

# model 1
pw.mod1 <- psem(lm(vegcover ~ firesev + age + elev, data=fdat), 
                lm(elev ~ coastdist, data=fdat), 
                lm(age ~ coastdist + elev, data=fdat), 
                lm(firesev ~ age + elev, data=fdat))
summary(pw.mod1)

# model 2 - alternative to model 1 with link from E -> F omitted
pw.mod2 <- psem(lm(vegcover ~ firesev + age + elev, data=fdat), 
                lm(elev ~ coastdist, data=fdat), 
                lm(age ~ coastdist + elev, data=fdat), 
                lm(firesev ~ age, data=fdat))
summary(pw.mod2)

# model 3 - alternative to model 2 with link from C -> F added
pw.mod3 <- psem(lm(vegcover ~ firesev + age + elev, data=fdat), 
                lm(elev ~ coastdist, data=fdat), 
                lm(age ~ coastdist + elev, data=fdat), 
                lm(firesev ~ age + coastdist, data=fdat))
summary(pw.mod3)

# model 4 - alternative to model 1 with additional link from C -> V
pw.mod4 <- psem(lm(vegcover ~ firesev + age + elev + coastdist, data=fdat), 
                lm(elev ~ coastdist, data=fdat), 
                lm(age ~ coastdist + elev, data=fdat), 
                lm(firesev ~ age + elev, data=fdat))
summary(pw.mod4)

# model 5 - simplification of model 1, omitting A -> V
pw.mod5 <- psem(lm(vegcover ~ firesev + elev, data=fdat), 
                lm(elev ~ coastdist, data=fdat), 
                lm(age ~ coastdist + elev, data=fdat), 
                lm(firesev ~ age + elev, data=fdat))
summary(pw.mod5)

# model 6 - simplification of model 1, omitting E -> V
pw.mod6 <- psem(lm(vegcover ~ firesev + age, data=fdat), 
                lm(elev ~ coastdist, data=fdat), 
                lm(age ~ coastdist + elev, data=fdat), 
                lm(firesev ~ age + elev, data=fdat))
summary(pw.mod6)


### Model comparison
AIC(pw.mod1, aicc=T)
AIC(pw.mod2, aicc=T)
AIC(pw.mod3, aicc=T)
AIC(pw.mod4, aicc=T)
AIC(pw.mod5, aicc=T)
AIC(pw.mod6, aicc=T)

# model with minimum AICc is model 1
summary(pw.mod1)
coefs(pw.mod1)
rsquared(pw.mod1)

### Extracting d-separation claims
dSep(pw.mod1)
dSep(pw.mod2)
dSep(pw.mod3)
dSep(pw.mod4)
dSep(pw.mod5)
dSep(pw.mod6)

