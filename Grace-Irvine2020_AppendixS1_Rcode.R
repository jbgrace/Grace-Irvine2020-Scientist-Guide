##### Appendix S1 - All subsets regression

fire.dat <- read.csv("AppendixS1_data.csv")
names(fire.dat)


######################################################
##### Multimodel Comparisons For Wildfire Study ######
######################################################
# library for model comparisons 
library(AICcmodavg)

### All Subsets Regression
## predictor set {firesev, age, elev, coastdist}
# null model
m1 <- lm(vegcover ~ 1, data=fire.dat); summary(m1)

## one-predictor models
m2 <- lm(vegcover ~ firesev, data=fire.dat); summary(m2)
m3 <- lm(vegcover ~ age, data=fire.dat); summary(m3)
m4 <- lm(vegcover ~ elev, data=fire.dat); summary(m4)
m5 <- lm(vegcover ~ coastdist, data=fire.dat); summary(m5)

## two-predictor models
m6 <- lm(vegcover ~ firesev + age, data=fire.dat); summary(m6)
m7 <- lm(vegcover ~ firesev + elev, data=fire.dat); summary(m7)
m8 <- lm(vegcover ~ firesev + coastdist, data=fire.dat); summary(m8)
#
m9 <- lm(vegcover ~ age + elev, data=fire.dat); summary(m9)
m10 <- lm(vegcover ~ age + coastdist, data=fire.dat); summary(m10)
#
m11 <- lm(vegcover ~ elev + coastdist, data=fire.dat); summary(m11)

## three-predictor models
m12 <- lm(vegcover ~ firesev + age + elev, data=fire.dat); summary(m12)
m13 <- lm(vegcover ~ firesev + age + coastdist, data=fire.dat); summary(m13)
m14 <- lm(vegcover ~ firesev + elev + coastdist, data=fire.dat); summary(m14)
m15 <- lm(vegcover ~ age + elev + coastdist, data=fire.dat); summary(m15)

## four-predictor model
m16 <- lm(vegcover ~ firesev + age + elev + coastdist, data=fire.dat); summary(m16)


aictab(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16), 
       c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16"))
