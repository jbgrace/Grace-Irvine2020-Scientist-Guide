##### APPENDIX_S2_Rcode - Code that produces results shown in Figure S1 in APPENDIX_S2.R
# Note: The illustration in Figure S1 of Appendix_S2 is accomplished here using standardized coefficients

# Load required libraries
library(lavaan)

# Read data
fdat <- read.csv("AppendixS1_data.csv")
names(fdat)

##### Set of models in Figure S1 of Appendix_S2
### Model A
# Specify model
modA <- 'vegcover ~ coastdist'

# Estimate model parameters
modA.fit <- sem(modA, data=fdat)

# Extract standardized coefficients
subset(parameterEstimates(modA.fit, standardized=T), op=="~", c("lhs", "op", "rhs", "std.all")) 


### Model B 
modB <- 'vegcover ~ coastdist + elev
         elev ~ coastdist'
modB.fit <- sem(modB, data=fdat)
subset(parameterEstimates(modB.fit, standardized=T), op=="~", c("lhs", "op", "rhs", "std.all")) 


### Model C 
modC <- 'vegcover ~ coastdist + elev + age
         elev ~ coastdist
         age ~ coastdist + elev'
modC.fit <- sem(modC, data=fdat)
subset(parameterEstimates(modC.fit, standardized=T), op=="~", c("lhs", "op", "rhs", "std.all")) 


### Model D 
modD <- 'vegcover ~ coastdist + elev + age + firesev
         elev ~ coastdist
         age ~ coastdist + elev
         firesev ~ coastdist + elev + age'
modD.fit <- sem(modD, data=fdat)
subset(parameterEstimates(modD.fit, standardized=T), op=="~", c("lhs", "op", "rhs", "std.all")) 

