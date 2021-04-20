# Load Data: 
df <- read_csv("https://www.dropbox.com/s/8elzpgdou9qvise/visits.csv?dl=1")
library(tidyverse)
library(modelr)
library(psyntur)

# Background:Is there a relationship between health anxiety + number of visits to the doctor?
# Simulated data of 50 people attending appointments in the last 6 months. 
# Health anxiety measured from multiple q = mean score, 0-100.
# H0 = no relationship between health anciety + doctor visits in last 6 months.
# H1: the higher the health anxiety, the more visits to the doctor an individual will make.

# Histogram:
histogram(data = df, x = visits, bins = 8)

# Boxplot: shows support for H1
tukeyboxplot(data = df, x = visits, y = anx)

# GLM = Generalised Linear Model(outcome ~ predictor, data = , family = poisson)
m1 <- glm(visits ~ anx, data = df, family = poisson)
summary(m1)
# Coefficient Table: for every one change in anxiety, how much does the number of visits change? (estimates)
# Intercept + estimate -> 0 anxiety = o.667 visits.
# +ive anxiety estimate = for ecery one increase in anxiety = increase in the amount of doctor visits.
# anxiety appears to be significant predictor of visits. 
# Standard error = measure of how wrong the model is in the units of the predictor.
# Z Value = the staistic used to make sense of the probability (how likely this is)

# To get rid of the logarithm, use the below function.
exp(0.75352)
