library(psyntur)
library(tidyverse)
ansur

# Run a multiple regression model
modelw4 <- lm(weight ~ height + age, data = ansur)
summary(modelw4)

# ASSUMPTIONS
# Look at histogram of residuals
  # Left leaning = +ive skew; right leaning = -ive skew
hist(residuals(modelw4), bins = 25)
# looks normally disributed (fairly symmetrical), but slight lean to left. 

# look standardised residuals on qq plot
  # another way of to see whether the data is normally disributed
plot(modelw4, which = 2)
# If the data was perfectly normally disributed, all the data points
  # would be directly along the dotted line.
# Data diverges towards the end, indicating a positive skew in the data
  # due to being on +ive side of quantiles. 
  # Due to deviation, the data is not normally disributed (violated)

# Can plot the fitted predicted values vs residuals
plot(modelw4, which = 1)
  # If valid, the spread of points would be symmetrical (homogenous spread across fitted values).
  # Homoegenous of residuals = violated due to lack of symmetry.

# Scale location plot
plot(modelw4, which = 3)
# Shows fitted values on x axis and squared residuals on Y.
# Red lign = regression line. 
# Data more tightly compact to left than right. 
# Red fitted line would be flat for homogeneous of variance. 

# MODEL EVALUATION: Want to look at how well the the variables predict the outcome.

summary(modelw4)

summary(modelw4)$r.sq
# 0 = rubbish predictors, 1 = perfect predictors. 
# 0.48 = reasonably good as almost 50%, it can reasonably predict someone's weight. 
# Adjusted r-squared = p
# Null hypothesis = no effect/relationship with outcome variable.
# F-statistics compares model to a null model <- significant = better than model with no predictors.
  summary(modelw4)$fstatistic
  # ^ Value = large, bigger than 3 and 6 = likely to be significant. 
  # p-value = very small + large f-statistic = very(dont say that though) SIGNIFICANT.

# Can predict a model that just has height as a predictor:
  height_model <- lm(weight ~ height, data = ansur)

# model with height and age is better than one with height alone. 
    anova(height_model, modelw4)
  