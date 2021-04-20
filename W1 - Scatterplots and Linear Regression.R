#Doing scatterplots and linear regression

#1 Run psyntur
library(psyntur)

# Make a scatterplot of the dataset 'faithfulness' against trustworthiness.
# type of plot (x = variable, y = variable, data = dataset used)
scatterplot(x = trustworthy, y = faithful, data = faithfulfaces)

#Add a line of best fit (use command below (TRUE or FALSE))
scatterplot(x = trustworthy, y = faithful, data = faithfulfaces,
            best_fit_line = TRUE)

# Do simple LINEAR REGRESSION:
# with 'faithful' as outcome and 'trustworthy' as predictor
# name the dataset <- lm(outcome variable ~ predictor variable, data = dataset used)
model <- lm(faithful ~ trustworthy, data = faithfulfaces)

# View lm results
summary(model)

# Interpreting results
# (intercept)-Estimate = intercept term
# predictor-Estimate = slope term of the line of best fit
# ^ slope term is positive =  +ive relationship. 
# Pr(>|t|)= p-values
# Multiple R-squared
# Adjusted R-squared = certain percent of the variabilityt in the outcome variable, is explained by the predictor variable.

# View in non scientific notation:
format(1.46e-11, scientific = FALSE)
