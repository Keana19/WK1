# Multiple linear regression predicting weight from height and age.
model_3 <- lm(weight ~ height + age, data = ansur)

# View summary of model
summary(model_3)
coef(model_3)

# Calculating confidence intervals for model
confint(model_3)

# Predict the av of the distibution of weight for a 'hypothetical person' whose
# height = 165cm + age = 35.
hypothetical_person <- data.frame(height = 165, age = 35)
hypothetical_person
predict(model_3, newdata = hypothetical_person)

# 95% prediction interval for predicted mean of this person.
predict(model_3, newdata = hypothetical_person, interval = 'confidence')
