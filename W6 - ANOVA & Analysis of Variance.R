library(tidyverse)
library(car)
library(psych)

# First Factor: Dosage (3 levels: 0.5, 1, 2 mg)
# Second Factor: Method of delivery (2 levels: Orange Juice OJ; Ascorbic Acid VC)

mydata <- ToothGrowth
head(mydata)

# Shows different levels of conditions within each factor
levels(mydata$supp)
levels(mydata$dose)
mydata$dose <- as.factor(mydata$dose)
levels(mydata$dose) <- c("low", "mid", "high")

# Box plot
boxplot(len ~ supp,
        data = mydata,
        ylab = "Length of Growth",
        xlab = "Delivery Method")

# T-Test = just comparing two things.
t.test(len ~ supp, data = mydata, paired = FALSE)
# Non significant effect... p-value = not enough evidence to say 
  # whether there is an effect or no effect.

boxplot(len ~ dose,
        data = mydata,
        ylab = "Length of Growth",
        xlab = "Delivery Method")

#ANOVA
  #Dependent variable (length) ~ 3 conditions of dosage
oneway <- aov(len ~ dose, data = mydata)
summary(oneway)
# F-statistic high - over 1 = somewhere there's an effect.
# P-value = very small = v significant. 

# ANOVA = special case of the normal linear model (lm).
oneway_lm <- lm(len ~ dose, data = mydata)
summary(oneway_lm)

hist(residuals(oneway_lm), bins = 25, main = "Histogram of Residuals", xlab = "Residuals")
plot(oneway_lm, which = 2)

leveneTest(mydata$len, mydata$dose)
#don't want this to be significant 

#significant effect in aNOVA tells you a difference, 
  # not where the difference is.

#Makes p-values bigger; if still significant, there's a significant difference.
paired_tests <- pairwise.t.test(mydata$len,
                                mydata$dose,
                                p.adj = "bonferroni")

# Factorial ANOVA: Interaction Plot
attach(mydata)
interaction.plot(x.factor = dose,
                 trace.factor = supp,
                 response = len,
                 fun = mean,
                 main = "Interaction Plot",
                 xlab = "Dose",
                 ylab = "Mean Tooth Growth")

# Factorial ANOVA:
twoway <- aov(len ~ supp * dose, data = mydata)
summary(twoway)
  # Main effect of supp type = significant = difference between acid and orange juice.
  # Main effect of dose = significant = difference between dosage.
  # Interaction = significant. 
