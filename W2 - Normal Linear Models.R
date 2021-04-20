# View Data Set
ansur

# View dataset as a histogram - can see that weight is roughly normally distributed with a slight skew)
histogram(x = weight, data = ansur, bins = 25)

# View distribution of weight for each tercile of height in histogram
# Terciles = grouping of those of low, medium and tall heights.
histogram(x = weight, bins=25, data = ansur, facet = height_tercile)
# see that for each height group, weight is normally disributed but mean increases with heigh group. 
# SD is roughly constasnt.

# View the descriptives of these distributions:
describe(ansur, by = height_tercile,
         avg_height = mean(height),
         avg_weight = mean(weight),
         sd_weight = sd(weight))

# Normal Linear Model (outcome ~ predictor)
model_1 <- lm(weight ~ height, data = ansur)
# Coefficients - means that assuming the assumptions of the model are valid,
# as height increases by 1 unit (1cm), weight increases by 1.148148 units (kg).
coef(model_1)

# SD - means the model of weight is ND with SD of 11.76 for any given height.
# As the value of height increases/decreases, the av of the distribution of weight
# increaseas/decreases by a proportional amount = 1.148139.
sigma(model_1)

# View distribution of weight for each age and heigh group combined
histogram(x = weight, bins=25, data = ansur, facet = c(
  height_tercile, age_tercile), facet_type = 'grid')
# Shows that ~ foreach age and height group, the distribution of weight is roughly normal.
# For any given age group, as height increases, the av weight distribution increases.
# For any given height group, as age increases, the av of the weight distribution increases.

# Multiple linear regression: Modelling weight as ND for any given height and age.
# As age increases, assuming height is constsant/increases = proportional increase in av weight.
model_2 <- lm(weight ~ height + age, data = ansur)
coef(model_2)
sigma(model_2)
# Means that for any given age/height, weight = ND with SD of 11.34.
# Age constant at any given value, as height increases by one unit, weight distribution increases by 1.13.
# Heright constant, as age increases by one unit, av weight distribution increases by 0.36.