library(datarium)
library(emmeans)
library(ez)
library(moments)
library(psyntur)
library(tidyverse)
library(apaTables)

#Set working directory - remember to set it to where your data is!
setwd("~/Desktop")

#You must download the AlcoholDataSet.csv dataset and have that in your working directory - else this will not work. 
df <- read.csv("AlcoholDataSet.csv", header = TRUE,quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, fileEncoding = 'UTF-8-BOM')

options(contrasts = c("contr.sum", "contr.poly")) # Set global options for orthogonal contrasts or I think they won't run for between and mixed models

df <- as_tibble(df)

#Make sure your factors are actually factors.
df$alcohol <- as.factor(df$alcohol)
df$id <- as.factor(df$id)
df$bar_snacks <- as.factor(df$bar_snacks)

#A nice boxplot for your data split up by conditions. 
tukeyboxplot(data = df,
             y = attractiveness,
             x = alcohol,by = bar_snacks, 
             jitter = TRUE)


#Normally distributed data for each condition?

psyntur::describe(df,
                  by = c(alcohol, bar_snacks),
                  skew = skewness(attractiveness),
                  kurt = kurtosis(attractiveness)
)


# Repeated Measures ANOVA with Mauchly's test for a WITHIN Subject Factor - for Homogeneity of Variance

within_model <- ezANOVA(data = df,
                         dv = attractiveness,
                         wid = id,
                         within = c(alcohol, bar_snacks),
                         type = 3,
                         detailed = TRUE,
                         return_aov = TRUE,
)
within_model

# Lets print out the output into a nice APA formatted table - this prints it in the command console below, and generates a word document with the APA table. 
#Note, because Mauchly's is violated in this example, it automatically does the Greenhouse Geisser Correction for you. 

within_table <- apa.ezANOVA.table(within_model,
                                   filename="within_ez_table.doc")

print(within_table)


#Prep for plotting data - calculating means and standard error (sem) that are needed or plotting the data
df2sem <- df %>% 
  group_by(alcohol, bar_snacks) %>% 
  summarise(mean = mean(attractiveness),
            sem = (sd(attractiveness)/sqrt(length(attractiveness))))

#Plotting the data - this is not your average interaction plt - this includes individual data points
df2sem %>% 
  ggplot() +
  aes(x = alcohol, y = mean, color = bar_snacks) +
  geom_line(aes(group = bar_snacks)) +
  geom_point() +
  geom_linerange(aes(x = alcohol, ymin = mean - sem, ymax = mean + sem), size = .5) +
  geom_point(data = df,aes(x = alcohol, y = attractiveness, colour = bar_snacks),position = position_jitter(width = .1), size = 1, shape = 20)+
  labs(title = "Effect of Alcohol and Scott Eggs on Attraction",
       subtitle = "Data from some silly study Darren made up") 


# Post-Hoc t-tests 

#For the within Samples...
pairs(emmeans(within_model$aov, specs = c("alcohol", "bar_snacks")), adjust = "bonf")


# Indepndent Samples ANOVA with Levene's test for a BETWEEN Subject Factor - for Homogeneity of Variance

#Lets make the dataset a between subjects design by including a new id column that is from 1>60.

df$id_between <- seq.int(nrow(df)) # Add a new id column to the data

between_model <- ezANOVA(data = df,
                         dv = attractiveness,
                         wid = id_between,
                         between = c(alcohol, bar_snacks),
                         type = 3,
                         detailed = TRUE,
                         return_aov = TRUE,
)
between_model

between_table <- apa.ezANOVA.table(between_model,
                                   filename="between_ez_table.doc")

print(between_table)

# Post Hoc Tests
# for the between model
pairs(emmeans(between_model$aov, specs = c("alcohol", "bar_snacks")), adjust = "bonf")



# MIXED ANOVA with Levene's test for a BETWEEN Subject Factor and with Mauchly's test for a WITHIN Subject Factor - for Homogeneity of Variance

# We have to use a different data set here to make things easier to interpret
dfmixed <- pivot_longer(anxiety, cols = -c(id, group), names_to = 'time', values_to = 'score')

mixed_model <- ezANOVA(data = dfmixed,
                       dv = score,
                       wid = id,
                       between = c(group),
                       within = c(time),
                       type = 3,
                       detailed = TRUE,
                       return_aov = TRUE
)
mixed_model



mixed_table <- apa.ezANOVA.table(mixed_model,
                                 filename="mixed_ez_table.doc")

print(mixed_table)

# Post Hoc Tests
# for the Mixed model
pairs(emmeans(mixed_model$aov, specs = c("alcohol", "bar_snacks")), adjust = "bonf")


