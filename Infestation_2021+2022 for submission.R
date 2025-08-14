# R Version 4.2.1
# Figure 3

library(tidyverse)
library(ggplot2)
library(ggpubr)


# Infested_rate ~ Adult_Trap #################
# Read in the CSV file with summarized infestation data
abiotic <- read.csv("Abiotic factor 2021+2022_Summary.csv", na.strings = c("","NA"), header=TRUE)
str(abiotic)
# Perform a Pearson correlation
cor.test(abiotic$Infested_rate, abiotic$Adult_Trap, method = "pearson")

# Fit a linear regression model
model_i <- lm(data = abiotic, Infested_rate  ~ Adult_Trap)
# Run an ANOVA test on the fitted linear model
anova(model_i)
summary(model_i)

# Perform test for homogeneity of variances
fligner.test(data = abiotic, Infested_rate  ~ Adult_Trap)
# Perform test for normality of residuals
shapiro.test(resid(model_i))

# Result
# There is strong statistical evidence that Adult_Trap has a significant effect on Infested_rate.
# (F = 57.03; df = 1,23; p<0.001)

# The data do not show heterogeneity of variance across Adult_Trap groups, so the assumption of homoscedasticity (equal variance) is supported.
# Fail to reject the null hypothesis that residuals are normally distributed. In other words, there’s no strong statistical evidence against normality.

# Plot: Figure 2


# Egg scars by varieties ################
# read the data into R
infestation <- read.csv("Infestation 2021+2022.csv", na.strings = c("","NA"), header=TRUE)
str(infestation)

# makes Variety a categorical factor
infestation$Variety <-as.factor(infestation$Variety) 
infestation$Bush <-as.factor(infestation$Bush) 

# check again      
str(infestation)

# Year combined
# define the most basic general linear model 
model <- aov(data = infestation, Average_scars ~ Variety)
summary(model)
# Significant effect of blueberry variety on blueberry infestation (F = 5.32; df = 2,397; P = 0.00525)
TukeyHSD(model)
# Elliott had significantly more egg scars than Bluecrop but not Blueray

# mean and se
bc <- infestation %>%
  filter(Variety == "C")
mean(bc$Average_scars) # 0.77
sd(bc$Average_scars)/sqrt(length((bc$Average_scars))) # 0.16

br <- infestation %>%
  filter(Variety == "R")
mean(br$Average_scars) # 1.41
sd(br$Average_scars)/sqrt(length((br$Average_scars))) # 0.26

e <- infestation %>%
  filter(Variety == "E")
mean(e$Average_scars) # 1.79
sd(e$Average_scars)/sqrt(length((e$Average_scars))) # 0.24


# Year 2021
y2021 <- infestation %>%
  filter(Year == "2021")

model2021 <- aov(data = y2021, Average_scars ~ Variety)
summary(model2021)
# Significant effect of blueberry variety on blueberry infestation (F = 3.676; df = 2,97; P = 0.0289)
TukeyHSD(model2021)
# No significant difference between varieties


bc2021 <- y2021 %>%
  filter(Variety == "C")
mean(bc2021$Average_scars) # 2.83
sd(bc2021$Average_scars)/sqrt(length((bc2021$Average_scars))) # 0.49

br2021 <- y2021 %>%
  filter(Variety == "R")
mean(br2021$Average_scars) # 5.35
sd(br2021$Average_scars)/sqrt(length((br2021$Average_scars))) # 0.68

e2021 <- y2021 %>%
  filter(Variety == "E")
mean(e2021$Average_scars) # 4.30
sd(e2021$Average_scars)/sqrt(length((e2021$Average_scars))) # 0.65


# Year 2022
y2022 <- infestation %>%
  filter(Year == "2022")

model2022 <- aov(data = y2022, Average_scars ~ Variety)
summary(model2022)
# Significant effect of blueberry variety on blueberry infestation (F = 13.54; df = 2,297; P < 0.001)
TukeyHSD(model2022)
# Elliott had significantly more egg scars than Bluecrop and Blueray

bc2022 <- y2022 %>%
  filter(Variety == "C")
mean(bc2022$Average_scars) # 0.16
sd(bc2022$Average_scars)/sqrt(length((bc2022$Average_scars))) # 0.06

br2022 <- y2022 %>%
  filter(Variety == "R")
mean(br2022$Average_scars) # 0.23
sd(br2022$Average_scars)/sqrt(length((br2022$Average_scars))) # 0.09

e2022 <- y2022 %>%
  filter(Variety == "E")
mean(e2022$Average_scars) # 0.79
sd(e2022$Average_scars)/sqrt(length((e2022$Average_scars))) # 0.12

# Plot: Figure 3
