# R Version 4.2.1
# Question: Influence of berry size, firmness, and sugar content on SWD blueberry infestation
# Table 5

library(tidyverse)
library(ggplot2)

#read the data into R
blueberry <- read.csv("Blueberry data 2022 for Analysis.csv", na.strings = c("","NA"), header=TRUE)
str(blueberry)

# makes Variety a categorical factor
# A total of three varieties in the study
blueberry$Variety <-as.factor(blueberry$Variety) 

# check again      
str(blueberry)

# define the most basic general linear model 
# GLM: 
# Total egg scars ~ weight + firmness + brix + Variety
# Total egg scars are count data with a lot of zeros
mod1 <- aov(data = blueberry, Average_scars ~ Average_weight_g + Average_penetration_mN + Average_Brix + Variety)
summary(mod1)
# Considering the biological sense, decided to separate variety and to consider the influence of blueberry 
# characteristics within variety


# separate variety
bc <- blueberry %>%
  filter(Variety == "C")

br <- blueberry %>%
  filter(Variety == "R")

e <- blueberry %>%
  filter(Variety == "E")

# Bluecrop
mean(bc$Average_weight_g) # 1.291433
# standard error
sqrt(var(bc$Average_weight_g) / length(bc$Average_weight_g)) # 0.03444641

mean(bc$Average_penetration_mN) # 169.4553
sqrt(var(bc$Average_penetration_mN) / length(bc$Average_penetration_mN)) # 4.696082

mean(bc$Average_Brix) # 14.2854
sqrt(var(bc$Average_Brix) / length(bc$Average_Brix)) # 0.1854849

# Blueray
mean(br$Average_weight_g) # 1.412133
sqrt(var(br$Average_weight_g) / length(br$Average_weight_g)) # 0.0411153

mean(br$Average_penetration_mN) # 161.5871
sqrt(var(br$Average_penetration_mN) / length(br$Average_penetration_mN)) # 3.807206

mean(br$Average_Brix) # 15.506
sqrt(var(br$Average_Brix) / length(br$Average_Brix)) # 0.2466646

# Elliott
mean(e$Average_weight_g) # 0.9063917
sqrt(var(e$Average_weight_g) / length(e$Average_weight_g)) # 0.03108724

mean(e$Average_penetration_mN) # 169.1112
sqrt(var(e$Average_penetration_mN) / length(e$Average_penetration_mN)) # 3.440073

mean(e$Average_Brix) # 13.53133
sqrt(var(e$Average_Brix) / length(e$Average_Brix)) # 0.2020917

# "Because the number of egg scars, berry weight, penetration force, and brix did not fulfil 
# the normality of residuals nor homogeneity of variance, and the mean and variance of the 
# distribution of number of egg scars exhibited overdispersion, generalized linear models (GLMs) 
# were used to determine how much variation in SWD infestation of each blueberry variety might 
# be explained by each of the blueberry qualities described above (berry weight, firmness, and sugar 
# content)."

# Negative binomial ################
library(MASS)

# Bluecrop
mean(bc$Total_scars) # 0.47
# standard error
sqrt(var(bc$Total_scars) / length(bc$Total_scars)) # 0.1904036

# Negative binomial
modelbc <- glm.nb(Total_scars ~ Average_weight_g + Average_penetration_mN + Average_Brix, data = bc)
summary(modelbc)

exp(modelbc$coefficients)

# Blueray
mean(br$Total_scars) # 0.69
# standard error
sqrt(var(br$Total_scars) / length(br$Total_scars)) # 0.2841592

# Negative binomial
modelbr <- glm.nb(Total_scars ~ Average_weight_g + Average_penetration_mN + Average_Brix, data = br)
summary(modelbr)

exp(modelbr$coefficients)

# Elliott
mean(e$Total_scars) # 2.38
# standard error
sqrt(var(e$Total_scars) / length(e$Total_scars)) # 0.3515564

# Negative binomial
var(e$Total_scars)/mean(e$Total_scars)

modele <- glm.nb(Total_scars ~ Average_weight_g + Average_penetration_mN + Average_Brix, data = e)
summary(modele)

exp(modele$coefficients)

# I found that berry size, measured as weight, and sugar content, measured as brix, had no 
# influence on infestation. However, penetration force had a significant negative effect on the number 
# of SWD egg scars observed in the three blueberry varieties: as the penetration force 
# required to pierce berries decreased (i.e., berries became softer) the number of SWD egg scars in 
# berries increased, regardless of variety. 

