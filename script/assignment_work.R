---
title:  "Assignment 1" 
author: "Brianna Maloney"
date: "March 2021"
---
  
  #1.3
library(tidyverse)
dicot <- read.csv("~/Documents/data/BIOSCI-738/data/dicots.csv")

#null hypothesis: no difference in across Treatment levels
glimpse(dicot)
dicot_aov <- lm(Total_natives ~ Treatment, data = dicot)
dicot_aov
anova(dicot_aov)

#change 'Control' Treatment level to baseline
dicot_control <- relevel(dicot$Treatment, ref = "Control")

#refit anova model
dicot_control_aov <- aov(Total_natives ~ Treatment, data = dicot_control)
dicot_control_aov

  #1.4
#create a plot of the data highlighting any aspect you may want a reader to take away
ggplot(data=dicot, aes(Treatment, Total_natives)) +
  geom_boxplot()
#a basic boxplot can show us the mean and some idea of the data range

ggplot(data=dicot, aes(Treatment, Total_natives)) +
  geom_violin(scale="area")
#a violin plot can further show us the range of our data
