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
dicot$Treatment <- factor(dicot$Treatment, ordered = FALSE) %>%
  relevel(dicot$Treatment, ref = "Biocontrol")
dicot %>% group_by(Treatment) %>% summarise(avg = mean(Total_natives))
#fit model
dicot_aov <- lm(Total_natives ~ Treatment, data = dicot)
dicot_aov
anova(dicot_aov)

#make Treatment an unordered factor and change "Control" to baseline level
dicot$Treatment <- factor(dicot$Treatment, ordered = FALSE) %>%
  relevel(dicot$Treatment, ref = "Control")
dicot %>% group_by(Treatment) %>% summarize(avg = mean(Total_natives))
#refit anova model
dicot_control_lm <- lm(Total_natives ~ Treatment, data = dicot)
dicot_control_lm
anova(dicot_control_lm)


  #1.4 
#create a plot of the data highlighting any aspect you may want a reader to take away
ggplot(data=dicot, aes(Treatment, Total_natives)) +
  geom_boxplot()
#a basic boxplot can show us the mean and some idea of the data range

ggplot(data=dicot, aes(Treatment, Total_natives)) +
  geom_violin(scale="area") +
  labs(x= "Treatment", y="Total Native Plants", title= "Native Plants Across Multiple Treatments")
#a violin plot can further show us the full range of our data

#first we must transform Block variable from numeric to factorial
dicot$Block <- as.factor(dicot$Block)
ggplot(data=dicot, aes(x=Treatment, y=Total_natives, fill=Block)) +
  geom_dotplot(binaxis="y", stackdir = "center") +
  labs(x= "Treatment", y="Total Native Plants", title= "Native Plants Across Multiple Treatments and Blocks")


  #essentially the same plots in with additional commentary submitted on CANVAS
#Assignment 1.4a script
#Brianna Maloney

#load tidyverse and dicot data
library(tidyverse)
dicot <- read.csv("dicots.csv")

#make Treatment an unordered factor and change "Control" to baseline level
dicot$Treatment <- factor(dicot$Treatment, ordered = FALSE) %>%
  relevel(dicot$Treatment, ref = "Control")

#make Block a factor 
dicot$Block <- as.factor(dicot$Block)

#create a plot of the data highlighting any aspect you may want a reader to take away
# a violin plot can show us the full range of our data
ggplot(data=dicot, aes(Treatment, Total_natives)) +
  geom_violin(scale="area") +
  labs(x= "Treatment", y="Total Native Plants", title= "Native Plants Across Multiple Treatments")

# while a basic boxplot can highlight differences between means
ggplot(data=dicot, aes(Treatment, Total_natives)) +
  geom_boxplot() +
  labs(x= "Treatment", y="Total Native Plants", title= "Native Plants Across Multiple Treatments")

# however I prefer a dotplot which highlights the range of our individual data points. 
ggplot(data=dicot, aes(x=Treatment, y=Total_natives, fill=Block)) +
  geom_dotplot(binwidth=0.5, binaxis="y", stackdir = "center") +
  scale_fill_brewer(palette = "Greens") +
  labs(x= "Treatment", y="Total Native Plants", title= "Native Plant Abundance Across Multiple Treatments and Blocks")
# I've added Block as a variable to see if there are any noticeable trends between blocks. 
# We could further check if Blocks are truly independent by running a multivariate analysis.

