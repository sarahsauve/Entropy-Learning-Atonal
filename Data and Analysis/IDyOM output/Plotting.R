##Visualize IDyOM data

library(tidyverse)
library(lme4)
library(lmerTest)

#Import
data <- read_csv("IDyOM_data.csv")

#Plotting data
plotting_data <- data %>%
  group_by(Time, melody.id, Viewpoint) %>%
  summarise(meanIC = mean(information.content),
            meanEntropy = mean(entropy))

#plot IC
ggplot(plotting_data, aes(x = melody.id, y = meanIC, fill = Time)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  theme_bw() +
  facet_wrap(~ Viewpoint)

#there are small differences when comparing tonal vs. tonal + atonal and much larger differences in the expected direction (smaller IC) when comparing
#tonal vs. atonal only training

#plot entropy
ggplot(plotting_data, aes(x = melody.id, y = meanEntropy, fill = Time)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  theme_bw() +
  facet_wrap(~ Viewpoint)

#here the pattern is flipped, a model trained on atonal music as much higher entropy - so higher uncertainty.
#is this related to having less information to go on, or the nature of the music?

anova <- lmer(information.content ~ Viewpoint * Time + (1|melody.id), data = data, REML = FALSE)
summary(anova) #significant intercept only
anova(anova) #main effect of viewpoint
confint(anova, method = "Wald") #nothing except intercept
