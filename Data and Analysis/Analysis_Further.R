##Further Analysis
#As outlined in 'Further Analysis.docx' and submitted to OSF in pre-registration

# Package load & Data Import ----------------------------------------------

library(tidyverse)
library(lme4)

#Import IDyOM data - tonal&atonal LTM
idyom.data.combined <- read_csv("IDyOM_data_combinedLTM.csv")

#Import IDyOM data - atonal LTM
idyom.data.atonal <- read_csv("IDyOM_data_atonalLTM.csv")

#Import behavioural data
human.data <- read_csv("Human_data.csv")

#Coefficient of determination function
variance.explained <- function(model, data){
  SStot <- sum((data-mean(data))^2)
  SSres <- sum(residuals(model)^2)
  n <- length(data)
  fit <- ((SStot-SSres)/n)/(SStot/n)
  fit
}


# Confounding effect of perceived closure ---------------------------------

#Correlation betwteen prediction & closure percepts
human.data.prediction <- filter(human.data, RatingType == "Prediction")
human.data.closure <- filter(human.data, RatingType == "Closure")
cor.test(human.data.prediction$Rating, human.data.closure$Rating)

#Variance explained by closure ratings
prediction.model <- lmer(Rating ~ Time + (1+MelodyID|Participant), data = human.data.prediction)

humna.data.prediction$Closure <- human.data.closure$Rating
prediction.closure <- lmer(Rating ~ Time + Closure + (1+MelodyID|Participant), data = human.data.prediction)

anova(prediction.model, prediction.closure)
r2.difference <- variance.explained(prediction.model) - variance.explained(prediction.closure)


# Compartmentalization of style -------------------------------------------

#Correlations between human and IDyOM for prediction ratings
combined.LTM.cor.prediction <- cor.test(human.data.prediction$Rating, idyom.data.combined$information.content)
atonal.LTM.cor.prediction <- cor.test(human.data.prediction$Rating, idyom.data.atonal$information.content)

#For precision ratings
human.data.precision <- filter(human.data, RatingType == "Precision")
combined.LTM.cor.precision <- cor.test(human.data.precision$Rating, idyom.data.combined$entropy)
atonal.LTM.cor.precision <- cor.test(human.data.precision$Rating, idyom.data.atonal$entropy)
