##Confirmatory Analysis
#As outlined in 'Confirmatory Analysis.docx' and submitted to OSF in pre-registration

# Package load & Data Import ----------------------------------------------

library(tidyverse)
library(lme4)

#Import IDyOM data
idyom.data <- read_csv("IDyOM_data.csv",
                       col_types = cols(X1 = col_skip()))

#Import IDyOM data specifically for hypothesis 2b
idyom.data.2b <- read_csv("IDyOM_data_2b.csv")

#Import behavioural data
human.data <- read_csv("Human_data.csv")

#Merge data
idyom.data$Type <- "IDyOM"
human.data$Type <- "Human"

data <- rbind(idyom.data, human.data)
data$Type <- as.factor(data$Type)

# Omnibus test ------------------------------------------------------------

#Prediction and precision models fitted with maximum random effects;
#Evaluation includes coefficients, their 95% confidence intervals and the variance explained (R2) of the model
#(variance.explained is a function written by me)
prediction.data <- filter(data, RatingType == "Prediction")
prediction.model <- lmer(Rating ~ Time * Type + (1+MelodyID|Participant), data = prediction.data)
summary(prediction.model)
variance.explained(prediction.model, data$prediction) #R2
confint(prediction.model, method = "Wald")

precision.data <- filter(data, RatingType == "Precision")
##Subtract ratings from 8 to match scales
precision.data$Rating <- 8-precision.data$Rating
precision.model <- lmer(Rating ~ Time * Type + (1+MelodyID|Participant), data = precision.data)
summary(precision.model)
variance.explained(precision.model, data$precision)
confint(precision.model, method = "Wald")


# Analysis 1a) ------------------------------------------------------------

human.before.prediction <- filter(human.data, Time == "Before", RatingType == "Prediction")
t.test(human.before.prediction$Rating, mu = 5, alternative = "greater")

human.before.precision <- filter(human.data, Time == "Before", RatingType == "Precision")
t.test(human.before.precision$Rating, mu = 5, alternative = "greater")


# Analysis 1b) ------------------------------------------------------------

human.after.prediction <- filter(human.data, Time == "After", RatingType == "Prediction")
t.test(human.before.prediction$Rating, human.after.prediction$Rating, alternative = "greater")

humna.after.precision <- filter(human.data, Time == "After", RatingType == "Precision")
t.test(human.before.precision$Rating, human.after.precision$Rating)

#create dataset organized by pairs for TOST analysis
paired.human.data.prediction <- cbind(human.before.prediction, human.after.prediction)
colnames(paired.human.data.prediction) <- c("Time1", "Participant1", "MelodyID1", "NoteID1", "RatingType1", "Rating1", "RT1",
                                            "Time2", "Participant2", "MelodyID2", "NoteID2", "RatingType2", "Rating2", "RT2")

paired.human.data.precision <- cbind(human.before.precision, human.after.precision)
colnames(paired.human.data.precision) <- c("Time1", "Participant1", "MelodyID1", "NoteID1", "RatingType1", "Rating1", "RT1",
                                            "Time2", "Participant2", "MelodyID2", "NoteID2", "RatingType2", "Rating2", "RT2")

dataTOSTpaired(paired.human.data.predicion, pairs = c(list(i1 = "Rating1", i2 = "Rating2")), high_eqbound = .5, eqbound_type = "raw")

dataTOSTpaired(paired.human.data.precision, pairs = c(list(i1 = "Rating1", i2 = "Rating2")), low_eqbound = -.5,
               high_eqbound = .5, eqbound_type = "raw")


# Analysis 2a) ----------------------------------------------------------

idyom.after <- filter(idyom.data, Time == "After")
t.test(scale(human.after.prediction$Rating), scale(idyom.after$information.content), alternative = "less")

t.test(scale(human.after.precision$Rating), scale(idyom.after$entropy), alternative = "less")


# Analysis 2b) ----------------------------------------------------------

#Create list of viewpoints
viewpoints <- c("cpitch", "cpint", "cpintfref", "cpitch-cpint", "cpitch-cpintfref", "cpint-cpintfref", "cpitch-cpint-cpintfref")

#Create relevant functions
#One to test correlations and make a data frame containing the results of those tests
cor.function <- function(human.data, idyom.data.2b, rating.type, idyom.values){
  results <- NULL
  for(i in 1:length(viewpoints)){ #for each viewpoint
    idyom.subset <- filter(idyom.data, Viewpoint == viewpoints[i]) #filter relevant data corresponding to that viewpoint
    human.subset <- filter(human.data, Ratingtype == rating.type) #identify column containing ratings
    idyom.colnum <- which(colnames(idyom.subset) == idyom.values) #identify column containing 
    r.value <- cor.test(human.subset$Rating, idyom.subset[idyom.colnum])$estimate #extract r value
    confints <- cor.test(human.subset$Rating, idyom.subset[,idyom.colnum])$conf.int[1:2] #extract 95% confidence intervals
    nrow <- c(r.value, confints, viewpoints[i], rating.type)
    results <- as.data.frame(rbind(results, nrow)) #add results to data frame as new row
  }
  colnames(results) <- c("cor", "CI1", "CI2", "Viewpoint", "Metric")
  results$cor <- as.numeric(as.character(pooled$cor))
  results$CI1 <- as.numeric(as.character(pooled$CI1))
  results$CI2 <- as.numeric(as.character(pooled$CI2))
  results
}

#One to illustrate these results
illustration.function <- function(results1, results2){
  #merge into one dataset
  merged.results <- rbind(results1, results2)
  #Find maximum correlation
  maximum.correlation <- max(merged.results$cor)
  #Find minimum correlation
  minimum.correlation <- min(merged.results$cor)
  #Print them - this will return the largest positive and negative correlations
  print(maximum.correlation, minimum.correlation)
  
  #Plot
  ggplot(merged.results, aes(x = Viewpoint, y = cor)) +
    geom_bar(stat = "identity", width = .7, position = position_dodge()) +
    geom_errorbar(aes(ymin = CI1, ymax = CI2)) +
    theme_bw() +
    facet_wrap(~ Metric)
}

###Correlation tests on pooled "Before" and "After" data
#Prediction
results.pooled.prediction <- cor.function(human.data, idyom.data, "prediction", "information.content")

#Precision
results.pooled.precision <- cor.function(human.data, idyom.data, "precision", "entropy")

#Illustrate correlations and their CIs along with extracting maximum and minimum correlation values
illustration.function(results.pooled.prediction, results.pooled.precision)

###Correlation tests on separated "Before" and "After" data
#Create IDyOM time-based data subsets
idyom.before <- filter(idyom.data.2b, Time == "Before")
idyom.after <- filter(idyom.data.2b, Time == "After")

##Before
#Prediction
results.before.prediction <- cor.function(human.data, idyom.before, "prediction", "information.content")

#Precision
results.before.precision <- cor.function(human.data, idyom.before, "precision", "entropy")

#Illustrate
illustration.function(results.before.prediction, results.before.precision)


##After
#Prediction
results.after.prediction <- cor.function(human.data, idyom.after, "prediction", "information.content")

#Precision
results.after.precision <- cor.function(human.data, idyom.after, "prediction", "information.content")

#Illustrate
illustration.function(results.after.prediction, results.after.precision)