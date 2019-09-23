##Power calculations based on simulated human data

library(tidyverse)
library(pwr)

##Import IDyOM data
idyom.data <- read_csv("IDyOM_data.csv",
                       col_types = cols(X1 = col_skip()))

#add data Type (IDyOM vs human)
idyom.data$Type <- "IDyOM"

##Simulate human data
human.data <- idyom.data %>%
  select(melody.id, note.id, melody.name, cpitch, Viewpoint, Time) %>%
  mutate(information.content = rnorm(n = nrow(idyom.data), mean = mean(idyom.data$information.content), sd = sd(idyom.data$information.content)),
         entropy = rnorm(n = nrow(idyom.data), mean = mean(idyom.data$entropy), sd = sd(idyom.data$entropy)),
         Type = "Human") %>%
  select(melody.id, note.id, melody.name, cpitch, information.content, entropy, Viewpoint, Time, Type)

##Merge the two
data <- rbind(idyom.data, human.data)

##Model to obtain u and v values for the power calculation

data$Viewpoint <- as.factor(data$Viewpoint)
data$Time <- as.factor(data$Time)
data$Type <- as.factor(data$Type)
model <- glm(information.content ~ Viewpoint + Time + Type, family = "gaussian", data = data)
summary(model)

#u = (6 x 2 x 2) - 1 = 23
#v = 2189 from output's Residual deviance degrees of freedom

#Effect size
#Calculate R2 first
r2 <- variance.explained(model = model, data$information.content) #.004
f2 <- r2/(1-r2) #.004

#Power calculation
pwr.f2.test(u = 23, v = 2189, f2 = f2, sig.level = 0.05)
#power = .36

#So, what kind of effect size can I detect with 80% power, assuming the same size dataset (averaging across participants for each pitch)
pwr.f2.test(u = 23, v = 2189, sig.level = 0.05, power = .80)
#f2 = .01
#That equates to an R2 or pretty much .1 too, so it wouldn't take much
#Also, since we're averaging across participants, the number of participants doesn't matter as much?

##Try power test with the paired t-test comparing before & after, which is also an effect we want to see and need participant power for
#n is unknown   d = .2      sig.level = .05     power = .8      type = paired       alternative = less
pwr.t.test(d = .2, sig.level = .05, power = .8, type = "paired")
#199 people... not gonna happen obviously
#with d = .5, a large effect size
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "paired")
#34 people, still not likely
#what's the power when we have 12?
pwr.t.test(n = 12, d = .2, sig.level = .05, type = "paired")
#power is .09, which is VERY low... how about 15 with larger effect size?
pwr.t.test(n = 15, d = .5, sig.level = .05, type = "paired")
#power is .43, still too low, so we might need to go over two years for proper power - that shouldn't be too much of a problem