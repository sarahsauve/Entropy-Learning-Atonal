##Based on simulation from 'Understanding mixed effects model through data simulation' (2019) DeBruine & Barr

library("lme4")
library("truncnorm")
library("afex")
library("broom.mixed")
library("faux")
library("tidyverse")

#set fixed effect parameters
b0 <- 4.5 #intercept
b1 <- .5 #slope

#set random effect parameters
S0s_sd <- .5 #by-subject random intercept sd (for the paper simulation it was 100 for an intercept of 800, so 4.5/8 = .56)
I0i_sd <- .2 #by-item random intercept sd
S1s_sd <- .2 #by-subject random slope sd
scor <- .2 #correlation between intercept and slope
err_sd <- 1 #residual error sd (x2 by-subject sd)

#define # of subjects and items
nsubj <- 30
nitem <- c(before = 8, after = 8)

#simulation function
my_sim_data <- function(
  nsubj, #number of subjects
  nitem = c(before = 8, after = 8), #number of items ###CHANGE BASED ON EXPERIMENT BEING SIMULATED###
  b0, #grand mean
  b1, # effect of category
  I0i_sd, #by-item random intercept sd
  S0s_sd, #by-subject random intercept sd
  S1s_sd, #by-subject random slope sd
  scor, #correlation between intercept and slope
  err_sd){ #resicual standard deviation
  
  #simulate items
  items <- faux::sim_design(
    between = list(category = c("before", "after")), ###CHANGE BASED ON EXPERIMENT BEING SIMULATED###
    n = nitem,
    sd = I0i_sd,
    dv = "I0i",
    id = "item_id",
    plot = FALSE
  )
  
  #effect code category
  items$cat <- recode(items$category, "before" = -0.25, "after" = 0.25) ###CHANGE BASED ON EXPERIMENT BEING SIMULATED###
  
  #simulate subjects
  subjects <- faux::sim_design(
    within = list(effect = c(S0s = "By-subject random intercepts",
                             S1s = "by-subject random slopes")),
    n = nsubj,
    sd = c(S0s_sd, S1s_sd),
    r = scor,
    id = "subj_id",
    plot = FALSE
  )
  
  #simulate trials
  dat_sim <- crossing(subj_id = subjects$subj_id,
                      item_id = items$item_id) %>%
    inner_join(subjects, "subj_id") %>%
    inner_join(items, "item_id") %>%
    mutate(err = rnorm(n = nrow(.), mean = 0, sd = err_sd)) %>%
    mutate(Rating = b0 + I0i + S0s + (b1 + S1s) * cat + err) %>%
    select(subj_id, item_id, category, cat, Rating)
  
  dat_sim
}

#generate simulated data
dat_sim <- my_sim_data(nsubj = nsubj, nitem = nitem, b0 = b0, b1 = b1, I0i_sd = I0i_sd, S0s_sd = S0s_sd, S1s_sd = S1s_sd, scor = scor, err_sd = err_sd)

#fit a linear mixed-effects model to the data
mod_sim <- lmer(Rating ~ 1 + cat + (1|item_id) + (1+cat|subj_id), data = dat_sim, REML = FALSE)
summary(mod_sim, corr = FALSE)

#get a tidy table of results
broom.mixed::tidy(mod_sim) %>%
  mutate(sim = c(b0, b1, S0s_sd, S1s_sd, scor, I0i_sd, err_sd)) %>%
  select(1:3, 9, 4:8)


#set up the power function
my_lmer_power <- function(...){
  dat_sim <- my_sim_data(...)
  mod_sim <- lmer(Rating ~ cat + (1|item_id) + (1+cat|subj_id),
                  dat_sim, REML = FALSE)
  
  broom.mixed::tidy(mod_sim)
}

#run model with default parameters
my_lmer_power(nsubj = nsubj, nitem = nitem, b0 = b0, b1 = b1, I0i_sd = I0i_sd, S0s_sd = S0s_sd, S1s_sd = S1s_sd, scor = scor, err_sd = err_sd)

#run simulations and save to a file
reps <- 100
sims <- purrr::map_df(1:reps, ~my_lmer_power(nsubj = nsubj, nitem = nitem, b0 = b0, b1 = b1, I0i_sd = I0i_sd, S0s_sd = S0s_sd, S1s_sd = S1s_sd, scor = scor, err_sd = err_sd))
write_csv(sims, "Mixedeffects_simulation.csv")

#read saved simulation data
sims <- read_csv("R_mixedeffects_simulation.csv", col_types = cols(
  group = col_factor(ordered = TRUE),
  term = col_factor(ordered = TRUE)
))

#calculate mean estimates and power for specified alpha
alpha <- 0.05

sims %>%
  filter(effect == "fixed") %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            mean_se = mean(std.error),
            power = mean(p.value < alpha)
  )
