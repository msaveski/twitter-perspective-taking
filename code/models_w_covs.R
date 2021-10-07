#
# Analysis with the covariates
#
# Y ~ T * X + fixed effects
# 
library(tidyverse)
library(lme4)
library(brms)
library(texreg)


# load utils
source("code/utils.R")

# load data
source("code/load_data.R")

# output paths
lmer_output_fpath <- "models/main/lmer_covs.Rdata"
brms_output_fpath <- "models/main/brms_covs.Rdata"

#
# brms settings
#
n_chains <- 4
n_iters <- 10000
n_cores <- 4
c_adapt_delta <- 0.99
c_max_treedepth <- 15
priors <- c(
  prior_string("normal(0, 30)", class = "Intercept"),
  prior_string("normal(0, 30)", class = "b"),
  prior_string("normal(0, 1)", class = "sd")
)


#
# Timespent
#
ts_lmer <- lmer(
  timespent ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df
)

ts_i_lmer <- lmer(
  timespent ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df
)


ts_brms <- brm(
  timespent ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df,
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

ts_i_brms <- brm(
  timespent ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df,
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)


#
# Engagement
#
ne_lmer <- lmer(
  n_engagements ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df
)

ne_i_lmer <- lmer(
  n_engagements ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df
)


ne_brms <- brm(
  n_engagements ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df,
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

ne_i_brms <- brm(
  n_engagements ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df,
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)


#
# Q1
#
df_q1 <- df %>% filter(!is.na(survey_feed_difference))

q1_lmer <- lmer(
  survey_feed_difference ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df_q1
)

q1_i_lmer <- lmer(
  survey_feed_difference ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df_q1
)

q1_brms <- brm(
  survey_feed_difference ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df_q1,
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

q1_i_brms <- brm(
  survey_feed_difference ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df_q1,
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)


#
# Q2
#
df_q2 <- df %>% filter(!is.na(survey_learned_something_new))

q2_lmer <- lmer(
  survey_learned_something_new ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df_q2
)

q2_i_lmer <- lmer(
  survey_learned_something_new ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df_q2
)


q2_brms <- brm(
  survey_learned_something_new ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df_q2, 
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors  
)

q2_i_brms <- brm(
  survey_learned_something_new ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df_q2, 
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)


#
# Q3
#
df_q3 <- df %>% filter(!is.na(survey_understood_why_views))

q3_lmer <- lmer(
  survey_understood_why_views ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df_q3
)

q3_i_lmer <- lmer(
  survey_understood_why_views ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df_q3
)


q3_brms <- brm(
  survey_understood_why_views ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df_q3, 
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

q3_i_brms <- brm(
  survey_understood_why_views ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df_q3, 
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)


#
# Q4
#
df_q4 <- df %>% filter(!is.na(survey_conversation_in_future))

q4_lmer <- lmer(
  survey_conversation_in_future ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df_q4
)

q4_i_lmer <- lmer(
  survey_conversation_in_future ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 | user) +
    (1 | giver),
  data = df_q4
)


q4_brms <- brm(
  survey_conversation_in_future ~
    feed_opp + prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df_q4, 
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

q4_i_brms <- brm(
  survey_conversation_in_future ~
    feed_opp * prompt +
    feed_opp * days_active +
    feed_opp * num_statuses +
    feed_opp * num_favorites +
    feed_opp * num_followers +
    feed_opp * num_friends +
    prompt * days_active +
    prompt * num_statuses +
    prompt * num_favorites +
    prompt * num_followers +
    prompt * num_friends +
    (1 + feed_opp | user) +
    (1 + prompt | giver), 
  data = df_q4, 
  family = gaussian(), 
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)


#
# Save models
#

# lmer
save(
  ts_lmer, ts_i_lmer,
  ne_lmer, ne_i_lmer,
  q1_lmer, q1_i_lmer,
  q2_lmer, q2_i_lmer,
  q3_lmer, q3_i_lmer,
  q4_lmer, q4_i_lmer,
  file = lmer_output_fpath
)

# brms
save(
  ts_brms, ts_i_brms, 
  ne_brms, ne_i_brms,
  q1_brms, q1_i_brms, 
  q2_brms, q2_i_brms,
  q3_brms, q3_i_brms, 
  q4_brms, q4_i_brms,
  file = brms_output_fpath
)


#
# Tables
#

# lmer
screenreg(
  list(
    ts_lmer = ts_lmer, ts_i_lmer = ts_i_lmer,
    ne_lmer = ne_lmer, ne_i_lmer = ne_i_lmer,
    q1_lmer = q1_lmer, q1_i_lmer = q1_i_lmer,
    q2_lmer = q2_lmer, q2_i_lmer = q2_i_lmer,
    q3_lmer = q3_lmer, q3_i_lmer = q3_i_lmer,
    q4_lmer = q4_lmer, q4_i_lmer = q4_i_lmer
  )
)

# brms
screenreg(
  list(
    ts_brms = ts_brms, ts_i_brms = ts_i_brms, 
    ne_brms = ne_brms, ne_i_brms = ne_i_brms,
    q1_brms = q1_brms, q1_i_brms = q1_i_brms,
    q2_brms = q2_brms, q2_i_brms = q2_i_brms,
    q3_brms = q3_brms, q3_i_brms = q3_i_brms,
    q4_brms = q4_brms, q4_i_brms = q4_i_brms
  )
)

# END