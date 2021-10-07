
library(tidyverse)
library(stargazer)
library(texreg)
library(broom)
library(lme4)
library(latex2exp)
library(brms)


# NOTE: switch between "trimming_bounds_feed" and "trimming_bounds_prompt"
#       to run analyses for each of the two treatments.

# load data
source("code/load_data.R")

# left join events
df_events <- read_csv("data/sessions_events.csv", col_types = "cc")

df <- left_join(df, df_events, by = "session_id")

# create a survey shown variable
df <- df %>%
  mutate(
    survey_shown =
      !is.na(event) |
      !is.na(survey_feed_difference) |
      !is.na(survey_learned_something_new) |
      !is.na(survey_understood_why_views) |
      !is.na(survey_conversation_in_future)
  ) %>%
  select(-event)

# add attrition variables
df <- df %>% 
  mutate(
    q1_na = is.na(survey_feed_difference),
    q2_na = is.na(survey_learned_something_new),
    q3_na = is.na(survey_understood_why_views),
    q4_na = is.na(survey_conversation_in_future)
  )

# make sure the treament variables are factors
df <- df %>%
  mutate(
    feed_opp = as.factor(feed != ideology),
    prompt = as.factor(prompt),
    user = factor(user)
  )


#########################################################################################################
#
# Monotonicity / Trimming bounds
#
#########################################################################################################

trimming_bounds_feed <- function(df_q, m_formula_no_int, m_formula_with_int, model_type) {
  # compute Q: [(missing in treatment) - (missing in control)] / (missing in treatment)
  # ng: comment is wrong, should be "non-missing" above?
  df_q_na <- df_q %>%
    filter(!is.na(feed_opp)) %>%
    summarise(
      q_feed_opp_f_missing = sum(!q_na & feed_opp == T) / sum(feed_opp == T),
      q_feed_same_f_missing = sum(!q_na & feed_opp == F) / sum(feed_opp == F),
      q = (q_feed_opp_f_missing - q_feed_same_f_missing) / q_feed_opp_f_missing
    )
  Q = df_q_na[[1, "q"]]
  
  # df control
  df_feed_same <- df_q %>% 
    filter(feed_opp == F, !q_na)
  
  # df treatment
  df_feed_opp <- df_q %>% 
    filter(feed_opp == T, !q_na) %>%
    mutate(y1_perc_rank = percent_rank(q_ans))
  
  # trim treatment obs
  df_feed_opp_lb <- df_feed_opp %>%
    filter(y1_perc_rank < (1 - Q))
  
  df_feed_opp_ub <- df_feed_opp %>%
    filter(y1_perc_rank > Q)
  
  # combine dfs
  df_q_lb <- rbind(df_feed_same, df_feed_opp_lb %>% select(-y1_perc_rank))
  df_q_ub <- rbind(df_feed_same, df_feed_opp_ub %>% select(-y1_perc_rank))
  
  # run regressions
  if (model_type == "lmer"){
    m_q_lb <-   lmer(m_formula_no_int, data=df_q_lb)
    m_i_q_lb <- lmer(m_formula_with_int, data=df_q_lb)
    
    m_q_ub <-   lmer(m_formula_no_int, data=df_q_ub)
    m_i_q_ub <- lmer(m_formula_with_int, data=df_q_ub)
  }
  else {
    m_q_lb <-   brm(m_formula_no_int, 
                    data=df_q_lb,
                    family = gaussian(),
                    chains = n_chains, iter = n_iters, cores = n_cores,
                    control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
                    prior = priors,
                    seed = 0
    )
    
    m_i_q_lb <- brm(m_formula_with_int,
                    data=df_q_lb,
                    family = gaussian(),
                    chains = n_chains, iter = n_iters, cores = n_cores,
                    control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
                    prior = priors,
                    seed = 0
    )
    
    m_q_ub <-   brm(m_formula_no_int, 
                    data=df_q_ub,
                    family = gaussian(),
                    chains = n_chains, iter = n_iters, cores = n_cores,
                    control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
                    prior = priors,
                    seed = 0
    )
    
    m_i_q_ub <- brm(m_formula_with_int,
                    data=df_q_ub,
                    family = gaussian(),
                    chains = n_chains, iter = n_iters, cores = n_cores,
                    control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
                    prior = priors,
                    seed = 0
    )
    
  }
  
  res <- list(
    m_q_lb = m_q_lb,
    m_q_ub = m_q_ub,
    m_i_q_lb = m_i_q_lb,
    m_i_q_ub = m_i_q_ub
  )
  
  return(res)
}

trimming_bounds_prompt <- function(df_q, m_formula_no_int, m_formula_with_int, model_type) {
  
  # compute Q: [(missing in treatment) - (missing in control)] / (missing in treatment)
  df_q_na <- df_q %>%
    summarise(
      q_prompt_empathic_f_missing = sum(!q_na & prompt == "empathic") / sum(prompt == "empathic"),
      q_prompt_conrol_f_missing = sum(!q_na & prompt == "control") / sum(prompt == "control"),
      q = (q_prompt_empathic_f_missing - q_prompt_conrol_f_missing) / q_prompt_empathic_f_missing
    )
  Q = df_q_na[[1, "q"]]
  
  # df control
  df_prompt_control <- df_q %>% 
    filter(prompt == "control", !q_na)
  
  # df treatment
  df_prompt_empathic <- df_q %>% 
    filter(prompt == "empathic", !q_na) %>%
    mutate(y1_perc_rank = percent_rank(q_ans))
  
  # trim treatment obs
  df_prompt_lb <- df_prompt_empathic %>%
    filter(y1_perc_rank < (1 - Q))
  
  df_prompt_ub <- df_prompt_empathic %>%
    filter(y1_perc_rank > Q)
  
  # combine dfs
  df_q_lb <- rbind(df_prompt_control, df_prompt_lb %>% select(-y1_perc_rank))
  df_q_ub <- rbind(df_prompt_control, df_prompt_ub %>% select(-y1_perc_rank))
  
  # run regressions
  if (model_type == "lmer"){
    m_q_lb <-   lmer(m_formula_no_int, data=df_q_lb)
    m_i_q_lb <- lmer(m_formula_with_int, data=df_q_lb)
    
    m_q_ub <-   lmer(m_formula_no_int, data=df_q_ub)
    m_i_q_ub <- lmer(m_formula_with_int, data=df_q_ub)
  }
  else {
    m_q_lb <-   brm(m_formula_no_int, 
                    data=df_q_lb,
                    family = gaussian(),
                    chains = n_chains, iter = n_iters, cores = n_cores,
                    control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
                    prior = priors,
                    seed = 0
    )
    
    m_i_q_lb <- brm(m_formula_with_int,
                    data=df_q_lb,
                    family = gaussian(),
                    chains = n_chains, iter = n_iters, cores = n_cores,
                    control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
                    prior = priors,
                    seed = 0
    )
    
    m_q_ub <-   brm(m_formula_no_int, 
                    data=df_q_ub,
                    family = gaussian(),
                    chains = n_chains, iter = n_iters, cores = n_cores,
                    control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
                    prior = priors,
                    seed = 0
    )
    
    m_i_q_ub <- brm(m_formula_with_int,
                    data=df_q_ub,
                    family = gaussian(),
                    chains = n_chains, iter = n_iters, cores = n_cores,
                    control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
                    prior = priors,
                    seed = 0
    )
    
  }
  
  res <- list(
    m_q_lb = m_q_lb,
    m_q_ub = m_q_ub,
    m_i_q_lb = m_i_q_lb,
    m_i_q_ub = m_i_q_ub
  )
  
  return(res)
}

# prep
df_q_sub <- df %>%
  filter(survey_shown) %>%
  select(
    feed_opp,
    prompt,
    user,
    giver,
    days_active,
    num_statuses,
    num_favorites,
    num_followers,
    num_friends,
    survey_feed_difference,
    survey_learned_something_new,
    survey_understood_why_views,
    survey_conversation_in_future
  )

################# LMER SETTINGS ################# 
lmer_formula_no_int = formula(
  q_ans ~
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
    (1 | giver)
)


lmer_formula_with_int = formula(
  q_ans ~
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
    (1 | giver)
)


################# BRM SETTINGS #################

n_chains <- 4
n_iters <- 10000
n_cores <- 4
c_adapt_delta <- 0.999
c_max_treedepth <- 15
priors <- c(
  prior_string("normal(0, 30)", class = "Intercept"), # intercept(s)
  prior_string("normal(0, 30)", class = "b"),         # coefficients
  prior_string("normal(0, 1)", class = "sd")          # standard deviation
)

brm_formula_no_int = formula(
  q_ans ~
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
    (1 + prompt | giver)
)

brm_formula_with_int = formula(
  q_ans ~
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
    (1 + prompt | giver)
)

# --> Q1
df_q1 <- df_q_sub %>%
  mutate(
    q_ans = survey_feed_difference,
    q_na = is.na(survey_feed_difference)
  )

q1_trim_bounds_lmer <- trimming_bounds_feed(df_q1,
                                       lmer_formula_no_int,
                                       lmer_formula_with_int,
                                       'lmer')

screenreg(q1_trim_bounds_lmer)


q1_trim_bounds_brm <- trimming_bounds_feed(df_q1,
                                      brm_formula_no_int,
                                      brm_formula_with_int,
                                      'brm')

screenreg(q1_trim_bounds_brm)

# --> Q2
df_q2 <- df_q_sub %>%
  mutate(
    q_ans = survey_learned_something_new,
    q_na = is.na(survey_learned_something_new)
  )

q2_trim_bounds_lmer <- trimming_bounds_feed(df_q2,
                                  lmer_formula_no_int,
                                  lmer_formula_with_int,
                                  'lmer')

screenreg(q2_trim_bounds_lmer)

q2_trim_bounds_brm <- trimming_bounds_feed(df_q2,
                                      brm_formula_no_int,
                                      brm_formula_with_int,
                                      'brm')

screenreg(q2_trim_bounds_brm)

# --> Q3
df_q3 <- df_q_sub %>%
  mutate(
    q_ans = survey_understood_why_views,
    q_na = is.na(survey_understood_why_views)
  )

q3_trim_bounds_lmer <- trimming_bounds_feed(df_q3,
                                  lmer_formula_no_int,
                                  lmer_formula_with_int,
                                  'lmer')


screenreg(q3_trim_bounds_lmer)

q3_trim_bounds_brm <- trimming_bounds_feed(df_q3,
                                      brm_formula_no_int,
                                      brm_formula_with_int,
                                      'brm')

screenreg(q3_trim_bounds_brm)

# --> Q4
df_q4 <- df_q_sub %>%
  mutate(
    q_ans = survey_conversation_in_future,
    q_na = is.na(survey_conversation_in_future)
  )

q4_trim_bounds_lmer <- trimming_bounds_feed(df_q4,
                                  lmer_formula_no_int,
                                  lmer_formula_with_int,
                                  'lmer')


screenreg(q4_trim_bounds_lmer)

q4_trim_bounds_brm <- trimming_bounds_feed(df_q4,
                                      brm_formula_no_int,
                                      brm_formula_with_int,
                                      'brm')

screenreg(q4_trim_bounds_brm)


########### LMER ###########

load("models/main/lmer_covs.Rdata")

# tables
screenreg(
  list(
    q1_lb = q1_trim_bounds_lmer$m_q_lb,
    q1_ub = q1_trim_bounds_lmer$m_q_ub,
    q2_lb = q2_trim_bounds_lmer$m_q_lb,
    q2_ub = q2_trim_bounds_lmer$m_q_ub,
    q3_lb = q3_trim_bounds_lmer$m_q_lb,
    q3_ub = q3_trim_bounds_lmer$m_q_ub,
    q4_lb = q4_trim_bounds_lmer$m_q_lb,
    q4_ub = q4_trim_bounds_lmer$m_q_ub
  )
)

screenreg(
  list(
    q1_i_lb = q1_trim_bounds_lmer$m_i_q_lb,
    q1_i_ub = q1_trim_bounds_lmer$m_i_q_ub,
    q2_i_lb = q2_trim_bounds_lmer$m_i_q_lb,
    q2_i_ub = q2_trim_bounds_lmer$m_i_q_ub,
    q3_i_lb = q3_trim_bounds_lmer$m_i_q_lb,
    q3_i_ub = q3_trim_bounds_lmer$m_i_q_ub,
    q4_i_lb = q4_trim_bounds_lmer$m_i_q_lb,
    q4_i_ub = q4_trim_bounds_lmer$m_i_q_ub
  )
)

# --> plot
extract_feed_opp_coef <- function(models, key) {
  n_models <- length(models)
  all_coefs <- rep(NA, n_models)
  
  for (i in 1:n_models) {
    model <- models[[i]][[key]]
    all_coefs[i] <- summary(model)$coefficients["feed_oppTRUE", "Estimate"]
  }
  
  return(all_coefs)
}

q1_est_lmer <- list(m = q1_lmer, m_i = q1_i_lmer)
q2_est_lmer <- list(m = q2_lmer, m_i = q2_i_lmer)
q3_est_lmer <- list(m = q3_lmer, m_i = q3_i_lmer)
q4_est_lmer <- list(m = q4_lmer, m_i = q4_i_lmer)

df_trim_models <- tribble(
  ~q, ~model, ~est,
  "Q1", q1_trim_bounds_lmer, q1_est_lmer,
  "Q2", q2_trim_bounds_lmer, q2_est_lmer,
  "Q3", q3_trim_bounds_lmer, q3_est_lmer,
  "Q4", q4_trim_bounds_lmer, q4_est_lmer
) %>%
  mutate(
    m_q_lb = extract_feed_opp_coef(model, "m_q_lb"),
    m_q_ub = extract_feed_opp_coef(model, "m_q_ub"),
    m_i_q_lb = extract_feed_opp_coef(model, "m_i_q_lb"),
    m_i_q_ub = extract_feed_opp_coef(model, "m_i_q_ub"),
    m = extract_feed_opp_coef(est, "m"),
    m_i = extract_feed_opp_coef(est, "m_i"),
  ) 

df_trim_models$q <- factor(df_trim_models$q, rev(c("Q1", "Q2", "Q3", "Q4")))

pos_above <- position_nudge(y = 0.1)
pos_below <- position_nudge(y = -0.1)

p <- ggplot(df_trim_models, aes(x = (m_q_lb - m_q_ub) / 2,  y = q)) +
  geom_errorbarh(aes(xmin = m_q_lb, xmax = m_q_ub), height = 0, position = pos_above) +
  geom_point(aes(x = m_q_lb), position = pos_above, shape = 18, size = 2) +
  geom_point(aes(x = m_q_ub), position = pos_above, shape = 18, size = 2) + 
  geom_point(aes(x = m), position = pos_above, shape = 20, size = 2) + 
  geom_errorbarh(aes(xmin = m_i_q_lb, xmax = m_i_q_ub), position = pos_below, height = 0, color="grey65") +
  geom_point(aes(x = m_i_q_lb), position = pos_below, shape = 18, size = 2, color="grey65") +
  geom_point(aes(x = m_i_q_ub), position = pos_below, shape = 18, size = 2, color="grey65") + 
  geom_point(aes(x = m_i), position = pos_below, shape = 20, size = 2, color="grey65") + 
  geom_vline(xintercept = 0, alpha = 0.5) +
  labs(x = TeX("$\\Delta$ response"), y = "") +
  xlim(-30, 30) + 
  theme_bw()

# print(p)
ggsave('plots/attrition_feed_lmer.pdf', p, width=9.4, height=5.62)


########### BRM ###########

load("models/main/brms_covs.Rdata")

# tables
screenreg(
  list(
    q1_lb = q1_trim_bounds_brm$m_q_lb,
    q1_ub = q1_trim_bounds_brm$m_q_ub,
    q2_lb = q2_trim_bounds_brm$m_q_lb,
    q2_ub = q2_trim_bounds_brm$m_q_ub,
    q3_lb = q3_trim_bounds_brm$m_q_lb,
    q3_ub = q3_trim_bounds_brm$m_q_ub,
    q4_lb = q4_trim_bounds_brm$m_q_lb,
    q4_ub = q4_trim_bounds_brm$m_q_ub
  )
)

screenreg(
  list(
    q1_i_lb = q1_trim_bounds_brm$m_i_q_lb,
    q1_i_ub = q1_trim_bounds_brm$m_i_q_ub,
    q2_i_lb = q2_trim_bounds_brm$m_i_q_lb,
    q2_i_ub = q2_trim_bounds_brm$m_i_q_ub,
    q3_i_lb = q3_trim_bounds_brm$m_i_q_lb,
    q3_i_ub = q3_trim_bounds_brm$m_i_q_ub,
    q4_i_lb = q4_trim_bounds_brm$m_i_q_lb,
    q4_i_ub = q4_trim_bounds_brm$m_i_q_ub
  )
)


# --> plot
extract_feed_opp_coef_brm <- function(models, key) {
  n_models <- length(models)
  all_coefs <- rep(NA, n_models)
  
  for (i in 1:n_models) {
    model <- models[[i]][[key]]
    all_coefs[i] <- fixef(model)["feed_oppTRUE", "Estimate"]
  }
  
  return(all_coefs)
}

q1_est_brm <- list(m = q1_brms, m_i = q1_i_brms)
q2_est_brm <- list(m = q2_brms, m_i = q2_i_brms)
q3_est_brm <- list(m = q3_brms, m_i = q3_i_brms)
q4_est_brm <- list(m = q4_brms, m_i = q4_i_brms)

df_trim_models <- tribble(
  ~q, ~model, ~est,
  "Q1", q1_trim_bounds_brm, q1_est_brm,
  "Q2", q2_trim_bounds_brm, q2_est_brm,
  "Q3", q3_trim_bounds_brm, q3_est_brm,
  "Q4", q4_trim_bounds_brm, q4_est_brm
) %>%
  mutate(
    m_q_lb = extract_feed_opp_coef_brm(model, "m_q_lb"),
    m_q_ub = extract_feed_opp_coef_brm(model, "m_q_ub"),
    m_i_q_lb = extract_feed_opp_coef_brm(model, "m_i_q_lb"),
    m_i_q_ub = extract_feed_opp_coef_brm(model, "m_i_q_ub"),
    m = extract_feed_opp_coef_brm(est, "m"),
    m_i = extract_feed_opp_coef_brm(est, "m_i"),
  ) 

df_trim_models$q <- factor(df_trim_models$q, rev(c("Q1", "Q2", "Q3", "Q4")))

pos_above <- position_nudge(y = 0.1)
pos_below <- position_nudge(y = -0.1)

p <- ggplot(df_trim_models, aes(x = (m_q_lb - m_q_ub) / 2,  y = q)) +
  geom_errorbarh(aes(xmin = m_q_lb, xmax = m_q_ub, color="black"), height = 0, position = pos_above) +
  geom_point(aes(x = m_q_lb, shape = as.factor(18)), position = pos_above, size = 2) +
  geom_point(aes(x = m_q_ub, shape = as.factor(18)), position = pos_above, size = 2) +
  geom_point(aes(x = m, shape = as.factor(19)), position = pos_above, size = 2) + 
  geom_errorbarh(aes(xmin = m_i_q_lb, xmax = m_i_q_ub, color="grey65"), position = pos_below, height = 0) +
  geom_point(aes(x = m_i_q_lb, shape = as.factor(18)), position = pos_below, size = 2, color="grey65") +
  geom_point(aes(x = m_i_q_ub, shape = as.factor(18)), position = pos_below, size = 2, color="grey65") + 
  geom_point(aes(x = m_i, shape = as.factor(19)), position = pos_below, size = 2, color="grey65") + 
  geom_vline(xintercept = 0, alpha = 0.5) +
  labs(x = TeX("$\\Delta$ response"), y = "") +
  scale_colour_manual(name = 'Models', 
                      values = c('black'='black','grey65'='grey65'), labels = c('Without interactions','With interactions')) +
  scale_shape_manual(name = "Estimates",
                     labels = c("Bound estimate", "ATE estimate"),
                     values = c(18, 19)) + 
  xlim(-30, 30) + 
  theme_bw()

# print(p)
ggsave('plots/attrition_feed_brm.pdf', p, width=9.4, height=4.6)

# END