
library(tidyverse)
library(lme4)
library(brms)
library(texreg)


# simulation settings
n_sim <- 100000
set.seed(0)

# load utils
source("code/utils.R")

# load data
source("code/load_data.R")

df <- df %>% mutate(prompt = as.factor(prompt))

# util function
empirical_two_sided_p_value <- function(obs_value, sim_values) {
  p_left <- mean(obs_value <= sim_values)
  p_right <- mean(obs_value >= sim_values)
  p_val <- min(2 * min(p_left, p_right), 1)
  return(p_val)
}


#
# PROMPT ASSIGNMENT
#
shuffle_prompt_randomization <- function(df_user_prompt_covs, model_formula, n_sim) {
  
  # fetch observed prompt assignments
  prompt_assignments <- df_user_prompt_covs$prompt
    
  # observed likelihood
  m_obs <- glm(model_formula, data=df_user_prompt_covs, family = binomial())
  ll_obs <- logLik(m_obs)
  
  # init
  loglikelihoods_sims <- rep(NA, n_sim)
  df_user_covs_sim <- df_user_prompt_covs %>% select(-prompt)
  
  for (i in seq(n_sim)) {
    # shuffle prompt assingment
    prompt_assignments_sim <- sample(prompt_assignments)
    
    # run regression
    df_user_covs_sim["prompt"] <- as.factor(prompt_assignments_sim)
    m_sim <- glm(model_formula, data = df_user_covs_sim, family = binomial())
    ll_sim <- logLik(m_sim)
    loglikelihoods_sims[i] <- ll_sim
    
    cat(i, "_")
  }
  
  return(
    list(
      loglikelihoods = loglikelihoods_sims,
      loglikelihood_obs = ll_obs,
      p = empirical_two_sided_p_value(ll_obs, loglikelihoods_sims)
    )
  )
}


# settings
model_formula <- formula(
  prompt ~ 
    days_active + 
    num_statuses + 
    num_favorites + 
    num_followers + 
    num_friends
)


# all
df_all_user_prompt <- df %>% 
  select(prompt, user) %>% 
  mutate(user = as.character(user)) %>%  
  distinct() %>% 
  na.omit()

df_all_user_prompt_covs <- inner_join(df_all_user_prompt, df_covariates, by="user")

all_res <- shuffle_prompt_randomization(df_all_user_prompt_covs, model_formula, n_sim)
print(all_res$p)


# q1
df_q1_user_prompt <- df %>% 
  select(prompt, user, survey_feed_difference) %>% 
  mutate(user = as.character(user)) %>%
  distinct() %>% 
  na.omit()

df_q1_user_prompt_covs <- inner_join(df_q1_user_prompt, df_covariates, by="user")

q1_res <- shuffle_prompt_randomization(df_q1_user_prompt_covs, model_formula, n_sim)
print(q1_res$p)


# q2
df_q2_user_prompt <- df %>% 
  select(prompt, user, survey_learned_something_new) %>% 
  mutate(user = as.character(user)) %>%  
  distinct() %>% 
  na.omit()

df_q2_user_prompt_covs <- inner_join(df_q2_user_prompt, df_covariates, by="user")

q2_res <- shuffle_prompt_randomization(df_q2_user_prompt_covs, model_formula, n_sim)
print(q2_res$p)


# q3
df_q3_user_prompt <- df %>% 
  select(prompt, user, survey_understood_why_views) %>% 
  mutate(user = as.character(user)) %>%  
  distinct() %>% 
  na.omit()

df_q3_user_prompt_covs <- inner_join(df_q3_user_prompt, df_covariates, by="user")

q3_res <- shuffle_prompt_randomization(df_q3_user_prompt_covs, model_formula, n_sim)
print(q3_res$p)


# q4
df_q4_user_prompt <- df %>% 
  select(prompt, user, survey_conversation_in_future) %>% 
  mutate(user = as.character(user)) %>%
  distinct() %>% 
  na.omit()

df_q4_user_prompt_covs <- inner_join(df_q4_user_prompt, df_covariates, by="user")

q4_res <- shuffle_prompt_randomization(df_q4_user_prompt_covs, model_formula, n_sim)
print(q4_res$p)


# plot
df_plt_lls <- rbind(
    tibble(ll = all_res$loglikelihoods, subset = "All"),
    tibble(ll = q1_res$loglikelihoods, subset = "Q1"),
    tibble(ll = q2_res$loglikelihoods, subset = "Q2"),
    tibble(ll = q3_res$loglikelihoods, subset = "Q3"),
    tibble(ll = q4_res$loglikelihoods, subset = "Q4")
  )

df_plt_lls_obs_p <- rbind(
    tibble(ll_obs = all_res$loglikelihood_obs, p_val = all_res$p, subset = "All"),
    tibble(ll_obs = q1_res$loglikelihood_obs, p_val = q1_res$p, subset = "Q1"),
    tibble(ll_obs = q2_res$loglikelihood_obs, p_val = q2_res$p, subset = "Q2"),
    tibble(ll_obs = q3_res$loglikelihood_obs, p_val = q3_res$p, subset = "Q3"),
    tibble(ll_obs = q4_res$loglikelihood_obs, p_val = q4_res$p, subset = "Q4")
  )

plt_prompt_ri <- df_plt_lls %>%
  ggplot(aes(x = ll)) + 
  geom_histogram(color="gray20", fill="grey80", bins = 50) + 
  geom_vline(
    data = df_plt_lls_obs_p,
    aes(xintercept = ll_obs, color = "red"),
    size = 0.5
  ) +
  facet_wrap(~ subset, scales = "free") +
  labs(x = "Log-likelihood", y = "Frequency") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_label(
    data = df_plt_lls_obs_p,
    aes(label = str_c("p = ", round(p_val, digits = 2))), 
    x = Inf, y = Inf,
    hjust = 1, vjust = 1
  ) 


# save data & plot
ggsave(
  "plots/balance_ri_prompt.pdf",
  plot = plt_prompt_ri,
  units = "in",
  width = 9,
  height = 6
)

save(all_res, q1_res, q2_res, q3_res, q4_res,
     file = "models/balance_ri/prompt_ri.Rdata")


#
# FEED ASSIGNMENT
#
shuffle_feed_randomization <- function(df_user_feed_covs, model_formula, n_sim) {
  # glmer controls
  controls <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000))
  
  # observed regression
  m_obs <- glmer(model_formula, family = binomial, data = df_user_feed_covs)
  ll_obs <- logLik(m_obs)
  
  # init
  df_user_feed_covs_sim <- df_user_feed_covs %>% select(-giver, -feed, -feed_opp)
  df_feed_givers <- df_user_feed_covs %>% select(giver, feed)
  loglikelihoods_sims <- rep(NA, n_sim)
  
  for (i in seq(n_sim)) {
    # shuffle feed givers
    df_feed_givers_sim <- df_feed_givers %>% sample_frac(size = 1, replace = F)
    
    # append to sessions
    df_sim <- bind_cols(df_user_feed_covs_sim, df_feed_givers_sim)
    df_sim <- df_sim %>% mutate(feed_opp = factor(feed != ideology))
    
    m_sim <- glmer(model_formula, family = binomial, data = df_sim)
    ll_sim <- logLik(m_sim)
    loglikelihoods_sims[i] <- ll_sim
    
    cat(i, "_")
  }
  
  return(
    list(
      loglikelihoods = loglikelihoods_sims,
      loglikelihood_obs = ll_obs,
      p = empirical_two_sided_p_value(ll_obs, loglikelihoods_sims)
    )
  )
}


# settings
model_formula <- formula(
  feed_opp ~ 
    days_active + 
    num_statuses + 
    num_favorites + 
    num_followers + 
    num_friends + 
    (1 | user)
)


# all
df_all <- df %>% select(
  giver,
  feed,
  user,
  ideology,
  feed_opp,
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

all_res <- shuffle_feed_randomization(df_all, model_formula, n_sim)
print(all_res$p)


# q1
df_q1 <- df_all %>% filter(!is.na(survey_feed_difference)) 

q1_res <- shuffle_feed_randomization(df_q1, model_formula, n_sim)
print(q1_res$p)


# q2
df_q2 <- df_all %>% filter(!is.na(survey_learned_something_new))

q2_res <- shuffle_feed_randomization(df_q2, model_formula, n_sim)
print(q2_res$p)


# q3
df_q3 <- df_all %>% filter(!is.na(survey_understood_why_views))

q3_res <- shuffle_feed_randomization(df_q3, model_formula, n_sim)
print(q3_res$p)


# q4
df_q4 <- df_all %>% filter(!is.na(survey_conversation_in_future))

q4_res <- shuffle_feed_randomization(df_q4, model_formula, n_sim)
print(q4_res$p)


# plot
df_plt_lls <- 
  rbind(
    tibble(ll = all_res$loglikelihoods, subset = "All"),
    tibble(ll = q1_res$loglikelihoods, subset = "Q1"),
    tibble(ll = q2_res$loglikelihoods, subset = "Q2"),
    tibble(ll = q3_res$loglikelihoods, subset = "Q3"),
    tibble(ll = q4_res$loglikelihoods, subset = "Q4")
  )

df_plt_lls_obs_p <- 
  rbind(
    tibble(ll_obs = all_res$loglikelihood_obs, p_val = all_res$p, subset = "All"),
    tibble(ll_obs = q1_res$loglikelihood_obs, p_val = q1_res$p, subset = "Q1"),
    tibble(ll_obs = q2_res$loglikelihood_obs, p_val = q2_res$p, subset = "Q2"),
    tibble(ll_obs = q3_res$loglikelihood_obs, p_val = q3_res$p, subset = "Q3"),
    tibble(ll_obs = q4_res$loglikelihood_obs, p_val = q4_res$p, subset = "Q4")
  )

plt_feed_opp_ri <- df_plt_lls %>% 
  ggplot(aes(x = ll)) + 
  geom_histogram(color="gray20", fill="grey80", bins = 50) + 
  geom_vline(
    data = df_plt_lls_obs_p,
    aes(xintercept = ll_obs, color = "red"),
    size = 0.5
  ) +
  facet_wrap(~ subset, scales = "free") +
  labs(x = "Log-likelihood", y = "Frequency") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_label(
    data = df_plt_lls_obs_p,
    aes(label = str_c("p = ", round(p_val, digits = 2))), 
    x = Inf, y = Inf,
    hjust = 1, vjust = 1
  ) 

# save data & plot
ggsave(
  "plots/balance_ri_feed_opp.pdf",
  plot = plt_feed_opp_ri,
  units = "in",
  width = 9,
  height = 6
)

save(all_res, q1_res, q2_res, q3_res, q4_res,
     file = "models/balance_ri/feed_opp.Rdata")

# END