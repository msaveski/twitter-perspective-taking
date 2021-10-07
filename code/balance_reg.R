
library(tidyverse)
library(lme4)
library(brms)
library(texreg)


# load data
source("code/load_data.R")

df <- df %>% mutate(prompt = as.factor(prompt))

# texreg for brms => slightly different version for logit
extract.brmsfit <- function(model) {
  s <- summary(model)
  
  names <- rownames(s$fixed)
  co <- s$fixed[, 1]
  ci.low <- s$fixed[, 3]
  ci.up <- s$fixed[, 4]
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    ci.low = ci.low,
    ci.up = ci.up
  )
  return(tr)
}

setMethod("extract",
          signature = className("brmsfit", "brms"),
          definition = extract.brmsfit)


#
# PROMPT ASSIGNMENT
#

glm_formula <- formula(
  prompt ~
    days_active +
    num_statuses +
    num_favorites +
    num_followers +
    num_friends
  )

# brm settings
n_chains <- 4
n_iters <- 10000
n_cores <- 4
c_adapt_delta <- 0.99
c_max_treedepth <- 15


# ---> all
df_all_user_prompt <- df %>%
  mutate(user = as.character(user)) %>%
  select(prompt, user) %>%
  distinct()

df_all_user_prompt_covs <- inner_join(df_all_user_prompt, df_covariates, by="user")

m_all_glm <- glm(glm_formula, data = df_all_user_prompt_covs, family = binomial())

m_all_brm <- brm(
  glm_formula,
  family = bernoulli,
  data = df_all_user_prompt_covs,
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth)
)


# ---> q1
df_q1_user_prompt <- df %>%
  filter(!is.na(survey_feed_difference)) %>%
  mutate(user = as.character(user)) %>%
  select(prompt, user) %>%
  distinct()

df_q1_user_prompt_covs <- inner_join(df_q1_user_prompt, df_covariates, by="user")

m_q1_glm <- glm(glm_formula, data = df_q1_user_prompt_covs, family = binomial())

m_q1_brm <- brm(
  glm_formula,
  family = bernoulli,
  data = df_q1_user_prompt_covs,
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth)  
)


# ---> q2
df_q2_user_prompt <- df %>%
  filter(!is.na(survey_learned_something_new)) %>%
  mutate(user = as.character(user)) %>%
  select(prompt, user) %>%
  distinct()

df_q2_user_prompt_covs <- inner_join(df_q2_user_prompt, df_covariates, by="user")

m_q2_glm <- glm(glm_formula, data = df_q2_user_prompt_covs, family = binomial())

m_q2_brm <- brm(
  glm_formula,
  family = bernoulli,
  data = df_q2_user_prompt_covs,
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth)  
)


# ---> q3
df_q3_user_prompt <- df %>%
  filter(!is.na(survey_understood_why_views)) %>%
  mutate(user = as.character(user)) %>%
  select(prompt, user) %>%
  distinct()

df_q3_user_prompt_covs <- inner_join(df_q3_user_prompt, df_covariates, by="user")

m_q3_glm <- glm(glm_formula, data = df_q3_user_prompt_covs, family = binomial())

m_q3_brm <- brm(
  glm_formula,
  family = bernoulli,
  data = df_q3_user_prompt_covs,
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth)  
)


# ---> q4
df_q4_user_prompt <- df %>%
  filter(!is.na(survey_understood_why_views)) %>%
  mutate(user = as.character(user)) %>%
  select(prompt, user) %>%
  distinct()

df_q4_user_prompt_covs <- inner_join(df_q4_user_prompt, df_covariates, by="user")

m_q4_glm <- glm(glm_formula, data = df_q4_user_prompt_covs, family = binomial())

m_q4_brm <- brm(
  glm_formula,
  family = bernoulli,
  data = df_q4_user_prompt_covs,
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth)  
)


# print glm models
screenreg(
  list(
    "All" = m_all_glm,
    "Q1" = m_q1_glm,
    "Q2" = m_q2_glm,
    "Q3" = m_q3_glm,
    "Q4" = m_q4_glm
  )
)

# print brm models
screenreg(
  list(
    "All" = m_all_brm,
    "Q1" = m_q1_brm,
    "Q2" = m_q2_brm,
    "Q3" = m_q3_brm,
    "Q4" = m_q4_brm
  )
)

# compare glm and brm models
screenreg(
  list(
    "All" = m_all_glm, "All" = m_all_brm,
    "Q1" = m_q1_glm, "Q1" = m_q1_brm,
    "Q2" = m_q2_glm, "Q2" = m_q2_brm,
    "Q3" = m_q3_glm, "Q3" = m_q3_brm,
    "Q4" = m_q4_glm, "Q4" = m_q4_brm
  )
)

# save
save(
  m_all_glm, m_q1_glm, m_q2_glm, m_q3_glm, m_q4_glm,
  m_all_brm, m_q1_brm, m_q2_brm, m_q3_brm, m_q4_brm,
  file = "models/balance_regs/prompt.Rdata"
)


#
# FEED ASSIGNMENT
#

glmer_formula <- formula(
  feed_opp ~ 
    days_active + 
    num_statuses + 
    num_favorites + 
    num_followers + 
    num_friends + 
    (1 | user) + 
    (1 | giver)
)

glmer_formula_no_giver <- formula(
  feed_opp ~ 
    days_active + 
    num_statuses + 
    num_favorites + 
    num_followers + 
    num_friends + 
    (1 | user)
)

glmer_settings <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 2e6)
)

# brm settings
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


# ---> all
df_all <- df %>% select(
  feed_opp,
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

m_all_glmer <- glmer(
  glmer_formula,
  family = binomial(link = "logit"),
  data = df_all,
  control = glmer_settings
)

m_all_ng_glmer <- glmer(
  glmer_formula_no_giver,
  family = binomial(link = "logit"),
  data = df_all,
  control = glmer_settings
)

m_all_brm <- brm(
  glmer_formula,
  data = df_all,
  family = bernoulli(link = "logit"),
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

# summary(m_all_brm)
# stanplot(m_all_brm, type="acf")
# stanplot(m_all_brm, type="trace")

screenreg(list(m_all_glmer, m_all_ng_glmer, m_all_brm))

# ---> q1
df_q1 <- df_all %>% filter(!is.na(survey_feed_difference))

m_q1_glmer <- glmer(
  glmer_formula,
  family = binomial(link = "logit"),
  data = df_q1,
  control = glmer_settings
)

m_q1_ng_glmer <- glmer(
  glmer_formula_no_giver,
  family = binomial(link = "logit"),
  data = df_q1,
  control = glmer_settings
)

m_q1_brm <- brm(
  glmer_formula,
  data = df_q1,
  family = bernoulli(link = "logit"),
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

# summary(m_q1_brm)
# stanplot(m_q1_brm, type="acf")
# stanplot(m_q1_brm, type="trace")

screenreg(list(m_q1_glmer, m_q1_ng_glmer, m_q1_brm))

# ---> q2
df_q2 <- df_all %>% filter(!is.na(survey_learned_something_new))

m_q2_glmer <- glmer(
  glmer_formula,
  family = binomial(link = "logit"),
  data = df_q2,
  control = glmer_settings
)

m_q2_ng_glmer <- glmer(
  glmer_formula_no_giver,
  family = binomial(link = "logit"),
  data = df_q2,
  control = glmer_settings
)

m_q2_brm <- brm(
  glmer_formula,
  data = df_q2,
  family = bernoulli(link = "logit"),
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

# summary(m_q2_brm)
# stanplot(m_q2_brm, type="acf")
# stanplot(m_q2_brm, type="trace")

screenreg(list(m_q2_glmer, m_q2_ng_glmer, m_q2_brm))

# ---> q3
df_q3 <- df_all %>% filter(!is.na(survey_understood_why_views))

m_q3_glmer <- glmer(
  glmer_formula,
  family = binomial(link = "logit"),
  data = df_q3,
  control = glmer_settings
)

m_q3_ng_glmer <- glmer(
  glmer_formula_no_giver,
  family = binomial(link = "logit"),
  data = df_q3,
  control = glmer_settings
)

m_q3_brm <- brm(
  glmer_formula,
  data = df_q3,
  family = bernoulli(link = "logit"),
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

# summary(m_q3_brm)
# stanplot(m_q3_brm, type="acf")
# stanplot(m_q3_brm, type="trace")

screenreg(list(m_q3_glmer, m_q3_ng_glmer, m_q3_brm))


# ---> q4
df_q4 <- df_all %>% filter(!is.na(survey_conversation_in_future))

m_q4_glmer <- glmer(
  glmer_formula,
  family = binomial(link = "logit"),
  data = df_q4,
  control = glmer_settings
)

m_q4_ng_glmer <- glmer(
  glmer_formula_no_giver,
  family = binomial(link = "logit"),
  data = df_q4,
  control = glmer_settings
)

m_q4_brm <- brm(
  glmer_formula,
  data = df_q4,
  family = bernoulli(link = "logit"),
  chains = n_chains, iter = n_iters, cores = n_cores, seed = 0,
  control = list(adapt_delta = c_adapt_delta, max_treedepth = c_max_treedepth),
  prior = priors
)

# summary(m_q4_brm)
# stanplot(m_q4_brm, type="acf")
# stanplot(m_q4_brm, type="trace")

screenreg(list(m_q4_glmer, m_q4_ng_glmer, m_q4_brm))

# print glmer models
screenreg(
  list(
    "All" = m_all_glmer,
    "Q1" = m_q1_glmer,
    "Q2" = m_q2_glmer,
    "Q3" = m_q3_glmer,
    "Q4" = m_q4_glmer 
  )
)

# print brms models
screenreg(
  list(
    "All" = m_all_brm,
    "Q1" = m_q1_brm,
    "Q2" = m_q2_brm,
    "Q3" = m_q3_brm,
    "Q4" = m_q4_brm
  )
)

# compare to the glmer models
screenreg(
  list(
    "All" = m_all_glmer, "All" = m_all_brm,
    "Q1" = m_q1_glmer, "Q1" = m_q1_brm,
    "Q2" = m_q2_glmer, "Q2" = m_q2_brm,
    "Q3" = m_q3_glmer, "Q3" = m_q3_brm,
    "Q4" = m_q4_glmer, "Q4" = m_q4_brm
  )
)

# save
save(
  m_all_ng_glmer, m_q1_ng_glmer, m_q2_ng_glmer, m_q3_ng_glmer, m_q4_ng_glmer,
  m_all_glmer,m_q1_glmer, m_q2_glmer,m_q3_glmer, m_q4_glmer,
  m_all_brm, m_q1_brm, m_q2_brm, m_q3_brm, m_q4_brm,
  file = "models/balance_regs/feed_opp.Rdata"
)


#
# Tables
#
coef_names_map <- list(
  "Intercept" = "\\textit{(Intercept)}",
  "days_active" = "\\textit{days active}",
  "num_statuses" = "\\textit{statuses count}",
  "num_favorites" = "\\textit{favorites count}",
  "num_followers" = "\\textit{followers count}",
  "num_friends" = "\\textit{friends count}"
)

model_names <- c(
  "\\textit{All}",
  "\\textit{Survey Q1}",
  "\\textit{Survey Q2}",
  "\\textit{Survey Q3}",
  "\\textit{Survey Q4}"
)

# prompt
load("models/balance_regs/prompt.Rdata")

texreg(
  list(m_all_brm, m_q1_brm, m_q2_brm, m_q3_brm, m_q4_brm),
  booktabs = T, 
  use.packages = F,
  custom.model.names = model_names,
  custom.coef.map = coef_names_map,
  label = "tab:balance-prompt"
)

# feed
load("models/balance_regs/feed_opp.Rdata")

texreg(
  list(m_all_brm, m_q1_brm, m_q2_brm, m_q3_brm, m_q4_brm),
  booktabs = T, 
  use.packages = F,
  custom.model.names = model_names,
  custom.coef.map = coef_names_map,
  label = "tab:balance-feed"
)

# Outcome Latex (add manually)
# \toprule
# & \multicolumn{5}{c}{\textbf{Outcome: Prompt Assignment}} \\
# \cmidrule(l){2-6}

# END