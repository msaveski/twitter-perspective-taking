
library(tidyverse)
library(lme4)
library(brms)
library(texreg)
library(broom.mixed)


# load utils
source("code/utils.R")

# helper functions
list_lmer <- function() {
  list(
    ts = ts_lmer, ts_i = ts_i_lmer,
    ne = ne_lmer, ne_i = ne_i_lmer,
    q1 = q1_lmer, q1_i = q1_i_lmer,
    q2 = q2_lmer, q2_i = q2_i_lmer,
    q3 = q3_lmer, q3_i = q3_i_lmer,
    q4 = q4_lmer, q4_i = q4_i_lmer
  )
}

list_brms <- function() {
  list(
    ts = ts_brms, ts_i = ts_i_brms,
    ne = ne_brms, ne_i = ne_i_brms,
    q1 = q1_brms, q1_i = q1_i_brms,
    q2 = q2_brms, q2_i = q2_i_brms,
    q3 = q3_brms, q3_i = q3_i_brms,
    q4 = q4_brms, q4_i = q4_i_brms
  )
}

extract_lmer_tab <- function(model) {
  m_tidy <- tidy(
    model,
    effects = "fixed",
    conf.int = T,
    conf.level = 0.95,
    conf.method = "Wald"
  ) %>%
    mutate(
      term = if_else(term == "(Intercept)", "Intercept", term)
    )
  
  gof <- c(nobs(model))
  gof.names <- c("Num.\\ obs.")
  
  createTexreg(
    coef.names = m_tidy$term,
    coef = m_tidy$estimate,
    ci.low = m_tidy$conf.low,
    ci.up = m_tidy$conf.high,
    gof.names = gof.names,
    gof = gof
  )
}

setMethod("extract",
          signature = className("lmerMod"),
          definition = extract_lmer_tab)


extract_brms_tab <- function(model) {
  m_tidy <- 
    tidy(model, effects = "fixed", conf.int = T, conf.level = 0.95) %>%
    mutate(term = if_else(term == "(Intercept)", "Intercept", term))
  
  gof <- c(nobs(model))
  gof.names <- c("Num.\\ obs.")
  
  createTexreg(
    coef.names = m_tidy$term,
    coef = m_tidy$estimate,
    ci.low = m_tidy$conf.low,
    ci.up = m_tidy$conf.high,
    gof.names = gof.names,
    gof = gof
  )
}

setMethod("extract",
          signature = className("brmsfit",  "brms"),
          definition = extract_brms_tab)


#
# load data 
#

# lmer
load("models/main/lmer.Rdata")
ls_lmer <- list_lmer()

# lmer covs
load("models/main/lmer_covs.Rdata")
ls_lmer_covs <- list_lmer()

# brms
load("models/main/brms.Rdata")
ls_brms <- list_brms()

# brms covs
load("models/main/brms_covs.Rdata")
ls_brms_covs <- list_brms()


#
# make tables
#
coef_names_map <- list(
  "Intercept" = "\\textit{(Intercept)}",
  "feed_oppTRUE" = "\\textit{feed=opp}",
  "promptempathic" = "\\textit{prompt=emp}",
  "feed_oppTRUE:promptempathic" = "\\textit{feed=opp $\\times$ prompt=emp}",
  "days_active" = "\\textit{days active}",
  "num_statuses" = "\\textit{statuses count}",
  "num_favorites" = "\\textit{favorites count}",
  "num_followers" = "\\textit{followers count}",
  "num_friends" = "\\textit{friends count}",
  "feed_oppTRUE:days_active" = "\\textit{feed=opp $\\times$ days active}", 
  "feed_oppTRUE:num_statuses" = "\\textit{feed=opp $\\times$ statuses count}", 
  "feed_oppTRUE:num_favorites" = "\\textit{feed=opp $\\times$ favorites count}", 
  "feed_oppTRUE:num_followers" = "\\textit{feed=opp $\\times$ followers count}", 
  "feed_oppTRUE:num_friends" = "\\textit{feed=opp $\\times$ friends count}", 
  "promptempathic:days_active" = "\\textit{prompt=emp $\\times$ days active}", 
  "promptempathic:num_statuses" = "\\textit{prompt=emp $\\times$ statuses count}", 
  "promptempathic:num_favorites" = "\\textit{prompt=emp $\\times$ favorites count}", 
  "promptempathic:num_followers" = "\\textit{prompt=emp $\\times$ followers count}", 
  "promptempathic:num_friends" = "\\textit{prompt=emp $\\times$ friends count}"
)

model_names <- c(
  "\\textit{BRMS}",
  "\\textit{\\makecell{BRMS \\\\ w/ interactions}}",  
  "\\textit{LMER}",
  "\\textit{\\makecell{LMER \\\\ w/ interactions}}"
)

caption <- "Results of the analysis of \\textit{%s} using different model specifications: with vs. without a treatments interaction term, and using BRMS vs. LMER."


make_latex_table <- function(brms_list, lmer_list, outcome, outcome_name, label) {
  outcome_i <- str_c(outcome, "_i")
  tab <- texreg(
    list(
      m_brms = brms_list[[outcome]],
      m_brms_i = brms_list[[outcome_i]],
      m_lmer = lmer_list[[outcome]],
      m_lmer_i = lmer_list[[outcome_i]]
    ),
    booktabs = T, 
    use.packages = F,
    custom.model.names = model_names,
    custom.coef.map = coef_names_map,
    caption = sprintf(caption, outcome_name),
    label = sprintf("tab:robustness-%s", label)
  )
  return(tab)
}

# Time Spent
tab_ts <- make_latex_table(ls_brms, ls_lmer, "ts", "Time Spent", "ts")
tab_ts_cov <- make_latex_table(ls_brms_covs, ls_lmer_covs, "ts", "Time Spent", "ts-covs")

# Engagement
tab_ne <- make_latex_table(ls_brms, ls_lmer, "ne", "Engagement", "ne")
tab_ne_cov <- make_latex_table(ls_brms_covs, ls_lmer_covs, "ne", "Engagement", "ne-covs")

# Q1
tab_q1 <- make_latex_table(ls_brms, ls_lmer, "q1", "Survey Question 1", "q1")
tab_q1_cov <- make_latex_table(ls_brms_covs, ls_lmer_covs, "q1", "Survey Question 1", "q1-covs")

# Q2
tab_q2 <- make_latex_table(ls_brms, ls_lmer, "q2", "Survey Question 2", "q2")
tab_q2_cov <- make_latex_table(ls_brms_covs, ls_lmer_covs, "q2", "Survey Question 2", "q2-covs")

# Q3
tab_q3 <- make_latex_table(ls_brms, ls_lmer, "q3", "Survey Question 3", "q3")
tab_q3_cov <- make_latex_table(ls_brms_covs, ls_lmer_covs, "q3", "Survey Question 3", "q3-covs")

# Q4
tab_q4 <- make_latex_table(ls_brms, ls_lmer, "q4", "Survey Question 4", "q4")
tab_q4_cov <- make_latex_table(ls_brms_covs, ls_lmer_covs, "q4", "Survey Question 4", "q4-covs")


cat(
  "% === Time Spent", tab_ts, tab_ts_cov,
  "% === Engagement", tab_ne, tab_ne_cov,
  "% === Q1", tab_q1, tab_q1_cov, 
  "% === Q2", tab_q2, tab_q2_cov, 
  "% === Q3", tab_q3, tab_q3_cov, 
  "% === Q4", tab_q4, tab_q4_cov, 
  sep = "\n",
  file = "plots/robustness_tables.txt"
)

# Outcome Latex (add manually)
# \\toprule
#  & \\multicolumn{4}{c}{\\textbf{Outcome: Timespent (Seconds)}} \\
# \\cmidrule{2-5}


# END