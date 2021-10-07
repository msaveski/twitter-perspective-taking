
library(tidyverse)
library(lme4)
library(brms)
library(texreg)
library(broom.mixed)
library(ggsci)
library(cowplot)


# load utils
source("code/utils.R")

#
# helper functions
#
tidy_lmer <- function(model) {
  tidy(
    model,
    effects = "fixed",
    conf.int = T,
    conf.level = 0.95,
    conf.method = "Wald"
  )
}

tidy_brms <- function(model) {
  tidy(
    model,
    effects = "fixed",
    conf.int = T,
    conf.level = 0.95
  )
}

process_lmer <- function() {
  tribble(
      ~outcome, ~ints, ~model,
      "Timespent", F, ts_lmer, "Timespent", T, ts_i_lmer,
      "Engagement", F, ne_lmer, "Engagement", T, ne_i_lmer,
      "Q1", F, q1_lmer, "Q1", T, q1_i_lmer,
      "Q2", F, q2_lmer, "Q2", T, q2_i_lmer,
      "Q3", F, q3_lmer, "Q3", T, q3_i_lmer,
      "Q4", F, q4_lmer, "Q4", T, q4_i_lmer
    ) %>%
    mutate(model_tidy = map(model, tidy_lmer)) %>%
    unnest(model_tidy) %>%
    select(-model, -effect, -statistic)
}

process_brms <- function() {
  tribble(
      ~outcome, ~ints, ~model,
      "Timespent", F, ts_brms, "Timespent", T, ts_i_brms,
      "Engagement", F, ne_brms, "Engagement", T, ne_i_brms,
      "Q1", F, q1_brms, "Q1", T, q1_i_brms,
      "Q2", F, q2_brms, "Q2", T, q2_i_brms,
      "Q3", F, q3_brms, "Q3", T, q3_i_brms,
      "Q4", F, q4_brms, "Q4", T, q4_i_brms
    ) %>%
    mutate(model_tidy = map(model, tidy_brms)) %>%
    unnest(model_tidy) %>%
    select(-model, -effect, -component)
}

tab_lmer <- function() {
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
}

tab_brms <- function() {
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
}


# lmer
load("models/main/lmer.Rdata")

df_lmer <- process_lmer() %>% 
  mutate(
    name = "LMER",
    model_type = "LMER",
    covs = F
  )

# tab_lmer()

# lmer covs
load("models/main/lmer_covs.Rdata")

df_lmer_covs <- process_lmer() %>% 
  mutate(
    name = "LMER w/ covariates",
    model_type = "LMER",
    covs = T
  )

# tab_lmer()


# brms
load("models/main/brms.Rdata")

df_brms <- process_brms() %>%
  mutate(
    name = "BRMS",
    model_type = "BRMS",
    covs = F
  )

# tab_brms()


# brms covs
load("models/main/brms_covs.Rdata")

df_brms_covs <- process_brms() %>%
  mutate(
    name = "BRMS w/ covariates",
    model_type = "BRMS",
    covs = T
  )

# tab_brms()


#
# stack all
#
df <- rbind(
    df_lmer, 
    df_lmer_covs,
    df_brms,
    df_brms_covs
  ) %>%
  filter(term %in% c("(Intercept)", "feed_oppTRUE", "promptempathic")) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "feed_oppTRUE" ~ "Feed: Opposite",
      term == "promptempathic" ~ "Prompt: Empathic"
    ),
    term = factor(term, levels = c("Intercept", "Feed: Opposite", "Prompt: Empathic")), 
    name = if_else(ints, str_c(name, " (w/ ints)"), name),
    name = factor(
      name, 
      levels = rev(c(
        "BRMS w/ covariates (w/ ints)", 
        "LMER w/ covariates (w/ ints)", 
        "BRMS (w/ ints)", 
        "LMER (w/ ints)",
        "BRMS w/ covariates",   
        "BRMS", 
        "LMER w/ covariates", 
        "LMER"
      ))
    ),
    color = if_else(name == "BRMS w/ covariates (w/ ints)", "black", "grey50")
  )


#
# plot
#
make_plot <- function(df) {
  df %>%
    ggplot(aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = name,
      color = color
    )) +
    geom_vline(xintercept = 0, size = 0.3, linetype = "dashed", color = "red") +
    geom_errorbarh(height = 0.0, size = 0.5) +
    geom_point(size = 2) +
    facet_grid(outcome ~ term, scales = "free_x") + 
    labs(x = "Estimate", y = NULL, color = "Model Type") + 
    scale_color_manual(values=c("black", "grey65")) + 
    theme_bw() + 
    theme(
      panel.grid.minor.x = element_blank(),
      legend.position = "none"
    )
}


plt_ts <- df %>% filter(outcome == "Timespent") %>% make_plot()

plt_ne <- df %>% filter(outcome == "Engagement") %>% make_plot()

plt_qs <- df %>% filter(outcome %in% c("Q1", "Q2", "Q3", "Q4")) %>% make_plot()

# arrange
plts <- plot_grid(
  plt_ts, plt_ne, plt_qs,
  nrow = 3, ncol = 1,
  rel_heights = c(1, 1, 3)
)

# print(plts)

# save plot and data
ggsave("plots/robustness_plot.pdf", plot = plts, width = 8, height = 10)

# END