
library(tidyverse)
library(lme4)
library(brms)
library(broom.mixed)
library(texreg)
library(ggsci)
library(grid)
library(gtable)
library(latex2exp)
library(cowplot)


# load models
load("models/main/brms_covs.Rdata")

#
# MAIN EFFECTS 
#

# compile data
df_models <- rbind(
  # Time Spent
  tidy(ts_i_brms, effects = "fixed", conf.int = T, conf.level = 0.95) %>% 
    mutate(outcome_group = "timespent", outcome = "Time Spent"),
  # Engagement
  tidy(ne_i_brms, effects = "fixed", conf.int = T, conf.level = 0.95) %>% 
    mutate(outcome_group = "engagement", outcome = "Engagement"),
  # Q1
  tidy(q1_i_brms, effects = "fixed", conf.int = T, conf.level = 0.95) %>% 
    mutate(outcome_group = "survey", outcome = "Survey: Q1"),
  # Q2
  tidy(q2_i_brms, effects = "fixed", conf.int = T, conf.level = 0.95) %>% 
    mutate(outcome_group = "survey", outcome = "Survey: Q2"),
  # Q3
  tidy(q3_i_brms, effects = "fixed", conf.int = T, conf.level = 0.95) %>% 
    mutate(outcome_group = "survey", outcome = "Survey: Q3"),
  # Q4
  tidy(q4_i_brms, effects = "fixed", conf.int = T, conf.level = 0.95) %>% 
    mutate(outcome_group = "survey", outcome = "Survey: Q4")
)

# rename variable names and factor values
df_models <- df_models %>%
  select(
    outcome_group,
    outcome,
    exp = term,
    est = estimate,
    lb = conf.low,
    ub = conf.high
  ) %>%
  filter(exp %in% c("feed_oppTRUE", "promptempathic")) %>%
  mutate(
    exp = case_when(
      exp == "feed_oppTRUE" ~ "(A)\nFeed: Opposite",
      exp == "promptempathic" ~ "(B)\nPrompt: Empathic"
    )
  )

# plot
theme_set(theme_classic())

theme_update(
  # text = element_text(family="Roboto"),
  title = element_text(size=15),
  plot.subtitle = element_text(size=15),
  # panel => to rectangle around the plot area
  panel.border = element_rect(colour = "grey", fill=NA, size=0.8),
  # lines along x (bottom only) and y (left only)
  axis.line = element_blank(),
  # facet lables
  strip.background = element_blank(),
  strip.text.x = element_blank(),
  # axis 
  axis.title.x = element_text(colour = "grey30", size=10),
  axis.title.y = element_blank(),
  axis.text.y = element_text(colour = "black", size=11)
)

p_1 <- df_models %>% filter(outcome_group=="timespent") %>%
  ggplot(aes(x=est, y=outcome)) +
  geom_vline(xintercept=0, alpha=0.5) +
  geom_errorbarh(aes(xmin=lb, xmax=ub), height=0, alpha=0.5, size=1) + 
  geom_point(stat='identity', fill="black", size=4) + 
  xlab(TeX("$\\Delta$ seconds")) +
  facet_grid(~ exp) +
  theme(
    strip.text.x = element_text(
      face="bold",
      size=12, 
      margin=margin(t=7, r=0, b=10, l=0, unit = "pt")
    )
  ) + 
  xlim(-22.1, 42)

p_2 <- df_models %>% filter(outcome_group=="engagement") %>%
  ggplot(aes(x=est, y=outcome)) +
  geom_vline(xintercept=0, alpha=0.5) +
  geom_errorbarh(aes(xmin=lb, xmax=ub), height=0, alpha=0.5, size=1) + 
  geom_point(stat='identity', fill="black", size=4) + 
  xlab(TeX("$\\Delta$ number of engagements")) + 
  facet_grid(~ exp) + 
  xlim(-0.092, 0.175)

p_3 <- df_models %>% filter(outcome_group=="survey") %>%
  ggplot(aes(x=est, y=outcome)) +
  geom_vline(xintercept=0, alpha=0.5) +
  geom_errorbarh(aes(xmin=lb, xmax=ub), height=0, alpha=0.5, size=1) + 
  geom_point(stat='identity', fill="black", size=4) +
  xlab(TeX("$\\Delta$ response")) + 
  facet_grid(~ exp) +
  scale_y_discrete(limits = c("Survey: Q4", "Survey: Q3", "Survey: Q2", "Survey: Q1"))+ 
  xlim(-24.4, 42)

# arrange then in a grid
g_1 <- ggplotGrob(p_1)
g_2 <- ggplotGrob(p_2)
g_3 <- ggplotGrob(p_3)

g_1$heights[8] <- unit(2, "null")
g_2$heights[8] <- unit(2, "null")
g_3$heights[8] <- unit(10, "null")

g <- rbind(g_1, g_2, g_3, size="max")

grid.newpage()
grid.draw(g)

ggsave(
  g, 
  filename="plots/main_effects.pdf", 
  dpi = 300, 
  device = cairo_pdf,
  width = 30, 
  height = 10, 
  units = "cm"
)

#
# INTERACTIONS
#

# compile data
df_ints <- rbind(
  # Time Spent
  marginal_effects(ts_i_brms, effects="feed_opp:prompt", robust=F, probs=c(0.025, 0.975))[[1]] %>%
    mutate(outcome="timespent") %>% select(-timespent),
  # Engagement
  marginal_effects(ne_i_brms, effects="feed_opp:prompt", robust=F, probs=c(0.025, 0.975))[[1]] %>%
    mutate(outcome="engagement") %>% select(-n_engagements),
  # Q1
  marginal_effects(q1_i_brms, effects="feed_opp:prompt", robust=F, probs=c(0.025, 0.975))[[1]] %>%
    mutate(outcome="q1") %>% select(-survey_feed_difference),
  # Q2
  marginal_effects(q2_i_brms, effects="feed_opp:prompt", robust=F, probs=c(0.025, 0.975))[[1]] %>%
    mutate(outcome="q2") %>% select(-survey_learned_something_new),
  # Q3
  marginal_effects(q3_i_brms, effects="feed_opp:prompt", robust=F, probs=c(0.025, 0.975))[[1]] %>%
    mutate(outcome="q3") %>% select(-survey_understood_why_views),
  # Q4
  marginal_effects(q4_i_brms, effects="feed_opp:prompt", robust=F, probs=c(0.025, 0.975))[[1]] %>%
    mutate(outcome="q4") %>% select(-survey_conversation_in_future)
)

df_ints <- df_ints %>%
  select(
    outcome,
    feed_opp,
    prompt,
    est = estimate__,
    lb = lower__,
    ub = upper__
  ) %>% 
  mutate(
    feed_opp=recode(feed_opp,  "TRUE"="Opposite", "FALSE"="Same"),
    prompt=recode(prompt, "empathic"="Empathic", "control"="Control"),
    prompt=factor(prompt, levels=c("Empathic", "Control"))
  )

# theme setup
theme_set(theme_classic())
theme_update(
  # text = element_text(family="Roboto"),
  plot.title = element_text(size=13, face="bold", hjust = 0.5),
  axis.line = element_blank(),
  panel.border = element_rect(colour = "grey", fill=NA, size=0.8),
  axis.title = element_text(size=12, color="black", face="plain"),
  axis.text.x = element_text(size=10, color="black", face="plain"),
  axis.text.y = element_text(size=10, color="black", face="plain"),
  legend.position = "bottom", 
  legend.box = "horizontal",
  legend.title = element_text(size=15, color="black", face="plain"),
  legend.text = element_text(size=13, color="black")
)

p_int_colors <- pal_npg()(4)[c(3,4)]
p_int_shapes <- c(15, 19)

# plot util function
make_interactions_plot <- function(df, title, y_label, n_breaks=11, y_max=NA) {
  ggplot(df, aes(feed_opp, est, ymin=lb, ymax=ub, color=prompt)) +
    geom_line(aes(group =prompt), position = position_dodge(width = 0.25)) +
    geom_pointrange(aes(shape=prompt), 
                    position=position_dodge(width = 0.25),
                    size=0.6) +
    scale_shape_manual(values=p_int_shapes) +
    scale_colour_manual(values=p_int_colors) +
    scale_y_continuous(
      breaks=scales::pretty_breaks(n_breaks), 
      limits = c(0, y_max)
    ) +
    labs(
      title = title,
      x = "Feed",
      y = y_label,
      color = "Prompt",
      shape = "Prompt"
    )  
}

plt_i_ts <- df_ints %>% 
  filter(outcome == "timespent") %>% 
  make_interactions_plot(
    title = "(A) Time Spent", 
    y_label = "Seconds",
    n_breaks = 11, 
    y_max = NA
  )

plt_i_en <- df_ints %>% 
  filter(outcome == "engagement") %>% 
  make_interactions_plot(
    title = "(B) Engagement", 
    y_label = "Number of Engagements",
    n_breaks = 8, 
    y_max = NA
  )

plt_i_q1 <- df_ints %>% 
  filter(outcome == "q1") %>%
  make_interactions_plot(
    title = "(C) Survey: Q1", 
    y_label = "Response",
    n_breaks = 11, 
    y_max = 100
  )

plt_i_q2 <- df_ints %>% 
  filter(outcome == "q2") %>%
  make_interactions_plot(
    title = "(D) Survey: Q2", 
    y_label = "Response",
    n_breaks = 11, 
    y_max = 100
  )

plt_i_q3 <- df_ints %>% 
  filter(outcome == "q3") %>%
  make_interactions_plot(
    title = "(E) Survey: Q3", 
    y_label = "Response",
    n_breaks = 11, 
    y_max = 100
  )

plt_i_q4 <- df_ints %>% 
  filter(outcome == "q4") %>%
  make_interactions_plot(
    title = "(F) Survey: Q4", 
    y_label = "Response",
    n_breaks = 11, 
    y_max = 100
  )

# plt_ints <- ggpubr::ggarrange(
#   plt_i_ts, plt_i_en, plt_i_q1, plt_i_q2, plt_i_q3, plt_i_q4,
#   nrow = 1, ncol = 6,
#   common.legend = TRUE, 
#   legend = "bottom"
# )

plt_ints <- plot_grid(
  plt_i_ts, plt_i_en, plt_i_q1, plt_i_q2, plt_i_q3, plt_i_q4,
  nrow = 1, 
  ncol = 6
)

ggsave(
  plt_ints, 
  filename="plots/interaction_effects.pdf", 
  dpi = 300, 
  device = cairo_pdf,
  width = 35, 
  height = 12, 
  units = "cm"
)

# END