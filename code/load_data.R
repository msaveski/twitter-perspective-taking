#
# Load data
#
library(tidyverse)


df_sessions <- read_csv("data/sessions.csv", col_types = "cccccdddddd")

df_alignments <- read_csv("data/alignments.csv", col_types = "cc")

df_covariates <- read_csv("data/covariates.csv", col_types = "cddddd")

# join: sessions x alignments x covariates
df <- df_sessions %>% 
  inner_join(df_alignments, by = "user") %>% 
  inner_join(df_covariates, by = "user") 

# make sure some are factors
df <- df %>%
  mutate(
    user = factor(user),
    ideology = factor(ideology),
    feed_opp = factor(feed != ideology)
  )

# rm intermediate data frames
rm(df_sessions, df_alignments)

# END