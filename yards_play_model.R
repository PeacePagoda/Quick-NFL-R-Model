library(nflfastR)
library(tidyverse)

pbp <- load_pbp(1999:2019)

features <- pbp %>%
  filter(rush == 1 | pass == 1, season_type == "REG", !is.na(epa), !is.na(posteam), posteam != "", !is.na(temp)) %>%
  select(half_seconds_remaining, game_seconds_remaining, total, down, goal_to_go, yardline_100, shotgun, no_huddle, score_differential, vegas_wpa, wpa, posteam_score, defteam_score, temp, pass, rush, yards_gained)

features <- features %>%
  mutate(
    fg_range = case_when(
      yardline_100 > 38 ~ 0,
      yardline_100 <= 38 ~ 1,
    )
  )

features %>%  
  cor(use="complete.obs") %>%
  round(2)

smp_size <- floor(0.75 * nrow(features))

set.seed(123)
train_ind <- sample(seq_len(nrow(features)), size = smp_size)

train <- features[train_ind, ]
test <- features[-train_ind, ]

fit <- lm(yards_gained ~ half_seconds_remaining + game_seconds_remaining + total + down + goal_to_go + yardline_100 + shotgun + no_huddle + score_differential + vegas_wpa + wpa + posteam_score + defteam_score + pass + rush, data = train)

summary(fit)
