# Load packages
library(tidyverse)
library(nflfastR)
library(ggthemes)
library(ranger)
library(vip)
library(caret)
library(xgboost)
library(ggimage)
options(scipen = 9999)

# Make custom package
theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.caption = element_text(size = 16),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14)
    )
}

# Load in play-by-play data
pbp <- load_pbp(2015:2022)

# Check what type of plays happen on 4th down
pbp %>% 
  filter(down == 4) %>% 
  group_by(play_type) %>% 
  tally(sort = T)

# Get 4th downs
fourth_downs <- pbp %>%
  filter(down == 4, !play_type %in% c("no_play", "qb_kneel", NA)) %>%
  mutate(went_for_it = ifelse(play_type %in% c("pass", "run"), 1, 0)) %>%
  select(posteam, defteam, home_team, season, week, game_id, play_id, desc, 
         play_type, down, yardline_100, ydstogo, half_seconds_remaining, wp, 
         wpa, score_differential, ep, epa, temp, wind, went_for_it) %>%
  filter(!is.na(epa))

# Check for NA's
colSums(is.na(fourth_downs))

fourth_downs <- fourth_downs %>%
  mutate(temp = ifelse(is.na(temp), 70, temp),
         wind = ifelse(is.na(wind), 0, wind))

# Select the data wanted for the model
model_data <- fourth_downs %>%
  select(went_for_it, yardline_100, ydstogo, half_seconds_remaining, wp,
         score_differential, ep, temp, wind, season)

# Build out random forest (using the '.' after the '~' here let's me predict 
# went_for_it using all the other variables above without having to type them out)
# using impurity here since we want rf_4th to be a 1 or 0
rf_4th <- ranger(went_for_it ~ ., data = model_data, 
                 num.trees = 100, importance = "impurity")

# Check variable importance of random forest
vip(rf_4th) + theme_reach()

# Make a grid for tuning
dim(model_data)
rf_grid <- expand.grid(mtry = seq(2, 8, by = 1), 
                       splitrule = "variance",
                       min.node.size = 5) # For classification

# Use the tuning grid
rf_4th_tune <- 
  train(went_for_it ~ ., data = model_data,
        method = "ranger", num.trees = 100,
        trControl = trainControl(method = "cv", number = 5),
        tuneGrid = rf_grid)

# Get the results from the best tune
rf_4th_tune$bestTune

# Remake random forest with tuning parameters
rf_4th_best <- ranger(went_for_it ~ ., data = model_data, 
                      num.trees = 100, importance = "impurity",
                      mtry = 5)

# Get predictions
rf_preds <- data.frame(predict(rf_4th_best, data.frame(model_data))$predictions) 

names(rf_preds)

rf_preds <- rf_preds %>%
  rename(exp_go = predict.rf_4th_best..data.frame.model_data...predictions)

# Bind the original dataset and predictions together
fourth_downs_rf_projs <- cbind(fourth_downs, rf_preds)

fourth_downs_rf_projs <- fourth_downs_rf_projs %>%
  mutate(go_over_expected = went_for_it - exp_go)

# Check 2021 stats
rf_team_stats <- fourth_downs_rf_projs %>%
  filter(season == 2021) %>%
  group_by(posteam) %>%
  summarize(avg_gooe = 100*mean(go_over_expected),
            wpa = 100*mean(wpa)) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# Make graph
rf_team_stats %>%
  ggplot(aes(x = avg_gooe, y = wpa)) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_reach() +
  labs(x = "Go For It Rate Over Expected",
       y = "WPA Added on 4th Downs",
       title = "Go For It Rate Over Expected and WPA on 4th Downs in 2021",
       subtitle = "WPA = Win Probability Added",
       caption = "By Santino Parlato @santinparlato")
ggsave('go-oe.png', width = 15, height = 10, dpi = "retina")