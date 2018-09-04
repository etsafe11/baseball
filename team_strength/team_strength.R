setwd("C:/Users/ethompson/Desktop/baseball/team_strength/game_logs")

library(tidyverse)
library(lubridate)
library(lme4)
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(rlang)
library(lme4)
library(scales)
library(ggplot2)
library(grDevices)
library(bit)

#Getting list of file names 
file_name <- list.files(pattern="gl") 
mydata <- lapply (file_name, read.table, sep=",", header=F, row.names=NULL) 
# rbind to dataframe 
game_logs <- do.call(rbind, mydata)

df_raw <-
  game_logs %>%
    mutate(matchID = 1:nrow(game_logs),
           date = ymd(V1),
           home_team = as.character(V7),
           home_league = as.character(V8),
           away_team = as.character(V4),
           away_league = as.character(V5),
           home_score = V11,
           away_score = V10,
           neutral = rep(FALSE, nrow(game_logs))
    ) %>%
    select(matchID, date, home_team, home_league, away_team, away_league, home_score, away_score, neutral)

#matchID <- 1:nrow(df_raw)
#df_raw <- data.frame(matchID, df_raw) 

df_join <- 
  df_raw %>% 
  select(home_team, away_team, matchID) %>% 
  gather(key = type, value = key_team, -matchID)

df_processed <- 
  df_raw %>% 
  mutate(year = year(date))

df_augmented <- 
  df_join %>% 
  left_join(df_processed) %>% 
  mutate(result = case_when(
    type == "home_team" & home_score > away_score ~ "win",
    type == "home_team" & home_score < away_score ~ "loss",
    type == "away_team" & away_score > home_score ~ "win",
    type == "away_team" & away_score < home_score ~ "loss"
  )) %>% 
  mutate(
    runs_for     = ifelse(type == "home_team", home_score, away_score),
    runs_against = ifelse(type == "home_team", away_score, home_score)
  ) %>% 
  mutate(
    tmp = 1
  ) %>% 
  spread(result, tmp, fill = 0)

df_summarized <- 
  df_augmented %>%
  group_by(year, key_team) %>% 
  summarize_at(vars(win, loss, runs_for, runs_against), sum) 

write_csv(df_summarized, str_c(today(), "_summarized-yearly-results.csv"))


# Model input dataset - since 1961 only
df_model <- 
  df_augmented %>% 
  mutate(
    team = ifelse(type == "home_team", home_team, away_team),
    runs = ifelse(type == "home_team", home_score, away_score),
    opponent = ifelse(type == "home_team", away_team, home_team),
    type = ifelse(neutral == TRUE, "neutral", type),
    home_team = ifelse(neutral == TRUE, "none", home_team)
  ) %>% 
  filter(year >= 1961) %>%
  select(year, team, runs, opponent, type, home_team)

write_csv(df_model, str_c(today(), "_df_model.csv"))


### Model 1 - home/away as nested random effect within team ### 

# Run overnight:

model_nb <- 
  glmer.nb(runs ~ (1 | team/year) + (1 | opponent/year) + (1 | type/home_team), 
           df_model)

save(model_nb, file = str_c(today(), "_model-nb.rda"))



load("2018-09-03_model-nb.rda")

# model 1 results - does not control for home/away effect 

model_coef <- coef(model_nb)

# residuals checking
model_predict <- predict(model_nb, type = "response")
plot(model_predict, df_model$goals)
plot(model_predict, model_predict - df_model$goals)

# best intrinsic national goodness over all history
(model_coef$team - model_coef$opponent) %>% 
  as.data.frame %>% 
  setNames("val") %>% 
  mutate(team = rownames(.)) %>% 
  arrange(desc(val)) %>% 
  head(20)

# munging coefficients

model_coef_offense <- 
  model_coef$team %>% 
  as.data.frame %>% 
  set_names("offense") %>% 
  mutate(team = rownames(.))

model_coef_defense <- 
  model_coef$opponent %>% 
  as.data.frame %>% 
  set_names("defense") %>% 
  mutate(team = rownames(.))

model_coef_offense_year <- 
  model_coef$`year:team` %>% 
  as.data.frame %>% 
  set_names("offense_year") %>% 
  mutate(team_year = rownames(.)) %>% 
  separate(team_year, c("year", "team"), sep = ":")

model_coef_defense_year <- 
  model_coef$`year:opponent` %>% 
  as.data.frame %>% 
  set_names("defense_year") %>% 
  mutate(team_year = rownames(.)) %>% 
  separate(team_year, c("year", "team"), sep = ":")

# rolling into one
model_coef_comparable <- 
  model_coef_defense_year %>% 
  left_join(model_coef_offense_year) %>% 
  left_join(model_coef_offense) %>% 
  left_join(model_coef_defense) %>% 
  mutate(overall_score = offense + offense_year - (defense + defense_year)) %>%
  na.omit()

# best 20 team-seasons of all time
model_coef_comparable %>% 
  arrange(desc(overall_score)) %>% 
  head(20)

# worst 10 team-seasons of all time
model_coef_comparable %>% 
  arrange(desc(overall_score)) %>% 
  tail(10)

# best teams in 2017
model_coef_comparable %>% 
  filter(year == 2017) %>% 
  arrange(desc(overall_score)) %>% 
  head

# how correlated are offense and defensive skill?
with(model_coef_comparable, cor(offense + offense_year, defense + defense_year))
# very.
# but it's because good teams are good overall
with(model_coef_comparable, cor(offense, defense))
# in a given year, offense and defense vary independently
with(model_coef_comparable, cor(offense_year, defense_year))


# defensive skill is 60% more variable year to year overall
with(model_coef_comparable, sd(offense_year))
with(model_coef_comparable, sd(defense_year))

# team-to-team variation much larger than year-to-year variation
with(model_coef_comparable, sd(offense))
with(model_coef_comparable, sd(defense))

# by about 5.7x

model_coef_sd <-
  model_coef_comparable %>% 
  group_by(team) %>% 
  mutate(sd_offense = sd(offense_year),
         sd_defense = sd(defense_year)) %>% 
  summarize(avg_score = mean(overall_score), sd_offense = first(sd_offense),
            sd_defense = first(sd_defense))

model_coef_sd_gathered <- 
  model_coef_sd %>% 
  gather(key = skill, value = value, -avg_score)


# better teams have less variable defense and more variable offense year to year
ggplot(model_coef_sd_gathered) +
  aes(x = avg_score, y = value, color = skill) + 
  geom_point() + 
  stat_smooth(method = 'gam')




