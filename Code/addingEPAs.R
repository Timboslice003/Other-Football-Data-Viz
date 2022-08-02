library(nflfastR)
library(tidyverse)
library(ggimage)
library(nflplotR)
team_info <- nflreadr::load_teams()

pbp <- load_pbp(2015:2021)
pbp2021 <- load_pbp(2021)

off_epa <- pbp2021 %>%
  filter(special_teams_play == 0,
         !is.na(epa), !is.na(posteam),
         season_type == 'REG') %>%
  group_by(posteam) %>%
  summarise(epa_play = mean(epa)) %>%
  arrange(-epa_play)

def_epa <- pbp2021 %>%
  filter(special_teams_play == 0,
         !is.na(epa), !is.na(defteam),
         season_type == 'REG') %>%
  group_by(defteam) %>%
  summarise(epa_play = mean(epa)) %>%
  arrange(-epa_play)

off_epa %>% ggplot(aes(x = fct_reorder(posteam, -epa_play), y = epa_play)) +
  geom_col()

def_epa %>% ggplot(aes(x = fct_reorder(defteam, -(epa_play*-1)), y = epa_play*-1)) +
  geom_col()

total_epa <- off_epa %>%
  left_join(def_epa, by = c('posteam' = 'defteam')) %>%
  rename(epa_O = epa_play.x, epa_D = epa_play.y) %>%
  mutate(epa_tot = epa_O*1.5 + (epa_D*-1)) %>%
  arrange(-epa_tot)

total_epa %>% ggplot(aes(fct_reorder(posteam, -epa_tot), y = epa_tot)) +
  geom_col()

total_epa <- total_epa %>%
  mutate(ranking = round((epa_tot-min(epa_tot))/(max(epa_tot)-min(epa_tot)) *100, 1)) %>%
  mutate(ranking_row = as.factor(rank(-epa_tot))) %>%
  left_join(team_info, by = c('posteam' = 'team_abbr'))

total_epa %>% 
  ggplot(aes(fct_reorder(ranking_row, -ranking), y = ranking)) +
  geom_col(fill = total_epa$team_color, color = total_epa$team_color2,
           alpha =ifelse(total_epa$posteam %in% c('CIN','LA'), 1,.2)) +
  geom_nfl_logos(aes(team_abbr = posteam), width = .04, alpha = ifelse(total_epa$posteam %in% c('CIN','LA'), 1,.6)) +
  ggthemes::theme_economist_white() +
  labs(title = 'NFL Teams\' Relative Performance',
       subtitle = '2021 Regular Season',
       caption = 'Data: NFLFastR | Plot: @Timboslice003',
       x = ' League Ranking',
       y = 'Relative Performance') +
  theme(plot.title = element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = .5))

ggsave('combined_epa.png', width = 14, height = 10, dpi = 'retina')  
       
  

  
  