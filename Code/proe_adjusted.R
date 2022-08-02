library(tidyverse)
library(ggplot2)
library(cowplot)

#Get Schedules and clean to get one row per team per game
schedule_loaded <- nflreadr::load_schedules(2021)
schedule_clean <- nflreadr::clean_homeaway(schedule_loaded)
schedule <- schedule_clean %>%
  filter(game_type == 'REG') %>%
  select(game_id, week, team, opponent)

#Get team defenses proe for the season
pbp_loaded <- nflreadr::load_pbp(2021)
def_proe <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe)) %>%
  group_by(defteam) %>%
  summarise(def_proe = mean(pass_oe),
            def_prex = mean(xpass)*100,
            def_pr = def_prex + def_proe)

#Get offenses proe for every game
off_proe_total <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         !is.na(xpass),
         special_teams_play == 0) %>%
  group_by(posteam, week) %>%
  summarise(off_proe = mean(pass_oe),
            off_prex = mean(xpass)*100,
            off_pr = off_prex + off_proe)

#Get team info for data viz
info_load <- nflreadr::load_teams()

#Gather all data
df <- schedule %>%
  left_join(off_proe_total, by = c('team' = 'posteam', 'week')) %>%
  left_join(def_proe, by = c('opponent' = 'defteam')) %>%
  group_by(team) %>%
  mutate(game = row_number(week)) %>%
  left_join(info_load, by = c('team' = 'team_abbr'))

#Create Plot with easy team switch
#pass rate vs expected
my_team <- 'DAL'
low_week = 1
high_week = 18
my_df <- df %>% filter(team == my_team,
                       week >= low_week,
                       week <= high_week)
my_color <- unique(my_df$team_color)
my_name <- unique(my_df$team_name)
my_df %>% ggplot(aes(x = game)) +
  geom_path(aes(y = off_prex), linetype = 'dashed',
            color = 'grey40', size = 1.35) +
  geom_path(aes(y = off_pr, color = team), size = 1.35) +
  nflplotR::scale_color_nfl(type = 'primary') +
  nflplotR::geom_nfl_logos(aes(y = off_pr, team_abbr = team), height = .11) + 
  theme_minimal() +
  scale_x_continuous(breaks = min(my_df$game):max(my_df$game), labels = my_df$opponent) +
  labs(x = 'Opponent',
       y = 'Pass Rate %',
       title = paste0(my_name,'\' Pass Rate Compared to Expected Pass Rate | By Week'),
       subtitle = 'Dotted line indicates expected pass rate',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  theme(plot.title = element_text(color = my_color,size = 24, face = 'bold'),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 17, face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
        panel.grid.minor = element_blank(),
        axis.text.x = nflplotR::element_nfl_logo(size = 1.2),
        plot.background = element_rect('white'),
        axis.line = element_line(),
        plot.caption = element_text(size = 13, face = 'bold', color = 'black')) -> p1

ggsave('DAL_proe.png', plot = p1, width = 14, height = 10, dpi = 'retina') 
  
#Create Plot with easy team switch
#proe vs opponent's seasonal proe
my_team = 'KC'
my_df <- df %>% filter(team == my_team)
my_color <- unique(my_df$team_color)
my_name <- unique(my_df$team_name)
my_df %>% ggplot(aes(x = game)) +
  geom_path(aes(y = def_proe), linetype = 'dashed',
            color = 'grey40', size = 1.35) +
  geom_path(aes(y = off_proe, color = team), size = 1.35) +
  nflplotR::scale_color_nfl(type = 'primary') +
  nflplotR::geom_nfl_logos(aes(y = off_proe, team_abbr = team), height = .11) + 
  theme_minimal() +
  scale_x_continuous(breaks = 1:17, labels = my_df$opponent) +
  labs(x = 'Opponent',
       y = 'Pass Rate',
       title = paste0(my_name,'\'s Pass Rate Compared to Expected Pass Rate | By Week'),
       subtitle = 'Dotted line indicates expected pass rate') +
  theme(plot.title = element_text(color = my_color,size = 24, face = 'bold'),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 17, face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
        panel.grid.minor = element_blank(),
        axis.text.x = nflplotR::element_nfl_logo(size = 1.2),
        axis.line = element_line()) -> p2

plot_grid(p1,p2,ncol = 1)

#Create PROE for defenses
def_proe %>% ggplot(aes(x = reorder(defteam, -def_proe), y = def_proe)) +
  geom_col(aes(fill = defteam, color = defteam)) +
  nflplotR::scale_fill_nfl(type = 'primary') +
  nflplotR::scale_color_nfl(type = 'secondary') +
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = .03) +
  labs(title = 'Pass Rate Over/Under Expected for Every NFL Defense | 2021',
       x = '',
       y = 'Pass Rate % Over Expected',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  scale_y_continuous(breaks = seq(-5,7.5,2.5)) +
  theme_minimal() +
  theme(axis.line = element_line(),
        panel.grid = element_blank(),
        plot.background = element_rect(color = 'white', fill = 'white'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
        panel.grid.major.y = element_line(color = 'grey90'),
        plot.title = element_text(color = 'black', size = 24, face = 'bold'),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 17, face = 'bold'),
        plot.caption = element_text(size = 13, face = 'bold', color = 'black'))

ggsave('def_proe.png', width = 14, height = 10, dpi = 'retina')
