library(tidyverse)
library(ggplot2)

#Load schedule and clean data set to get each teams and 
#their weekly opponent
s_loaded <- nflreadr::load_schedules(2021)
s_loaded <- s_loaded %>% filter(game_type == 'REG')
s_clean1 <- s_loaded %>%
  select(season, team_abbr = home_team, opponent = away_team, week)
s_clean2 <- s_loaded %>%
  select(season, team_abbr = away_team, opponent = home_team, week)
s_clean3 <- rbind(s_clean1,s_clean2) %>%
  arrange(week)

#Load rosters to get player id and position
roster <- nflreadr::load_rosters(2021)
roster_clean <- roster %>%
  select(player_id = gsis_id, position)

#Load player stats
loaded_stats <- nflreadr::load_player_stats(2021)

#Join with rosters to filter for QBs. Get each teams 
#fantasy points from all their QBs each week as well as
#the points per game for the season
stats_qb <- loaded_stats %>%
  left_join(roster_clean, by = 'player_id') %>%
  filter(position == 'QB',
         season_type =='REG') %>%
  group_by(opponent = recent_team, week) %>%
  summarise(QB_pts = sum(fantasy_points_ppr)) %>%
  ungroup() %>%
  group_by(opponent) %>%
  mutate(QB_pts_pg = sum(QB_pts)/17) %>%
  ungroup()

#Join data frames together to get all data.
#Create a difference column and group by defense
my_df <- s_clean3 %>%
  left_join(stats_qb, by = c('opponent','week')) %>%
  mutate(pts_d = QB_pts - QB_pts_pg) %>%
  group_by(team_abbr) %>%
  mutate(QB_def = median(pts_d))

#Plot QBs
my_df %>% ggplot(aes(x = reorder(team_abbr,QB_def), y = pts_d)) +
  geom_jitter(width = .1) +
  geom_boxplot(aes(fill = team_abbr, color = team_abbr), size = 1) +
  nflplotR::scale_color_nfl(type = 'secondary') +
  nflplotR::scale_fill_nfl(alpha = .8) +
  theme(axis.text.x = nflplotR::element_nfl_logo(size = 1.1),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line('grey70'),
        panel.background = element_rect('white'),
        plot.background = element_rect('grey85'),
        plot.title = element_text(face = 'bold', size = 24,
                                  hjust = .5, color = 'black'),
        plot.subtitle = element_text(size = 15,
                                     hjust = .5, color = 'black'),
        plot.caption = element_text(face = 'bold', size = 15,
                                    color = 'black'),
        plot.margin = margin(r = 20, l = 10, t = 5, b = 10),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(face = 'bold', size = 12, color = 'black')) +
  labs(title = 'Relative Defensive Performance vs QBs',
       y = '',
       x = '',
       subtitle = 'Y-Axis: QB Fantasy Points Allowed vs Opponent\'s QB Fantasy Points per Game',
       caption = 'Figure: @Timboslice003 | Data: NFLverse')

#Save
ggsave('QbsDef.png', width = 14, height = 10, dpi = 'retina')
