library(tidyverse)
library(ggplot2)

#Get Schedule
s_loaded <- nflreadr::load_schedules(2021)
s_loaded <- s_loaded %>% filter(game_type == 'REG')
s_clean1 <- s_loaded %>%
  select(season, team_abbr = home_team, opp = away_team, week)
s_clean2 <- s_loaded %>%
  select(season, team_abbr = away_team, opp = home_team, week)
sclean3 <- rbind(s_clean1,s_clean2)

#load stats
stats_loaded <- nflreadr::load_player_stats(seasons = 2021)

#Get WR/TE/RB Stats only
wr1_schedule <- stats_loaded %>%
  filter(season_type == 'REG') %>%
  select(player_id:week, pts = fantasy_points_ppr, targets) %>%
  group_by(player_id, recent_team) %>%
  mutate(ave_pts = round(mean(pts),2),
         tgts_gm = sum(targets)/n(),
         games = n()) %>%
  filter(games > 7,
         tgts_gm > 5) %>%
  ungroup() %>%
  group_by(recent_team) %>%
  filter(tgts_gm == max(tgts_gm)) %>%
  left_join(sclean3, by = c('season', 'week', 'recent_team' ='team_abbr')) %>%
  arrange(opp)

#Group by defense 
def_wr1 <- wr1_schedule %>%
  group_by(opp) %>%
  summarise(wr1_pts = mean(pts - ave_pts))

#plot
def_wr1 %>%
  ggplot(aes(x = reorder(opp, wr1_pts), y = wr1_pts, team_abbr = opp)) +
  geom_col(aes(color = opp, fill = opp), width = .75) +
  nflplotR::scale_color_nfl(type = 'secondary') +
  nflplotR::scale_fill_nfl(alpha = .8) +
  nflplotR::geom_nfl_logos(height = .06) +
  theme_minimal() +
  scale_y_reverse(breaks = 5:-4,
                  expand = c(0,0),
                  limits = c(5,-4)) +
  labs(title = 'Which Defenses Shut Down the Other Team\'s WR1',
       y = 'Difference',
       x = 'WR1 = Highest Targets/Game',
       subtitle = 'Opponent\'s WR1\'s FPTs minus Season Average',
       caption = 'Figure: @Timboslice003 | Data: #NFLverse') +
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line('grey50'),
        plot.background = element_rect('grey85'),
        plot.title = element_text(face = 'bold', size = 20,
                                  hjust = .5, color = 'black'),
        plot.subtitle = element_text(size = 12,
                                     hjust = .5, color = 'black'),
        plot.caption = element_text(face = 'bold', size = 10,
                                    color = 'black'),
        plot.margin = margin(r = 20, l = 10, t = 5, b = 10),
        axis.title.y = element_text(face = 'bold', size = 19, vjust = 2,
                                    hjust = .5, color = 'black'),
        axis.text = element_text(face = 'bold', size = 13,
                                 hjust = .5, color = 'black'),
        axis.ticks.y = element_line('grey'))

ggsave('WR1_opp.png', width = 14, height = 10, dpi = 'retina') 
