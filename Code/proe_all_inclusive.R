#Needed libraries
library(tidyverse)
library(ggplot2)
#Also need the following:
# nflreadr
# nflplotR

#Set Variables
#EX: Want PROE for week 4 - 7 in 2022 season, week_start will be 4,
# week_end would be 7, and season would be 2022. For season long,
# set week_end to the week you want the data through. week_end will
# be included in the data.
week_start <- 10
week_end <- 13
season <- 2022

#         *WARNING*
# Running the whole script will save the following files to your directory:
# Defensive PROE for season up to week_end (.png)
# Defensive PROE from week_start up to and including week_end (.png)
# Defensive PROE for season up to week_end (.png)
# Defensive PROE from week_start up to and including week_end (.png)
# Def and Off PROE for each team for the season up to week_end (.csv)
# Def and Off PROE for each team from week_start up to and including week_end (.csv)

#Names for files (used later)
proe_def_szn <- paste0('def_proe_',season,'_szn_through_',week_end,'.png')
proe_def_group <- paste0('def_proe_',season,'_',week_start,'_through_',week_end,'.png')
proe_off_szn <- paste0('off_proe_',season,'_szn_through_',week_end,'.png')
proe_off_group <- paste0('off_proe_',season,'_',week_start,'_through_',week_end,'.png')

#Load the play by play data for season
pbp_loaded <- nflreadr::load_pbp(season)

#First up is defensive PROE over time period set above
#----------
def_proe_db_tp <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         week >= week_start,
         week <= week_end) %>%
  group_by(defteam) %>%
  summarise(def_proe = mean(pass_oe))

#Set plotting limits and plot
ymax <- max(def_proe_db_tp$def_proe) + .5
ymin <- min(def_proe_db_tp$def_proe) - .5
def_proe_db_tp %>% ggplot(aes(x = reorder(defteam, -def_proe), y = def_proe)) +
  geom_col(aes(fill = defteam, color = defteam)) +
  nflplotR::scale_fill_nfl(type = 'primary') +
  nflplotR::scale_color_nfl(type = 'secondary') +
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = .03) +
  labs(title = paste0('Pass Rate Over Expected Against for Every NFL Defense | 2022 Weeks ',
                      week_start,'-',week_end),
       x = '',
       y = 'Pass Rate % Over Expected',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  scale_y_continuous(breaks = seq(-100,100,2.5),
                     limits = c(ymin,ymax)) +
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

ggsave(proe_def_group, width = 14, height = 10, dpi = 'retina')
#----------
#Next is offensive PROE for time period
#----------
off_proe_db_tp <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         week >= week_start,
         week <= week_end) %>%
  group_by(posteam) %>%
  summarise(off_proe = mean(pass_oe))

#Set plotting limits and plot
ymax <- max(off_proe_db_tp$off_proe) + .5
ymin <- min(off_proe_db_tp$off_proe) - .5
off_proe_db_tp %>% ggplot(aes(x = reorder(posteam, -off_proe), y = off_proe)) +
  geom_col(aes(fill = posteam, color = posteam)) +
  nflplotR::scale_fill_nfl(type = 'primary') +
  nflplotR::scale_color_nfl(type = 'secondary') +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = .03) +
  labs(title = paste0('Pass Rate Over Expected for Every NFL Offense | 2022 Weeks ',
                       week_start,'-',week_end),
       x = '',
       y = 'Pass Rate % Over Expected',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  scale_y_continuous(breaks = seq(-100,100,2.5),
                     limits = c(ymin,ymax)) +
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

ggsave(proe_off_group, width = 14, height = 10, dpi = 'retina')
#---------
#Next is season long defensive PROE
def_proe_db <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         week <= week_end) %>%
  group_by(defteam) %>%
  summarise(def_proe = mean(pass_oe))

#Set plotting limits and plot
ymax <- max(def_proe_db$def_proe) + .5
ymin <- min(def_proe_db$def_proe) - .5
def_proe_db %>% ggplot(aes(x = reorder(defteam, -def_proe), y = def_proe)) +
  geom_col(aes(fill = defteam, color = defteam)) +
  nflplotR::scale_fill_nfl(type = 'primary') +
  nflplotR::scale_color_nfl(type = 'secondary') +
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = .03) +
  labs(title = paste0('Pass Rate Over Expected Against for Every NFL Defense | 2022 through Week ', week_end),
       x = '',
       y = 'Pass Rate % Over Expected',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  scale_y_continuous(breaks = seq(-100,100,2.5),
                     limits = c(ymin,ymax)) +
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

ggsave(proe_def_szn, width = 14, height = 10, dpi = 'retina')
#--------------
#Last is season long offensive PROE
off_proe_db <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         week <= week_end) %>%
  group_by(posteam) %>%
  summarise(off_proe = mean(pass_oe))
#Set plotting limits and plot
ymax <- max(off_proe_db$off_proe) + .5
ymin <- min(off_proe_db$off_proe) - .5
off_proe_db %>% ggplot(aes(x = reorder(posteam, -off_proe), y = off_proe)) +
  geom_col(aes(fill = posteam, color = posteam)) +
  nflplotR::scale_fill_nfl(type = 'primary') +
  nflplotR::scale_color_nfl(type = 'secondary') +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = .03) +
  labs(title = paste0('Pass Rate Over Expected for Every NFL Offense | 2022 through Week ',week_end),
       x = '',
       y = 'Pass Rate % Over Expected',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  scale_y_continuous(breaks = seq(-100,100,2.5),
                     limits = c(ymin,ymax)) +
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

ggsave(proe_off_szn, width = 14, height = 10, dpi = 'retina')

#Creating csvs
#csv for time period
def_proe_db_tp <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         week >= week_start,
         week <= week_end) %>%
  group_by(defteam) %>%
  summarise(def_proe = mean(pass_oe),
            games_played = length(unique(game_id)))

off_proe_db_tp <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         week >= week_start,
         week <= week_end) %>%
  group_by(posteam) %>%
  summarise(off_proe = mean(pass_oe))

tp_csv <- def_proe_db_tp %>%
  left_join(off_proe_db, by = c('defteam' = 'posteam')) %>%
  mutate(off_proe = round(off_proe, 2),
         def_proe = round(def_proe, 2)) %>%
  select(team = defteam, off_proe, def_proe, games_played)

write.csv(tp_csv, file = paste0('proe_',season,'_',week_start,'_through_',week_end,'.csv'),
          row.names = FALSE)

#csv for whole season up to week_end
def_proe_db_tp <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         week <= week_end) %>%
  group_by(defteam) %>%
  summarise(def_proe = mean(pass_oe),
            games_played = length(unique(game_id)))

off_proe_db_tp <- pbp_loaded %>%
  filter(season_type == 'REG',
         !is.na(pass_oe),
         week <= week_end) %>%
  group_by(posteam) %>%
  summarise(off_proe = mean(pass_oe))

total_csv <- def_proe_db_tp %>%
  left_join(off_proe_db, by = c('defteam' = 'posteam')) %>%
  mutate(off_proe = round(off_proe, 2),
         def_proe = round(def_proe, 2)) %>%
  select(team = defteam, off_proe, def_proe, games_played)

write.csv(total_csv, file = paste0('proe_',season,'_through_',week_end,'.csv'),
          row.names = FALSE)
