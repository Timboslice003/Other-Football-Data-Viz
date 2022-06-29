library(tidyverse)

adp_loaded <- read.csv('adp_6_9.csv')

adp <- adp_loaded %>%
  filter(Team != 'N/A') %>%
  group_by(Team, Pos) %>%
  mutate(team_rank = row_number(Current.ADP)) %>%
  ungroup()

team_stack <- adp %>%
  filter(Pos == 'QB' & team_rank == 1 |
         Pos == 'TE' & Current.ADP < 210 |
         Pos == 'WR' & Current.ADP < 210) %>%
  group_by(Team) %>%
  mutate(qb_adp = ifelse(Pos == 'QB', Current.ADP, 300),
         qb_adp = min(qb_adp)) %>%
  ungroup() %>%
  group_by(Player) %>%
  mutate(last_name = strsplit(Player,' ')[[1]][2],
         last_name = case_when(last_name == 'St.' ~ 'St. Brown',
                               TRUE ~ last_name))

team_stack %>% ggplot(aes(x = reorder(Team,qb_adp), y = Current.ADP,
                          label = last_name)) +
  geom_line(color = 'grey', size = 2, alpha = .5) +
  geom_text(aes(color = ifelse(Pos == 'QB','PIT',Team)),
            size = 3, fontface = 'bold') +
  nflplotR::scale_color_nfl() +
  scale_y_reverse(breaks = seq(0,12*18,36)) +
  theme_minimal() +
  labs(x = '',
       y = 'ADP',
       title = 'Stacking QBs with Pass Catchers in Best Ball Fantasy Drafts',
       subtitle = 'Sorted by QB ADP | Pass Catchers Drafted In Top 210 | ADP as of 6/9/22',
       caption = 'Idea: @SamHoppen | Figure: @Timboslice003 | Data: @UnderdogFantasy') +
  theme(axis.text.x = nflplotR::element_nfl_logo(size = 1.1),
        axis.line = element_line(),
        panel.grid = element_blank(),
        plot.background = element_rect('white'),
        axis.text.y = element_text(size = 13, face = 'bold', color = 'black'),
        panel.grid.major.y = element_line(color = 'grey90'),
        plot.title = element_text(color = 'black', size = 24, face = 'bold'),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 17, face = 'bold'),
        plot.caption = element_text(size = 13, face = 'bold', color = 'black'))

ggsave('stacks.png', width = 14, height = 10, dpi = 'retina')

#Get Picks list
pick <- 5
picks <- tibble(d_round = 1:18)
picks <- picks %>%
  mutate(overall = case_when(d_round %in% seq(2,18,2) ~ (d_round)*12 - pick + 1,
                             d_round %in% seq(1,17,2) ~ (d_round-1)*12 + pick),
         adp_low = overall - 4,
         adp_high = overall + 4)
