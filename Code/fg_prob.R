library(tidyverse)
library(ggplot2)
library(nflverse)
library(ggtext)

#Load play by play
pbp_loaded <- load_pbp(2017:2022)

#Get FG data and clean
fgs <- pbp_loaded %>%
  filter(!is.na(field_goal_result)) %>%
  select(kick_distance, field_goal_result) %>%
  mutate(result = case_when(field_goal_result == 'made' ~ 1,
                            TRUE ~ 0))

#Exploratory look
fgs %>% ggplot(aes(x = kick_distance, y =result)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(),
        panel.grid = element_blank(),
        plot.background = element_rect(color = 'white', fill = 'white'),
        axis.text.x = element_text(size = 30, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 30, face = 'bold', color = 'black'),
        panel.grid.major.y = element_line(color = 'grey90'),
        panel.grid.major.x = element_line(color = 'grey90'),
        plot.title = element_text(color = 'black', size = 36, face = 'bold', hjust = .5),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 30, face = 'bold'),
        plot.caption = element_text(size = 30, face = 'bold', color = 'black')) +
  labs(title = 'Fg Success Rate vs Distance',
       y = 'Success Rate',
       x = 'Distance (Yards)',
       caption = 'Seasons: 2017-2022 | Data: NFLverse') +
  scale_y_continuous(breaks = seq(0,1,.2),
                     limits = c(-.02,1.02))


#Group by distance
fgs_grouped <- fgs %>%
  group_by(kick_distance) %>%
  summarise(Total = n(),
            success_rate = mean(result)) %>%
  ungroup()

#Plot
fgs_grouped %>% ggplot(aes(x = kick_distance, y =success_rate)) +
  geom_point(aes(size=Total)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(),
        panel.grid = element_blank(),
        plot.background = element_rect(color = 'white', fill = 'white'),
        axis.text.x = element_text(size = 30, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 30, face = 'bold', color = 'black'),
        panel.grid.major.y = element_line(color = 'grey90'),
        panel.grid.major.x = element_line(color = 'grey90'),
        plot.title = element_text(color = 'black', size = 36, face = 'bold', hjust = .5),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 30, face = 'bold'),
        plot.caption = element_text(size = 30, face = 'bold', color = 'black'),
        legend.title = element_text(size=30),
        text = element_text(size=30)) +
  labs(title = 'Fg Success Rate vs Distance',
       y = 'Success Rate',
       x = 'Distance (Yards)',
       caption = 'Seasons: 2017-2022 | Data: NFLverse') +
  scale_y_continuous(breaks = seq(0,1,.2),
                     limits = c(-.02,1.02))

#Create DF for model input
fg_model_data <- fgs %>% select(-field_goal_result)

#Split data
set.seed(1972) #Go Dolphins
training_rows <- sample(1:nrow(fg_model_data),0.7*nrow(fg_model_data),replace = F)
training_data <- fg_model_data[training_rows,]
testing_data <- fg_model_data[-training_rows,]

#Create model
my_model = lm(data = training_data, formula = result ~ poly(kick_distance, 2, raw = TRUE))
summary(my_model)

#Predict testing set
testing_data$pred <- predict(my_model, testing_data)

#Group by distance
test_grouped <- testing_data %>%
  group_by(kick_distance, pred) %>%
  summarise(Total = n(),
            success_rate = mean(result)) %>%
  ungroup()

#Plot
test_grouped %>% ggplot(aes(x = kick_distance)) +
  geom_line(aes(y = pred), color = 'blue', size = 2) +
  geom_point(aes(y = success_rate, size = Total), color = 'red') +
  theme_minimal() +
  theme(axis.line = element_line(),
        panel.grid = element_blank(),
        plot.background = element_rect(color = 'white', fill = 'white'),
        axis.text.x = element_text(size = 30, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 30, face = 'bold', color = 'black'),
        panel.grid.major.y = element_line(color = 'grey90'),
        panel.grid.major.x = element_line(color = 'grey90'),
        plot.title = element_markdown(color = 'black', size = 36, face = 'bold', hjust = .5),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 30, face = 'bold'),
        plot.caption = element_text(size = 30, face = 'bold', color = 'black'),
        legend.title = element_text(size=30),
        text = element_text(size=30)) +
  labs(title = "**Polynomial Regression Model**  
    <span style='color:red; font-size:24pt'>Actual Values 
    <span style='color:black; font-size:24pt'>vs
    <span style='color:blue;'>Predicted Values</span>
    </span>",
       y = 'Success Rate',
       x = 'Distance (Yards)',
       caption = 'Seasons: 2017-2022 | Data: NFLverse') +
  scale_y_continuous(breaks = seq(0,1,.2),
                     limits = c(-.02,1.02))

#Model check
cor(test_grouped$success_rate,test_grouped$pred)
