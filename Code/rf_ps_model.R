library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(ggtext)

#Load in pitch data
pitches_loaded <- read.csv('mlb_pitch_velo_assessment.csv')

#Look into pitch number effects
pitches_loaded %>%
  filter(!is.na(release_speed)) %>%
  ggplot(aes(x=pitch_number, y=release_speed)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~pitch_type) +
  theme(axis.title = element_text(size = 30, face = 'bold'),
        plot.title = element_text(size = 30, face = 'bold'))

#Get list of all pitchers
pitcher_list <- unique(pitches_loaded$pitcher_name)

#For loop for each pitcher's model
tested_df <- tibble()
set.seed(2001) #Go DBacks
for (name in pitcher_list) {
  #Pitchers data
  temp_model_data <- pitches_loaded %>%
    select(pitcher_name, pre_pitch_outs, pre_pitch_balls, pre_pitch_strikes, release_speed) %>%
    filter(pitcher_name == name,
           !is.na(release_speed)) %>%
    mutate(pre_pitch_outs = as.factor(pre_pitch_outs),
           pre_pitch_balls = as.factor(pre_pitch_balls),
           pre_pitch_strikes = as.factor(pre_pitch_strikes),
           faster = ifelse(release_speed > 89.95, 1, 0))
  
  #Split data
  training_rows <- sample(1:nrow(temp_model_data),0.7*nrow(temp_model_data),replace = F)
  training_data <- temp_model_data[training_rows,]
  testing_data <- temp_model_data[-training_rows,]
  
  #Get model
  model <- randomForest(data = training_data, faster ~ pre_pitch_outs + pre_pitch_balls + 
                          pre_pitch_strikes,
                        mtry = 3, ntree = 100)
  
  #Use model
  testing_data$pred <- predict(model, testing_data)
  
  #Add to tibble of data
  tested_df <- rbind(tested_df, testing_data)
  
  
}

#Filter for first pitch of the game
final_df <- tested_df %>%
  filter(pre_pitch_outs == 0,
         pre_pitch_balls == 0,
         pre_pitch_strikes == 0)
varImpPlot(model)

#Graph
final_df %>%
  group_by(pred) %>%
  summarise(fast = mean(faster))%>%
  ggplot(aes(x = pred, y = fast)) +
  geom_point(size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "blue", linetype = "dashed", linewidth = 2) +
  scale_x_continuous(breaks = seq(0,1,.1)) +
  scale_y_continuous(breaks = seq(0,1,.1)) +
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
  labs(title = 'Predicting Pitch Speed > 89.95 MPH | First Pitch of First AB of Inning',
       y = 'Actual Rate',
       x = 'Predicted Rate')

#Get results and check correlation
results <- final_df %>%
  group_by(pred) %>%
  summarise(fast = mean(faster))

cor(results$pred, results$fast)

#write.csv(results, row.names = F, 'test.csv')
