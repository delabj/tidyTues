library(tidyverse)
library(janitor)
library(ggalt)

#load the data
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')
season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

#Clean CSV names with Janitor
game_goals <- clean_names(game_goals)
top_250 <- clean_names(top_250)
season_goals <- clean_names(season_goals)

#select the top 10 players we have data for
top_10 <- top_250 %>%
  filter(player %in% game_goals$player) %>%
  filter(raw_rank <= 13)%>%
  mutate_if(is.character, as.factor) %>%
  mutate(start_year = str_sub(years, 1,4))
  

indexed_10 <- game_goals %>%
  filter(player %in% top_10$player) %>%
  select(player, goals, penalty_min, season) %>%
  group_by(player, season)%>%
  summarise(goals = sum(goals),
            penalty_min = sum(penalty_min)) %>%
  ungroup %>%
  mutate(player = fct_inorder(player)) %>%
  complete(player, nesting(season)) %>%
  left_join(top_10) %>%
  group_by(player) %>%
  mutate(index = 1:n()) %>%
  ungroup() %>%
  mutate(goals =replace_na(goals, 0), 
         penalty_min =replace_na(penalty_min, 0))%>%
  select(player, season, raw_rank, goals, penalty_min, index, start_year)
  

  
indexed_10$player <- fct_reorder(indexed_10$player, indexed_10$start_year, min)
  


indexed_10 %>%
  ggplot(aes(x=season, y=goals, fill=raw_rank, color=raw_rank))+
  geom_area( alpha = .5, stat="xspline")+
  geom_hline(
    data = distinct(indexed_10, player, raw_rank),
    aes(yintercept = 0, color = raw_rank), size = 0.5
  ) +
  
  facet_wrap(~player, ncol = 1, strip.position = "left")+
  labs(
    x = NULL, y = NULL, fill = NULL, colour = NULL,
    title = "When Were The Good Old Days of Hockey?",
    subtitle = "Height is number of points scored by a player in a season, 
    \nColor is the players all-time ranking, darker is higher",
    caption = "Source: Tidy Tuesday/HockeyReference.com"
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 105)) +
  theme_minimal()+
  theme(panel.spacing.y = unit(-0.75, "lines")) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none")+
  theme(strip.text.y = element_text(angle=180, hjust = 1))+
  theme(panel.grid = element_blank())
  

 
 
ggsave("hockey_top_10_goal.png", dpi= 300, type = "cairo")  




indexed_10 %>%
  ggplot(aes(x=season, y=penalty_min, fill=raw_rank, color=raw_rank))+
  geom_area( alpha = .5)+
  geom_hline(
    data = distinct(indexed_10, player, raw_rank),
    aes(yintercept = 0, color = raw_rank), size = 0.5
  ) +
  
  facet_wrap(~player, ncol = 1, strip.position = "left")+
  labs(
    x = NULL, y = NULL, fill = NULL, colour = NULL,
    title = "When Did They Fight?",
    subtitle = "Height is number of minutes in the penalty box by a player in a season, 
    \nColor is the players all-time ranking, darker is higher",
    caption = "Source: Tidy Tuesday/HockeyReference.com"
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 105)) +
  theme_minimal()+
  theme(panel.spacing.y = unit(-0.75, "lines")) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none")+
  theme(strip.text.y = element_text(angle=180, hjust = 1))+
  theme(panel.grid = element_blank())




ggsave("hockey_top_10_Penalties.png", dpi= 300, type = "cairo")  


