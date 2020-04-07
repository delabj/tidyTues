library(tidyverse)
library(delabj)
library(ggtext)
library(ggchicklet)

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
stage_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/stage_data.csv')
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')

# How has the reace changed?


stage_numbers <-  c("1","2","3","4","5","6","7","8","9","10",
                    "11","12","13","14", "15", "16", "17", "18","19", "20",
                    "21","22", "23", "24", "25")

tdf_stages %>%
  mutate(Type = stringr::str_to_lower(Type)) %>%
  mutate(Type = case_when(
    grepl("flat", Type) ~ "Flat",
    grepl("plain", Type) ~ "Flat",
    grepl("time trial", Type) ~"Time Trial",
    grepl("mountain", Type) ~"Mountain",
    grepl("transition", Type) ~"Transition",
    grepl("intermediate", Type) ~"Transition",
    grepl("half", Type) ~"Transition",
    grepl("hilly", Type) ~ "Hilly",
    
    TRUE ~ Type
    
  )) %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>%
  mutate(Stage = factor(Stage,levels  = stage_numbers))%>%
  filter(year > 2007, 
         Stage %in% stage_numbers) %>%
  ggplot(aes(fill = Type, x = Stage) )+
  geom_bar(position = "dodge")+
  facet_wrap(~year, ncol = 1, strip.position = "left")+
  theme_delabj()+
  labs(
    x = NULL, y = NULL,
    title = "The Tour De France: How Have Course Stages  Changed Over Time?",
    
    subtitle = "<b style=color:#ad856e> Time Trial </b>, <b style=color:#721121>Flat</b>, <b style=color:#5B3758>Mountainous</b>, or,<b style =color:#83B692>Tranitioning</b>")+
  theme(axis.text.y = element_blank())+
  theme(plot.subtitle = element_markdown(element_markdown(lineheight = 12)))+
  scale_fill_delabj()+
  delabj::legend_none()
ggsave("test.png", dpi=320, type="cairo")




tdf_stages %>%
  mutate(Type = stringr::str_to_lower(Type)) %>%
  mutate(Type = case_when(
    grepl("flat", Type) ~ "Flat",
    grepl("plain", Type) ~ "Flat",
    grepl("time trial", Type) ~"Time Trial",
    grepl("mountain", Type) ~"Mountain",
    grepl("transition", Type) ~"Transition",
    grepl("intermediate", Type) ~"Transition",
    grepl("half", Type) ~"Transition",
    grepl("hilly", Type) ~ "Hilly",
    
    TRUE ~ Type
    
  )) %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>%
  mutate(Stage = factor(Stage,levels  = stage_numbers))%>%
  group_by(year ) %>%
  arrange(year) %>%
  mutate(cum_dist = cumsum(Distance)) %>%
  mutate(start_dist = lag(cum_dist)) %>%
  mutate(start_dist = replace_na(start_dist, 0)) %>%
  filter( year>= 2000) %>%
  ggplot(aes(fill = Type))+
    geom_rect(aes(ymin =  year-.45, ymax=year+.45, 
                  xmin = start_dist, xmax=cum_dist))+
  theme_delabj()+
  scale_y_reverse()+
  scale_fill_delabj()+
  labs(title= "How has the Tour de France Changed?", subtitle = "Section Types and Distance By Year." )+
  legend_bottom()


tdf_stages %>%
  mutate(Type = stringr::str_to_lower(Type)) %>%
  mutate(Type = case_when(
    grepl("flat", Type) ~ "Flat",
    grepl("plain", Type) ~ "Flat",
    grepl("time trial", Type) ~"Time Trial",
    grepl("mountain", Type) ~"Mountain",
    grepl("transition", Type) ~"Transition",
    grepl("intermediate", Type) ~"Transition",
    grepl("half", Type) ~"Transition",
    grepl("hilly", Type) ~ "Hilly",
    
    TRUE ~ Type
    
  )) %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>%
  mutate(Stage = factor(Stage,levels  = stage_numbers))%>%
  group_by(year ) %>%
  arrange(year) %>%
  mutate(cum_dist = cumsum(Distance)) %>%
  mutate(start_dist = lag(cum_dist)) %>%
  mutate(start_dist = replace_na(start_dist, 0)) %>%
  filter( year>= 2000) %>%
  ggplot(aes(y=Distance, x=year, group = Stage, fill = Type))+
  scale_x_reverse(breaks = seq( 2000, 2017, by= 1))+
  geom_chicklet()+
  coord_flip()+
  theme_delabj()+
  scale_fill_delabj()+
  theme(axis.title = element_markdown(lineheight =4.5))+
  labs(x="", y= "",
    title= "How has the Tour de France Changed?", 
    subtitle = "Section Types and Distance By Year since 2000.",
    caption = "Data: alastairrushworth/tdf\nViz: @delabjl")
  
ggsave("since2000.png", type= "cairo")



