# install.packages("schrute")
# remotes::install_github("hrbrmstr/ggchicklet")

library(schrute)
library(tidyverse)
library(tidytext)
library(ggchicklet)
library(delabj)


office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
episode_text <- schrute::theoffice


data("debates2019")

sapply(strsplit(episode_text$text[1], " "), length)


episode_text_word_count <- episode_text %>%
  mutate(words_spoken =  sapply(strsplit(text, " "), length))
  max(episode_text_word_count$words_spoken)

  
season_word_count <-  episode_text_word_count %>%
  group_by(character, season ) %>%
  summarise(word_count = sum(words_spoken), 
            words_so_far = cumsum(word_count)) %>%

  ungroup()


total_line <- episode_text %>%
  group_by(character, season) %>%
  count(episode) %>%
  mutate( num_lines = sum(n)) %>%
  summarise(num_lines = sum(num_lines)) %>%
  ungroup() 
  
main_characters <- c("Angela", "Kelly", "Toby",
                     "Phyllis", "Oscar", "Dwight",
                     "Darryl", "Jan", "Ryan",
                     "Pam", "Kevin","Michael",
                      "Erin", "Jim", "Andy" , 
                     "Other")

top_10_characters <- c("", "Dwight", "Jim", 
                       "Pam", "Andy", "Angela", 
                       "Kevin", "Erin", "Oscar", 
                       "Ryan")

season_word_count %>% 
  mutate( character = if_else(!(character %in% top_10_characters), "Other", character)) %>%
  filter(character %in% top_10_characters) %>%
  mutate(character = fct_reorder(character, words_so_far)) %>%
  group_by(season, character) %>%
  summarise(word_count = sum(word_count)) %>%
  ungroup %>%
  ggplot(aes(character, word_count, fill = season, group = season ))+
  geom_chicklet()+
  theme_delabj()+
  coord_flip()
  
####################### Sentiment Analysis ############################


top_10_by_season <- total_line %>%
  group_by(season) %>%
  top_n( n = 10, wt= num_lines) %>%
  arrange(desc(season), desc(num_lines)) %>%
  ungroup()

main_char <- top_10_by_season %>%
  select(character) %>%
  distinct() %>%
  pull()

library(ggridges)


characters_to_plot <- c("Angela", "Dwight" ) #"Jim", "Pam", "Ryan", "Andy")
characters_to_plot_fac <- factor("Angela", "Dwight")# "Jim", "Pam", "Ryan", "Andy")
episode_text %>% 
  filter(character %in% characters_to_plot) %>%
  group_by(character, season,  episode) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(character, season, index = index, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  summarise(avg_sentiment = sum(positive)/(sum(negative)+sum(positive)), 
            positive = sum(positive),
            negative = sum(negative)) %>%
  ungroup() %>%
  #mutate(character =  fct_relevel(character, levels = characters_to_plot)) %>%
  ggplot(aes(x=avg_sentiment, y =season, fill = character))+
  geom_density_ridges(col = "Black", alpha =.75)+
  #facet_wrap(~character)+
  xlim(0,1)+
  theme_delabj()+
  scale_fill_delabj("main")+
  #legend_none()
  labs(y="", x = "Percent Positive Sentiment")


###################### Network ? #######################


# top_10_by_season <- total_line %>%
#   group_by(season) %>%
#   top_n( n = 10, wt= num_lines) %>%
#   arrange(desc(season), desc(num_lines)) %>%
#   ungroup()
# 
# top_10_by_season %>%
#   select(character) %>%
#   distinct() %>%
#   pull()
# 
# mentions <-episode_text %>%
#   filter(character %in% main_characters) %>%
#   group_by(character, season) %>% 
#   mutate(dwight_count  =   if_else(sapply(strsplit(text, "Dwight"), length) -1 <= 0,
#                                    0,sapply(strsplit(text, "Dwight"), length) -1), 
#          jim_count = if_else( sapply(strsplit(text, "Jim"), length)-1 <= 0, 
#                               0,  sapply(strsplit(text, "Jim"), length)-1), 
#          pam_count =  if_else(sapply(strsplit(text, "Pam"), length)-1 <= 0,
#                               0,
#                               sapply(strsplit(text, "Pam"), length)-1), 
#          andy_count =   if_else(sapply(strsplit(text, "Andy"), length)-1 <= 0,
#                                 0,
#                                 sapply(strsplit(text, "Andy"), length)-1), 
#          angela_count =  if_else(sapply(strsplit(text, "Angela"), length)-1 <= 0,
#                                  0,
#                                  sapply(strsplit(text, "Angela"), length)-1),  
#          erin_count =   if_else(sapply(strsplit(text, "Erin"), length)-1 <= 0,
#                                 0,
#                                 sapply(strsplit(text, "Erin"), length)-1), 
#          oscar_count =   if_else(sapply(strsplit(text, "Oscar"), length)-1 <= 0,
#                                  0,
#                                  sapply(strsplit(text, "Oscar"), length)-1), 
#          nellie_count =   if_else(sapply(strsplit(text, "Nellie"), length)-1 <= 0,
#                                   0,
#                                   sapply(strsplit(text, "Nellie"), length)-1), 
#          kevin_count =   if_else(sapply(strsplit(text, "Nellie"), length)-1 <= 0,
#                                  0,
#                                  sapply(strsplit(text, "Nellie"), length)-1), 
#          darryl_count =   if_else(sapply(strsplit(text, "Darryl"), length)-1 <= 0,
#                                   0,
#                                   sapply(strsplit(text, "Darryl"), length)-1), 
#          robert_count =   if_else(sapply(strsplit(text, "Robert"), length)-1 <= 0,
#                                   0,
#                                   sapply(strsplit(text, "Robert"), length)-1), 
#          ryan_count =   if_else(sapply(strsplit(text, "Ryan"), length)-1 <= 0,
#                                 0,
#                                 sapply(strsplit(text, "Ryan"), length)-1), 
#          michael_count =  if_else(sapply(strsplit(text, "Michael"), length)-1 <= 0,
#                                   0,
#                                   sapply(strsplit(text, "Michael"), length)-1), 
#          gabe_count =  if_else(sapply(strsplit(text, "Gabe"), length)-1 <= 0,
#                                0,
#                                sapply(strsplit(text, "Gabe"), length)-1), 
#          phyllis_count =  if_else(sapply(strsplit(text, "Phyllis"), length)-1 <= 0,
#                                   0,
#                                   sapply(strsplit(text, "Phyllis"), length)-1), 
#          kelly_count =   if_else(sapply(strsplit(text, "Kelly"), length)-1 <= 0,
#                                  0,
#                                  sapply(strsplit(text, "Kelly"), length)-1), 
#          jan_count =  if_else(sapply(strsplit(text, "Jan"), length)-1 <= 0,
#                               0,
#                               sapply(strsplit(text, "Jan"), length)-1), 
#          karen_count = if_else(sapply(strsplit(text, "Karen"), length)-1 <= 0,
#                                0,
#                                sapply(strsplit(text, "Karen"), length)-1), 
#          stanley_count =  if_else(sapply(strsplit(text, "Stanley"), length)-1 <= 0,
#                                   0,
#                                   sapply(strsplit(text, "Stanley"), length)-1), 
#          roy_count =  if_else(sapply(strsplit(text, "Roy"), length)-1 <= 0,
#                               0,
#                               sapply(strsplit(text, "Roy"), length)-1) )
# 
# 
# 
# mentions <- mentions %>%
#   select(-c(index, episode, episode_name, director, writer, text, text_w_direction)) %>%
#   summarise(Dwight = sum(dwight_count), 
#             Jim = sum(jim_count), 
#             Pam = sum(pam_count), 
#             Andy = sum(andy_count), 
#             Angela = sum(angela_count), 
#             Erin = sum(erin_count), 
#             Oscar = sum(oscar_count), 
#             Nellie = sum(nellie_count), 
#             Kevin = sum(kevin_count), 
#             Darryl = sum(darryl_count), 
#             Robert = sum(robert_count), 
#             Ryan = sum(ryan_count),
#             Michael = sum(michael_count), 
#             Gabe = sum(gabe_count), 
#             Phyllis = sum(phyllis_count),
#             Kelly = sum(kelly_count), 
#             Jan = sum(jan_count), 
#             Karen = sum(karen_count), 
#             Stanley = sum(stanley_count),
#             Roy = sum(roy_count)
#   ) %>%
#   arrange(season) %>%
#   pivot_longer( cols = -c(season, character), names_to = "mentions", values_to = "count") %>%
#   uncount(count)
# 
# 
# mentions %>% filter(season == "01") %>%
#   unite(character, character, mentions, sep = " ") %>%
#   pairwise_cor(character, season) %>%
#   graph_from_data_frame() %>%
#   ggraph("linear", circular = TRUE) +
#   geom_edge_arc(aes(alpha = correlation,
#                     width = correlation),
#                 colour = "#238b45") +
#   geom_node_label(aes(label = name),
#                   colour = "gray30",
#                   fill = "white",
#                   family = "sans",
#                   fontface = "bold",
#                   label.padding = unit(0.15, "lines"),
#                   label.r = unit(0, "lines"),
#                   label.size = NA) +
#   
#   scale_edge_width(range = c(0.5, 2)) +
#   coord_fixed(ratio = 0.75) +
#   
#   theme(legend.position = "none") +
#   labs(title = "Who Are You Talking About?",
#        subtitle = "Wider, darker lines mean those characters menioned each other more often. "
#       )
# 
# 
# 
