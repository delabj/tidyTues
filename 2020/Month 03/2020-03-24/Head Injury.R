library(tidyverse)
library(janitor)
library(delabj)
library(patchwork)
library(ggtext)

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

tbi_age <- tbi_age %>% clean_names()
tbi_year <- tbi_year %>% clean_names()
tbi_military <- tbi_military %>% clean_names()

#### Looked at this, but nothing really grabbed my attention. 
tbi_year %>% 
  filter(injury_mechanism != "Total") %>%
  mutate(injury_mechanism = case_when(injury_mechanism %in% c("Other unintentional injury, mechanism unspecified", 
                                                           "Other or no mechanism specified") ~ "Other", 
                                    injury_mechanism %in% c("Unintentional falls", 
                                                            "Unintentionally struck by or against an object")~ "Unintentional Injury",
                                    injury_mechanism == "Motor vehicle crashes"~"Motor Accident",
                                    injury_mechanism == "Intentional Self-harm" ~ "Self Harm", 
                                    TRUE ~ injury_mechanism)) %>%
  group_by(type, year, injury_mechanism) %>%
  summarise(rate_est = sum(rate_est)) %>% 
  ungroup %>%
  group_by(type, injury_mechanism)%>%
  summarise(rate_est = mean (rate_est))%>%
  ggplot(aes(fill=injury_mechanism,  values=rate_est))+
  geom_waffle(n_rows = 15, size = .33, col="#F9F1F1" )+
  theme_delabj()+
  theme_enhance_waffle()+
  facet_wrap(~type, ncol=1)+
  legend_left()


tbi_age %>%
  filter(type=="Deaths") %>%
filter(age_group != "Total", 
       age_group != "0-17") %>%
 mutate(injury_mechanism = case_when(injury_mechanism %in% c("Other unintentional injury, mechanism unspecified", 
                                                            "Other or no mechanism specified") ~ "Other", 
                                    injury_mechanism %in% c("Unintentional Falls", 
                                                            "Unintentionally struck by or against an object")~ "Unintentional Injury",
                                    injury_mechanism == "Motor Vehicle Crashes"~"Motor Accident",
                                    injury_mechanism == "Intentional self-harm" ~ "Self Harm", 
                                    TRUE ~ injury_mechanism))   %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")), 
         injury_mechanism = factor(injury_mechanism, levels = c("Assault", "Motor Accident", "Self Harm", "Unintentional Injury", "Other" ))) %>%
  ggplot(aes(x=rate_est, y=reorder(injury_mechanism, desc(injury_mechanism) )))+
  geom_bar(stat= "identity", fill = "#721121")+
  facet_wrap(~age_group, nrow=1)+
  theme_delabj()+
  labs(y="",x="")+
  gridlines_minor_off()+
  scale_x_continuous(breaks = seq(0, 50, 25))+
  theme(panel.spacing = unit(2, "lines"))->deaths
  

tbi_age %>%
  filter(type=="Hospitalizations") %>%
  filter(age_group != "Total", 
         age_group != "0-17") %>%
  mutate(injury_mechanism = case_when(injury_mechanism %in% c("Other unintentional injury, mechanism unspecified", 
                                                              "Other or no mechanism specified") ~ "Other", 
                                      injury_mechanism %in% c("Unintentional Falls", 
                                                              "Unintentionally struck by or against an object")~ "Unintentional Injury",
                                      injury_mechanism == "Motor Vehicle Crashes"~"Motor Accident",
                                      injury_mechanism == "Intentional self-harm" ~ "Self Harm", 
                                      TRUE ~ injury_mechanism))   %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")), 
         injury_mechanism = factor(injury_mechanism, levels = c("Assault", "Motor Accident", "Self Harm", "Unintentional Injury", "Other" ))) %>%
  ggplot(aes(x=rate_est, y=reorder(injury_mechanism, desc(injury_mechanism) )))+
  geom_bar(stat= "identity", fill = "#F19953")+
  facet_wrap(~age_group, nrow=1)+
  theme_delabj()+
  labs(y="",x="")+
  gridlines_minor_off()+
  xlim(0,450)+
  scale_x_continuous(breaks = seq(0, 600, 150))+
  theme(panel.spacing = unit(2, "lines"))-> hospital


tbi_age %>%
  filter(type=="Emergency Department Visit") %>%
  filter(age_group != "Total", 
         age_group != "0-17") %>%
  mutate(injury_mechanism = case_when(injury_mechanism %in% c("Other unintentional injury, mechanism unspecified", 
                                                              "Other or no mechanism specified") ~ "Other", 
                                      injury_mechanism %in% c("Unintentional Falls", 
                                                              "Unintentionally struck by or against an object")~ "Unintentional Injury",
                                      injury_mechanism == "Motor Vehicle Crashes"~"Motor Accident",
                                      injury_mechanism == "Intentional self-harm" ~ "Self Harm", 
                                      TRUE ~ injury_mechanism))   %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")), 
         injury_mechanism = factor(injury_mechanism, levels = c("Assault", "Motor Accident", "Self Harm", "Unintentional Injury", "Other" ))) %>%
  ggplot(aes(x=rate_est, y=reorder(injury_mechanism, desc(injury_mechanism) )))+
  geom_bar(stat= "identity", fill = "#2660A4")+
  facet_wrap(~age_group, nrow=1)+
  theme_delabj()+
  gridlines_minor_off()+
  labs(y="",x="")+
  scale_x_continuous(breaks = seq(0, 1500, 750))+
  theme(panel.spacing = unit(2, "lines"))-> ervisit


full_plot <- deaths/hospital/ervisit
patchwork::wrap_elements(full_plot) +
  labs(title = "Tramatic Brain Injuries: Who Gets Hurt and How?", 
          subtitle = "Injuries Leading To: <b style='color:#721121'>Deaths</b>,
          <b style='color:#F19953'>Hospitalizations</b>, and <b style='color:#2660A4'>ER Visits</b>",
          caption="Data: CDC \n Code: @delabjl")+
  theme_delabj()+
  theme(plot.subtitle = element_markdown())->tbi


ggsave("TBI.png",plot=tbi, dpi= 300, height =  10, width =20, type = "cairo") 
  
