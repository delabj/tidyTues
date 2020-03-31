library(tidyverse)
library(janitor)
library(broom)
library(delabj)


# Get the Data
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

####### PLOT IDEAS ########
# Showing how the number of brewers of a certain size have changed over time
# Hex Map of locations
# Tree Map showing puounds used per year?



##### brewer size over time
library(ggtext)
summary(as.factor(brewer_size$brewer_size))
  
brewer_size %>%
  filter(brewer_size != "Total")%>%
  mutate(size = case_when(
    brewer_size == "Zero Barrels" ~ "1,000 or fewer",
    brewer_size == "Under 1 Barrel" ~ "1,000 or fewer",
    brewer_size == "1 to 1,000 Barrels" ~ "1,000 or fewer", 
    TRUE ~ "More than 1,000"
    
                          )
  ) %>%
  group_by(size, year)%>%
  summarise(n_of_brewers = sum(n_of_brewers)) %>%
  ggplot(aes(x=year, y=n_of_brewers, color = size))+
  geom_line(size = 2)+
  theme_delabj()+
  scale_color_delabj()+
  legend_none()+
  labs(title= "The Rise of Microbreweries", 
       subtitle ="Breweries producing <b style = 'color:#721121'>less than 1,000 barrels</b>, vs <b, style = 'color:#83B692'> more than 1,000 Barrels", 
       y="", x="",
       caption = "Data:  Alcohol and Tobacco Tax and Trade Bureau \n
       Visualization By @delabjl")+
  scale_x_continuous(breaks = round(seq(min(brewer_size$year), max(brewer_size$year), by= 1),1))+
  theme(plot.subtitle = element_markdown())


ggsave("MicroBrewerygrowth.png", dpi= 320, height = 10, width=20,  type = "cairo", units = "in")
#### Hex map
library(geojsonio)
library(rgeos)
library(gganimate)
library(transformr)

spdf <- geojson_read("~/tidyTues/2020-03-31/us_states_hexgrid.geojson", what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "iso3166_2")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

data <- spdf_fortified %>%
  left_join(beer_states,  by=c("id"="state")) %>%
  filter(type == "Bottles and Cans")

p <- ggplot() +
  geom_polygon(data = data, aes( x = long, y = lat,  fill=barrels/1000, group = group), color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color = "#D6D6D6") +
  coord_map()+
  labs(title = "Bottels And Cans Produced In: {as.integer(frame_time)}", 
       subtitle = "In Thousands of Barrels",
       caption = "Data:  Alcohol and Tobacco Tax and Trade Bureau \n
       Visualization By @delabjl")+
  theme_delabj()+
  legend_bottom()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  theme(legend.key.width = unit(3,"cm"))+
  scale_fill_viridis_b(name="Barrels (Thousands)")+
  guides(fill= guide_colorbar(title.position = "top", title.hjust = .5))+
  gridlines_off()+
  transition_time(
    year
  ) +
  enter_fade()+
  exit_fade()+
  ease_aes('sine-in-out')

animate(p, duration = 48,type = "cairo")
anim_save("Bottles and Cans.gif")



data <- spdf_fortified %>%
  left_join(beer_states,  by=c("id"="state")) %>%
  filter(type == "On Premises")

p <- ggplot() +
  geom_polygon(data = data, aes( x = long, y = lat,  fill=barrels/1000, group = group), color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color = "#D6D6D6") +
  coord_map()+
  labs(title = "Barrels Produced On Premises In: {as.integer(frame_time)}", 
       subtitle = "In Thousands of Barrels",
       caption = "Data:  Alcohol and Tobacco Tax and Trade Bureau \n
       Visualization By @delabjl")+
  theme_delabj()+
  legend_bottom()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  theme(legend.key.width = unit(3,"cm"))+
  scale_fill_viridis_b(name="Barrels (Thousands)")+
  guides(fill= guide_colorbar(title.position = "top", title.hjust = .5))+
  gridlines_off()+
  transition_time(
    year
  ) +
  enter_fade()+
  exit_fade()+
  ease_aes('sine-in-out')

animate(p, duration = 48,type = "cairo")
anim_save("On Premises.gif")


data <- spdf_fortified %>%
  left_join(beer_states,  by=c("id"="state")) %>%
  filter(type == "Kegs and Barrels")

p <- ggplot() +
  geom_polygon(data = data, aes( x = long, y = lat,  fill=barrels/1000, group = group), color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color = "#D6D6D6") +
  coord_map()+
  labs(title = "Kegs and Barrels In: {as.integer(frame_time)}", 
       subtitle = "In Thousands of Barrels",
       caption = "Data:  Alcohol and Tobacco Tax and Trade Bureau \n
       Visualization By @delabjl")+
  theme_delabj()+
  legend_bottom()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  theme(legend.key.width = unit(3,"cm"))+
  scale_fill_viridis_b(name="Barrels (Thousands)")+
  guides(fill= guide_colorbar(title.position = "top", title.hjust = .5))+
  gridlines_off()+
  transition_time(
    year
  ) +
  enter_fade()+
  exit_fade()+
  ease_aes('sine-in-out')

animate(p, duration = 48,type = "cairo")
anim_save("Kegs and Barrels.gif")

