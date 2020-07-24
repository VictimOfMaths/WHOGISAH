library(tidyverse)
library(gganimate)
library(gifski)
theme_set(theme_classic())

data <- read.csv("GISAH_top_long.csv")

gap <- data %>%
  group_by(year) %>%
  mutate(rank=min_rank(-value)*1,
         Value_rel=value/value[rank==1],
         Value_lbl=paste0(" ",sprintf('%.1f', value))) %>%
  filter(rank <= 15) %>%
  ungroup()


p <- ggplot(gap, aes(rank, group=name, fill=as.factor(region)))+
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  scale_fill_manual(values=c("#FFE62A", "#DA4040", "#2DB875", "#0093D8", "#F4989B"))+
  #scale_colour_manual(values=c("#FFE62A", "#DA4040", "#2DB875", "#0093D8", "#F4989B"))+
  guides(fill=guide_legend(title="WHO region"), colour=FALSE) +
  
  labs(subtitle='{closest_state}', x = "", y = "Per capita annual consumption of alcohol (litres)",
       caption = "Sources: WHO GISAH database | Plot by @VictimOfMaths", 
       title="The 15 heaviest drinking countries in the world 1961-2015") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm"),
        plot.subtitle=element_text(size=20)) +
  transition_states(year, transition_length = 4, state_length = 1, wrap=FALSE) +
  ease_aes('cubic-in-out')

animate(p, 200, fps = 10, duration = 60, width = 800, height = 600, start_pause=10,
        renderer = gifski_renderer("gganim.gif"), end_pause=30)
