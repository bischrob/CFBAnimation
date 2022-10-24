library(dplyr)
library(readr)
library(ggplot)
library(gganimate)
library(ggimage)
# set directory to current location
# setwd()
data = read_csv("data.csv")

options(gganimate.dev_args = list(width = 800, height = 800))
a = data %>% 
  ggplot(aes(x = off_eff,y = def_eff, image = logo)) +
  geom_image() + 
  theme_gdocs() + 
  labs(title = "Week {floor(frame_time)}") + 
  theme(text = element_text(size = 21)) +
  xlab("Offense") + 
  ylab("Defense") +
  transition_time(week) 
animate(a, nframes = 440, fps = 20)
anim_save("CFBEfficiencyWk2-Wk8.gif")