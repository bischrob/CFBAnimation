library(dplyr)
library(magrittr)
library(tibble)
library(stringr)
library(rio)
library(ggplot2)
library(gganimate)
library(ggimage)
library(cfbfastR)
library(ggthemes)
library(ggtext)
library(purrr)
library(gifski)
if (!require("cfbplotR")) remotes::install_github("sportsdataverse/cfbplotR")

cfbd_key()

week = 1

data = import(glue::glue("data/week{week}spplus.xlsx"),setclass='tibble') %>%
  mutate_at(vars(TEAM),~ .x %>% str_remove_all("[0-9]|\\.|\\(|\\)|-") %>% stringr::str_trim()) %>%
  mutate_at(vars(RATING,OFFENSE,DEFENSE),~ .x %>% str_remove_all("\\(.*\\)")) %>%
  mutate_all(str_trim) %>% left_join(import("data/teamNames.xlsx")) %>%
  mutate_at(vars(RATING,OFFENSE,DEFENSE),as.numeric) %>%
  mutate_at(vars(team_id),as.integer) %>%
  mutate(week = !!get("week"))

if(isTRUE(any(data$logo %>% is.na()))) stop("warning: there are team names that don't match")

export(data,glue::glue("data/week{week}spplus_updated.xlsx"))

# current week
xmin = min(data$OFFENSE)
xmax = max(data$OFFENSE)
ymin = min(data$DEFENSE)
ymax = max(data$DEFENSE)
data = data %>%
  mutate(img = paste0("logos/",team_id,".png"),
         alphaImg = paste0("logos/",team_id,"_alpha75.png"))

ls = list.files("data", pattern = "updated.xlsx", full.names = T)
total = map_df(ls, import, setclass = 'tibble') %>%
  mutate(img = paste0("logos/",team_id,".png"),
         alphaImg75 = paste0("logos/",team_id,"_alpha75.png"),
         alphaImg = paste0("logos/",team_id,"_alpha.png"))
week0 = total %>%
  filter(week == 0) #%>%
# filter(team %in% c("BYU","Utah"))
# week0 = bind_rows(week0,week0 %>% mutate(week = !!get("week")))

g = data %>%
  ggplot(aes(x = OFFENSE,y = DEFENSE)) +
  geom_image(aes(image = alphaImg),size = .05, by = 'width') +
  # geom_image(data = week0,aes(x = OFFENSE,y = DEFENSE, image = alphaImg),size = .025, by = 'width') +
  theme_gdocs() +
  labs(title = glue::glue("**SP+** Week {week}"),
       x = "<img src='arrowO.png' width='300'>",
       y = "<img src='arrowD.png' width='300'>") +
  theme(text = element_text(size = 36),
        plot.title = element_markdown(),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.background = element_rect(color = NA)) +
  scale_y_reverse() +
  coord_equal(clip = 'off') +
  xlim(c(xmin,xmax)) +
  ylim(c(ymax,ymin))
ggsave(glue::glue("figures/week{week}sppcfb.png"),dpi = 300, width = 9, height = 6)

# by conference
teams = cfbfastR::cfbd_team_info(only_fbs = T)
conferences = teams$conference %>% unique %>% sort
walk(conferences,function(conf){
  cf = tibble(team = conf, OFFENSE = xmin + 2.5, DEFENSE = ymin + 2.5)
  cfweek0 = week0 %>%
    inner_join(teams %>% select(team_id,conference) %>%
                 filter(conference == conf))
  g = data %>%
    inner_join(teams %>% select(team_id,conference) %>%
                 filter(conference == conf)) %>%
    ggplot(aes(x = OFFENSE,y = DEFENSE)) +
    geom_image(data = cfweek0,aes(x = OFFENSE,y = DEFENSE, image = alphaImg),size = .033, by = 'width') +
    geom_image(aes(image = alphaImg),size = .075, by = 'width') +
    geom_cfb_logos(data = cf, aes(x = OFFENSE,y = DEFENSE, team = team), width = .2, alpha = .75) +
    theme_gdocs() +
    labs(title = glue::glue("**SP+** Week {week}:<br>**<span style = 'font-size:24pt'>{conf}</span>**"),
         x = "<img src='arrowO.png' width='300'>",
         y = "<img src='arrowD.png' width='300'>") +
    theme(text = element_text(size = 24),
          plot.title = element_textbox(lineheight = .5),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.background = element_rect(color = NA)) +
    scale_y_reverse() +
    coord_equal(clip = 'off') +
    xlim(c(xmin,xmax)) +
    ylim(c(ymax,ymin))
  ggsave(glue::glue("figures/week{week}sppcfb-{conf}.png"),dpi = 300, width = 9, height = 6)
})

# animations


xmin = min(total$OFFENSE)
xmax = max(total$OFFENSE)
ymin = min(total$DEFENSE)
ymax = max(total$DEFENSE)

a = total %>%
  # filter(team %in% c("BYU","Utah")) %>%
  ggplot(aes(x = OFFENSE,y = DEFENSE)) +
  geom_image(aes(image = alphaImg75),size = .065, by = 'width') +
  # geom_image(data = week0,aes(x = OFFENSE,y = DEFENSE, image = alphaImg),size = .025, by = 'width') +
  theme_gdocs() +
  labs(title = "**SP+** Week {as.integer(frame_time)}",
       x = "<img src='arrowO.png' width='300'>",
       y = "<img src='arrowD.png' width='300'>") +
  theme(text = element_text(size = 36),
        plot.title = element_markdown(),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.background = element_rect(color = NA)) +
  scale_y_reverse() +
  coord_equal(clip = 'off') +
  xlim(c(xmin,xmax)) +
  ylim(c(ymax,ymin)) +
  transition_time(week)
animate(a, nframes = 195, fps = 15, height = 900, width = 1350, end_pause = 30, renderer = gganimate::gifski_renderer(glue::glue("figures/CFBEfficiency-week{week}.gif")))

# biggest differences
current = total %>%
  filter(week == !!get("week")) %>%
  select(TEAM1 = TEAM,team_id,RATING) %>%
  left_join(week0 %>% select(TEAM2 = TEAM,team_id,start = RATING,team)) %>%
  mutate(change = RATING - start) %>%
  distinct_all()
export(current,"data/current.xlsx")
