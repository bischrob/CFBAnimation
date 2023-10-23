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
teams = cfbfastR::cfbd_team_info(only_fbs = T)

year = 2023
week = 8
# for(year in 2019:2023){
# for(week in 0:3){
  # try({
    print(paste(year,week))
data = import(glue::glue("data/{year}-week{week}-spplus.xlsx"),setclass='tibble') %>%
  select(1:5) %>%
  setNames(c("TEAM","RATING","OFFENSE","DEFENSE","SPECIALTEAMS")) %>%
  mutate_at(vars(TEAM),~ .x %>% str_remove_all("[0-9]|\\.|\\(|\\)|-") %>% stringr::str_trim()) %>%
  mutate_at(vars(RATING,OFFENSE,DEFENSE,SPECIALTEAMS),~ .x %>% str_remove_all("\\(.*\\)")) %>%
  mutate_all(str_trim) %>% left_join(import("data/teamNames.xlsx"), by = "TEAM") %>%
  mutate_at(vars(RATING,OFFENSE,DEFENSE,SPECIALTEAMS),as.numeric) %>%
  mutate_at(vars(team_id),as.integer) %>%
  mutate(year = !!get("year"),
         week = !!get("week")) %>%
  left_join(teams %>% select(team_id,conference), by = "team_id")

if(isTRUE(any(data$logo %>% is.na()))) stop("warning: there are team names that don't match")

export(data,glue::glue("data/{year}-week{week}-spplus_updated.xlsx"))
  # })
  # }
# }
# current week
xmin = min(data$OFFENSE)
xmax = max(data$OFFENSE)
ymin = min(data$DEFENSE)
ymax = max(data$DEFENSE)
data = data %>%
  mutate(img = paste0("logos/",team_id,".png"),
         alphaImg = paste0("logos/",team_id,"_alpha75.png"))

ls = list.files("data", pattern = "spplus_updated.xlsx", full.names = T) %>%
  .[which(str_detect(.,as.character(year)))]
total = map_df(ls, import, setclass = 'tibble') %>%
  mutate(img = paste0("logos/",team_id,".png"),
         alphaImg75 = paste0("logos/",team_id,"_alpha75.png"),
         alphaImg = paste0("logos/",team_id,"_alpha.png"))
prior = total %>%
  filter(week == !!get("week")-1) #%>%
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

# Conference overview
conferenceRanking = data %>%
  group_by(conference) %>%
  summarize(RATING = mean(RATING)) %>%
  arrange(desc(RATING))
library(cfbplotR)
data %>%
  mutate(conference = factor(conference, levels = conferenceRanking$conference %>% unique)) %>%
  filter(conference != "FBS Independents") %>%
  ggplot(aes(x = RATING, y = conference, color = conference)) +
  geom_violin(fill = 'lightblue', alpha = .5, linewidth = 1) +
  geom_jitter(position = position_jitter(height = .2), color = "#404040") +
  cfbplotR::scale_y_cfb(size = 36) +
  theme_bw() +
  guides(color = 'none') +
  ylab("") +
  theme_y_cfb()
ggsave(glue::glue("figures/week{week}sppcfbconferences.png"),dpi = 300, width = 9, height = 6)

# by conference
conferences = teams$conference %>% unique %>% sort
walk(conferences,function(conf){
  cf = tibble(team = conf, OFFENSE = xmin + 2.5, DEFENSE = ymin + 2.5)
  cfprior = prior %>%
    inner_join(teams %>% select(team_id,conference) %>%
                 filter(conference == conf))
  g = data %>%
    inner_join(teams %>% select(team_id,conference) %>%
                 filter(conference == conf)) %>%
    ggplot(aes(x = OFFENSE,y = DEFENSE)) +
    geom_image(data = cfprior,aes(x = OFFENSE,y = DEFENSE, image = alphaImg),size = .033, by = 'width') +
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
animate(a, nframes = 275, fps = 15, height = 900, width = 1350, end_pause = 30, renderer = gganimate::gifski_renderer(glue::glue("figures/CFBEfficiency-week{week}.gif")))

# biggest differences
current = total %>%
  filter(week == !!get("week"),
         year == !!get("year")) %>%
  select(TEAM1 = TEAM,team_id,RATING) %>%
  left_join(prior %>% select(TEAM2 = TEAM,team_id,start = RATING,team)) %>%
  mutate(change = RATING - start) %>%
  distinct_all()
export(current,"data/current.xlsx")

# # all time
# ls = list.files("data",pattern = "spplus_updated.xlsx", full.names = T)
# all = map_df(ls, import, setclass = 'tibble') %>%
#   mutate(img = paste0("logos/",team_id,".png"),
#          alphaImg75 = paste0("logos/",team_id,"_alpha75.png"),
#          alphaImg = paste0("logos/",team_id,"_alpha.png"))
#
# xmin = min(all$OFFENSE)
# xmax = max(all$OFFENSE)
# ymin = min(all$DEFENSE)
# ymax = max(all$DEFENSE)
#
# dates = all %>%
#   select(year,week) %>%
#   distinct_all()  %>%
#   mutate_at(vars(year,week),as.integer) %>%
#   arrange(year,week) %>%
#   mutate(date = 1:n())
#
# all = left_join(all,dates)
#
# fdate = function(date){
#   x = as.integer(date)
#   row = dates %>%
#     filter(date == x)
#   return(paste("year",row$year,"week",row$week))
# }
# # fdate(date)
# a = all %>%
#   # filter(team %in% c("BYU","Utah")) %>%
#   ggplot(aes(x = OFFENSE,y = DEFENSE)) +
#   geom_image(aes(image = alphaImg75),size = .065, by = 'width') +
#   theme_gdocs() +
#   labs(title = "**SP+** Week {fdate(frame_time)}",
#        x = "<img src='arrowO.png' width='300'>",
#        y = "<img src='arrowD.png' width='300'>") +
#   theme(text = element_text(size = 36),
#         plot.title = element_markdown(),
#         axis.title.x = element_markdown(),
#         axis.title.y = element_markdown(),
#         plot.background = element_rect(color = NA)) +
#   scale_y_reverse() +
#   coord_equal(clip = 'off') +
#   xlim(c(xmin,xmax)) +
#   ylim(c(ymax,ymin)) +
#   transition_time(date)
# animate(a, nframes = 1000, fps = 15, height = 900, width = 1350, end_pause = 30, renderer = gganimate::gifski_renderer(glue::glue("figures/CFBEfficiency-allweeks.gif")))

# # combine all
# ls = list.files("data",pattern = "spplus_updated.xlsx", full.names = T)
# combined = map_df(ls,rio::import)
# rio::export(combined,"data/combined.xlsx")
# conferenceMatching = combined %>%
#   mutate(time = paste0(year,"-",week),
#          id = paste0(TEAM,'_',year,'_',week)) %>%
#   filter(time %in% c("2019-14","2020-14","2021-14","2022-14","2023-3"))
# conferenceMatching$time %>% unique
# export(conferenceMatching,"data/conferenceMatching.xlsx")
