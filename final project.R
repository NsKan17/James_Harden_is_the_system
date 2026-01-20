library(ggplot2)
library(janitor)
library(ggrepel)
library(tidyverse)
library(ggiraph)
abr <- read.csv("nba_draft.csv")
nba_teams <- tibble(
  team_name = c(
    "Boston Celtics", "Brooklyn Nets", "New York Knicks", "Philadelphia 76ers", "Toronto Raptors",
    "Chicago Bulls", "Cleveland Cavaliers", "Detroit Pistons", "Indiana Pacers", "Milwaukee Bucks",
    "Atlanta Hawks", "Charlotte Hornets", "Miami Heat", "Orlando Magic", "Washington Wizards",
    "Denver Nuggets", "Minnesota Timberwolves", "Oklahoma City Thunder", "Portland Trail Blazers", "Utah Jazz",
    "Golden State Warriors", "Los Angeles Clippers", "Los Angeles Lakers", "Phoenix Suns", "Sacramento Kings",
    "Dallas Mavericks", "Houston Rockets", "Memphis Grizzlies", "New Orleans Pelicans", "San Antonio Spurs"
  ),
  acronym = c(
    "BOS", "BKN", "NYK", "PHI", "TOR",
    "CHI", "CLE", "DET", "IND", "MIL",
    "ATL", "CHA", "MIA", "ORL", "WAS",
    "DEN", "MIN", "OKC", "POR", "UTA",
    "GSW", "LAC", "LAL", "PHX", "SAC",
    "DAL", "HOU", "MEM", "NOP", "SAS"
  )
)
write_csv(nba_teams, "nba_teams.csv")

team<- read.csv("test.csv")

team_analysis <-left_join(team, nba_teams, by = c("Team" = "team_name")) |>
  clean_names()
team_analysis<-team_analysis|>
  mutate(rank_ortg = min_rank(desc(o_rtg)))|>
  mutate(rank_drtg = min_rank(d_rtg))

y1 <- 116.8
y2 <- 111.9

x1 = 111
x2 = 110.4
set.seed(2)
x <- team_analysis|>
  ggplot(aes(x = d_rtg, y = o_rtg )) +
  geom_point()+
  geom_hline(yintercept = 114.5, linetype = "dotted")+
  geom_vline(xintercept = 114.5, linetype = "dotted")+
  geom_point(aes(x = x1, y = y1 ), color = "blue")+
  annotate("text", x=109.4, y=116.8, label = "LAC w/ Harden", color = "blue")+
  geom_point(aes(x = x2, y = y2), color = "red")+
  annotate("text", x=108.8, y=111.7, label = "LAC wo/ Harden", color = "red")+
  geom_text_repel(aes(label = acronym ), force = 2) + 
  scale_x_reverse()+
  annotate_interactive(
    "rect", 
    xmin = 114.5, xmax = Inf, ymin = -Inf, ymax = 114.5,
    fill = "red", alpha = 0.1,
    tooltip = "Poor Defense & Poor Offense",
    data_id = "bottom_right"
  ) +
  annotate_interactive(
    "rect",
    xmin = Inf, xmax = 114.5, ymin = 114.5, ymax = Inf,
    fill = "yellow", alpha = 0.1,
    tooltip = "Good Offense, Poor Defense",
    data_id = "top_right"
  ) +
  annotate_interactive(
    "rect",
    xmin = -Inf, xmax = 114.5, ymin = 114.5, ymax = Inf,
    fill = "green", alpha = 0.1,
    tooltip = "Elite: Good Defense & Good Offense",
    data_id = "top_left"
  ) +
  annotate_interactive(
    "rect",
    xmin = -Inf, xmax = 114.5, ymin = -Inf, ymax = 114.5,
    fill = "blue", alpha = 0.1,
    tooltip = "Good Defense, Poor Offense",
    data_id = "bottom_left"
  )
girafe(ggobj = x)   


player <- read.csv("player.csv") |>
  clean_names()

player<- player |>
  filter(g >= 41)|>
  group_by(player) |>
  filter(if(any(team == "2TM")) team == "2TM" else TRUE) %>%
  ungroup()

player |>
  ggplot(aes(x = ts, y = usg)) +
  geom_point(aes(color = player == "James Harden")) +
  geom_text_repel(
    data = subset(player, player %in% c("James Harden")),
    aes(label = player),
    nudge_x = 0.03,              
    nudge_y = 3,
    color = "red",             
    segment.color = "red",
  ) +
  geom_text_repel(
    data = subset(player, player %in% c("Stephen Curry")),
    aes(label = player),
    nudge_y = 1.7,
    nudge_x = 0.04
  ) + 
  geom_text_repel(
    data = subset(player, player %in% c("LeBron James")),
    aes(label = player),
    nudge_x = 0.05,
    nudge_y = 0.5
  ) +
  geom_text_repel(
    data = subset(player, player %in% c("Alex Sarr")),
    aes(label = player),
    nudge_x = -0.005
  ) +
  geom_text_repel(
    data = subset(player, player %in% c("Jaxson Hayes")),
    aes(label = player),
    nudge_y = -1
  ) +
  geom_text_repel(
    data = subset(player, player %in% c("Gary Harris")),
    aes(label = player),
    nudge_y = -1
  )+
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none")



  
player |>
  ggplot(aes(x = ws)) +
  geom_histogram(aes(fill = after_stat(count)), bins = 25, color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()+
  geom_vline(xintercept = 8.3, color = "red")+
  annotate("text", x=11, y=30, label = "James Harden", color = "red")
  





