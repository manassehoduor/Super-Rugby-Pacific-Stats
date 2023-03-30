rm(list=ls())

# load libraries
pacman::p_load(knitr, tidyverse, rio, gt, gtExtras, extrafont, ggtext, showtext, htmltools, patchwork, ggpath)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import/Load data sets
Tries_Players <- import("Tries_R5.xlsx", which="Tries_Players")
Tries_Team <- import("Tries_R5.xlsx", which="Tries_Team")
Pen_Try <- import("Tries_R5.xlsx", which="Pen_Try")

# Data preparation
Tries_Players <- Tries_Players |>
  mutate(Team = factor(Team),
         Player = factor(Player),
         Position = factor(Position),
         Time = factor(Time),
         Round = factor(Round))

Tries_Players_1 <- Tries_Players |>
  select(Team, Player, Position) |>
  group_by(Player, Team, Position) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  rename("No_of_Tries" = "count")

Top_Tries_Scorers = Tries_Players_1 |>
  mutate(color = case_when(Position == "Forward"~'#65e8e8', TRUE~'#4d0377')) |>
  head(11)


title = "<span style='font-size:22pt;'> Super Rugby Pacific Top Try Scorers</span>"
subtitle = "<span style='font-size:15pt;'> After the 5th round, Chiefs fullback Shaun Stevenson is the top try scorer with 7 tries</span>"

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'>.</span>",
               "<span style='font-family:sans;'>**Manasseh**</span><br><br>",
               "<span style='font-family:fb;'>&#xf099; </span>**_Manasseh__** --> **#SuperRugbyPacific** *@Round5*")
# Labels
labels = paste0("<span style='font-size:11pt;'><b>", Top_Tries_Scorers$Player, "</b><br></span>",
                "<span style='font-size:10pt;'><b>", Top_Tries_Scorers$Team, "</b><br></span>",
                "<span style='font-size:9pt;'><b>", Top_Tries_Scorers$No_of_Tries, "</span>")

# plot
ggplot(Top_Tries_Scorers) +
  geom_col(aes(y=reorder(Player, No_of_Tries), x=No_of_Tries, fill=color)) +
  geom_richtext(aes(y=reorder(Player, No_of_Tries), x=No_of_Tries, label=labels, color=color),
                hjust=1, fill=NA, label.colour=NA, lineheight=.9, family = "Roboto Condensed") +
  scale_fill_manual(values=c('#33415C', 'orange')) +
  scale_color_manual(values=c('white', 'black')) +
  labs(title = title, subtitle = subtitle, caption = cap) +
  theme_void() +
  theme(
        plot.margin = margin(t=10, b=5),
        plot.title = element_markdown(hjust=.5, family = "Roboto Condensed",  color = "white"),
        plot.subtitle = element_markdown(hjust=.5, family = "Roboto Condensed",  color = "white"),
        plot.caption = element_markdown(size=10, hjust=.97, family = "Roboto Condensed",  color = "white"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1a1a1a", color = NA),
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank())+
  annotate(
    geom = "curve", x = 4, y = 7, xend = 5, yend = 6, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm")), color = 'white'
  ) +
  annotate(geom = "text", x = 5.1, y = 6, label = "Tevita Ikanivere is the only Forward player\n in the top try scorers list  with 4 tries", 
           hjust = "left", color = "#F7DB6A", size = 3.5, family = "Roboto Condensed")

# Save plot
ggsave("super_rugby_2023_Top_Tries_Scorers_R5.png", width = 10, height = 7.5)


