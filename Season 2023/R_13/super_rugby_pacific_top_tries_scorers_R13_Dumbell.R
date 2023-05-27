
rm(list=ls())

# load libraries
pacman::p_load(tidyverse, rio, janitor, hrbrthemes, extrafont, ggtext, showtext, htmltools, glue, png, ggpath)

# Font
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rubik Beastly")
font_add_google(name = "Barriecito")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #SuperRugbyPacific @R13")


# Import/Load data sets
Tries_Players <- import("super_rugby_2023_tries_R13.xlsx", which="Tries_Players")
Tries_Team <- import("super_rugby_2023_tries_R13.xlsx", which="Tries_Team")
Pen_Try <- import("super_rugby_2023_tries_R13.xlsx", which="Pen_Try")

# Wrangle data
# Back/Forward Table
Team_Tries <- Tries_Team |>
  group_by(Team) |>
  summarise(Tries = sum(Try)) |> 
  arrange(desc(Tries))

Back_Forward_Tries = Tries_Players |>
  group_by(Position, Team) |>
  tally() 

Back_Forward_Tries_P = Back_Forward_Tries |>
  pivot_wider(names_from = Position, values_from = n)

# Add team logos to the data frame
logo_path <- "C:/Users/ADMIN/Documents/R/#TidyTuesday/2023/#wk3 Art History/team_logos/"

# add the resized logos to the data frame
Back_Forward_Tries_P$logo <- paste0(logo_path, Back_Forward_Tries_P$Team, ".png")

# First/Second Half Table
Players_Tries_P <- Tries_Players |>
  select(Team, Time)

Tries_Pen <- Pen_Try |>
  select(Team, Time)

Players_Tries_P_combine = rbind(Players_Tries_P, Tries_Pen)

First_Second_Half_Tries = Players_Tries_P_combine |>
  group_by(Time, Team) |>
  tally() |>
  spread(Time, n) 

# add the resized logos to the data frame
First_Second_Half_Tries$logo <- paste0(logo_path, First_Second_Half_Tries$Team, ".png")

# Dumbbells Plot: Backs/Forwards Try split
Back_Forward_Tries_P |>
  mutate(d = Back - Forward) |>
  arrange(d) |>
  ggplot() +
  geom_segment(aes(x= fct_inorder(Team), xend=fct_inorder(Team), 
                   y=Forward, yend=Back), color="#f0c808", linewidth = 1) +
  geom_point(aes(x=Team, y=Forward), color= "black",
             size = 3.5, shape = 21, fill = "#080ca7", stroke = 1, show.legend = FALSE) +
  geom_point(aes(x=Team, y=Back), color= "black",
             size = 3.5, shape = 21, fill = "#4d3407", stroke = 1, show.legend = FALSE) +
  geom_text(aes(Team, y = Forward,
                label = glue("F: {Forward} Tries")), family = "Roboto Condensed", 
            size = 3, vjust = -2, hjust = 1, colour = "#080ca7") +
  geom_text(aes(Team, y = Back,
                label = glue("Backs: {Back} Tries")), family = "Roboto Condensed", 
            size = 3, vjust = -2, hjust = -0.2, colour = "#4d3407") +
  geom_from_path(aes(x = Team, y=4, path = logo), width = 0.05) +
  ggthemes::scale_fill_tableau(name=NULL) +
  scale_y_continuous(limits = c(4,50)) +
  coord_flip() +
  labs(
    caption = cap,
    x = "",
    y = "No. of Tries",
    subtitle = "Total Tries Scored by <span style='color:#4d3407;'>**Backs**</span> & <span style='color:#080ca7;'>**Forwards**</span> after Round 13<br>",
    title = "SUPER RUGBY PACIFIC"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(family = "Rubik Beastly", face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_markdown(family = "Roboto Condensed", size = 13, 
                                     hjust = 0.5, colour = "#e02222"),
    panel.grid = element_blank(),
    axis.title.y = element_text(family = "Roboto Condensed", face = "bold", size = 12, 
                                vjust = 1, hjust = 0.5),
    axis.text.x = element_text(family = "Roboto Condensed", size = 12, colour = "black"),
    axis.text.y = element_text(family = "Barriecito", size = 12, face = "bold", colour = "#131211"),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.caption = element_markdown(colour = '#080ca7', hjust = 0.5, size = 9,
                                    family = 'Rosario', margin = margin(t = 10, b = 5)),
    plot.margin = margin(b = 5, t = 20, r = 20, l = 0))

# Save plot
ggsave("super_rugby_pacific_team_tries_R13_dumbell.png", 
       width = 8, height = 7.5, bg = "white")


# Dumbbells Plot: First-Half/Second-Half Try split
First_Second_Half_Tries |>
  clean_names() |>
  mutate(d = abs(first_half-second_half)) |>
  arrange(d) |>
  ggplot() +
  geom_segment(aes(x= fct_inorder(team), xend=fct_inorder(team), 
                   y=first_half, yend=second_half), color="#3f1a09", linewidth = 1) +
  geom_point(aes(x=team, y=first_half), color= "black",
             size = 3.5, shape = 21, fill = "#e09320", stroke = 1, show.legend = FALSE) +
  geom_point(aes(x=team, y=second_half), color= "black",
             size = 3.5, shape = 21, fill = "#1acbc8", stroke = 1, show.legend = FALSE) +
  geom_text(data = First_Second_Half_Tries |> 
              clean_names() |>
              filter(team == 'Hurricanes'),
    aes(team, y = first_half,
                label = glue("1st-half: {first_half} Tries")), family = "Roboto Condensed", 
            size = 3, vjust = -2, hjust = 0.5, colour = "#e09320") +
  geom_text(data = First_Second_Half_Tries |>
              clean_names() |>
              filter(team == 'Hurricanes'),
    aes(team, y = second_half,
                label = glue("2nd-half: {second_half} Tries")), family = "Roboto Condensed", 
            size = 3, vjust = -2, hjust = 0.5, colour = "#1acbc8") +
  geom_from_path(aes(x = team, y=9, path = logo), width = 0.05) +
  ggthemes::scale_fill_tableau(name=NULL) +
  scale_y_continuous(limits = c(9,45), breaks = c(14,21, 28,35,42)) +
  coord_flip() +
  labs(
    caption = cap,
    x = "",
    y = "No. of Tries",
    subtitle = "Total Tries Scored in <span style='color:#e09320;'>**First-half**</span> & <span style='color:#1acbc8;'>**Second-Half**</span> after Round 13<br>",
    title = "SUPER RUGBY PACIFIC"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(family = "Rubik Beastly", face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_markdown(family = "Roboto Condensed", size = 13, 
                                     hjust = 0.5, colour = "#131211"),
    panel.grid = element_blank(),
    axis.title.y = element_text(family = "Roboto Condensed", face = "bold", size = 12, 
                                vjust = 1, hjust = 0.5),
    axis.text.x = element_text(family = "Roboto Condensed", size = 12, colour = "black"),
    axis.text.y = element_text(family = "Barriecito", size = 12, face = "bold", colour = "#131211"),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.caption = element_markdown(colour = '#080ca7', hjust = 0.5, size = 9,
                                    family = 'Rosario', margin = margin(t = 10, b = 5)),
    plot.margin = margin(b = 5, t = 20, r = 20, l = 0))

# Save plot
ggsave("super_rugby_pacific_team_tries_R13_dumbell2.png", 
       width = 8, height = 7.5, bg = "white")


