rm(list=ls())

# load libraries
pacman::p_load(ggbump, tidyverse, ggimage, scales, extrafont, ggtext, showtext, htmltools)

# Font
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# create a data frame with team rankings over time
super_rugby_2023_data <- read.csv(file = "super_rugby_2023_table_rankings.csv")

# Add team logos to the data frame
logo_path <- "team_logos/"
super_rugby_2023_data$logo <- paste0(logo_path, super_rugby_2023_data$Team, ".png")

# Define custom team colors for bump plot
team_colors <- c(
  "Blues" = "#362FD9",
  "Brumbies" = "#850000",
  "Hurricanes" = "#F1BF00",
  "Chiefs" = "#ffffff",
  "Reds" = "#DC0000",
  "Force" = "#BFACE2",
  "Waratahs" = "#7D6E83",
  "Highlanders" = "#F36C21",
  "Crusaders" = "#9fc610",
  "Rebels" = "#379237",
  "Moana Pasifika" = "#FEC260",
  "Fijian Drua" = "#159895"
)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:#ffffff;'>.</span>",
               "<span style='font-family:Roboto Condensed;'>**Manasseh**</span><br><br>",
               "<span style='font-family:fb;'>&#xf099; </span>**Manasseh_6** --> **#SuperRugbyPacific** **#R6**")
  
#  bump plot
super_rugby_rankings_bump_chart <- super_rugby_2023_data %>%
  ggplot(aes(Round, Ranking, col = Team)) +
  geom_point(size = 3) +
  geom_bump(size = 1) +
  geom_text(
    data = super_rugby_2023_data %>%
      filter(Round == 1),
    aes(label = Team),
    hjust = 1, nudge_x = -0.1, size = 4, fontface = "bold", family = "Roboto Condensed"
  ) +
  geom_text(
    data = super_rugby_2023_data %>%
      filter(Round == 6),
    aes(label = Ranking), hjust = 0, nudge_x = 0.1, size = 4, fontface = "bold", family = "Roboto Condensed"
  ) +
  annotate(
    "text",
    x = c(1, 6),
    y = c(0.25, 0.25),
    label = c("Round 1", "Round 6"),
    hjust = c(0, 1), vjust = 1, size = 3, fontface = "bold", colour = "#ffffff", family = "Roboto Condensed"
  ) +
  scale_y_reverse(position = "right", breaks = seq(16, 2, -2)) +
  scale_color_manual(values = team_colors) +
  coord_cartesian(xlim = c(0.05, 7), ylim = c(12.5, 0.05), expand = F) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(family = "Roboto Condensed", colour = "#ffffff", size = 14,
      margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.5
      ),
    plot.title = element_text(
      face = "bold", colour = "#ffffff", family = "Roboto Condensed", size = 16, hjust = 0.5
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.4, colour = "#ffffff", size = 9, 
      margin = margin(0,0,0,100,"mm"), family = "Roboto Condensed"
    )
  ) +
  labs(
    x = "",
    y = "",
    title = "The Evolution of the Super Rugby Leaderboard on a Weekly basis in 2023 Season",
    subtitle = "No. of Rounds Complete: 6",
    caption = cap
  )

# Save plot
ggsave("super_rugby_2023_rankings_R6.png", width = 10, height = 6, bg = "#222f04")

