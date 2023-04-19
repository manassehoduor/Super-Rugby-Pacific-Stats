rm(list=ls())

# load libraries
pacman::p_load(ggbump, tidyverse, ggimage, scales, extrafont, ggtext, 
               showtext, htmltools, magick, grid, png, ggpath)

# Font
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:#ffffff;'>.</span>",
               "<span style='font-family:Rosario;'>Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span> Manasseh_6 | #SuperRugbyPacific @R8")

# create a data frame with team rankings over time
super_rugby_2023_data <- read.csv(file = "super_rugby_2023_table_rankings.csv")

# Add team logos to the data frame
logo_path <- "C:/Users/ADMIN/Documents/R/#TidyTuesday/2023/#wk3 Art History/team_logos/"

# add the resized logos to the data frame
super_rugby_2023_data$logo <- paste0(logo_path, super_rugby_2023_data$Team, ".png")

# Define custom team colors for bump plot
team_colors <- c(
  "Blues" = "#a7f3d0",
  "Brumbies" = "#030712",
  "Hurricanes" = "#eab308",
  "Chiefs" = "#ca8a04",
  "Reds" = "#a7f3d0",
  "Force" = "#a7f3d0",
  "Waratahs" = "#a7f3d0",
  "Highlanders" = "#a7f3d0",
  "Crusaders" = "#cf3333",
  "Rebels" = "#a7f3d0",
  "Moana Pasifika" = "#a7f3d0",
  "Fijian Drua" = "#a7f3d0"
)

#  bump plot
super_rugby_rankings_bump_chart <- super_rugby_2023_data |>
  ggplot(aes(Round, Ranking)) +
  geom_point(aes(col = Team), size = 3) +
  geom_bump(aes(col = Team), size = 1) +
  geom_text(
    data = super_rugby_2023_data |>
      filter(Round == 1),
    aes(label = Team, col = Team),
    hjust = 1, nudge_x = -0.1, size = 4, fontface = "bold", family = "Roboto Condensed"
  ) +
  geom_text(
    data = super_rugby_2023_data |>
      filter(Round == 8),
    aes(label = Ranking, col = Team), hjust = 0, nudge_x = 0.1, size = 4, 
    fontface = "bold", family = "Roboto Condensed"
  ) +
  geom_from_path(data = super_rugby_2023_data |>
                   filter(Round == 8),
                 aes(x = 8.5, y=Ranking, path = logo), width = 0.05) +
  annotate(
    "text",
    x = c(1, 8),
    y = c(0.25, 0.25),
    label = c("Round 1", "Round 8"),
    hjust = c(0, 1), vjust = 1, size = 4, fontface = "bold", colour = "#3730a3", 
    family = "Roboto Condensed"
  ) +
  scale_y_reverse(position = "right", breaks = seq(16, 2, -2)) +
  scale_color_manual(values = team_colors) +
  coord_cartesian(xlim = c(0.05, 9), ylim = c(12.5, 0.05), expand = F) +
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
    plot.subtitle = element_text(family = "Roboto Condensed", colour = "#0e0e0b", size = 16,
                                 margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.5
    ),
    plot.title = element_text(
      face = "bold", colour = "#0e0e0b", family = "Roboto Condensed", size = 20, hjust = 0.5
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.4, colour = "#4a044e", linewidth = 9.5, 
      margin = margin(0,0,0,60,"mm"), family = "Rosario", size = 9.5
    )
  ) +
  labs(
    x = "",
    y = "",
    title = "\n SUPER RUGBY PACIFIC 2023",
    subtitle = "Weekly Evolution of the Standings following Rounds 8",
    caption = cap
  )

# Save plot
ggsave("super_rugby_rankings_2023_R8_.png", width = 8.5, height = 7.5, bg = "white")

