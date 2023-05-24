rm(list=ls())

# load libraries
pacman::p_load(tidyverse, rio, ggsvg, extrafont, ggtext, showtext, htmltools, glue, viridis, hrbrthemes)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rubik Beastly")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'>  Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #SuperRugbyPacific @R13")


rugby_url <- "https://www.svgrepo.com/download/407364/rugby-football.svg"
svg_txt <- paste(readLines(rugby_url), collapse = "\n")
grid::grid.draw( svg_to_rasterGrob(svg_txt))

# Import/Load data sets
Tries_Players <- import("super_rugby_2023_tries_R13.xlsx", which="Tries_Players")
Tries_Team <- import("super_rugby_2023_tries_R13.xlsx", which="Tries_Team")
Pen_Try <- import("super_rugby_2023_tries_R13.xlsx", which="Pen_Try")

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
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  rename("No_of_Tries" = "count")

Top_Tries_Scorers = Tries_Players_1 |>
  head(11)

Top_Tries_Scorers1 <- Top_Tries_Scorers |>
  mutate(
    Player = factor(Player),
    Player = fct_reorder(Player, No_of_Tries))

Top_Tries_Scorers1 <- map_dfr(Top_Tries_Scorers1$Player, ~{
  tibble(
    Player = .x,
    x = 1:Top_Tries_Scorers1$No_of_Tries[Top_Tries_Scorers1$Player == .x]
  )
})

# Plot
Top_Tries_Scorers1 |>
  ggplot() +
  geom_point_svg(aes(x, Player), svg = svg_txt, size = 5.5) +
  geom_text(aes(1, as.numeric(Player)+0.5, label = glue("{Player}")),  
            family = 'Roboto Condensed', size = 5.5, 
            colour = "#f49d0c", lineheight = 0.2) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(1.2, 1)) +
  labs(
    x = "",
    y = "",
    title = 'SUPER RUGBY PACIFIC TOP-TRY SCORERS',
    subtitle = "At the conclusion of the 13th round, Shaun Stevenson of the Chiefs Rugby team &<br>Leicester Fainga'anuku of the Crusaders Rugby team are joint leaders of the try scorer chart,<br>having scored 10 tries each<br>",
    caption = cap
  ) +
  theme_modern_rc() +
  theme(
    text = element_text(family = 'Roboto Condensed', colour = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22, colour = 'white', 
                              family = "Rubik Beastly"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 14, color = "#f5f9c8"),
    plot.caption = element_markdown(colour = 'white', hjust = 0.5, size = 11,
                                    family = 'Rosario', margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 20, r = 50, l = 50)
  )

# Save plot
ggsave("super_rugby_2023_top_tries_scorers_R13_svg.png", width = 10, height = 9)


#@@@@@@@@@@@@@@@@@@@@@@@@ Plot 2 @@@@@@@@@@@@@@@@@@@@@@@@#
top_players <- Tries_Players_1 |>
  arrange(desc(No_of_Tries), Player) |>
  group_by(Team) |>
  slice_max(n = 4, with_ties = TRUE, order_by = No_of_Tries) |> 
  ungroup() |>
  pull(Player)

p = Tries_Players_1[Tries_Players_1$Player %in% top_players, ]
p <- p |>
  arrange(No_of_Tries) |>
  mutate(Player = fct_inorder(Player),
         Team = fct_inorder(factor(Team)))

# Create bar plot
ggplot(p, aes(x = Player, y = No_of_Tries, fill = Position)) +
  geom_bar(position="dodge", show.legend = FALSE, stat="identity") +
  geom_text(aes(label = No_of_Tries), vjust = 0.5, size = 5, colour = "white", nudge_y = 0.8) +
  scale_fill_manual(values = c("#d0f7fd","#0c9eae")) +
  facet_wrap(~Team, scales = "free_y", strip.position = "top", ncol = 3) +
  coord_flip() +
  labs(title = "SUPER RUGBY PACIFIC 2023: TOP-TRY SCORERS BY TEAM",
       subtitle = "<span style='color:#0c9eae;'>**Forward**</span> : <span style='color:#d0f7fd;'>**Back**</span>",
       x = "", 
       y = "",
       caption = cap) +
  theme_modern_rc() +
  theme(
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 20, colour = "#d5eb07"),
        legend.position = "none",
        plot.title = element_text(size = 20, hjust = 0.5, family = "Rubik Beastly"),
        plot.subtitle = element_markdown(hjust = 0.5, size = 20, color = "white"),
        plot.caption = element_markdown(colour = 'white', hjust = 0.5, size = 14,
                                        family = 'Rosario', margin = margin(t = 20)),
        plot.margin = margin(b = 10, t = 20, r = 10, l = 10))

# Save plot
ggsave("super_rugby_2023_top_tries_scorers_team_R13.png", width = 13, height = 12)

