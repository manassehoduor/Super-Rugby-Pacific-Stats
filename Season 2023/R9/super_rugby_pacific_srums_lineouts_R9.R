rm(list=ls())

# load libraries
pacman::p_load(tidyverse, rio, extrafont, ggtext, showtext, glue)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'>.</span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: super.rugby | #SuperRugbyPacific @R9")

# load data
scrums_lineouts <- import("scrums_lineouts.xlsx", which="R9")

# data prep
scrums_lineouts <- scrums_lineouts |> 
  mutate(Team = factor(Team)) |>
  mutate(avg_scrum_lineouts = (LineOuts_Won+Scrums_Won)/2) |>
  arrange(desc(Pos))
  
# plot
  scrums_lineouts |>
    ggplot(aes(fct_inorder(Team))) +
    geom_col(aes(y = LineOuts_Won), fill = "#3d8d2c") +
    geom_col(aes(y = -Scrums_Won), fill = "#12369d") +
    geom_hline(yintercept = 0, col = "white") +
    ylim(c(-110,110)) +
    geom_text(aes(y=LineOuts_Won, label = glue("{LineOuts_Won}%")), vjust = 0.5, hjust = 2, 
              color = "white", size = 3, nudge_x = 0.15) +
    geom_text(aes(y=-Scrums_Won, label = glue("{Scrums_Won}%")), vjust = 0.5, hjust = -2, 
              color = "white", size = 3, nudge_x = -0.15) +
    geom_text(aes(y = -110, label = glue("{round(avg_scrum_lineouts, 1)}%")), 
              vjust = 0.5, color = "#6f5d5f", size = 3.5) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = 'SUPER RUGBY PACIFIC: SCRUMS & LINE-OUTS WON',
      subtitle = "<span style='color:#3d8d2c;'>**Lineout**</span> Brilliance: Queensland Reds rugby triumph with 90.1% Wins, Chiefs' <span style='color:#12369d;'>**Scrum**</span> Prowess with 89% success rate.<br>Chiefs Rugby reigns supreme leading the #SuperRugbyPacific in average lineouts and scrums victories (87.3%)<br>",
      caption = cap
    ) +
    theme_void() +
    theme(
      text = element_text(family = 'Roboto Condensed'),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      strip.text = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_markdown(face = "bold", hjust = 0.5, size = 18),
      plot.subtitle = element_markdown(hjust = 0.05, size = 12, color = "black"),
      plot.background = element_rect(fill = "white", colour = "white"),
      plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 10,
                                      family = 'Rosario', margin = margin(t = 20)),
      plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
    )
  
# Save plot
ggsave("super_rugby_2023_lineout_scrums_R9.png", width = 9.5, height = 6.5, bg = "#f7f6f6")



