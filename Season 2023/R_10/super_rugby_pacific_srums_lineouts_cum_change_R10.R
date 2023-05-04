
rm(list=ls())

# load libraries
pacman::p_load(tidyverse, rio, extrafont, ggtext, showtext, glue, janitor, ggrepel)


# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: super.rugby | #SuperRugbyPacific @R10")

# load data
scrums_lineouts_R9 <- import("scrums_lineouts.xlsx", which="R9")
scrums_lineouts_R10 <- import("scrums_lineouts.xlsx", which="R10")

# data wrangle
scrums_lineouts_R9 <- scrums_lineouts_R9 |>
  mutate(round = rep(9,12)) |>
  clean_names()

scrums_lineouts_R10 <- scrums_lineouts_R10 |>
  mutate(round = rep(10,12)) |>
  clean_names()

# Bind the two data sets
team_success_rates <- bind_rows(scrums_lineouts_R10, scrums_lineouts_R9)

# Define the color palette
my_palette <- c("#CC0000", "#CCCCCC", "#1e40af")

#@@@@@@@@@@@@@@@@@@@@@ Lineouts @@@@@@@@@@@@@@@@@@@@@@#
# Calculate the percent change for all the teams and Assign colors based on the sign of the percent change
team_success_rates_change <- team_success_rates |>
  mutate(team = fct_inorder(factor(team))) |>
  group_by(team) |>
  summarize(success_rate_change = ((line_outs_won[round == 10] - line_outs_won[round == 9]) / line_outs_won[round == 9]) * 100) |>
  mutate(color = ifelse(success_rate_change > 0, my_palette[3], 
                        ifelse(success_rate_change < 0, my_palette[1], my_palette[2])))

# Create the plot: Lineouts
ggplot(team_success_rates, aes(x = fct_inorder(team), y = line_outs_won, fill = fct_inorder(team))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = team_success_rates_change, 
            aes(x = fct_inorder(team), y = 155, 
                label = paste0(ifelse(success_rate_change > 0, "↑",
                                      ifelse(success_rate_change < 0, "↓", "→")), "\n",
                               abs(round(success_rate_change, 2)), "%")), 
            color = team_success_rates_change$color, size = 4) +
  geom_label(data = team_success_rates|> filter(round==9),
             aes(label = paste0(round(line_outs_won, 1), "%")), 
             color = "white", fill = "#9ca3af", size = 4, nudge_y = 30) +
  geom_label(data = team_success_rates|> filter(round==10),
             aes(label = paste0(round(line_outs_won, 1), "%")), 
             color = "white", fill = "#4b5563", size = 4, nudge_y = 40) +
  scale_fill_manual(values = team_success_rates_change$color) +
  scale_y_continuous(breaks = seq(0, 100, 10), labels = function(x) paste0(x, "%")) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = 'Roboto Condensed'),
        axis.text.y = element_text(size = 13, family = 'Roboto Condensed'),
        axis.text.x = element_text(size = 13, family = 'Roboto Condensed',
                                   angle = -90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 13, face = "bold", family = 'Roboto Condensed'),
        strip.text.x = element_blank(),
        plot.title = element_text(face = "bold", color = "black", hjust = 0.5, 
                                  size = 22, family = 'Roboto Condensed'),
        plot.subtitle = element_markdown(hjust = 0.5, size = 15, color = "black", 
                                         family = 'Roboto Condensed'),
        plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 11,
                                        family = 'Rosario', margin = margin(t=5, b=5)),
        plot.margin = margin(b = 5, t = 40, r = 50, l = 50)) +
  labs(x = "", 
       y = "Cum. Lineout Success Rate",
       title = "SUPER RUGBY PACIFIC: LINE-OUTS SUCCESS RATE",
       subtitle = "Analysis of teams' cumulative success-rate in lineouts: Which teams are <span style='color:#CCCCCC;'>**Consistent**</span>, <span style='color:#1e40af;'>**Improving**</span> or <span style='color:#CC0000;'>**Declining**</span>?",
       caption = cap
       ) +
  annotate("text", x = team_success_rates$team[6], y = 105, family = "Roboto Condensed", 
           label = "Cum. lineout success-rate at round 9", size =3.5, colour = "#9ca3af") +
  annotate("segment", x = team_success_rates$team[6], 
           xend = team_success_rates$team[6], y = 117, yend = 106, colour = "#9ca3af",
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = team_success_rates$team[6], y = 140, family = "Roboto Condensed", 
           label = "Cum. lineout success-rate at round 10", size =3.5, colour = "#4b5563") +
  annotate("segment", x = team_success_rates$team[6], 
           xend = team_success_rates$team[6], y = 134, yend = 138, colour = "#4b5563",
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("segment", x = team_success_rates$team[10], 
           xend = team_success_rates$team[3], y = 144, yend = 144, colour = "#130101",
           arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  annotate("text", x = team_success_rates$team[6], y = 147, family = "Roboto Condensed", 
           label = "Teams arranged in order of their table ranking", size =3.5, colour = "#000000")

# Save plot
ggsave("super_rugby_2023_lineouts_cum_change_R10.png", width = 11, height = 10, bg = "#f7f6f6")



#@@@@@@@@@@@@@@@@@@@@@ Scrums @@@@@@@@@@@@@@@@@@@@@@#
# Define the color palette
my_palette <- c("#22c55e", "#CCCCCC", "#052e16")

# Calculate the percent change for all the teams and Assign colors based on the sign of the percent change
team_success_rates_change <- team_success_rates |>
  mutate(team = fct_inorder(factor(team))) |>
  group_by(team) |>
  summarize(success_rate_change = ((scrums_won[round == 10] - scrums_won[round == 9]) / scrums_won[round == 9]) * 100) |>
  mutate(color = ifelse(success_rate_change > 0, my_palette[3], 
                        ifelse(success_rate_change < 0, my_palette[1], my_palette[2])))

# Create the plot: Scrums
ggplot(team_success_rates, aes(x = fct_inorder(team), y = scrums_won, fill = fct_inorder(team))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = team_success_rates_change, 
            aes(x = fct_inorder(team), y = 155, 
                label = paste0(ifelse(success_rate_change > 0, "↑",
                                      ifelse(success_rate_change < 0, "↓", "→")), "\n",
                               abs(round(success_rate_change, 2)), "%")), 
            color = team_success_rates_change$color, size = 4) +
  geom_label(data = team_success_rates|> filter(round==9),
             aes(label = paste0(round(scrums_won, 1), "%")), 
             color = "white", fill = "#60a5fa", size = 4, nudge_y = 30) +
  geom_label(data = team_success_rates|> filter(round==10),
             aes(label = paste0(round(scrums_won, 1), "%")), 
             color = "white", fill = "#1d4ed8", size = 4, nudge_y = 40) +
  scale_fill_manual(values = team_success_rates_change$color) +
  scale_y_continuous(breaks = seq(0, 100, 10), labels = function(x) paste0(x, "%")) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    text = element_text(family = 'Roboto Condensed'),
    axis.text.y = element_text(size = 13, family = 'Roboto Condensed'),
    axis.text.x = element_text(size = 13, family = 'Roboto Condensed',
                               angle = -90, vjust = 0.5, hjust = 1),
    axis.title.y = element_text(size = 13, face = "bold", family = 'Roboto Condensed'),
    strip.text.x = element_blank(),
    plot.title = element_text(face = "bold", color = "black", hjust = 0.5, 
                              size = 22, family = 'Roboto Condensed'),
    plot.subtitle = element_markdown(hjust = 0.5, size = 15, color = "black", 
                                     family = 'Roboto Condensed'),
    plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 11,
                                    family = 'Rosario', margin = margin(t=5, b=5)),
    plot.margin = margin(b = 5, t = 40, r = 50, l = 50)) +
  labs(x = "", 
       y = "Cum. Scrums Success Rate",
       title = "SUPER RUGBY PACIFIC: SCRUMS SUCCESS RATE",
       subtitle = "Analysis of teams' cumulative success-rate in Scrums: Which teams are <span style='color:#CCCCCC;'>**Consistent**</span>, <span style='color:#052e16;'>**Improving**</span> or <span style='color:#22c55e;'>**Declining**</span>?",
       caption = cap
  ) +
  annotate("text", x = team_success_rates$team[7], y = 105, family = "Roboto Condensed", 
           label = "Cum. scrums success-rate at round 9", size =3.5, colour = "#60a5fa") +
  annotate("segment", x = team_success_rates$team[7], 
           xend = team_success_rates$team[7], y = 112, yend = 106, colour = "#60a5fa",
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = team_success_rates$team[7], y = 140, family = "Roboto Condensed", 
           label = "Cum. scrums success-rate at round 10", size =3.5, colour = "#1d4ed8") +
  annotate("segment", x = team_success_rates$team[7], 
           xend = team_success_rates$team[7], y = 130, yend = 138, colour = "#1d4ed8",
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("segment", x = team_success_rates$team[10], 
           xend = team_success_rates$team[3], y = 144, yend = 144, colour = "#130101",
           arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  annotate("text", x = team_success_rates$team[6], y = 147, family = "Roboto Condensed", 
           label = "Teams arranged in order of their table ranking", size =3.5, colour = "#000000")

# Save plot
ggsave("super_rugby_2023_scrums_cum_change_R10.png", width = 11, height = 10, bg = "#f7f6f6")

