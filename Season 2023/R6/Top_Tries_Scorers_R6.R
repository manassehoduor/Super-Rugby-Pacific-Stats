rm(list=ls())

# load libraries
pacman::p_load(knitr, tidyverse, rio, gt, gtExtras, extrafont, ggtext, showtext, htmltools, patchwork, ggpath, webshot2)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import/Load data sets
Tries_Players <- import("Tries_R6.xlsx", which="Tries_Players")
Tries_Team <- import("Tries_R6.xlsx", which="Tries_Team")
Pen_Try <- import("Tries_R6.xlsx", which="Pen_Try")

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
  mutate(color = case_when(Position == "Forward"~'#65e8e8', TRUE~'#4d0377')) |>
  head(8)


title = "<span style='font-size:24pt;'> SUPER RUGBY PACIFIC TOP TRY SCORERS</span>"
subtitle = "<span style='font-size:18pt;'> At the conclusion of the 6th round, Shaun Stevenson, Leicester Fainga'anuku, and Jordan Petaia are <br> currently tied as the top try scorers, with each having scored 7 tries </span>"

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'>.</span>",
               "<span style='font-family:sans;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #SuperRugbyPacific @R6")
# Labels
labels = paste0("<span style='font-size:15pt;'><b>", Top_Tries_Scorers$Player, "</b><br></span>",
                "<span style='font-size:14pt;color:#d98e27;'><b>", Top_Tries_Scorers$Team, "</b><br></span>",
                "<span style='font-size:13pt;'><b>", Top_Tries_Scorers$No_of_Tries, "</span>")

#@--------- plot 1
ggplot(Top_Tries_Scorers) +
  geom_col(aes(y=reorder(Player, No_of_Tries), x=No_of_Tries, fill=color)) +
  geom_richtext(aes(y=reorder(Player, No_of_Tries), x=No_of_Tries, label=labels, color=color),
                hjust=1, fill=NA, label.colour=NA, lineheight=0.5, family = "Roboto Condensed") +
  geom_segment(aes(x=-0.2, y=reorder(Player, No_of_Tries), xend=12, 
                   yend=reorder(Player, No_of_Tries)), color="NA") +
  scale_fill_manual(values=c('#33415C', 'orange')) +
  scale_color_manual(values=c('white', 'black')) +
  labs(title = title, subtitle = subtitle, caption = cap) +
  theme_void() +
  theme(
        plot.margin = margin(t=10, b=5),
        plot.title = element_markdown(hjust=.5, family = "Roboto Condensed",  color = "white"),
        plot.subtitle = element_markdown(hjust=.5, family = "Roboto Condensed",  color = "white"),
        plot.caption = element_markdown(size=11, hjust=.97,  color = "white"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1a1a1a", color = NA),
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank())+
  annotate(
    geom = "curve", x = 5, y = 3, xend = 6.0, yend = 3.1, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm")), color = 'white'
  ) +
  annotate(geom = "text", x = 6.1, y = 3, label = "Tevita Ikanivere is the only Forward player\n in the top try scorers list  with 5 tries", 
           hjust = "left", color = "#f6ff05", size = 5, family = "Roboto Condensed")

# Save plot
ggsave("super_rugby_2023_top_tries_scorers_R6.png", width = 10, height = 7.5)


#@--------- Table 1
Team_Tries <- Tries_Team |>
  group_by(Team) |>
  summarise(Tries = sum(Try)) |> 
  arrange(desc(Tries))

# Back/Forward Table
Back_Forward_Tries = Tries_Players |>
  group_by(Position, Team) |>
  tally() |>
  spread(Position, n) 
  
Back_Forward_Tries_P = left_join(Team_Tries, Back_Forward_Tries, by = "Team")

Back_Forward_Tries_P_List = Back_Forward_Tries_P |>
  mutate(list_data = list(c(Back, Forward)))

# Reorganize list data
# create list_data column for table graphic
Back_Forward_Tries_P_List_data = Back_Forward_Tries_P_List |> 
  gather(attr_num, list_data, c(Back, Forward)) |>
  group_by_at(vars(-attr_num, -list_data)) |>
  summarise(list_data = list(list_data),.groups = "drop")


# Add logo
Back_Forward_Tries_P_List_data <- Back_Forward_Tries_P_List_data |>
  mutate(Logo = Team) |>
  arrange(desc(Tries))


# Teams Logo
blues_logo <- local_image('logos/blues_logo.png', height = 20)
brumbies_logo <- local_image('logos/brumbies_logo.png', height = 20)
chiefs_logo <- local_image('logos/chiefs_logo.png', height = 20)
crusaders_logo <- local_image('logos/crusaders_logo.png', height = 20)
fijian_drua_logo <- local_image('logos/fijian_drua_logo.png', height = 20)
force_logo <- local_image('logos/force_logo.png', height = 20)
highlanders_logo <- local_image('logos/highlanders_logo.png', height = 20)
hurricanes_logo <- local_image('logos/hurricanes_logo.png', height = 20)
moana_pasifika_logo <- local_image('logos/moana_pasifika_logo.png', height = 20)
rebels_logo <- local_image('logos/rebels_logo.png', height = 20)
reds_logo <- local_image('logos/reds_logo.png', height = 20)
waratahs_logo <- local_image('logos/waratahs_logo.png', height = 20)


# reorder the columns
Back_Forward_Tries_P_List_data <- Back_Forward_Tries_P_List_data[,c(1,4, 2:3)]

# Table
Back_Forward_Tries_tbl = Back_Forward_Tries_P_List_data %>%
  gt() %>%
  gt_theme_538() %>% 
  gt::data_color(
    columns = Tries, colors = c("white", "orange")) %>% 
  cols_width(
    Team ~ px(120),
    Tries ~ px(40),
    Logo ~ px(50)) %>% 
  tab_header(title = md("**SUPER RUGBY PACIFIC**"), subtitle = md("*2023 Season:* Aggregate tries scored by the team, with a breakdown of back and forward tries, after *Round 6*")) %>%
  cols_label(Team = "Team", Logo = "", Tries = "Tries", list_data = "Backs/Forwards Tries") %>%
  tab_options(
    table.font.size = "14px",
    table.width = "470px",
    column_labels.font.weight = "bolder",
    column_labels.background.color = "#af7058",
    column_labels.padding = "8px",
    table.border.top.width = "1px",
    table.border.top.color = "black",
    table.border.bottom.width = "1px",
    table.border.bottom.color = "black",
    table.border.left.width = "1px",
    table.border.left.color = "black",
    table.border.right.width = "1px",
    table.border.right.color = "black",
    data_row.padding = "3px",
    heading.title.font.size = "20px",
    heading.subtitle.font.size = "13px",
    heading.title.font.weight = "bold",
    heading.padding = "7px",
    source_notes.font.size = 8,
    source_notes.background.color = "#edede7",
  ) %>% 
  gt_plt_bar_stack(list_data,
                   width = 60,
                   labels = c("  Backs  ", "  Forwards  "),
                   palette = c("#590696", "#212529")) %>% 
  text_transform(
    locations = cells_body(columns = Logo),
    fn = function(x) {
      paste0(
        dplyr::case_when(
          x ==  'Blues' ~ blues_logo, 
          x == "Brumbies"  ~ brumbies_logo,
          x == 'Chiefs' ~ chiefs_logo,
          x == 'Crusaders' ~ crusaders_logo,
          x == 'Fijian Drua' ~ fijian_drua_logo,
          x == 'Force' ~ force_logo,
          x == 'Highlanders' ~ highlanders_logo,
          x == 'Hurricanes' ~ hurricanes_logo,
          x == 'Moana Pasifika' ~ moana_pasifika_logo,
          x == 'Rebels' ~ rebels_logo,
          x == 'Reds' ~ reds_logo,
          x == 'Waratahs' ~ waratahs_logo))
    }
  ) %>% 
  cols_align(
    align = "center",
    columns = Tries) %>% 
  tab_source_note(
    source_note = md("**Table:** Manasseh Oduor | **Source:** super.rugby | **#SuperRugbyPacific:** @R6"))


new_gt_save <- function (data, filename, path = NULL, ..., zoom = 2, expand = 5) 
{
  filename <- gt:::gtsave_filename(path = path, filename = filename)
  tempfile_ <- tempfile(fileext = ".html")
  tempfile_ <- tempfile_ %>% gt:::tidy_gsub("\\\\", "/")
  gt:::gt_save_html(data = data, filename = tempfile_, path = NULL)
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("The `webshot2` package is required for saving images of gt tables.", 
         call. = FALSE)
  }
  else {
    webshot2::webshot(url = paste0("file:///", tempfile_), 
                      file = filename, selector = "table", zoom = zoom, 
                      expand = expand, ...)
  }
}

# Save table
new_gt_save(Back_Forward_Tries_tbl, "super_rugby_2023_B_F_tries_R6.png")




#@--------- Table 2
Players_Tries_P <- Tries_Players |>
  select(Team, Time)

Tries_Pen <- Pen_Try |>
  select(Team, Time)
  
Players_Tries_P_combine = rbind(Players_Tries_P, Tries_Pen)

# First/Second Half Table
First_Second_Half_Tries = Players_Tries_P_combine |>
  group_by(Time, Team) |>
  tally() |>
  spread(Time, n) 

colnames(First_Second_Half_Tries)[2] <- "First_Half"
colnames(First_Second_Half_Tries)[3] <- "Second_Half"

First_Second_Half_Tries <- First_Second_Half_Tries |>
  mutate(Tries = (First_Half+Second_Half))

First_Second_Half_Tries_List = First_Second_Half_Tries |>
  mutate(list_data = list(c(First_Half, Second_Half)))

# Reorganize list data
# create list_data column for table graphic
First_Second_Half_Tries_List_data = First_Second_Half_Tries_List %>% 
  gather(attr_num, list_data, c(First_Half, Second_Half)) %>%
  group_by_at(vars(-attr_num, -list_data)) %>%
  summarise(list_data = list(list_data),.groups = "drop")


# Add logo
First_Second_Half_Tries_List_data <- First_Second_Half_Tries_List_data |>
  mutate(Logo = Team) |>
  arrange(desc(Tries))

# reorder the columns
First_Second_Half_Tries_List_data <- First_Second_Half_Tries_List_data[,c(1,4, 2:3)]

# Table
First_Second_Half_Tries_tbl = First_Second_Half_Tries_List_data %>% 
  gt() %>%
  gt_theme_538() %>% 
  gt::data_color(
    columns = Tries, colors = c("white", "#027a5c")) %>% 
  cols_width(
    Team ~ px(120),
    Tries ~ px(40),
    Logo ~ px(50)) %>% 
  tab_header(title = md("**SUPER RUGBY PACIFIC**"), subtitle = md("*2023 Season:* Aggregate tries scored by the team, with a breakdown of 1st Half and 2nd Half tries, after *Round 6*")) %>%
  cols_label(Team = "Team", Logo = "", Tries = "Tries", list_data = "First Half/Second Half Tries") %>%
  tab_options(
    table.font.size = "14px",
    table.width = "470px",
    column_labels.font.weight = "bolder",
    column_labels.background.color = "#22af01",
    column_labels.padding = "8px",
    table.border.top.width = "1px",
    table.border.top.color = "black",
    table.border.bottom.width = "1px",
    table.border.bottom.color = "black",
    table.border.left.width = "1px",
    table.border.left.color = "black",
    table.border.right.width = "1px",
    table.border.right.color = "black",
    data_row.padding = "3px",
    heading.title.font.size = "20px",
    heading.subtitle.font.size = "13px",
    heading.title.font.weight = "bold",
    heading.padding = "7px",
    source_notes.font.size = 8,
    source_notes.background.color = "#edede7",
  ) %>% 
  gt_plt_bar_stack(list_data,
                   width = 60,
                   labels = c("  First Half  ", "  Second Half  "),
                   palette = c("#073102", "#0e066f")) %>% 
  text_transform(
    locations = cells_body(columns = Logo),
    fn = function(x) {
      paste0(
        dplyr::case_when(
          x ==  'Blues' ~ blues_logo, 
          x == "Brumbies"  ~ brumbies_logo,
          x == 'Chiefs' ~ chiefs_logo,
          x == 'Crusaders' ~ crusaders_logo,
          x == 'Fijian Drua' ~ fijian_drua_logo,
          x == 'Force' ~ force_logo,
          x == 'Highlanders' ~ highlanders_logo,
          x == 'Hurricanes' ~ hurricanes_logo,
          x == 'Moana Pasifika' ~ moana_pasifika_logo,
          x == 'Rebels' ~ rebels_logo,
          x == 'Reds' ~ reds_logo,
          x == 'Waratahs' ~ waratahs_logo))
    }
  ) %>% 
  cols_align(
    align = "center",
    columns = Tries) %>% 
  tab_source_note(
    source_note = md("**Table:** Manasseh Oduor | **Source:** super.rugby | **#SuperRugbyPacific:** @R6"))


new_gt_save <- function (data, filename, path = NULL, ..., zoom = 2, expand = 5) 
{
  filename <- gt:::gtsave_filename(path = path, filename = filename)
  tempfile_ <- tempfile(fileext = ".html")
  tempfile_ <- tempfile_ %>% gt:::tidy_gsub("\\\\", "/")
  gt:::gt_save_html(data = data, filename = tempfile_, path = NULL)
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("The `webshot2` package is required for saving images of gt tables.", 
         call. = FALSE)
  }
  else {
    webshot2::webshot(url = paste0("file:///", tempfile_), 
                      file = filename, selector = "table", zoom = zoom, 
                      expand = expand, ...)
  }
}

# Save table
new_gt_save(First_Second_Half_Tries_tbl, "super_rugby_2023_F_S_tries_R6.png")


