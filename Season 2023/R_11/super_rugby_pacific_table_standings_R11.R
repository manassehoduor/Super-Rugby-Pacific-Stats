rm(list=ls())

# load libraries
pacman::p_load(knitr, tidyverse, gt, gtExtras)

# create a dataframe with team names and initial values
teams <- c("Blues", "Chiefs", "Brumbies", "Reds", "Hurricanes", "Highlanders", 
           "Crusaders", "Fijian Drua", "Force", "Waratahs", "Rebels", "Moana Pasifika")

W <- rep(0, length(teams))
D <- rep(0, length(teams))
L <- rep(0, length(teams))
PD <- rep(0, length(teams))
Pts <- rep(0, length(teams))
P <- rep(0, length(teams))
Pos <- 1:length(teams)

table_data <- data.frame(Team = teams, P = P, W = W, D = D, L = L, PD = PD, Pts = Pts)

# create a function to update the table after each match
update_table <- function(home_team, away_team, home_score, away_score, home_tries, away_tries, table_data) {
  # find the indices of the home and away teams in the data frame
  home_index <- which(table_data$Team == home_team)
  away_index <- which(table_data$Team == away_team)
  
  # update the P/W/L/PD/Pts values for the home team
  if (home_score > away_score) {
    table_data$W[home_index] <- table_data$W[home_index] + 1
    table_data$L[away_index] <- table_data$L[away_index] + 1
    table_data$PD[home_index] <- table_data$PD[home_index] + (home_score - away_score)
    table_data$PD[away_index] <- table_data$PD[away_index] - (home_score - away_score)
    table_data$Pts[home_index] <- table_data$Pts[home_index] + 4
    if (abs(home_score - away_score) <= 7) {
      table_data$Pts[away_index] <- table_data$Pts[away_index] + 1
    }
    if (home_tries - away_tries >= 3) {
      table_data$Pts[home_index] <- table_data$Pts[home_index] + 1
    }
  } else if (home_score < away_score) {
    table_data$L[home_index] <- table_data$L[home_index] + 1
    table_data$W[away_index] <- table_data$W[away_index] + 1
    table_data$PD[home_index] <- table_data$PD[home_index] - (away_score - home_score)
    table_data$PD[away_index] <- table_data$PD[away_index] + (away_score - home_score)
    if (abs(away_score - home_score) <= 7) {
      table_data$Pts[home_index] <- table_data$Pts[home_index] + 1
    }
    table_data$Pts[away_index] <- table_data$Pts[away_index] + 4
    if (away_tries - home_tries >= 3) {
      table_data$Pts[away_index] <- table_data$Pts[away_index] + 1
    }
  } else {
    table_data$D[home_index] <- table_data$D[home_index] + 1
    table_data$D[away_index] <- table_data$D[away_index] + 1
    table_data$Pts[home_index] <- table_data$Pts[home_index] + 2
    table_data$Pts[away_index] <- table_data$Pts[away_index] + 2
  }
  
  # update games played for both teams
  table_data$P[home_index] <- table_data$P[home_index] + 1
  table_data$P[away_index] <- table_data$P[away_index] + 1
  
  # sort the table by Pts, Wins (W) then PD
  table_data <- table_data[order(-table_data$Pts, -table_data$W, -table_data$PD),]
  rownames(table_data) <- NULL
  return(table_data)
}

# Round 1
table_data <- update_table("Crusaders", "Chiefs", 10, 31, 1, 4, table_data)
table_data <- update_table("Waratahs", "Brumbies", 25, 31, 3, 3, table_data)
table_data <- update_table("Moana Pasifika", "Fijian Drua", 34, 36, 5, 6, table_data)
table_data <- update_table("Highlanders", "Blues", 20, 60, 2, 8, table_data)
table_data <- update_table("Reds", "Hurricanes", 13, 47, 1, 6, table_data)
table_data <- update_table("Force", "Rebels", 34, 27, 4, 3, table_data)

# Round 2
table_data <- update_table("Crusaders", "Highlanders", 52, 15, 7, 2, table_data)
table_data <- update_table("Rebels", "Hurricanes", 33, 39, 5, 5, table_data)
table_data <- update_table("Moana Pasifika", "Chiefs", 29, 52, 4, 8, table_data)
table_data <- update_table("Fijian Drua", "Waratahs", 17, 46, 2, 6, table_data)
table_data <- update_table("Blues", "Brumbies", 20, 25, 2, 3, table_data)
table_data <- update_table("Force", "Reds", 20, 71, 3, 10, table_data)

# Round 3
table_data <- update_table("Chiefs", "Highlanders", 28, 7, 4, 1, table_data)
table_data <- update_table("Rebels", "Waratahs", 34, 27, 5, 3, table_data)
table_data <- update_table("Fijian Drua", "Crusaders", 25, 24, 4, 4, table_data)
table_data <- update_table("Hurricanes", "Blues", 19, 25, 3, 3, table_data)
table_data <- update_table("Brumbies", "Reds", 23, 17, 2, 3, table_data)
table_data <- update_table("Force", "Moana Pasifika", 21, 18, 2, 2, table_data)

# Round 4
table_data <- update_table("Hurricanes", "Waratahs", 34, 17, 5, 3, table_data)
table_data <- update_table("Chiefs", "Rebels", 44, 25, 6, 4, table_data)
table_data <- update_table("Blues", "Crusaders", 28, 34, 4, 5, table_data)
table_data <- update_table("Brumbies", "Moana Pasifika", 62, 36, 9, 5, table_data)
table_data <- update_table("Highlanders", "Force", 43, 35, 6, 5, table_data)
table_data <- update_table("Reds", "Fijian Drua", 27, 24, 4, 3, table_data)

# Round 5
table_data <- update_table("Crusaders", "Brumbies", 35, 17, 5, 2, table_data)
table_data <- update_table("Waratahs", "Chiefs", 14, 24, 2, 3, table_data)
table_data <- update_table("Highlanders", "Fijian Drua", 57, 24, 9, 4, table_data)
table_data <- update_table("Moana Pasifika", "Hurricanes", 0, 59, 0, 9, table_data)
table_data <- update_table("Rebels", "Reds", 40, 34, 6, 5, table_data)
table_data <- update_table("Blues", "Force", 30, 17, 4, 3, table_data)

# Round 6
table_data <- update_table("Moana Pasifika", "Highlanders", 17, 45, 2, 7, table_data)
table_data <- update_table("Reds", "Crusaders", 12, 25, 2, 3, table_data)
table_data <- update_table("Fijian Drua", "Rebels", 38, 28, 6, 4, table_data)
table_data <- update_table("Chiefs", "Blues", 20, 13, 2, 2, table_data)
table_data <- update_table("Brumbies", "Waratahs", 40, 36, 6, 5, table_data)
table_data <- update_table("Hurricanes", "Force", 45, 42, 7, 6, table_data)

# Round 7
table_data <- update_table("Crusaders", "Moana Pasifika", 38, 21, 5, 3, table_data)
table_data <- update_table("Reds", "Brumbies", 24, 52, 3, 7, table_data)
table_data <- update_table("Highlanders", "Hurricanes", 14, 29, 2, 4, table_data)
table_data <- update_table("Rebels", "Blues", 17, 54, 2, 9, table_data)

# Round 8
table_data <- update_table("Moana Pasifika", "Reds", 28, 40, 4, 6, table_data)
table_data <- update_table("Brumbies", "Fijian Drua", 43, 28, 7, 4, table_data)
table_data <- update_table("Hurricanes", "Chiefs", 17, 33, 2, 4, table_data)
table_data <- update_table("Waratahs", "Force", 36, 16, 5, 2, table_data)

# Round 9
table_data <- update_table("Chiefs", "Fijian Drua", 50, 17, 8, 3, table_data)
table_data <- update_table("Rebels", "Crusaders", 27, 43, 3, 6, table_data)
table_data <- update_table("Blues", "Waratahs", 55, 21, 7, 3, table_data)
table_data <- update_table("Force", "Highlanders", 30, 17, 3, 2, table_data)

# Round 10
table_data <- update_table("Hurricanes", "Brumbies", 32, 27, 4, 4, table_data)
table_data <- update_table("Waratahs", "Highlanders", 21, 20, 3, 2, table_data)
table_data <- update_table("Fijian Drua", "Blues", 14, 30, 2, 3, table_data)
table_data <- update_table("Moana Pasifika", "Rebels", 33, 43, 5, 7, table_data)
table_data <- update_table("Chiefs", "Crusaders", 34, 24, 3, 3, table_data)
table_data <- update_table("Reds", "Force", 31, 17, 4, 3, table_data)

# Round 11
table_data <- update_table("Highlanders", "Chiefs", 28, 52, 4, 7, table_data)
table_data <- update_table("Fijian Drua", "Hurricanes", 27, 24, 3, 4, table_data)
table_data <- update_table("Crusaders", "Force", 48, 13, 8, 1, table_data)
table_data <- update_table("Blues", "Moana Pasifika", 31, 30, 5, 4, table_data)
table_data <- update_table("Reds", "Waratahs", 24, 32, 3, 4, table_data)
table_data <- update_table("Rebels", "Brumbies", 26, 33, 4, 5, table_data)

# add the Position column to the data frame
table_data$Pos <- 1:nrow(table_data)

# Add logo column 
table_data = table_data %>% 
  mutate(Logo = Team) 

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
table_data <- table_data[,c(8:9, 1:7)]

# sort the table by Position, Pts, Wins (W), then PD
table_data <- table_data[order(table_data$Pos, -table_data$Pts, -table_data$W, -table_data$PD),]

# create a nicely formatted table using gt and gtExtras
super_rugby_2023 = table_data %>%
  gt() %>%
  gt_theme_dark() %>% 
  gt::data_color(
    columns = Pts, palette = c("white", "orange")) %>% 
  cols_width(
    Team ~ px(120),
    PD ~ px(80),
    Pts ~ px(40),
    Logo ~ px(45),
    Pos ~ px(35)) %>% 
  tab_header(title = md("**SUPER RUGBY PACIFIC**"), subtitle = md("2023 Season Standings after Round 11")) %>%
  cols_label(Pos = "Pos", Team = "Team", Logo = "", P = "P", W = "W", D = "D", L = "L", PD = "PD", Pts = "Pts") %>%
  tab_options(
    table.font.size = "14px",
    table.width = "470px",
    column_labels.font.weight = "bolder",
    column_labels.background.color = "#002B5B",
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
    heading.title.font.size = "22px",
    heading.subtitle.font.size = "16px",
    heading.title.font.weight = "bold",
    heading.padding = "7px",
    source_notes.font.size = 8,
    source_notes.background.color = "#002B5B",
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#e0f8cf")),
    locations = cells_body(
      columns = Logo
    )) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#1f5710"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Pos,
      rows = Pos %in% 1:4
    )) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#268407"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Pos,
      rows = Pos %in% 5:8
    )) %>% 
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
    columns = c(Pos, Logo, PD, Pts)) %>% 
  tab_source_note(
    source_note = md("**Table:** Manasseh Oduor | **Source:** super.rugby | **#SuperRugbyPacific:** @R11"))


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
new_gt_save(super_rugby_2023,"super_rugby_standings_2023_R11.png")

