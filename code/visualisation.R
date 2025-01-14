library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

########################## import data ##########################
match_player_stats <- read.csv("Coursework/match_player_stats.csv")
match_line_ups <- read.csv("Coursework/match_line_ups.csv")



########################## cleaning and joining data ##########################
######### match_player_stats #########
# drop unused column for pivot data
match_player_stats <- match_player_stats [-c(9, 12)]

# pivot the data
match_player_stats <- match_player_stats %>%
  pivot_wider(names_from = StatsName, values_from = Value)

# create full name by joining first name and last name
match_player_stats$full_name <- paste(match_player_stats$PlayerName, match_player_stats$PlayerSurname)


######### match_line_ups #########
# create full name by joining first name and last name
match_line_ups$full_name <- paste(match_line_ups$OfficialName, match_line_ups$OfficialSurname)

# check unique value
duplicates <- duplicated(match_line_ups[, c("MatchID", "full_name")])
match_line_ups[duplicates, ]

# drop full name that are “None None” value (staff)
match_line_ups <- match_line_ups %>%
  filter(IsStaff != "True")


######### joining #########
# check for duplicate column name
intersect(names(match_player_stats), names(match_line_ups))

# test join to see that each pair of duplicate columns have all same values
left_join_result <- left_join(match_player_stats, match_line_ups, by = c("MatchID", "full_name"))
all(left_join_result$HomeTeamName.x == left_join_result$HomeTeamName.y) # TRUE
all(left_join_result$AwayTeamName.x == left_join_result$AwayTeamName.y) # TRUE
all(left_join_result$IsGoalkeeper.x == left_join_result$IsGoalkeeper.y) # FALSE

# found that IsGoalkeeper.x is all false, so we decided to drop duplicate column in match_player_stats
match_player_stats <- match_player_stats %>%
  select(-HomeTeamName, -AwayTeamName, -IsGoalkeeper)

# left join match_player_stats with match_line_ups by MatchID and full_name
left_join_result <- left_join(match_player_stats, match_line_ups, by = c("MatchID", "full_name"))

# check unique roles
unique(left_join_result$Role)

# filter goalkeeper and defender out
left_join_result <- left_join_result %>%
  filter(Role %in% c("forwards", "midfielders"))

# trim white space before and after full name
left_join_result$full_name <- str_trim(left_join_result$full_name)

# check number of samples
nrow(left_join_result) # 953

# convert goals and assists column to numeric
left_join_result$Goals <- as.numeric(left_join_result$Goals)
left_join_result$Assists <- as.numeric(left_join_result$Assists)

# create goal contributions column
df <- left_join_result %>%
  mutate(GACombined = Goals + Assists)



########################## visual 1 ##########################
# convert to numeric
visual_1_df <- df %>%
  mutate(across(c(`GACombined`,
                  `PlayedTime`), as.numeric))

# select columns
visual_1_df <- visual_1_df %>%
  select(full_name,
         `GACombined`,
         `PlayedTime`)

# check missing value
colSums(is.na(visual_1_df))

# drop
visual_1_df <- visual_1_df %>%
  filter(if_all(everything(), ~ !is.na(.)))

# group by player
visual_1_df <- visual_1_df %>%
  group_by(full_name) %>%
  summarise(
    ga_combined = sum(GACombined),
    played_time = sum(PlayedTime)
  )

# create goal contribution per 90 minutes column
visual_1_df <- visual_1_df %>%
  mutate(ga_combined_90 = (ga_combined / played_time) * (90 * 60))

# filter top 10 goal scorers (actual goals)
visual_1_df <- visual_1_df %>%
  arrange(desc(ga_combined)) %>% 
  slice_head(n = 10)

# pivot data for create visual
visual_1_df <- visual_1_df %>%
  pivot_longer(cols = c(ga_combined, ga_combined_90), 
               names_to = "metric", 
               values_to = "value")

# fix the order of top 10 goal scorers
visual_1_df$full_name <- factor(visual_1_df$full_name, levels = unique(visual_1_df$full_name))

# visual - grouped bar chart
ggplot(visual_1_df, aes(x = full_name, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Actual vs. Per 90-Minute Goal Contributions: Top 10 Players",
    subtitle = "Player rankings changed when considering goal contributions per 90 minutes",
    x = "Player",
    y = "Amount of goal contribution (goals+assists)",
    fill = "Metric",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  scale_fill_manual(
    values = c("ga_combined" = "#B0B0B0", "ga_combined_90" = "#E69F00"),
    labels = c("ga_combined" = "Goal Contribution", "ga_combined_90" = "Goal Contribution Per 90 Minutes")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = 'top', 
    legend.justification = 'left',
    legend.direction = 'horizontal'
  )



########################## visual 2 ##########################
# convert to numeric
visual_2_df <- df %>%
  mutate(across(c(`GACombined`,
                  `PlayedTime`,
                  `Delivery into attacking third`,
                  `Delivery into key play area`,
                  `Delivery into penalty area`), as.numeric))

# select columns
visual_2_df <- visual_2_df %>%
  select(full_name,
         `GACombined`,
         `PlayedTime`,
         `Delivery into attacking third`,
         `Delivery into key play area`,
         `Delivery into penalty area`)

# check missing value
colSums(is.na(visual_2_df))

# drop
visual_2_df <- visual_2_df %>%
  filter(if_all(everything(), ~ !is.na(.)))

# group by player
visual_2_df <- visual_2_df %>%
  group_by(full_name) %>%
  summarise(
    ga_combined = sum(GACombined),
    played_time = sum(PlayedTime),
    delivery_atkthird = sum(`Delivery into attacking third`),
    delivery_keyarea = sum(`Delivery into key play area`),
    delivery_penalty = sum(`Delivery into penalty area`)
  )

# convert into per 90 mins
visual_2_df <- visual_2_df %>%
  mutate(across(c(delivery_atkthird,
                  delivery_keyarea,
                  delivery_penalty), ~ . * 90 * 60 / played_time))

# group by goal contribution
visual_2_df <- visual_2_df %>%
  group_by(ga_combined) %>%
  summarise(
    delivery_atkthird = mean(delivery_atkthird),
    delivery_keyarea = mean(delivery_keyarea),
    delivery_penalty = mean(delivery_penalty)
  )

# pivot data for create visual
visual_2_df <- visual_2_df %>%
  pivot_longer(cols = c(delivery_atkthird, delivery_keyarea, delivery_penalty),
               names_to = "Subgroup",
               values_to = "Value")

# convert column type for visual
visual_2_df$ga_combined <- as.character(visual_2_df$ga_combined)

# normalize data and calculate proportions
visual_2_df <- visual_2_df %>%
  group_by(ga_combined) %>%
  mutate(
    Proportion = Value / sum(Value),
    Label = paste0(round(Proportion * 100, 1), "%")
  ) %>%
  ungroup()

# drop 5 and 6 goal contributions to make a better communication
visual_2_df <- visual_2_df %>%
  filter(!(ga_combined %in% c(5, 6)))

# visual - 100% horizontal stacked bar chart
ggplot(visual_2_df, aes(x = ga_combined, y = Proportion, fill = Subgroup)) +
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  geom_text(
    aes(label = Label),
    position = position_fill(vjust = 0.5),
    size = 4, color = "white"
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Impact of Delivery Zones on Goal Contributions",
    subtitle = "Higher percentages of deliveries into the penalty area link to increased goal contributions",
    x = "Amount of goal contribution (goals+assists)",
    y = "Percentage",
    fill = "Delivery Target",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  scale_fill_discrete(labels = c("Attacking Third", "Key Area", "Penalty")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("delivery_penalty" = "#E69F00", "delivery_keyarea" = "#505050", "delivery_atkthird" = "#B0B0B0"),
    labels = c("delivery_penalty" = "Penalty", "delivery_keyarea" = "Key Area", "delivery_atkthird" = "Attacking Third")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050"),
    legend.position = 'top',
    legend.justification = 'left',
    legend.direction = 'horizontal'
  )



########################## visual 3 ##########################
# convert to numeric
visual_3_df <- df %>%
  mutate(across(c(`GACombined`,
                  `Total Attempts`), as.numeric))

# select columns
visual_3_df <- visual_3_df %>%
  select(full_name,
         `GACombined`,
         `Total Attempts`)

# check missing value
colSums(is.na(visual_3_df))

# drop
visual_3_df <- visual_3_df %>%
  filter(if_all(everything(), ~ !is.na(.)))

# group by player
visual_3_df <- visual_3_df %>%
  group_by(full_name) %>%
  summarise(
    ga_combined = sum(GACombined),
    total_attempts = sum(`Total Attempts`)
  )

# convert column type for visual
visual_3_df$ga_combined <- as.character(visual_3_df$ga_combined)

# drop 5 and 6 to make better communicate
visual_3_df <- visual_3_df %>%
  filter(!(ga_combined %in% c(5, 6)))

# visual - box plot
ggplot(visual_3_df, aes(x = ga_combined, y = total_attempts, fill = ga_combined)) +
  geom_boxplot(varwidth=TRUE, outlier.shape = 21, outlier.size = 2, outlier.fill = "darkgrey") +
  scale_fill_manual(
    values = c("0" = "#0072B2", "4" = "#E69F00")
  ) +
  labs(
    title = "Relationship Between Total Attempts and Goal Contributions",
    subtitle = "Player with higher total attempts tend to have more goal contributions",
    x = "Amount of goal contribution (goals+assists)",
    y = "Total attempts",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050"),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )



########################## visual 4 ##########################
# select columns
visual_4_df <- df %>%
  select(full_name,
         `GACombined`,
         `Time spent in opposite half`,
         `Time spent in attacking third`,
         `Time spent in penalty area`)

# check missing value
colSums(is.na(visual_4_df))

# drop
visual_4_df <- visual_4_df %>%
  filter(if_all(everything(), ~ !is.na(.)))

# convert time to numeric (`Time spent in opposite half`, `Time spent in attacking third`, `Time spent in penalty area`)
# create function to handle all time patterns in these 3 columns
time_to_seconds <- function(time_str) {
  if (time_str == "") return(0) 
  parts <- unlist(strsplit(time_str, "[:.]")) 
  h <- as.numeric(parts[1])
  m <- as.numeric(parts[2])
  s <- as.numeric(parts[3])
  ms <- ifelse(length(parts) > 3, as.numeric(parts[4]), 0)
  total_seconds <- h * 3600 + m * 60 + s + ms / 10000000
  return(total_seconds)
}

visual_4_df$`Time spent in opposite half` <- sapply(visual_4_df$`Time spent in opposite half`, time_to_seconds)
visual_4_df$`Time spent in attacking third` <- sapply(visual_4_df$`Time spent in attacking third`, time_to_seconds)
visual_4_df$`Time spent in penalty area` <- sapply(visual_4_df$`Time spent in penalty area`, time_to_seconds)

# group by player
visual_4_df <- visual_4_df %>%
  group_by(full_name) %>%
  summarise(
    ga_combined = sum(GACombined),
    time_opphalf = sum(`Time spent in opposite half`),
    time_atkthird = sum(`Time spent in attacking third`),
    time_penarea = sum(`Time spent in penalty area`)
  )

# group by goal contribution
visual_4_df <- visual_4_df %>%
  group_by(ga_combined) %>%
  summarise(
    time_opphalf = mean(time_opphalf),
    time_atkthird = mean(time_atkthird),
    time_penarea = mean(time_penarea)
  )

# pivot data for create visual
visual_4_df <- visual_4_df %>%
  pivot_longer(cols = c(time_opphalf, time_atkthird, time_penarea), 
               names_to = "metric", 
               values_to = "value")

# convert column type for visual
visual_4_df$ga_combined <- as.character(visual_4_df$ga_combined)

# fix the order of legend
visual_4_df$metric <- factor(visual_4_df$metric, levels = c("time_opphalf", "time_atkthird", "time_penarea"))

# visual - line chart
ggplot(visual_4_df, aes(x = ga_combined, y = value, color = metric, group = metric)) +
  geom_line(size = 1.2) +
  labs(
    title = "Impact of Time Spent in Pitch Areas on Goal Contributions",
    subtitle = "Exploring which pitch areas contribute most to creating attacking threats",
    x = "Amount of goal contribution (goals+assists)",
    y = "Time (seconds)",
    color = "Pitch Areas",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  scale_color_manual(
    values = c("time_opphalf" = "#009E73", "time_atkthird" = "#56B4E9", "time_penarea" = "#E69F00"),
    labels = c("time_opphalf" = "Opposition Half", "time_atkthird" = "Attacking Third", "time_penarea" = "Penalty Area")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050"),
    legend.position = 'top', 
    legend.justification = 'left',
    legend.direction = 'horizontal'
  )