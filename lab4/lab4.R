library(ggplot2)
library(dplyr)
library(cowplot)

# 1
dataset <- read.csv('lab4/athlete_events_dataset.csv', header=T)
germany <- dataset[dataset$NOC == "GER", ]
cycling <- germany[germany$Sport == "Cycling", ]
female <- cycling[cycling$Sex == "F", ]
male <- cycling[cycling$Sex == "M", ]
last_30_years_female <- cycling[cycling$Year >= 1993 & cycling$Year <= 2023 & cycling$Sex == "F", ]
last_30_years_male <- cycling[cycling$Year >= 1993 & cycling$Year <= 2023 & cycling$Sex == "M", ]


# 2
# Subset the data for Germany and cycling
germany_cycling <- dataset[dataset$NOC == "GER" & dataset$Sport == "Cycling", ]

# Create a bar chart of top places by Olympic Games
ggplot(germany_cycling, aes(x = Year, fill = factor(Medal))) +
  geom_bar() +
  ggtitle("Top Places Won by German Cyclists in Olympic Games") +
  xlab("Year") +
  ylab("Number of Top Places") +
  scale_fill_manual(values = c("#FFD700", "#C0C0C0", "#CD7F32"),
                    labels = c("Gold", "Silver", "Bronze")) +
  theme_bw()


# Subset the data for Germany, cycling, and gold medals in Olympic Games
germany_cycling_gold <- germany_cycling[germany_cycling$Medal == "Gold", ]

# Create a pie chart of gold medals
ggplot(germany_cycling_gold, aes(x = "", fill = factor(Medal))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste("Gold Medals:", nrow(germany_cycling[germany_cycling$Medal == "Gold", ] %>% filter(!is.na(Medal))))),
            x = 1.5, y = 0.5, size = 6, fontface = "bold") +
  ggtitle("Gold Medals Won by German Cyclists in Olympic Games") +
  scale_fill_manual(values = c("#FFD700"), labels = c("Gold")) +
  theme_void()



# Subset the data for cycling and the last 30 years
cycling_last_30_years <- cycling[cycling$Year >= 1993 & cycling$Year <= 2023, ]
cycling_last_30_years_with_medal <- cycling_last_30_years %>% filter(!is.na(Medal))
# Aggregate the data to calculate medal counts per year and sex
cycling_medal_counts <- cycling_last_30_years_with_medal %>%
  group_by(Year, Sex) %>%
  summarize(count = n())

# Create a functional plot of medal counts by year and sex
ggplot(cycling_medal_counts, aes(x = Year, y = count, color = Sex)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Trend in Medal Counts for Men and Women in Cycling (Last 30 Years)") +
  xlab("Year") +
  ylab("Number of Medals") +
  scale_color_manual(values = c("blue", "pink"), labels = c("Men", "Women")) +
  theme_bw()

# 3
# Filter the data to only include the last six Olympics
olympic_data <- dataset %>% filter(Year >= 2000)

# Calculate the total number of gold and bronze medals for each year and country
medal_counts <- olympic_data %>%
  filter(Medal %in% c("Gold", "Bronze")) %>%
  group_by(Year, NOC, Medal, Season) %>%
  summarize(count = n()) %>%
  ungroup()

# Filter medal_counts to only include the countries_to_include
medal_counts_filtered <- medal_counts %>% filter(NOC %in% c("USA", "CHN", "RUS", "GBR", "GER", "FRA", "ITA"))

# Create a line plot of gold medals won by the 7 most successful countries
gold_plot <- ggplot(medal_counts_filtered %>% filter(Medal == "Gold"), aes(x = Year, y = count, color = NOC)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("USA" = "#ffff00", "CHN" = "#de2910", "RUS" = "#00abab", "GBR" = "#00247d", "GER" = "#000000", "FRA" = "#0055a4", "ITA" = "#009246")) +
  facet_wrap(~ Season) +
  ggtitle("Gold Medals Won by 7 Most Successful Countries in Last Six Olympic Games") +
  xlab("Year") +
  ylab("Number of Gold Medals") +
  theme_minimal()

# Create a line plot of bronze medals won by the 7 most successful countries
bronze_plot <- ggplot(medal_counts_filtered %>% filter(Medal == "Bronze"), aes(x = Year, y = count, color = NOC)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("USA" = "#ffff00", "CHN" = "#de2910", "RUS" = "#00abab", "GBR" = "#00247d", "GER" = "#000000", "FRA" = "#0055a4", "ITA" = "#009246")) +
  facet_wrap(~ Season) +
  ggtitle("Bronze Medals Won by 7 Most Successful Countries in Last Six Olympic Games") +
  xlab("Year") +
  ylab("Number of Bronze Medals") +
  theme_minimal()

# Combine the two plots using the cowplot package
library(cowplot)
plot_grid(gold_plot, bronze_plot, ncol = 2)

# 4
# Сreate a graph of dynamics and statistics for men and women
olympic_data <- cycling %>% filter(Year >= 2000)

# Calculate the total number of gold and bronze medals for each year and country
medal_counts <- olympic_data %>%
  filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
  group_by(Year, Sex, Medal, Season) %>%
  summarize(count = n()) %>%
  ungroup()

# Filter medal_counts to only include the countries_to_include
medal_counts_filtered <- medal_counts %>% filter(!is.na(Medal))

# Create a line plot of Trend in Medal Counts for Germany in Cycling in the Last Six Olympic Games
line_plot <- ggplot(medal_counts_filtered, aes(x = Year, y = count, color = Sex)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("blue", "pink"), labels = c("Men", "Women")) +
  facet_wrap(~ Season) +
  ggtitle("Trend in Medal Counts for Germany in Cycling in the Last Six Olympic Games") +
  xlab("Year") +
  ylab("Number of Medals") +
  theme_minimal()

# Create a pie chart of Trend in Medal Counts for Germany in Cycling in the Last Six Olympic Games
pie_chart <- ggplot(medal_counts_filtered, aes(x = factor(1), y = count, fill = Sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Men", "Women")) +
  facet_wrap(~ Season) +
  ggtitle("Trend in Medal Counts for Germany in Cycling in the Last Six Olympic Games") +
  theme_minimal() +
  theme_void()

# Create a bar chart of Trend in Medal Counts for Germany in Cycling in the Last Six Olympic Games
bar_chart <- ggplot(medal_counts_filtered, aes(x = Year, fill = factor(Sex))) +
  geom_bar() +
  ggtitle("Trend in Medal Counts for Germany in Cycling in the Last Six Olympic Games") +
  xlab("Year") +
  ylab("Number of Top Places") +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Men", "Women")) +
  theme_bw()

# Combine the two plots using the cowplot package
library(cowplot)
plot_grid(line_plot, pie_chart, bar_chart, ncol = 3)
