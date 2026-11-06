library(ggplot2)
library(e1071)
library(dplyr)
library(corrplot)
library(GGally)
library(tidyr) 

# Data: "Chess Game Dataset (Lichess)" by Mitchell J. on Kaggle (CC0):
# https://www.kaggle.com/datasets/datasnaek/chess
# Download games.csv and place it in data/ before running.
chess <- read.csv("data/games.csv")

## ----------------- Exploratory data analysis ----------------- ##

#columns,data types,sample values
str(chess)

#dimensions of the data set
dim(chess)

#summary statistics of features
summary(chess)

#class of each col
sapply(chess, class)

#null counts (all zeroes)
colSums(is.na(chess))

############## Categorical columns analysis

### how many games were rated?
table(chess$rated)
ggplot(chess,aes(x=rated)) +
  geom_bar()+
  theme_minimal()


### final game status count
table(chess$victory_status)
ggplot(chess,aes(x=victory_status)) +
  geom_bar()+
  theme_minimal()

# Most games either end with a checkmate or with someone resigning


### Black vs White wins distribution (and draw count)
table(chess$winner)
ggplot(chess[chess$winner %in% c("white", "black"), ],
       aes(x = winner, fill = winner)) +
  geom_bar() +
  scale_fill_manual(values = c("white" = "grey", "black" = "black")) +
  theme_minimal()


# White wins slightly more games than black


### How many different opening moves?
length(unique(chess$opening_name))

############## Numerical columns analysis
summary(chess$turns)
sd(chess$turns)

skewness(chess$turns)

#skewness without considering outliers:
remove_outliers_skewness<-function(column){
  iqr<- IQR(column)
  lower_bound<- quantile(column,0.25)-1.5*iqr
  upper_bound<- quantile(column,0.75)+ 1.5*iqr
  data_no_outliers<-column[column>=lower_bound & column<=upper_bound]
  return(skewness(data_no_outliers))
}
remove_outliers_skewness(chess$turns)
#data is still skewed without considering outliers

### Turns distribution
ggplot(chess, aes(x = turns)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red",size=1)


### Turns box plot 
ggplot(chess, aes(y= turns))+
  geom_boxplot(outlier.colour= "red",outlier.shape=16,outlier.size= 2,alpha=0.5)

#not normally distributed, heavy right tail
qqnorm(chess$turns)
qqline(chess$turns, col="red",lwd=2)


summary(chess$white_rating)
sd(chess$white_rating)
skewness(chess$white_rating)
remove_outliers_skewness(chess$white_rating)

### white rating distribution
ggplot(chess, aes(x =white_rating))+
  geom_histogram(aes(y = ..density..),bins=30,fill= "skyblue",color= "black")+
  geom_density(color= "red",size=1)


### white rating boxplot
ggplot(chess,aes(y = white_rating)) +
  geom_boxplot(outlier.colour="red",outlier.shape= 16, outlier.size= 2,alpha=0.5)

#almost normally distributed
qqnorm(chess$white_rating)
qqline(chess$white_rating, col="red",lwd=2)

summary(chess$black_rating)
sd(chess$black_rating)
skewness(chess$black_rating)
remove_outliers_skewness(chess$black_rating)

### black rating distribution
ggplot(chess, aes(x = black_rating)) +
  geom_histogram(aes(y = ..density..),bins=30,fill= "skyblue",color= "black")+
  geom_density(color= "red",size=1)

### black rating boxplot
ggplot(chess,aes(y= black_rating))+
  geom_boxplot(outlier.colour ="red",outlier.shape= 16, outlier.size=2,alpha=0.5)

#almost normally distributed
qqnorm(chess$black_rating)
qqline(chess$black_rating, col="red",lwd=2)

summary(chess$opening_ply)
sd(chess$opening_ply)
skewness(chess$opening_ply)
remove_outliers_skewness(chess$opening_ply)


### Number of moves in the opening phase dist
ggplot(chess, aes(x=opening_ply))+
  geom_histogram(bins=30,fill= "skyblue",color= "black")


### box plot of that number
ggplot(chess, aes(y =opening_ply)) +
  geom_boxplot(outlier.colour= "red", outlier.shape= 16,outlier.size=2,alpha=0.5)

#not normally distributed
qqnorm(chess$opening_ply)
qqline(chess$opening_ply, col="red",lwd=2)

####### MULTIVARIATE ANALYSIS AND RELATIONSHIPS #######


### Winner vs Victory Status

# This shows that the person starting with white wins more often than
# black, however they also resign more often
ggplot(chess[chess$winner %in% c("white", "black"), ],
       aes(x = victory_status, fill = winner)) +
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = c("white" = "grey", "black" = "black")) +
  labs(title = "Count of Winners by Victory Status",
       x = "Victory Status",
       y = "Count",
       fill = "Winner") +
  theme_minimal()


### Victory status vs turns(game length)

# When the outcome is a draw the number of turns is almost always higher than
# any other outcome
ggplot(chess, aes(x = victory_status, y = turns)) +
  geom_boxplot() +
  labs(title = "Number of Turns by Victory Status",
       x = "Victory Status",
       y = "Number of Turns") +
  theme_minimal() 
  


### White and black rating vs turns(game length)

# it seems that the number of turns isn't affected by the rating of the player
# white or black doesn't matter as the plots are similar
ggplot(chess, aes(x = white_rating, y = turns)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Number of Turns vs. White's Rating",
       x = "White's Rating",
       y = "Number of Turns") +
  theme_minimal()

ggplot(chess, aes(x = black_rating, y = turns)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Number of Turns vs. Black's Rating",
       x = "Black's Rating",
       y = "Number of Turns") +
  theme_minimal()



### Correlation between numerical features

numerical_features <- chess %>% select(turns, white_rating, black_rating, opening_ply)
cor_matrix <- cor(numerical_features, use = "complete.obs") # 'complete.obs' drops rows with missing values (listwise deletion)

# Heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         diag = FALSE) 

# There is a weak relationship between player rating and opening_ply
# so the higher the rating, the slightly longer the opening phase
# otherwise no relationship between numerical variables (other than white vs black rating)



### Pair plot that shows multiple types of relationships and plots in one plot

ggpairs_plot <- ggpairs(chess,
                        columns = c("turns", "white_rating", "black_rating", "opening_ply", "winner"),
                        aes(color = winner, alpha = 0.4)) +
  labs(title = "Pair Plot of Selected Chess Game Features") +
  theme_minimal()

print(ggpairs_plot)


### average ratings, win rates for most frequent openings
opening_summary <- chess %>%
  group_by(opening_eco) %>%
  summarise(
    total_games = n(),
    avg_white_rating = mean(white_rating, na.rm = TRUE),
    avg_black_rating = mean(black_rating, na.rm = TRUE),
    avg_turns = mean(turns, na.rm = TRUE),
    white_win_rate = sum(winner == "white") / total_games,
    black_win_rate = sum(winner == "black") / total_games,
    draw_rate = sum(winner == "draw") / total_games
  ) %>%
  arrange(desc(total_games)) %>%
  ungroup()


#top 5 openings
top5_openings_summary <- head(opening_summary, 5)

top5_openings_plot_data <- top5_openings_summary %>%
  select(opening_eco, white_win_rate, black_win_rate, draw_rate) %>%
  pivot_longer(cols = c(white_win_rate, black_win_rate, draw_rate),
               names_to = "outcome_type",
               values_to = "rate")

# This plot shows the outcome rates (white win / black win / draw) for the 5 most frequent openings
ggplot(top5_openings_plot_data, aes(x = opening_eco, y = rate, fill = outcome_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "white_win_rate" = "grey",
    "black_win_rate" = "black",
    "draw_rate" = "steelblue"
  )) +
  labs(title = "Outcome Rates for Top 5 Most Frequent Openings",
       x = "Opening ECO Code",
       y = "Rate",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# ################### Question: Does piece colour influence winning outcomes?

ggplot(chess[chess$winner %in% c("white", "black"), ],
       aes(x = winner, fill = winner)) +
  geom_bar() +
  scale_fill_manual(values = c("white" = "grey", "black" = "black")) +
  labs(title = "Game Outcomes by Winner",
       x = "Winner",
       y = "Count",
       fill = "Winner") +
  theme_minimal()

ggplot(chess[chess$winner %in% c("white", "black"), ],
       aes(x = victory_status, fill = winner)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("white" = "grey", "black" = "black")) +
  labs(title = "Victory Status by Winning Color",
       x = "Victory Status",
       y = "Count",
       fill = "Winner") +
  theme_minimal()


# ################### Question: What are the five most common opening moves by colour?

top_openings <- chess %>%
  group_by(opening_name) %>%
  tally(sort = TRUE) %>%
  slice_head(n = 5)


chess %>%
  filter(opening_name %in% top_openings$opening_name,
         winner %in% c("white", "black")) %>%
  ggplot(aes(x = opening_name, fill = winner)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("white" = "grey", "black" = "black")) +
  
  labs(title = "Wins by Color in Top 5 Most Common Openings",
       x = "Opening Name",
       y = "Number of Wins",
       fill = "Winner") +
  theme_minimal(base_family = "sans") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ################### Question: How does game length vary based on player ranking?

chess <- chess %>%
  mutate(avg_rating = (white_rating + black_rating) / 2)

ggplot(chess, aes(x = avg_rating, y = turns)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Correlation Between Player Ratings and Number of Moves",
    x = "Average Player Rating",
    y = "Number of Moves (Turns)"
  ) +
  theme_minimal()
cor(chess$avg_rating, chess$turns)
#on average, higher-ranked players tend to have slightly longer games.

# ################### Question: How do openings correlate with win rates?
opening_outcomes <- chess %>%
  filter(winner %in% c("white", "black")) %>%  # exclude draws
  group_by(opening_name, winner) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = winner, values_from = count, values_fill = 0) %>%
  mutate(
    total = white + black,
    white_win_rate = white / total,
    black_win_rate = black / total
  ) %>%
  filter(total >= 50)


opening_outcomes %>%
  arrange(desc(total)) %>%
  slice_head(n = 12) %>%
  pivot_longer(cols = c(white_win_rate, black_win_rate), names_to = "outcome", values_to = "rate") %>%
  ggplot(aes(x = opening_name, y = rate, fill = outcome)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("white_win_rate" = "grey", "black_win_rate" = "black")) +
  labs(title = "Win Rates for Top 12 Most Common Openings", x = "Opening", y = "Win Rate", fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

################### Question: What openings do players prefer based on their rating levels?

chess <- chess %>%
  mutate(avg_rating = (white_rating + black_rating) / 2) %>%
  mutate(rating_category = case_when(
    avg_rating < 1500 ~ "Below 1500",
    avg_rating >= 1500 & avg_rating < 2000 ~ "1500-1999",
    avg_rating >= 2000 ~ "2000+",
    TRUE ~ NA_character_
  ))
top_openings_by_rating <- chess %>%
  group_by(rating_category, opening_name) %>%
  tally(sort = TRUE) %>%
  slice_head(n = 5) %>% 
  ungroup()

ggplot(top_openings_by_rating, aes(x = opening_name, y = n, fill = rating_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Top 5 Openings by Average Player Rating Category",
    x = "Opening Name",
    y = "Number of Games",
    fill = "Average Rating"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Based on the graph, players do show preferences for different openings based on their rating levels


################### Question: How does the opening move affect game length?
avg_duration_by_opening <- chess %>%
  group_by(opening_name) %>%
  summarise(avg_turns = mean(turns, na.rm = TRUE), n = n()) %>%
  filter(n > 100)

ggplot(avg_duration_by_opening, aes(x = avg_turns, y = reorder(opening_name, avg_turns), size = n)) +
  geom_point(alpha = 0.6, color = "purple") +
  labs(
    title = "Average Game Duration by Opening",
    x = "Average Number of Turns",
    y = "Opening Name",
    size = "Frequency"
  ) +
  theme_minimal()
#The graph shows that different chess openings tend to result in games of different lengths,
#measured by the average number of turns

################### Question:	Who tends to win more often when players have a large rating gap?
chess <- chess %>%
  mutate(rating_difference = white_rating - black_rating,
         higher_rated = case_when(
           rating_difference > 0 ~ "white",
           rating_difference < 0 ~ "black",
           TRUE ~ "tie"
         ),
         higher_rated_won = case_when(
           (higher_rated == "white" & winner == "white") | (higher_rated == "black" & winner == "black") ~ "Yes",
           (higher_rated == "white" & winner == "black") | (higher_rated == "black" & winner == "white") ~ "No",
           TRUE ~ NA_character_ # For draws or ties in rating
         )) %>%
  filter(!is.na(higher_rated_won))

chess <- chess %>%
  mutate(rating_gap_category = case_when(
    abs(rating_difference) >= 0 & abs(rating_difference) < 100 ~ "0-99",
    abs(rating_difference) >= 100 & abs(rating_difference) < 200 ~ "100-199",
    abs(rating_difference) >= 200 & abs(rating_difference) < 300 ~ "200-299",
    abs(rating_difference) >= 300 ~ "300+",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(rating_gap_category))

win_rate_by_gap <- chess %>%
  group_by(rating_gap_category) %>%
  summarise(
    total_games = n(),
    higher_rated_wins = sum(higher_rated_won == "Yes"),
    win_percentage = (higher_rated_wins / total_games) * 100,
    .groups = 'drop'
  )

ggplot(win_rate_by_gap, aes(x = rating_gap_category, y = win_percentage, fill = rating_gap_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", win_percentage)), vjust = -0.5) +
  labs(
    title = "Win Rate of Higher-Rated Player by Rating Gap",
    x = "Rating Gap (Elo)",
    y = "Win Percentage (%)",
    fill = "Rating Gap"
  ) +
  theme_minimal()
# The graph shows that the likelihood of the higher-rated player winning increases,
# as the rating gap between the two players becomes larger


################### Question: Does game length vary significantly for players below or above a specific rating threshold?
rating_threshold <- 1700

chess <- chess %>%
  mutate(rating_group = case_when(
    white_rating < rating_threshold & black_rating < rating_threshold ~ "Both Below",
    white_rating >= rating_threshold & black_rating >= rating_threshold ~ "Both Above",
    TRUE ~ "Mixed"
  ))

ggplot(chess, aes(x = rating_group, y = turns, fill = rating_group)) +
  geom_boxplot() +
  labs(
    title = paste("Game Length vs. Player Rating Group (Threshold:", rating_threshold, ")"),
    x = "Player Rating Group",
    y = "Number of Turns",
    fill = "Rating Group"
  ) +
  theme_minimal()

# We can conclude that game length does not vary much based on a rating threshold
# outside of a few outliers



