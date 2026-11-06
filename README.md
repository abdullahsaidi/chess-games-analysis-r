# Chess Games Analysis in R

Exploratory data analysis and visualization of **20,000+ online chess games from Lichess**, looking at how piece colour, player rating and the choice of opening relate to game outcomes and game length. Team course project for Data Visualization at Princess Sumaya University for Technology.

## Data

[Chess Game Dataset (Lichess)](https://www.kaggle.com/datasets/datasnaek/chess) by Mitchell J. on Kaggle (CC0). Each row is one game, with the number of turns, how it ended (mate, resign, out of time, draw), the winner, both players' ratings and the opening (ECO code, name and opening length in plies).

Download `games.csv` from Kaggle and place it in a `data/` folder next to the script.

## What the analysis covers

**Univariate EDA**
- Summary statistics, standard deviation and skewness (with and without IQR outliers) for turns, ratings and opening length
- Histograms with density curves, box plots and Q–Q plots to check normality
- Counts of rated games, victory status and winners

**Relationships between variables**
- Victory status by winning colour, and game length by victory status
- Ratings vs. number of turns (LOESS and linear fits)
- Correlation heatmap and a pair plot of the numeric features coloured by winner
- Win and draw rates for the most frequent openings

**Questions answered**
1. Does piece colour influence winning outcomes?
2. What are the most common openings, and who wins more with them?
3. How does game length vary with player rating?
4. How do openings relate to win rates?
5. Which openings do players prefer at different rating levels?
6. How does the opening affect game length?
7. How often does the higher-rated player win as the rating gap grows?
8. Does game length differ above and below a rating threshold?

## Key findings

- White wins slightly more often than black, and most games end by resignation or checkmate.
- Drawn games are almost always longer than decisive ones.
- Player rating has only a weak relationship with game length; higher-rated games run slightly longer on average.
- Higher-rated players tend to play slightly longer opening sequences.
- Opening preferences change between rating levels, and different openings lead to noticeably different average game lengths.
- The larger the rating gap, the more likely the higher-rated player wins.

## Run it

```r
install.packages(c("ggplot2", "e1071", "dplyr", "corrplot", "GGally", "tidyr"))
source("chess_analysis.R")
```

## Tech stack

R · ggplot2 · dplyr · tidyr · GGally · corrplot · e1071
