library(tidyverse)
library(ggplot2)
library(readr)
library(VennDiagram)
library(GGally)
library(dplyr)
library (MASS)
library(outliers)
library(corrplot)

New_Dataset <- read_csv(file = "C:/Uni/Coding/R/Exam_r/archive/Tagged-Data-Final.csv")

head(New_Dataset)
summary(New_Dataset)

# Remove white spaces from all columns in data frame:
New_Dataset <- New_Dataset %>% 
  mutate_all(str_trim) 

summary(New_Dataset)

# Remove the duplicates containing the same values in the columns Name, Year of Release, Genre and Developer

New_Dataset <- New_Dataset[!duplicated(New_Dataset[c("Name", "Year_of_Release", "Genre", "Developer")]),]

# Remove the duplicates containing the same values in the columns Name, Year of Release and Genre 

New_Dataset <- New_Dataset[!duplicated(New_Dataset[c("Name", "Year_of_Release", "Genre")]),]


summary(New_Dataset)

# Change the formatting and make a frame with the data required for analysis.
final_dataset <- New_Dataset %>%
  rename(story_focus = "Story Focus",
         gameplay_focus = "Gameplay Focus",
         year = "Year_of_Release",
         genre = "Genre",
         critic_score = "Critic_Score",
         user_score = "User_Score",
         series = "Series",
         prev_NA_sales = "NA_Sales",
         prev_EU_sales = "EU_Sales",
         prev_JP_sales = "JP_Sales",
         prev_global_sales = "Global_Sales")


final_dataset <- final_dataset[, c(1,2,3,5,6,7,9,10,12,15,16,17,18)]

head(final_dataset)

# Change the data type of chosen columns to numeric
final_dataset$year <- as.numeric(final_dataset$year)
final_dataset$prev_NA_sales <- as.numeric(final_dataset$prev_NA_sales)
final_dataset$prev_EU_sales <- as.numeric(final_dataset$prev_EU_sales)
final_dataset$prev_JP_sales <- as.numeric(final_dataset$prev_JP_sales)
final_dataset$prev_global_sales <- as.numeric(final_dataset$prev_global_sales)
final_dataset$critic_score <- as.numeric(final_dataset$critic_score)
final_dataset$user_score <- as.numeric(final_dataset$user_score)

print(str(final_dataset))

# In the columns story_focus, gameplay_focus, and Series, replace
# "x" represents the presence of a feature, 
# while "NA" represents the absence of a feature.
final_dataset$story_focus <- ifelse(is.na(final_dataset$story_focus), 0, 1)
final_dataset$gameplay_focus <- ifelse(is.na(final_dataset$gameplay_focus), 0, 1)
final_dataset$series <- ifelse(is.na(final_dataset$series), 0, 1)

head(final_dataset)


# Videogame sales trends per year
ggplot(final_dataset, aes(year)) +
  geom_col(aes(y = prev_global_sales), fill = "orange3") +
  labs(title = "Videogame Global Sales over the year",
       x = "Year of Release", y = "Global Sales")


# Comparison between global sales and regional sales of North America, Europe and Japan 
ggplot(final_dataset, aes(x = year)) +
  geom_col(aes(y = prev_global_sales, fill = "Global"), alpha = 0.5) +
  geom_col(aes(y = prev_NA_sales, fill = "NA"), alpha = 0.5) +
  geom_col(aes(y = prev_EU_sales, fill = "EU"), alpha = 0.5) +
  geom_col(aes(y = prev_JP_sales, fill = "JP"), alpha = 0.5) +
  stat_summary(aes(y = prev_NA_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "royalblue", linewidth = 1) +
  stat_summary(aes(y = prev_EU_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "green3", linewidth = 1) +
  stat_summary(aes(y = prev_JP_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "coral", linewidth = 1) +
  stat_summary(aes(y = prev_global_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "orange3", linewidth = 1) +
  labs(title = "Videogame Sales over the Year",
       x = "Year of Release",
       y = "Cumulative Sales",
       fill = "Region") +
  scale_fill_manual(values = c("Global" = "orange3", "NA" = "royalblue", "EU" = "green3", "JP" = "coral")) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100))


# Analysis by genre
ggplot(final_dataset, aes(x = genre, y = prev_global_sales)) +
  geom_col(fill = "seagreen3") +
  labs(title = "Global Sales by Genre",
       x = "Genre",
       y = "Global Sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Global Sales by genre over years
ggplot(final_dataset, aes(x = year, y = prev_global_sales)) +
  geom_bar(stat = "identity", position = position_stack()) + 
  facet_wrap(~genre, ncol = 4) + 
  labs(title = "Global sales by genre over Years")


# NA Sales by genre over years
ggplot(final_dataset, aes(x = year, y = prev_NA_sales)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~genre, ncol=4) + 
  labs(title="NA sales by genre over Years") +
  ylim(0, 60)


# EU sales by genre over years
ggplot(final_dataset, aes(x = year, y = prev_EU_sales)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~genre, ncol=4) + 
  labs(title="EU sales by genre over Years") +
  ylim(0, 60)


# JP sales by genre over years
ggplot(final_dataset, aes(x = year, y = prev_JP_sales)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~genre, ncol=4) + 
  labs(title="JP sales by genre over Years") +
  ylim(0, 60)


# Critic Scores and Global Sales relation
ggplot(final_dataset, aes(x = prev_global_sales, y = critic_score), shape = carrier) +
  geom_jitter() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(title="Critic Score and Global Sales")


# Critic Scores and User Score relation
ggplot(final_dataset, aes(x = critic_score, y = user_score), shape = carrier) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(title="Critic Score and User Score")


# Global sales vs global story_game sales
ggplot(data = final_dataset, aes(x = year)) +
  geom_col(aes(y = prev_global_sales, fill = "Global Sales"), alpha = 0.5) +
  geom_col(aes(y = ifelse(story_focus == 1, prev_global_sales, 0), fill = "Story driven VideoGames Sales"), alpha = 0.5) +
  stat_summary(aes(y = prev_global_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "dodgerblue3", linewidth = 1) +
  stat_summary(aes(y = ifelse(story_focus == 1, prev_global_sales, 0)), fun = cumsum, geom = "line", linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Video Game Sales Across Years: Total vs. Story-driven",
       x = "Year of Release",
       y = "Cumulative Sales",
       fill = "Sales") +
  scale_fill_manual(values = c("Global Sales" = "orange3", "Story driven VideoGames Sales" = "dodgerblue3")) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100))


# New dataset with only useful information
story_driven <- final_dataset %>%
  filter(story_focus == 1) %>%
  dplyr:: select(name = Name,
                 year = year,
                 genre = genre,
                 critic_score = critic_score,
                 user_score = user_score,
                 prev_NA_sales = prev_NA_sales,
                 prev_EU_sales = prev_EU_sales,
                 prev_JP_sales = prev_JP_sales,
                 prev_global_sales = prev_global_sales)
summary(story_driven)
head(story_driven)


#Story driven Videogame Sales over Years
ggplot(story_driven, aes(x = year)) +
  geom_col(aes(y = prev_global_sales, fill = "Global Sales"), alpha = 0.5) +
  geom_col(aes(y = prev_NA_sales, fill = "NA Sales"), alpha = 0.5) +
  geom_col(aes(y = prev_EU_sales, fill = "EU Sales"), alpha = 0.5) +
  geom_col(aes(y = prev_JP_sales, fill = "JP Sales"), alpha = 0.5) +
  stat_summary(aes(y = prev_NA_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "royalblue", size = 1) +
  stat_summary(aes(y = prev_EU_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "green3", size = 1) +
  stat_summary(aes(y = prev_JP_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "coral", size = 1) +
  stat_summary(aes(y = prev_global_sales), fun = cumsum, geom = "line", linetype = "dashed", color = "orange3", size = 1) +
  labs(title = "Story driven Videogame Sales over Years",
       x = "Year of Release",
       y = "Cumulative Sales",
       fill = "Sales") +
  scale_fill_manual(values = c("Global Sales" = "orange3", "NA Sales" = "royalblue", "EU Sales" = "coral", "JP Sales" = "green3")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 100))


# Correlation matrix
correlation_matrix <- cor(story_driven[c("prev_NA_sales", "prev_EU_sales", "prev_JP_sales", "prev_global_sales", "critic_score", "user_score")])
print(correlation_matrix)
corrplot(correlation_matrix, method="color", type="upper", order="hclust", tl.col="black", tl.srt=45)


# Boxplot of Sales Distribution by genre
ggplot(story_driven, aes(x=genre, y=prev_global_sales, fill=genre)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Global Sales Distribution of Story Driven Videogames by Genre",
       x = "Genre",
       y = "Global Sales")


ggplot(story_driven, aes(x=genre, y=prev_NA_sales, fill=genre)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "NA Sales Distribution of Story Driven Videogames by Genre",
       x = "Genre",
       y = "NA Sales")


ggplot(story_driven, aes(x=genre, y=prev_EU_sales, fill=genre)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "EU Sales Distribution of Story Driven Videogames by Genre",
       x = "Genre",
       y = "EU Sales")


ggplot(story_driven, aes(x=genre, y=prev_JP_sales, fill=genre)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "JP Sales Distribution of Story Driven Videogames by Genre",
       x = "Genre",
       y = "JP Sales")





