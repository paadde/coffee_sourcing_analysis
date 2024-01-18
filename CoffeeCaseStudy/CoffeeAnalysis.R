# Install and load necessary packages
install.packages("tidyverse")
library(tidyverse)
library(janitor)

# Upload datasets from csv files
arabica_cleaned_data <- read.csv("arabica_data_cleaned.csv")
robusta_cleaned_data <- read.csv("robusta_data_cleaned.csv")


# Explore data from both dataframes to be familiarized and identification of key variables needed for the analysis

str(arabica_cleaned_data)
str(robusta_cleaned_data)

summary(arabica_cleaned_data)
summary(robusta_cleaned_data)

head(arabica_cleaned_data)
head(robusta_cleaned_data)


# Proceeding to the data cleaning process

# Checking column index for each variables that are not significant
print(data.frame(Column_Name = names(arabica_cleaned_data)))
print(data.frame(Column_Name = names(robusta_cleaned_data)))

# Dropping unrelated variables in the data frame for the analysis
arabica_trimmed_data <- arabica_cleaned_data %>%
  select(-1, -6, -8, -10, -13, -14, -15, -16, -37, -38, -39, -40, -41)

robusta_trimmed_data <- robusta_cleaned_data %>%
  select(-1, -6, -8, -10, -13, -14, -15, -16, -37, -38, -39, -40, -41)

# Checking missing values
sum(is.na(arabica_trimmed_data))
sum(is.na(robusta_trimmed_data))

# Due to the presence of numerous missing values in the dataset, particularly in the altitude data, I opted against excluding those rows to avoid potential deletion of related information, which could compromise the accuracy of my analysis.


# Conducting an assessment for duplicate values revealed the absence of any duplications in both data frames.
sum(duplicated(arabica_trimmed_data))
sum(duplicated(robusta_trimmed_data))

# Aligning variable names for uniformity to enhance readability and facilitate easier coding.
arabica_trimmed_data <- janitor::clean_names(arabica_trimmed_data)
robusta_trimmed_data <- janitor::clean_names(robusta_trimmed_data)


print(data.frame(Column_Name = names(arabica_trimmed_data)))

# Identification and handling of potential outliers that can skew the result of the analysis
# Using descriptive statistics to identify outliers
min(arabica_trimmed_data$total_cup_points)
mean(arabica_trimmed_data$total_cup_points)
max(arabica_trimmed_data$total_cup_points)

min(robusta_trimmed_data$total_cup_points)
mean(robusta_trimmed_data$total_cup_points)
max(robusta_trimmed_data$total_cup_points)

# Utilizing visualization tools for the purpose of outlier identification.
ggplot(data = arabica_trimmed_data) +
  geom_histogram(mapping = aes(x = total_cup_points, fill = "Brown")) +
  labs(
    title = "Histogram of Arabica Total Cup Points",
    x = "Total Cup Points",
    y = "Count of Beans"
  )

ggplot(data = robusta_trimmed_data) +
  geom_histogram(mapping = aes(x = total_cup_points, fill = "Brown")) +
  labs(
    title = "Histogram of Robusta Total Cup Points",
    x = "Total Cup Points",
    y = "Count of Beans"
  )


# Eliminating two outliers attributed to potential data entry errors in recording scores for a specific bean, following data entry testing
arabica_trimmed_data <- arabica_trimmed_data %>%
  filter(total_cup_points > 0)

# Converting 'grading_date' variable from string to date format
arabica_trimmed_data$grading_date <-
  mdy(str_replace_all(arabica_trimmed_data$grading_date,
                      "\\b(\\d+)th\\b", "\\1"))

robusta_trimmed_data$grading_date <-
  mdy(str_replace_all(robusta_trimmed_data$grading_date,
                      "\\b(\\d+)th\\b", "\\1"))


# Combining both data frames in preparation for the overall analysis
combined_coffee_data <- rbind(arabica_trimmed_data, robusta_trimmed_data)

summary(combined_coffee_data)

# Exporting csv file for safe keeping and further analysis
write.csv(arabica_trimmed_data, "arabica_trimmed_data.csv", row.names = FALSE)
write.csv(robusta_trimmed_data, "robusta_trimmed_data.csv", row.names = FALSE)
write.csv(combined_coffee_data, "combined_coffee_data.csv", row.names = FALSE)



# Analysis Phase
# Install and load necessary packages for the analysis phase
install.packages("reshape2")
library(reshape2)

# For Arabica
# Calculation of the correlation matrix
arabica_correlation_matrix <-
  cor(arabica_trimmed_data[, c(13:22, 23)])

# Melting of correlation matrix for ggplot
arabica_melted_correlation <- melt(arabica_correlation_matrix)

# Visualization of the correlation matrix through heatmap
ggplot(arabica_melted_correlation) +
  geom_tile(mapping = aes(Var1, Var2, fill = value), color = "white") +
  geom_text(aes(Var1, Var2, label = paste0(round(value * 100), "%")),
            size = 3) +
  labs(title = "Arabica Correlation Heatmap") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))


# For Robusta
# Calculation of the correlation matrix
robusta_correlation_matrix <-
  cor(robusta_trimmed_data[, c(13:22, 23)])

# Melting of correlation matrix for ggplot
robusta_melted_correlation <- melt(robusta_correlation_matrix)

# Visualization of the correlation matrix through heatmap
ggplot(robusta_melted_correlation) +
  geom_tile(mapping = aes(Var1, Var2, fill = value), color = "white") +
  geom_text(aes(Var1, Var2, label = paste0(round(value * 100), "%")),
            size = 3) +
  labs(title = "Robusta Correlation Heatmap") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))


# For Both Coffee Specie
# Calculation of the correlation matrix
combined_correlation_matrix <-
  cor(combined_coffee_data[, c(13:22, 23)])

# Melting of correlation matrix for ggplot
combined_melted_correlation <- melt(combined_correlation_matrix)

# Visualization of the correlation matrix through heatmap
ggplot(combined_melted_correlation) +
  geom_tile(mapping = aes(Var1, Var2, fill = value), color = "white") +
  geom_text(aes(Var1, Var2, label = paste0(round(value * 100), "%")),
            size = 3) +
  labs(title = "Arabica & Robusta Correlation Heatmap") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))

ggsave("comparison_arabica_robusta.png", bg = "white")


# Visualization of the difference between arabica and robusta beans
# Calculating mean values for each sensory attributes
mean_arabica <- colMeans(arabica_trimmed_data[, 13:22], na.rm = TRUE)
mean_robusta <- colMeans(robusta_trimmed_data[, 13:22], na.rm = TRUE)

# Combining mean values into a single data frame in preparation for viz
mean_sensory_values <- data.frame(
  Attribute = names(mean_arabica),
  Arabica = mean_arabica,
  Robusta = mean_robusta
)

# Melting the data frame for use in ggplot2
melted_mean_sensory <- melt(mean_sensory_values)

write.csv(mean_sensory_values, "melted_mean_sensory.csv", row.names = FALSE)

# Creating a bar chart comparison for both species of coffee bean
ggplot(data = melted_mean_sensory) +
  geom_bar(mapping = aes(x = Attribute, y = value, fill = variable),
           stat = "identity", position = "dodge", color = "white") +
  labs(title = "Comparison of Mean Sensory Attributes: Arabica vs Robusta",
       y = "Mean Value",
       x = "Sensory Attribute",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 45, vjust = 1, size = 10,
                                    hjust = 1))


# Analyzing which country tends to produce better quality arabica coffee
# Calculating average quality score by country
arabica_quality_country <-
  aggregate(arabica_trimmed_data[, 13:23],
            by = list(country = arabica_trimmed_data$country_of_origin),
            FUN = mean)

# Identification of Top Quality Producing Countries
arabica_quality_country <-
  arabica_quality_country[order(-arabica_quality_country$total_cup_points), ]

# Creating a bar graph for the top producing countries for arabica
ggplot(data = arabica_quality_country) +
  geom_bar(mapping = aes(x = reorder(country, -total_cup_points),
                         y = total_cup_points),
           stat = "identity", fill = "red") +
  labs(title = "Top Quality Producing Countries for Arabica",
       x = "Country of Origin",
       y = "Average Total Cup Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exporting csv file for more advance visualization
write.csv(arabica_quality_country, "arabica_quality_country.csv",
          row.names = FALSE)

# Saving visualization
ggsave("top_countries_arabica.png", width = 10, height = 7, bg = "white")



# Analyzing which country tends to produce better quality robusta coffee
# Calculating average quality score by country
robusta_quality_country <-
  aggregate(robusta_trimmed_data[, 13:23],
            by = list(country = robusta_trimmed_data$country_of_origin),
            FUN = mean)

# Identification of Top Quality Producing Countries
robusta_quality_country <-
  robusta_quality_country[order(-robusta_quality_country$total_cup_points), ]

# Creating a bar graph for the top producing countries for robusta
ggplot(data = robusta_quality_country) +
  geom_bar(mapping = aes(x = reorder(country, -total_cup_points),
                         y = total_cup_points),
           stat = "identity", fill = "red") +
  labs(title = "Top Quality Producing Countries for Robusta",
       x = "Country of Origin",
       y = "Average Total Cup Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exporting csv file for more advance visualization
write.csv(robusta_quality_country, "robusta_quality_country.csv",
          row.names = FALSE)

# Saving visualization
ggsave("top_countries_robusta.png", width = 10, height = 7, bg = "white")



# Analyzing which country tends to produce overall quality coffee
# Calculating average quality score by country
combined_quality_country <-
  aggregate(combined_coffee_data[, 13:23],
            by = list(country = combined_coffee_data$country_of_origin),
            FUN = mean)

# Identification of Top Quality Producing Countries
combined_quality_country <-
  combined_quality_country[order(-combined_quality_country$total_cup_points), ]

# Creating a bar graph for the top overall producing countries
ggplot(data = combined_quality_country) +
  geom_bar(mapping = aes(x = reorder(country, -total_cup_points),
                         y = total_cup_points),
           stat = "identity", fill = "red") +
  labs(title = "Top Overall Quality Coffee Producing Countries",
       x = "Country of Origin",
       y = "Average Total Cup Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exporting csv file for more advance visualization
write.csv(combined_quality_country, "combined_quality_country.csv",
          row.names = FALSE)

# Saving visualization
ggsave("top_countries_combined.png", width = 18, height = 7, bg = "white")


# Analysis for top owners based on top 5 coffee producing countries

# Filtering the data based on the top countries producing arabica
filtered_arabica_country <- c("United States", "Papua New Guinea",
                              "Ethiopia", "Japan", "Kenya")

# Grouping owners by country
grouped_owners_arabica <- arabica_trimmed_data %>%
  filter(country_of_origin %in% filtered_arabica_country) %>%
  group_by(owner, country_of_origin) %>%
  summarise(mean_total_cup_points = mean(total_cup_points, na.rm = TRUE)) %>%
  arrange(-mean_total_cup_points)

grouped_owners_robusta <- robusta_trimmed_data %>%
  group_by(owner, country_of_origin) %>%
  summarise(mean_total_cup_points = mean(total_cup_points, na.rm = TRUE)) %>%
  arrange(-mean_total_cup_points)

# Identifying top 10 owners that produces quality coffee bean
top_owners_arabica <- head(grouped_owners_arabica, 10)
top_owners_robusta <- head(grouped_owners_robusta, 10)


# Visualizing the output using a bar graph
ggplot(data = top_owners_arabica) +
  geom_bar(mapping = aes(x = reorder(owner, -mean_total_cup_points),
                         y = mean_total_cup_points,
                         fill = country_of_origin),
           stat = "identity") +
  labs(title = "Top 10 Owners that Produce Quality Arabica Beans",
       x = "Name of Owner",
       y = "Average Total Cup Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = top_owners_robusta) +
  geom_bar(mapping = aes(x = reorder(owner, -mean_total_cup_points),
                         y = mean_total_cup_points,
                         fill = country_of_origin),
           stat = "identity", position = "dodge") +
  labs(title = "Top 10 Owners that Produce Quality Robusta Beans",
       x = "Name of Owner",
       y = "Average Total Cup Points") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


print(top_owners_arabica)
print(top_owners_robusta)

write.csv(top_owners_robusta, "top_owners_robusta.csv", row.names = FALSE)
write.csv(top_owners_arabica, "top_owners_arabica.csv", row.names = FALSE)


# Analyzing where altitude has impact on the quality of coffee beans
# Calculating correlation between sensory values and altitude
altitude_correlation_arabica <-
  cor(na.omit(arabica_trimmed_data[, c(13:23, 31)]))

altitude_correlation_robusta <-
  cor(na.omit(robusta_trimmed_data[, c(13:23, 31)]))

# Melting data frame for use in ggplot
melted_altitude_arabica <- melt(altitude_correlation_arabica)
melted_altitude_robusta <- melt(altitude_correlation_robusta)

# Visualizing correlation between total_cup_points and altitude_mean_meters
ggplot(data = arabica_trimmed_data) +
  geom_point(mapping = aes(x = total_cup_points, y = altitude_mean_meters)) +
  labs(title = "Scatterplot of Total Cup Points vs Average Altitude (Arabica)",
       x = "Total Cup Points",
       y = "Average Altitude")

ggplot(data = robusta_trimmed_data) +
  geom_point(mapping = aes(x = total_cup_points, y = altitude_mean_meters)) +
  labs(title = "Scatterplot of Total Cup Points vs Average Altitude (Robusta)",
       x = "Total Cup Points",
       y = "Average Altitude")

ggsave("scatterplot_altitude_arabica.png", bg = "white")
ggsave("scatterplot_altitude_robusta.png", bg = "white")

# Using heatmap to visualize correlation of various sensory values and the average altitude
ggplot(melted_altitude_arabica) +
  geom_tile(mapping = aes(Var1, Var2, fill = value), color = "white") +
  geom_text(aes(Var1, Var2, label = paste0(round(value * 100), "%")),
            size = 3) +
  labs(title = "Heatmap: Sensory Values vs Average Altitude - Arabica") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))


ggplot(melted_altitude_robusta) +
  geom_tile(mapping = aes(Var1, Var2, fill = value), color = "white") +
  geom_text(aes(Var1, Var2, label = paste0(round(value * 100), "%")),
            size = 3) +
  labs(title = "Heatmap: Sensory Values vs Average Altitude - Robusta") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))

ggsave("heatmap_altitude_robusta.png", bg = "white")

# Analyzing what range of altitude does the top quality producing countries have
grouped_altitude_arabica <- arabica_trimmed_data %>%
  filter(country_of_origin %in% filtered_arabica_country) %>%
  group_by(country_of_origin) %>%
  summarise(AvgAltLow = mean(altitude_low_meters, na.rm = TRUE),
            AvgAltHigh = mean(altitude_high_meters, na.rm = TRUE),
            mean_total_cup_points = mean(total_cup_points, na.rm = TRUE)) %>%
  arrange(-mean_total_cup_points)


grouped_altitude_robusta <- robusta_trimmed_data %>%
  group_by(country_of_origin) %>%
  summarise(AvgAltLow = mean(altitude_low_meters, na.rm = TRUE),
            AvgAltHigh = mean(altitude_high_meters, na.rm = TRUE),
            mean_total_cup_points = mean(total_cup_points, na.rm = TRUE)) %>%
  arrange(-mean_total_cup_points)


print(grouped_altitude_arabica)
print(grouped_altitude_robusta)







