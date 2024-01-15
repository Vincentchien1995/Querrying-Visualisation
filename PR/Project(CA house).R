library(dplyr)
library(ggplot2)

#Read data
data <- read.csv("https://drive.google.com/uc?id=1fCX5OIcRd44y3QYwfP_pkh9Ci-tIEzOc", header = TRUE)
data

# Add new column 'New_ocean' 
data <- data %>%
  mutate(New_ocean = case_when(
    ocean_proximity %in% c('NEAR BAY', 'NEAR OCEAN') ~ 3,
    ocean_proximity == '<1H OCEAN' ~ 2,
    ocean_proximity == 'INLAND' ~ 1,
    TRUE ~ NA_real_
  ))

# Drop column ocean_proximity and null value in New_ocean
data <- subset(data, select = -ocean_proximity)
data <- data[!is.na(data$New_ocean), ]

# Check number on nulls or missing values in each column
colSums(is.na(data))
#drop null vaules
data_p <- na.omit(data)

# Bar chart with distribution of median house value
ggplot(data_p, aes(x = median_house_value)) +
  geom_histogram(binwidth = 10000, fill = "gold", color = "black", alpha = 0.7) +
  # Adding vertical lines for median and mean
  geom_vline(aes(xintercept = median(median_house_value), color = "Median"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean(median_house_value), color = "Mean"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("Median" = "blue", "Mean" = "red")) +
  labs(title = "Distribution of Median House Value",
       x = "Median House Value",
       y = "Frequency")

# Correlation of all variable with house price using correlation heatmap
correlation_matrix <- cor(data_p)
ggplot(data = as.data.frame(as.table(correlation_matrix)), 
       aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = NULL,y = NULL)

# Perform a plot on US map using the latitude and longitude of the housing
us_map <- map_data("usa")
p <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = data_p, aes(x = longitude, y = latitude, color = median_house_value), size = 3) +
  scale_color_gradient(name = "Median House Value", low = "lightpink", high = "black") +
  labs(title = "CA House Median Values on US Map")
print(p)

# Create a boxplot using New_ocean columns
ggplot(data_p, aes(x = factor(New_ocean), y = median_house_value, fill = factor(New_ocean))) +
  geom_boxplot() +
  labs(title = "Median House Value by Location",
       x = "Location",
       y = "Median House Value",
       fill = NULL) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink"),
                    breaks = c(1, 2, 3),
                    labels = c("INLAND", "<1H OCEAN", "Near Ocean")) +
  theme_minimal()

# Median income histogram
ggplot(data_p, aes(x = median_income)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Median Income Histogram",
       x = "Median Income",
       y = "Frequency")

# Compare income and  house values with Hexbin Plot
ggplot(data_p, aes(x = median_income, y = median_house_value)) +
  geom_hex(aes(fill = ..count..), color = "orange") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(title = "Relationship between Median Income and Median House Value",
       x = "Median Income",
       y = "Median House Value") +
  theme_minimal() +
  theme(text = element_text(size = 18))

# Compare housing_median_age and median_house_value with Scatter plot
ggplot(data_p, aes(x = housing_median_age, y = median_house_value)) +
  geom_point(color = "lightgray", size = 2) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  labs(title = "Scatter Plot with Linear Regression",
       x = "Median Age",
       y = "Median House Value") +
  theme_minimal()

# Compare total_room and total_bed_room base on house_price
ggplot(data_p, aes(x = total_rooms, y = total_bedrooms)) +
  geom_point(color = "lightblue", size = 3) +
  labs(title = "Relationship between Total Rooms and Total Bedrooms",
       x = "Total Rooms",
       y = "Total Bedrooms") +
  facet_wrap(~cut(median_house_value, breaks = 4), scales = "free") + 
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20)  
  )

# Population and Household with median_house_value
ggplot(data_p, aes(x = population, y = households, size = median_house_value, color = median_house_value)) +
  geom_point(alpha = 0.3) +
  scale_size_continuous(range = c(5, 20)) +
  scale_color_gradient(low = "gold", high = "purple") + 
  labs(title = "Population vs Households with Median House Value",
       x = "Population",
       y = "Households",
       size = "Median House Value",
       color = "Median House Value") +
  theme(plot.title = element_text(size = 18)) 





