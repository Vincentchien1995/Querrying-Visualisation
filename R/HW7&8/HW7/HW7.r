# Exercise.

library(ggplot2)
?economics

# 1. Please analyse using visual technics #economics dataset available in package # ggplot2.
# Visualize charasteristics of this dataset.
# 3. Create bublechart for 3 variables of this dataset.

# The personal savings rate varies over times 
qplot(date, psavert, data=economics, 
      main="Pesrsonal Saving Rate",
      geom=c("point", "line"))

# The median duration of unemployment varies over time
ggplot(economics, aes(x = date, y = uempmed)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Median Duration of Unemployment",
       x = "Date",
       y = "Uempmed") +
  theme_minimal() 

# The correlation coefficient comparing the unemployed population and the median duration of unemployment
correlation <- cor(economics$uempmed, economics$unemploy)
ggplot(economics, aes(x = uempmed, y = unemploy)) +
  geom_point() +
  labs(title = paste("Correlation Between Unemploy and Uempmed\nCorrelation:", round(correlation, 2)),
       x = "uempmed",
       y = "unemploy") +
  theme_minimal()

#Comparing the relationship between consumption expenditure and personal savings rate
ggplot(economics, aes(x = pce, y = psavert)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Consumption vs Savings Rate",
       x = "Personal Consumption",
       y = "Personal Savings Rate") +
  theme_minimal()

# The total population and the personal saving rate vary over time.
qplot(pop, psavert, data=economics, colour = date , xlab =  "Population" , ylab = "Personal savings rate" )


# The unemployed population and the median duration of unemployment vary over time
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_point(aes(color =uempmed)) +
  labs(x = "Date",
       y = "Unemployment",
       title = "Number of Unemployed Over Time") +
  theme_bw()+
  scale_color_gradient(low = "blue", high = "red")








  



