# Exercise.

Before doing this exercise please install package ggplot2movies

library(ggplot2movies)
library(ggplot2)
library(dplyr)
library(tidyverse)
?movies

#Pleasae analyse dataset movies available after installing package ggplot2movies
# 1. Please visualize main features of this dataset using ggplot2 package
# 2. Create chart with few panels characterising 3 most important fetaures of
# this dataset.

#Distribution of movie length
ggplot(movies, aes(x = length)) +
  geom_bar(fill = "black", color = "skyblue") + 
  coord_cartesian(xlim = c(0, 200)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12) 
  ) +
  labs(
    title = "Distribution of Movie Length",
    x = "Movie Length",
    y = "Count"
  )


# Top 10 movies
top10 <- movies %>%
  arrange(desc(rating)) %>%
  head(10) %>%
  mutate(title = fct_reorder(title, rating))
top10.bar <- ggplot(top10, aes(title, rating, fill = title))
top10.bar +
  geom_col() +
  geom_text(aes(label = rating), vjust = 1.5, color = "white", size = 3, fontface = "bold")  +
  coord_flip() +
  scale_fill_manual(values = viridisLite::viridis(10))+
  theme(legend.position = "none")

#The correlation coefficient between movie length and budget
movies <- na.omit(movies)
correlation <- cor(movies$length, movies$budget)
ggplot(movies, aes(x = length, y = budget)) +
  geom_point(color = "blue", size = 2) +  
  labs(title = paste("Correlation Between Length and Budget\nCorrelation:", round(correlation, 2)),
       x = "Length",
       y = "Budget") +
  theme_minimal() 


# Ditribution of rating in each genre
movies <- na.omit(movies)
count <- rowSums(movies[, 18:24])
movies <- movies %>%
  mutate(
    genre = case_when(
      count == 1 & Action == 1 ~ "Action",
      count == 1 & Animation == 1 ~ "Animation",
      count == 1 & Comedy == 1 ~ "Comedy",
      count == 1 & Drama == 1 ~ "Drama",
      count == 1 & Documentary == 1 ~ "Documentary",
      count == 1 & Romance == 1 ~ "Romance",
      count == 1 & Short == 1 ~ "Short"
    )
  ) %>%
  filter(!is.na(genre)) %>%
  mutate(
    genre = as.factor(genre)
  )
ggplot(data = movies, aes(x = rating, fill = genre)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~genre) +
  ggtitle("Histogram of Rating by Genre") +
  xlab("Rating") +
  ylab("Count")
