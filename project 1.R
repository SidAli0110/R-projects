library(ggplot2)

# Computing area of sepal.
iris<-iris%>%
  mutate(Area=Sepal.Length*Sepal.Width)

# Doing statistical analysis for significance.
anova_model <- aov(Area ~ Species, data = iris)
summary(anova_model)


# Perform pairwise t-test with Bonferroni adjustment
pair_t_test <- pairwise.t.test(iris$Area, iris$Species, p.adjust.method = "bonferroni")

# Extract p-values from the pairwise t-test results
p_values_pairwise <- pair_t_test$p.value  # This is a matrix of p-values

# Print the p-value matrix to inspect it
print(p_values_pairwise)

# foramting the p values.
p_values_df <- data.frame(
  comparison = c("versicolor vs setosa", "virginica vs setosa", "virginica vs versicolor"),
  p_value = c(p_values_pairwise["versicolor", "setosa"], 
              p_values_pairwise["virginica", "setosa"], 
              p_values_pairwise["virginica", "versicolor"]))

# Set the maximum area for annotating the line, for p_value.

max_y <- max(iris$Area)

# Making a variable to add caption in the plot.
caption_text <- str_wrap(
  "In this graph area of sepal is represnted on Y-axis, 
  while species are on X-axis. Area of sepal, from three different 
  species are compared. Accorindg to statisitics, area difference 
  between setosa and versicolr is non_siginficant i.e. 0.718. On the 
  contrary, difference between setosa and varginica, versicolor and 
  verginica are significant i.e.4e-04 and 0, respectively.(Iris Dataset)",
  width=200
)

# making plot
plot <- ggplot(data = iris, aes(x=Species, y=Area, fill=Species))+
  geom_col(alpha=1)+
  theme_classic()+
  labs(title = "Area of  Sepal",
       subtitle = "Different Species Represented by Color",
       y = "Area",
       caption = caption_text)+
  geom_text(
    aes(
      x = 1.4,  # Adjust the p_value in the horizontal position, for versicolor and setosa.
      y = max(Area) + 980,  # Adjust the p_value in the vertical position, for versicolor and setosa
      label = paste("p-value = ", round(p_values_pairwise["versicolor", "setosa"], 4))
    ),
    size = 2.5, hjust = 0
  ) +
  geom_text(
    aes(
      x = 1.9,  # Adjust the p_value in the horizontal position, for virginica setosa.
      y = max(Area) + 1200,  # Adjust the p_value in the vertical position, for virginica setosa.
      label = paste("p-value = ", round(p_values_pairwise["virginica", "setosa"], 4))
    ),
    size = 2.5, hjust = 0
  ) +
  geom_text(
    aes(
      x = 2.4,  # Adjust the p_value in the horizontal position, for virginica versicolor.
      y = max(Area) + 1070,  # Adjust the p_value in the vertical position, for virginica versicolor.
      label = paste("p-value = ", round(p_values_pairwise["virginica", "versicolor"], 4))
    ),
    size = 2.5, hjust = 0
  )+
   # Adjust and make the line using max_Y variable, for setosa and versicolor.
  annotate("segment", x = 1, xend = 2, y = max_y + 930, yend = max_y + 930, 
           color = "black", size = 0.7) + 
  
  # Adjust and make the line using max_Y variable, for virginica and versicolor.
  annotate("segment", x = 2, xend = 3, y = max_y + 1020, yend = max_y + 1020, 
           color = "black", size = 0.7) +
  
  # Adjust and make the line using max_Y variable, for setosa and virginica.
  annotate("segment", x = 1, xend = 3, y = max_y + 1150, yend = max_y + 1150, 
           color = "black", size = 0.7)+
   
  theme(
    axis.title.x = element_blank(), # Removing x-axis title.
    legend.position = "none", # Removing legend.
    plot.caption = element_text(hjust = 0, size = 10, margin = margin(t = 10)) # Adjusting caption.
  )+ 
  
  # Make the labels of x-axis in capital.
  
  scale_x_discrete(labels = str_to_title(levels(iris$Species)))

# Print Plot.

print(plot) 
  
