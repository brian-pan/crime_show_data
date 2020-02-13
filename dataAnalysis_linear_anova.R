library(tidyverse)

# input data
originaldata <- readRDS("crime_show_ratings.RDS")

# create side-by-side box plots
originaldata %>%
  ggplot(aes(x = decade, y = season_rating)) +
  geom_boxplot() + 
  ggtitle("Decade Average Rating for Crime Shows Boxplot")

# create Facetted histograms
originaldata %>%
  ggplot(aes(x = season_rating)) + 
  geom_histogram(bins = 30) +
  facet_wrap(~decade) +
  ggtitle("Decade Average Rating for Crime Shows Histogram")

# Conduct a one-way ANOVA
anova_1 <- originaldata %>% 
  aov(season_rating ~ as.factor(decade), data=.)

summary(anova_1)

# Residual plot
plot(anova_1, 1)

# Normal QQ plot
plot(anova_1, 2)

# find the variance of each decade(using tidyverse)
originaldata %>%
  group_by(decade) %>%
  summarise(variance_rating = sd(season_rating)^2)

# Conduct a linear regression model
lm_1<- lm(season_rating ~ as.factor(decade), data = originaldata)

summary(lm_1)