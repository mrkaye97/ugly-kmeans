library(tidyverse)

source("ugly_kmeans.R")

data <- iris %>%
  select(-Species) %>%
  mutate(across(everything(), ~ scale(.x)[,1])) %>%
  as_tibble()

k <- 3

clusters <- ugly_kmeans(data, k) %>%
  as.factor()

my_kmeans <- iris %>%
  mutate(cluster = clusters) %>%
  ggplot(aes(Sepal.Length, Sepal.Width, color = cluster, shape = Species)) +
  geom_point(size = 5)

## using R's built in `stats::kmeans()`
actual <- kmeans(
  data, centers = 3
)

stats_kmeans <- iris %>%
  mutate(cluster = as.factor(actual$cluster)) %>%
  ggplot(aes(Sepal.Length, Sepal.Width, color = cluster, shape = Species)) +
  geom_point(size = 5)

library(patchwork)

my_kmeans + stats_kmeans + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
