library(ggplot2)
library(tidyverse)

#Reading Data
fig5_data <- readRDS("fig5_data.rds")

#Making Graph
fig5_data |>
  ggplot(aes(x = year, y = outlays_per_capita)) +
  geom_line() +
  geom_point() +
  ylim(2500, 5500) +
  labs(title = "Apportionment Cycles as Natural Experiments", x = "Fiscal Year", y = "Outlays Per Capita ($90)")