library(ggplot2)
library(tidyverse)

#Reading Data
model1data <- readRDS("model1data.rds")
model2data <- readRDS("model2data.rds")
model3data <- readRDS("model3data.rds")

#Models
(model1 <- lm(diffdiff ~ deltarep, data = model1data))
(model2 <- lm(diffdiff ~ deltarep + deltarep*state_big + factor(year), data = model2data)) 
(model3 <- lm(diffdiff ~ deltarep + deltarep*change_indicator2, data = model3data))

#Graph
data_t3_final_size |>
  ggplot(aes(x = deltarep, y = diffdiff)) +
  geom_point(aes(color = factor(state_big))) +
  facet_wrap(~ year) +
  geom_smooth(method = "lm", aes(color = factor(state_big)))