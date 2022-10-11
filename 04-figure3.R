library(ggplot2)
library(tidyverse)

#Reading Data
graph1data <- readRDS("graph1data.rds")
graph2data <- readRDS("graph2data.rds")
graph3data <- readRDS("graph3data.rds")

#Graphs

g1 <- graph1data |>
  ggplot(aes(x = year, y = share_seats, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Year", y = "Share of House of Representatives") 

g2 <- graph2data |>
  ggplot(aes(x = year, y = share_pop, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Year", y = "Share of Total Population")

g3 <- graph3data |>
  ggplot(aes(x = year, y = share_outlays, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Fiscal Year", y = "Share of Federal Outlays")

#Compiling Graphs
g1 + g2 + g3 + plot_layout(nrow = 3)
