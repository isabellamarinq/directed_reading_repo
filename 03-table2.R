library(ggplot2)
library(tidyverse)

#Reading Data
tab2data <- readRDS("tab2data.rds")

#Forming Table

summary <- rep(NA, 8)

for (i in c(1972, 1973, 1982, 1983, 1992, 1993, 2002, 2003)) {
  output3 <- tab2data |>
    filter(year == i) |>
    summarize(
      median(lnrri), 
      min(lnrri), 
      max(lnrri), 
      quantile(lnrri, .1),
      quantile(lnrri, .9)
    )
  
  summary = rbind(summary, output3) 
}

summary <- summary[ -1, ]

summary <- summary |>
  mutate(range = `max(lnrri)` - `min(lnrri)`, year = c(1972, 1973, 1982, 1983, 1992, 1993, 2002, 2003))

summary |>
  select(year, `median(lnrri)`, range, `quantile(lnrri, 0.1)`, `quantile(lnrri, 0.9)`) |>
  mutate_if(is.numeric, round, digits = 2)
