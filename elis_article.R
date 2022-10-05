library(haven)
library(tidyverse)
library(ggplot2)
library(patchwork)

##Fig 5

data <- haven::read_dta("data/ReplicationData.dta")
data2 <- data |> 
  select(year, fedex90, pop_census) |>
  filter(year > 1969) |>
  mutate(fedex90 = ifelse(is.na(fedex90), 0, fedex90))

#Loop for Sums

years <- rep(NA, 50)

for (i in 1970:2016) {
  output <- data2 |>
    filter(year == i) |>
    summarise_all(sum)
  
  years = rbind(years, output)
}

years

#Data Frame with Outlays per Capita

composite_data <- years |>
  mutate(year = year / 50)

composite_data <- composite_data[-c(1, 39:48), ]

composite_data <- composite_data |>
  mutate(outlays_per_capita = (fedex90 / pop_census)*1000000)

composite_data2 <- na_if(composite_data, 0)

composite_data2 <- composite_data2 |>
  filter(!is.na(outlays_per_capita))

#Plot

composite_data2 |>
  ggplot(aes(x = year, y = outlays_per_capita)) +
  geom_line() +
  geom_point() +
  ylim(2500, 5500) +
  labs(title = "Apportionment Cycles as Natural Experiments", x = "Fiscal Year", y = "Outlays Per Capita ($90)")

##Table 2---------------------------------------------------------------------

#Formatting Data

data3 <- data |>
  filter(year > 1969) |>
  arrange(year)

country_pop <- composite_data |>
  select(pop_census)

rep_country_pop <- rep(NA, 50)

for (i in 1:37) {  #Getting repeated list of national spending for each state every year
  output2 <- print(rep(country_pop[i, ], 50))
  
  rep_country_pop = rbind(rep_country_pop, output2)
}

rep_country_pop <- as.data.frame(t(rep_country_pop))

rep_country_pop <- rep_country_pop[ , -(1)]
rep_country_pop <- rep_country_pop[ , -(38:75)]

rep_country_pop_df <- data.frame(a=unlist(rep_country_pop, use.names = FALSE))

data3 <- data3 |>
  mutate(nat_pop = rep(0, 1850))

data3$nat_pop <- rep_country_pop_df$a

data3 <- transform(data3, nat_pop = as.numeric(nat_pop))

data3 <- data3 |>
  group_by(fips) |>
  arrange(year) |>
  mutate(seats_budget_nplus1 = lead(seats_budget))

data3 <- data3 |>  #RRI Equation
  mutate(lnrri = log((seats_budget_nplus1/pop_census)/(435/nat_pop)))

data4 <- data3|>  #Ungrouping to make sure the loop works
  ungroup()

#Forming Table

summary <- rep(NA, 8)

for (i in c(1972, 1973, 1982, 1983, 1992, 1993, 2002, 2003)) {
  output3 <- data4 |>
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

##Fig 3------------------------------------------------------------------------

data5 <- data4 |>
  filter(year %in% c(1993:1994)) |>
  group_by(fips) |>
  arrange(year) |>
  pivot_wider(id_cols = fips, names_from = year, values_from = seats_budget) |>
  mutate(sign_change = sign(`1994` - `1993`))

states_key <- tibble::tribble(
  ~state, ~fips,
  
  "AL",    1,
  
  "AK",    2,
  
  "AZ",    4,
  
  "AR",    5,
  
  "CA",    6,
  
  "CO",    8,
  
  "CT",    9,
  
  "DE",   10,
  
  "FL",   12,
  
  "GA",   13,
  
  "HI",   15,
  
  "ID",   16,
  
  "IL",   17,
  
  "IN",   18,
  
  "IA",   19,
  
  "KS",   20,
  
  "KY",   21,
  
  "LA",   22,
  
  "ME",   23,
  
  "MD",   24,
  
  "MA",   25,
  
  "MI",   26,
  
  "MN",   27,
  
  "MS",   28,
  
  "MO",   29,
  
  "MT",   30,
  
  "NE",   31,
  
  "NV",   32,
  
  "NH",   33,
  
  "NJ",   34,
  
  "NM",   35,
  
  "NY",   36,
  
  "NC",   37,
  
  "ND",   38,
  
  "OH",   39,
  
  "OK",   40,
  
  "OR",   41,
  
  "PA",   42,
  
  "RI",   44,
  
  "SC",   45,
  
  "SD",   46,
  
  "TN",   47,
  
  "TX",   48,
  
  "UT",   49,
  
  "VT",   50,
  
  "VA",   51,
  
  "WA",   53,
  
  "WV",   54,
  
  "WI",   55,
  
  "WY",   56
)

data6 <- data4 |>
  filter(year %in% c(1987:2001))

data6 <- data5 |>
  select(fips, sign_change) |>
  right_join(data6, by = "fips")

data4 <- states_key |>
  left_join(data4, by = "fips")

data <- states_key |>
  left_join(data, by = "fips")

#Graph 1 Subset

graph1 <- data6 |>
  group_by(sign_change, year) |>
  summarize(tot_seats = sum(seats_budget_nplus1)) |>
  mutate(share_seats = tot_seats/435)

subset_nat_pop <- data6 |>
  filter(fips == 1) |>
  select(year, nat_pop)

subset_nat_pop <- subset_nat_pop[, -1]

graph1 <- graph1[ , c(2, 1, 3, 4)]

#Graph 2 Subset

graph2 <- data6 |>
  group_by(sign_change, year) |>
  summarize(tot_pop = sum(pop_census)) 

graph2 <- subset_nat_pop |>
  right_join(graph2, by = "year")

graph2 <- graph2 |>
  mutate(share_pop = tot_pop/nat_pop)

graph2 <- graph2[ , c(1, 3, 2, 4)]

#Graph 3 Subset

graph3 <- data6 |>
  group_by(sign_change, year) |>
  summarize(tot_outlays = sum(fedex90)) 

year_outlays <- graph3 |>
  group_by(year) |>
  summarize(yearly_outlays = sum(tot_outlays))

graph3 <- year_outlays |>
  right_join(graph3, by = "year")

graph3 <- graph3|>
  mutate(share_outlays = tot_outlays / yearly_outlays)

graph3 <- graph3[ , c(1, 3, 2, 4, 5)]

#Putting it all together

graph4 <- graph1 |>
  unite(joint, c(year, sign_change))

graph2j <- graph2 |>
  unite(joint, c(year, sign_change))

graph3j <- graph3 |>
  unite(joint, c(year, sign_change))

graph4 <- graph2j |>
  right_join(graph4, by = "joint")

graph4 <- graph3j |>
  right_join(graph4, by = "joint")

#Making Figure

g1 <- graph1 |>
  ggplot(aes(x = year, y = share_seats, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Year", y = "Share of House of Representatives") 

g2 <- graph2 |>
  ggplot(aes(x = year, y = share_pop, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Year", y = "Share of Total Population")

g3 <- graph3 |>
  ggplot(aes(x = year, y = share_outlays, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Fiscal Year", y = "Share of Federal Outlays")

g1 + g2 + g3 + plot_layout(nrow = 3)

##Table 3------------------------------------------------------------------

#Model 1

#Delta Rep (Independent Variable)

data_t3 <- data |>
  group_by(state) |>
  mutate(seats_budget_lag = lag(seats_budget, n = 2))

data_t3 <- data_t3 |>
  mutate(deltarep = log(seats_budget) - log(seats_budget_lag))

#Lagged Outlays (for Dependent Variable)

data_t3 <- data_t3 |>
  group_by(state) |>
  arrange(year) |>
  mutate(share_outlays_lag2 = lag(fedex, n = 2),
         share_outlays_lag4 = lag(fedex, n = 4))

#Diff Variables (Dependent Variable) 

data_t3 <- data_t3 |>   #Diff
  filter(year %in% c(1974, 1984, 1994, 2004)) |>
  mutate(diff = log(fedex) - log(share_outlays_lag2))

data_t3 <- data_t3 |>   #LagDiff
  filter((year == 1974 & fips != 41) | year == 1984 | year == 1994 | year == 2004) |>
  mutate(lagdiff = log(share_outlays_lag2)  - log(share_outlays_lag4)) 

data_t3_final <- data_t3 |>   #DiffDiff
  mutate(diffdiff = diff - lagdiff) |>
  filter(year %in% c(1974, 1994, 2004))

#Model

(model1 <- lm(diffdiff ~ deltarep, data = data_t3_final))

#Model 2------

#Creating Indicator Variable

data_t3_final_size <- data_t3_final |>
  mutate(state_big = state) |>
  mutate(state_big = case_when(
    state_big == "CA" ~ 1,
    state_big == "FL" ~ 1,
    state_big == "IL" ~ 1,
    state_big == "MI" ~ 1, 
    state_big == "NY" ~ 1,
    state_big == "OH" ~ 1,
    state_big == "PA" ~ 1,
    state_big == "TX" ~ 1)) 

data_t3_final_size <- data_t3_final_size |>
  mutate_at('state_big', ~replace_na(.,0))

#Model

(model2 <- lm(diffdiff ~ deltarep + deltarep*state_big + factor(year), data = data_t3_final_size)) 

#Model 3------

#Creating Indicator Variable

data_t3_final_ind <- data_t3_final |>
  mutate(change_indicator = sign(deltarep)) 

data_t3_final_ind <- data_t3_final_ind |>
  mutate(change_indicator2 = case_when(
    change_indicator == 0 ~ 0,
    change_indicator == -1 ~ 1,
    change_indicator == 1 ~ 0)) 

#Model

(model3 <- lm(diffdiff ~ deltarep + deltarep*change_indicator2, data = data_t3_final_ind))

data_t3_final_size |>
  ggplot(aes(x = deltarep, y = diffdiff)) +
  geom_point(aes(color = factor(state_big))) +
  facet_wrap(~ year) +
  geom_smooth(method = "lm", aes(color = factor(state_big)))