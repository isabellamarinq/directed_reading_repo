library(haven)
library(tidyverse)
library(patchwork)

##fig 5 data

data <- read_dta("data/ReplicationData.dta")

data2 <- data |>  #cleaning subset
  select(year, fedex90, pop_census) |>
  filter(year > 1969) |>
  mutate(fedex90 = ifelse(is.na(fedex90), 0, fedex90))

years <- rep(NA, 50)  #creating year variable

for (i in 1970:2016) {
  output <- data2 |>
    filter(year == i) |>
    summarise_all(sum)
  
  years = rbind(years, output)
}

composite_data <- years |>  #fixing year variable
  mutate(year = year / 50)

composite_data <- composite_data[-c(1, 39:48), ]  #subset

composite_data <- composite_data |>  #creating outlays per capita variable
  mutate(outlays_per_capita = (fedex90 / pop_census)*1000000)

composite_data2 <- na_if(composite_data, 0) #from 0 to n/a

composite_data2 <- composite_data2 |>  #deleting n/a
  filter(!is.na(outlays_per_capita))

saveRDS(data, "data.rds")
saveRDS(composite_data2, "fig5_data.rds")

##table 2 data

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

data3 <- transform(data3, nat_pop = as.numeric(nat_pop))   #data3 now includes national population variable

data3 <- data3 |>
  group_by(fips) |>
  arrange(year) |>
  mutate(seats_budget_nplus1 = lead(seats_budget))

data3 <- data3 |>  #RRI Equation, adding RRI variable
  mutate(lnrri = log((seats_budget_nplus1/pop_census)/(435/nat_pop)))

data4 <- data3|>  #data4 is the final version with national population and lnRRI after ungrouping to make sure the loop works
  ungroup()

saveRDS(data4, "tab2data.rds")

#fig3 data

data5 <- data4 |>   #data5 holds information about whether the number of seats changed from 1993 to 1994 for each state
  filter(year %in% c(1993:1994)) |>
  group_by(fips) |>
  arrange(year) |>
  pivot_wider(id_cols = fips, names_from = year, values_from = seats_budget) |>
  mutate(sign_change = sign(`1994` - `1993`))

saveRDS(data5, "fig3data.rds")

#adding state labels to several dataframes

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

#data for fig3 graph1

graph1 <- data6 |>
  group_by(sign_change, year) |>
  summarize(tot_seats = sum(seats_budget_nplus1)) |>
  mutate(share_seats = tot_seats/435)

subset_nat_pop <- data6 |>
  filter(fips == 1) |>
  select(year, nat_pop)

subset_nat_pop <- subset_nat_pop[, -1]

graph1 <- graph1[ , c(2, 1, 3, 4)]

saveRDS(graph1, "graph1data.rds")

#data for fig3 graph2

graph2 <- data6 |>
  group_by(sign_change, year) |>
  summarize(tot_pop = sum(pop_census)) 

graph2 <- subset_nat_pop |>
  right_join(graph2, by = "year")

graph2 <- graph2 |>
  mutate(share_pop = tot_pop/nat_pop)

graph2 <- graph2[ , c(1, 3, 2, 4, 5)]

saveRDS(graph2, "graph2data.rds")

#data for fig3 graph3

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

saveRDS(graph3, "graph3data.rds")

#data for Model 1

data_t3 <- data |>  #getting lag to create deltarep
  group_by(state) |>
  mutate(seats_budget_lag = lag(seats_budget, n = 2))

data_t3 <- data_t3 |>  #Delta Rep (Independent Variable)
  mutate(deltarep = log(seats_budget) - log(seats_budget_lag))


data_t3 <- data_t3 |>  #Lagged Outlays (for Dependent Variable)
  group_by(state) |>
  arrange(year) |>
  mutate(share_outlays_lag2 = lag(fedex, n = 2),
         share_outlays_lag4 = lag(fedex, n = 4))

data_t3 <- data_t3 |>   #adding variable Diff
  filter(year %in% c(1974, 1984, 1994, 2004)) |>
  mutate(diff = log(fedex) - log(share_outlays_lag2))

data_t3 <- data_t3 |>   #adding variable LagDiff
  filter((year == 1974 & fips != 41) | year == 1984 | year == 1994 | year == 2004) |>
  mutate(lagdiff = log(share_outlays_lag2)  - log(share_outlays_lag4)) 

data_t3_final <- data_t3 |>   #adding variable DiffDiff
  mutate(diffdiff = diff - lagdiff) |>
  filter(year %in% c(1974, 1994, 2004))

saveRDS(data_t3_final, "model1data.rds")

#data for Model 2

data_t3_final_size <- data_t3_final |>  #adding size variable
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

data_t3_final_size <- data_t3_final_size |>  #cleaning size variable
  mutate_at('state_big', ~replace_na(.,0))

saveRDS(data_t3_final_size, "model2data.rds")

#data for Model 3

data_t3_final_ind <- data_t3_final |>  #creating change indicator variable
  mutate(change_indicator = sign(deltarep)) 

data_t3_final_ind <- data_t3_final_ind |>  #clearning change indicator variable
  mutate(change_indicator2 = case_when(
    change_indicator == 0 ~ 0,
    change_indicator == -1 ~ 1,
    change_indicator == 1 ~ 0)) 

saveRDS(data_t3_final_ind, "model3data.rds")
